#' Draw SSPs Map and Barplot for CMIP6
#'
#' This script loads scenario data and polygons, then displays a spatial map
#' and barplot similar to Matlab's Draw_SSPs.m using ggOceanMaps.
#'
#' Dependencies: ggOceanMaps, ggspatial, ggplot2, dplyr, readr, sf, cowplot, viridis, R.matlab

suppressPackageStartupMessages({
  library(ggOceanMaps)
  library(ggspatial)
  library(ggplot2)
  library(dplyr)
  library(readr)
  library(sf)
  library(cowplot)
  library(viridis)
  library(R.matlab)
})

# ---- Parameters ----
DIR <- "./"
region_files <- c(
  "polygon_gomss_subarea_gom.csv",   # 1
  "polygon_ss_merged.csv",           # 2
  "polygon_gsl_subarea_gsl_clipped.csv", # 3
  "polygon_ns_subarea_sns.csv",      # 4
  "polygon_ns_merged.csv",           # 5
  "polygon_ls_merged.csv",           # 6
  "polygon_hb_subarea_hb.csv",       # 7
  "polygon_bb_subarea_bb.csv",       # 8
  "polygon_bcs_subarea_bcs.csv",     # 9
  "polygon_ca_subarea_sbs.csv"       # 10
)
region_names <- c("GoM", "SS", "GSL", "SNS", "NNS", "LS", "HB", "BB", "BCS", "SBS")

# ---- Load scenario data ----
mat_data <- readMat(file.path(DIR, "SSP_2040_2059_annual.mat"))
get_var <- function(x) as.vector(mat_data[[x]])
diff_mean_126 <- get_var("diff_mean_126")
diff_mean_245 <- get_var("diff_mean_245")
diff_mean_370 <- get_var("diff_mean_370")
diff_mean_585 <- get_var("diff_mean_585")
diff_std_126  <- get_var("diff_std_126")
diff_std_245  <- get_var("diff_std_245")
diff_std_370  <- get_var("diff_std_370")
diff_std_585  <- get_var("diff_std_585")

# ---- Color mapping ----
temp_bins <- c(0.5, 0.8, 1.0, 1.2, 1.4, 1.6, 1.8, 2.0)
n_bins <- length(temp_bins)
thermal_colors <- viridis(n_bins, option = "plasma")
assign_color <- function(value, bins, colors) {
  idx <- findInterval(value, bins, rightmost.closed=TRUE)
  idx[idx == 0] <- 1
  idx[idx > length(colors)] <- length(colors)
  colors[idx]
}

# ---- Read and prepare polygons ----
regions_data <- list()
label_positions <- list(
  `2` = c(2, -2), `3` = c(0.8, 0.1), `4` = c(6, -2),
  `5` = c(4, 0), `6` = c(2, 0), `9` = c(-5, 1), `10`= c(-140, 72)
)
for (i in seq_along(region_files)) {
  poly_path <- file.path(DIR, region_files[i])
  poly_data <- read_csv(poly_path, show_col_types=FALSE)
  # Ensure columns named "lon", "lat"
  if (!all(c("lon", "lat") %in% names(poly_data))) {
    names(poly_data)[1:2] <- c("lon", "lat")
  }
  # Skip if not enough points for a polygon
  if (nrow(poly_data) < 3) {
    warning(sprintf("Polygon for region %d (%s) has less than 3 points, skipping.", i, region_files[i]))
    next
  }
  # Try-catch for sf conversion
  poly_sf <- tryCatch({
    poly_data %>%
      st_as_sf(coords = c("lon", "lat"), crs=4326) %>%
      summarise(geometry = st_combine(geometry)) %>%
      st_cast("POLYGON")
  }, error = function(e) {
    warning(sprintf("Polygon conversion failed for region %d (%s): %s", i, region_files[i], e$message))
    return(NULL)
  })
  if (is.null(poly_sf) || nrow(poly_sf) == 0) next
  poly_sf$region_id <- i
  poly_sf$region_name <- region_names[i]
  poly_sf$temp_change <- diff_mean_245[i]
  poly_sf$color <- assign_color(diff_mean_245[i], temp_bins, thermal_colors)
  # Label position logic ...
  if (as.character(i) %in% names(label_positions)) {
    adj <- label_positions[[as.character(i)]]
    if (i == 10) {
      poly_sf$xlon <- adj[1]; poly_sf$ylat <- adj[2]
    } else {
      lon <- poly_data$lon; lat <- poly_data$lat
      poly_sf$xlon <- mean(lon, na.rm=TRUE) + adj[1]
      poly_sf$ylat <- mean(lat, na.rm=TRUE) + adj[2]
    }
  } else {
    lon <- poly_data$lon; lat <- poly_data$lat
    poly_sf$xlon <- mean(lon, na.rm=TRUE)
    poly_sf$ylat <- mean(lat, na.rm=TRUE)
  }
  regions_data[[length(regions_data) + 1]] <- poly_sf
}
if (length(regions_data) == 0) stop("No valid polygons found!")
all_regions <- do.call(rbind, regions_data)

# ---- Main map ----
main_map <- basemap(
  limits = c(-150, -40, 40, 80),
  crs = 3978,
  land.col = "grey70",
  grid.col = "grey90",
  grid.size = 0.1
) +
  geom_sf(data = all_regions, aes(fill = temp_change), color = "black", size = 0.2, alpha = 1) +
  scale_fill_gradientn(
    colors = thermal_colors,
    limits = c(min(temp_bins), max(temp_bins)),
    breaks = temp_bins,
    name = "SST Change (°C)",
    guide = guide_colorbar(barwidth=1, barheight=10, title.position="top")
  ) +
  geom_text(
    data = all_regions,
    aes(x = xlon, y = ylat + 0.1, label = region_id),
    size = 3, color = "white", fontface = "bold"
  ) +
  theme(
    legend.position = "right",
    legend.title = element_text(size=8),
    legend.text = element_text(size=6)
  )

# ---- Barplot for all scenarios ----
df <- data.frame(
  region = factor(1:10, labels=paste0(1:10, "-", region_names)),
  mean_126 = diff_mean_126,
  mean_245 = diff_mean_245,
  mean_370 = diff_mean_370,
  mean_585 = diff_mean_585,
  std_126  = diff_std_126,
  std_245  = diff_std_245,
  std_370  = diff_std_370,
  std_585  = diff_std_585
)
bar_colors <- c("#CCCCCC", "#999999", "#666666", "#333333")
bar_width <- 0.1

barplot_data <- tidyr::pivot_longer(
  df, cols=starts_with("mean_"), names_to="scenario", values_to="change"
) %>%
  mutate(
    std = c(df$std_126, df$std_245, df$std_370, df$std_585),
    xpos = as.numeric(region) + c(rep(-0.4,10), rep(-0.2,10), rep(0,10), rep(0.2,10)),
    scenario = factor(scenario, levels=c("mean_126","mean_245","mean_370","mean_585"),
                      labels=c("SSP1-2.6","SSP2-4.5","SSP3-7.0","SSP5-8.5")),
    fill = rep(bar_colors, each=10)
  )

bar_chart <- ggplot(barplot_data, aes(x=xpos, y=change, fill=fill)) +
  geom_col(width=bar_width, color="black") +
  geom_errorbar(aes(ymin=change-std, ymax=change+std), width=0.05) +
  scale_x_continuous(
    breaks = 1:10,
    labels = paste0(1:10, "-", region_names),
    limits = c(0.2, 10.8)
  ) +
  scale_fill_identity() +
  ylab("T change (°C)") +
  xlab("") +
  theme_minimal(base_size=8) +
  theme(
    axis.text.x = element_text(angle=45, hjust=1, size=8),
    axis.text.y = element_text(size=6),
    axis.title.y = element_text(size=7),
    legend.position = "none"
  ) +
  ylim(0, 3.8) +
  labs(fill="Scenario") +
  guides(fill=guide_legend(title="Scenario"))

# ---- Combine plots ----
combined_plot <- plot_grid(
  main_map,
  bar_chart,
  ncol = 1,
  rel_heights = c(3, 1),
  align = "v"
)

print(combined_plot)
# ggsave("SSP_map.png", combined_plot, width=12, height=10, dpi=300)
