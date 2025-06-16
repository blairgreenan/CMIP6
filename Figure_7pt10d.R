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
  library(RColorBrewer)
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
#region_names <- rev(c("GoM", "SS", "GSL", "SNS", "NNS", "LS", "HB", "BB", "BCS", "SBS"))


# ---- Load scenario data ----
mat_data <- readMat(file.path(DIR, "SSP_2080_2099_annual.mat"))
diff_mean_126 <- as.vector(mat_data$diff.mean.126)
diff_mean_245 <- as.vector(mat_data$diff.mean.245)
diff_mean_370 <- as.vector(mat_data$diff.mean.370)
diff_mean_585 <- as.vector(mat_data$diff.mean.585)
diff_std_126  <- as.vector(mat_data$diff.std.126)
diff_std_245  <- as.vector(mat_data$diff.std.245)
diff_std_370  <- as.vector(mat_data$diff.std.370)
diff_std_585  <- as.vector(mat_data$diff.std.585)

# ---- Color mapping ----
temp_bins <- c(0.6, 0.8, 1.0, 1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.4, 2.6, 2.8, 3.0)
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
# Create sf objects for annotation
anno_sf2 <- st_sf(label = "Annual SST Change\n SSP2-4.5 2080-2099\n Baseline Period 1990-2014",
                  geometry = st_sfc(
                    st_point(c(-105, 45))
                  ),
                  crs = 4326)

main_map <- basemap(
  limits = c(-140, -50, 40, 80),
  crs = 3978,
  land.col = "grey70",
  grid.col = "grey90",
  grid.size = 0.1
) +
  geom_sf(data = all_regions, aes(fill = temp_change), color = "black", size = 0.2, alpha = 1) +
  scale_fill_gradientn(
    colors = thermal_colors,
#    limits = c(min(temp_bins), max(temp_bins)),
#    breaks = temp_bins,
    limits = c(1.0, 4.5),
    breaks = c(1.5, 2.5, 3.5, 4.5),
    name = "SST\nChange (°C)",
    guide = guide_colorbar(barwidth=1, barheight=10, title.position="top")
  ) +
#  geom_text(
#    data = all_regions,
#    aes(x = xlon, y = ylat + 0.1, label = region_id),
#    size = 3, color = "white", fontface = "bold"
#  ) +
  geom_sf_text(data = all_regions,
               aes(label = region_id),
               size = 3,
               color = "white",
               fontface = "bold") +
  # Add season labels
  geom_sf_text(data = anno_sf2, aes(label = label), size = 3, fontface = "plain") +
  theme(
    legend.position = "right",
    legend.title = element_text(size=8),
    legend.text = element_text(size=6),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

# ---- Barplot for all scenarios (fixed) ----
library(tidyr) # for pivot_longer

# Prepare tidy data for plotting: one row per region+scenario
df <- data.frame(
  region = factor(paste0(1:10, "-", region_names), levels = paste0(1:10, "-", region_names)),
  mean_126 = diff_mean_126,
  mean_245 = diff_mean_245,
  mean_370 = diff_mean_370,
  mean_585 = diff_mean_585,
  std_126  = diff_std_126,
  std_245  = diff_std_245,
  std_370  = diff_std_370,
  std_585  = diff_std_585
)

barplot_data <- df %>%
  pivot_longer(
    cols = starts_with("mean_"),
    names_to = "scenario",
    values_to = "change"
  ) %>%
  mutate(
    std = case_when(
      scenario == "mean_126" ~ std_126,
      scenario == "mean_245" ~ std_245,
      scenario == "mean_370" ~ std_370,
      scenario == "mean_585" ~ std_585
    ),
    scenario = factor(scenario, levels = c("mean_126", "mean_245", "mean_370", "mean_585"),
                      labels = c("SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5"))
  )

bar_colors <- c("SSP1-2.6"="#CCCCCC", "SSP2-4.5"="#999999", "SSP3-7.0"="#666666", "SSP5-8.5"="#333333")

# Reverse the order of regions for barplot
barplot_data$region <- factor(barplot_data$region, levels = rev(levels(barplot_data$region)))

bar_chart <- ggplot(barplot_data, aes(x = region, y = change, fill = scenario)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7, color = "black", linewidth = 0.1) +
  geom_errorbar(
    aes(ymin = change - std, ymax = change + std),
    width = 0.3,
    position = position_dodge(width = 0.8),
    linewidth = 0.1
  ) +
  scale_fill_manual(values = bar_colors, name = "Scenario") +
  ylab("SST change (°C)") +
  xlab("") +
  scale_y_continuous(expand = c(0,0), limits = c(0, 10)) +
  theme_minimal(base_size = 8) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    legend.position = "right"
  )

# Now, when you combine with your map, the bars will be correct:
combined_plot <- plot_grid(
  main_map,
  bar_chart,
  ncol = 1,
  rel_heights = c(3, 1),
  align = "v"
)
print(combined_plot)
# ggsave("SSP_map.png", combined_plot, width=12, height=10, dpi=300)
# Uncomment to save the plot
ggsave("Figure7pt10d.png", combined_plot,
       width = 12, height = 10, dpi = 300)
ggsave("Figure7pt10d.svg", combined_plot,width = 12,
       height = 10, units = "in", device = "svg", scale = 0.5)
