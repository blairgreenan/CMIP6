clear
close all

addpath('/media/data/wangz/m_map/colorbar')
%colormap(cmocean('ballence'))



load SSP_2040_2059_annual.mat


DIR='./'

R1='polygon_gomss_subarea_gom.csv';


R2='polygon_ss_merged.csv'


R3='polygon_gsl_subarea_gsl_clipped.csv';
R4='polygon_ns_subarea_sns.csv';


R5='polygon_ns_merged.csv'

R6='polygon_ls_merged.csv';

R7='polygon_hb_subarea_hb.csv';
R8='polygon_bb_subarea_bb.csv';
R9='polygon_bcs_subarea_bcs.csv';


R10='polygon_ca_subarea_sbs.csv';



temp_diff=diff_mean_245;


T1='GoM';


T2='SS'

T3='GSL';
T4='SNS';

T5='NNS';


T6='LS'


T7='HB';
T8='BB';
T9='BCS';
T10='SBS';







RI1=[0.5 0.8];
RI2=[0.8 1.0];
RI3=[1.0 1.2];
RI4=[1.2 1.4];
RI5=[1.4 1.6];
RI6=[1.6 1.8]
RI7=[1.8 2.0]



Lmin=0.5;Lmax=2.0;
Mnum=7;
map=colormap(cmocean('thermal',Mnum+1))



axes('position',[0.90 0.3 0.02 0.65 ])
x=[0 1];
y=[Lmin:0.2:2.0];
z=[y(:) y(:)];
pcolor(x,y,z);shading flat;
set(gca,'xticklabel','','fontsize',8)
caxis([Lmin Lmax]);
set(gca, 'YAxisLocation', 'right');
%set(gca,'yticklabel','','fontsize',8)
title('SSP2-4.5 (^oC)')



axes('position',[0.05 0.3 0.80 0.65 ])
m_proj('lambert','long',[-150 -40],'lat',[40 80]);

m_grid('box','fancy','tickdir','in');

m_coast('patch',[0.7 0.7 0.7])

hold on
for n=1:10
subn=eval(['R',num2str(n)]);
filename=[DIR,subn];
data=load(filename);
lon=data(:,1);lat=data(:,2);

xlon=mean(lon(:));ylat=mean(lat(:));

if n==2  %SS
xlon=mean(lon(:))+2;ylat=mean(lat(:))-2;
end

if n==4  %SNS
xlon=mean(lon(:))+6;ylat=mean(lat(:))-2;
end

if n==5  %NNS
xlon=mean(lon(:))+4;ylat=mean(lat(:));
end

if n==6  %LS
xlon=mean(lon(:))+2;ylat=mean(lat(:));
end

if n==9   %BCS
xlon=mean(lon(:))-5;ylat=mean(lat(:))+1;
end
if n==3  %GSL
xlon=mean(lon(:))+0.8;ylat=mean(lat(:))+0.1;
end
if n==10   %BCS
xlon=-140;ylat=72;
end



 hold on
if (n==1 || n==3 || n==8 || n==7)
 m_plot(xlon,ylat+0.1,'ow','Markersize',9)
 m_text(xlon,ylat+0.1,num2str(n),...
          'HorizontalAlignment','center', 'VerticalAlignment','middle', 'fontsize', 2,'color',[1 1 1])
else
 m_plot(xlon,ylat+0.1,'ok','Markersize',9)
 m_text(xlon,ylat+0.1,num2str(n),...
          'HorizontalAlignment','center', 'VerticalAlignment','middle', 'fontsize', 2,'color',[0 0 0])

end

for m=1:Mnum
    tmp=~isnan(discretize(temp_diff(n),eval(['RI',num2str(m)])))
    if tmp
        m_patch(lon,lat,map(m,:));
    end
end
end





caxis([Lmin Lmax])



x1=[0.6:1:10];
x2=[0.8:1:10];
x3=[1:1:10];
x4=[1.2:1:10.8];


axes('position',[0.1 0.05 0.80 0.17])
h1=bar(x1,diff_mean_126,0.1,'FaceColor',[0.8 0.8 0.8],'EdgeColor',[0.8 0.8 0.8],'LineWidth',1);
hold on
for r=1:10
   tanghulu(x1(r),eval(['diff_mean_',num2str(126),'(r)']),eval(['diff_std_',num2str(126),'(r)']),[0.5 0.5 0.5],'o',2,0.05);         
end

hold on
h2=bar(x2,diff_mean_245,0.1,'FaceColor',[0.6 0.6 0.6],'EdgeColor',[0.6 0.6 0.6],'LineWidth',1);
hold on
for r=1:10
   tanghulu(x2(r),eval(['diff_mean_',num2str(245),'(r)']),eval(['diff_std_',num2str(245),'(r)']),[0.5 0.5 0.5],'o',2,0.05);         
end


h3=bar(x3,diff_mean_370,0.1,'FaceColor',[0.4 0.4 0.4],'EdgeColor',[0.4 0.4 0.4],'LineWidth',1);
hold on
for r=1:10
   tanghulu(x3(r),eval(['diff_mean_',num2str(370),'(r)']),eval(['diff_std_',num2str(370),'(r)']),[0.5 0.5 0.5],'o',2,0.05);         
end


h4=bar(x4,diff_mean_585,0.1,'FaceColor',[0.2 0.2 0.2],'EdgeColor',[0.2 0.2 0.2],'LineWidth',1);
hold on
for r=1:10
   tanghulu(x4(r),eval(['diff_mean_',num2str(585),'(r)']),eval(['diff_std_',num2str(585),'(r)']),[0.5 0.5 0.5],'o',2,0.05);         
end


xticks([1:1:13]);
xticklabels({['1-',T1],['2-',T2],['3-',T3],['4-',T4],['5-',T5],['6-',T6],['7-',T7],['8-',T8],['9-',T9],['10-',T10] })
xlim([0.2 10.8])
ylabel('T change(^oC)','fontsize',6)
grid on
ylim([0 3.8])
legend([h1 h2 h3 h4],'SSP1-2.6','SSP2-4.5','SSP3-7.0','SSP5-8.5','location','eastoutside')


