function []=tanghulu(x,y,std,color,marker,markersize,lr)


plot(x,y,marker,...
                       'MarkerEdgeColor',color,...
                       'MarkerFaceColor',color,...
                       'MarkerSize',markersize)
                   
hold on
plot([x,x],[y y+std],'-','color',color);
plot([x-lr,x+lr],[y+std, y+std],'-','color',color);
plot([x,x],[y y-std],'-','color',color);
plot([x-lr,x+lr],[y-std, y-std],'-','color',color);
