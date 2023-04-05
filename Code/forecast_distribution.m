
%This scripts generates the distribution of tornado forecast probabilities 
%by using the observed tradeoff between POD and FAR

clc;
close all;
rng(1100);
%set(gca,"fontname", 'Arial')
%Write down the name of your own project's folder below:
base_folder='C:/Tornado_warnings/Benefits_ProbWarnings/';

%Distribution assumes that the storm is there, so 10\% is the baseline
%probability
p0=0.1;
D=1.35;
N0=10000;
gridN=1001;
S0=normrnd(0,1,((1-p0)/p0)*N0,1);
S1=normrnd(D,1,N0,1);
S=[S0;S1];

posterior=p0*normpdf(S,D)./(p0*normpdf(S,D)+(1-p0)*normpdf(S,0));
%forecast=posterior(posterior>0.1);
FAR0=linspace(0,1,gridN);
POD0=1-normcdf(norminv(1-FAR0),D);
figure(1);

histogram(posterior, 'Normalization', 'pdf', 'FaceColor','#000080','FaceAlpha',0.8);
title("PDF, D'=1.35",'FontName', 'Arial', 'Position', [0.5, 12, 0]);
xlabel("Forecasted probability",'FontName', 'Arial');
fname=fullfile(base_folder, 'Graphs/pdf_forecast0.png');
set(gca,'YTick',[]);
xlim([0 1]);
%set(gca,'XTick',[0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0]);
set(gca,"fontname", 'Arial');
%saveas(figure(1),fname);
%print(figure(1), fname,'-dpng');
exportgraphics(figure(1), fname);



posterior_adj=posterior(posterior>0.05);
figure(2);
histogram(posterior_adj, 'Normalization', 'pdf');
title("Separation D'=1.35",'FontName', 'Arial', 'Position', [0.5, 10, 0]);
xlabel("Forecasted probability",'FontName', 'Arial');
fname=fullfile(base_folder, 'Graphs/pdf_forecast1.png');
set(gca,'YTick',[]);
xlim([0 1]);
%set(gca,'XTick',[0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0]);
set(gca,"fontname", 'Arial');
exportgraphics(figure(2), fname);

figure(3);
histogram(posterior, 'Normalization', 'cdf', 'FaceColor','#000080','FaceAlpha',0.8);
title("CDF, D'=1.35",'FontName', 'Arial', 'Position', [0.12, 0.9, 0]);
xlabel("Forecasted probability",'FontName', 'Arial');
ylabel("Probability",'FontName', 'Arial');
fname=fullfile(base_folder, 'Graphs/cdf_forecast0.png');
xlim([0 1]);
%set(gca,'XTick',[0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0]);
set(gca,"fontname", 'Arial');
exportgraphics(figure(3), fname);


stop;

