
%This scripts generates the distribution of tornado forecast probabilities 
%by using the observed tradeoff between POD and FAR

clc;
close all;
rng(1100);
%set(gca,"fontname", 'Arial')
%Write down the name of your own project's folder below:
base_folder='C:/Tornado_warnings/Benefits_Ext_TornadoWarnings/';

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
fname=fullfile(base_folder, 'Graphs/cdf_forecast0.png');
xlim([0 1]);
%set(gca,'XTick',[0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0]);
set(gca,"fontname", 'Arial');
exportgraphics(figure(3), fname);


fuck;


hist=histogram(posterior, 'Normalization', 'pdf');


[N, edges]=histcounts(posterior,gridN);

%[h,stats] = cdfplot(posterior);

probs=linspace(0,1,gridN);
POD=zeros(size(probs));
FAR=zeros(size(probs));

post_pdf=(p0/N0)*N;

for i=1:gridN
    threschold=probs(i);
    POD(i)=(1/p0)*sum(post_pdf.*probs.*(probs>threschold));
    FAR(i)=(1/(1-p0))*sum(post_pdf.*(1-probs).*(probs>threschold));
end



figure(2);
plot(FAR, POD);
hold on;
plot(FAR0, POD0);
axis([0 1 0 1]);
title("ROC diagram (D'=1.35)");
xlabel("Probability of false detection");
ylabel("Probability of detection");
legend('Prob. forecast based', 'Distribution-based')
hold off;
fname=fullfile(base_folder, 'Graphs/ROC0.png');
saveas(figure(2),fname);
%plot(probs,FAR);


figure(3);
cdfplot(posterior);
title("Cumulative Distribution of Probabilistic Forecasts (D'=1.35)");
xlabel("Forecasted probability");
ylabel("Probability");
fname=fullfile(base_folder, 'Graphs/cdf_forecast0.png');
saveas(figure(3),fname);
%histogram(forecast);
%cdfplot(forecast);

posterior=sort(posterior,1);
K=length(posterior);
disp(K);

%forecasts with probability of less than 5%
LB=0.05;
no_forecasts=posterior(posterior<LB);
forecasts=posterior(posterior>=LB);
K=length(forecasts);
prob_no=length(no_forecasts)/length(posterior);
disp(prob_no);
disp(mean(no_forecasts));

forecast_probs=zeros(6,3);
forecast_probs(:,1)=[0.05;0.1;0.2;0.35; 0.6; 1];
disp(forecast_probs);
lev=2;
Nt=0;
prob_sum=0;
for j=1:K
    prob_sum=prob_sum+forecasts(j,1);
    Nt=Nt+1;
    average=prob_sum/Nt;
    if (lev<6)
        if (average>forecast_probs(lev,1))
            disp(lev);
            disp(average);
            disp(Nt);
            disp(j);
            forecast_probs(lev,2)=average;
            forecast_probs(lev,3)=Nt/K;
            Nt=0;
            prob_sum=0;
            lev=lev+1;
        end
    end
end

forecast_probs(lev,2)=average;
forecast_probs(lev,3)=Nt/K;

disp(forecast_probs);

forecast_probs_range=zeros(5,4);
forecast_probs_range(:,1:2)=[0 0.15; 0.15 0.3; 0.3 0.55; 0.55 0.7; 0.7 1];
nintervals=size(forecast_probs_range,1);

for j=1:nintervals
    sel_forecasts=posterior((posterior>=forecast_probs_range(j,1))&(posterior<forecast_probs_range(j,2)));
    forecast_probs_range(j,3)=mean(sel_forecasts);
    forecast_probs_range(j,4)=length(sel_forecasts)/length(posterior);
end
disp(forecast_probs_range);

disp(forecast_probs_range(:,3)'*forecast_probs_range(:,4));
