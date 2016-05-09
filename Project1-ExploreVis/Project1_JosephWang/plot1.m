clear all
%After downloading the time serie data from Yahoo Finance through R
%library(quantmod), we save the data into .csv files and then converted into
%Matlab data files in .mat 
%Time series data are loaded based on closing time on business days.
load XOM  %EXXON stock prices 
load CVX  %Chevron stock price
load INTC %Intel stock price
load AMD  %AMD stock price
load JPM  %J.P. Morgan stock price
load GS   %Goldman Sachs stock price

x=0:1:length(XOM(:,6))-1;
plot(x,XOM(:,6),'k') %Plot the sixth column of the Exxon data which is the adjusted stock price
hold on;
plot(x,CVX(:,6),'b');%Plot the Chevron data
hold on
plot(x,INTC(:,6),'r');%Plot the intel data
hold on
plot(x,AMD(:,6),'y') %Plot the AMD data
hold on;
plot(x,JPM(:,6),'m'); %Plot the JPM data 
hold on
plot(x,GS(:,6),'c'); %Plot the GS data
ylabel('Scaled Stock prices(dolloars)','fontsize',14,'fontweight','b');

%To observe better on the trend, we renormalize each stock
%prices based on its maxima price through the selected time series
figure
plot(x,XOM(:,6)/max(XOM(:,6)),'k')
hold on;
plot(x,CVX(:,6)/max(CVX(:,6)),'b');
hold on
plot(x,INTC(:,6)/max(INTC(:,6)),'r');
hold on
plot(x,AMD(:,6)/max(AMD(:,6)),'y')
hold on;
plot(x,JPM(:,6)/max(JPM(:,6)),'m');
hold on
plot(x,GS(:,6)/max(GS(:,6)),'c');
xlabel('Business Days','fontsize',14,'fontweight','b');
ylabel('Renormalized Stock prices(dolloars)','fontsize',14,'fontweight','b');

%By observing the trend, we do not expect the data is useful
%for statistical inference due to its non-normal distribution.
%Instead, what is more interesting is the "up and down" for the stock
%prices which is defined as the difference of the stock prices in adjacent 
%days, which can be calculated by diff function in MATLAB.
figure
diff_XOM=diff(XOM(:,6));
diff_CVX=diff(CVX(:,6));
diff_INTC=diff(INTC(:,6));
diff_AMD=diff(AMD(:,6));
diff_JPM=diff(JPM(:,6));
diff_GS=diff(GS(:,6));
xx=0:1:length(XOM(:,6))-2;
subplot(6,1,1)
plot(xx,diff_XOM,'k')
subplot(6,1,2)
plot(xx,diff_CVX,'b');
subplot(6,1,3)
plot(xx,diff_INTC,'r');
subplot(6,1,4)
plot(xx,diff_AMD,'y')
subplot(6,1,5)
plot(xx,diff_JPM,'m');
subplot(6,1,6)
plot(xx,diff_GS,'c');
xlabel('Business Days','fontsize',14,'fontweight','b')
ylabel('Stock Price Difference Daily','fontsize',14,'fontweight','b')

%Histograms showing the normal distributed stock price difference 
subplot(1,3,1)
hist(diff_XOM+diff_CVX,100,'b')
ylabel('Counts in 100 bins','fontsize',14,'fontweight','b')
subplot(1,3,2)
hist(diff_INTC+diff_AMD,100,'r')
xlabel('Stock Price Difference Daily for semiconductor sector ','fontsize',14,'fontweight','b')
ylabel('Counts in 100 bins','fontsize',14,'fontweight','b')
subplot(1,3,3)
hist(diff_JPM+diff_GS,100,'g')

ylabel('Counts in 100 bins','fontsize',14,'fontweight','b')



%Sacatter plot between companies
%figure
%plot(diff_INTC,diff_XOM,'O')
%xlabel('INTC');ylabel('XOM')
%figure
%plot(diff_INTC,diff_CVX,'*')
%xlabel('INTC');ylabel('CVX')
%figure
%plot(diff_AMD,diff_XOM,'p')
%xlabel('AMD');ylabel('XOM')
%figure
%plot(diff_AMD,diff_CVX,'+')
%xlabel('AMD');ylabel('CVX')
%hold on;

%Plot scatter plots for different sectors
%Stock prices from the same industrial sectors are added together
subplot(1,3,1)
plot(diff_AMD+diff_INTC,diff_JPM+diff_GS,'.')
subplot(1,3,2)
plot(diff_JPM+diff_GS,diff_XOM+diff_CVX,'.')
subplot(1,3,3)
plot(diff_AMD+diff_INTC,diff_XOM+diff_CVX,'.')

%Calculation of correlation between companies and sectors
%X=[diff_AMD diff_INTC diff_JPM diff_GS diff_XOM diff_CVX];
%[correlation_com,pval_com]=corr(X);

%Calculation of correlation matrix and p values
Y=[diff_AMD+diff_INTC diff_JPM+diff_GS  diff_XOM+diff_CVX];
[correlation_sec,pval_sec] = corr(Y);

%Here we only care about the correlation between the signs of the diffference of stocks
%What will be the probability of stocks in one sector goes up or down next
%day and the stocks in another sectors also goes up or down 
%I found almost that almost 66 percent of the time this occured. 
%Y1=diff_AMD+diff_INTC;
%Y2=diff_JPM+diff_GS;
%Y3=diff_XOM+diff_CVX;
%Only catch the sign
%for j=1:length(Y1)
%    if Y1(j)>0
%        Y1(j)=1;
%    else
%        Y1(j)=-1;
%    end
%    if Y2(j)>0
%        Y2(j)=1;
%    else
%        Y2(j)=-1;
%    end
    
%     if Y3(j)>0
%        Y3(j)=1;
%    else
%        Y3(j)=-1;
%    end   
%end
%Count the number of days both stocks are all up or down
%N1=0;
%N2=0;
%N3=0;
%for j=1:length(Y1)
%    if Y1(j)*Y2(j)>0
%        N1=N1+1;
%    end   
%    if Y1(j)*Y3(j)>0
%        N2=N2+1;
%    end   
%    if Y2(j)*Y3(j)>0
%        N3=N3+1;
%    end   
    
%end
%P1=N1/length(Y1);
%P2=N2/length(Y2);
%P3=N3/length(Y3);































