############################################
##  file: stats.R
##   R Example of performing basic stats on sample data
##   - Reads in hourly sea-level anomalies at Port of LA
##   - Performs basic (empirical) statistical analysis on the data
##   - Test data for normality
######################################################
##  authors: Ryan Sriver and Klaus Keller
##  copyright by the authors
##  distributed under the GNU general public license
##  no warranty
##################################################
# version: 1, Feb.5, 2012
# version: 2, March 16 2015
##################################################
# how to run:
# - save the file in a directory
# - go to the directory with this file
# - open R
# - type 'source(stats.R)'
# - open the new pdf files to analyze the results
############################################


# read in the global data from Jevrejava
data=read.table("./hourly_data.ascii")
  time=data[,1]  # column 1
  sl_in=data[,2]   # column 2

sl=sl_in[sl_in!=9999]  # exclude missing values (9999)

### Calculate and plot histogram
pdf(file="plot1.pdf")  # write to pdf, define a pdf file to write to
histogram=hist(sl, plot=TRUE, col="lightblue", main=("Histogram"),
  xlab=("hourly sea level anomaly [mm]")
)
dev.off()

qlo=0.025 # define quantiles (95% range)
qhi=0.975

### Calculate and plot empirical probability density function (pdf)
d=density(sl)
pdf(file="plot2.pdf")  # write to pdf, define a pdf file to write to
par(mfcol=c(2,1))  # panel plot (2 rows, 1 column)
plot(d,main="Empirical PDF",
  xlab="hourly sea level anomaly [mm]",
  ylab="Density",
  xlim=c(500,3500)
)
polygon(d, col="lightblue", border="black")
# add reference lines to plot
  abline(v=mean(sl),lwd=3,col="red")
  abline(v=quantile(sl,qlo),lwd=3,col="red",lty="dashed")
  abline(v=quantile(sl,qhi),lwd=3,col="red",lty="dashed")

### Calculate and plot empirical cumulative distribution function (cdf)
cdf=ecdf(sl)
plot(cdf,main="Empirical CDF", col="black", lwd=3,
  xlab="hourly sea level anomalies [mm]",
  ylab="Probability",
  xlim=c(500,3500)
)
# add reference lines to plot
  abline(v=mean(sl),lwd=3,col="red")
  abline(v=quantile(sl,qlo),lwd=3,col="red",lty="dashed")
  abline(v=quantile(sl,qhi),lwd=3,col="red",lty="dashed")
  abline(h=0.5,lwd=1,col="black",lty="dashed")
dev.off()

###################################################
### Is the data normally distributed?
### We can visually compare the quantiles
### against theoretical normal distribution.
###################################################

sl_thin <- sample(sl, 5000, replace=F)  # thin data for easier plotting
pdf(file="plot3.pdf")  # write to pdf, define a pdf file to write to
par(mar=c(5,4.5,1,1)+0.1)
sl_norm=rnorm(5000, mean=mean(sl_thin), sd=sd(sl_thin))
qqplot(sl_norm, sl_thin, xlab="Estimated Normal Distribution",
       ylab="Empirical Distribution from Data")
abline(0,1)  # plot reference line
dev.off()

###################################################
### More formal test for normality  
###   Apply Shapiro-Wilk test.        
###   Null hypothesis is that data is normally
###   distributed. If p-value < alpha, then
###   we reject null hypothesis.
###	- i.e. distribution not normal
###	- typical alpha value ~ 0.05
###################################################

sl_test=shapiro.test(sl_thin)
# We need to thin the data to 5000 for the shapiro.test using sample function
  print(sl_test) # print test result to screen

### Compare to a theoretical normal distribution
norm_test=shapiro.test(sl_norm)
  print(norm_test) # print test result to screen 
