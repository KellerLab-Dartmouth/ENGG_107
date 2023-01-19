# file: demo.growth.R
# demonstrates simple exponential growth
# author and copyright: klaus keller
# keller.klaus@gmail.com
# distributed under GNU general public license
# no warranty

# Problem (from homework Niels Keller, Gray's Wood SCASD)
# is it better to work for one month with 
# (a) one million dollar salary OR
# (b) one cent, day one, and double the pay for each subsequent day

# make clear ship

# Clear any existing variables and plots. 
rm(list = ls())
graphics.off()

# Set the working directory to where the files are stored. 
# this needs to be updated to fit the file system on your machine
setwd("~/Desktop/wip/teaching/risk_analysis-spring-2015/lab-sessions/labs-2015/lab_2")

# define variables
##################
# We look at one month with 30 days
ndays<-30;

# allocate memory to speed up
# fill enties with NA to catch some errors
pay = rep(NA, ndays);
sumpay = rep(NA, ndays);
day = rep(NA, ndays);

# set up initial values
pay[1]<-0.01;
sumpay[1]=pay[1]

# calculate sequence of values
for(i in 2:ndays) 
{
  day[i]=i;
  pay[i]=pay[i-1]*2;
  sumpay[i]=sumpay[i-1]+pay[i];
}

# make the plot
###############
# define height
h=5;
# calculate width by the golden ratio
w=h*1.618;

# open the pdf file for output and define the size
pdf('plot_growth.pdf', width = w, height = h);

# plot the data for one option as points
plot(day,sumpay, type="b", col="blue", xlab="Days worked (total)", 
     log="y",
     lty=1, lwd=3, ylab="total pay [Dollar]");
# add points to the same plot
points(day,day*0+1E6,type="l",lty=2, col="black",lwd=3)

# add a legend
legend(10,1e0, 
# places a legend at the appropriate place 
c("growing pay","constant pay (one month full work)"), 
# puts text in the legend 
lty=c(1,2), # gives the legend appropriate symbols (lines)
lwd=c(3,3),
col=c("blue","black")) 
# gives the legend lines the correct color and width

# close and save the pdf file
dev.off()

# have fun and speak like a Pirate ;-)
