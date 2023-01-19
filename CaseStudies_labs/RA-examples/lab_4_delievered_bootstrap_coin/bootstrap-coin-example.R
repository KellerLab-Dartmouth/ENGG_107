##################################################
##  file: bootstrap-coin-example.R
##   R Example of coin flip analysis
###################################################
##  author(s): Klaus Keller (klaus@psu.edu)
##  copyright by the author(s)
##  distributed under the GNU general public license
##  no warranty
##################################################
# version: 1, last changes: Jan. 27. 2011 by kk
# version: 2, last changes: Feb 5, 2015, kk
##################################################
# how to run:
# - save the file in a directory
# - go to the directory with this file
# - open R
# - type 'source(bootstrap-coin-example.R)'
# - open the new pdf file(s) to analyze the results
############################################
# problem definition:
# you have then tosses of a coin
# Heads=1, Tails =0
# results are HTHHTHTTTT
# Q1: What is the best guess about the probability of H?
# Q2: How certain are you about thus best guess?
# Q3: How more confidence do you have in your best guess
#     compared to a null hypothesis of a fair coin?
#####################################################
# make clear ship
# Clear any existing variables and plots. 
rm(list = ls())
graphics.off()

# define the data (translation from problem definition)
data = c(1,0,1,1,0,1,0,0,0,0)

# how many data points do we have?

N.data = length(data)

# how many bootstrap samples do we want?
N.boot=1E5
# careful, this is a choice,
# test the robustness of the results
# to this choice


# define the matrix we want to fill
resamples = matrix(nrow=N.boot, ncol=N.data)
mean.boot = vector(length=N.boot)

# reset counters for specifiic outcomes 
# (hypotheses) we want to track / compare
n1=0
n2=0

# do the bootstrap sampling and analysis using a loop
for(i in 1:N.boot)
{
  bootstrap.samples=sample(data,size=N.data,replace=TRUE)
  # produce the bootstrap sample
  mean.boot[i]=mean(bootstrap.samples)
  # calculate the quantity of interest
   if(mean.boot[i] == 0.5) n1 = n1+1
   if(mean.boot[i] == 0.4) n2 = n2+1
  # calculate the freqencies for outcomes 0.5 and 0.4
  # note how we do not overwrite the results and use the loop counter to store the data
}

### plot the resuls 
# set the plot parameters for size
# define w
w=10;
# calc h by golden ratio
h=w/1.618


# open the pdf file for output and define the size
pdf('plot_1.pdf', width = w, height = h);
hist(mean.boot, breaks=seq(-0.5/N.data,1+0.5/N.data,by=1/N.data),
     col="gray",main="",xlab="sample outcome (10 tosses)")
# plot the empirical distribution of the bootstrap samples
#title("empirical distribution of the bootstrap samples")
# plots for pubs typically have no title
abline(v=mean(data), col="red")
abline(v=mean(mean.boot), col="blue", lty="dashed")
abline(v=0.4,col="green")
abline(v=0.5,col="black")
# annotate the plot
legend(x="topleft", legend=c("data mean","bootstrap mean","H=0.4","H=0.5"),
       col=c("red","blue","green","black"),
       lty=c("solid","dashed"))
dev.off()   # write the results to the pdf file

print(n1/n2)
# display the ratio of the outcomes of 0.5 to 0.4
