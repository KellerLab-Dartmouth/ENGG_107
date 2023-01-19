############################################
##  file: coin-example.R
##   R Example of coin flip game
##   - Play the game once and plot the distribution
##   - Repeat the game many times and plot the means
######################################################
##  authors: Ryan Sriver and Klaus Keller
##  copyright by the authors
##  distributed under the GNU general public license
##  https://www.gnu.org/licenses/gpl.html
##  no warranty (see license details at the link above)
##################################################
# version: 2, last changes: Jan. 22. 2012 by rls
# version: 3, last changes: Jan. 29 2015 by klaus keller
# version: 4, last changes: January 13 2023 by klaus keller
# contact: klaus.keller@dartmouth.edu
##################################################
# sources:
# - R help files accessed through R-studio for syntax
# - discussions with many students in the Risk Analysis classes at Penn State
# - discussions with Patrick Applegate, Prabgat Hedge, and Maggie Zarekarizi
####################################################
# how to run:
# - save the file in a directory
# - go to the directory with this file
# - open R
# - type 'source(coin-example.R)'
# - open the new pdf files to analyze the results
############################################
# problem definition:
# You flip a coin 500 times 
# Heads=1, Tails=0
# Question 1) What do you expect as a mean outcome and what do you get?
# Question 2) What is a 95% confidence interval for this mean outcome?
########################################################

#########
#Approaches (for students, for teaching):
# - draw the figures you expect in the design you want
# - Make a flow diagram of the method
# - review the R function “sample”
# - recycle old script components (with proper source acknowledgements)
# - write code and *test*
# - document
# - share back

###
# OK, let's get going...
###
# Clear any existing variables and plots. 
rm(list = ls())
graphics.off()

# define a seed for reproducibility
set.seed(314)

#Define parameters
outcome<-0:1  	# possible flip outcomes: T-0, H-1

### define how many flips per game and how many games to play
n_flips=500
n_games=100000

# do the calculation
### play the game once 
#######################
flips=sample(outcome,size=n_flips,replace=TRUE)
# random draws from the possible outcomes defined above of either 0 or 1
game_mean=mean(flips)


### repeat the game many times 
################################
game_mean=rep(NA,n_games) 
# fill the vector game_mean with NA, safe and *may* save compution time

# Use a for-loop to repeat the game many (n_games) times 
  for(i in 1:n_games) 
    {
	flips=sample(outcome,size=n_flips,replace=TRUE)
	game_mean[i]=mean(flips)
    }

## produce summary plots ##

### plot the resultant distribution for single game
pdf(file="plot1.pdf")  # write to pdf, define a pdf file to write to
hist(flips,col="blue",xlab="outcome")
abline(v=game_mean,col="blue",lty=2,lwd=2)
abline(v=0.5,col="black",lty=2,lwd=2)
legend("topleft", inset=0.2, c("game mean","prior expectation"),
       lwd=2, lty=c(2,2), col=c("blue","black"), cex=0.75)
dev.off()   # write the results to the pdf file

### plot the distribution of the means of repeated game with a smoothed pdf
pdf(file="plot2.pdf")  # write to pdf, define a pdf file to write to
d<-density(game_mean)
  plot(d, main="PDF of means from repeated game",lwd=2,lty=1,col="blue")
  abline(v=mean(game_mean),col="blue",lty=2,lwd=2)
  abline(v=0.5,col="black",lty=2,lwd=2)
  abline(v=quantile(game_mean,0.025), col="red", lty=2, lwd=2)
  abline(v=quantile(game_mean,0.975), col="red", lty=2, lwd=2)
  legend("topleft", c("game mean","prior expectation","95% confidence interval"),
           lwd=c(2,2,2), lty=c(2,2,2), col=c("blue","black","red"), cex=0.75)
dev.off()   # write the results to the pdf file

### plot histogram of outputs
pdf(file="histogram_repeated_game.pdf",10,6.17)
# questions: 
# 1) What are the two parameters at the end of the pdf call?
# 2) Why choose these two numbers?
# 3) Why use a histogram over a smoothed pdf?
# 4) Howq would you improve this figure?
# 
# write to pdf, define a pdf file to write to
hist(game_mean, main="Histogram of the mean game outcome",xlab="mean of the outcome")
abline(v=mean(game_mean),col="blue",lty=2,lwd=2)
abline(v=0.5,col="black",lty=2,lwd=2)
abline(v=quantile(game_mean,0.025), col="red", lty=2, lwd=2)
abline(v=quantile(game_mean,0.975), col="red", lty=2, lwd=2)
legend("topleft", c("game mean","prior expectation","95% confidence interval"),
       lwd=c(2,2,2), lty=c(2,2,2), col=c("blue","black","red"), cex=0.75)
dev.off()   # write the results to the pdf file


