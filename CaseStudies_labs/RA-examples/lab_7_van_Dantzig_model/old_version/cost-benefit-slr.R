############################################
##  file: cost-benefit_slr.R
##   R Example of a Sea Level Risk Model
##   - Cost-Benefit analysis of sea wall heightening
##   - Based on: van Dantzig D (1956) Economic Decision Problems for flood prevention. 
##	  Econometrica 24:276-287
######################################################
##  authors: Ryan Sriver and Klaus Keller 
##  (rsriver@psu.edu, klaus@psu.edu)
##  copyright by the authors
##  distributed under the GNU general public license
##  no warranty
##################################################
# version: 2, last changes: March 31, 2012 by rls
##################################################
# how to run:
# - save the file in a directory
# - go to the directory with this file
# - open R
# - type 'source(cost-benefit-slr.R)'
# - open the new pdf files to analyze the results
############################################


# Define parameters as given in van Dantzig
#	- check with Table 4.1 in ref 2

# flood frequency in 1/a at zero height increase
p_zero_p=0.0038

# rate of exponential flood frequency decrease per m height increase
alpha_p=2.6

# costs of flooding in Gulders (include the "factor of two" from the ms
V_p=2e10

# effective discount rate in % per year, this is net of increase in value damages
delta_prime_p=0.02

# costs of heightening per m in gulders.
k_p=4.2e7

# ime horizon to the next revision of the dike (in years), from paper 
T=75

# subsidence rate in m/a, from the paper. Equivalent to 0.7 m per century.
subs_rate=0.01

# Example sea_level rise rate in m/a. Equivalent to 0.9 m per century
sea_level_rate=0.009

# time scale
beta_p=alpha_p*1


# parameters for the probabilistic analysis of SLR

# number of samples
N_SOW=3

# SL_rates in m/a
SL_rate_samples=c(0.003, 0.009, 0.015)


# probabilty of each SOW, assumed so far equally likely
p_SOW=c(1/N_SOW, 1/N_SOW, 1/N_SOW)


# perform simple risk and economic analysis

# case 1: without cosndieration of uncertainty

# determine optimal X: (eq. 6 in paper)
C=(100*p_zero_p*V_p*alpha_p)/((delta_prime_p*100-beta_p)*k_p) *
    (1-exp(-(delta_prime_p*100-beta_p)*T/100)) / 
    (1-exp(-delta_prime_p*100*T/100));

X_analytical=1/alpha_p*log(C)
print(X_analytical)


# case 2:  with uncertainty

# considerd range of Deike heigtening in meters
X=seq(0,10,by=0.05)

# considered time horison, in annual increments
time=seq(0,T,by=1)

# Define variables
 p_exceed=array(NA,dim=c(length(X)))
 costs=array(NA,dim=c(length(X)))
 NPV_expected_losses=array(NA,dim=c(length(X)))
 EV_p_exceed_transient=array(NA,dim=c(length(X)))
 Total_flood_frequency=array(NA,dim=c(length(X)))
 total_costs=array(NA,dim=c(length(X)))

 discount_factor=array(NA, dim=c(length(time)))
 effective_height=array(NA, dim=c(length(X),length(time)))
 p_exceed_transient=array(NA, dim=c(length(X),length(time)))
 NPV_costs_flooding=array(NA, dim=c(length(X),length(time)))

subsidence=subs_rate*time
sea_level_rise=sea_level_rate*time

for(i in 1:length(X)) {
   # analyze for each considered Deike heightening (eq. 1 in paper)
   p_exceed[i]= p_zero_p*exp(-alpha_p*X[i])
   
   for (j in 1:length(time)) {
	
	# analze for each year of the initial time period
	year =j-1
	
	#losses in the future are discounted at the annual net discount rate
	discount_factor[j]=1/(1+delta_prime_p)^year

	#The effective deike height decreases with subsidence and sea-level rise.
	effective_height[i,j]=X[i]-subsidence[j]-sea_level_rise[j]


	# For a stationary flooding frequency, we can evaluate the annual flooding
	#frequency with the old observeations and the new effective height.
	p_exceed_transient[i,j]= p_zero_p*exp(-alpha_p*effective_height[i,j])


	#The net present value of the losses per year are the product of the
	#frequency of flooding per year, the damages per flood, and the discount factor.
	NPV_costs_flooding[i,j]=p_exceed_transient[i,j]*V_p*discount_factor[j]
   }

   #The costs of building the dike increase linearly with respect to height.
   costs[i]=k_p*X[i]

   #The total discounted expected losses are the sum of the discounted expected annual losses.
   NPV_expected_losses[i]=sum(NPV_costs_flooding[i,])
   
   #The average annual flood frequency is the mean the annual flood frequencies.
   EV_p_exceed_transient[i]=mean(p_exceed_transient[i,])

   #The total flood frequency over the life-time of the project is the sum of the flood frequencies,
   #assuminG independence, as in the original paper
   Total_flood_frequency[i]=sum(p_exceed_transient[i,])

   #The total costs that depend on the deike height. Note that the fixed
   #costs of setting up the deike heightening as well as the effects of
   #deike height on costs beyond the time horizon are neglected
   total_costs[i]=costs[i]+NPV_expected_losses[i]

 }

print(X)
print(p_exceed)

print(total_costs)

# find the minimum cost
min_ind=seq(along=total_costs)[total_costs == min(total_costs)]
min_cost_X=X[min_ind]
  print(min_cost_X)


# find de-minimis risk defense
  de_minimis_risk=1e-6  # define minimum risk height

# find height increase corresponding to min cost de-mininis risk defense
  cost=total_costs[1]
  for(i in 1:length(X)) {
    if (Total_flood_frequency[i] < de_minimis_risk  && total_costs[i] < cost) {
      min_cost_de_minimis_X=X[i]
      cost=total_costs[i]
    }
  }

print(min_cost_de_minimis_X)


############################
### Plot up Results
############################

### Plot modeled flooding frequencies 
pdf(file="plot1.pdf")  # write to pdf, define a pdf file to write to
par(mfcol=c(2,1))  # panel plot (2 rows, 1 column)
plot(X,p_exceed,type="l",col="red",
    cex=1.5,ylim=c(0,5e-3), xlim=c(0,10), lwd=5,
    ylab="Flood frequency (1/a)",xlab="Height of high tide [m]"
    )

abline(v=3.7,lwd=3,col="black",lty="solid")

legend(x="topright",
       inset=c(0.1,0.1),
       legend=c("Flood frequency model","1953 flooding event"),
       col=c("red","black"),
       lty=c("solid","solid"),
       lwd=c(2)
       )

plot(X,p_exceed,type="l",col="red",
    cex=1.5,log="y",ylim=c(1e-15,1), xlim=c(0,10), lwd=5,
    ylab="Flood frequency (1/a)",xlab="Height of high tide [m]"
    )

abline(v=3.7,lwd=3,col="black",lty="solid")

dev.off()


### Plot discount factor
pdf(file="plot2.pdf")  # write to pdf, define a pdf file to write to
plot(time,discount_factor,type="l",col="black",
    cex=1.5,ylim=c(0.2,1), xlim=c(0,80), lwd=5,
    ylab="Discount Factor",xlab="Time"
    )

legend(x="topright",
       inset=c(0.1,0.1),
       legend=c("r=0.02"),
       col=c("black"),
       lty=c("solid"),
       lwd=c(2)
       )

dev.off()



### Plot expected flooding frequencies and possible height increases 
###   as determined by cost-benefit analyses
pdf(file="plot3.pdf")  # write to pdf, define a pdf file to write to
plot(X,Total_flood_frequency,type="l",col="red",lwd=3,
    cex=1.5,log="y",ylim=c(1e-15,1.), xlim=c(0,10),
    ylab="Expected Flood frequency",xlab="Height increase [m]"
    )

lines(X,EV_p_exceed_transient,col="blue",lwd=3)
abline(h=1e-6,lwd=3,col="black",lty="solid")
abline(v=X[min_ind],lwd=3,col="gray",lty="solid")
abline(v=min_cost_de_minimis_X,lwd=3,col="gray",lty="dashed")

legend(x="topright",
       inset=c(0.,0.),
       legend=c("Mean annual flood frequency","Total flood frequency","minimum risk level",
	"Height for minimum cost","Height for minimum risk"),
       col=c("blue","red","black","gray","gray"),
       lty=c("solid","solid","solid","solid","dashed"),
       lwd=c(2,2,2,2)
       )

dev.off()


### Results of cost-benefit analysis
pdf(file="plot4.pdf")  # write to pdf, define a pdf file to write to
plot(X,total_costs,type="l",lwd=3,col="red",xlim=c(1,6),ylim=c(0,1e9),
     ylab="Gulders",xlab="Height increase [m]"
     )

abline(v=X[min_ind],lwd=3,col="gray",lty="solid")
abline(v=min_cost_de_minimis_X,lwd=3,col="gray",lty="dashed")

legend(x="topright",
       inset=c(0.,0.),
       legend=c("Total costs",
        "Height for minimum cost","Height for minimum risk"),
       col=c("red","gray","gray"),
       lty=c("solid","solid","dashed"),
       lwd=c(2,2,2)
       )

dev.off()

