############################################
##  file: bootstrap_slr.R
##  - R Example of bootstrapping correlated data
##  - Application to thermosteric sea level rise (SLR) projections 
##	- create random realizations of "natural variability"
##	- resample residuals (data - best fit) and simulate SLR time series
##	- preserve AR structure of the observations
##	- project new realizations to 2100
######################################################
##  Authors: 
##  Ryan Sriver (rsriver@psu.edu)
##  Klaus Keller (klaus@psu.edu)
##  copyright by authors
##  distributed under the GNU general public license
##  no warranty
##################################################
# versions: 
# 2, Feb.20, 2012
# 3, Feb. 16, 2015
##################################################


### Read in SLR data (domingues et al., 2008) ###
  slr = read.table("./dom_w.err.txt") 
  years.obs = as.vector(slr[12:54,1])
  slr.obs = as.vector(slr[12:54,2])
  err.obs= as.vector(slr[12:54,3])

nyears.obs=length(years.obs)  # number of years in observational time series

  err_pos=slr.obs+err.obs # observational uncertainty
  err_neg=slr.obs-err.obs

### Calculate polynomial best fit to domingues data  ###
  fit <- lm(slr.obs ~ years.obs + I(years.obs^2))  
# 2nd order polynomial ~ a + bx + cx^2
    print(fit)
  slr.fit_obs<-predict(fit)


### plot the observational time series and polynomial best fit
pdf(file="plot1.pdf")  # write to pdf, define a pdf file to write to
plot(years.obs, slr.obs, type="l", ylim=c(-10,20),main="Thermosteric SLR (700m)",xlab="Year",ylab="SLR [mm]",col="black",lwd=3)
  points(years.obs, err_pos, col="black", pch=20, cex=0.75)
  points(years.obs, err_neg, col="black", pch=20, cex=0.75)
  lines(years.obs, slr.fit_obs,col="gray",lwd=5)
    legend("topleft", c("Observations (domingues et al., 2008)","Polynomial Best Fit"), lwd=2, col=c("black","gray"))
dev.off()   # write the results to the pdf file


### Calculate Residuals during observed time series (data - polynomial fit)  ###
res <- slr.obs - slr.fit_obs

### Bootstrap the residuals ###
N=5000  # Number of bootstrap samples
white.boot = mat.or.vec(N, nyears.obs) # create matrix (nr,nc)
white.boot_sd = rep(NA,N)

for(i in 1:N) 
  {
    white.boot[i,1:nyears.obs] = sample(res,size=nyears.obs,replace=TRUE)
    white.boot_sd[i] = sd(white.boot[i,])
  }


### plot the residuals ###
pdf(file="plot2.pdf")  # write to pdf, define a pdf file to write to
plot(years.obs, res, type="l",main="Residuals",xlab="Year",ylim=c(-5,5), ylab="Observations - Best Fit [mm]",col="blue",lwd=2)

### add 3 random bootstrap samples to verify method ###
nexamp=3
nrand=sample(N,nexamp)
  for(i in 1:nexamp) {
    lines(years.obs, white.boot[nrand[i],1:nyears.obs], col="red", lwd=1)
  }
   
legend("topleft", c("Observations - Best Fit","Bootstrap Residuals"), lwd=2, col=c("blue","red"))
dev.off()


### We need to retain the auto-correlated structure of the residuals ###
### determine AR coefficients of the original residuals (data - polynomial fit)  ###
pdf(file="plot2a.pdf")  
# write to pdf, define a pdf file to write to
rho=rep(NA,3)
          ac <- acf(res, lag.max=5, plot=TRUE)  
# apply partial auto-correlation to determine correlation coefficients
           rho[1] <- ac$acf[ 1 ]
           rho[2] <- ac$acf[ 2 ]
           rho[3] <- ac$acf[ 3 ]
           rho[4] <- ac$acf[ 4 ]
           rho[5] <- ac$acf[ 5 ]
dev.off()

### create new residuals from bootstraps with best estimnate of 
# original AR structure
res.boot=mat.or.vec(N, nyears.obs) #(nr,nc)

  for(n in 1:N) {
    for(i in 2:nyears.obs) {
        res.boot[n,i] = rho[2]*res.boot[n,i-1] + rnorm(1,mean=0,sd=white.boot_sd[n])
    }
  }


### plot the new AUTOCORRELATED residuals ###
pdf(file="plot3.pdf")  # write to pdf, define a pdf file to write to
plot(years.obs, res, type="l",main="Residuals",xlab="Year",ylim=c(-5,5), ylab="Observations - Best Fit [mm]",col="blue",lwd=2)

### add 3 random bootstrap samples
nexamp=3
nrand=sample(N,nexamp)
  for(i in 1:nexamp) {
    lines(years.obs, res.boot[nrand[i],1:nyears.obs], col="red", lwd=1)
  }

legend("topleft", c("Observations - Best Fit","Correlated Bootstrap Residuals"), lwd=2, col=c("blue","red"))
dev.off()



### apply new residuals to polynomial fit
slr.boot=mat.or.vec(N, nyears.obs) #(nr,nc)

  for(i in 1:N) {
    slr.boot[i,]=slr.fit_obs+res.boot[i,]
  }


### plot observations and polynomial fit with superimposed autocorrelated bootstraps ###
pdf(file="plot4.pdf")  
# write to pdf, define a pdf file to write to
plot(years.obs, slr.obs, type="l", ylim=c(-10,20),main="Thermosteric SLR (700m)",xlab="Year",ylab="SLR [mm]",col="black",lwd=3)
  points(years.obs, err_pos, col="black", pch=20, cex=0.75)
  points(years.obs, err_neg, col="black", pch=20, cex=0.75)
  lines(years.obs, slr.fit_obs,col="gray",lwd=5)

nexamp=3
nrand=sample(N,nexamp)
  for(i in 1:nexamp) {
    lines(years.obs, slr.boot[nrand[i],1:nyears.obs], col="red", lwd=1)
  }
legend("topleft", c("Obs (Dom 2008)","Obs Best Fit","Boot Res"), lwd=2, col=c("black","gray","red"))
dev.off()


### Repeat previous analysis for new bootstrap time series 
###  and project to 2100 ###
years.mod=seq(1962,2100)  ### define time sequence
nyears.mod=length(years.mod)

### Project polynomial best fit to 2100 ###
  slr.fit_proj=mat.or.vec(1, nyears.mod) #(nr,nc)
    for(i in 1:nyears.mod) {
      slr.fit_proj[i]=fit$coefficients[1] + fit$coefficients[2]*years.mod[i] + fit$coefficients[3]*years.mod[i]^2
    }


### calculate polynomial coefficients for projections ###
boot.fit_coef=mat.or.vec(N, 3)
boot.fit=mat.or.vec(N, nyears.obs)
for(i in 1:N) {
  fit.new <- lm(slr.boot[i,] ~ years.obs + I(years.obs^2))
  boot.fit_coef[i,1]=fit.new$coefficients[1]
  boot.fit_coef[i,2]=fit.new$coefficients[2]
  boot.fit_coef[i,3]=fit.new$coefficients[3]
  boot.fit[i,]<-predict(fit.new)
  }

  fit.boot_proj=mat.or.vec(N, nyears.mod) #(nr,nc)
  for(n in 1:N) {
  for(i in 1:nyears.mod) {
      fit.boot_proj[n,i]=boot.fit_coef[n,1] + boot.fit_coef[n,2]*years.mod[i] + boot.fit_coef[n,3]*years.mod[i]^2
    }
  }

pdf(file="plot5.pdf")  # write to pdf, define a pdf file to write to
plot(years.mod, slr.fit_proj, type="l",main="Projected Bootstrap Fits",xlab="Year",ylab="SLR [mm]",col="black",lwd=1,ylim=c(-50,500))
  for(i in 1:N) {
    lines(years.mod, fit.boot_proj[i,], col="red", lwd=1)
  }
    lines(years.mod, slr.fit_proj, type="l", col="black", lwd=1)

legend("topleft", c("Obs Best Fit","Boot Best Fits"), lwd=2, col=c("black","red"))
dev.off()




### calculate projected residuals ###
res.boot_proj=mat.or.vec(N, nyears.mod) #(nr,nc)
slr.boot_proj=res.boot_proj


  for(n in 1:N) {
    for(i in 2:nyears.mod) {
        res.boot_proj[n,i] = rho[2]*res.boot_proj[n,i-1] + rnorm(1,mean=0,sd=white.boot_sd[n])
    }
  }



### superimpose residuals on polynomial fit projections ###
  for(i in 1:N) {
    slr.boot_proj[i,]=fit.boot_proj[i,]+res.boot_proj[i,]
  }

pdf(file="plot6.pdf")  # write to pdf, define a pdf file to write to
plot(years.mod, slr.boot_proj[1,], type="l",main="Bootstrap Time Series",xlab="Year",ylab="SLR [mm]",col="red",lwd=1,ylim=c(-50,500))
  for(i in 1:N) {
    lines(years.mod, slr.boot_proj[i,], col="red", lwd=1)                          
  }
    lines(years.obs, slr.obs, type="l", col="blue", lwd=2)

legend("topleft", c("Obs(Dom 2008) ","Boot Fits + Noise"), lwd=2, col=c("black","red"))
dev.off()



### plot the SLR PDF ###
pdf(file="plot7.pdf")  # write to pdf, define a pdf file to write to
prob_proj=mat.or.vec(N,1)
prob_proj=slr.boot_proj[,nyears.mod]
 pdf <- density(prob_proj)
 plot(pdf, main="2100 SLR PDF")
 polygon(pdf, col="red", border="black")
  abline(v=mean(prob_proj),lwd=1,col="black",lty=1)
  abline(v=quantile(prob_proj,0.025), col="black", lty=2)
  abline(v=quantile(prob_proj,0.975), col="black", lty=2)
dev.off()


