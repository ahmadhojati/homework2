setwd("H:/EEB697/homework2-master/homework2-master")

################ MSE Function ####################
MSE=function(y_hat,y)
{
  return(mean((y-y_hat)^2))
}
##################################################
#*[----------------------------------------------------]*#
#*[                        Q1                          ]*#
#*[----------------------------------------------------]*#

# ################## Seedling Survival vs Height amount #######################

seedling <- read.csv("SEEDLING_SURVIVAL.csv")                            # Reading the raw data
summary(seedling)                                                        # Summary of the data

dev.new()                                                                # Creating new window for plot
plot(seedling$survival~seedling$HEIGHT,col="red")                        # Plot raw data (seedling survival vs Height change)  
glm_height<- glm(seedling$survival~seedling$HEIGHT,family = "binomial")  # Computing the intercept and slope parameters
Intercept_height <- coef(glm_height)[1]                                  # Hieght Intercept
Slope_height <- coef(glm_height)[2]                                      # Slope of Linear fit 
curve(plogis(Intercept_height+Slope_height*x),add=T)                     # adding "plogis(a+b*x)" to raw data plot
plogis(Intercept_height)                                                 # The Baseline 
Slope_height/4                                                           # effect size, by 1 unit increase in height seedling suvrival increases 3.5% 
confint(glm_height)                                                      # intercept and slope confident interval

#*[ Note: According to confidence interval, the Intercept crosses the zero ]*#
#*[ and Binomial does not seem to be the proper deistribution for fitting  ]*#
#*[ to our data                                                            ]*#

# ################## Seedling Survival vs Light amount #######################

dev.new()                                                                # Creating new window for plot
plot(seedling$survival~seedling$LIGHT,col="blue")                        # Plot raw data (seedling survival vs Light change)
glm_light<- glm(seedling$survival~seedling$LIGHT,family = "binomial")    # Computing the intercept and slope parameters
Intercept_light <- coef(glm_light)[1]                                    # Light Intercept (a)
Slope_light <- coef(glm_light)[2]                                        # Slope of Linear fit (b)
curve(plogis(Intercept_light+Slope_light*x),add=T)                       # adding "plogis(a+b*x)" to raw data plot
plogis(Intercept_light)                                                  # The Baseline
Slope_light/4                                                            # effect size, by 1 unit increase in light seedling suvrival decreases 1.6% 
confint(glm_light)                                                       # intercept and slope confident interval

#*[ Note: According to confidence interval Binomial model for Light effect ]*#
#*[ on seedling survival seems to fit the data in a reasonble way          ]*#

#*[----------------------------------------------------]*#
#*[                        Q2                          ]*#
#*[----------------------------------------------------]*#

seeds <- read.csv("Seeds.csv")                                           # Reading the raw data
summary(seeds)                                                           # Summary of the data
head(seeds)                                                              # Header of the data
prop_data <- seeds$recruits/seeds$seeds                                  # Probability (proportional): the amount of recruits per seeds number
dev.new()                                                                # Creating new window for plot
plot(prop_data~seeds$seedlings,col="cyan")                               # Plot raw data (seedling vs probability)
response <- cbind(seeds$recruits, seeds$seeds-seeds$recruits)            # 2 columns matrix of success and failure amount in germinating
predictor <- seeds$seedlings                                             #
glm_seedlings <- glm(response~predictor,family="binomial")               # Computing the intercept and slope parameters
slope_seedlings <- coef(glm_seedlings)[2]                                # Slope (b) of Linear fit on probability
intercept_seedlings <- coef(glm_seedlings)[1]                            # Intercept (a) of linear fit on probability 
curve(plogis(intercept_seedlings+slope_seedlings*x),add=T)               # Adding "plogis(a+b*x)" to raw data plot
plogis(intercept_seedlings)                                              # The Baseline
slope_seedlings/4                                                        # Effect size, by 1 unit increase in total number of conspecific
                                                                         # seedlings the germinating decreases arround 30% 

#*[ Note:                                                                             ]*#
#*[ This result shows that the amount of conspecific seedlings has significant effect ]*#
#*[ of seedling germination which 1 unit increase in its amount results 30 percent    ]*#
#*[ decrease in germination chance                                                    ]*#
confint(glm_seedlings)                                                   # intercept and slope confident interval

#*[----------------------------------------------------]*#
#*[                        Q3                          ]*#
#*[----------------------------------------------------]*#

mosquito <- read.csv("mosquito_data.csv")                                 # Reading the raw data
summary(mosquito)                                                         # Summary of the data
head(mosquito)                                                            # Header of the data
prop_eggs <- mosquito$Emergent_adults/mosquito$Egg_Count                  # Probability (proportional): the amount of adaults emerged per egg number

dev.new()                                                                 # Creating new window for plot
plot(prop_eggs~mosquito$Detritus,col="magenta")                           # Plot raw data (Probability vs mosquito detritus)
curve(plogis(1.44-0.19*x-0.21*x^2+0.04*x^3),add=T, col="blue",lwd=3)      # Adding "plogis(polynomial)" to raw data plot
curve(plogis(10*x*exp(-2*x)),add=T,lwd=3)                                 # Adding "plogis(exponential)" to raw data plot

#*[ Note :                                                                       ]*#
#*[ In polynomial function the probability first decreases from 0 to 4 and then  ]*#
#*[ it increases for the detruits values more than                               ]*#
#*[ 4. However, in exponentioal function after reaching maximum probability      ]*#
#*[ between 0 and 1 detruits, it decreases and become fix at 0.5 probability.    ]*#
#*[ The polynomial distribution shows the and exponential distribution shows     ]*#
#*[ that by and exponential distribution shows that by increasing organic        ]*#
#*[ detruits more larva emerge as adult and as this increase continous           ]*#
#*[ the amount of adult mosquito decreases and after some level organic amounts  ]*#
#*[ in the water does not play any role in adult mosquito population.            ]*#
#*[ In polynomial function, the increase in organic detruits in water first      ]*#
#*[ results in less adult mosquito and the it make the water environment suitable]*#
#*[ for mosquito population increase.                                            ]*#

# Finding the Binomial Likelihood for both polynomial and exponential functions (this code  minimizes the negative log-likelihood )
Likelihood_exp <- -sum(dbinom(x=mosquito$Emergent_adults,size=mosquito$Egg_Count,
                              prob=plogis(10*mosquito$Detritus*exp(-2*mosquito$Detritus)),log=T))
print(Likelihood_exp)
Likelihood_poly <- -sum(dbinom(x=mosquito$Emergent_adults,size=mosquito$Egg_Count,
                               prob=plogis(1.44-.19*mosquito$Detritus-0.21*mosquito$Detritus^2+0.04*mosquito$Detritus^3),log=T))
print(Likelihood_poly)

#*[ Note :                                                                     ]*#
#*[ As the negative likelihood of the exponential is less than polynomial,     ]*#
#*[ it seems that the exponential function fits better the data                ]*#

#*[----------------------------------------------------]*#
#*[                        Q4                          ]*#
#*[----------------------------------------------------]*#

# ################## Binomial ##################
intercept=50                                                           # True Intercept Value                                           
slope=-0.5                                                             # True Slope value
sample_size=500                                                        # Sample size
score <- seq(from=55,to=145,length=sample_size)                        # IQ score
y.smp <- rbinom(n=sample_size,plogis(intercept+slope*score),size = 10) # Simulated data from Bionomial distribution
dev.new()                                                              # Creating new window for plot
hist(y.smp)                                                            # Creating histogram of simulated bionomial distribution data
dev.new()                                                              # Creating new window for plot
plot(y.smp~score)                                                      # plot raw data (graduated students vs IQ score)

sample_size=rep(seq(from=40,to=90),times=7)                            # Make random repetitive samples which is used in the following for Loop 
estimated_slope=rep(NA,times=length(sample_size))                      # Create an empty vector for pasting estimated slopes in it
p_val_bionom=rep(NA,times=length(sample_size))                         # Create an empty vector for pasting estimated P-values in it

for(j in 1:length(sample_size)){
  y=rbinom(n=sample_size[j],                                           # make random binomial distribution data for each sample
           prob=plogis(intercept+slope*
                         seq(from=55,to=145,
                             length=sample_size[j])),size=5)          
  response=cbind(y,5-y)                                                # generating response matrix with success and failure
  m1<-glm(response~seq(from=55,to=145,                                 # estimating the slope and intercept value per each sample
                       length=sample_size[j]),family="binomial")
  estimated_slope[j]=coef(m1)[2]                                       # Slope estimation
  p_val_bionom[j] <- summary(m1)$coefficients[2,4]                     # P-value estimation
}

dev.new()                                                              # Creating new window for plot
plot(estimated_slope~sample_size)                                      # Plot estimated slope vs sample size
abline(h=-0.5,col="red",lwd=2)                                         # Adding the real slope as a red line to the previous plot
Binomial_MSE <- MSE(estimated_slope,slope)                             # Estimating the result's Mean Square Error (MSE) 
print(Binomial_MSE)                                                    # Print the result's MSE

# ################## Gaussian ##################
variance = 69.25                                                       # True Variance Value 
slope = -2.3                                                           # True Slope value
Intercept = 60                                                         # True Intercept value
sample_size=500                                                        # Sample size
score <- seq(from=55,to=145,length=sample_size)                        # IQ score
y_norm <- rnorm(n=sample_size,mean=Intercept+                          # Simulated data from Normal distribution
                  slope*score,sqrt(variance))
dev.new()                                                              # Creating new window for plot
hist(y_norm)                                                           # Creating histogram of simulated Normal distribution data
dev.new()                                                              # Creating new window for plot
plot(y_norm~score)                                                     # plot raw data (graduated students vs IQ score)
sample_size=rep(seq(from=68,to=112),times=6)                           # Make random repetitive samples which is used in the following for Loop 
p_val_gauss=rep(NA,times=length(sample_size))                          # Create an empty vector for pasting estimated P-values in it
estimated_slope=rep(NA,times=length(sample_size))                      # Create an empty vector for pasting estimated slopes in it
for(j in 1:length(sample_size)){
  y<-rnorm(n=sample_size[j],                                           # make random Normal distribution data for each sample
           mean=Intercept+slope*
             seq(from=55,to=145,
                 length=sample_size[j]),sqrt(variance))
  m1<-glm(y~seq(from=55,to=145,
                length=sample_size[j]),family=gaussian)                # Estimating the slope and intercept value per each sample
  estimated_slope[j]=coef(m1)[2]                                       # Slope estimation
  p_val_gauss[j] <- summary(m1)$coefficients[2,4]                      # P-value estimation
}

dev.new()                                                              # Creating new window for plot
plot(estimated_slope~sample_size)                                      # Plot estimated slope vs sample size
abline(h=mean(estimated_slope),col="red",lwd=2)                        # Adding the real slope as a red line to the previous plot
Gaussian_MSE <- MSE(estimated_slope,slope)                             # Estimating the result's Mean Square Error (MSE)
print(Gaussian_MSE)                                                    # Print the result's MSE

#*[ Note:                                                                                     ]*#
#*[ The MSE value for normal destribution is smaller than the MSE in Bionomial                ]*#
#*[ In both examples all estimated P-values where less than 0.05                              ]*#
#*[ Statistical power is generally higher for continuous than discrete response variables,    ]*#
#*[ because the continous variables are more likely to be simulated with                      ]*#
#*[ generative models and then with smaller sample size we can achieve the reasonable estimate]*#

