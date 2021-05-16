theta <- 0.5 # this is a fair coin
Ntoss <- 10

flips <- rbinom(n = Ntoss, 
                size = 1, 
                prob = theta)
                
Nheads = length(flips[flips==1])
thetaTrial = Nheads / Ntoss
thetaTrial


#####################

theta <- 0.5 # this is a fair coin
Ntoss <- 500
flips <- rbinom(n = Ntoss, size = 1, prob = theta)

trialTheta = c()
nHead = 0 # number of heads
for (i in 1:Ntoss) {	                
	# Cumulative number of heads at step i
	nHead = nHead + flips[i]
  # Compute the running proportion of heads	
	trialTheta = c(trialTheta, nHead / i)
	# Print proportion of heads	at step i
	print(paste0("Theta after ", i, " trials is ", trialTheta[i]))
}

setEPS()
postscript("Fig2.2.eps",width=9.5,height=6.5)

# Graph the running proportion
plot( 1:Ntoss , trialTheta , type="o" , log="x" , col="skyblue" , xlim=c(1,Ntoss) , ylim=c(0.0,1.0) , cex.axis=1.5 ,
xlab="Flip Number" , ylab="Proportion Heads" , cex.lab=1.5 , main="Running Proportion of Heads" , cex.main=1.5, lwd=3 )

# Plot a dotted horizontal reference line at theta
abline( h=theta , lty="dotted" )
text( 1 , theta, theta, adj=c(1,1.2) , cex=1.3 )

dev.off()

# Test likelihood
trials <- c(1,0,1,1,0,1,0,0,0,0,1,0,0,1,1,0,1,1,1,1)

nToss = length(trials)
nHead = length(trials[trials==1])
# generate the bag of coins
theta = seq(0, 1, by=0.01)
likelihood = c()
for (i in 1:length(theta)) {	                
  # Compute likelihood of coin
	likelihood = c(likelihood, theta[i]^nHead * (1-theta[i])^(nToss-nHead) )
	# Print proportion of heads	at step i
	print(paste0("P(D | theta=", theta[i], ") = ", likelihood[i]))
}

setEPS()
postscript("Fig2.3.eps",width=9.5,height=6.5)

plot(theta, likelihood, xlab=expression(theta), ylab=expression(paste("P(D | ", theta, ")")), type ="l", col="blue", xaxt="none", yaxt="none", lwd=3)
axis(side=1, at=seq(0,1,by=0.1))
abline( v=max(likelihood) , lty="dotted" )

dev.off()



# Test likelihood
trials <- c(1,0,1,1,0,1,0,0,0,0,1,0,0,1,1,0,1,1,1,1)

maxLikelihood = 0
maxLikelihoodTheta = 0

nToss = length(trials)
nHead = length(trials[trials==1])
# generate the bag of coins
theta = seq(0, 1, by=0.01)
likelihood = c()
for (i in 1:length(theta)) {	                
  # Compute likelihood of coin
	likelihood = c(likelihood, theta[i]^nHead * (1-theta[i])^(nToss-nHead) )
	# Print proportion of heads	at step i
	print(paste0("P(D | theta=", theta[i], ") = ", likelihood[i]))
	if (maxLikelihood < likelihood[i])
	{
		maxLikelihood = likelihood[i]
		maxLikelihoodTheta = theta[i]
	}
}

setEPS()
postscript("4.3LikelihoodTest.eps",width=9.5,height=6.5)

plot(theta, likelihood, xlab=expression(theta), ylab=expression(paste("P(D | ", theta, ")")), type ="l", col="blue", xaxt="none", yaxt="none", lwd=3)
axis(side=1, at=seq(0,1,by=0.1))
abline( v=maxLikelihoodTheta, lty="dotted", lwd=3)

dev.off()


###################
theta <- 0.5 # this is a fair coin
Ntrials <- 10000
Ntoss = 20
# Simulate 10000 trials
flipseq = c(1,0,1,0,0,0,1,0,0,0,1,0,0,1,1,0,1,1,1,1)
count = 0
for (i in 1:Ntrials) {
	flips <- rbinom(n = Ntoss, size = 1, prob = theta)
	if (all(flips == flipseq))
	  count = count + 1
}

print(count)


x <- seq(from = 0, to = 1, by = 0.01)
prior <- dnorm(x, mean = 0.5, sd = 0.5)
plot(x, prior, xlab=expression(theta), ylab=expression(paste("P(", theta, ")")), type = "l", col = "red", xaxt="none", lwd=3)
axis(side=1, at=seq(0,1,by=0.1))

p = seq(0,1, length=100)
plot(p, dbeta(p, 900, 100), ylab="density", type ="l", col=4, xaxt="none")
axis(side=1, at=seq(0,1,by=0.1))

p = seq(0,1, length=100)
plot(p, dbeta(p, 1, 1), xlab=expression(theta), ylab=expression(paste("P(", theta, ")")), type ="l", col="red", xaxt="none", lwd=3)
axis(side=1, at=seq(0,1,by=0.1))

p = seq(0,1, length=100)
plot(p, dbeta(p, 6, 5), xlab=expression(theta), ylab=expression(paste("P(D | ", theta, ")")), type ="l", col="blue", xaxt="none", lwd=3)
axis(side=1, at=seq(0,1,by=0.1))

p = seq(0,1, length=100)
plot(p, dbeta(p, 4.5, 6), xlab=expression(theta), ylab=expression(paste("P(", theta, " | D)")), type ="l", col="green", xaxt="none", lwd=3)
axis(side=1, at=seq(0,1,by=0.1))

p = seq(0,1, length=100)
prior = dbeta(p, 1, 1)
likelihood = dbeta(p, 6, 5)
posterior = dbeta(p, 6, 5.5)
plot(p, posterior, type = "l", col = "green", ylab="", xlab=expression(theta), lwd=3)
lines(p, likelihood , type = "l", col = "blue", lwd=3)
lines(p, prior, type = "l", col = "red", lwd=3)
legend(0.8,2.5, c("prior", "likelihood", "posterior"),lty=c(1,1,1),lwd=c(3,3,3),col=c(2,1,3))

p = seq(0,1, length=100)
prior = dbeta(p, 5, 5)
likelihood = dbeta(p, 6, 5)
posterior = dbeta(p, 6, 5.5)
plot(p, posterior, type = "l", col = "green", ylab="", xlab=expression(theta))
lines(p, prior, type = "l", col = "red")
lines(p, likelihood , type = "l", col = "blue")
legend(0.8,2.5, c("prior", "likelihood", "posterior"),lty=c(1,1,1),col=c(2,1,3))

#############
library(rstan)

# The Stan model as a string.
model_string <- "
// Here we define the data we are going to pass into the model
data {
  int<lower=0> N; // Number of trials
  int<lower=0,upper=1> y[N]; // Sample of N flips (heads=1, tails=0)
}

// Here we define what 'unknowns' aka parameters we have.
parameters {
  real<lower=0, upper=1> theta;
}

// The generative model
model {
  theta ~ beta(1, 1);
  for (n in 1:N)
    y[n] ~ bernoulli(theta);
}
"

N = 20       # Specify the total number of flips, denoted N.
theta = 0.5  # Specify underlying probability of heads.
data_list <- c(1,0,1,1,0,1,0,0,0,0,1,0,0,1,1,0,1,1,1,1)
data <- list(N=N, y=data_list)

# Compiling and producing posterior samples from the model.
stan_samples <- stan(model_code = model_string, data = data, warmup=2000, iter = 10000)

# Export the samples to a data.frame for easier handling.
posterior <- as.data.frame(stan_samples)

library(tidyverse)
library(HDInterval)
library(ggridges)

#mcmc = as.matrix(stan_samples)

## plot density curve with qplot and mark 95% hdi
ggplot(posterior, aes(x = theta, y = 0, fill = stat(quantile), size = 1)) + 
  geom_density_ridges_gradient(quantile_lines = TRUE, quantile_fun = hdi, vline_linetype = 2, size = 1) +
  scale_fill_manual(values = c("transparent", "lightblue", "transparent"), guide = "none") +
  annotate(geom="segment", x = 0.344, xend = 0.747, y = 0, yend = 0, colour = "black", size=1) +
  annotate(geom="label", x=0.344, y=0.2, label="0.344", color="black", fill = "white") +
  annotate(geom="label", x=0.747, y=0.2, label="0.747", color="black", fill = "white") +
  annotate(geom="text", x=0.55, y=0.3, label="95% HPDI", color="black", size=6) +
  #geom_label(aes(x = 0.344, y = 0.5, label = "3.44"), fill = "white") +
  scale_x_continuous(breaks = seq(0.2, 0.8, by = 0.1)) +
  xlab("theta") +
  theme_bw()



# Plotting and summarizing the posterior distribution
stan_samples
traceplot(stan_samples)
plot(stan_samples)

# Extract parameter theta to plot
theta_draw <- extract(stan_samples)$theta

# Plot theta histogram
ptheta_df <- data.frame(list(theta = theta_draw));
plot <-
  ggplot(ptheta_df, aes(x = theta)) +
  geom_histogram(bins=50, color = "gray") +
  stat_function(fun = dnorm, args = list(mean = mean(ptheta_df$theta), sd = sd(ptheta_df$theta)))
print(plot)

bayesplot::mcmc_areas(
  posterior, 
  pars = c("theta"),
  prob = 0.8, # 80% intervals
  prob_outer = 0.99, # 99%
  point_est = "mean"
)


############################### Chapter 5 - A/B Test
library(rstan)

# The Stan model as a string.
model_string <- "
data {
  // Number of tourists
  int nChina;
  int nKorea;
  // Number of infected
  int infectedChina;
  int infectedKorea;
}

parameters {
  real<lower=0, upper=1> rateChina;
  real<lower=0, upper=1> rateKorea;
}

model {
  rateChina ~ uniform(0, 1);
  rateKorea ~ uniform(0, 1);
  infectedChina ~ binomial(nChina, rateChina);
  infectedKorea ~ binomial(nKorea, rateKorea); 
}

generated quantities {
  real rate_diff;
  rate_diff = rateChina - rateKorea;
}
"

data_list <- list(nChina = 190, nKorea = 170, infectedChina = 26, infectedKorea = 18)

# Compiling and producing posterior samples from the model.
stan_samples <- stan(model_code = model_string, data = data_list)


# Export the samples to a data.frame for easier handling.
posterior <- as.data.frame(stan_samples)

bvl_plotParams(stan_samples)

plotDens(stan2coda(stan_samples))

bayesplot::mcmc_intervals(posterior, pars = c("rateChina", "rateKorea", "rate_diff"), point_est = "mean", prob = 0.8, prob_outer = 0.95, color_scheme = "blue")

bayesplot::mcmc_areas(
  posterior, 
  pars = c("rate_diff"),
  prob = 0.8, # 80% intervals
  prob_outer = 0.99, # 99%
  point_est = "mean"
)


library(tidyverse)
library(HDInterval)
library(ggridges)

#mcmc = as.matrix(stan_samples)

## plot density curve with qplot and mark 95% hdi
ggplot(posterior, aes(x = rate_diff, y = 0, fill = stat(quantile))) + 
  geom_density_ridges_gradient(quantile_lines = TRUE, quantile_fun = hdi, vline_linetype = 2) +
  scale_fill_manual(values = c("transparent", "lightblue", "transparent"), guide = "none") +
  theme_bw()


a_quant = quantile(posterior$rate_diff,c(0.025, 0.50, 0.975))
	
## plot density curve with qplot and mark 97.5% hdi
ggplot(posterior, aes(x = rate_diff, y = 0, fill = stat(quantile, quantiles=c(0.05, 0.50, 0.95)), size = 1)) + 
  geom_density_ridges_gradient(quantile_lines = TRUE, quantile_fun = hdi, vline_linetype = 2, size = 1) +
  scale_fill_manual(values = c("transparent", "lightblue", "transparent"), guide = "none") +
  annotate(geom="segment", x = a_quant[1], xend = a_quant[3], y = 0, yend = 0, colour = "black", size=1) +
  annotate(geom="label", x=a_quant[1], y=0.1, label=round(a_quant[1],3), color="black", fill = "white") +
  annotate(geom="label", x=a_quant[3], y=0.1, label=round(a_quant[3],3), color="black", fill = "white") +
  annotate(geom="text", x=a_quant[2], y=1, label="97.5% HPDI", color="black", size=6) +
  #scale_x_continuous(breaks = seq(0.2, 0.8, by = 0.1)) +
  theme_bw()

par(lwd=2)
hist(rateChina, lwd=2)
hist(rateKorea, lwd=2)

# calculating the estimated posterior costs:
# China cost of 200 (transprtation) + 1200 (the cost per treament) add
# Korea cost of 40 (transprtation) + 1500 (the cost per treament) add

costChina <- 200 + posterior$rateChina * 1200 
costKorea <- 40 + posterior$rateKorea * 1500 

par(lwd=2)
hist(costChina, lwd=2)
hist(costKorea, lwd=2)

par(lwd=2)
hist(costChina - costKorea, lwd=2)
est_cost_diff <- mean(costChina - costKorea)
abline(v = est_cost_diff, col = "red", lwd =2)



############## Linear
data <- mtcars
fit <- lm(mpg ~ hp, data = data)
summary(fit)  # Report the results

data$predicted <- predict(fit)   # Save the predicted values
data$residuals <- residuals(fit) # Save the residual values

library(ggplot2)
ggplot(data, aes(x = hp, y = mpg)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +  # Plot regression slope
  geom_segment(aes(xend = hp, yend = predicted), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = predicted), shape = 1) +
  theme_bw()  # Add theme for cleaner look


#############
ds <- cars
fit <- lm(dist ~ speed, data = ds)
summary(fit)  # Report the results

library(ggplot2)
ggplot(ds, aes(x = speed, y = dist)) +  # Set up canvas with outcome variable on y-axis
  geom_point()  # Plot the actual points

ds$predicted <- predict(fit)   # Save the predicted values
ds$residuals <- residuals(fit) # Save the residual values

ggplot(ds, aes(x = dist, y = speed)) +
  geom_point() +
  geom_point(aes(y = predicted), shape = 1)  # Add the predicted values

library(ggplot2)
ggplot(ds, aes(x = speed, y = dist)) +
  geom_abline(intercept=fit$coefficients[1],slope=fit$coefficients[2],colour = "blue", size=1) +
  geom_segment(aes(xend = speed, yend = predicted), alpha = .2) +  # alpha to fade lines
  geom_point() +
  geom_point(aes(y = predicted), shape = 15, color="red") +
  theme_bw()  # Add theme for cleaner look

summary(fit)$r.squared

######  

library(viridis)
postsig <- rstan::extract(fit@stanfit, pars = c("a_dist","b_speed_dist"))
ggplot(data = cars, 
       aes(x = speed, y= dist)) + 
  xlim(0, max(cars$speed)) +
  ylim(0, max(cars$dist)) +
  ylab("dist") +
  xlab("speed") +
  theme_minimal(base_size=15) +
  theme(axis.text.x = element_text(size=15), axis.text.y = element_text(size=15), plot.margin = margin(0, 0, 0, 0)) +
  geom_abline(aes(intercept = mean(postsig$a_dist), slope = postsig$b_speed_dist), as.data.frame(postsig$b_speed_dist), 
                alpha = 0.05, color = "gray50") +  
  geom_point(shape=1, color=2) +
  geom_abline(intercept=mean(postsig$a_dist),slope=mean(postsig$b_speed_dist),colour = "blue", size=1) +
	geom_vline(xintercept = 0) +
	geom_hline(yintercept = 0)
	
ggplot(data = cars, 
       aes(x = speed, y= dist)) + 
  xlim(0, max(cars$speed)) +
  ylim(0, max(cars$dist)) +
  ylab("dist") +
  xlab("speed") +
  theme_minimal(base_size=15) +
  theme(axis.text.x = element_text(size=15), axis.text.y = element_text(size=15), plot.margin = margin(0, 0, 0, 0)) +
  geom_abline(aes(intercept = postsig$a_dist, slope = mean(postsig$b_speed_dist)), as.data.frame(postsig$a_dist), 
                alpha = 0.05, color = "gray50") +  
  geom_point(shape=1, color=2) +
  geom_abline(intercept=mean(postsig$a_dist),slope=mean(postsig$b_speed_dist),colour = "blue", size=1) +
	geom_vline(xintercept = 0) +
	geom_hline(yintercept = 0)
	
######### Chapter 5
stem <- read.csv(file="/Users/Shared/Previously Relocated Items/Security/Statistics/STEM/STEM_model1.csv",header=T)

cols <- c("aps45", "timesci", "sex", "gradeid", "school")
stem<-stem[ , names(stem) %in% cols]

stem6 = stem[stem$gradeid ==6,]
stem7 = stem[stem$gradeid ==7,]
stem8 = stem[stem$gradeid ==8,]
stem9 = stem[stem$gradeid ==9,]
fit6 <- lm(aps45 ~ timesci, data = stem6)
fit7 <- lm(aps45id ~ timesci, data = stem7)
fit8 <- lm(aps45id ~ timesci, data = stem8)
fit9 <- lm(aps45id ~ timesci, data = stem9)
summary(fit6)  # Report the results
summary(fit7)  # Report the results
summary(fit8)  # Report the results
summary(fit9)  # Report the results

library(ggplot2)
ggplot(stem, aes(x = timesci, y = aps45)) +
  #geom_abline(intercept=fit6$coefficients[1],slope=fit6$coefficients[2],colour = "blue", size=1) +
  stat_smooth(method = "lm", se = FALSE) +
  geom_point() +
  scale_x_continuous(breaks = seq(0, 3, by = 1)) +
  facet_wrap( ~ gradeid) +
  theme_bw()  # Add theme for cleaner look

#timesci ~ aps45 | school
ggplot(stem, aes(x = timesci, y = aps45, group=school))+
   xlab("Social Class")+
   ylab("Math Score")+
   geom_boxplot()


# Design the model
model <- bayesvl()
model <- bvl_addNode(model, "infectedChina", "binorm")
model <- bvl_addNode(model, "infectedKorea", "binorm")


##### Bai toan cua HOANG
data1 <- read.csv("/Users/Shared/Previously Relocated Items/Security/Statistics/network/bayesvl-docs/Book/Data_full.csv")

	#Variables selection
	data1$Suicide <- data1$Suicide.Ideation
	data1$TCC <- data1$Connectedness.Companionship
	
	keeps <- c("Suicide","Religion","TCC")
	data1 <- data1[keeps]
	data1<-na.omit(data1)
	
	#Model Design
	model<-bayesvl()
	model<-bvl_addNode(model,"Suicide","binom") 
	model<-bvl_addNode(model,"Religion","binom")
	model<-bvl_addNode(model,"TCC","norm")
	model<-bvl_addNode(model,"Religion_TCC","trans")
	model<-bvl_addArc(model,"Religion","Religion_TCC","*")
	model<-bvl_addArc(model,"TCC","Religion_TCC","*")
	model<-bvl_addArc(model,"Religion","Suicide","slope")
	model<-bvl_addArc(model,"Religion_TCC","Suicide","slope")
	
	#Generating and checking Stan code
	model_string <- bvl_model2Stan(model)
	cat(model_string) 
	
	#Model fitting
	model<-bvl_modelFit(model, data1, warmup = 2000, iter = 5000, chains = 4,cores = 4)
	
	densMode <- function(x){
    td <- density(x)
    maxDens <- which.max(td$y)
    list(x=td$x[maxDens], y=td$y[maxDens])
	}
	
	posterior = as.matrix(model@stanfit)
	
	#Religion = 1
	Suicide1_1 = invlogit(posterior[, "a_Suicide"] + posterior[, "b_Religion_Suicide"] * 1 + posterior[, "b_Religion_TCC_Suicide"] * 1 * 1)
  Suicide1_2 = invlogit(posterior[, "a_Suicide"] + posterior[, "b_Religion_Suicide"] * 1 + posterior[, "b_Religion_TCC_Suicide"] * 1 * 2)
  Suicide1_3 = invlogit(posterior[, "a_Suicide"] + posterior[, "b_Religion_Suicide"] * 1 + posterior[, "b_Religion_TCC_Suicide"] * 1 * 3)
  Suicide1_4 = invlogit(posterior[, "a_Suicide"] + posterior[, "b_Religion_Suicide"] * 1 + posterior[, "b_Religion_TCC_Suicide"] * 1 * 4)
  Suicide1_5 = invlogit(posterior[, "a_Suicide"] + posterior[, "b_Religion_Suicide"] * 1 + posterior[, "b_Religion_TCC_Suicide"] * 1 * 5)
  Suicide1_6 = invlogit(posterior[, "a_Suicide"] + posterior[, "b_Religion_Suicide"] * 1 + posterior[, "b_Religion_TCC_Suicide"] * 1 * 6)
  
  ggplot() + 
  geom_density(aes(Suicide1_1, fill=1), alpha=0.25, size=1) + 
  geom_vline(xintercept = densMode(Suicide1_1)$x) +
  geom_density(aes(Suicide1_2, fill=2), alpha=0.25, size=1) + 
  geom_vline(xintercept = densMode(Suicide1_2)$x) +
  geom_density(aes(Suicide1_3, fill=3), alpha=0.25, size=1) + 
  geom_vline(xintercept = densMode(Suicide1_3)$x) +
  geom_density(aes(Suicide1_4, fill=4), alpha=0.25, size=1) + 
  geom_vline(xintercept = densMode(Suicide1_4)$x) +
  geom_density(aes(Suicide1_5, fill=5), alpha=0.25, size=1) + 
  geom_vline(xintercept = densMode(Suicide1_5)$x) +
  geom_density(aes(Suicide1_6, fill=6), alpha=0.25, size=1) + 
  #geom_vline(xintercept = densMode(Suicide1_6)$x) +
  annotate(geom="segment", x = densMode(Suicide1_6)$x, xend = densMode(Suicide1_6)$x, y = 0, yend = densMode(Suicide1_6)$y, colour = "black", linetype="dashed") +
  annotate(geom="label", x=densMode(Suicide1_6)$x, y=0.2, label=round(densMode(Suicide1_6)$x,2), color="black", fill = "white") +
	#scale_fill_discrete(labels = c("TCC=1", "TCC=2", "TCC=3", "TCC=4", "TCC=5", "TCC=6")) + 
	labs(fill="TCC")	+ labs(x = "probability of suicide")


  ggplot() + 
  geom_density(aes(Suicide1_1, fill=1), alpha=0.25, size=1) + 
  annotate(geom="segment", x = densMode(Suicide1_1)$x, xend = densMode(Suicide1_1)$x, y = 0, yend = densMode(Suicide1_1)$y, colour = "black", linetype="dashed") +
  geom_density(aes(Suicide1_2, fill=2), alpha=0.25, size=1) + 
  annotate(geom="segment", x = densMode(Suicide1_2)$x, xend = densMode(Suicide1_2)$x, y = 0, yend = densMode(Suicide1_2)$y, colour = "black", linetype="dashed") +
  geom_density(aes(Suicide1_3, fill=3), alpha=0.25, size=1) + 
  annotate(geom="segment", x = densMode(Suicide1_3)$x, xend = densMode(Suicide1_3)$x, y = 0, yend = densMode(Suicide1_3)$y, colour = "black", linetype="dashed") +
  geom_density(aes(Suicide1_4, fill=4), alpha=0.25, size=1) + 
  annotate(geom="segment", x = densMode(Suicide1_4)$x, xend = densMode(Suicide1_4)$x, y = 0, yend = densMode(Suicide1_4)$y, colour = "black", linetype="dashed") +
  geom_density(aes(Suicide1_5, fill=5), alpha=0.25, size=1) + 
  annotate(geom="segment", x = densMode(Suicide1_5)$x, xend = densMode(Suicide1_5)$x, y = 0, yend = densMode(Suicide1_5)$y, colour = "black", linetype="dashed") +
  geom_density(aes(Suicide1_6, fill=6), alpha=0.25, size=1) + 
  annotate(geom="segment", x = densMode(Suicide1_6)$x, xend = densMode(Suicide1_6)$x, y = 0, yend = densMode(Suicide1_6)$y, colour = "black", linetype="dashed") +
  annotate(geom="label", x=densMode(Suicide1_1)$x, y=0.2, label=round(densMode(Suicide1_1)$x,2), color="black", fill = "white") +
  annotate(geom="label", x=densMode(Suicide1_2)$x, y=0.2, label=round(densMode(Suicide1_2)$x,2), color="black", fill = "white") +
  annotate(geom="label", x=densMode(Suicide1_3)$x, y=0.2, label=round(densMode(Suicide1_3)$x,2), color="black", fill = "white") +
  annotate(geom="label", x=densMode(Suicide1_4)$x, y=0.2, label=round(densMode(Suicide1_4)$x,2), color="black", fill = "white") +
  annotate(geom="label", x=densMode(Suicide1_5)$x, y=0.2, label=round(densMode(Suicide1_5)$x,2), color="black", fill = "white") +
  annotate(geom="label", x=densMode(Suicide1_6)$x, y=0.2, label=round(densMode(Suicide1_6)$x,2), color="black", fill = "white") +
	labs(fill="TCC")	+ labs(x = "probability of suicide")



  #Religion = 0
  Suicide0_1 = invlogit(posterior[, "a_Suicide"] + posterior[, "b_Religion_Suicide"] * 1 + posterior[, "b_Religion_TCC_Suicide"] * 1 * 1)
  Suicide0_2 = invlogit(posterior[, "a_Suicide"] + posterior[, "b_Religion_Suicide"] * 1 + posterior[, "b_Religion_TCC_Suicide"] * 1 * 2)
  Suicide0_3 = invlogit(posterior[, "a_Suicide"] + posterior[, "b_Religion_Suicide"] * 1 + posterior[, "b_Religion_TCC_Suicide"] * 1 * 3)
  Suicide0_4 = invlogit(posterior[, "a_Suicide"] + posterior[, "b_Religion_Suicide"] * 1 + posterior[, "b_Religion_TCC_Suicide"] * 1 * 4)
  Suicide0_5 = invlogit(posterior[, "a_Suicide"] + posterior[, "b_Religion_Suicide"] * 1 + posterior[, "b_Religion_TCC_Suicide"] * 1 * 5)
  Suicide0_6 = invlogit(posterior[, "a_Suicide"] + posterior[, "b_Religion_Suicide"] * 1 + posterior[, "b_Religion_TCC_Suicide"] * 1 * 6)

	
	test_script = "
	for (i in 1:Nobs) {
        yrep_TCC_1[i] = binomial_rng(Suicide[i], inv_logit(a_Suicide + b_Religion_Suicide * Religion[i] + b_Religion_TCC_Suicide * Religion[i] * 1));
     }
     for (i in 1:Nobs) {
        yrep_TCC_2[i] = binomial_rng(Suicide[i], inv_logit(a_Suicide + b_Religion_Suicide * Religion[i] + b_Religion_TCC_Suicide * Religion[i] * 2));
     }
     for (i in 1:Nobs) {
        yrep_TCC_3[i] = binomial_rng(Suicide[i], inv_logit(a_Suicide + b_Religion_Suicide * Religion[i] + b_Religion_TCC_Suicide * Religion[i] * 3));
     }
     for (i in 1:Nobs) {
        yrep_TCC_4[i] = binomial_rng(Suicide[i], inv_logit(a_Suicide + b_Religion_Suicide * Religion[i] + b_Religion_TCC_Suicide * Religion[i] * 4));
     }
     for (i in 1:Nobs) {
        yrep_TCC_5[i] = binomial_rng(Suicide[i], inv_logit(a_Suicide + b_Religion_Suicide * Religion[i] + b_Religion_TCC_Suicide * Religion[i] * 5));
     }
     for (i in 1:Nobs) {
        yrep_TCC_6[i] = binomial_rng(Suicide[i], inv_logit(a_Suicide + b_Religion_Suicide * Religion[i] + b_Religion_TCC_Suicide * Religion[i] * 6));
     }
   "

############### DKAP - Chapter 9
model <- bvl_addArc(model, "sex",  "ict", "slope")
model <- bvl_addArc(model, "ecostt",  "ict", "slope")
model <- bvl_addArc(model, "edumot",  "ict", "slope")
model <- bvl_addArc(model, "edufat",  "ict", "slope")

model <- bvl_addArc(model, "schoolid", "ict", "varint")

model <- bvl_modelFix(model, data1)
model_string <- bvl_model2Stan(model)
cat(model_string)

options(mc.cores = parallel::detectCores())

# Fit the model
model <- bvl_modelFit(model, data1, warmup = 2000, iter = 5000, chains = 4, cores = 4)

posterior <- as.data.frame(stan_samples)
sum(posterior$edumot > 0) / length(posterior$edumot)
sum(posterior$edufat > 0) / length(posterior$edufat)


## Complete-pooling Sample Model
pooled_model = "data {
  int<lower=0> N; 
  vector[N] x;
  vector[N] y;
} 
parameters {
  real alpha;
  real beta;
  real<lower=0,upper=100> sigma;
} 
model {
  y ~ normal(alpha + beta * x, sigma);
}"

data_list <- list(N = nrow(STEM5000), school = as.numeric(as.factor(STEM5000$School)), x = STEM5000$TimeSci, y = STEM5000$APS45)
poolSample <- stan(model_code = pooled_model, data = data_list)

## Non-pooling Sample Model
unpooled_model = "data {
  int<lower=0> N; 
  int<lower=1,upper=16> school[N];
  vector[N] x;
  vector[N] y;
} 
parameters {
  vector[16] alpha;
  real beta;
  real<lower=0,upper=100> sigma;
} 
transformed parameters {
  vector[N] y_hat;

  for (i in 1:N)
    y_hat[i] <- beta * x[i] + alpha[school[i]];
}
model {
  y ~ normal(y_hat, sigma);
}"

data_list <- list(N = nrow(STEM5000), school = as.numeric(as.factor(STEM5000$School)), x = STEM5000$TimeSci, y = STEM5000$APS45)
unpoolSample <- stan(model_code = unpooled_model, data = data_list)




fit_ss <- extract(unpoolSample, permuted = TRUE) # fit_ss is a list 

school_names = c()
school_mean = c()
school_sd = c()
school_min = c()
school_max = c()
school_20 = c()
school_80 = c()
for(schoolid in 1:16)
{
	#print(schoolid)
	school_mean = c(school_mean, mean(fit_ss$alpha[,schoolid]))
	school_sd = c(school_sd, sd(fit_ss$alpha[,schoolid]))
	
	a_quant = quantile(fit_ss$alpha[,schoolid],c(0.025, 0.2, 0.50, 0.8, 0.975))
	a_quant <- data.frame(t(a_quant))
	names(a_quant) <- c("Q5",  "Q20", "Q50", "Q80", "Q95")
	
	school_min = c(school_min, a_quant$Q5)
	school_max = c(school_max, a_quant$Q95)
	school_20 = c(school_20, a_quant$Q20)
	school_80 = c(school_80, a_quant$Q80)
	
	school_names = c(school_names, schoolid)
}  
a_df <- data.frame(school_mean, school_max, school_min, school_20, school_80, school_sd, school_names)
#round(head(a_df), 2)

a_df <- a_df[order(a_df$school_mean), ]
a_df$school_rank <- c(1 : dim(a_df)[1])

ggplot(data = a_df, 
       aes(x = school_names, 
           y = school_mean)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_linerange(aes(ymin=school_20,ymax=school_80),size=2) +
  geom_pointrange(aes(ymin = school_min, 
                      ymax = school_max)) + 
  geom_hline(yintercept = mean(a_df$school_mean), 
             size = 0.5, 
             col = "red") +
  ylab("alpha") +
  xlab("school") +
  scale_x_discrete(limits=a_df$school_names) +
  theme_bw()




## Partial-pooling Sample Model
partial_pooling = "
data {
  int<lower=0> N; 
  int<lower=1,upper=16> school[N];
  vector[N] x;
  vector[N] y;
} 
parameters {
  vector[16] alpha;
  real beta;
  real mu_a;
  real<lower=0,upper=100> sigma_a;
  real<lower=0,upper=100> sigma_y;
} 
transformed parameters {
  vector[N] y_hat;
  for (i in 1:N)
    y_hat[i] <- beta * x[i] + alpha[school[i]];
}
model {
  mu_a ~ normal(0, 1);
  alpha ~ normal (10 * mu_a, sigma_a);

  y ~ normal(y_hat, sigma_y);
}"


## Partial-pooling Sample Model
partial_pooling = "
data {
  int<lower=0> N; 
  int<lower=1,upper=16> school[N];
  vector[N] x;
  vector[N] y;
} 
parameters {
  real beta;

  real mu0;
  //real<lower=0> sigma0;

  real u_alpha[16];
  real<lower=0> sigma_u;
  
  real<lower=0,upper=100> sigma_a;
  real<lower=0,upper=100> sigma_y;
} 
transformed parameters {
  vector[16] alpha;
  vector[N] y_hat;
  for (k in 1:16)
		alpha[k] = mu0 + u_alpha[k];
		
  for (i in 1:N)
    y_hat[i] = beta * x[i] + alpha[school[i]];
}
model {
  mu0 ~ normal(0, 10);
  u_alpha  ~ normal(0, sigma_u);
  
  y ~ normal(y_hat, sigma_y);
}"

data_list <- list(N = nrow(STEM5000), school = as.numeric(as.factor(STEM5000$School)), x = STEM5000$TimeSci, y = STEM5000$APS45)
partialSample <- stan(model_code = partial_pooling, data = data_list)



fit_ss <- extract(partialSample, permuted = TRUE) # fit_ss is a list 

school_names = c()
school_mean = c()
school_sd = c()
school_min = c()
school_max = c()
school_20 = c()
school_80 = c()
for(schoolid in 1:16)
{
	#print(schoolid)
	school_mean = c(school_mean, mean(fit_ss$alpha[,schoolid]))
	school_sd = c(school_sd, sd(fit_ss$alpha[,schoolid]))
	
	a_quant = quantile(fit_ss$alpha[,schoolid],c(0.025, 0.2, 0.50, 0.8, 0.975))
	a_quant <- data.frame(t(a_quant))
	names(a_quant) <- c("Q5",  "Q20", "Q50", "Q80", "Q95")
	
	school_min = c(school_min, a_quant$Q5)
	school_max = c(school_max, a_quant$Q95)
	school_20 = c(school_20, a_quant$Q20)
	school_80 = c(school_80, a_quant$Q80)
	
	school_names = c(school_names, schoolid)
}  
a_df <- data.frame(school_mean, school_max, school_min, school_20, school_80, school_sd, school_names)
#round(head(a_df), 2)

a_df <- a_df[order(a_df$school_mean), ]
a_df$school_rank <- c(1 : dim(a_df)[1])

ggplot(data = a_df, 
       aes(x = school_names, 
           y = school_mean)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_linerange(aes(ymin=school_20,ymax=school_80),size=2) +
  geom_pointrange(aes(ymin = school_min, 
                      ymax = school_max)) + 
  geom_hline(yintercept = mean(a_df$school_mean), 
             size = 0.5, 
             col = "red") +
  ylab("alpha") +
  xlab("school") +
  scale_x_discrete(limits=a_df$school_names) +
  theme_bw()




data_list <- list(Nobs = nrow(stem), APS45ID = as.numeric(stem$APS45ID), NTimeSci = 3, TimeSci = as.numeric(stem$TimeSci), NGradeid = 4, Gradeid = as.numeric(stem$Gradeid))
stemSample <- stan(model_code = model_string, data = data_list)

############ Chapter 6 ###############
theta <- 0.5 # this is a fair coin
Ntoss <- 50
flips <- rbinom(n = Ntoss, size = 1, prob = theta)

trialTheta = c()
nHead = 0 # number of heads
for (i in 1:Ntoss) {	                
	# Cumulative number of heads at step i
	nHead = nHead + flips[i]
  # Compute the running proportion of heads	
	trialTheta = c(trialTheta, nHead / i)
	# Print proportion of heads	at step i
	print(paste0("Theta after ", i, " trials is ", trialTheta[i]))
}

p1 <- ggplot(data=data.frame(x=1:Ntoss,y=trialTheta), aes(x=x, y=y, group=1)) +
geom_line(color="skyblue")+
geom_point(color="skyblue", size=2, shape=21, fill='white') +
scale_x_continuous(limits = c(0, 50))

p2 <- ggplot(data=data.frame(x=-4:45,y=trialTheta), aes(x=x, y=y, group=1)) +
geom_line(color="skyblue")+
geom_point(color="skyblue", size=2, shape=21, fill='white') +
scale_x_continuous(limits = c(0, 50))

ggplot(data=data.frame(x=1:50,x1=-3:46,x2=0:49,y=trialTheta)) +
geom_line(aes(x, y, color="skyblue"))+
geom_point(aes(x, y, color="skyblue"), size=2, shape=21, fill='white') +
geom_line(aes(x1, y, color="blue"))+
geom_point(aes(x1, y, color="blue"), size=2, shape=21, fill='white') +
geom_line(aes(x2, y, color="green"))+
geom_point(aes(x2, y, color="green"), size=2, shape=21, fill='white') +
scale_x_continuous(limits = c(1, 50)) +
geom_polygon(data=data.frame(y=c(0.42,0.42,0.455,0.455), x=c(27.5,28.5,28.5,27.5)), aes(x=x, y=y), colour="black", fill=NA) +
geom_polygon(data=data.frame(y=c(0.44,0.44,0.51,0.51), x=c(27.43,28.57,28.57,27.43)), aes(x=x, y=y), colour="blue", fill=NA, linetype="dashed") +
#geom_polygon(data=data.frame(y=c(0.244,0.244,0.314,0.314), x=c(11.4,12.5,12.5,11.5)), aes(x=x, y=y), colour="black", fill=NA) +
theme_bw() +
scale_color_discrete(name = element_blank(), labels=c("Lag = 5","Lag = 1","Lag = 0"))

scale_color_manual(name = element_blank(), values=c("Lag=5"="blue","Lag=1"="green","Lag=0"="skyblue"))

coda::gelman.plot( codaObject[,params] , main="" , auto.layout=TRUE , 
                       col=DBDAplColors )

autocorr.diag (stan2coda(stan_samples), c(0, 1, 5, 10, 50))

