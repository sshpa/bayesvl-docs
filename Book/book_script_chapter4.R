rateChina <- drop(rstan::extract(stan_samples, "rateChina")[[1]])
rateKorea <- drop(rstan::extract(stan_samples, "rateKorea")[[1]])
mean(rateChina > rateKorea)


ggplot(rateChina) + geom_density(

ggplot(rateChina, aes(x)) +
  geom_histogram(aes(y = stat(density))) +
  stat_function(
    fun = dnorm, 
    args = list(mean = mean(rateChina$x), sd = sd(rateChina$x)), 
    lwd = 2, 
    col = 'red'
  )
  
ggplot() + geom_histogram(aes(rateChina), bins = 5)

ggplot() + geom_histogram(aes(rateChina), bins = 50) + geom_density(aes(rateChina))

ggplot() + geom_density(aes(rateChina, fill="blue"), alpha=0.25, size=1) + geom_density(aes(rateKorea, fill="red"), alpha=0.25, size=1) + 
	scale_fill_discrete(labels = c("China", "Korea")) + theme(legend.title=element_blank())	+ labs(x = "probability")


guide_legend(
    title.theme = element_text(
      size = 15,
      face = "italic",
      colour = "red",
      angle = 0
    )

 + labs(fill =c("rateChina", "rateKorea"))
guide_legend(title="New Legend Title")

ggplot() + geom_histogram(aes(rateChina, fill=1), alpha=0.25) + geom_histogram(aes(rateKorea, fill=2), alpha=0.25)


	library(bayesvl)
	dat <- data(STEM5000)
	
	dat <- data.frame(dat$Sex,dat$APS45)
	names(dat) <- c("Sex", "APS45")
	dat$SexLab <- ifelse(dat$Sex==1,'Male','Female')
	dat$Sex <- dat$Sex-1
	
	male <- dat[dat$Sex == 1,]
	female <- dat[dat$Sex == 2,]

model <- bayesvl()
model <- bvl_addNode(model, "y", "norm")
model <- bvl_addNode(model, "x", "binom")

model <- bvl_addArc(model, "x", "y", "slope")

summary(model)

stan_code <- bvl_model2Stan(model)
cat(stan_code)

model <- bvl_modelFit(model, data.frame(x=dat$Sex-1,y=dat$APS45), iter=5000 , warmup=2000 , chains=4 , cores=4)

stan_diag(model@stanfit)
bvl_plotTrace(model)

mcmc = as.matrix(model@stanfit)
group1_mean = mcmc[, "a_y"]
group2_mean = mcmc[, "a_y"] + mcmc[, "b_x_y"]

group_sigma = mcmc[, "sigma_y"]

## Cohen's D
cohenD = mcmc[, "b_x_y"] / group_sigma

# Percentage change (relative to Group A)
ES = 100 * mcmc[, "b_x_y"]/mcmc[, "a_y"]
hist(ES)


ggplot() + geom_density(aes(group1_mean, fill="blue"), alpha=0.25) + geom_density(aes(group2_mean, fill="red"), alpha=0.25) + 
	scale_fill_discrete(labels = c("Male", "Female")) + theme(legend.title=element_blank())	+ labs(x = "")


ggplot() + geom_histogram(aes(male$APS45), bins = 50) + theme_bw()


ggplot(dat, aes(x=APS45, color=Sex)) +
  geom_histogram(fill="white", alpha=0.5, position="identity")
  
ggplot(male, aes(x=APS45)) + 
  geom_histogram(color="black", fill="white") +
  geom_vline(aes(xintercept=mean(APS45)),
            color="blue", linetype="dashed", size=1)

ggplot(female, aes(x=APS45)) + 
  geom_histogram(color="black", fill="white") +
  geom_vline(aes(xintercept=mean(APS45)),
            color="blue", linetype="dashed", size=1)


mcmc = as.matrix(fit)
## Calculate the fitted values
newdata = data.frame(x = levels(as.factor(dat$SexLab)))
Xmat = model.matrix(~x, newdata)
coefs = mcmc[, c("alpha", "beta")]
fit_coefs = coefs %*% t(Xmat)
newdata = newdata %>% cbind(tidyMCMC(fit_coefs, conf.int = TRUE, conf.method = "HPDinterval"))
newdata
  x  estimate std.error  conf.low conf.high
1 A 105.31718 0.3665387 104.57812 106.01442
2 B  77.84126 0.4282323  77.01206  78.63169
ggplot(newdata, aes(y = estimate, x = x)) + geom_pointrange(aes(ymin = conf.low,
    ymax = conf.high)) + scale_y_continuous("Y") + scale_x_discrete("X") +
    theme_classic()
    
mcmc = as.matrix(fit)
## Cohen's D
cohenD = mcmc[, "beta"]/mcmc[, "sigma"]
tidyMCMC(as.mcmc(cohenD), conf.int = TRUE, conf.method = "HPDinterval")
  term  estimate std.error  conf.low conf.high
1 var1 -10.05256 0.7697586 -11.60651 -8.615262
# Percentage change (relative to Group A)
ES = 100 * mcmc[, "beta"]/mcmc[, "alpha"]
# Probability that the effect is greater than 10% (a decline of >10%)
sum(-1 * ES > 5)/length(ES)
[1] 1


############# ANOVA #######################
	dat <- data.frame(as.factor(dat$Gradeid),dat$APS45)
	names(dat) <- c("Grade", "APS45")
  dat$is7 = ifelse(dat$Grade == 7, 1, 0)
  dat$is8 = ifelse(dat$Grade == 8, 1, 0)
  dat$is9 = ifelse(dat$Grade == 9, 1, 0)
  
boxplot(APS45 ~ Grade, dat)

# Compute the analysis of variance
res.aov <- aov(weight ~ group, data = my_data)
# Summary of the analysis
summary(res.aov)

anova(lm(APS45 ~ Grade, dat))

library(bayesplot)
mcmc_combo(as.matrix(data.rstan), regex_pars = "beta|sigma")

model <- bayesvl()
model <- bvl_addNode(model, "y", "norm")
model <- bvl_addNode(model, "x7", "binom")
model <- bvl_addNode(model, "x8", "binom")
model <- bvl_addNode(model, "x9", "binom")

model <- bvl_addArc(model, "x7", "y", "slope")
model <- bvl_addArc(model, "x8", "y", "slope")
model <- bvl_addArc(model, "x9", "y", "slope")

summary(model)
stan_code <- bvl_model2Stan(model)
cat(stan_code)
