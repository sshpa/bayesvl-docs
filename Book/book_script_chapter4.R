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

ggplot() + geom_density(aes(rateChina, fill="blue"), alpha=0.25) + geom_density(aes(rateKorea, fill="red"), alpha=0.25) + 
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

dat[dat$Sex == 1,]


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
