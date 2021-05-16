	#Variables selection
	data1$Suicide <- data1$Suicide.Ideation
	data1$TCC <- data1$Connectedness.Companionship
	
	keeps <- c("Suicide","Religion","TCC")
	data1 <- data1[keeps]
	data1<-na.omit(data1)
	
	#Model Design
	model<-bayesvl()
	model<-bvl_addNode(model,"Suicide","binom") 
	model<-bvl_addNode(model,"Religion","cat")
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
	Religion = 1
	Suicide1_1 = invlogit(posterior[, "a_Suicide"] + posterior[, "b_Religion_Suicide"] * Religion + posterior[, "b_Religion_TCC_Suicide"] * Religion * 1)
  Suicide1_2 = invlogit(posterior[, "a_Suicide"] + posterior[, "b_Religion_Suicide"] * Religion + posterior[, "b_Religion_TCC_Suicide"] * Religion * 2)
  Suicide1_3 = invlogit(posterior[, "a_Suicide"] + posterior[, "b_Religion_Suicide"] * Religion + posterior[, "b_Religion_TCC_Suicide"] * Religion * 3)
  Suicide1_4 = invlogit(posterior[, "a_Suicide"] + posterior[, "b_Religion_Suicide"] * Religion + posterior[, "b_Religion_TCC_Suicide"] * Religion * 4)
  Suicide1_5 = invlogit(posterior[, "a_Suicide"] + posterior[, "b_Religion_Suicide"] * Religion + posterior[, "b_Religion_TCC_Suicide"] * Religion * 5)
  Suicide1_6 = invlogit(posterior[, "a_Suicide"] + posterior[, "b_Religion_Suicide"] * Religion + posterior[, "b_Religion_TCC_Suicide"] * Religion * 6)

	#Religion = 2
	Religion = 2
	Suicide0_1 = invlogit(posterior[, "a_Suicide"] + posterior[, "b_Religion_Suicide"] * Religion + posterior[, "b_Religion_TCC_Suicide"] * Religion * 1)
  Suicide0_2 = invlogit(posterior[, "a_Suicide"] + posterior[, "b_Religion_Suicide"] * Religion + posterior[, "b_Religion_TCC_Suicide"] * Religion * 2)
  Suicide0_3 = invlogit(posterior[, "a_Suicide"] + posterior[, "b_Religion_Suicide"] * Religion + posterior[, "b_Religion_TCC_Suicide"] * Religion * 3)
  Suicide0_4 = invlogit(posterior[, "a_Suicide"] + posterior[, "b_Religion_Suicide"] * Religion + posterior[, "b_Religion_TCC_Suicide"] * Religion * 4)
  Suicide0_5 = invlogit(posterior[, "a_Suicide"] + posterior[, "b_Religion_Suicide"] * Religion + posterior[, "b_Religion_TCC_Suicide"] * Religion * 5)
  Suicide0_6 = invlogit(posterior[, "a_Suicide"] + posterior[, "b_Religion_Suicide"] * Religion + posterior[, "b_Religion_TCC_Suicide"] * Religion * 6)

  #Religion = 1
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


  #Religion = 2
  ggplot() + 
  geom_density(aes(Suicide0_1, fill=1), alpha=0.25, size=1) + 
  annotate(geom="segment", x = densMode(Suicide0_1)$x, xend = densMode(Suicide0_1)$x, y = 0, yend = densMode(Suicide0_1)$y, colour = "black", linetype="dashed") +
  geom_density(aes(Suicide0_2, fill=2), alpha=0.25, size=1) + 
  annotate(geom="segment", x = densMode(Suicide0_2)$x, xend = densMode(Suicide0_2)$x, y = 0, yend = densMode(Suicide0_2)$y, colour = "black", linetype="dashed") +
  geom_density(aes(Suicide0_3, fill=3), alpha=0.25, size=1) + 
  annotate(geom="segment", x = densMode(Suicide0_3)$x, xend = densMode(Suicide0_3)$x, y = 0, yend = densMode(Suicide0_3)$y, colour = "black", linetype="dashed") +
  geom_density(aes(Suicide0_4, fill=4), alpha=0.25, size=1) + 
  annotate(geom="segment", x = densMode(Suicide0_4)$x, xend = densMode(Suicide0_4)$x, y = 0, yend = densMode(Suicide0_4)$y, colour = "black", linetype="dashed") +
  geom_density(aes(Suicide0_5, fill=5), alpha=0.25, size=1) + 
  annotate(geom="segment", x = densMode(Suicide0_5)$x, xend = densMode(Suicide0_5)$x, y = 0, yend = densMode(Suicide0_5)$y, colour = "black", linetype="dashed") +
  geom_density(aes(Suicide0_6, fill=6), alpha=0.25, size=1) + 
  annotate(geom="segment", x = densMode(Suicide0_6)$x, xend = densMode(Suicide0_6)$x, y = 0, yend = densMode(Suicide0_6)$y, colour = "black", linetype="dashed") +
  annotate(geom="label", x=densMode(Suicide0_1)$x, y=0.2, label=round(densMode(Suicide0_1)$x,2), color="black", fill = "white") +
  annotate(geom="label", x=densMode(Suicide0_2)$x, y=0.2, label=round(densMode(Suicide0_2)$x,2), color="black", fill = "white") +
  annotate(geom="label", x=densMode(Suicide0_3)$x, y=0.2, label=round(densMode(Suicide0_3)$x,2), color="black", fill = "white") +
  annotate(geom="label", x=densMode(Suicide0_4)$x, y=0.2, label=round(densMode(Suicide0_4)$x,2), color="black", fill = "white") +
  annotate(geom="label", x=densMode(Suicide0_5)$x, y=0.2, label=round(densMode(Suicide0_5)$x,2), color="black", fill = "white") +
  annotate(geom="label", x=densMode(Suicide0_6)$x, y=0.2, label=round(densMode(Suicide0_6)$x,2), color="black", fill = "white") +
	labs(fill="TCC")	+ labs(x = "probability of suicide")

