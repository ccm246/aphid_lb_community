# Predatory Feeding Efficiency Analyses #
## Colleen Miller, Spring 2019 ##

setwd('~/Downloads/')

d <- read.csv('Predator Efficiency Data Sheet - Sheet1.csv', header=T)

library(lme4)
library(ggplot2)
library(sjPlot)

hist(d$Num_Aphids_Post)
hist(d$Num_Aphids_Before-d$Num_Aphids_Post)
d$Num_Aphids_Eaten <- d$Num_Aphids_Before - d$Num_Aphids_Post
d$Prop_Aphids_Eaten <- d$Num_Aphids_Eaten/d$Num_Aphids_Before

mod1 <- lm(Num_Aphids_Eaten ~ Light*Temp, d)
summary(mod1)

mod2 <- lm(Num_Aphids_Eaten ~ Light*Temp*Species, d)
summary(mod2)

mod3 <- lmer(Num_Aphids_Eaten ~ Light*Temp*Species + (1|Location_Name), d)
summary(mod3)

mod4 <- glmer(Num_Aphids_Eaten ~ Light*Temp*Species + (1|Location_Name), family = poisson, d)
summary(mod4)

mod5 <- glmer(Prop_Aphids_Eaten ~ Light*Temp*Species + (1|Location_Name), family = binomial, weights = Num_Aphids_Before, d)
summary(mod5)

mod6 <- glmer(Num_Aphids_Eaten ~ Light + Temp + Species + (1|Location_Name), family = poisson, d)
summary(mod6)

mod7 <- glmer(Prop_Aphids_Eaten ~ Light + Temp + Species + (1|Location_Name), family = binomial, weights = Num_Aphids_Before, d)
summary(mod7)

mod8 <- glmer(Num_Aphids_Eaten ~ Treatment + Species + (1|Location_Name), family = poisson, d)
summary(mod8)

mod9 <- glmer(Prop_Aphids_Eaten ~ Treatment + Species + (1|Location_Name), family = binomial, weights = Num_Aphids_Before, d)
summary(mod9)

mod10 <- glmer(Num_Aphids_Eaten ~ Treatment*Species + (1|Location_Name), family = poisson, d)
summary(mod10)

mod11 <- glmer(Prop_Aphids_Eaten ~ Treatment*Species + (1|Location_Name), family = binomial, weights = Num_Aphids_Before, d)
summary(mod11)

mod12 <- glm(Num_Aphids_Eaten ~ Treatment*Species, family = poisson, d)
summary(mod12)

mod13 <- glm(Bin_Aphids_Remain ~ Treatment*Species, family=binomial(link = "logit"), data=d)
summary(mod13)

par(mfrow=c(2,2))
plot(Num_Aphids_Eaten ~ Species, d[d$Treatment==1,], col=alpha('darkblue', .7), pch=19, cex=2, xlab = '', ylab = 'Number Aphids Eaten', ylim=c(0,33))
plot(Num_Aphids_Eaten ~ Species, d[d$Treatment==2,], col=alpha('lightblue', .7), pch=19, cex=2, xlab = '', ylab = '', ylim=c(0,33))
plot(Num_Aphids_Eaten ~ Species, d[d$Treatment==2,], col=alpha('goldenrod', .7), pch=15, cex=2, xlab = '', ylab = 'Number Aphids Eaten', ylim=c(0,33))
plot(Num_Aphids_Eaten ~ Species, d[d$Treatment==3,], col=alpha('yellow', .7), pch=15, cex=2, xlab = '', ylab = '', ylim=c(0,33))
#legend(x = 31, y = .65, cex = c(1,1,1,1), pch=c(19,19,15,15), col=c('darkblue', 'lightblue', 'goldenrod', 'yellow'), legend=c("Light/Warm", "Light/Cold", "Dark/Warm", "Dark/Cold"))

d$Treatment <- as.factor(d$Treatment)

p <- ggplot(d, aes(x=Treatment, y=Num_Aphids_Eaten, color=Species, fill=Species)) + geom_boxplot(position=position_dodge(1)) + theme_linedraw() + theme(text = element_text(size=15))
p + scale_fill_manual(values=c(alpha("#E69F00", .3), alpha("#E69F00", .6))) + scale_color_manual(values=c("#E69F00", '#E69F00')) + scale_x_discrete(breaks = c(1,2,3,4), labels=c("Cold/Control", "Cold/ALAN",
                                                 "Warm/Control", "Warm/ALAN"))
p + facet_wrap(~Species) + scale_fill_manual(values=c(alpha("#E69F00", .3), alpha("#E69F00", .6))) + scale_color_manual(values=c("#E69F00", '#E69F00')) + theme(text = element_text(size=15), axis.text.x = element_text(angle=45, hjust=1)) + scale_x_discrete(breaks = c(1,2,3,4), labels=c("Cold/Control", "Cold/ALAN",
                                                                                                                                                                                                         "Warm/Control", "Warm/ALAN"))

p <- ggplot(d, aes(x=Treatment, y=Num_Aphids_Eaten, fill=interaction(Treatment, Species)))+ theme_linedraw() + theme(text = element_text(size=15))
p + geom_boxplot(position=position_dodge(1)) + theme_linedraw() + scale_fill_manual(values=c("blue","yellow","cyan","orange","darkblue","goldenrod","darkcyan","darkorange")) 


par(mfrow=c(1,1))
plot(Num_Aphids_Eaten ~ Temp, d, col=alpha('darkblue', .7), xlab = 'Treatment', ylab = 'Number Aphids Eaten')

par(mfrow=c(2,2))
plot(Bin_Aphids_Remain ~ Species, d[d$Treatment==1,], col=alpha('darkblue', .6), pch=19, cex=1, xlab = 'Species', ylab = 'Do Aphids Remain (1/0)', main = 'Binary Data')
plot(Bin_Aphids_Remain ~ Species, d[d$Treatment==2,], col=alpha('lightblue', .6), pch=19, cex=1)
plot(Bin_Aphids_Remain ~ Species, d[d$Treatment==3,], col=alpha('darkred', .6), pch=15, cex=1)
plot(Bin_Aphids_Remain ~ Species, d[d$Treatment==4,], col=alpha('pink', .6), pch=15, cex=1)

par(mfrow=c(2,1))
plot(Binned_Remain ~ Treatment, d[d$Species=='C7',], col=alpha('darkblue', .6), pch=19, cex=1, xlab = 'Treatment', ylab = 'Do Aphids Remain (1/0)', main = 'Binary Data')
plot(Binned_Remain ~ Treatment, d[d$Species=='CMAC',], col=alpha('lightblue', .6), pch=19, cex=1)

p <- plot_model(mod12, typ='int') + theme_classic() + theme(text = element_text(size=15), axis.text.x = element_text(angle=45, hjust=1)) 
p
