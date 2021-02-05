# Predatory Feeding Efficiency Analyses #
## Colleen Miller, Spring 2019 ##
library(lme4)
library(ggplot2)
library(sjPlot)
library(emmeans)
library(plyr)

setwd('~/Downloads/')

d <- read.csv('Predator Efficiency Data Sheet - Sheet1.csv', header=T)
head(d)
d$Notes <- NULL
d$Chamber_Num <- NULL
d <- na.omit(d)

d$Species = revalue(d$Species, c('C7' = 'C. septempunctata', 'CMAC' = 'C. maculata'))

hist(d$Num_Aphids_Post, col='mediumorchid')
hist(d$Num_Aphids_Before-d$Num_Aphids_Post, col='skyblue')
d$Num_Aphids_Eaten <- d$Num_Aphids_Before - d$Num_Aphids_Post
d$Prop_Aphids_Eaten <- d$Num_Aphids_Eaten/d$Num_Aphids_Before
d$Temp <- as.factor(d$Temp)
d$Light <- as.factor(d$Light)

mod1 <- lm(Num_Aphids_Eaten ~ Light*Temp, d)
summary(mod1)

mod2 <- lm(Num_Aphids_Eaten ~ Light*Temp*Species, d)
summary(mod2)

mod3 <- lmer(Num_Aphids_Eaten ~ Light*Temp*Species + (1|Location_Name) + (1|Trial), d)
summary(mod3)

mod4 <- glmer(Num_Aphids_Eaten ~ Light*Temp*Species + (1|Location_Name) + (1|Trial), family = poisson, d)
summary(mod4)

mod4.00 <- glmer(Num_Aphids_Eaten ~ Light + Temp + Species + (1|Location_Name) + (1|Trial), family = poisson, d)
summary(mod4.00)
g <- ggplot(d, aes(x=Light, y=Num_Aphids_Eaten, fill=Temp)) + labs(y='Number of Aphids Consumed') + geom_boxplot() + theme_classic() + scale_fill_brewer()
g + facet_wrap(~Species) 
e00 <- emmeans(mod4.00, pairwise ~ Light*Temp)
e00
plot(e00, comparisons=T) + theme_sjplot2()

mod4.0 <- glmer(Num_Aphids_Eaten ~ Light*Temp + Species + (1|Location_Name) + (1|Trial), family = poisson, d)
summary(mod4.0)
plot_model(mod4.0, type='int') + theme_classic()
e0 <- emmeans(mod4.0, pairwise ~ Light*Temp)
e0
plot(e0, comparisons=T) + theme_sjplot2()

mod4.1 <- glmer(Num_Aphids_Eaten ~ Light*Temp*Species + (1|Location_Name) , family = poisson, d[d$Trial ==2,])
summary(mod4.1)

## MAIN MODEL 1 & MAIN FIGURE##
mod4.2 <- glmer(Num_Aphids_Eaten ~ Light*Temp + Species + (1|Location_Name) , family = poisson, d[d$Trial ==2,])
summary(mod4.2)
p <- ggplot(d[d$Trial==2,], aes(x=Light, y=Num_Aphids_Eaten, fill=Temp)) + geom_boxplot() + theme_classic() + scale_fill_brewer() + labs(y='Nummber of Aphids Consumed')
p + facet_wrap(~Species)

tiff("ecology_ms_figure3.tif", units="in", width=7, height=6, res=600)

q <- ggplot(d[d$Trial==2,], aes(x=Temp, y=Num_Aphids_Eaten, fill=interaction(Temp, Light))) + geom_boxplot() + geom_jitter() + theme_classic() 
q + facet_wrap(~Species) + scale_fill_manual(values=c(alpha("navy", .8), alpha("moccasin", .8), alpha('lightslateblue', .8), alpha('orange', .8)), name='Treatment', labels=c('Cold/Control', 'Warm/Control', 'Cold/Light', 'Warm/Light')) + labs(y='Nummber of Aphids Consumed', x='Temperature (Celsius)') 

dev.off()
######

e2 <- emmeans(mod4.2, pairwise~ Light*Temp)
e2
plot(e2, comparisons=T) + theme_sjplot2()

mod5 <- glmer(Prop_Aphids_Eaten ~ Light*Temp*Species + (1|Location_Name) + (1|Trial), family = binomial, weights = Num_Aphids_Before, d)
summary(mod5)

mod5.0 <- glmer(Prop_Aphids_Eaten ~ Light*Temp + Species + (1|Location_Name) + (1|Trial), family = binomial, weights = Num_Aphids_Before, d)
summary(mod5.0)
plot_model(mod5.0, type='int') + theme_classic()
e0 <- emmeans(mod5.0, pairwise ~ Light*Temp)
e0
plot(e0, comparisons=T) + theme_sjplot2()

mod5.1 <- glmer(Prop_Aphids_Eaten ~ Light*Temp*Species + (1|Location_Name) , family = binomial, weights = Num_Aphids_Before, d[d$Trial==2,])
summary(mod5.1)

## MAIN MODEL 2 ##
mod5.2 <- glmer(Prop_Aphids_Eaten ~ Light*Temp + Species + (1|Location_Name) , family = binomial, weights = Num_Aphids_Before, d[d$Trial==2,])
summary(mod5.2)
plot_model(mod5.2, type='int') + theme_classic()
e1 <- emmeans(mod5.2, pairwise ~ Light*Temp)
e1
plot(e1, comparisons=T) + theme_sjplot2()

mod6 <- glmer(Num_Aphids_Eaten ~ Light + Temp + Species + (1|Location_Name) + (1|Trial) , family = poisson, d)
summary(mod6)

mod7 <- glmer(Prop_Aphids_Eaten ~ Light + Temp + Species + (1|Location_Name) + (1|Trial), family = binomial, weights = Num_Aphids_Before, d)
summary(mod7)

par(mfrow=c(1,1))

plot(Prop_Aphids_Eaten ~ Light, d)
plot(Prop_Aphids_Eaten ~ Temp, d)
plot(Prop_Aphids_Eaten ~ Species, d)

mod8 <- glmer(Num_Aphids_Eaten ~ Treatment + Species + (1|Location_Name) + (1|Trial), family = poisson, d)
summary(mod8)

mod9 <- glmer(Prop_Aphids_Eaten ~ Treatment + Species + (1|Location_Name) + (1|Trial), family = binomial, weights = Num_Aphids_Before, d)
summary(mod9)

mod10 <- glmer(Num_Aphids_Eaten ~ Treatment*Species + (1|Location_Name) + (1|Trial), family = poisson, d)
summary(mod10)

mod11 <- glmer(Prop_Aphids_Eaten ~ Treatment*Species + (1|Location_Name) + (1|Trial), family = binomial, weights = Num_Aphids_Before, d)
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
p + scale_fill_manual(values=c(alpha("cornflowerblue", .3), alpha("red", .6))) + scale_color_manual(values=c("cornflowerblue", 'red')) + scale_x_discrete(breaks = c(1,2,3,4), labels=c("Cold/Control", "Cold/ALAN",
                                                 "Warm/Control", "Warm/ALAN"))
p + facet_wrap(~Species) + scale_fill_manual(values=c(alpha("cornflowerblue", .3), alpha("red", .6))) + scale_color_manual(values=c("cornflowerblue", 'red')) + theme(text = element_text(size=15), axis.text.x = element_text(angle=45, hjust=1)) + scale_x_discrete(breaks = c(1,2,3,4), labels=c("Cold/Control", "Cold/ALAN",
                                                                                                                                                                                                         "Warm/Control", "Warm/ALAN")) + ylab('Number Aphids Consumed')

p <- ggplot(d, aes(x=Treatment, y=Num_Aphids_Eaten, fill=interaction(Treatment, Species)))+ theme_linedraw() + theme(text = element_text(size=15))
p + geom_boxplot(position=position_dodge(1)) + theme_linedraw() + scale_fill_manual(values=c("blue","gray87","orange","gray27","darkblue","grey67","darkorange","gray7")) 


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
