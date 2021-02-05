# APHID DROPPING TRIAL 1 ANALYSIS #
# COLLEEN MILLER #
# COLLAB. WITH JENNIFER THALER, TODD UGINE AND MAREN VITOUSEK #

library(lme4)
library(ggplot2)
library(sjPlot)
library(car)
library(emmeans)

setwd('~/Desktop/Cornell_Remnant/2018-2019/Thaler Rotation ')

dat3 <- read.csv("Dropping Trial Data Sheet.csv", header=T)
head(dat3)

# remove notes column so I can remove NAs
dat3$Notes <- NULL
dat3$Day_Two_Drop <- NULL
dat3$Day_Two_Prop <- NULL
dat3$Day_Two_Total <- NULL

# IDS
dat3$ID <- c(1:nrow(dat3))

# remove NAS

dat3 <- na.omit(dat3)

# it looks like the data is count data and poisson distribution
hist(dat3$Perc_Aphids_Dropped)
# a log transform could normalize this, but I can also do a glm
hist(log(dat3$Perc_Aphids_Dropped))

# run as gaussian, because the poisson may not be doing it
lmm2.1 <- lmer(Perc_Aphids_Dropped ~ Light + Temperature + (1 | Chamber_Num), data = dat3)
summary(lmm2.1)

# we may have an outlier
plot(dat3$Perc_Aphids_Dropped ~ dat3$Light, col='goldenrod', ylim=c(0,0.3))
plot(dat3$Perc_Aphids_Dropped ~ dat3$Temperature, col = 'goldenrod')
dat3$temp_factor <- as.factor(dat3$Temperature)
plot(dat3$Perc_Aphids_Dropped ~ dat3$temp_factor, col = 'goldenrod')

# the plots looks better, even if the models aren't very interesting
#light
p1 <- ggplot(dat3, aes(x=Light, y=Perc_Aphids_Dropped, color=Light, fill=Light)) + geom_boxplot() + scale_fill_manual(values=c('lightgrey', 'lightgoldenrod'))
p1 + geom_jitter(shape=16, position=position_jitter(0.05)) + theme_classic() + scale_color_manual(values=c("#999999", "#E69F00"))

#temperature
dat3$fac_Temperature <- as.factor(dat3$Temperature)
p2 <- ggplot(dat3, aes(x=fac_Temperature, y=Perc_Aphids_Dropped, fill=interaction(Light, fac_Temperature))) + geom_boxplot() + ylab('Percent Aphids Dropped') + geom_jitter()
p2  + labs(x="Temperature") + theme_classic()+ scale_fill_manual(values=c(alpha("navy", .8), alpha('lightslateblue', .8), alpha("moccasin", .8), alpha('orange', .8)), name = 'Treatment', labels=c('Cold/Control', 'Cold/Light', 'Warm/Control', 'Warm/Light'))


##### FIGURE 3 #####
dat3$TreatmentCh <-'Cold/Control'
dat3$TreatmentCh[dat3$Treatment==2] <- 'Cold/Light'
dat3$TreatmentCh[dat3$Treatment==3] <- 'Warm/Control'
dat3$TreatmentCh[dat3$Treatment==4] <- 'Warm/Light'

par(mfrow=c(2,1))
dat3$fac_Temperature <- as.factor(dat3$Temperature)

tiff("ecology_ms_figure3a", units="in", width=7, height=7, res=600)

p2 <- ggplot(dat3, aes(x=TreatmentCh, y=Perc_Aphids_Dropped, fill=interaction(TreatmentCh))) + geom_boxplot() + ylab('Proportion Aphids Dropped') + geom_jitter(width=.2) + labs(tag='A') + labs(x="Treatment") + theme_classic(base_size = 12) + theme(legend.position = "none") + scale_fill_manual(values=c(alpha("navy", .8), alpha('lightslateblue', .8), alpha("moccasin", .8), alpha('orange', .8)), name = 'Treatment', labels=c('Cold/Control', 'Cold/Light', 'Warm/Control', 'Warm/Light'))

dev.off()

+ scale_x_discrete(breaks=c(1,2,3,4), labels=c('Cold/Control', 'Cold/Light', 'Warm/Control','Warm/Light'))
# what about an interaction between light and temperature?
lmm5 <- lmer(Perc_Aphids_Dropped ~ Light* Temperature + (1 | Chamber_Num), data = dat3)
summary(lmm5)

plot_model(lmm5, type = "pred", terms = c("Temperature", "Light")) + theme_classic()

#make temperature numeric for better plotting
dat3$num_Temperature <- as.numeric(dat3$Temperature)
lmm5 <- lmer(Perc_Aphids_Dropped ~ Light* num_Temperature + (1 | Chamber_Num), data = dat3)
summary(lmm5)

plot_model(lmm5, type = "pred", terms = c("num_Temperature", "Light"), col=c('darkgoldenrod', 'darkblue')) + theme_classic()
plot_model(lmm5, type = "int", terms = c("Temperature", "Light"), col=c('darkgoldenrod', 'blue')) + theme_classic()


# run a lm
lm2 <- lm(Perc_Aphids_Dropped ~ Light * Temperature, data = dat4)
summary(lm2)

# run a glm
lmm3.1 <- glm(Perc_Aphids_Dropped ~ Light * Temperature, family=binomial(), weights=dat3$Num_Aphids_Prior , data=dat3)
summary(lmm3.1)

# run a glmm
lmm4 <- glmer(Perc_Aphids_Dropped ~ Light + Temperature + (1 | Chamber_Num), family = binomial, weights=dat3$Num_Aphids_Prior, data = dat3)
summary(lmm4)
emmeans(lmm4, pairwise~Light|Temperature)

### THE FINAL MODEL ###
dat3$Light <- as.factor(dat3$Light)
dat3$Temperature <- as.factor(dat3$Temperature)
lmm4.1 <- glmer(Perc_Aphids_Dropped ~ Light * Temperature + (1 | Chamber_Num), family = binomial, weights=dat3$Num_Aphids_Prior, data = dat3)
summary(lmm4.1)
e1 <- emmeans(lmm4.1, pairwise~Light|Temperature)
e2 <- emmeans(lmm4.1, pairwise~Temperature|Light)
plot(e1, comparisons=T) + theme_sjplot2() + scale_color_manual('goldenrod', 'goldenrod')
plot(e2, comparison=T) + theme_sjplot2()
pwpp(lmm4.1) + theme_sjplot2()

plot_model(lmm4.1, type = "pred", terms = c("Temperature", "Light"), col=c('darkgoldenrod', 'darkblue')) + theme_classic()
plot_model(lmm4.1, type = "int", terms = c("Temperature", "Light"), col=c('darkgoldenrod', 'blue')) + theme_classic()

head(dat3)
d1 <- dat3[dat3$Temperature==15 & dat3$Light == 'Treat',]
d2 <- dat3[dat3$Temperature == 25 & dat3$Light == 'Control',]

t <- t.test(d1, d2)

#### The Final Model and Final Plots ####

### THE FINAL MODEL ###

#set up data
dat3$Light <- as.factor(dat3$Light)
dat3$Temperature <- as.factor(dat3$Temperature)

# run final model
lmm4.1 <- glmer(Perc_Aphids_Dropped ~ Light * Temperature + (1 | Chamber_Num), family = binomial, weights=dat3$Num_Aphids_Prior, data = dat3)
summary(lmm4.1)

#posthoc final model
e1 <- emmeans(lmm4.1, pairwise~Light|Temperature)
e1
e2 <- emmeans(lmm4.1, pairwise~Temperature|Light)
e2

#plot final model
plot(e1, comparisons=T) + theme_sjplot2()
plot(e2, comparison=T) + theme_sjplot2()
