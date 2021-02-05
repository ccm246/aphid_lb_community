# Plant Health and Growth Study #
#################################

# Colleen Miller                #

#### load data and packages    ####

library(ggplot2)
library(sjPlot)
library(lme4)
library(emmeans)
library(patchwork)
library(ggpubr)

setwd('~/Downloads/')
d <- read.csv('Plant Growth and Health Experiment - Sheet1.csv', header=T)

d$Notes <- NULL

d$GrowthEarly <- (d$Plant_Height_Five-d$Plant_Height_Beginning)/5
d$GrowthLate <- (d$Plant_Height_Ten-d$Plant_Height_Five)/5
d$GrowthAll <- (d$Plant_Height_Ten-d$Plant_Height_Beginning)/10
d$Treatment <- as.factor(paste(d$Temp, '/', d$Light))

d$Trt <- 'Cold/Control'
d$Trt[d$Treatment=='25 / 16'] <- 'Warm/Control'
d$Trt[d$Treatment=='15 / 24'] <- 'Cold/Light'
d$Trt[d$Treatment=='25 / 24'] <- 'Warm/Light'

d$Temp <- as.factor(d$Temp)
d$Light <- as.factor(d$Light)


#### initial plots ####

par(mfrow=c(3,3))
plot(d$GrowthEarly~Temp, d, pch=19, col=alpha('pink', .2))
plot(d$GrowthEarly~Light, d, pch=19, col=alpha('red', .2), main='First 5 Days')
plot(d$GrowthEarly~Treatment, d, col='orange')

plot(d$GrowthLate~Temp, d, pch=19, col=alpha('yellow', .2))
plot(d$GrowthLate~Light, d, pch=19, col=alpha('green', .2), main='Last 5 Days')
plot(d$GrowthLate~Treatment, d, col='turquoise')

plot(d$GrowthAll~Temp, d, pch=19, col=alpha('blue', .2))
plot(d$GrowthAll~Light, d, pch=19, col=alpha('purple', .2), main='Total Ten Days')
plot(d$GrowthAll~Treatment, d, col='hotpink')
# it looks like there is overall lower plant growth in the cold treatments, particulary the one with less light
# plants like warmer treatments without the aphids

par(mfrow=c(2,2))
hist(d$GrowthEarly, col='orchid1', main='early plant growth')
hist(d$GrowthLate, col='orange', main='late plant growth')
hist(d$GrowthAll, col='darkred', main='total plant growth')
# data is normally distributed for the most part

#d$Treatment = revalue(d$Treatment, c('15/16' = 'Cold/Control', '15/24' = 'Cold/Light', '25/16' = 'Warm/Control', '25/24' = 'Warm/Light'))

# early
e <- ggplot(d, aes(x=Treatment, y=GrowthEarly, fill=Treatment))+ theme_linedraw() + theme(text = element_text(size=15))+ xlab('Treatment') + ylab('Plant Growth (cm/day)')
e + geom_boxplot(position=position_dodge(1)) + theme_classic() + scale_fill_manual(values=c('cornflowerblue','darkblue',"tomato1",'tomato4')) + theme(text = element_text(size=15)) + theme(text = element_text(size=15), legend.position = 'none')



# late
l <- ggplot(d, aes(x=Temp, y=GrowthLate, fill=interaction(Light, Temp)))+ theme_linedraw() + theme(text = element_text(size=15))+ xlab('Temperature') + ylab('Plant Growth (cm/day)')
l + geom_boxplot(position=position_dodge(1)) + theme_linedraw() + scale_fill_manual(values=c('cornflowerblue','darkblue', "tomato1",'tomato4')) + theme(text = element_text(size=15)) + theme(text = element_text(size=15))

# all
a <- ggplot(d, aes(x=Temp, y=GrowthAll, fill=interaction(Light, Temp)))+ theme_linedraw() + theme(text = element_text(size=15))+ xlab('Temperature') + ylab('Plant Growth (cm/day)')
a + geom_boxplot(position=position_dodge(1)) + theme_linedraw() + scale_fill_manual(values=c('cornflowerblue','darkblue', "tomato1",'tomato4')) + theme(text = element_text(size=15))+ theme(text = element_text(size=15))


r <- ggplot(d, aes(x=Treatment, y=GrowthAll, fill=Treatment)) + theme_linedraw() + theme(text = element_text(size=15))+ xlab('Treatment') + ylab('Plant Growth (cm/day)')
r + geom_boxplot(position=position_dodge(1)) + theme_linedraw() + scale_fill_manual(values=c('white','grey80', "grey60",'grey30')) + theme(text = element_text(size=15))+ theme(text = element_text(size=15), legend.position = 'none')

lm1 <- lm(GrowthAll ~ Light*Temp, d)
plot_model(lm1, type='int', colors=c('cornflowerblue', 'tomato1')) + theme_classic()



#### FIGURE 4 ####

tiff("ecology_ms_figure4a", units="in", width=7, height=7, res=600)

q1 <- ggplot(d, aes(x=Trt, y=GrowthEarly, fill=Trt)) + geom_boxplot(position=position_dodge(4)) + geom_jitter() + theme_classic() + theme(text = element_text(size=15))+ xlab('Treatment') + ylab('Plant Growth (cm/day)') 
q1 + ggtitle('Day 5') + labs(tag ='A') + scale_fill_manual(values=c(alpha("navy", .8), alpha('lightslateblue', .8), alpha("moccasin", .8), alpha('orange', .8), alpha("navy", .8), alpha('mediumslateblue', .8), alpha("moccasin", .8), alpha('orange', .8), alpha("navy", .8), alpha('mediumslateblue', .8), alpha("moccasin", .8), alpha('orange', .8))) + theme(text = element_text(size=15), axis.text.x = element_text(angle = 50, hjust = 1)) + theme(text = element_text(size=15), legend.position='none')                                                                                                                                                       
dev.off()


tiff("ecology_ms_figure4b", units="in", width=7, height=7, res=600)

q2 <- ggplot(d, aes(x=Trt, y=GrowthAll, fill=Trt)) + geom_boxplot(position=position_dodge(4)) + geom_jitter() + theme_classic() + theme(text = element_text(size=15))+ xlab('Treatment') + ylab('Plant Growth (cm/day)') 
q2 + ggtitle('Day 10') + labs(tag ='B') + scale_fill_manual(values=c(alpha("navy", .8), alpha('lightslateblue', .8), alpha("moccasin", .8), alpha('orange', .8), alpha("navy", .8), alpha('mediumslateblue', .8), alpha("moccasin", .8), alpha('orange', .8), alpha("navy", .8), alpha('mediumslateblue', .8), alpha("moccasin", .8), alpha('orange', .8))) + theme(text = element_text(size=15), axis.text.x = element_text(angle = 50, hjust = 1)) + theme(text = element_text(size=15), legend.position='none')                                                                                                                                                       
dev.off()


tiff("ecology_ms_figure5.tif", units="in", width=7, height=4.5, res=600)

q1 <- ggplot(d, aes(x=Trt, y=GrowthEarly, fill=Trt)) + geom_boxplot(position=position_dodge(4)) + geom_jitter() + theme_classic() + theme(text = element_text(size=15))+ labs(title='Day 5', x='Treatment', y='Plant Growth (cm/day)') + labs(tag ='A') + scale_fill_manual(values=c(alpha("navy", .8), alpha('lightslateblue', .8), alpha("moccasin", .8), alpha('orange', .8), alpha("navy", .8), alpha('mediumslateblue', .8), alpha("moccasin", .8), alpha('orange', .8), alpha("navy", .8), alpha('mediumslateblue', .8), alpha("moccasin", .8), alpha('orange', .8))) + theme(text = element_text(size=15), axis.text.x = element_text(angle = 50, hjust = 1)) + theme(text = element_text(size=15), legend.position='none')                                                                                                                                                       

q2 <- ggplot(d, aes(x=Trt, y=GrowthAll, fill=Trt)) + geom_boxplot(position=position_dodge(4)) + geom_jitter() + theme_classic() + theme(text = element_text(size=15))+ xlab('Treatment') + ylab('Plant Growth (cm/day)') + ggtitle('Day 10') + labs(tag ='B') + scale_fill_manual(values=c(alpha("navy", .8), alpha('lightslateblue', .8), alpha("moccasin", .8), alpha('orange', .8), alpha("navy", .8), alpha('mediumslateblue', .8), alpha("moccasin", .8), alpha('orange', .8), alpha("navy", .8), alpha('mediumslateblue', .8), alpha("moccasin", .8), alpha('orange', .8))) + theme(text = element_text(size=15), axis.text.x = element_text(angle = 50, hjust = 1)) + theme(text = element_text(size=15), legend.position='none')                                                                                                                                                       

q3 <- ggarrange(q1, q2, ncol=2, nrow=1)

q3

dev.off()

#### modeling ####

lmm1 <- lmer(GrowthAll ~ Light*Temp + (1|Chamber_Name), d)
summary(lmm1)

e1 <- emmeans(lmm1, pairwise ~ Light|Temp)
e1
plot(e1, comparisons=T) + theme_sjplot2()
e1.1 <- emmeans(lmm1, pairwise ~ Temp|Light)
e1.1
plot(e1.1, comparison=T) + theme_sjplot2()

# no significant differences in plant growth rate between treatments overall
# however, it does look like plant growth rate was potentially lower in the cool, control treatment

lmm2 <- lmer(GrowthEarly ~ Light*Temp + (1|Chamber_Name), d)
summary(lmm2)

e2 <- emmeans(lmm2, pairwise ~ Light|Temp)
e2
plot(e2, comparisons=T) + theme_sjplot2()
e2.1 <- emmeans(lmm2, pairwise ~ Temp|Light)
e2.1
plot(e2.1, comparison=T) + theme_sjplot2()

lmm3 <- lmer(GrowthLate ~ Light*Temp + (1|Chamber_Name), d)
summary(lmm3)

e3 <- emmeans(lmm3, pairwise ~ Light|Temp)
e3
plot(e3, comparisons=T) + theme_sjplot2()
e3.1 <- emmeans(lmm3, pairwise ~ Temp|Light)
e3.1
plot(e3.1, comparison=T) + theme_sjplot2()



