# DENSITY WORK #
# COLLEEN MILLER #
# COLLAB. WITH JENNIFER THALER, TODD UGINE AND MAREN VITOUSEK #

library(lme4)
library(ggplot2)
library(sjPlot)
library(car)
library(gridBase)
library(grid)

setwd("~/Desktop/Cornell_Remnant/2018-2019/Thaler Rotation ")

dat3 <- read.csv("Dropping Trial Data Sheet.csv", header=T)
head(dat3)

# remove notes column so I can remove NAs
dat3$Notes <- NULL
dat3 <- na.omit(dat3)

# remove NAs
dat3$ID <- c(1:nrow(dat3))

lmm2 <- lmer(Perc_Aphids_Dropped ~ Num_Aphids_Prior + ( 1 | Treatment), data=dat3)
summary(lmm2)

# transformations
dat3$scale_Num <- scale(dat3$Num_Aphids_Prior)
dat3$scale_Prop <- scale(dat3$Perc_Aphids_Dropped)
dat3$log_Prop <- log(dat3$Perc_Aphids_Dropped)
dat3$log_Num <- log(dat3$Num_Aphids_Prior)

# plots
plot(Perc_Aphids_Dropped ~ Num_Aphids_Prior, dat3, col=alpha('blue', .3), pch=19)
plot(Perc_Aphids_Dropped ~ Num_Aphids_Prior, dat3, col=dat3$Treatment)
plot(scale_Prop ~ scale_Num, dat3, col=alpha('darkblue', .3), pch=19, xlab='Scaled Number Aphids Per Plant', ylab='Scaled Proportion Aphids Dropped From Plant')
plot(log_Prop ~ scale_Num, dat3, col=alpha('darkblue', .3), pch=19, xlab='Scaled Number Aphids Per Plant', ylab='Scaled Proportion Aphids Dropped From Plant')
plot(log_Prop ~ log_Num, dat3, col=alpha('darkblue', .3), pch=19, xlab='Scaled Number Aphids Per Plant', ylab='Scaled Proportion Aphids Dropped From Plant')
plot(scale_Prop ~ scale_Num, dat3, col=alpha('darkblue', .3), pch=19, xlab='Scaled Number Aphids Per Plant', ylab='Scaled Proportion Aphids Dropped From Plant')

# do logistic regression with non-zeros as 1s
# drop zeros, then run linear/poisson/binomial
# # overdispersion poisson versus binomial ?
# # does it have boundaries?
# # is it skewed? 

# original data, no transformations, no nothing
log_lm <- glm(Perc_Aphids_Dropped ~ Num_Aphids_Prior, dat3, family=binomial, weights = Num_Aphids_Prior)
summary(log_lm)

# not sure if can put num aphids prior and treatment conditions in same model since they're not indepedent
log_lm_int <- glm(Perc_Aphids_Dropped ~ Num_Aphids_Prior*Treatment, dat3, family=binomial, weights = Num_Aphids_Prior)
summary(log_lm_int)

log_lm_int_2 <- glm(Perc_Aphids_Dropped ~ Num_Aphids_Prior*Light*Temperature, dat3, family=binomial, weights = Num_Aphids_Prior)
summary(log_lm_int_2)

log_lm_int_3 <- glm(Perc_Aphids_Dropped ~ Light*Temperature, dat3, family=binomial, weights = Num_Aphids_Prior)
summary(log_lm_int_3)

log_lmm <- glmer(Perc_Aphids_Dropped ~ Num_Aphids_Prior + (1 | Treatment), dat3, family=binomial, weights = Num_Aphids_Prior)
summary(log_lmm)

par(mfrow=c(1,1))
plot(Perc_Aphids_Dropped ~ Num_Aphids_Prior, dat3[dat3$Treatment==1,], col=alpha('darkblue', .7), pch=19, cex=2, xlab = 'Total Number of Aphids on Plant', ylab = 'Proportion Aphids Dropped', main = 'Original Data')
points(Perc_Aphids_Dropped ~ Num_Aphids_Prior, dat3[dat3$Treatment==2,], col=alpha('lightblue', .7), pch=19, cex=2)
points(Perc_Aphids_Dropped ~ Num_Aphids_Prior, dat3[dat3$Treatment==3,], col=alpha('darkred', .7), pch=15, cex=2)
points(Perc_Aphids_Dropped ~ Num_Aphids_Prior, dat3[dat3$Treatment==4,], col=alpha('pink', .7), pch=15, cex=2)
legend(x = 31, y = .65, cex = c(1,1,1,1), pch=c(19,19,15,15), col=c('darkblue', 'lightblue', 'darkred', 'pink'), legend=c("Dark/Cold", "Light/Cold", "Dark/Hot", "Light/Hot"))

newdat <- data.frame(Num_Aphids_Prior=seq(min(dat3$Num_Aphids_Prior), max(dat3$Num_Aphids_Prior),len=100))
newdat$Perc_Aphids_Dropped = predict(log_lm, newdata=newdat, type="response")
lines(Perc_Aphids_Dropped ~ Num_Aphids_Prior, newdat, col="black", lwd=2)

par(mfrow=c(2,2))
plot(Perc_Aphids_Dropped ~ Num_Aphids_Prior, dat3[dat3$Treatment==1,], col=alpha('darkblue', .7), pch=19, cex=2, xlab = 'Total Number of Aphids on Plant', ylab = 'Proportion Aphids Dropped', main = '16:8, 15 C')
plot(Perc_Aphids_Dropped ~ Num_Aphids_Prior, dat3[dat3$Treatment==2,], col=alpha('lightblue', .7), pch=19, cex=2, xlab = 'Total Number of Aphids on Plant', ylab = 'Proportion Aphids Dropped', main = '24:0, 15 C')
plot(Perc_Aphids_Dropped ~ Num_Aphids_Prior, dat3[dat3$Treatment==3,], col=alpha('darkred', .7), pch=19, cex=2, xlab = 'Total Number of Aphids on Plant', ylab = 'Proportion Aphids Dropped', main = '16:8, 25 C')
plot(Perc_Aphids_Dropped ~ Num_Aphids_Prior, dat3[dat3$Treatment==4,], col=alpha('pink', .7), pch=19, cex=2, xlab = 'Total Number of Aphids on Plant', ylab = 'Proportion Aphids Dropped', main = '24:0, 25 C')


# logistic regression with plant as the unit
log_lm <- glm(Bin_Aphids_Dropped ~ Num_Aphids_Prior, dat3, family=binomial(link = "logit"))
summary(log_lm)

# not sure if can put num aphids prior and treatment conditions in same model since they're not indepedent
log_lm_int <- glm(Bin_Aphids_Dropped ~ Num_Aphids_Prior*Treatment, dat3, family=binomial(link = "logit"))
summary(log_lm_int)

log_lm_int_2 <- glm(Bin_Aphids_Dropped ~ Num_Aphids_Prior*Light*Temperature, dat3, family=binomial(link = "logit"))
summary(log_lm_int_2)

log_lm_int_3 <- glm(Bin_Aphids_Dropped ~ Light*Temperature, dat3, family=binomial(link = "logit"))
summary(log_lm_int_3)

log_lmm <- glmer(Bin_Aphids_Dropped ~ Num_Aphids_Prior + (1 | Treatment), dat3, family=binomial(link = "logit"))
summary(log_lmm)


##fig label

line2user <- function(line, side) {
  lh <- par('cin')[2] * par('cex') * par('lheight')
  x_off <- diff(grconvertX(c(0, lh), 'inches', 'npc'))
  y_off <- diff(grconvertY(c(.015, lh), 'inches', 'npc'))
  switch(side,
         `1` = grconvertY(-line * y_off, 'npc', 'user'),
         `2` = grconvertX(-line * x_off, 'npc', 'user'),
         `3` = grconvertY(1 + line * y_off, 'npc', 'user'),
         `4` = grconvertX(1 + line * x_off, 'npc', 'user'),
         stop("Side must be 1, 2, 3, or 4", call.=FALSE))
}

addfiglab <- function(lab, xl = par()$mar[2], yl = par()$mar[3]) {
  
  text(x = line2user(xl, 2), y = line2user(yl, 3), 
       lab, xpd = NA, font = 1, cex = 1.3, adj = c(0, 1))
  
}

#### FIGURE 3B ####

tiff("ecology_ms_figure3b", units="in", width=7, height=7, res=600)

par(mfrow=c(1,1))
plot(Bin_Aphids_Dropped ~ Num_Aphids_Prior, dat3[dat3$Treatment==1,], col=alpha('navy', .8), pch=19, cex=2, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, xlab = 'Total Number Aphids on Plant', ylab = 'Did Aphids Drop (1/0)', main = '')
points(Bin_Aphids_Dropped ~ Num_Aphids_Prior, dat3[dat3$Treatment==2,], col=alpha('slateblue', .8), pch=15, cex=2)
points(Bin_Aphids_Dropped ~ Num_Aphids_Prior, dat3[dat3$Treatment==3,], col=alpha('moccasin', .8), pch=19, cex=2)
points(Bin_Aphids_Dropped ~ Num_Aphids_Prior, dat3[dat3$Treatment==4,], col=alpha('orange', .8), pch=15, cex=2)
legend(x = 28, y = .3, cex = c(1,1,1,1), pch=c(19,15,19,15), col=c('navy', 'slateblue', 'moccasin', 'orange'), legend=c("Cold/Control", "Cold/Light", "Warm/Control", "Warm/Light"))

newdat <- data.frame(Num_Aphids_Prior=seq(min(dat3$Num_Aphids_Prior), max(dat3$Num_Aphids_Prior),len=100))
newdat$Perc_Aphids_Dropped = predict(log_lm, newdata=newdat, type="response")
lines(Perc_Aphids_Dropped ~ Num_Aphids_Prior, newdat, col="black", lwd=2)

addfiglab('B')
 
dev.off()

###after loading and naming dropping figure in aphid_drop_exp script
tiff("ecology_ms_figure4.tif", units="in", width=8, height=5, res=600)

#ggarrange(p2, ncol=2, nrow=1)

par(mfrow=c(1,2))

plot.new()

plot(Bin_Aphids_Dropped ~ Num_Aphids_Prior, dat3[dat3$Treatment==1,], col=alpha('navy', .8), pch=19, cex=2, cex.lab=1, cex.axis=1, cex.main=1, cex.sub=1, xlab = 'Total Number Aphids on Plant', ylab = 'Did Aphids Drop (1/0)', main = '')
points(Bin_Aphids_Dropped ~ Num_Aphids_Prior, dat3[dat3$Treatment==2,], col=alpha('slateblue', .8), pch=15, cex=2)
points(Bin_Aphids_Dropped ~ Num_Aphids_Prior, dat3[dat3$Treatment==3,], col=alpha('moccasin', .8), pch=19, cex=2)
points(Bin_Aphids_Dropped ~ Num_Aphids_Prior, dat3[dat3$Treatment==4,], col=alpha('orange', .8), pch=15, cex=2)
legend(x = 28, y = .3, cex = c(.5,.5,.5,.5), pch=c(19,15,19,15), col=c('navy', 'slateblue', 'moccasin', 'orange'), legend=c("Cold/Control", "Cold/Light", "Warm/Control", "Warm/Light"))

newdat <- data.frame(Num_Aphids_Prior=seq(min(dat3$Num_Aphids_Prior), max(dat3$Num_Aphids_Prior),len=100))
newdat$Perc_Aphids_Dropped = predict(log_lm, newdata=newdat, type="response")
lines(Perc_Aphids_Dropped ~ Num_Aphids_Prior, newdat, col="black", lwd=2)

addfiglab('B')

vp <- viewport (height = unit(1, 'npc'), width=unit(0.5, 'npc'), 
                just = c('left', 'top'),
                y=1, x=0)

print(p2, vp=vp)

dev.off()

####

par(mfrow=c(2,2))
plot(Bin_Aphids_Dropped ~ Num_Aphids_Prior, dat3[dat3$Treatment==1,], col=alpha('darkblue', .6), pch=19, cex=2, xlab = 'Total Number Aphids on Plant', ylab = 'Did Aphids Drop (1/0)', main = '16:8, 15 C')
plot(Bin_Aphids_Dropped ~ Num_Aphids_Prior, dat3[dat3$Treatment==2,], col=alpha('lightblue', .6), pch=19, cex=2, xlab = 'Total Number Aphids on Plant', ylab = 'Did Aphids Drop (1/0)', main = '24:0, 15 C')
plot(Bin_Aphids_Dropped ~ Num_Aphids_Prior, dat3[dat3$Treatment==3,], col=alpha('darkred', .6), pch=19, cex=2, xlab = 'Total Number Aphids on Plant', ylab = 'Did Aphids Drop (1/0)', main = '16:8, 25 C')
plot(Bin_Aphids_Dropped ~ Num_Aphids_Prior, dat3[dat3$Treatment==4,], col=alpha('pink', .6), pch=19, cex=2, xlab = 'Total Number Aphids on Plant', ylab = 'Did Aphids Drop (1/0)', main = '24:0, 25 C')


# binomial regression, with zeroes dropped
dat4 <- dat3[dat3$Perc_Aphids_Dropped > 0,]

bin_lm <- glm(Perc_Aphids_Dropped ~ Num_Aphids_Prior, data = dat4, family = binomial, weights=dat4$Num_Aphids_Prior)
summary(bin_lm)

bin_lm_int <- glm(Perc_Aphids_Dropped ~ Num_Aphids_Prior*Treatment, data = dat4, family = binomial, weights=dat4$Num_Aphids_Prior)
summary(bin_lm_int)

# not enough data for this? Barely enough?
bin_lm_int_2 <- glm(Perc_Aphids_Dropped ~ Num_Aphids_Prior*Light*Temperature, data = dat4, family = binomial, weights=dat4$Num_Aphids_Prior)
summary(bin_lm_int_2)

bin_lm_int_2 <- glm(Perc_Aphids_Dropped ~Light*Temperature, data = dat4, family = binomial, weights=dat4$Num_Aphids_Prior)
summary(bin_lm_int_2)

bin_lmm <- glmer(Perc_Aphids_Dropped ~ Num_Aphids_Prior + (1 | Treatment), data = dat4, family = binomial, weights=dat4$Num_Aphids_Prior)
summary(bin_lmm)

par(mfrow=c(1,1))
plot(Perc_Aphids_Dropped ~ Num_Aphids_Prior, dat4[dat4$Treatment==1,], col=alpha('darkblue', .7), pch=19, cex=2, xlab = 'Total Number of Aphids on Plant', ylab = 'Proportion Aphids Dropped', main='Zeroes Dropped Data')
points(Perc_Aphids_Dropped ~ Num_Aphids_Prior, dat4[dat4$Treatment==2,], col=alpha('lightblue', .7), pch=19, cex=2)
points(Perc_Aphids_Dropped ~ Num_Aphids_Prior, dat4[dat4$Treatment==3,], col=alpha('darkred', .7), pch=15, cex=2)
points(Perc_Aphids_Dropped ~ Num_Aphids_Prior, dat4[dat4$Treatment==4,], col=alpha('pink', .7), pch=15, cex=2)
legend(x = 31, y = .65, cex = c(1,1,1,1), pch=c(19,19,15,15), col=c('darkblue', 'lightblue', 'darkred', 'pink'), legend=c("Dark/Cold", "Light/Cold", "Dark/Hot", "Light/Hot"))

# fit line
newdat <- data.frame(Num_Aphids_Prior=seq(min(dat3$Num_Aphids_Prior), max(dat3$Num_Aphids_Prior),len=100))
newdat$Perc_Aphids_Dropped = predict(bin_lm, newdata=newdat, type="response")
lines(Perc_Aphids_Dropped ~ Num_Aphids_Prior, newdat, col="black", lwd=2)

 # # fit confidence intervals
 # newx <- seq(min(dat3$Num_Aphids_Prior), max(dat3$Num_Aphids_Prior), length.out=100)
 # preds <- predict(bin_lm, newdata = data.frame(Num_Aphids_Prior=newx), 
 #                  interval = 'confidence')
 # 
 # polygon(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]), col = 'grey60', border = NA)
 # # model
 # abline(bin_lm)
 # # intervals
 # lines(newx, preds[ ,3], lty = 'dashed', col = 'grey')
 # lines(newx, preds[ ,2], lty = 'dashed', col = 'grey')

par(mfrow=c(2,2))
plot(Perc_Aphids_Dropped ~ Num_Aphids_Prior, dat4[dat4$Treatment==1,], col=alpha('darkblue', .7), pch=19, cex=2, xlab = 'Total Number of Aphids on Plant', ylab = 'Proportion Aphids Dropped', main = '16:8, 15 C')
plot(Perc_Aphids_Dropped ~ Num_Aphids_Prior, dat4[dat4$Treatment==2,], col=alpha('lightblue', .7), pch=19, cex=2, xlab = 'Total Number of Aphids on Plant', ylab = 'Proportion Aphids Dropped', main = '24:0, 15 C')
plot(Perc_Aphids_Dropped ~ Num_Aphids_Prior, dat4[dat4$Treatment==3,], col=alpha('darkred', .7), pch=19, cex=2, xlab = 'Total Number of Aphids on Plant', ylab = 'Proportion Aphids Dropped', main = '16:8, 25 C')
plot(Perc_Aphids_Dropped ~ Num_Aphids_Prior, dat4[dat4$Treatment==4,], col=alpha('pink', .7), pch=19, cex=2, xlab = 'Total Number of Aphids on Plant', ylab = 'Proportion Aphids Dropped', main = '24:0, 25 C')

# logistic regression with aphid as the unit

outmat <- matrix(ncol = 2, nrow = sum(dat3$Num_Aphids_Prior))
count <- 1
for (i in 1: nrow(dat3))
{
  stop <- dat3$Num_Aphids.Dropped[i]
  
  for (j in 1:dat3$Num_Aphids_Prior[i])
  { 
    if (j > stop)
    { 
      outmat[count, 1] <- dat3$ID[i]
      outmat[count, 2] <- 0
    }
    else
    { 
      outmat[count, 1] <- dat3$ID[i]
      outmat[count, 2] <- 1
    }
    count <- count + 1
    }
}

outmat
dat1 <- as.data.frame(outmat)
head(dat1)

colnames(dat1) <- c('ID', 'Indiv_Drop')

dat2 <- merge(dat3, dat1, by='ID')

log_lm <- glm(Indiv_Drop ~ Num_Aphids_Prior, dat2, family=binomial(link = "logit"))
summary(log_lm)

# not sure if can put num aphids prior and treatment conditions in same model since they're not indepedent
log_lm_int <- glm(Indiv_Drop ~ Num_Aphids_Prior*Treatment, dat2, family=binomial(link = "logit"))
summary(log_lm_int)

log_lm_int_2 <- glm(Indiv_Drop ~ Num_Aphids_Prior*Light*Temperature, dat2, family=binomial(link = "logit"))
summary(log_lm_int_2)

log_lm_int_3 <- glm(Indiv_Drop ~ Light*Temperature, dat2, family=binomial(link = "logit"))
summary(log_lm_int_3)

log_lmm <- glmer(Indiv_Drop ~ Num_Aphids_Prior + (1 | Treatment), dat2, family=binomial(link = "logit"))
summary(log_lmm)

par(mfrow=c(1,1))
plot(Indiv_Drop ~ Num_Aphids_Prior, dat2[dat2$Treatment==1,], col=alpha('darkblue', .6), pch=19, cex=2, xlab = 'Total Number Aphids on Plant', ylab = 'Did Aphid Drop (1/0)', main = 'Binary Data: Aphid as Unit')
points(Indiv_Drop ~ Num_Aphids_Prior, dat2[dat2$Treatment==2,], col=alpha('lightblue', .6), pch=19, cex=2)
points(Indiv_Drop ~ Num_Aphids_Prior, dat2[dat2$Treatment==3,], col=alpha('darkred', .6), pch=15, cex=2)
points(Indiv_Drop ~ Num_Aphids_Prior, dat2[dat2$Treatment==4,], col=alpha('pink', .6), pch=15, cex=2)
legend(x = 30, y = .4, cex = c(1,1,1,1), pch=c(19,19,15,15), col=c('darkblue', 'lightblue', 'darkred', 'pink'), legend=c("Dark/Cold", "Light/Cold", "Dark/Hot", "Light/Hot"))

newdat <- data.frame(Num_Aphids_Prior=seq(min(dat2$Num_Aphids_Prior), max(dat2$Num_Aphids_Prior),len=100))
newdat$Indiv_Drop = predict(log_lm, newdata=newdat, type="response")
lines(Indiv_Drop ~ Num_Aphids_Prior, newdat, col="black", lwd=2)

par(mfrow=c(2,2))
plot(Indiv_Drop ~ Num_Aphids_Prior, dat2[dat2$Treatment==1,], col=alpha('darkblue', .6), pch=19, cex=2, xlab = 'Total Number Aphids on Plant', ylab = 'Did Aphid Drop (1/0)', main = '16:8, 15 C')
plot(Indiv_Drop ~ Num_Aphids_Prior, dat2[dat2$Treatment==2,], col=alpha('lightblue', .6), pch=19, cex=2, xlab = 'Total Number Aphids on Plant', ylab = 'Did Aphid Drop (1/0)', main = '24:0, 15 C')
plot(Indiv_Drop ~ Num_Aphids_Prior, dat2[dat2$Treatment==3,], col=alpha('darkred', .6), pch=19, cex=2, xlab = 'Total Number Aphids on Plant', ylab = 'Did Aphid Drop (1/0)', main = '16:8, 25 C')
plot(Indiv_Drop ~ Num_Aphids_Prior, dat2[dat2$Treatment==4,], col=alpha('pink', .6), pch=19, cex=2, xlab = 'Total Number Aphids on Plant', ylab = 'Did Aphid Drop (1/0)', main = '24:0, 25 C')


