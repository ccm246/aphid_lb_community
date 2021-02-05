###############################################################
#                     Food Web Study Analysis                 #
###############################################################
# Colleen Miller, July 2019                                   #

# Background: C7 and CMAC are both common predators of pea 
# aphids. C7 is an invasive and voracious predator of the aphid
# that focuses largely on their carnivorous diet. They are known 
# to hunt using vision. CMAC are more omnivorous predators and 
# much smaller in size than the C7. They are native and not 
# known to rely on vision to hunt. 

#### data set up                                 ####

# library necessary packages
# install.packages("ggplot2")
# install.packages("outlier")
# install.packages("sjPlot")
# install.packages("lme4")
# install.packages("nlme")
# install.packages("emmeans")
# install.packages("dabestr")
# install.packages("wesanderson")

library(ggplot2)
library(outlier)
library(sjPlot)
library(lme4)
library(nlme)
library(emmeans)
library(dabestr)
library(wesanderson)

#create outlier function and test
outlierKD <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  tot <- sum(!is.na(var_name))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  message("Outliers identified: ", na2 - na1, " from ", tot, " observations")
  message("Proportion (%) of outliers: ", (na2 - na1) / tot*100)
  message("Mean of the outliers: ", mo)
  m2 <- mean(var_name, na.rm = T)
  message("Mean without removing outliers: ", m1)
  message("Mean if we remove outliers: ", m2)
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    message("Outliers successfully removed", "\n")
    return(invisible(dt))
  } else{
    message("Nothing changed", "\n")
    return(invisible(var_name))
  }
}

# read in data sheet
setwd('~/Downloads/')
d <- read.csv('Food Web Study Data Sheet - Sheet1.csv', header=T)
# remove powers chamber data: chamber overheated and killed everything
d <- d[!d$Location_Name=='Powers',]
d$Species = revalue(d$Species, c('C7' = 'C. septempunctata', 'CMAC' = 'C. maculata'))


# add in columns for plant growth, and factorize other columns
d$Plant_Growth <- ((d$Plant_Heigh_End2+d$Plant_Height_End1)/2)-((d$Plant_Height_Start1+d$Plant_Height_Start2)/2)
d$Treatment <- as.factor(d$Treatment)
d$Light <- as.factor(d$Light)
d$Temp <- as.factor(d$Temp)
d$scale_Num <- scale(d$Num_Aphids_DayTen)
d$Treatment <- as.factor(d$Treatment)
d$scale_Light <- as.factor(d$Light)
d$scale_Temp <- as.factor(d$Temp)

# create numeric treatment column
d$Trt <- 'Cold/Control'
d$Trt[d$Treatment==2] <- 'Warm/Control'
d$Trt[d$Treatment==3] <- 'Cold/Light'
d$Trt[d$Treatment==4] <- 'Warm/Light'

# create growth rate columns
d$Growth_Rate_Late <- (d$Num_Aphids_DayTen-d$Num_Aphids_DayFive)/5
d$Growth_Rate_Early <- (d$Num_Aphids_DayFive - d$Num_Aphids_Innoc)/5
d$Growth_Average <- (d$Growth_Rate_Early+d$Growth_Rate_Late)/2
d$Growth_Total <- (d$Num_Aphids_DayTen-d$Num_Aphids_Innoc)/10

# create data frames specific to each species and the control at day five
d5 <- d[d$Species =='control',]
dc7 <- d[d$Species == 'C7',]
dcmac <- d[d$Species == 'CMAC',]
dc <- d[d$Species == 'control',]
d5$dayfive <- as.numeric(d5$Num_Aphids_DayFive)

## mean pop levels for CMAC on days 5 and ten
# trt 1
c1 <- c(60, mean(d$Num_Aphids_DayFive[d$Species=='CMAC' & d$Treatment==1]), mean(d$Num_Aphids_DayTen[d$Species=='CMAC' & d$Treatment==1]))
# trt 2
c2 <- c(60, mean(d$Num_Aphids_DayFive[d$Species=='CMAC' & d$Treatment==2]), mean(d$Num_Aphids_DayTen[d$Species=='CMAC' & d$Treatment==2]))
# trt 3
c3 <- c(60, mean(d$Num_Aphids_DayFive[d$Species=='CMAC' & d$Treatment==3]), mean(d$Num_Aphids_DayTen[d$Species=='CMAC' & d$Treatment==3]))
# trt 4
c4 <- c(60, mean(d$Num_Aphids_DayFive[d$Species=='CMAC' & d$Treatment==4]), mean(d$Num_Aphids_DayTen[d$Species=='CMAC' & d$Treatment==4]))

## mean pop levels for C7 on days 5 and ten
# trt 1
s1 <- c(60, mean(d$Num_Aphids_DayFive[d$Species=='C7' & d$Treatment==1]), mean(d$Num_Aphids_DayTen[d$Species=='C7' & d$Treatment==1]))
# trt 2
s2 <- c(60, mean(d$Num_Aphids_DayFive[d$Species=='C7' & d$Treatment==2]), mean(d$Num_Aphids_DayTen[d$Species=='C7' & d$Treatment==2]))
# trt 3
s3 <- c(60, mean(d$Num_Aphids_DayFive[d$Species=='C7' & d$Treatment==3]), mean(d$Num_Aphids_DayTen[d$Species=='C7' & d$Treatment==3]))
# trt 4
s4 <- c(60, mean(d$Num_Aphids_DayFive[d$Species=='C7' & d$Treatment==4]), mean(d$Num_Aphids_DayTen[d$Species=='C7' & d$Treatment==4]))


## mean pop levels for CONTROL on days 5 and ten
# trt 1
a1 <- c(60, mean(d$Num_Aphids_DayFive[d$Species=='control' & d$Treatment==1]), mean(d$Num_Aphids_DayTen[d$Species=='control' & d$Treatment==1]))
# trt 2
a2 <- c(60, mean(d$Num_Aphids_DayFive[d$Species=='control' & d$Treatment==2]), mean(d$Num_Aphids_DayTen[d$Species=='control' & d$Treatment==2]))
# trt 3
a3 <- c(60, mean(d$Num_Aphids_DayFive[d$Species=='control' & d$Treatment==3]), mean(d$Num_Aphids_DayTen[d$Species=='control' & d$Treatment==3]))
# trt 4
a4 <- c(60, mean(d$Num_Aphids_DayFive[d$Species=='control' & d$Treatment==4]), mean(d$Num_Aphids_DayTen[d$Species=='control' & d$Treatment==4]))


## Looking at the drop or off leaf levels of aphids. Unsatisfied and angry aphids. 

d$Aphid_Drop_DayFive[30] <- NA

## mean DROP/off plant levels for CMAC on days 5 and ten
# trt 1
dc1 <- c(0, mean(d$Aphid_Drop_DayFive[d$Species=='CMAC' & d$Treatment==1]), mean(d$Aphid_Drop_DayTen[d$Species=='CMAC' & d$Treatment==1]))
# trt 2
dc2 <- c(0, mean(d$Aphid_Drop_DayFive[d$Species=='CMAC' & d$Treatment==2]), mean(d$Aphid_Drop_DayTen[d$Species=='CMAC' & d$Treatment==2]))
# trt 3
dc3 <- c(0, mean(d$Aphid_Drop_DayFive[d$Species=='CMAC' & d$Treatment==3]), mean(d$Aphid_Drop_DayTen[d$Species=='CMAC' & d$Treatment==3]))
# trt 4
dc4 <- c(0, mean(d$Aphid_Drop_DayFive[d$Species=='CMAC' & d$Treatment==4]), mean(d$Aphid_Drop_DayTen[d$Species=='CMAC' & d$Treatment==4]))

## mean DROP/off plant levels for C7 on days 5 and ten
# trt 1
ds1 <- c(0, mean(d$Aphid_Drop_DayFive[d$Species=='C7' & d$Treatment==1]), mean(d$Aphid_Drop_DayTen[d$Species=='C7' & d$Treatment==1]))
# trt 2
ds2 <- c(0, mean(d$Aphid_Drop_DayFive[d$Species=='C7' & d$Treatment==2]), mean(d$Aphid_Drop_DayTen[d$Species=='C7' & d$Treatment==2]))
# trt 3
ds3 <- c(0, mean(d$Aphid_Drop_DayFive[d$Species=='C7' & d$Treatment==3]), mean(d$Aphid_Drop_DayTen[d$Species=='C7' & d$Treatment==3]))
# trt 4
ds4 <- c(0, mean(d$Aphid_Drop_DayFive[d$Species=='C7' & d$Treatment==4]), mean(d$Aphid_Drop_DayTen[d$Species=='C7' & d$Treatment==4]))


## mean DROP/off plant levels for CONTROL on days 5 and ten
# trt 1
da1 <- c(0, mean(d$Aphid_Drop_DayFive[d$Species=='control' & d$Treatment==1]), mean(d$Aphid_Drop_DayTen[d$Species=='control' & d$Treatment==1]))
# trt 2
da2 <- c(0, mean(d$Aphid_Drop_DayFive[d$Species=='control' & d$Treatment==2]), mean(d$Aphid_Drop_DayTen[d$Species=='control' & d$Treatment==2]))
# trt 3
da3 <- c(0, mean(d$Aphid_Drop_DayFive[d$Species=='control' & d$Treatment==3]), mean(d$Aphid_Drop_DayTen[d$Species=='control' & d$Treatment==3]))
# trt 4
da4 <- c(0, mean(d$Aphid_Drop_DayFive[d$Species=='control' & d$Treatment==4]), mean(d$Aphid_Drop_DayTen[d$Species=='control' & d$Treatment==4]))

d$Aphid_Drop_DayFive[30] <- NA

#### initial plots displaying data distributions ####
par(mfrow=c(2,2))

# plots of all species
plot(Num_Aphids_DayFive~Treatment, d, col=alpha('goldenrod', 1), pch=19)
plot(Num_Aphids_DayFive~Light, d, col=alpha('dodgerblue3', .5), pch=19)
plot(Num_Aphids_DayFive~Temp, d, col=alpha('tomato2', .5), pch=19)
plot(Num_Aphids_DayFive~Species, d, col=alpha('slateblue1'), pch=19)

plot(Num_Aphids_DayTen~Treatment, d, col=alpha('goldenrod', 1), pch=19)
plot(Num_Aphids_DayTen~Light, d, col=alpha('dodgerblue3', .5), pch=19)
plot(Num_Aphids_DayTen~Temp, d, col=alpha('tomato2', .5), pch=19)
plot(Num_Aphids_DayTen~Species, d, col=alpha('slateblue1'), pch=19)

plot(Plant_Growth~Treatment, d, col=alpha('cornflowerblue', 1), pch=19)
plot(Plant_Growth~Light, d, col=alpha('grey60', .5), pch=19)
plot(Plant_Growth~Temp, d, col=alpha('navyblue', .5), pch=19)
plot(Plant_Growth~Species, d, col=alpha('skyblue', .5), pch=19)

# plots of C7 only
par(mfrow=c(1,3))
plot(Num_Aphids_DayFive~Treatment, dc7, col=alpha('goldenrod', 1), pch=19)
plot(Num_Aphids_DayFive~Light, dc7, col=alpha('dodgerblue3', .5), pch=19)
plot(Num_Aphids_DayFive~Temp, dc7, col=alpha('tomato2', .5), pch=19)

plot(Num_Aphids_DayTen~Treatment, dc7, col=alpha('goldenrod', 1), pch=19)
plot(Num_Aphids_DayTen~Light, dc7, col=alpha('dodgerblue3', .5), pch=19)
plot(Num_Aphids_DayTen~Temp, dc7, col=alpha('tomato2', .5), pch=19)

plot(Plant_Growth~Treatment, dc7, col=alpha('cornflowerblue', 1), pch=19)
plot(Plant_Growth~Light, dc7, col=alpha('grey60', .5), pch=19)
plot(Plant_Growth~Temp, dc7, col=alpha('navyblue', .5), pch=19)

# plots of CMAC only
plot(Num_Aphids_DayFive~Treatment, dcmac, col=alpha('goldenrod', 1), pch=19)
plot(Num_Aphids_DayFive~Light, dcmac, col=alpha('dodgerblue3', .5), pch=19)
plot(Num_Aphids_DayFive~Temp, dcmac, col=alpha('tomato2', .5), pch=19)

plot(Num_Aphids_DayTen~Treatment, dcmac, col=alpha('goldenrod', 1), pch=19)
plot(Num_Aphids_DayTen~Light, dcmac, col=alpha('dodgerblue3', .5), pch=19)
plot(Num_Aphids_DayTen~Temp, dcmac, col=alpha('tomato2', .5), pch=19)

plot(Plant_Growth~Treatment, dcmac, col=alpha('cornflowerblue', 1), pch=19)
plot(Plant_Growth~Light, dcmac, col=alpha('grey60', .5), pch=19)
plot(Plant_Growth~Temp, dcmac, col=alpha('navyblue', .5), pch=19)

# plots of control only
plot(Num_Aphids_DayFive~Treatment, dc, col=alpha('goldenrod', .5), pch=19)
plot(Num_Aphids_DayFive~Light, dc, col=alpha('dodgerblue3', .5), pch=19)
plot(Num_Aphids_DayFive~Temp, dc, col=alpha('tomato2', .5), pch=19)

plot(Num_Aphids_DayTen~Treatment, dc, col=alpha('goldenrod', .5), pch=19)
plot(Num_Aphids_DayTen~Light, dc, col=alpha('dodgerblue3', .5), pch=19)
plot(Num_Aphids_DayTen~Temp, dc, col=alpha('tomato2', .5), pch=19)

plot(Plant_Growth~Treatment, dc, col=alpha('cornflowerblue', 1), pch=19)
plot(Plant_Growth~Light, dc, col=alpha('grey60', .5), pch=19)
plot(Plant_Growth~Temp, dc, col=alpha('navyblue', .5), pch=19)

# histograms of different patterns
par(mfrow=c(2,3))

hist(d$Num_Aphids_DayTen, col='tomato1', main='aphids on day five')
hist(d$Num_Aphids_DayFive, col='cornflowerblue', main='aphids on day ten')
hist(d$Plant_Growth, col='orchid1', main='plant growth')
hist(d$Growth_Average, col='skyblue', main='average growth rate')
hist(d$Aphid_Drop_DayFive, col='mediumorchid', main='day five drop')
hist(d$Aphid_Drop_DayTen, col='slateblue', main='day ten drop')

#### exploratory modeling                        ####

# creating numeric variables can make modeling or plotting easier depending on context
d$Temp <- as.numeric(d$Temp)
d$Light <- as.numeric(d$Light)

# linear models of patterns for aphid population size on days ten and five
lm1 <- lm(Num_Aphids_DayFive ~ Trt*Species, d)
summary(lm1)
lm2 <- lm(Num_Aphids_DayFive ~ Temp*Light*Species, d)
summary(lm2)
lm3 <- lm(Num_Aphids_DayTen ~ Trt*Species, d)
summary(lm3)
lm4 <- lm(Num_Aphids_DayTen ~ Temp*Light*Species, d)
summary(lm4)
lm5 <- lm(Num_Aphids_DayFive ~ Species*Trt, d)
summary(lm5)
lm6 <- lm(Num_Aphids_DayFive ~ Light*Temp*Species, d)
summary(lm5)
lm7 <- lm(Num_Aphids_DayTen ~ Species*Trt, d)
summary(lm6)
lm8 <- lm(Num_Aphids_DayTen ~ Light*Temp*Species, d)
summary(lm7)

# re-factorize the variables
d$Temp <- as.factor(d$Temp)
d$Light <- as.factor(d$Light)

# linear models for aphids on day ten and five for all species
lm1 <- lm(Num_Aphids_DayFive ~ Trt*Species, d)
summary(lm1)
lm2 <- lm(Num_Aphids_DayFive ~ Temp*Light*Species, d)
summary(lm2)
lm3 <- lm(Num_Aphids_DayTen ~ Trt*Species, d)
summary(lm3)
lm4 <- lm(Num_Aphids_DayTen ~ Temp*Light*Species, d)
summary(lm4)
lm5 <- lm(Num_Aphids_DayFive ~ Species*Trt, d)
summary(lm5)
lm6 <- lm(Num_Aphids_DayFive ~ Light*Temp*Species, d)
summary(lm5)
lm7 <- lm(Num_Aphids_DayTen ~ Species*Trt, d)
summary(lm6)
lm8 <- lm(Num_Aphids_DayTen ~ Light*Temp*Species, d)
summary(lm7)

# linear models for aphids on day ten and five for control on day five
lm9 <- lm(Num_Aphids_DayFive ~ Light*Temp, d5)
summary(lm9)
lm10 <- lm(Num_Aphids_DayFive ~ Trt, d5)
summary(lm10)
lm15 <- lm(Num_Aphids_DayTen ~ Light*Temp, d5)
summary(lm15)
lm16 <- lm(Num_Aphids_DayTen ~ Trt, d5)
summary(lm16)

# linear models for aphids on day ten and five for C7
lm11 <- lm(Num_Aphids_DayFive ~ Light*Temp, dc7)
summary(lm11)
lm12 <- lm(Num_Aphids_DayFive ~ Trt, dc7)
summary(lm12)
lm17 <- lm(Num_Aphids_DayTen ~ Light*Temp, dc7)
summary(lm17)
lm18 <- lm(Num_Aphids_DayTen ~ Trt, dc7)
summary(lm18)

# linear models for aphids on day ten and five for CMAC
lm13 <- lm(Num_Aphids_DayFive ~ Light*Temp, dcmac)
summary(lm13)
lm14 <- lm(Num_Aphids_DayFive ~ Trt, dcmac)
summary(lm14)
lm19 <- lm(Num_Aphids_DayTen ~ Light*Temp, dcmac)
summary(lm19)
lm20 <- lm(Num_Aphids_DayTen ~ Trt, dcmac)
summary(lm20)

# plotting the interactions for the models including all species with numeric variables. 
# plotting both using treatment as a variable or light/temperature as variables
plot_model(lm1, type = 'int', colors=c('goldenrod', 'tomato1', 'cornflowerblue')) + theme_classic()
plot_model(lm2, type = 'int', colors=c('grey60', 'goldenrod', 'cornflowerblue')) + theme_classic()
plot_model(lm3, type = 'int', colors=c('goldenrod', 'tomato1', 'cornflowerblue')) + theme_classic()
plot_model(lm4, type = 'int', colors=c('grey60', 'goldenrod', 'cornflowerblue')) + theme_classic()
plot_model(lm6, type = 'int', colors=c('navyblue', 'orange', 'cornflowerblue')) + theme_classic()
plot_model(lm8, type = 'int', colors=c('navyblue', 'orange', 'cornflowerblue')) + theme_classic()

# better boxplots of everything

# day ten, treatment
p <- ggplot(d, aes(x=Trt, y=Num_Aphids_DayTen, fill=interaction(Species, Trt)))+ theme_linedraw() + xlab('Treatment') + ylab('Number of Aphids/Plant')
p + geom_boxplot(position=position_dodge(1)) + theme_linedraw() + scale_fill_manual(values=c("goldenrod","tomato1",'cornflowerblue',"goldenrod", "tomato1",'cornflowerblue',"goldenrod","tomato1",'cornflowerblue',"goldenrod","tomato1", 'cornflowerblue')) + theme(text = element_text(size=15), legend.position='none')

tiff("ecology_ms_figure2.tif", units="in", width=7, height=3.5, res=600)
# day ten, species
q <- ggplot(d, aes(x=Trt, y=Num_Aphids_DayTen, fill=interaction(Trt, Species))) + geom_boxplot(position=position_dodge(4)) + geom_jitter() + theme_classic() + theme(text = element_text(size=15))+ xlab('Treatment') + ylab('Number of Aphids/Plant')
q + facet_wrap(~Species) + scale_fill_manual(values=c(alpha("navy", .8), alpha('lightslateblue', .8), alpha("moccasin", .8), alpha('orange', .8), alpha("navy", .8), alpha('mediumslateblue', .8), alpha("moccasin", .8), alpha('orange', .8), alpha("navy", .8), alpha('mediumslateblue', .8), alpha("moccasin", .8), alpha('orange', .8))) + theme(text = element_text(size=7), axis.text.x = element_text(angle = 50, hjust = 1)) + theme(text = element_text(size=7), legend.position='none')                                                                                                                                                       
dev.off() 


# day five, treatment
r <- ggplot(d, aes(x=Trt, y=Num_Aphids_DayFive, fill=interaction(Species, Trt)))+ theme_linedraw() + theme(text = element_text(size=15))+ xlab('Treatment') + ylab('Number of Aphids/Plant')
r + geom_boxplot(position=position_dodge(1)) + theme_linedraw() + scale_fill_manual(values=c("goldenrod","tomato1",'cornflowerblue',"goldenrod", "tomato1",'cornflowerblue',"goldenrod","tomato1",'cornflowerblue',"goldenrod","tomato1", 'cornflowerblue')) + theme(text = element_text(size=15), legend.title=element_blank()) + theme(text = element_text(size=15), legend.position='none')

# day five, species
s <- ggplot(d, aes(x=Trt, y=Num_Aphids_DayFive, fill=interaction(Trt, Species))) + geom_boxplot(position=position_dodge(4)) + theme_linedraw() + theme(text = element_text(size=15), legend.position='none')+ xlab('Treatment') + ylab('Number of Aphids/Plant')
s + facet_wrap(~Species) + scale_fill_manual(values=c(alpha("tomato2", .8), alpha('violet', .8), alpha("goldenrod", .8), alpha('dodgerblue3', .8), alpha("tomato2", .8), alpha('violet', .8), alpha("goldenrod", .8), alpha('dodgerblue3', .8), alpha("tomato2", .8), alpha('violet', .8), alpha("goldenrod", .8), alpha('dodgerblue3', .8))) + theme(text = element_text(size=15), axis.text.x = element_text(angle = 50, hjust = 1))                                                                                                                                                        

# plant growth, treatment
t <- ggplot(d, aes(x=Trt, y=Plant_Growth, fill=interaction(Species, Trt)))+ theme_linedraw() + theme(text = element_text(size=15), legend.position = 'none')+ xlab('Treatment') + ylab('Number of Aphids/Plant')
t + geom_boxplot(position=position_dodge(1)) + theme_linedraw() + scale_fill_manual(values=c("goldenrod","tomato1",'cornflowerblue',"goldenrod", "tomato1",'cornflowerblue',"goldenrod","tomato1",'cornflowerblue',"goldenrod","tomato1", 'cornflowerblue')) + theme(text = element_text(size=15), legend.title=element_blank()) + theme(text = element_text(size=15), legend.position='none')

# plant growth, species
u <- ggplot(d, aes(x=Trt, y=Plant_Growth, fill=interaction(Trt, Species))) + geom_boxplot(position=position_dodge(4)) + theme_linedraw() + theme(text = element_text(size=15), legend.position = 'none')+ xlab('Treatment') + ylab('Number of Aphids/Plant')
u + facet_wrap(~Species) + scale_fill_manual(values=c(alpha("navy", .8), alpha('lightslateblue', .8), alpha("salmon", .8), alpha('red', .8), alpha("red", .8), alpha('violet', .8), alpha("goldenrod", .8), alpha('dodgerblue3', .8), alpha("tomato2", .8), alpha('violet', .8), alpha("goldenrod", .8), alpha('dodgerblue3', .8))) + theme(text = element_text(size=15), axis.text.x = element_text(angle = 50, hjust = 1))                                                                                                                                                        

#### looking at aphid drop rate                  ####

# OutVals1 <- boxplot(d$Aphid_Drop_DayFive)$out
# which(d$Aphid_Drop_DayFive %in% OutVals1)
# 
# outlierKD(d, Aphid_Drop_DayFive)
# yes
# 
# OutVals2 <- boxplot(d$Aphid_Drop_DayTen)$out
# which(d$Aphid_Drop_DayTen %in% OutVals1)
# 
# outlierKD(d, Aphid_Drop_DayTen)
# yes

# linear models for aphids on day ten and five for all species
lm1 <- lm(Aphid_Drop_DayFive ~ Trt*Species, d)
summary(lm1)
lm2 <- lm(Aphid_Drop_DayFive ~ Temp*Light*Species, d)
summary(lm2)
lm3 <- lm(Aphid_Drop_DayTen ~ Trt*Species, d)
summary(lm3)
lm4 <- lm(Aphid_Drop_DayTen ~ Temp*Light*Species, d)
summary(lm4)
lm5 <- lm(Aphid_Drop_DayFive ~ Species*Trt, d)
summary(lm5)
lm6 <- lm(Aphid_Drop_DayFive ~ Light*Temp*Species, d)
summary(lm5)
lm7 <- lm(Aphid_Drop_DayTen ~ Species*Trt, d)
summary(lm6)
lm8 <- lm(Aphid_Drop_DayTen ~ Light*Temp*Species, d)
summary(lm7)

# linear models for aphids on day ten and five for control on day five
lm9 <- lm(Aphid_Drop_DayFive ~ Light*Temp, d5)
summary(lm9)
lm10 <- lm(Aphid_Drop_DayFive ~ Trt, d5)
summary(lm10)
lm15 <- lm(Aphid_Drop_DayTen ~ Light*Temp, d5)
summary(lm15)
lm16 <- lm(Aphid_Drop_DayTen ~ Trt, d5)
summary(lm16)

# linear models for aphids on day ten and five for C7
lm11 <- lm(Aphid_Drop_DayFive ~ Light*Temp, dc7)
summary(lm11)
lm12 <- lm(Aphid_Drop_DayFive ~ Trt, dc7)
summary(lm12)
lm17 <- lm(Aphid_Drop_DayTen ~ Light*Temp, dc7)
summary(lm17)
lm18 <- lm(Aphid_Drop_DayTen ~ Trt, dc7)
summary(lm18)

# linear models for aphids on day ten and five for CMAC
lm13 <- lm(Aphid_Drop_DayFive ~ Light*Temp, dcmac)
summary(lm13)
lm14 <- lm(Aphid_Drop_DayFive ~ Trt, dcmac)
summary(lm14)
lm19 <- lm(Aphid_Drop_DayTen ~ Light*Temp, dcmac)
summary(lm19)
lm20 <- lm(Aphid_Drop_DayTen ~ Trt, dcmac)
summary(lm20)

# plotting the interactions for the models including all species with numeric variables. 
# plotting both using treatment as a variable or light/temperature as variables
plot_model(lm1, type = 'int', colors=c('goldenrod', 'tomato1', 'cornflowerblue')) + theme_classic()
plot_model(lm2, type = 'int', colors=c('grey60', 'goldenrod', 'cornflowerblue')) + theme_classic()
plot_model(lm3, type = 'int', colors=c('goldenrod', 'tomato1', 'cornflowerblue')) + theme_classic()
plot_model(lm4, type = 'int', colors=c('grey60', 'goldenrod', 'cornflowerblue')) + theme_classic()
plot_model(lm6, type = 'int', colors=c('navyblue', 'orange', 'cornflowerblue')) + theme_classic()
plot_model(lm8, type = 'int', colors=c('navyblue', 'orange', 'cornflowerblue')) + theme_classic()

# better boxplots of everything

# day ten, treatment
p <- ggplot(d, aes(x=Trt, y=Aphid_Drop_DayTen, fill=interaction(Species, Trt)))+ theme_linedraw() + xlab('Treatment') + ylab('Number of Aphids Drops')
p + geom_boxplot(position=position_dodge(1)) + theme_linedraw() + scale_fill_manual(values=c("goldenrod","tomato1",'cornflowerblue',"goldenrod", "tomato1",'cornflowerblue',"goldenrod","tomato1",'cornflowerblue',"goldenrod","tomato1", 'cornflowerblue')) + theme(text = element_text(size=15), legend.position='none')

# day ten, species
q <- ggplot(d, aes(x=Trt, y=Aphid_Drop_DayTen, fill=interaction(Trt, Species))) + geom_boxplot(position=position_dodge(4)) + theme_linedraw() + theme(text = element_text(size=15))+ xlab('Treatment') + ylab('Number of Aphid Drops')
q + facet_wrap(~Species) + scale_fill_manual(values=c(alpha("tomato2", .8), alpha('violet', .8), alpha("goldenrod", .8), alpha('dodgerblue3', .8), alpha("tomato2", .8), alpha('violet', .8), alpha("goldenrod", .8), alpha('dodgerblue3', .8), alpha("tomato2", .8), alpha('violet', .8), alpha("goldenrod", .8), alpha('dodgerblue3', .8))) + theme(text = element_text(size=15), axis.text.x = element_text(angle = 50, hjust = 1)) + theme(text = element_text(size=15), legend.position='none')                                                                                                                                                       

# day five, treatment
r <- ggplot(d, aes(x=Trt, y=Aphid_Drop_DayFive, fill=interaction(Species, Trt)))+ theme_linedraw() + theme(text = element_text(size=15))+ xlab('Treatment') + ylab('Number of Aphids Drops')
r + geom_boxplot(position=position_dodge(1)) + theme_linedraw() + scale_fill_manual(values=c("goldenrod","tomato1",'cornflowerblue',"goldenrod", "tomato1",'cornflowerblue',"goldenrod","tomato1",'cornflowerblue',"goldenrod","tomato1", 'cornflowerblue')) + theme(text = element_text(size=15), legend.title=element_blank()) + theme(text = element_text(size=15), legend.position='none')

# day five, species
s <- ggplot(d, aes(x=Trt, y=Aphid_Drop_DayFive, fill=interaction(Trt, Species))) + geom_boxplot(position=position_dodge(4)) + theme_linedraw() + theme(text = element_text(size=15), legend.position='none')+ xlab('Treatment') + ylab('Number of Aphids Drops')
s + facet_wrap(~Species) + scale_fill_manual(values=c(alpha("tomato2", .8), alpha('violet', .8), alpha("goldenrod", .8), alpha('dodgerblue3', .8), alpha("tomato2", .8), alpha('violet', .8), alpha("goldenrod", .8), alpha('dodgerblue3', .8), alpha("tomato2", .8), alpha('violet', .8), alpha("goldenrod", .8), alpha('dodgerblue3', .8))) + theme(text = element_text(size=15), axis.text.x = element_text(angle = 50, hjust = 1))                                                                                                                                                        

glmm1 <- glmer(Aphid_Drop_DayTen ~ Trt*Species + (1|Location_Name) + (1|Trial), d, family='poisson')
summary(glmm1)  
emmeans(glmm1, pairwise~Trt|Species)
emmeans(glmm1, pairwise~Species|Trt)

glmm2 <- glmer(Aphid_Drop_DayTen ~ Light*Temp*Species + (1|Location_Name) + (1|Trial), d, family='poisson')
summary(glmm2)  

e1.2 <- emmeans(glmm2, pairwise~Light|Species)
e1.2
e2.2 <- emmeans(glmm2, pairwise~Light|Temp)
e2.2
e3.2 <- emmeans(glmm2, pairwise~Temp|Species)
e3.2
e4.2 <- emmeans(glmm2, pairwise~Temp|Light)
e4.2
e5.2 <- emmeans(glmm2, pairwise~Species|Light)
e5.2
e6.2 <- emmeans(glmm2, pairwise~Species|Temp)
e6.2
e7.2 <- emmeans(glmm2, pairwise~Species|Temp|Light)
e7.2
e7.2c <- e7.2$contrasts %>%
  confint() %>%
  as.data.frame()
e8.2 <- emmeans(glmm2, pairwise~Light|Temp|Species)
e8.2
e8.2c <- e8.2$contrasts %>%
  confint() %>%
  as.data.frame()
e9.2 <- emmeans(glmm2, pairwise~Temp|Species|Light)
e9.2
e9.2c <- e9.2$contrasts %>%
  confint() %>%
  as.data.frame()

plot(e1.2, comparisons=T) + theme_sjplot2()
plot(e2.2, comparisons=T) + theme_sjplot2()
plot(e3.2, comparisons=T) + theme_sjplot2()
plot(e4.2, comparisons=T) + theme_sjplot2()
plot(e5.2, comparisons=T) + theme_sjplot2()
plot(e6.2, comparisons=T) + theme_sjplot2()
plot(e7.2, comparisons=T) + theme_sjplot2()
plot(e8.2, comparisons=T) + theme_sjplot2()
plot(e9.2, comparisons=T) + theme_sjplot2()

glmm3 <- glmer(Aphid_Drop_DayFive ~ Light*Temp*Species + (1|Location_Name) + (1|Trial), d, family='poisson')
summary(glmm3)  

e1.3 <- emmeans(glmm3, pairwise~Light|Species)
e1.3
e2.3 <- emmeans(glmm3, pairwise~Light|Temp)
e2.3
e3.3 <- emmeans(glmm3, pairwise~Temp|Species)
e3.3
e4.3 <- emmeans(glmm3, pairwise~Temp|Light)
e4.3
e5.3 <- emmeans(glmm3, pairwise~Species|Light)
e5.3
e6.3 <- emmeans(glmm3, pairwise~Species|Temp)
e6.3
e7.3 <- emmeans(glmm3, pairwise~Species|Temp|Light)
e7.3
e7.3c <- e7.3$contrasts %>%
  confint() %>%
  as.data.frame()
e8.3 <- emmeans(glmm3, pairwise~Light|Temp|Species)
e8.3
e8.3c <- e8.3$contrasts %>%
  confint() %>%
  as.data.frame()
e9.3 <- emmeans(glmm3, pairwise~Temp|Species|Light)
e9.3
e9.3c <- e9.3$contrasts %>%
  confint() %>%
  as.data.frame()

emmip(glmm3, ~ Temp | Species | Light, CIs =T, col=c('cornflowerblue', 'tomato1', 'mediumorchid'), pch=2) + 
  theme_classic()
emmip(glmm3, ~ Species | Temp | Light, CIs =T, col=c('cornflowerblue', 'tomato1', 'mediumorchid'), pch=2) + 
  theme_classic()
emmip(glmm3, ~ Light | Species | Temp, CIs =T, col=c('cornflowerblue', 'tomato1', 'mediumorchid'), pch=2) + 
  theme_classic()
emmip(glmm3, ~ Temp | Light | Species, CIs =T, pch=2) + 
  theme_classic() + labs(color='')
geom_point(aes(x=Temp, y=Aphid_Drop_DayFive, d, col='cornflowerblue'))

plot(e1.3, comparisons=T) + theme_sjplot2()
plot(e2.3, comparisons=T) + theme_sjplot2()
plot(e3.3, comparisons=T) + theme_sjplot2()
plot(e4.3, comparisons=T) + theme_sjplot2()
plot(e5.3, comparisons=T) + theme_sjplot2()
plot(e6.3, comparisons=T) + theme_sjplot2()
plot(e7.3, comparisons=T) + theme_sjplot2()
plot(e8.3, comparisons=T) + theme_sjplot2()
plot(e9.3, comparisons=T) + theme_sjplot2()

#### mixed models: population size               ####

# lme4 package models

# poisson distribution, all species
glmm1 <- glmer(Num_Aphids_DayTen ~ Trt*Species + (1|Location_Name) + (1|Trial), d, family='poisson')
summary(glmm1)  
emmeans(glmm1, pairwise~Trt|Species)
emmeans(glmm1, pairwise~Species|Trt)

glmm2 <- glmer(Num_Aphids_DayTen ~ Light*Temp*Species + (1|Location_Name) + (1|Trial), d, family='poisson')
summary(glmm2)  


e1.2 <- emmeans(glmm2, pairwise~Light|Species)
e1.2
e2.2 <- emmeans(glmm2, pairwise~Light|Temp)
e2.2
e3.2 <- emmeans(glmm2, pairwise~Temp|Species)
e3.2
e4.2 <- emmeans(glmm2, pairwise~Temp|Light)
e4.2
e5.2 <- emmeans(glmm2, pairwise~Species|Light)
e5.2
e6.2 <- emmeans(glmm2, pairwise~Species|Temp)
e6.2
e7.2 <- emmeans(glmm2, pairwise~Species|Temp|Light)
e7.2
e7.2c <- e7.2$contrasts %>%
  confint() %>%
  as.data.frame()
e8.2 <- emmeans(glmm2, pairwise~Light|Temp|Species)
e8.2
e8.2c <- e8.2$contrasts %>%
  confint() %>%
  as.data.frame()
e9.2 <- emmeans(glmm2, pairwise~Temp|Species|Light)
e9.2
e9.2c <- e9.2$contrasts %>%
  confint() %>%
  as.data.frame()

plot(e1.2, comparisons=T) + theme_sjplot2()
plot(e2.2, comparisons=T) + theme_sjplot2()
plot(e3.2, comparisons=T) + theme_sjplot2()
plot(e4.2, comparisons=T) + theme_sjplot2()
plot(e5.2, comparisons=T) + theme_sjplot2()
plot(e6.2, comparisons=T) + theme_sjplot2()
plot(e7.2, comparisons=T) + theme_sjplot2()
plot(e8.2, comparisons=T) + theme_sjplot2()
plot(e9.2, comparisons=T) + theme_sjplot2()

glmm3 <- glmer(Num_Aphids_DayFive ~ Light*Temp*Species + (1|Location_Name) + (1|Trial), d, family='poisson')
summary(glmm3)  

e1.3 <- emmeans(glmm3, pairwise~Light|Species)
e1.3
e2.3 <- emmeans(glmm3, pairwise~Light|Temp)
e2.3
e3.3 <- emmeans(glmm3, pairwise~Temp|Species)
e3.3
e4.3 <- emmeans(glmm3, pairwise~Temp|Light)
e4.3
e5.3 <- emmeans(glmm3, pairwise~Species|Light)
e5.3
e6.3 <- emmeans(glmm3, pairwise~Species|Temp)
e6.3
e7.3 <- emmeans(glmm3, pairwise~Species|Temp|Light)
e7.3
e7.3c <- e7.3$contrasts %>%
  confint() %>%
  as.data.frame()
e8.3 <- emmeans(glmm3, pairwise~Light|Temp|Species)
e8.3
e8.3c <- e8.3$contrasts %>%
  confint() %>%
  as.data.frame()
e9.3 <- emmeans(glmm3, pairwise~Temp|Species|Light)
e9.3
e9.3c <- e9.3$contrasts %>%
  confint() %>%
  as.data.frame()

emmip(glmm3, ~ Temp | Species | Light, CIs =T, col=c('cornflowerblue', 'tomato1', 'mediumorchid'), pch=2) + 
  theme_classic()
emmip(glmm3, ~ Species | Temp | Light, CIs =T, col=c('cornflowerblue', 'tomato1', 'mediumorchid'), pch=2) + 
  theme_classic()
emmip(glmm3, ~ Light | Species | Temp, CIs =T, col=c('cornflowerblue', 'tomato1', 'mediumorchid'), pch=2) + 
  theme_classic()
emmip(glmm3, ~ Temp | Light | Species, CIs =T, pch=2) + 
  theme_classic() + labs(color='')
  geom_point(aes(x=Temp, y=Num_Aphids_DayFive, d, col='cornflowerblue'))

plot(e1.3, comparisons=T) + theme_sjplot2()
plot(e2.3, comparisons=T) + theme_sjplot2()
plot(e3.3, comparisons=T) + theme_sjplot2()
plot(e4.3, comparisons=T) + theme_sjplot2()
plot(e5.3, comparisons=T) + theme_sjplot2()
plot(e6.3, comparisons=T) + theme_sjplot2()
plot(e7.3, comparisons=T) + theme_sjplot2()
plot(e8.3, comparisons=T) + theme_sjplot2()
plot(e9.3, comparisons=T) + theme_sjplot2()

glmm4 <- glmer(Num_Aphids_DayFive ~ Trt*Species + (1|Location_Name) + (1|Trial), d, family='poisson')
summary(glmm4) 

# just day five, control
glmm5 <- glmer(Num_Aphids_DayFive ~ Temp + (1|Location_Name) + (1|Trial), d5, family='poisson')
summary(glmm5) 

glmm6 <- glmer(Num_Aphids_DayFive ~ Trt + (1|Location_Name) + (1|Trial), d5, family='poisson')
summary(glmm6) 

glmm7 <- glmer(Num_Aphids_DayFive ~ Temp*Light + (1|Location_Name) + (1|Trial), d5, family='poisson')
summary(glmm7) 

# scaled light and temperature variables
glmm8 <- glmer(Num_Aphids_DayTen ~ scale_Light*scale_Temp*Species + (1|Location_Name) + (1|Trial), d, family='poisson')
summary(glmm8) 

# linear mixed models (gaussian distribution)
lmm1 <- lmer(scale_Num ~ Trt*Species + (1|Location_Name) + (1|Trial), d)
summary(lmm1)          

lmm2 <- lmer(scale_Num ~ Light*Temp*Species + (1|Location_Name) + (1|Trial), d)
summary(lmm2)   

lmm3 <- lmer(scale_Num ~ scale_Light*scale_Temp*Species + (1|Location_Name) + (1|Trial), d)
summary(lmm3)    

lmm4 <- lmer(Plant_Growth ~ Light*Temp*Species + (1|Location_Name) + (1|Trial), d)
summary(lmm4)

# nlme package models (gives slightly different output, which can be helpful). all gaussian.
lme1 <- lme(scale_Num ~ Trt*Species, data=d, random= list(~1|Location_Name, ~1|Trial))
summary(lme1)          
anova(lme1)

lme2 <- lme(scale_Num ~ Light*Temp*Species, d, random= list(~1|Location_Name, ~1|Trial))
summary(lme2)  
anova(lme2)

lme3 <- lme(scale_Num ~ scale_Light*scale_Temp*Species, d, list(~1|Location_Name, ~1|Trial))
summary(lme3)    
anova(lme3)

lme4 <- lme(Plant_Growth ~ Light*Temp*Species, d, list(~1|Location_Name, ~1|Trial))
summary(lme4)
anova(lme4)

# nlme, scaled variables
lme5 <- lme(scale_Num ~ Trt*Species, data=d, random= ~1|Location_Name)
summary(lme5)          
anova(lme5)

lme6 <- lme(scale_Num ~ Light*Temp*Species, d, random= ~1|Location_Name)
summary(lme6)  
anova(lme6)

lme7 <- lme(scale_Num ~ scale_Light*scale_Temp*Species, d, random= ~1|Location_Name)
summary(lme7)    
anova(lme7)

#### anovas and tukey post hoc                   ####

# function to plot tukey error bars
plotTukeyHSD <- plotTukeysHSD <- function(tukey.out,
                                         x.axis.label = "Comparison",
                                         y.axis.label = "Effect Size",
                                         axis.adjust = 0,
                                         adjust.x.spacing = 5){
  
  tukey.out <- as.data.frame(tukey.out[[1]])
  means <- tukey.out$diff
  categories <- row.names(tukey.out)
  groups <- length(categories)
  ci.low <- tukey.out$lwr
  ci.up  <- tukey.out$upr                         
  
  n.means <- length(means)
  
  #determine where to plot points along x-axis
  x.values <- 1:n.means
  x.values <- x.values/adjust.x.spacing
  
  
  # calculate values for plotting limits            
  y.max <- max(ci.up) +                    
    max(ci.up)*axis.adjust
  y.min <- min(ci.low) - 
    max(ci.low)*axis.adjust
  
  if(groups == 2){ x.values <- c(0.25, 0.5)}
  if(groups == 3){ x.values <- c(0.25, 0.5,0.75)}
  
  x.axis.min <- min(x.values)-0.05
  x.axis.max <- max(x.values)+0.05
  
  x.limits <- c(x.axis.min,x.axis.max)
  
  #Plot means
  plot(means ~ x.values,
       xlim = x.limits,
       ylim = c(y.min,y.max),
       xaxt = "n",
       xlab = "",
       ylab = "",
       cex = 1.25,
       pch = 16)
  
  axis(side = 1, 
       at = x.values,
       labels = categories,
  )
  
  #Plot upper error bar 
  lwd. <- 2
  arrows(y0 = means,
         x0 = x.values,
         y1 = ci.up,
         x1 = x.values,
         length = 0,
         lwd = lwd.)
  
  #Plot lower error bar
  arrows(y0 = means,
         x0 = x.values,
         y1 = ci.low,
         x1 = x.values,
         length = 0,
         lwd = lwd.) 
  
  #add reference line at 0
  abline(h = 0, col = 2, lwd = 2, lty =2)
  
  mtext(text = x.axis.label,side = 1,line = 1.75)
  mtext(text = y.axis.label,side = 2,line = 1.95)
  mtext(text = "Error bars = 95% CI",side = 3,line = 0,adj = 0)
  
  
}

# ANOVA analysis, Tukey post hoc analysis and plots
par(mfrow=c(1,2))

# all species, day ten
a2.1 <- aov(Num_Aphids_DayTen ~ Trt, data=d)
summary(a2.1)
t2.1 <- TukeyHSD(a2.1)
plot(t2.1, las=2, col='tomato1')
plotTukeyHSD(t2.1)

# control, day five
a2.2 <- aov(dayfive ~ Trt, data=d5)
summary(a2.2)
t2.2 <- TukeyHSD(a2.2)
plot(t2.2,  las=2,col='tomato1')
plotTukeyHSD(t2.2)

# control, day ten
a2.22 <- aov(Num_Aphids_DayTen ~ Trt, data=d5)
summary(a2.22)
t2.22 <- TukeyHSD(a2.22)
plot(t2.22, las=2, col='tomato1')
plotTukeyHSD(t2.22)

# c7 day ten
a2.3 <- aov(Num_Aphids_DayTen ~ Trt, data=dc7)
summary(a2.3)
t2.3 <- TukeyHSD(a2.3)
plot(t2.3, las=2, col='tomato1')
plotTukeyHSD(t2.3)

# c7 day 5
a2.33 <- aov(Num_Aphids_DayFive ~ Trt, data=dc7)
summary(a2.33)
t2.33 <- TukeyHSD(a2.33)
plot(t2.33, las=2, col='tomato1')
plotTukeyHSD(t2.33)

# cmac day ten
a2.4 <- aov(Num_Aphids_DayTen ~ Trt, data=dcmac)
summary(a2.4)
t2.4 <- TukeyHSD(a2.4)
plot(t2.4, las=2, col='tomato1')
plotTukeyHSD(t2.4)

# cmac day five
a2.44 <- aov(Num_Aphids_DayFive ~ Trt, data=dcmac)
summary(a2.44)
t2.44 <- TukeyHSD(a2.44)
plot(t2.44, las=2, col='tomato1')
plotTukeyHSD(t2.44)

# control day ten
a2.5 <- aov(Num_Aphids_DayTen ~ Trt, data=dc)
summary(a2.5)
t2.5 <- TukeyHSD(a2.5)
plot(t2.5, las=2, col='tomato1')
plotTukeyHSD(t2.5)

# control day five
a2.55 <- aov(Num_Aphids_DayFive ~ Trt, data=dc)
summary(a2.55)
t2.55 <- TukeyHSD(a2.55)
plot(t2.55, las=2, col='tomato1')
plotTukeyHSD(t2.55)

# control, day five
a2.6 <- aov(Num_Aphids_DayFive ~ Trt, data=d5)
summary(a2.6)
t2.6 <- TukeyHSD(a2.6)
plot(t2.6, las=2, col='tomato1')
plotTukeyHSD(t2.6)

# plant growth, control, day five
p2.2 <- aov( Plant_Growth~ Trt, data=d5)
summary(p2.2)
tp2.2 <- TukeyHSD(p2.2)
plot(tp2.2, las=2, col='tomato1')
plotTukeyHSD(tp2.2)

# plant growth, all species
p2.3 <- aov(Plant_Growth ~ Trt, data=d)
summary(p2.3)
tp2.3 <- TukeyHSD(p2.3)
plot(tp2.3, las=2, col='tomato1')
plotTukeyHSD(tp2.3)

# growth rate, all species
p2.4 <- aov( Growth_Total ~ Trt, data=d)
summary(p2.4)
tp2.4 <- TukeyHSD(p2.4)
plot(tp2.4, las=2, col='tomato1')
plotTukeyHSD(tp2.4)

# first five days growth rate, all species
p2.5 <- aov(Growth_Rate_Early ~ Trt, data=d)
summary(p2.5)
tp2.5 <- TukeyHSD(p2.5)
plot(tp2.5, las=2, col='tomato1')
plotTukeyHSD(tp2.5)

# first five days growth rate, control
p2.6 <- aov(Growth_Rate_Early ~ Trt, data=dc)
summary(p2.6)
tp2.6 <- TukeyHSD(p2.6)
plot(tp2.6, las=2, col='tomato1')
plotTukeyHSD(tp2.6)

# average growth rate, control
p2.7 <- aov(Growth_Average ~ Trt, data=dc)
summary(p2.7)
tp2.7 <- TukeyHSD(p2.7)
plot(tp2.7, las=2, col='tomato1')
plotTukeyHSD(tp2.7)

# t test control, day five, between warm treatments
tt2.7 <- t.test(d5$Num_Aphids_DayFive[d5$Temp == 25 & d5$Light == 24], d5$Num_Aphids_DayFive[d5$Temp ==25 & d5$Light ==16])
summary(tt2.7)

#### looking specifically at growth rate         ####

# all species
gr1 <- lme( Growth_Average~Light*Temp*Species, random= list(~1|Location_Name, ~1|Trial), d)
summary(gr1)

gr2 <- lme( Growth_Average~Trt*Species, random= list(~1|Location_Name, ~1|Trial), d)
summary(gr2)

# c7
gr3 <- lme( Growth_Average~Light*Temp, random= list(~1|Location_Name, ~1|Trial), dc7)
summary(gr3)

gr4 <- lme( Growth_Average~Trt, random= list(~1|Location_Name, ~1|Trial), dc7)
summary(gr4)

# cmac
gr5 <- lme( Growth_Average~Light*Temp, random= list(~1|Location_Name, ~1|Trial), dcmac)
summary(gr5)

gr6 <- lme( Growth_Average~Trt, random= list(~1|Location_Name, ~1|Trial), dcmac)
summary(gr6)

# control
gr7 <- lme( Growth_Average~Light*Temp, random= list(~1|Location_Name, ~1|Trial), dc)
summary(gr7)

gr8 <- lme( Growth_Average~Trt, random= list(~1|Location_Name, ~1|Trial), dc)
summary(gr8)

# histograms of growth rates
par(mfrow=c(1,3))
hist(d$Growth_Average, col = 'tomato1', xlab = 'Aphids/Day', main='Average', cex.axis=1.5, cex.lab=1.5, cex.main=2)
hist(d$Growth_Rate_Early, col = 'dodgerblue2', xlab = 'Aphids/Day', main='Early', cex.axis=1.5, cex.lab=1.5, cex.main=2)
hist(d$Growth_Rate_Late, col = 'goldenrod', xlab = 'Aphids/Day', main='Late', cex.axis=1.5, cex.lab=1.5, cex.main=2)

# box plots of growth rate
boxplot(Growth_Average ~ Trt, d, ylab = 'Aphids/Day', col = 'tomato1', main='Average', cex.axis=1.25, cex.lab=1.5, cex.main=2, ylim=c(-30,100))
boxplot(Growth_Rate_Early ~ Trt, d, ylab = 'Aphids/Day', col = 'dodgerblue2', main='Early', cex.axis=1.25, cex.lab=1.5, cex.main=2, ylim=c(-30,100))
boxplot(Growth_Rate_Late ~ Trt, d, ylab = 'Aphids/Day', col = 'goldenrod', main='Late', cex.axis=1.25, cex.lab=1.5, cex.main=2, ylim=c(-30,100))

# box plots of growth rate by species
boxplot(Growth_Average ~ Trt, d[d$Species=='CMAC',], ylab = 'Aphids/Day', col = 'tomato2', main='Average: CMAC', cex.axis=1.25, cex.lab=1.5, cex.main=2, ylim=c(-30,100))
boxplot(Growth_Average ~ Trt, d[d$Species=='control',], ylab = 'Aphids/Day', col = 'tomato3', main='Average: Control', cex.axis=1.25, cex.lab=1.5, cex.main=2, ylim=c(-30,100))
boxplot(Growth_Average ~ Trt, d[d$Species=='C7',], ylab = 'Aphids/Day', col = 'tomato4', main='Average: C7', cex.axis=1.5, cex.lab=1.25, cex.main=2, ylim=c(-30,100))

# box plots of early growth rate by species
boxplot(Growth_Rate_Early ~ Trt, d[d$Species=='CMAC',], ylab = 'Aphids/Day', col = 'dodgerblue1', main='Rate_Early: CMAC', cex.axis=1.25, cex.lab=1.5, cex.main=2, ylim=c(-30,100))
boxplot(Growth_Rate_Early ~ Trt, d[d$Species=='control',], ylab = 'Aphids/Day', col = 'dodgerblue3', main='Rate_Early: Control', cex.axis=1.25, cex.lab=1.5, cex.main=2, ylim=c(-30,100))
boxplot(Growth_Rate_Early ~ Trt, d[d$Species=='C7',], ylab = 'Aphids/Day', col = 'dodgerblue4', main='Rate_Early: C7', cex.axis=1.5, cex.lab=1.25, cex.main=2, ylim=c(-30,100))

# box plots of late growth rate by species
boxplot(Growth_Rate_Late ~ Trt, d[d$Species=='CMAC',], ylab = 'Aphids/Day', col = 'goldenrod2', main='Rate_Late: CMAC', cex.axis=1.25, cex.lab=1.5, cex.main=2, ylim=c(-30,100))
boxplot(Growth_Rate_Late ~ Trt, d[d$Species=='control',], ylab = 'Aphids/Day', col = 'goldenrod3', main='Rate_Late: Control', cex.axis=1.25, cex.lab=1.5, cex.main=2, ylim=c(-30,100))
boxplot(Growth_Rate_Late ~ Trt, d[d$Species=='C7',], ylab = 'Aphids/Day', col = 'goldenrod4', main='Rate_Late: C7', cex.axis=1.5, cex.lab=1.25, cex.main=2, ylim=c(-30,100))

#### line charts                                 ####

# charts that show a timeline of the study, days 0, 5, 10 for number of aphids

# By Species
par(mfrow=c(1,3))
plot(a1,type = "o", cex.main=2, lwd=2, col='cornflowerblue', ylim=c(0, 500), cex.axis=1.5, 
     cex.lab=2,ylab = 'Number of Aphids', main='Control', xlab = '', xaxt='n')
axis(side=1, a=c(1,2,3), labels=c('Day 0', 'Day 5', 'Day 10'), srt=45, cex=2, xpd=T)
lines(a2, type = "o", lwd=2, lty=2, col='tomato1')
lines(a3, type = 'o', lwd=2, col='dodgerblue4')
lines(a4, type = 'o', lwd=2, lty=2, col='firebrick4')
legend('topleft', cex=c(1.5,1.5,1.5,1.5), lty=c(1,2,1,2), lwd=3, col=c('cornflowerblue', 'tomato1', 'dodgerblue4', 'firebrick4'), 
       legend=c('Cold/Control', 'Warm/Control', 'Cold/Light', 'Warm/Light'))

plot(c1,type = "o", cex.main=2, lwd=2, col='cornflowerblue', ylim=c(0, 500), cex.axis=1.5, 
     cex.lab=2,ylab='', main='CMAC', xlab = '', xaxt='n')
axis(side=1, a=c(1,2,3), labels=c('Day 0', 'Day 5', 'Day 10'), srt=45, cex=2, xpd=T)
lines(c2, type = "o", lwd=2, lty=2, col='tomato1')
lines(c3, type = 'o', lwd=2, col='dodgerblue4')
lines(c4, type = 'o', lwd=2, lty=2, col='firebrick4')

plot(s1,type = "o", cex.main=2, lwd=2, col='cornflowerblue', ylim=c(0, 500), cex.axis=1.5, 
     cex.lab=2,ylab='', main='C7', xlab = '', xaxt='n')
axis(side=1, a=c(1,2,3), labels=c('Day 0', 'Day 5', 'Day 10'), srt=45, cex=2, xpd=T)
lines(s2, type = "o", lwd=2, lty=2, col='tomato1')
lines(s3, type = 'o', lwd=2, col='dodgerblue4')
lines(s4, type = 'o', lwd=2, lty=2, col='firebrick4')

# By Treatment
par(mfrow=c(2,2))
plot(a1,type = "o", cex.main=1.5, lwd=2, col='goldenrod', ylim=c(0, 500), cex.axis=1.5, 
     cex.lab=1.5, ylab = 'Number of Aphids', main='Cold/Control', xlab = '', xaxt='n')
axis(side=1, a=c(1,2,3), labels=c('Day 0', 'Day 5', 'Day 10'), srt=45, cex=2, xpd=T)
lines(c1, type = "o", lwd=2, col='coral1')
lines(s1, type = 'o', lwd=2, col='firebrick4')
legend('topleft', cex=c(1,1,1), lty=c(1,1,1), lwd=3, col=c('firebrick4', 'coral1', 'goldenrod'), 
       legend=c('C7', 'CMAC', 'Control'))

plot(a2,type = "o", cex.main=1.5, lwd=2, col='goldenrod', ylim=c(0, 500), cex.axis=1.5, 
     cex.lab=1.5, ylab='', main='Warm/Control', xlab = '', xaxt='n')
axis(side=1, a=c(1,2,3), labels=c('Day 0', 'Day 5', 'Day 10'), srt=45, cex=2, xpd=T)
lines(c2, type = "o", lwd=2, col='coral1')
lines(s3, type = 'o', lwd=2, col='firebrick4')

plot(a3,type = "o", cex.main=1.5, lwd=2, col='goldenrod', ylim=c(0, 500), cex.axis=1.5, 
     cex.lab=1.5, ylab = 'Number of Aphids',main='Cold/Light', xlab = '', xaxt='n')
axis(side=1, a=c(1,2,3), labels=c('Day 0', 'Day 5', 'Day 10'), srt=45, cex=2, xpd=T)
lines(c3, type = "o", lwd=2, col='coral1')
lines(s3, type = 'o', lwd=2, col='firebrick4')

plot(a4,type = "o", cex.main=1.5, lwd=2, col='goldenrod', ylim=c(0, 500), cex.axis=1.5, 
     cex.lab=1.5, ylab='', main='Warm/Light', xlab = '', xaxt='n')
axis(side=1, a=c(1,2,3), labels=c('Day 0', 'Day 5', 'Day 10'), srt=45, cex=2, xpd=T)
lines(c4, type = "o", lwd=2, col='coral1')
lines(s4, type = 'o', lwd=2, col='firebrick4')

# charts that show a timeline of the study, days 0, 5, 10 for dropped aphids

# By Species
par(mfrow=c(1,3))
plot(da1,type = "o", cex.main=2, lwd=2, col='cornflowerblue', ylim=c(0, 70), cex.axis=1.5, 
     cex.lab=2,ylab = 'Number of Dropped Aphids', main='', xlab = '', xaxt='n')
axis(side=1, a=c(1,2,3), labels=c('Day 0', 'Day 5', 'Day 10'), srt=45, cex=2, xpd=T)
lines(da2, type = "o", lwd=2, col='tomato1')
lines(da3, type = 'o', lwd=2, col='dodgerblue4')
lines(da4, type = 'o', lwd=2, col='firebrick4')

plot(dc1,type = "o", cex.main=2, lwd=2, col='cornflowerblue', ylim=c(0, 70), cex.axis=1.5, 
     cex.lab=2,ylab='', main='', xlab = '', xaxt='n')
axis(side=1, a=c(1,2,3), labels=c('Day 0', 'Day 5', 'Day 10'), srt=45, cex=2, xpd=T)
lines(dc2, type = "o", lwd=2, col='tomato1')
lines(dc3, type = 'o', lwd=2, col='dodgerblue4')
lines(dc4, type = 'o', lwd=2, col='firebrick4')

plot(ds1,type = "o", cex.main=2, lwd=2, col='cornflowerblue', ylim=c(0, 70), cex.axis=1.5, 
     cex.lab=2,ylab='', main='', xlab = '', xaxt='n')
axis(side=1, a=c(1,2,3), labels=c('Day 0', 'Day 5', 'Day 10'), srt=45, cex=2, xpd=T)
lines(ds2, type = "o", lwd=2, col='tomato1')
lines(ds3, type = 'o', lwd=2, col='dodgerblue4')
lines(ds4, type = 'o', lwd=2, col='firebrick4')
legend('topleft', cex=c(1.5,1.5,1.5,1.5), lty=c(1,1,1,1), lwd=3, col=c('cornflowerblue', 'tomato1', 'dodgerblue4', 'firebrick4'), 
       legend=c('Cold/Control', 'Warm/Control', 'Cold/Light', 'Warm/Light'))


#### dabestr #####

multi.group <-
  d %>%
  dabest(Trt, Num_Aphids_DayTen, 
         idx = list(c('Warm/Control', 'Warm/Light'),
                    c('Cold/Control', 'Cold/Light')),
         paired=F)
multi.group
plot(multi.group, color.column = Species,
     rawplot.markersize = 1.5,
     rawplot.groupwidth = 0.4,
     rawplot.ylabel = "Number of Aphids on Day Ten",
     axes.title.fontsize = 12,
     palette = 'Dark2')

multi.group2 <-
  d %>%
  dabest(Trt, Num_Aphids_DayTen, 
         idx = list(c('Warm/Control', 'Cold/Control'),
                    c('Warm/Light', 'Cold/Light')),
         paired=F)
multi.group2
plot(multi.group2, color.column = Species,
     rawplot.markersize = 1.5,
     rawplot.groupwidth = 0.4,
     rawplot.ylabel = "Number of Aphids on Day Ten",
     axes.title.fontsize = 12,
     palette = 'Dark2')

multi.group3 <-
  d %>%
  dabest(Trt, Num_Aphids_DayTen, 
         idx = list(c('Warm/Control', 'Warm/Light'),
                    c('Cold/Control', 'Cold/Light')),
         paired=F)
multi.group3
plot(multi.group3, color.column = Species,
     rawplot.markersize = 1.5,
     rawplot.groupwidth = 0.4,
     rawplot.ylabel = "Number of Aphids on Day Ten",
     axes.title.fontsize = 12
     palette = 'Dark2')


two.group.C7 <-
  d %>%
  dabest(Species, Num_Aphids_DayTen, 
         idx = c('control', 'C7'),
         paired=F)
two.group.C7
plot(two.group.C7, color.column = Trt,
     rawplot.markersize = 1.5,
     rawplot.groupwidth = 0.4,
     rawplot.ylabel = "Number of Aphids on Day Ten",
     axes.title.fontsize = 12,
     palette = 'Dark2')

two.group.CMAC <-
  d %>%
  dabest(Species, Num_Aphids_DayTen, 
         idx = c('control', 'CMAC'),
         paired=F)
two.group.CMAC
plot(two.group.CMAC, color.column = Trt,
     rawplot.markersize = 1.5,
     rawplot.groupwidth = 0.4,
     rawplot.ylabel = "Number of Aphids on Day Ten",
     axes.title.fontsize = 12,
     palette = 'Dark2')
