source('calcN.R')
library(car)
library(lmtest)
library(multcomp)


## DATA EXPLORATION
v_data = readr::read_csv('results_fixed.csv')
v_data$algorithm = as.factor(v_data$algorithm)

v_dataSdRuns = aggregate(cbind(quality,time)~instance:algorithm:size,data=v_data, FUN=sd)

v_data = aggregate(cbind(quality,time,size,avgTransportations,avgAccommodations,destinations)~algorithm:instance, data=v_data, FUN=mean)

v_data$qualityRelative = v_data$quality/array(sapply(subset(v_data, algorithm=='ACO')$quality, function(v) return(rep(v,2))))


par(mfrow=c(1,2), mai=.4*c(2.5,1,1,1))

plot(x = log(subset(v_data, algorithm=='ACO')$size),
     y = log(subset(v_data, algorithm=='ACO')$time),
     cex  = 1,
     las  = 1,
     pch  = 16,
     col='red',
     xlab = "log size",
     ylab = "Time")
points(x = log(subset(v_data, algorithm=='ACO_Local')$size),
     y = log(subset(v_data, algorithm=='ACO_Local')$time),
     cex  = 1,
     pch  = 18,
     col='blue')
legend("topleft", legend=c('ACO', 'ACO+LocalSearch'), col=c("red", "blue"),  pch=c(16,18), cex=1)

plot(x = log(subset(v_data, algorithm=='ACO')$size),
     y = subset(v_data, algorithm=='ACO')$quality,
     cex  = 1,
     las  = 1,
     pch  = 16,
     col='red',
     xlab = "log size",
     ylab = "Quality")
points(x = log(subset(v_data, algorithm=='ACO_Local')$size),
       y = subset(v_data, algorithm=='ACO_Local')$quality,
       cex  = 1,
       pch  = 18,
       col='blue')
legend(x=10, y=0.25, legend=c('ACO', 'ACO+LocalSearch'), col=c("red", "blue"),  pch=c(16,18), cex=1)

plot(x = log(subset(v_data, algorithm=='ACO')$size),
     y = subset(v_data, algorithm=='ACO')$qualityRelative,
     cex  = 1,
     las  = 1,
     pch  = 16,
     col='red',
     xlab = "log size",
     ylab = "Relative Quality")
points(x = log(subset(v_data, algorithm=='ACO_Local')$size),
       y = subset(v_data, algorithm=='ACO_Local')$qualityRelative,
       cex  = 1,
       pch  = 18,
       col='blue')
legend("bottomleft", legend=c('ACO', 'ACO+LocalSearch'), col=c("red", "blue"),  pch=c(16,18), cex=1)


v_dataDiff = aggregate(cbind(quality, time, qualityRelative)~size:instance, data=v_data, FUN=diff)

par(mfrow=c(2,2), mai=.3*c(2.5,1,1,1))
plot(x = log(v_dataDiff$size),
     y = log(v_dataDiff$time),
     cex  = 1,
     las  = 1,
     pch  = 16,
     xlab = "log(size)",
     ylab = "Time Diff")

plot(x = log(v_dataDiff$size),
     y = v_dataDiff$quality,
     cex  = 1,
     las  = 1,
     pch  = 16,
     xlab = "log(size)",
     ylab = "Quality Diff")

plot(x = log(v_dataDiff$size),
     y = v_dataDiff$qualityRelative,
     cex  = 1,
     las  = 1,
     pch  = 16,
     xlab = "log(size)",
     ylab = "Quality Diff %")



## ANOVA with blocking

summary.aov(aov(log(time)~algorithm, data=v_data))

# Time - simple model
v_modelAnovaTime <- aov(log(time)~algorithm+log(size), 
                        data = v_data)
summary.aov(v_modelAnovaTime)
summary.lm(v_modelAnovaTime)

par(mfrow=c(2,2), mai=.3*c(2.5,1,1,1))
plot(v_modelAnovaTime)

v_tTestTime = t.test(log(time)~algorithm,
                        data=v_data,
                        mu = 0,
                        conf.level = 0.95, 
                        alternative = 'two.sided',
                        paired=TRUE
)
v_tTestTime

# Time - iteraction effects
v_modelAnovaTime <- aov(log(time)~algorithm*log(size), 
                        data = v_data)
summary.aov(v_modelAnovaTime)
summary.lm(v_modelAnovaTime)

par(mfrow=c(2,2), mai=.3*c(2.5,1,1,1))
plot(v_modelAnovaTime)


# Quality - simple model
v_modelAnovaQuality <- aov((qualityRelative)~algorithm+log(size), 
                           data = v_data)
summary.aov(v_modelAnovaQuality)
summary.lm(v_modelAnovaQuality)

par(mfrow=c(2,2), mai=.3*c(2.5,1,1,1))
plot(v_modelAnovaQuality)

v_tTestQuality = t.test(qualityRelative~algorithm,
                        data=v_data,
                        mu = -0.05,
                        conf.level = 0.95, 
                        alternative = 'less',
                        paired=TRUE
)
v_tTestQuality


## ANCOVA

v_data2 = v_data[seq(nrow(v_data)/2)*2 - rep(c(0,1),nrow(v_data)/4),]


plot(x = log(subset(v_data2, algorithm=='ACO')$size),
     y = log(subset(v_data2, algorithm=='ACO')$time),
     cex  = 1,
     las  = 1,
     pch  = 16,
     col='red',
     xlab = "log size",
     ylab = "Time")
points(x = log(subset(v_data2, algorithm=='ACO_Local')$size),
       y = log(subset(v_data2, algorithm=='ACO_Local')$time),
       cex  = 1,
       pch  = 18,
       col='blue')
legend("topleft", legend=c('ACO', 'ACO+LocalSearch'), col=c("red", "blue"),  pch=c(16,18), cex=1)

# Time model fit
v_modelAncovaTime = aov(log(time)~algorithm+log(size), data=v_data2)
summary.aov(v_modelAncovaTime)
summary.lm(v_modelAncovaTime)

v_modelAncovaTime2 = aov(log(time)~log(size)+algorithm, data=v_data2)
summary.aov(v_modelAncovaTime2)
summary.lm(v_modelAncovaTime2)

v_modelFitTime = lm(log(time)~log(size)+algorithm, data=v_data2)

plot(x = log(subset(v_data2, algorithm=='ACO')$size),
     y = log(subset(v_data2, algorithm=='ACO')$time),
     cex  = 1,
     las  = 1,
     pch  = 16,
     col='red',
     xlab = "log size",
     ylab = "Time")
points(x = log(subset(v_data2, algorithm=='ACO_Local')$size),
       y = log(subset(v_data2, algorithm=='ACO_Local')$time),
       cex  = 1,
       pch  = 18,
       col='blue')
legend("topleft", legend=c('ACO', 'ACO+LocalSearch'), col=c("red", "blue"),  pch=c(16,18), cex=1)
abline(coef=c(v_modelFitTime$coefficients[1],v_modelFitTime$coefficients[2]), col='red')
abline(coef=c(v_modelFitTime$coefficients[1]+v_modelFitTime$coefficients[3],v_modelFitTime$coefficients[2]), col='blue')

#abline(coef=c(v_modelFitTime$coefficients[1],v_modelFitTime$coefficients[2]), col='red')
#abline(coef=c(v_modelFitTime$coefficients[1]+v_modelFitTime$coefficients[3],v_modelFitTime$coefficients[2]+v_modelFitTime$coefficients[4]), col='blue')

par(mfrow=c(2,2), mai=.3*c(2.5,1,1,1))
plot(v_modelFitTime)

# Time model - diferentes inclinações

v_modelAncovaTime2 = aov(log(time)~algorithm*log(size), data=v_data2)
summary.aov(v_modelAncovaTime2)
v_r2Model2 = summary.lm(v_modelAncovaTime2)$r.square
  
v_modelFitTime = lm(log(time)~log(size)*algorithm, data=v_data2)

plot(x = log(subset(v_data2, algorithm=='ACO')$size),
     y = log(subset(v_data2, algorithm=='ACO')$time),
     cex  = 1,
     las  = 1,
     pch  = 16,
     col='red',
     xlab = "log size",
     ylab = "Time")
points(x = log(subset(v_data2, algorithm=='ACO_Local')$size),
       y = log(subset(v_data2, algorithm=='ACO_Local')$time),
       cex  = 1,
       pch  = 18,
       col='blue')
legend("topleft", legend=c('ACO', 'ACO+LocalSearch'), col=c("red", "blue"),  pch=c(16,18), cex=1)

abline(coef=c(v_modelFitTime$coefficients[1],v_modelFitTime$coefficients[2]), col='red')
abline(coef=c(v_modelFitTime$coefficients[1]+v_modelFitTime$coefficients[3],v_modelFitTime$coefficients[2]+v_modelFitTime$coefficients[4]), col='blue')


# Quality model fit
v_modelAncovaTime = aov((qualityRelative)~algorithm+log(size), data=v_data2)
summary.aov(v_modelAncovaTime)
summary.lm(v_modelAncovaTime)

v_modelAncovaTime2 = aov((qualityRelative)~log(size)+algorithm, data=v_data2)
summary.aov(v_modelAncovaTime2)
summary.lm(v_modelAncovaTime2)

v_modelFitTime = lm((qualityRelative)~log(size)+algorithm, data=v_data2)

plot(x = log(subset(v_data2, algorithm=='ACO')$size),
     y = (subset(v_data2, algorithm=='ACO')$qualityRelative),
     cex  = 1,
     las  = 1,
     pch  = 16,
     col='red',
     xlab = "log size",
     ylab = "Time")
points(x = log(subset(v_data2, algorithm=='ACO_Local')$size),
       y = (subset(v_data2, algorithm=='ACO_Local')$qualityRelative),
       cex  = 1,
       pch  = 18,
       col='blue')
legend("bottomright", legend=c('ACO', 'ACO+LocalSearch'), col=c("red", "blue"),  pch=c(16,18), cex=1)
abline(coef=c(v_modelFitTime$coefficients[1],v_modelFitTime$coefficients[2]), col='red')
abline(coef=c(v_modelFitTime$coefficients[1]+v_modelFitTime$coefficients[3],v_modelFitTime$coefficients[2]), col='blue')

par(mfrow=c(2,2), mai=.3*c(2.5,1,1,1))
plot(v_modelFitTime)

