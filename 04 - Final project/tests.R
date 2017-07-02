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
#v_minQuality = aggregate(cbind(quality)~size:instance, data=v_data, FUN=min)$quality


par(mfrow=c(1,2), mai=.3*c(2.5,1,1,1))
plot(x = log(subset(v_dataSdRuns, algorithm=='ACO')$size),
     y = subset(v_dataSdRuns, algorithm=='ACO')$quality,
     cex  = 1,
     las  = 1,
     pch  = 16,
     col='red',
     xlab = "log size",
     ylab = "Quality Run SD")
points(x = log(subset(v_dataSdRuns, algorithm=='ACO_Local')$size),
     y = subset(v_dataSdRuns, algorithm=='ACO_Local')$quality,
     cex  = 1,
     pch  = 18,
     col='blue')

plot(x = log(subset(v_dataSdRuns, algorithm=='ACO')$size),
     y = subset(v_dataSdRuns, algorithm=='ACO')$time,
     cex  = 1,
     las  = 1,
     pch  = 16,
     col='red',
     xlab = "log size",
     ylab = "Time Run SD")
points(x = log(subset(v_dataSdRuns, algorithm=='ACO_Local')$size),
     y = subset(v_dataSdRuns, algorithm=='ACO_Local')$time,
     cex  = 1,
     pch  = 18,
     col='blue')


par(mfrow=c(2,2), mai=.4*c(2.5,1,1,1))

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
     y = v_dataDiff$time,
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


# HIPOTHESIS VALIDATIONS
qqPlot(v_data$time)
qqPlot(log(v_data$time))
qqPlot(v_data$quality)
qqPlot(v_data$qualityRelative)

## ANCOVA
# Time model fit
v_modelTimeFit = lm(time~size*factor(algorithm)+I(size^2), data=v_data)
Anova(v_modelTimeFit)

abline(coef = c(v_modelTimeFit$coefficients[1], v_modelTimeFit$coefficients[2]), col='red')
abline(coef = c(v_modelTimeFit$coefficients[1]+v_modelTimeFit$coefficients[3], v_modelTimeFit$coefficients[2]), col='blue')
plot(v_modelTimeFit, which=1)
plot(v_modelTimeFit, which=2)

# quality model fit
v_modelQualityFit = lm(quality~size*factor(algorithm)+I(size^2), data=v_data)
Anova(v_modelTimeFit)

abline(coef = c(v_modelQualityFit$coefficients[1], v_modelQualityFit$coefficients[2]), col='red')
abline(coef = c(v_modelQualityFit$v_modelQualityFit[1]+v_modelQualityFit$coefficients[3], v_modelQualityFit$coefficients[2]), col='blue')
plot(v_modelQualityFit, which=1)
plot(v_modelQualityFit, which=2)


# ANOVA with blocking

# Time - simple model
v_modelAnovaTime <- aov(log(time)~algorithm+log(size), 
                        data = v_data)
summary.aov(v_modelAnovaTime)
summary.lm(v_modelAnovaTime)

par(mfrow=c(2,2), mai=.3*c(2.5,1,1,1))
plot(v_modelAnovaTime)

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
                        mu = 0,
                        conf.level = 0.95, 
                        alternative = 'two.sided',
                        paired=TRUE
                        )