source('calcN.R')
library(car)
library(lmtest)


## EXPERIMENTAL DEFINITIONS
v_alpha = 0.05
v_a = 4
v_k = v_a-1
v_alphaAdj = v_alpha/v_k
v_beta = 0.15
v_power = 1-v_beta
v_delta = 0.25


## SAMPLE SIZE CALC

v_dataHist = readr::read_csv('riser1.csv')
v_sdHist = sd(v_dataHist$LogTTF)

v_n = calcN_oneVsAll(p_alpha = v_alphaAdj,
            p_beta = v_beta,
            p_alternative = 'one-sided',
            p_k = v_k,
            p_sd = v_sdHist,
            p_delta = v_delta
)
#power.t.test(delta=v_delta,sd = v_sdHist, sig.level = v_alphaAdj,power = v_power,type = "two.sample",alternative = "one.sided")

v_nControl = ceiling(v_n*sqrt(v_k))

v_tau = c(-v_delta*(v_a-1)/v_a, rep(v_delta/v_a, v_k))
#v_tau = c(-v_delta/2, v_delta/2, rep(0, v_k-1))
vartau = var(v_tau)
v_nAnova = power.anova.test(groups = v_a, 
                            between.var = vartau, 
                            within.var = v_sdHist^2, 
                            sig.level = v_alpha, 
                            power = v_power)$n


### EXPERIMENT

## ANOVA

# sample data
#v_data = readr::read_csv('1995-01-01_121_68_68_68.csv')
#v_data = readr::read_csv('1995-01-01_20_20_20_20.csv')
v_data = readr::read_csv('1992-10-15_60_60_60_60.csv')
v_data$Riser = as.factor(v_data$Riser)
boxplot(LogTTF~Riser, 
        data = v_data, 
        xlab = "Riser",
        ylab = "LogTTF", 
        main = "Riser data",
        pch  = 16,
        col  = "gray")
v_model <- aov(LogTTF~Riser, 
             data = v_data)
summary.aov(v_model)


## HYPOTHESIS VALIDATION

# Homocedasticity
fligner.test(LogTTF~Riser, 
             data = v_data)

plot(x    = v_model$fitted.values,
     y    = v_model$residuals,
     cex  = 2,
     las  = 1,
     pch  = 16,
     xlab = "Fitted values",
     ylab = "Residuals")
grid(NULL,NULL, lwd=2, col = "#44444422")

# Normality
shapiro.test(v_model$residuals)

qqPlot(v_model$residuals, 
       pch = 16, 
       lwd = 3, 
       cex = 2, 
       las = 1)

# Independence
durbinWatsonTest(v_model)

plot(x    = seq_along(v_model$residuals),
     y    = v_model$residuals,
     type = "l",
     las  = 1,
     lwd  = 2,
     lty  = 1,
     xlab = "Residual order",
     ylab = "Residual value")
points(x    = seq_along(v_model$residuals),
       y    = v_model$residuals,
       type = "p",
       cex  = 2,
       pch  = 5,
       col  = as.numeric(v_data[, 2]))
grid(NA,NULL, lwd=2, col = "#44444422")

