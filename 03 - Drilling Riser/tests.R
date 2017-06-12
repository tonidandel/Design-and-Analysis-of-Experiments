source('calcN.R')
library(car)
library(lmtest)


v_dataHist = readr::read_csv('riser1.csv')
v_sdHist = sd(v_dataHist$LogTTF)

v_alpha = 0.05
v_a = 4
v_k = v_a-1
v_alphaAdj = v_alpha/v_k
v_beta = 0.15
v_power = 1-v_beta
v_delta = 0.25

v_n = calcN_oneVsAll(p_alpha = v_alphaAdj,
            p_beta = v_beta,
            p_alternative = 'two-sided',
            p_k = v_k,
            p_sd = v_sdHist,
            p_delta = v_delta
)
#power.t.test(delta=v_delta,sd = v_sdHist, sig.level = v_alphaAdj,power = v_power,type = "two.sample",alternative = "one.sided")

v_nControl = ceiling(v_n*sqrt(v_k))


v_tau = c(-v_delta*(v_a-1)/v_a, rep(v_delta/v_a, v_k))
vartau = var(v_tau)
v_nAnova = power.anova.test(groups = v_a, between.var = vartau, within.var = v_sdHist^2, sig.level = v_alpha, power = v_power)$n


# sample data
v_data = readr::read_csv('1992-11-21_33_30.csv')
v_data = aggregate(cbind(Accuracy,Time.s)~Instance:Algorithm,data=v_data, FUN=mean)

splitInst = function (p_str){
  return (as.numeric(unlist(strsplit(p_str, 'Inst'))[2]))
}
v_data$Instance = unlist(lapply(v_data$Instance, splitInst))
v_data = v_data[order(v_data$Instance),]

# hypothesis test Time

v_diffTime = subset(v_data, Algorithm=='Proposed')$Time.s - subset(v_data, Algorithm=='Standard')$Time.s
v_tTestTime = t.test(v_diffTime,
                     conf.level = 0.05,
                     mu=0,
                     alternative = 'less'
)
v_pTime = v_tTestTime$p.value


# hypothesis test acc
v_diffAcc = subset(v_data, Algorithm=='Proposed')$Accuracy - subset(v_data, Algorithm=='Standard')$Accuracy
v_tTestAcc = t.test(v_diffAcc, 
                    mu = -0.05, 
                    conf.level = 0.05, 
                    alternative = 'greater'
)
v_pAcc = v_tTestAcc$p.value

## Hypothesis validation - Time

# Normality
qqPlot(v_diffTime)
v_shapiroTime = shapiro.test(v_diffTime)

# Independence
v_dwTime = dwtest(v_diffTime~1)
plot(v_diffTime, type='b')


## Hypothesis validation - Acc

# Normality
qqPlot(v_diffAcc)
v_shapiroAcc = shapiro.test(v_diffAcc)

# Independence
v_dwAcc = dwtest(v_diffAcc~1)
plot(v_diffAcc, type='b')

