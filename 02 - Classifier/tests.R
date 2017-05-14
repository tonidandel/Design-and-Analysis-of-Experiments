vsource('calcN.R')
library(car)
library(lmtest)


# pilot size calc
v_nPilot = calcN_pilotD(p_alpha = 0.05,
                        p_beta = 0.2,
                        p_type = 'one-sided',
                        p_d = 1
                        )


# sample size calc
v_data = readr::read_csv('pilot_1992-11-21_17_30.csv')

v_data = aggregate(cbind(Accuracy,Time.s)~Instance:Algorithm,data=v_data, FUN=mean)

v_dataSD = aggregate(cbind(Accuracy,Time.s)~Algorithm,data=v_data, FUN=sd)

v_nTime = calcN_tost2(alpha = 0.05,
            beta = 0.2,
            diff_mu = 1,
            tolmargin = min(v_dataSD$Time.s),
            s1 = v_dataSD$Time.s[1],
            s2 = v_dataSD$Time.s[2]
            )

v_nAcc = calcN_tost2(alpha = 0.05,
                      beta = 0.2,
                      diff_mu = 0.01,
                      tolmargin = 0.05,
                      s1 = v_dataSD$Accuracy[1],
                      s2 = v_dataSD$Accuracy[2]
                     )

v_n = ceiling(max(v_nTime, v_nAcc))


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

