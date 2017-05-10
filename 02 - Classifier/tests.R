source('calcN_tost.R')


# pilot
v_data = readr::read_csv('pilot_1992-11-21_17_30.csv')

v_dataAcc = aggregate(Accuracy~Instance:Algorithm,data=v_data, FUN=mean)
v_dataTime = aggregate(Time.s~Instance:Algorithm,data=v_data, FUN=mean)

v_dataTimeSD = aggregate(Time.s~Algorithm,data=v_dataTime, FUN=sd)
v_dataAccSD = aggregate(Accuracy~Algorithm,data=v_dataAcc, FUN=sd)


v_nTime = calcN_tost2(alpha = 0.05,
            beta = 0.2,
            diff_mu = 1,
            tolmargin = 16.13294,
            s1 = 26.33450,
            s2 = 16.13294)

v_nAcc = calcN_tost2(alpha = 0.05,
                      beta = 0.2,
                      diff_mu = 0.01,
                      tolmargin = 0.05,
                      s1 = 0.02128468,
                      s2 = 0.01945886)


# real data
v_data = readr::read_csv('1992-11-21_33_30.csv')

v_dataAcc = aggregate(Accuracy~Instance:Algorithm,data=v_data, FUN=mean)
v_dataTime = aggregate(Time.s~Instance:Algorithm,data=v_data, FUN=mean)

# hypothesis test Time

v_diffTime = v_dataTime[1:33,3] - v_dataTime[34:66,3]
t.test(v_diffTime,
       conf.level = 0.05,
        mu=0,
       alternative = 'less')


# hypothesis test acc
#v_diffAcc = v_dataAcc[1:33,3] - v_dataAcc[34:66,3]
v_diffAcc =  v_dataAcc[34:66,3] - v_dataAcc[1:33,3]
t.test(v_diffAcc, mu = 0.05, conf.level = 0.05, alternative = 'less')

t.test(v_diffAcc, mu = -0.05, conf.level = 0.05, alternative = 'greater')
