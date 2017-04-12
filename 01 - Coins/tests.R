if (!require(readr, quietly = TRUE)){
  install.packages("readr")
}
if (!require(car, quietly = TRUE)){
  install.packages("car")
}
if (!require(lmtest, quietly = TRUE)){
  install.packages("lmtest")
}

v_data = readr::read_csv('data.csv')
v_coins = v_data$coins
v_value = v_data$value


###
v_coins.mean = mean(v_coins)
v_coins.sd = sd(v_coins)
v_coins.rsd = sd(v_coins)/mean(v_coins)

v_value.mean = mean(v_value)
v_value.sd = sd(v_value)
v_value.rsd = sd(v_value)/mean(v_value)

## normality assumption
qqPlot(v_coins)
qqPlot(v_value)
title("fdisdfi")

hist(v_coins)
hist(v_value)

shapiro.test(v_coins)
shapiro.test(v_value)

## independence assumption
dwtest(v_coins~1)
dwtest(v_value~1)


## inference
t.test(v_coins, mu=130)
t.test(v_value, mu=2.60*3.5)


## 
power.t.test(n=29, delta=10, sd=v_coins.sd, sig.level=0.05, type="one.sample", alternative="two.sided")
power.t.test(power=0.8, delta=5, sd=v_coins.sd, sig.level=0.05, type="one.sample", alternative="two.sided")

power.t.test(n=29, delta=0.05, sd=v_value.sd, sig.level=0.05, type="one.sample", alternative="two.sided")
power.t.test(power=0.8, delta=0.5, sd=v_value.sd, sig.level=0.05, type="one.sample", alternative="two.sided")

