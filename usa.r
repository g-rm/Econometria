USA <- read.csv("usa.csv")

#Regressao total
#---------------
pred <- lm(um ~ hdem + sdem + pres + elect + reagan + reagan + opp + ww2 + kwar + vwar + krus + gorb + salt + gnpdef + ug, data=USA)

summary(pred)

modelo_restrito <- function(explivativas, 



#Modelo Restrito 1
#-----------------
restrito <- lm(um ~ pres + reagan + reagan + ww2 + kwar + vwar + krus + gorb + salt + gnpdef + ug, data=USA)

#armazena residuo
USA$res <- restrito$residuals

#regressao todas explicativas vs residuo
pred1 <- lm(res ~ hdem + sdem + pres + elect + reagan + reagan + opp + ww2 + kwar + vwar + krus + gorb + salt + gnpdef + ug, data=USA)

summary(pred1)

#R2 n ajustado
R2 <- pred1.squared

#calcula p-valor
limiar <- nrow(USA) * R2

#n explicativas 
df1 <- 4

qchisq(.05,df1,,FALSE)

pchisq(limiar, df1,,FALSE)



#Modelo Restrito 2
#-----------------
restrito2 <- lm(um ~ hdem + sdem + pres + elect + reagan + reagan + opp + ww2 + kwar + vwar + krus + gnpdef + ug, data=USA)

#armazena
USA$res2 <- restrito2$residuals

#todas vs residuo
pred2 <- lm(res2 ~ hdem + sdem + pres + elect + reagan + reagan + opp + ww2 + kwar + vwar + krus + gorb + salt + gnpdef + ug, data=USA)

summary(pred2)

#R2 n ajustado
R2 <- pred1.squared

#calcula p-valor
limiar2 <- nrow(USA) * R2

df2 <- 2

qchisq(.05,df2,,FALSE)

pchisq(limiar2, df2,,FALSE)



#Modelo Restrito 3
#-----------------
restrito3 <- lm(um ~ hdem + sdem + pres + elect + reagan + reagan + opp + ww2 + gorb + salt + gnpdef + ug, data=USA)

#armazena
USA$res3 <- restrito3$residuals

#todas vs residuo
pred3 <- lm(res3 ~ hdem + sdem + pres + elect + reagan + reagan + opp + ww2 + kwar + vwar + krus + gorb + salt + gnpdef + ug, data=USA)

summary(pred3)

R2 <- 0.3064

limiar3 <- nrow(USA) * R2
limiar3

df3 <- 3

qchisq(.05,df3,,FALSE)

pchisq(limiar3, df3,,FALSE)
