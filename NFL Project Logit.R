library(nflfastR)
library(tidyverse)
library(ggimage)
library(ggrepel)
library(mosaic)
library(Stat2Data)

######################################## Read the data in ######################################


seasons <- 1999:2019
highPressure <- map_df(seasons, function(x) {
  readRDS(
    url(
      paste0("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_",x,".rds")
    )
  ) %>%
    filter(field_goal_attempt==1, week <= 17, score_differential<=0 & score_differential>=-3, game_seconds_remaining<=60,
           field_goal_result!="blocked") %>%
    select(season, field_goal_attempt, down, surface, temp, field_goal_result, kick_distance, score_differential, wind,
           kicker_player_name)
})

lowPressure <- map_df(seasons, function(x) {
  readRDS(
    url(
      paste0("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_",x,".rds")
    )
  ) %>%
    filter(field_goal_attempt==1, week <= 17, (game_seconds_remaining>2700 & game_seconds_remaining<=3600) | 
             (game_seconds_remaining<300 & score_differential>=21), field_goal_result!="blocked") %>%
    select(season, field_goal_attempt, down, surface, temp, field_goal_result, kick_distance, score_differential, wind,
           kicker_player_name)
})


##################### Logistic Regression HP & LP Model ##############################################


hpFiltered <- na.omit(highPressure)
lpFiltered <- na.omit(lowPressure)
hpFiltered$field_goal_result <- as.numeric(hpFiltered$field_goal_result=="made")
lpFiltered$field_goal_result <- as.numeric(lpFiltered$field_goal_result=="made")

hpFiltered$surface[hpFiltered$surface=="grass"] <- 1
hpFiltered$surface[hpFiltered$surface=="dessograss"] <- 0
hpFiltered$surface[hpFiltered$surface=="astroturf"] <- 0
hpFiltered$surface[hpFiltered$surface=="fieldturf"] <- 0
hpFiltered$surface[hpFiltered$surface=="a_turf"] <- 0
hpFiltered$surface[hpFiltered$surface=="astroplay"] <- 0
hpFiltered$surface[hpFiltered$surface=="sportturf"] <- 0

lpFiltered$surface[lpFiltered$surface=="grass"] <- 1
lpFiltered$surface[lpFiltered$surface=="dessograss"] <- 0
lpFiltered$surface[lpFiltered$surface=="astroturf"] <- 0
lpFiltered$surface[lpFiltered$surface=="fieldturf"] <- 0
lpFiltered$surface[lpFiltered$surface=="a_turf"] <- 0
lpFiltered$surface[lpFiltered$surface=="astroplay"] <- 0
lpFiltered$surface[lpFiltered$surface=="sportturf"] <- 0



hpLogit <- glm(field_goal_result ~ as.factor(surface) + temp + kick_distance + wind, data = hpFiltered,
               family = binomial(link="logit"))

lpLogit <- glm(field_goal_result ~ as.factor(surface) + temp + kick_distance + wind, data = lpFiltered,
               family = binomial(link="logit"))

summary(hpLogit)
summary(lpLogit)

lpSamp <- lpFiltered[sample(nrow(lpFiltered), 431),]
summary(glm(field_goal_result ~ as.factor(surface) + temp + kick_distance + wind, data = lpSamp,
            family = binomial(link="logit")))

confint(hpLogit)
confint(lpLogit)


##################### Logistic Regression 1 large Model; EDA ##############################


write.csv(hpFiltered,"C:\\temp\\hpfiltered.csv", row.names = FALSE)
write.csv(lpFiltered,"C:\\temp\\lpfiltered.csv", row.names = FALSE)

lphpLogit <- glm(field_goal_result ~ as.factor(surface) + as.factor(surface):Pressure + temp + temp:Pressure + kick_distance + kick_distance:Pressure + wind + wind:Pressure, data = lphpfiltered,
                 family = binomial(link="logit"))

summary(lphpLogit)

lphpLogitKick = glm(field_goal_result ~ kick_distance, family = binomial, data = lphpfiltered)
plot(jitter(field_goal_result, amount = 0.025) ~ kick_distance, ylab="Probability of Making Field Goal", xlab="Kick Distance", data = lphpfiltered)
curve(predict(lphpLogitKick, data.frame(kick_distance = x), type = "response"), col = "blue", add = TRUE)

lphpLogitTemp = glm(field_goal_result ~ temp, family = binomial, data = lphpfiltered)
plot(jitter(field_goal_result, amount = 0.025) ~ temp, ylab="Probability of Making Field Goal", xlab="Temperature (F)", data = lphpfiltered)
curve(predict(lphpLogitTemp, data.frame(temp = x), type = "response"), col = "red", add = TRUE)

lphpLogitWind = glm(field_goal_result ~ wind, family = binomial, data = lphpfiltered)
plot(jitter(field_goal_result, amount = 0.025) ~ wind, ylab="Probability of Making Field Goal", xlab="Wind (mph)", data = lphpfiltered)
curve(predict(lphpLogitWind, data.frame(wind = x), type = "response"), col = "red", add = TRUE)


xyplot(wind~temp, type = c("p", "r"), data = lphpfiltered)
xyplot(wind~kick_distance,type = c("p", "r"), data = lphpfiltered)
xyplot(temp~kick_distance, type = c("p", "r"),data = lphpfiltered)

lphpLogitSurf1 = glm(surface ~ wind, family = binomial, data = lphpfiltered)
plot(jitter(surface, amount = 0.025) ~ wind, ylab="Surface", xlab="Wind (mph)", data = lphpfiltered)
curve(predict(lphpLogitSurf1, data.frame(wind = x), type = "response"), col = "red", add = TRUE)

lphpLogitSurf2 = glm(surface ~ temp, family = binomial, data = lphpfiltered)
plot(jitter(surface, amount = 0.025) ~ wind, ylab="Surface", xlab="Temperature (F)", data = lphpfiltered)
curve(predict(lphpLogitSurf2, data.frame(temp = x), type = "response"), col = "red", add = TRUE)

lphpLogitSurf3 = glm(surface ~ kick_distance, family = binomial, data = lphpfiltered)
plot(jitter(surface, amount = 0.025) ~ kick_distance, ylab="Surface", xlab="Kick Distance", data = lphpfiltered)
curve(predict(lphpLogitSurf3, data.frame(kick_distance = x), type = "response"), col = "red", add = TRUE)

summary(lphpLogitSurf2)
boxplot(temp~field_goal_result+surface, data = lphpfiltered)
boxplot(kick_distance~field_goal_result+surface, data = lphpfiltered)

table(lphpfiltered$field_goal_result, lphpfiltered$surface)

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  corrplot::
  corrplot(lphpfiltered, method = "circle")
cor.test(lphpfiltered)
cor(wind~temp, data = lphpfiltered)
cor(wind~kick_distance, data = lphpfiltered)
cor(temp~kick_distance, data = lphpfiltered)
cor(as.factor(surface)~kick_distance, data = lphpfiltered)
chisq.test(surfacec~roof, data = lphpfiltered)
anova(surfroof)


###################### Logistic Regression, LP = 4th Quarter, 5 min, 21 pt. down only ################


lowPressure4 <- map_df(seasons, function(x) {
  readRDS(
    url(
      paste0("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_",x,".rds")
    )
  ) %>%
    filter(field_goal_attempt==1, week <= 17, (game_seconds_remaining<300 & score_differential>=21),
           field_goal_result!="blocked") %>%
    select(season, field_goal_attempt, down, surface, temp, field_goal_result, kick_distance, score_differential, wind,
           roof, kicker_player_name)
})

clpFiltered4 <- na.omit(lowPressure4)
lpFiltered4$field_goal_result <- as.numeric(lpFiltered4$field_goal_result=="made")

lpFiltered4$surface[lpFiltered4$surface=="grass"] <- 1
lpFiltered4$surface[lpFiltered4$surface=="dessograss"] <- 0
lpFiltered4$surface[lpFiltered4$surface=="astroturf"] <- 0
lpFiltered4$surface[lpFiltered4$surface=="fieldturf"] <- 0
lpFiltered4$surface[lpFiltered4$surface=="a_turf"] <- 0
lpFiltered4$surface[lpFiltered4$surface=="astroplay"] <- 0
lpFiltered4$surface[lpFiltered4$surface=="sportturf"] <- 0


lpLogit4 <- glm(field_goal_result ~ as.factor(surface) + temp + kick_distance + wind, data = lpFiltered4,
                family = binomial(link="logit"))

summary(lpLogit4)

confint(hpLogit)
confint(lpLogit4)

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  write.csv(lpFiltered4,"C:\\temp\\lpfiltered4.csv", row.names = FALSE)

lp4hpLogit <- glm(field_goal_result ~ as.factor(surface) + as.factor(surface):Pressure + temp + temp:Pressure + kick_distance + kick_distance:Pressure + wind + wind:Pressure, data = lp4_hpfiltered,
                  family = binomial(link="logit"))

summary(lp4hpLogit)


###################### Logistic Regression LP = 1st Quarter only ################


lowPressure1 <- map_df(seasons, function(x) {
  readRDS(
    url(
      paste0("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_",x,".rds")
    )
  ) %>%
    filter(field_goal_attempt==1, week <= 17, (game_seconds_remaining>2700 & game_seconds_remaining<=3600),
           field_goal_result!="blocked") %>%
    select(season, field_goal_attempt, down, surface, temp, field_goal_result, kick_distance, score_differential, wind,
           roof, kicker_player_name)
})

lpFiltered1 <- na.omit(lowPressure1)
lpFiltered1$field_goal_result <- as.numeric(lpFiltered1$field_goal_result=="made")

lpFiltered1$surface[lpFiltered1$surface=="grass"] <- 1
lpFiltered1$surface[lpFiltered1$surface=="dessograss"] <- 0
lpFiltered1$surface[lpFiltered1$surface=="astroturf"] <- 0
lpFiltered1$surface[lpFiltered1$surface=="fieldturf"] <- 0
lpFiltered1$surface[lpFiltered1$surface=="a_turf"] <- 0
lpFiltered1$surface[lpFiltered1$surface=="astroplay"] <- 0
lpFiltered1$surface[lpFiltered1$surface=="sportturf"] <- 0

lpLogit1 <- glm(field_goal_result ~ as.factor(surface) + temp + kick_distance + wind, data = lpFiltered1,
                family = binomial(link="logit"))

summary(lpLogit1)

confint(hpLogit)
confint(lpLogit1)

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  write.csv(lpFiltered1,"C:\\temp\\lpfiltered1.csv", row.names = FALSE)

lp1hpLogit <- glm(field_goal_result ~ as.factor(surface) + as.factor(surface):Pressure + temp + temp:Pressure + kick_distance + kick_distance:Pressure + wind + wind:Pressure, data = lp1hpfiltered,
                  family = binomial(link="logit"))

summary(lp1hpLogit)


############################################# Final Logistic Regression (Narrowing Variables)#######################


lpLogit1f <- glm(field_goal_result ~ temp + kick_distance, data = lpFiltered1,
                 family = binomial(link="logit"))
hpLogitf <- glm(field_goal_result ~ temp + kick_distance, data = hpFiltered,
                family = binomial(link="logit"))

summary(lpLogit1f)
summary(hpLogitf)

confint(hpLogitf)
confint(lpLogit1f)



lp1hpLogitf <- glm(field_goal_result ~  temp + temp:Pressure + kick_distance + kick_distance:Pressure, data = lp1hpfiltered,
                   family = binomial(link="logit"))

summary(lp1hpLogitf)


############################################## Logit Plots of LP = 1 quarter only ###############################################


emplogitplot1(field_goal_result ~ kick_distance, data = lpFiltered1)

lp1Logitkick = glm(field_goal_result ~ kick_distance, family = binomial, data = lpFiltered1)
plot(jitter(field_goal_result, amount = 0.1) ~ kick_distance, data = lpFiltered1)
curve(predict(lp1Logitkick, data.frame(kick_distance = x), type = "response"), col = "blue", add = TRUE)

lp1Logittemp = glm(field_goal_result ~ temp, family = binomial, data = lpFiltered1)
plot(jitter(field_goal_result, amount = 0.1) ~ temp, data = lpFiltered1)
curve(predict(lp1Logittemp, data.frame(temp = x), type = "response"), col = "blue", add = TRUE)



hpLogittemp = glm(field_goal_result ~ temp, family = binomial, data = hpFiltered)
lp1Logittemp = glm(field_goal_result ~ temp, family = binomial, data = lpFiltered1)
plot(field_goal_result ~ temp, type = "n", ylab="Probability of Making Field Goal", xlab="Temperature (F)", data = hpFiltered)
curve(predict(hpLogittemp, data.frame(temp = x), type = "response"), col = "blue", add = TRUE)
curve(predict(lp1Logittemp, data.frame(temp = x), type = "response"), col = "red", add = TRUE)
legend("topleft", legend=c("Low pressure", "High pressure"),
       col=c("red", "blue"), lty=1:1, cex=0.8)

hpLogitkick = glm(field_goal_result ~ kick_distance, family = binomial, data = hpFiltered)
lp1Logitkick = glm(field_goal_result ~ kick_distance, family = binomial, data = lpFiltered1)
plot(field_goal_result ~ kick_distance, type = "n", ylab="Probability of Making Field Goal", xlab="Kick Distance", data = hpFiltered)
curve(predict(hpLogitkick, data.frame(kick_distance = x), type = "response"), col = "blue", add = TRUE)
curve(predict(lp1Logitkick, data.frame(kick_distance = x), type = "response"), col = "red", add = TRUE)
legend("topright", legend=c("Low pressure", "High pressure"),
       col=c("red", "blue"), lty=1:1, cex=0.8)

hpLogitwind = glm(field_goal_result ~ wind, family = binomial, data = hpFiltered)
lp1Logitwind = glm(field_goal_result ~ wind, family = binomial, data = lpFiltered1)
plot(field_goal_result ~ wind, type = "n", ylab="Probability of Making Field Goal", xlab="Wind (mph)", data = hpFiltered)
curve(predict(hpLogitwind, data.frame(wind = x), type = "response"), col = "blue", add = TRUE)
curve(predict(lp1Logitwind, data.frame(wind = x), type = "response"), col = "red", add = TRUE)
legend("topleft", legend=c("Low pressure", "High pressure"),
       col=c("red", "blue"), lty=1:1, cex=0.8)



################################### Additional, Unusused R code ################################


set.seed(1234)
mydat <- data.frame(
  won=as.factor(sample(c(0, 1), 250, replace=TRUE)), 
  bid=runif(250, min=0, max=1000)
)
hpLogitwind = glm(field_goal_result ~ wind, data = hpFiltered,
                  family = binomial(link="logit"))
plotdat <- data.frame(bid=(0:1000))
preddat <- predict(hpLogitwind, se.fit=TRUE)
plot(field_goal_result ~ wind, type="n", 
     ylim=c(0, 1), ylab="Probability of making", xlab="mph", data = hpFiltered)
with(preddat, lines(0:1000, exp(fit)/(1+exp(fit)), col="blue"))
with(preddat, lines(0:1000, exp(fit+1.96*se.fit)/(1+exp(fit+1.96*se.fit)), lty=2))
with(preddat, lines(0:1000, exp(fit-1.96*se.fit)/(1+exp(fit-1.96*se.fit)), lty=2))


set.seed(1234)
mydat <- data.frame(
  won=as.factor(sample(c(0, 1), 250, replace=TRUE)), 
  bid=runif(250, min=0, max=1000)
)
mod1 <- glm(won~bid, data=mydat, family=binomial(link="logit"))
plotdat <- data.frame(bid=(0:1000))
preddat <- predict(mod1, newdata=plotdat, se.fit=TRUE)
with(mydat, plot(bid, won, type="n", 
                 ylim=c(0, 1), ylab="Probability of winning", xlab="Bid"))
with(preddat, lines(0:1000, exp(fit)/(1+exp(fit)), col="blue"))
with(preddat, lines(0:1000, exp(fit+1.96*se.fit)/(1+exp(fit+1.96*se.fit)), lty=2))
with(preddat, lines(0:1000, exp(fit-1.96*se.fit)/(1+exp(fit-1.96*se.fit)), lty=2))


hpLogitsurf = glm(field_goal_result ~ as.factor(surface), family = binomial, data = hpFiltered)
lp1Logitsurf = glm(field_goal_result ~ as.factor(surface), family = binomial, data = lpFiltered1)
dotplot(field_goal_result ~ as.factor(surface), data = hpFiltered)
curve(predict(hpLogitkick, data.frame(as.factor(surface) = x), type = "response"), col = "blue", add = TRUE)
curve(predict(lp1Logitkick, data.frame(as.factor(surface) = x), type = "response"), col = "red", add = TRUE)
legend("topright", legend=c("Low pressure", "High pressure"),
       col=c("red", "blue"), lty=1:1, cex=0.8)
