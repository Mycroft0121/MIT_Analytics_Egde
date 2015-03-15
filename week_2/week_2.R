#The Statiscal Sommelier
wine <- read.csv("wine.csv")
model1<-lm(Price ~ AGST, data = wine)
SSE= sum(model1$residuals^2)
model2<-lm(Price ~ AGST + HarvestRain, data = wine)
model3 <- lm(Price ~ AGST + HarvestRain + WinterRain +Age + FrancePop, data= wine)
model4<-lm(Price ~ HarvestRain + WinterRain, data = wine)
cor(wine$HarvestRain,wine$WinterRain)
model5<-lm(Price ~ AGST+ HarvestRain + WinterRain + Age, data = wine)
wineTest <- read.csv("wine_test.csv")
predictTest <- predict(model5, newdata=wineTest)
SSE <- sum((wineTest$Price-predictTest)^2)
SST <- sum((wineTest$Price-mean(wineTest$Price))^2)
Rsquare = 1- SSE/SST


#Moneyball
baseball = read.csv("baseball.csv")
moneyball = subset(baseball, Year < 2002)
moneyball$RD =moneyball$RS-moneyball$RA
plot(moneyball$RD, moneyball$W)
WinsReg = lm(moneyball$W ~moneyball$RD)
teamRank = c(1,2,3,3,4,4,4,4,5,5)
wins2012 = c(94,88,95,88,93,94,98,97,93,94)
wins2013 = c(97,97,92,93,92,96,94,96,92,90)
cor(teamRank, wins2013)


#Recitation Play Moneyball in the NBA
NBA = read.csv("NBA_train.csv")
table(NBA$W, NBA$Playoffs)
NBA$PTSdiff = NBA$PTS - NBA$oppPTS
plot(NBA$PTSdiff, NBA$W)
WinsReg = lm(W~PTSdiff,data=NBA)
PointsReg = lm(PTS~ X2PA+X3PA + FTA+ AST+ORB+DRB+TOV+STL+BLK, data= NBA)
PointsReg$residuals
SSE = sum(PointsReg$residuals^2)
RMSE=sqrt(SSE/nrow(NBA))
mean(NBA$PTS)
PointsReg1 = lm(PTS~ X2PA+X3PA + FTA+ AST+ORB+DRB+STL+BLK, data= NBA)
PointsReg2 = lm(PTS~ X2PA+X3PA + FTA+ AST+ORB+STL+BLK, data= NBA)
PointsReg3 = lm(PTS~ X2PA+X3PA + FTA+ AST+ORB+DRB+STL, data= NBA)
SSE3 = sum(PointsReg3$residuals^2)
RMSE3=sqrt(SSE3/nrow(NBA))
NBA_test = read.csv("NBA_test.csv")
PointsPredictions = predict(PointsReg3, newdata = NBA_test)
SSE = sum((PointsPredictions-NBA_test$PTS)^2)
SST = sum((mean(NBA$PTS)-NBA_test$PTS)^2)
R2 = 1- SSE/SST
RMSE = sqrt(SSE/nrow(NBA_test))


#Homework
#Climate Change
climate_change =read.csv("climate_change.csv")
training = subset(climate_change, climate_change$Year<=2006)
testing = subset(climate_change, climate_change$Year>2006)
TempReg = lm(Temp~MEI+CO2+CH4+N2O+CFC.11+CFC.12+TSI+Aerosols, data = training)
summary(TempReg)
cor(training)
TempReg1 = lm(Temp~MEI+N2O+TSI+Aerosols, data = training)
summary(TempReg1)
stepModel = step(TempReg)
summary(stepModel)
TempPrediction =  predict(stepModel, newdata = testing)
summary(TempPrediction)
SSE = sum((TempPrediction-testing$Temp)^2)
SST = sum((mean(training$Temp)-testing$Temp)^2)
R2 = 1- SSE/SST
RMSE = sqrt(SSE/nrow(NBA_test))
R2

#Reading Test Scores
pisatraining = read.csv("pisa2009train.csv")
pisatesting = read.csv("pisa2009test.csv")
str(pisatraining)
tapply(pisatraining$readingScore,pisatraining$male,mean)
