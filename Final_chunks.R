## @knitr 1.1_read_training_data
pathToTrainingData <- paste0(RawData, "/fredgraph.csv")
mbsTrain <- data.table(read.csv(pathToTrainingData, sep=',', stringsAsFactors = F))

pathToTrainingData2 <- paste0(RawData, "/cleandt2.csv")
mortTrain <- data.table(read.csv(pathToTrainingData2, sep=',', stringsAsFactors = F))

pathToTraingData3 <- paste0(RawData, "/dgs19.csv")
treas_10yr <- data.table(read.csv(pathToTraingData3, sep=',', stringsAsFactors = F))

pathToTraingData4 <- paste0(RawData, "/hpi_case_cpi.csv")
hpi_cpi <- data.table(read.csv(pathToTraingData4, sep=',', stringsAsFactors = F))

pathToTrainingData5 <- paste0(RawData, "/payems.csv")
nfp_rate <- data.table(read.csv(pathToTrainingData5, sep=',', stringsAsFactors = F))

pathToTrainingData6 <- paste0(RawData, "/ism_index.csv")
ism <- data.table(read.csv(pathToTrainingData6, sep=',', stringsAsFactors = F))

kable(head(mbsTrain,5))
kable(head(mortTrain,5))
kable(head(treas_10yr,5))
kable(head(hpi_cpi,5))

## @knitr 1.2_data_cleaning
# This part will consist of renaming and setting our data to a time-series format so we can set time-series
# forecasting models

setnames(mbsTrain, old=c("DATE", "WSHOMCB", "TREAST"),
         new=c("date", "mbs_bal","treas_bal"))

setnames(treas_10yr, old=c("DATE", "DGS10"),
         new=c("date", "treas_10"))

mbsTrain$date <- as.Date(mbsTrain$date, "%Y-%m-%d")
mortTrain$week <- as.Date(mortTrain$week, "%m/%d/%Y")
treas_10yr$date <- as.Date(treas_10yr$date, "%m/%d/%Y")
hpi_cpi$date <- as.Date(hpi_cpi$date, "%m/%d/%Y")


mbsTrain$date <- NULL
mortTrain$week <- NULL
treas_10yr$date <- NULL
hpi_cpi$date <- NULL

mbsTrain1 <- ts(mbsTrain, start=c(2002,12,18), frequency = 52.18) ## Weekly
mortTrain1 <- ts(mortTrain, start=c(1971,4,2), frequency = 52.18) ## Weekly
treas_10yr1 <- ts(treas_10yr, start=c(2002,12,18), frequency = 52.18) ## Weekly
hpi_cpi1 <- ts(hpi_cpi, start=c(2001,1,1), frequency = 12) ## Monthly

str(mbsTrain)
str(mortTrain)
str(hpi_cpi)
str(treas_10yr)

sapply(mbsTrain, function(x) sum(is.na(x)))
sapply(mortTrain, function(x) sum(is.na(x)))
sapply(hpi_cpi, function(x) sum(is.na(x)))
sapply(treas_10yr, function(x) sum(is.na(x)))

## @knitr 1.3_mbs_plt
mt_dta <- autoplot(mbsTrain1) +
  xlab("Year") + ylab("in Millions of dollars") +
  ggtitle("Graph 1: Securities Held Outright by Federal Reserve") + labs(subtitle="Shaded Areas Indicate U.S. Recession")

mt_dta + labs(colour = "Security") + labs(caption="Source: Board of Governors of the Federal Reserve System (US) (2019)") +
  scale_color_manual(labels = c("MBS", "Treasuries"), values = c(2, 4)) + scale_y_continuous(label=comma)+       geom_line() +
  annotate("rect", fill = "grey", alpha = 0.6, 
           xmin = 2007 + 12/12, xmax = 2009 + 06/12,
           ymin = -Inf, ymax = Inf)

## @knitr 1.3_mortgage_plt
mort_plt <- autoplot(mortTrain1) +
  xlab("Years (Weekly Observations)") + ylab("Mortgage Interest Rate") +
  ggtitle("Graph 2: Mortgage Rates By Mauturities") + labs(subtitle="Shaded Areas Indicate U.S. Recession") + 
  labs(caption="Source: Freddie Mac Primary Mortgage Market Survey (2019)")

mort_plt + labs(colour = "Maturities") +
  scale_color_manual(labels = c("30YR Fixed", "15YR Fixed", "1 YR Treasury", "1 YR Margin")
                     , values = c(1, 2, 3, 4))+ geom_line() +
  annotate("rect", fill = "grey", alpha = 0.6
           , xmin = c(1980 + 01/4, 1981 + 03/7, 1990 + 06/7, 2001 + 02/3, 2007 + 12/12)
           , xmax = c(1980 + 04/7, 1982 + 05/11, 1991 + 01/3, 2001 + 02/11, 2009 + 06/12),
           ymin = -Inf, ymax = Inf)

## @knitr 1.3_treas_plt
treas_plt <- autoplot(treas_10yr1) +
  xlab("Years (Weekly Observations)") + ylab("Interest Rate") +
  ggtitle("Graph 3: 10 Year constant Treasury") + labs(subtitle="Shaded Areas Indicate U.S. Recession")+labs(caption="Source: Board of Governors of the Federal Reserve System (US) (2019)")

treas_plt + labs(colour = "Maturities") +
  scale_color_manual(labels = c("10YR Treas")
                     , values = c(1, 2, 3, 4))+ geom_line() +
  annotate("rect", fill = "grey", alpha = 0.6
           , xmin = c(2007 + 12/12)
           , xmax = c(2009 + 06/12),
           ymin = -Inf, ymax = Inf)

## @knitr 1.3_hpi_plt
hpi_plt <- autoplot(hpi_cpi1) +
  xlab("Years (Monthly Observations)") + ylab("Rate") +
  ggtitle("Graph 4: Case-Schiller Home Price Index") + labs(subtitle="Shaded Areas Indicate U.S. Recession")+labs(caption="Source:  S&P Dow Jones Indices LLC")

hpi_plt + labs(colour = "Label") +
  scale_color_manual(labels = c("HPI", "Nominal HPI", "CPI")
                     , values = c(1, 2, 3, 4))+ geom_line() +
  annotate("rect", fill = "grey", alpha = 0.6
           , xmin = c(2007 + 12/12)
           , xmax = c(2009 + 06/12),
           ymin = -Inf, ymax = Inf)

## @knitr 1.3_merge_data
mbsTrain2 <- data.table(read.csv(pathToTrainingData, sep=',', stringsAsFactors = F))
mortTrain2 <- data.table(read.csv(pathToTrainingData2, sep=',', stringsAsFactors = F))
treas_10yr2 <- data.table(read.csv(pathToTraingData3, sep=',', stringsAsFactors = F))
hpi_cpi2 <- data.table(read.csv(pathToTraingData4, sep=',', stringsAsFactors = F))

mortTrain2$yr1_treas <- NULL
mortTrain2$yr1_margin <- NULL
hpi_cpi2$nominal_hpi <- NULL
hpi_cpi2$cpi <- NULL

setnames(mbsTrain2, old=c("DATE", "WSHOMCB", "TREAST"),
         new=c("date", "mbs_bal","treas_bal"))

setnames(treas_10yr2, old=c("DATE", "DGS10"),
         new=c("date", "treas_10"))

mbsTrain2[,dummy:=.I]
mortTrain2[,dummy:=.I]
treas_10yr2[,dummy:=.I]
hpi_cpi2[,dummy:=.I]

hpi_cpi3 <- subset(hpi_cpi2, dummy >= 24)
mortTrain3 <- subset(mortTrain2, dummy >= 1654)

mbsTrain2$date <- as.Date(mbsTrain2$date, "%Y-%m-%d")
mortTrain3$week <- as.Date(mortTrain3$week, "%m/%d/%Y")
treas_10yr2$date <- as.Date(treas_10yr2$date, "%m/%d/%Y")
hpi_cpi3$date <- as.Date(hpi_cpi3$date, "%m/%d/%Y")

setnames(mortTrain3, old="week", new = "date")

x_test <- merge(mbsTrain2, treas_10yr2)
x_test$dummy <- NULL
x_test[,w_1:=week(date)]
x_test[,y_1:=year(date)]
mortTrain3$dummy <- NULL
mortTrain3[,w_1:=week(date)]
mortTrain3[,y_1:=year(date)]

x2_test <- merge(mortTrain3, x_test
                 , by.x=c("w_1","y_1")
                 , by.y=c("w_1","y_1"), all.x=TRUE)

x2_test[!complete.cases(x2_test), ] #We have some NA values, which appears that we can drop them as they are extra generated variables

x2_test <- na.omit(x2_test)

hpi_cpi3[,w_1:=week(date)]
hpi_cpi3[,y_1:=year(date)]
hpi_cpi3[,m_1:=month(date)]

hpi_cpi3$dummy <- NULL

x2_test[,m_1:=month(date.x)]

# x3_test <- merge(x2_test, hpi_cpi3
#                  , by.x=c("m_1", "y_1")
#                  , by.y=c("m_1", "y_1"), all.x=TRUE)
# 
# x3_test[, c("m_1","y_1","date","w_1.y","w_1.x","date.y"):=NULL]
# 
# setnames(x3_test, old = "date.x", new = "date")

# THIS IS GOING TO BE CORRECT THIS TIME#
x2_test[, mix_d:=paste0(m_1,y_1)]

d1 <- x2_test[,.(yr_15_fixed=mean(yr15_fixed)
                 , yr_30_fixed=mean(yr30_fixed)
                 , mbs1=mean(mbs_bal)
                 , treas1=mean(treas_bal)
                 , treas_10=mean(treas_10)), by = mix_d]

hpi_cpi3[, mix_d:=paste0(m_1,y_1)]
hpi_cpi3 <- hpi_cpi3[-c(194:195)]
cmb_master <- merge(hpi_cpi3,d1
                 , by.x=c("mix_d")
                 , by.y=c("mix_d"), all.x=TRUE)
cmb_master <- cmb_master[(-49)]
cmb_master <- cmb_master[order(cmb_master$date)]
cmb_master[,c("w_1") := NULL]
####### Let's also include some macroeconomic variables, to be discussed #####

ism<-ism[-c(194:197)]
ism<-ism[-1]
nfp_rate<-nfp_rate[-1]

nfp_rate$date <- as.Date(nfp_rate$date, "%m/%d/%Y")
ism$date <- as.Date(ism$date, "%m/%d/%Y")
nfp_rate[,y_1:=year(date)]
nfp_rate[,m_1:=month(date)]
ism[,y_1:=year(date)]
ism[,m_1:=month(date)]
macro <- merge(nfp_rate, ism
                 , by.x=c("m_1", "y_1")
                 , by.y=c("m_1", "y_1"), all.x=TRUE)
macro <- macro[order(macro$date.x)]

macro[ ,c("date.y") := NULL]
setnames(macro, "date.x", "date")
setcolorder(macro, c("date", "y_1", "m_1","payems","ism_pmi"))
macro[, mix_d:=paste0(m_1,y_1)]

macro_t <- ts(macro, start=c(2003,1), frequency = 12) ## Monthly
autoplot(macro_t[,c("payems","ism_pmi")], facets = TRUE) + ylab("") + xlab("Years") +
  ggtitle("Non-Farm Payrolls % Change & ISM Manufacturing Index")

cmb_master1 <- merge(cmb_master,macro)
cmb_master1[,c("y_1","m_1"):= NULL]
cmb_master1 <- cmb_master1[order(date),]
cmb_master2 <- cmb_master1[-c(1:72)]
## @knitr 1.3_visuals

# First difference of MBS and Treasuries owned by the Federal Reserve

cmb_master2 <- cmb_master2[order(-date),] #reverse order by date

#custom function
vec=c(1,2,3,4,5)
i=2
ma=function(i,vec){(vec[i-1]-vec[i])/(vec[i])}

#using for loops
for (i in seq(2,length(vec))){
  print(ma(i,vec)) 
}
#using sapply - better option
result=sapply(2:length(cmb_master2$mbs1),function(x)ma(x,cmb_master2$mbs1))

cmb_master2[,mbs_diff:=sapply(2:length(cmb_master2$mbs1),function(x)ma(x,cmb_master2$mbs1))]

cmb_master2[,treas_diff:=sapply(2:length(cmb_master2$treas1),function(x)ma(x,cmb_master2$treas1))]

za=function(i,vec){(vec[i-1]-vec[i])}
cmb_master2[,real_hpi_diff:=sapply(2:length(cmb_master2$real_hpi),function(x)za(x,cmb_master2$real_hpi))]
cmb_master2[,ism_pmi_diff:=sapply(2:length(cmb_master2$ism_pmi),function(x)za(x,cmb_master2$ism_pmi))]


cmb_master2 <- cmb_master2[order(date),]
cmb_master2 <- cmb_master2[-c(1:6)]



### dropping NA values, which was when the Federal Reserve started purchasing MBS products

ggplot(melt(cmb_master2[,-c("date","mbs1","treas1","mix_d","real_hpi","ism_pmi")]),aes(x=value, y = ..density..)) + 
  geom_histogram(bins = 50, col="black", fill="grey") +
  stat_density(geom = "line", colour ="red", size = 1) + 
  facet_wrap(~variable, scales = "free")

## @knitr 1.3_scatter

s1 <- ggplot(cmb_master2, aes(x = yr_15_fixed, y = real_hpi_diff)) +
  geom_point(size=1, colour='blue') +
  geom_smooth(method='lm', colour='red')+
  theme(axis.title.y = element_blank())

s2 <- ggplot(cmb_master2, aes(x = yr_30_fixed, y = real_hpi_diff)) +
  geom_point(size=1, colour='blue') +
  geom_smooth(method='lm', colour='red')+
  theme(axis.title.y = element_blank())

s3 <- ggplot(cmb_master2, aes(x = treas_10, y = real_hpi_diff)) +
  geom_point(size=1, colour='blue') +
  geom_smooth(method='lm', colour='red')+
  theme(axis.title.y = element_blank())

s4 <- ggplot(cmb_master2, aes(x = treas_diff, y = real_hpi_diff)) +
  geom_point(size=1, colour='blue') +
  geom_smooth(method='lm', colour='red')+
  theme(axis.title.y = element_blank())

s5 <- ggplot(cmb_master2, aes(x = mbs_diff, y = real_hpi_diff)) +
  geom_point(size=1, colour='blue') +
  geom_smooth(method='lm', colour='red')+
  theme(axis.title.y = element_blank())

s6 <- ggplot(cmb_master2, aes(x = payems, y = real_hpi_diff)) +
  geom_point(size=1, colour='blue') +
  geom_smooth(method='lm', colour='red')+
  theme(axis.title.y = element_blank())

s7 <- ggplot(cmb_master2, aes(x = ism_pmi, y = real_hpi_diff)) +
  geom_point(size=1, colour='blue') +
  geom_smooth(method='lm', colour='red')+
  theme(axis.title.y = element_blank())

grid.arrange(s1,
             s2,
             s3,
             s4,
             s5,
             s6,
             s7,
             ncol = 3, nrow = 3,
             left = textGrob("Real House Price Index", rot = 90, vjust = 1), top=textGrob("Graph 6: Scatter Plot by Real HPI"))

## @knitr 1.3_corr

cplot <- cor(cmb_master2[,-c("date","mbs1","treas1","mix_d","real_hpi","ism_pmi")] ,use="pairwise.complete.obs", method = "pearson")
par(mfrow=c(1,1))
corrplot(cplot, method = "square",
         type = "lower", order = "original", number.cex = .6,
         addCoef.col = "black",
         tl.col = "black", tl.srt = 0.1, tl.cex = .8, diag = TRUE)

## @knitr 2.1_modeling

x5_test <- cmb_master2
x5_test$date <- NULL
#x5_test$mix_d <- NULL

x5_test$mbs1<- NULL
x5_test$treas1 <- NULL

# Setting as time-series model
x5_test_ts<-ts(x5_test, start=c(2009, 7), end =c(2018,12), frequency = 12)
arima1 <- auto.arima(x5_test_ts[,"real_hpi_diff"])
autoplot(x5_test_ts[,"real_hpi_diff"])

model1 <- tslm(real_hpi_diff ~ mbs_diff + ism_pmi_diff + treas_10 + payems, data = x5_test_ts)
summary(model1)
checkresiduals(model1, main="Model 1: Linear Reg")

model2 <- tslm(real_hpi_diff ~ mbs_diff + ism_pmi_diff + payems + yr_30_fixed, data = x5_test_ts)
summary(model2)
checkresiduals(model2, main="Model 2: Linear Reg")

model3 <- tslm(real_hpi_diff ~ treas_diff + ism_pmi_diff + yr_15_fixed +payems, data = x5_test_ts)
summary(model3)
checkresiduals(model3, main="Model 3: Linear Reg")

fc1 <- forecast(arima1, h=6) #forecasting 6 months into the future#
autoplot(fc1, scenario=model2) +
ggtitle("Forecast ARIMA (1,0,0)(0,0,1)[6 months]") +
  xlab("Year") +
  ylab("House Price Index Diff")
lambda <- BoxCox.lambda(x5_test_ts[,"real_hpi_diff"])
autoplot(BoxCox(x5_test_ts[,"real_hpi_diff"], lambda)) 
lambda
fit1 <- nnetar(x5_test_ts[,"real_hpi_diff"],lambda=.44)
fc2 <- forecast(fit1, h=6)
autoplot(fc2, scenario=model2) + #forecasting 6 months into the future#
  ggtitle("Forecasting Multilayer Feed-Forward Network") +
  xlab("Year") +
  ylab("House Price index")

## @knitr 2.2_split_data

train_clean <- window(x5_test_ts, start=c(2009,7), end=2017)
test_clean <- window(x5_test_ts, start=2017)
model_2_4 <- tslm(real_hpi_diff ~  mbs_diff + ism_pmi_diff + payems + yr_30_fixed, data = train_clean)
summary(model_2_4)

hpi_fit1 <- meanf(train_clean[,"real_hpi_diff"], h=24)
hpi_fit2 <- rwf(train_clean[,"real_hpi_diff"], h=24)
hpi_fit3 <- snaive(train_clean[,"real_hpi_diff"], h=24)
hpi_fit4 <- rwf(train_clean[,"real_hpi_diff"], drift=TRUE, h=24)
lambda2 <- BoxCox.lambda(train_clean[,"real_hpi_diff"])
fit_net <- nnetar(train_clean[,"real_hpi_diff"],lambda=.44)
fc_net <- forecast(fit_net, h=24)
combo <- (hpi_fit3[["mean"]] + fc_net[["mean"]]/2)

autoplot(window(x5_test_ts[,"real_hpi_diff"], start=c(2009,7)))+
  autolayer(hpi_fit1, series="Mean", PI=FALSE)+
  autolayer(hpi_fit2, series="Naive", PI=FALSE)+
  autolayer(hpi_fit3, series="Seasonal Naive", PI=FALSE)+
  autolayer(hpi_fit4, series="Drift", PI=FALSE)+
  autolayer(fc_net, series="NNET", PI=FALSE)+
  autolayer(combo, series="Combo")+
  ggtitle("Forecasting models") +
  xlab("Year") +
  ylab("House Price index")

kable(accuracy(hpi_fit1,test_clean[,"real_hpi_diff"]))
kable(accuracy(hpi_fit2,test_clean[,"real_hpi_diff"]))
kable(accuracy(hpi_fit3,test_clean[,"real_hpi_diff"]))
kable(accuracy(hpi_fit4,test_clean[,"real_hpi_diff"]))
kable(accuracy(fc_net,test_clean[,"real_hpi_diff"]))
kable(accuracy(combo,test_clean[,"real_hpi_diff"]))

fc_net2 <- forecast(hpi_fit3,PI=TRUE, scenario=model_2_4)
autoplot(fc_net2)
