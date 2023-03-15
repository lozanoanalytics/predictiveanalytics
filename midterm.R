#### Ivan Lozano          ####
#### Midterm Project Code ###
#### Prof. Bradley        ###
#### March 20, 2019       ###


##======================================================================##
## Capturing the Effects of the Federal Reserve MBS Purchase Program
##======================================================================##

rm(list = ls()) # Clear the enviroment
gc()            # Clear memory
cat("\f")       # Clear the console

# Prepare needed libraries
tmp.library.list <- c("data.table",
                      "stringr",
                      "stargazer",
                      "fpp2",
                      "gridExtra",
                      "quantmod",
                      "psych",
                      "Hmisc")
for (i in 1:length(tmp.library.list)) {
  if (!tmp.library.list[i] %in% rownames(installed.packages())) {
    install.packages(tmp.library.list[i])
  }
  library(tmp.library.list[i], character.only = TRUE)
}
rm(tmp.library.list)

setwd("C:/Users/ANONYMOUS/Documents/BC/BC Spring 2019/Predictive_Analytics/Midterm/RawData/")

mbs_yield <- read.csv("fredgraph.csv", header = T, na.strings=c(""," ","NA"))
setnames(mbs_yield, old=c("DATE", "WSHOMCB", "TREAST"),
                    new=c("date", "mbs_bal","treas_bal"))

mbs_yield$date <- as.Date(mbs_yield$date, "%Y-%m-%d")
mbs_yield$date <- NULL

mbs_treas<-ts(mbs_yield, start=c(2002,12,18), frequency = 52.18)

mt_dta <- autoplot(mbs_treas) +
  xlab("Years") + ylab("in Millions of dollars") +
  ggtitle("Securities Held Outright by Federal Reserve")

mt_dta + labs(colour = "Security") +
  scale_color_manual(labels = c("MBS", "Treasuries"), values = c(2, 4)) + geom_line() +
  annotate("rect", fill = "grey", alpha = 0.6, 
           xmin = 2007 + 12/12, xmax = 2009 + 06/12,
           ymin = -Inf, ymax = Inf)

mort_rate <- read.csv("cleandt2.csv", header = T, na.strings=c(""," ","NA"))

mort_rate$week <- as.Date(mort_rate$week, "%m/%d/%Y")
summary(mort_rate)

ggplot(mort_rate, aes(mort_rate$yr30_fixed, y= ..density..)) +
                  geom_histogram(bins = 50) +
                  stat_density(geom = "line", colour ="red", size =1) +
                  ggtitle("30-Year Fixed Rate Mortgage Histogram 1971-2018") +
                  xlab("Mortgage Rates") +
                  ylab("Density")

mort_rate$week <- NULL

# START DATE: 1971-04-02 END DATE: 2018-12-27
mort_rate2 <- ts(mort_rate, start=c(1971,4,2), frequency = 52.18)
mort_rates3 <- autoplot(mort_rate2) +
  xlab("Years (Weekly Observations)") + ylab("Mortgage Interest Rate") +
  ggtitle("Mortgage Rates By Mauturities")

mort_rates3 + labs(colour = "Maturities") +
  scale_color_manual(labels = c("30YR Fixed", "15YR Fixed", "1 YR Treasury", "1 YR Margin")
                     , values = c(1, 2, 3, 4))+ geom_line() +
                      annotate("rect", fill = "grey", alpha = 0.6, 
                               xmin = 2007 + 12/12, xmax = 2009 + 06/12,
                               ymin = -Inf, ymax = Inf)

###==========================================================###
#   STEP 2: Let's begin by cleaning up our data time data into data 
###==========================================================###

### importing clean set of my data and repopulating
clean_mbs <- read.csv("fredgraph.csv")
clean_mbs <- as.data.table(clean_mbs)
### replacing zero's with missing data
clean_mbs$WSHOMCB[clean_mbs$WSHOMCB == "0"] <- NA
clean_mbs$DATE <- as.Date(clean_mbs$DATE, "%Y-%m-%d")
setnames(clean_mbs, old=c("DATE", "WSHOMCB", "TREAST"),
         new=c("date", "mbs_bal","treas_bal"))


cut_mbs2 <- ts(clean_mbs, start=c(2002,12,18), frequency = 52.18)
win_mbs <- window(cut_mbs2[, 2:3], start=c(2006,10))
autoplot(win_mbs)
ggAcf(win_mbs) + ggtitle("Autocorrelation for Treasuries: Fed Owned")

clean_mbs$DATE <- NULL


clean_mort <- read.csv("cleandt2.csv")
clean_mort$week <- as.Date(clean_mort$week, "%m/%d/%Y")

clean_mort2 <- ts(clean_mort, start=c(1971,4,2), frequency=52.18)

win_clean <- window(clean_mort2[, 2:3], start=c(2006,39))
ggAcf(win_clean) + ggtitle("Autocorrelation for mortgage rates")

autoplot(win_clean[,"yr30_fixed"]) +
  autolayer(meanf(win_clean[,"yr30_fixed"], h=52),
            series="Mean", PI=FALSE) +
  autolayer(naive(win_clean[,"yr30_fixed"], h=52),
            series="Naive", PI=FALSE) +
  autolayer(snaive(win_clean[,"yr30_fixed"], h=52),
            series="Seasonal Naive", PI=FALSE) +
  autolayer(rwf(win_clean[,"yr30_fixed"], drift = TRUE, h=52),
            series="Drift", PI=FALSE) +
  ggtitle("Forecasts for 30-year Mortgage Rates") +
  xlab("Year") + ylab("Rate") +
  guides(colour=guide_legend(title="forecast"))

autoplot(win_clean[,"yr15_fixed"]) +
  autolayer(meanf(win_clean[,"yr15_fixed"], h=52),
            series="Mean", PI=FALSE) +
  autolayer(naive(win_clean[,"yr15_fixed"], h=52),
            series="Naive", PI=FALSE) +
  autolayer(snaive(win_clean[,"yr15_fixed"], h=52),
            series="Seasonal Naive", PI=FALSE) +
  autolayer(rwf(win_clean[,"yr15_fixed"], drift = TRUE, h=52),
            series="Drift", PI=FALSE) +
  ggtitle("Forecasts for 15-year Mortgage Rates") +
  xlab("Year") + ylab("Rate") +
  guides(colour=guide_legend(title="forecast"))


autoplot(win_mbs[,"treas_bal"]) +
  autolayer(meanf(win_mbs[,"treas_bal"], h=52),
            series="Mean", PI=FALSE) +
  autolayer(naive(win_mbs[,"treas_bal"], h=52),
            series="Naive", PI=FALSE) +
  autolayer(snaive(win_mbs[,"treas_bal"], h=52),
            series="Seasonal Naive", PI=FALSE) +
  autolayer(rwf(win_mbs[,"mbs_bal"], drift = TRUE, h=52),
            series="Drift", PI=FALSE) +
  ggtitle("Forecasts of Treasuries") +
  xlab("Year") + ylab("Value") +
  guides(colour=guide_legend(title="forecast"))

win_mbs2 <- window(cut_mbs2[, 2:3], start=c(2008,15))

autoplot(win_mbs2[,"mbs_bal"]) +
  autolayer(meanf(win_mbs2[,"mbs_bal"], h=52),
            series="Mean", PI=FALSE) +
  autolayer(naive(win_mbs2[,"mbs_bal"], h=52),
            series="Naive", PI=FALSE) +
  autolayer(snaive(win_mbs2[,"mbs_bal"], h=52),
            series="Seasonal Naive", PI=FALSE) +
  autolayer(rwf(win_mbs2[,"mbs_bal"], drift = TRUE, h=52),
            series="Drift", PI=FALSE) +
  ggtitle("Forecasts of Treasuries") +
  xlab("Year") + ylab("Value") +
  guides(colour=guide_legend(title="forecast"))


#####=================== Splitting Data ========================####
class(clean_mbs)
clean_mbs
clean_mort <- data.table(clean_mort)

mort_mod_30_year <- clean_mort[,.(week,yr30_fixed)]
mort_mod_30_year<- mort_mod_30_year[week>=as.Date("2007-12-06") & week <= as.Date("2018-12-27"),]
mort_mod_30_year[,dummy:=.I]

test_prop <- .20
test_len <- floor(test_prop * nrow(mort_mod_30_year))
test_mort_30 <- mort_mod_30_year[dummy>(length(dummy)-test_len),]
test_mort_30

train_mort_30<- mort_mod_30_year[!(mort_mod_30_year$dummy %in% (test_mort_30$dummy)),]


test_mort_30[,dummy:=NULL]
train_mort_30[,dummy:=NULL]


year_train <- year(min(train_mort_30$week))
month_train <- month(min(train_mort_30$week))
day_train <- mday(min(train_mort_30$week))

train_mort_30_ts <- ts(train_mort_30$yr30_fixed, start=c(year_train,month_train,day_train),frequency=52.18)


year_test <- year(min(test_mort_30$week))
month_test <- month(min(test_mort_30$week))
day_test <- mday(min(test_mort_30$week))

test_mort_30_ts <- ts(test_mort_30$yr30_fixed, start=c(year_test,month_test,day_test),frequency=52.18)







plot(ts(mort_mod_30_year))


str(mort_mod_30_year)
win_clean
str(win_clean)


auto.arima(win_mbs2[,"mbs_bal"])

autoplot(newts2)

autoplot(newts) + labs(colour = "Maturity") +
  scale_color_manual(labels = "1 YR Margin"
                     , values = 1)

qplot(mbs_bal, treas_bal, data=as.data.frame(mbs_treas))

data1 <- ts(clean_mbs, start=c(2002,12,18), frequency = 52.18)

