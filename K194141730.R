# Import library
library(readxl)
library(tidyverse)
library(SciViews)
library(dplyr)
library(tidyr)
library(zoo)
# Data collection and input
df <- read_excel('D:/University/N3-II/UDCT2/K194141730.xlsx')
df <- transmute(df,Quarter=Quarter,
                ROA=ROA,
                GRO=(`Total assets`-lag(`Total assets`))/lag(`Total assets`),
                LIQ=`Current assets`/`Short -term liabilities`,
                Size= ln(`Total assets`),
                LEV=Liabilities/`Total assets`)
df <- df[complete.cases(df), ]
df <- df %>% 
  mutate(Quarter = as.yearqtr(format(Quarter), "%Y Q%q"))
# Create COVID-19 Column
df$`COVID-19` <- NA
for (i in 1:nrow(df)){
  if (i < which((df$Quarter) == "2020 Q1")){
    df$`COVID-19`[i] = "BEFORE" 
  }
  else {
    df$`COVID-19`[i] = "AFTER"
  }
}
# Descriptive stats BEFORE and AFTER Covid
df %>%
  filter(`COVID-19`=='BEFORE') %>% 
  select(ROA) %>%
  summarise(Median = median(ROA),
            Mean = mean(ROA),
            Min = min(ROA),
            Max = max(ROA),
            Standard_Deviation = sd(ROA)) %>%
  print()

df %>%
  filter(`COVID-19`=='AFTER') %>% 
  select(ROA) %>%
  summarise(Median = median(ROA),
            Mean = mean(ROA),
            Min = min(ROA),
            Max = max(ROA),
            Standard_Deviation = sd(ROA)) %>%
  print()

df %>%
  filter(`COVID-19`=='BEFORE') %>% 
  select(GRO) %>%
  summarise(Median = median(GRO),
            Mean = mean(GRO),
            Min = min(GRO),
            Max = max(GRO),
            Standard_Deviation = sd(GRO)) %>%
  print()

df %>%
  filter(`COVID-19`=='AFTER') %>% 
  select(GRO) %>%
  summarise(Median = median(GRO),
            Mean = mean(GRO),
            Min = min(GRO),
            Max = max(GRO),
            Standard_Deviation = sd(GRO)) %>%
  print()

df %>%
  filter(`COVID-19`=='BEFORE') %>% 
  select(LIQ) %>%
  summarise(Median = median(LIQ),
            Mean = mean(LIQ),
            Min = min(LIQ),
            Max = max(LIQ),
            Standard_Deviation = sd(LIQ)) %>%
  print()

df %>%
  filter(`COVID-19`=='AFTER') %>% 
  select(LIQ) %>%
  summarise(Median = median(LIQ),
            Mean = mean(LIQ),
            Min = min(LIQ),
            Max = max(LIQ),
            Standard_Deviation = sd(LIQ)) %>%
  print()

df %>%
  filter(`COVID-19`=='BEFORE') %>% 
  select(Size) %>%
  summarise(Median = median(Size),
            Mean = mean(Size),
            Min = min(Size),
            Max = max(Size),
            Standard_Deviation = sd(Size)) %>%
  print()

df %>%
  filter(`COVID-19`=='AFTER') %>% 
  select(Size) %>%
  summarise(Median = median(Size),
            Mean = mean(Size),
            Min = min(Size),
            Max = max(Size),
            Standard_Deviation = sd(Size)) %>%
  print()
# Distribution of Leverage
df %>%
  ggplot(aes(LEV,`COVID-19`,color= `COVID-19`))+
  geom_boxplot()+
  theme_minimal()+
  labs(title = 'Boxplot of Leverage',x= 'COVID-19')

df %>%
  ggplot(aes(LEV))+
  geom_histogram()+
  theme_minimal()+
  labs(title = 'Histogram of Leverage')

# Model 1
input <- df[c('LEV','ROA','Size','LIQ','GRO')]
model <- lm(LEV~ROA+Size+LIQ+GRO,data=input)
summary(model)


# Model 2
df$`COVID-19`[df$`COVID-19`=="BEFORE"] <- "0"
df$`COVID-19`[df$`COVID-19`=="AFTER"] <- "1"
df$`COVID-19`<- as.numeric(df$`COVID-19`)
df$ROAC <- df$ROA*df$`COVID-19`
df$GROC <- df$GRO*df$`COVID-19`
df$LIQC <- df$LIQ*df$`COVID-19`
df$SizeC <- df$Size*df$`COVID-19`
input <- df[c('LEV','ROA','Size','LIQ','GRO','ROAC','GROC','LIQC','SizeC')]
model <- lm(LEV~ROA+Size+LIQ+GRO+ROAC+GROC+LIQC+SizeC,data=input)
summary(model)

# Predict model 1
input <- df[c('LEV','ROA','Size','LIQ','GRO')]
model <- lm(LEV~ROA+Size+LIQ+GRO,data=input)
pred <- predict(model, data = input, type='response')

# Create df for predicted data
predicted <- data.frame(matrix(ncol = 3, nrow = 57)) 
colnames(predicted) <- c("Quarter", 'Actual', 'Predicted')
predicted$Quarter <- df$Quarter
predicted$Actual <- df$LEV
predicted$Predicted <- pred

# Convert to numeric
predicted$Actual <- as.numeric(predicted$Actual)
predicted$Predicted <- as.numeric(predicted$Predicted)
predicted

# RMSE, MSE 
library("Metrics")
rmse(predicted$Actual , predicted$Predicted)
mse(predicted$Actual , predicted$Predicted)

library(forecast) #forecast, accuracy
library(tseries) #adf.test
library(lmtest) #coeftest
library(stats) #Box.test
# Leverage ADF test
lev <- diff(df$LEV,differences = 2)
acf(lev,main= "ACF for Leverage")
pacf(lev,main="PACF for Leverage")
adf.test(lev)# Leverage is not stationary
# Use auto.arima function to determine best P, D, Q
auto=auto.arima(lev,seasonal=F,trace = T,max.order=4,
                ic='aic')
coeftest(auto.arima(lev,seasonal=F))
acf(auto$residuals)
pacf(auto$residuals)
Box.test(auto$residuals,lag=20,type='Ljung-Box')
# Prediction
forecast(auto,h=4) 
plot(forecast(auto,h=4))

