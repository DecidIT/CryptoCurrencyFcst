#--------------------------------  CryptoEDA.R  --------------------------------------------------------
# This program is an attempt to write an algorithm that is able to predict crypto currency.
#     - In program CryptoEDA.R, a first part analyses the data set issued from Kabble at 
#          https://www.kaggle.com/sudalairajkumar/cryptocurrencypricehistory
#     - In program cryptoFcst,  I used tree/forest/,,, algorithms to predict the curracy volume? and Marketcap? or price?
#     - Both are neatly merged in CryptoFcst.Rmd that will grnerate the .pdf report
#------
# 20.09.2021  | Start  | Exploration Data Analysis
# 03.10.2021  | Step 2 | Forecast 
# 05.10.2021  | Step 3 | Start report
#--------------------------------------------------------------------------------------------------------                        
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(ggrepel)) install.packages("ggrepel", repos = "http://cran.us.r-project.org")
if(!require(ggpubr)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(pls)) install.packages("pls", repos = "http://cran.us.r-project.org")


#--- Exploration Data Analysis
library(tidyverse)    # Organization and visualization data
library(caret)        # Machine learning procedure
library(data.table)   # For table manipulation
library(kableExtra)   # For table presentation
library(corrplot)     # For correlation visualization
library(ggrepel)      # Advanced geometric objects managenment
library(ggpubr)       # Complent to ggplot2
library(lubridate)    # For date management
#--- Forecast
library(rpart)        # Recursive Partitioning and Regression Trees
library(randomForest) # Random Forests for Classification and Regression
library(Rborist)      # Parallel Implementation of the Random Forest Algorithm
library(pls)          # Partial Least Squares and Principal Component Regressio


#--------------------------------------------------------------------------
#----------------------- EXPLORATION DATA ANALYSIS ------------------------
#--------------------------------------------------------------------------
#--- Read files
#-----------------------
options(digits = 4)
cryptos <- read_csv("cryptos.csv")
# files   <- lapply(csv_files, function(x) fread(file=paste0(dir_data,x)))
#cryptos <- do.call(rbind, lapply(csv_files, function(x) fread(file=paste0(dir_data,x))))
cryptos <- cryptos %>% mutate(Symbol= as.factor(Symbol),
                              Date= as.Date(Date))
#-----------------------
#--- Correlation graph
#-----------------------
# correlation Num columns
df <- cryptos[,-1] %>% select_if(is.numeric)
corrplot.mixed(cor(df),
               lower = "number", 
               upper = "ellipse",
               tl.col = "black")
# If bivariate only one  in the 4 (High, Low, Open, Close) can be used for 3 othere
#----------------------
#--- Volume vs Time
#-----------------------
cryptos <- cryptos %>%
  mutate(Week=    week(Date),
         Month=   month(Date),
         Quarter= quarter(Date),
         Year=    year(Date))  %>%
  mutate(Nmonth = recode(Month, "1" = "JAN", "2" = "FEB", "3" = "MAR", "4" = "APR", "5" = "MAY", "6" = "JUN", 
                         "7" = "JUL", "8" = "AUG", "9" = "SEP", "10" = "OCT", "11" = "NOV", "12" = "DEC"))
#------------------------------
#--- Volume per day of the week
#------------------------------
cryptos %>% 
  mutate(Day = factor(weekdays(Date))) %>%
  group_by(Day) %>% 
  summarize(n=n(), mv=mean(Volume)) %>%
  mutate(NoDay= recode(Day, "Monday"= 1, "Tuesday"= 2, "Wednesday"= 3, "Thursday"= 4, 
                       "Friday"= 5, "Saturday"= 6, "Sunday"= 7)) %>%
  mutate(Day= reorder(Day, NoDay)) %>%
  ggplot(aes(Day, mv)) +
  geom_bar(stat = "identity", fill= "steelblue")
#---------------------------------
#--- Volume per month of the year
#---------------------------------
span <- 60/ as.numeric(diff(range(cryptos$Date)))

cryptos %>% 
  mutate(Month= month(Date)) %>%
  group_by(Month) %>% 
  summarize(n= n(), mv= mean(Volume)) %>%
  mutate(Nmonth = recode(Month, "1" = "JAN", "2" = "FEB", "3" = "MAR", "4" = "APR", "5" = "MAY", "6" = "JUN", 
                         "7" = "JUL", "8" = "AUG", "9" = "SEP", "10" = "OCT", "11" = "NOV", "12" = "DEC"))  %>%
  mutate(Nmonth= reorder(Nmonth, Month)) %>%
  ggplot(aes(Nmonth, mv)) +
  geom_bar(stat = "identity", fill= "steelblue")

#---------------------------------
#--- Volume per year
#---------------------------------
fit <- cryptos  %>%  
  mutate(x = as.numeric(Date)) %>% loess(Volume ~ x, data = ., 
                                         span = span, degree = 2)
cryptos %>%  
  mutate(smooth = predict(fit, as.numeric(Date)), day = yday(Date), 
         year = as.character(year(Date))) %>%
  ggplot(aes(day, smooth, col = year)) +
  geom_line(lwd = 0.8)

#-------------------------------------------------
#--- Importance of currency
#-------------------------------------------------
#--- Importance of market capitalisation
cryptos %>% 
  group_by(Name) %>%
  summarize(med_mc = median(Marketcap)) %>%
  ungroup() %>%
  arrange(desc(med_mc)) %>% 
  mutate(Name= reorder(Name, med_mc))  %>%
  ggplot(aes(Name, med_mc)) + 
  geom_bar(stat = "identity", fill= "steelblue") +
  coord_flip()

#--- Regularisation relative to period
dat <- cryptos %>%
  group_by(Name) %>%
  mutate(elapse= as.numeric(diff(range(Date)))) %>%
  summarise(elapse= max(elapse)) 

mean_elapse <- mean(dat$elapse)

#-------------------------------------------------
#--- Time series: Importance of currencies in market
#-------------------------------------------------
#- Market capitalisation vs time
cryptos %>%
  mutate(elapse= as.numeric(diff(range(Date)))/mean_elapse,    # Regularisation
         Symbol= Symbol) %>% 
  group_by(Name) %>%
  summarise(med_mc= median(Marketcap) * elapse) %>%
  arrange(desc(med_mc)) %>% 
  mutate(Name= reorder(Name, med_mc)) %>%
  ggplot(aes(Name, med_mc, fill= Name)) + 
  labs(title= "Market capitalisation Share",
       x = "Currency",
       y = "Market Capitalisation") +
  theme(plot.title  = element_text(color="steelblue", size=14, face= "bold", hjust = 0.5),
        axis.title.x = element_text(color="steelblue", size=11, face= "bold"),
        axis.title.y = element_text(color="steelblue", size=11, face= "bold")) +
  geom_bar(stat = "identity") + #fill= "steelblue") +
  coord_flip() +
  theme(legend.position = "none")


#-------------------------------------------------
#--- "Attractievness" & Outliers
#-------------------------------------------------

# Attractiveness vs Date/Time
cryptos %>% 
  filter(Marketcap != 0 & Volume != 0) %>%
  ggplot(aes(Date, Volume/Marketcap,  color= Name)) +
  geom_line() +
  theme(legend.position = "none")

# Log-transforemd attractivenes
LogAttract <- cryptos %>% 
  # Subset data: Bitcoin history for 2014 and 2015
  filter(Symbol== "BTC" & Year %in% c("2014":"2017") ) %>%
  mutate(Value= Volume/Marketcap) %>% 
  .$Value %>%
  log()         # Log transformation
# Histogram
hist(LogAttract)

# Vector
attractiveness_log_scaled <- cryptos %>% 
  # Subset data: Bitcoin history for 2014 and 2015
  filter(Symbol== "BTC" & Year %in% c("2014":"2017") ) %>%
  mutate(Value= Volume/Marketcap) %>% 
  .$Value %>%
  log()   # Log transformation

# Dataframe
crypto_attract <- cryptos %>% 
  filter(Symbol== "BTC"  & Year %in% c("2014":"2017") ) %>%
  mutate(LogAttract= log(Volume/Marketcap))
#---------------------------------------
#----- Outliers smooth out
#---------------------------------------
#- Data subset
crypto_attract <- cryptos %>%
  filter(Symbol== "BTC" &           # Bitcoin 
           Year  %in% c(2014:2017) &  # 2014:2017
           Marketcap !=0  & Volume != 0) %>% 
  mutate(Day= yday(Date),
         LogAttract= log(Volume/Marketcap)) # log(Attractiveness)

#- Step 1 - Populate a naive forecast as an average of the historical attractiveness
#           by day of the year
crypto_avgattract <-  crypto_attract %>%
  group_by(Day) %>%
  summarise(AvgLogAttract= mean(LogAttract)) %>%
  mutate(Year= "Avg")

#- Step 2 - Compute the error, then the mean and standard deviation of the error
error <- crypto_attract %>% 
  right_join(crypto_avgattract, by= "Day") %>%
  mutate(error=  LogAttract - AvgLogAttract) %>%
  .$error

m1 <- mean(error)     # Average
s1 <- sd(error)       # Standard deviation

#------ QQ PLOT -----#
prob <- 0.97
error %>% 
  # Graph
  ggqqplot(conf.int.level = prob, color= "steelblue") +
  labs(title= "QQ-Plot",
       subtitle = "Price log transformed") +
  theme(plot.title    = element_text(color="steelblue", size=14,  face= "bold", hjust = 0.5),
        plot.subtitle = element_text(color="steelblue", size=14,  face= "italic", hjust = 0.5))
#------ QQ PLOT -----#

# Step 3  - Set upper limit that implies the error has 1.5% chance to be higher
prob <- 0.97
quant1 <- qnorm(prob, m1, s1)                              # Quantile
upper_limit <- crypto_avgattract$AvgLogAttract + quant1    # Upper limit = Average + quantile(0.97)


#- Step 4 -  Indentify outliers i.e for which error is greater than the upper limit
ind_outliers <- which(error > quant1)

#- Step 5 - Recompute error distribution parameters excluding outliers
m2 <- mean(error[-ind_outliers])      # Average excluding outliers
s2 <- sd(error[-ind_outliers])        # Standard deviation excluding outliers

#- Step 6 - Update the upper limit with the new average and standard deviation
quant2 <- qnorm(prob, m2, s2)                          # Quantile
upper_limit <- crypto_avgattract$AvgLogAttract + quant2    # Upper limit = Average + quantile(0.97)

#- Step 7 - Update the outliers based on the new upper limit
ind_outliers <- which(error > quant2)
crypto_updattract <- crypto_attract 
crypto_updattract$LogAttract[ind_outliers] <- upper_limit


#---------------------------------------------------------
#----------------------- FORECAST ------------------------
#---------------------------------------------------------
#--- Error Functions 
#--------------------------------
#--- RMSE
RMSE <- function(true, predicted){
  sqrt(mean((true - predicted)^2))
}
#--- MAE
MAE <- function(true, predicted){
  sum(abs(true-predicted))/length(true)
}
#--- MASE
MASE <- function(mae_model, mae_naive){
  mae_model/mae_naive
}
#----------------------------------
#--- Prepare data before forecast
#----------------------------------
Intervals <- list(period_1= 2013:2017,
                  period_2= 2018:2021)


AttractSmooth <- sapply(Intervals, function(period) { 
  #-------------------------------------------------------------------------------------
  prob <- 0.98
  
  crypto_attract <- cryptos %>%
    filter(Marketcap != 0 &  
             Volume!= 0 &  
             Year  %in% period ) %>% #&
    #Symbol != "BTC") %>%  #???
    mutate(Day= yday(Date),
           LogAttract= log(Volume/Marketcap)) # log(Attractiveness)
  
  crypto_avgattract <-  crypto_attract %>%
    filter(Symbol!= "BTC") %>%  #???
    group_by(Symbol, Day) %>%
    summarise(AvgLogAttract= mean(LogAttract)) %>%
    mutate(Year= "Avg")
  
  #- Step 2 - Compute the error, then the mean and standard deviation of the error
  error <- crypto_attract %>% 
    right_join(crypto_avgattract, by= c("Symbol","Day")) %>%
    mutate(error=  LogAttract - AvgLogAttract) %>%
    .$error
  
  m1 <- mean(error)     # Average
  s1 <- sd(error)       # Standard deviation
  
  # Step 3  - Set upper limit that implies the error has 1.5% chance to be higher
  
  quant1 <- qnorm(prob, m1, s1)                              # Quantile
  upper_limit <- crypto_avgattract$AvgLogAttract + quant1    # upper limit
  
  #- Step 4 -  Indentify outliers i.e for which error is greater than the upper limit
  ind_outliers <- which(error > quant1)
  
  #- Step 5 - Recompute error distribution parameters excluding outliers
  m2 <- mean(error[-ind_outliers])      # Average excluding outliers
  s2 <- sd(error[-ind_outliers])        # Standard deviation excluding outliers
  
  #- Step 6 - Update the upper limit with the new average and standard deviation
  quant2 <- qnorm(prob, m2, s2)                              # Quantile
  upper_limit <- crypto_avgattract$AvgLogAttract + quant2    # Upper limit
  
  #- Step 7 - Update the outliers based on the new upper limit
  ind_outliers <- which(error > quant2)
  crypto_updattract <- crypto_attract 
  crypto_updattract$LogAttract[ind_outliers] <- upper_limit
  
  exp(crypto_updattract$LogAttract)
  
} # function(period)

) # sapply 

#-------------------------------------------------------------------------------------
#--- Attractiveness with outliers
cryptos %>% 
  filter(Marketcap!= 0 & 
           Volume!= 0) %>%
  ggplot(aes(Date, Volume/Marketcap,  color= Name)) +
  labs(title= "Attractiveness vs Time",
       x   = "Date",
       y   = "Attractiveness") +
  theme(plot.title   = element_text(color="steelblue", size=14, face= "bold", hjust = 0.5),
        axis.title.x = element_text(color="steelblue", size=11, face= "bold"),
        axis.title.y = element_text(color="steelblue", size=11, face= "bold")) +
  geom_line() +
  ylim(0, 20) +
  theme(legend.position = "none")


#--- Attractiveness outliers smoothed out
cryptos %>% 
  filter(Marketcap!= 0 & Volume!= 0) %>% 
  bind_cols(UpdAttract= unlist(AttractSmooth)) %>% 
  ggplot(aes(Date, UpdAttract,  color= Name)) +
  labs(title= "Attractiveness vs Time",
       x   = "Date",
       y   = "Attractiveness") +
  theme(plot.title   = element_text(color="steelblue", size=14,  face= "bold", hjust = 0.5),
        axis.title.x = element_text(color="steelblue", size=11, face= "bold"),
        axis.title.y = element_text(color="steelblue", size=11, face= "bold")) +
  geom_line() +
  ylim(0, 20) +
  theme(legend.position = "none")

#--- 
cryptos_periods <- cryptos %>% 
  group_by(Name) %>% 
  summarise(Name= Name, Symbol= Symbol, 
            From= min(Date), To= max(Date), 
            Duration= as.integer(To-From)) %>%
  unique()

cryptos_feat <- cryptos %>% 
  left_join(cryptos_periods) %>%
  select(-c(From,To)) %>%
  filter(Marketcap!= 0 & Volume!= 0) %>% # & Symbol!= "BTC") %>%
  bind_cols(Attractiveness= unlist(AttractSmooth)) %>% # Outliers smoothed out
  filter(Duration > 800)  

# Create Predictors (Day, Month, Quarter, Year, Weekend, Semester)
cryptos_feat <- cryptos_feat %>%
  mutate(Day     = as.factor(yday(Date)),
         Month   = as.factor(month(Date)),
         Quarter = as.factor(quarter(Date)),
         Year    = as.factor(year(Date)))

cryptos_feat <- cryptos_feat %>%
  mutate(Weekdays = as.factor(weekdays(Date)),
         Weekend  = as.factor(if_else(Weekdays %in% c("Saturday", "Sunday") , 1, 0)),
         Semester = as.factor(if_else(Quarter %in% c("1", "2"),  1, 2))) %>%
  select(Symbol, Name, Low, Volume, Volume, Day,       #Attractiveness
         Weekend, Month, Semester, Year, Date,Attractiveness)

#----------------------------------
#--- Data partition
#----------------------------------
set.seed(1997, sample.kind="Rounding")

cryptos_feat <- cryptos_feat %>% filter(Year== 2013:2021) # & Symbol== "BTC")
# Validation partition
feat_set  <- cryptos_feat %>% filter(Date <  "2021-01-01")# $ Date > "2018-01-01") 
valid_set <- cryptos_feat %>% filter(Date >= "2021-01-01") 

# Test partition
train_set <- feat_set %>% filter(Date <  "2020-06-01") 
test_set  <- feat_set %>% filter(Date >= "2020-06-01") 
#---------------------------------------------------------------
# currencies <- list("XRP")
currencies <- list("XRP")
data <- list(train= train_set, test= test_set )
output <- lapply(currencies, function(currency, 
                                      train = train_set,
                                      test  = test_set) { 
  #--- Data partition
  
  # Create datafreme for Price error measures
  if (!exists("ResError")) {
    ResError <- data.frame(Variable = character(),
                           Currency= character(),
                           Model= character(),
                           Description= character(),
                           RMSE = double(),
                           MAE  = double(),
                           MASE = double()
    )
  }
  
  l_train <- train %>% filter(Symbol== currency)
  l_test  <- test %>% filter(Symbol== currency)
  #----------------------------------
  #--- Forecast models
  #----------------------------------
  # MODELE 0: lm
  #----------------------------------
  #Price
  fit_low_lm <- lm(Low ~ Volume+Weekend+Month+Semester, 
                   data = l_train)
  predicted_low <- predict(fit_low_lm, l_test)
  # KPIs  
  low_naive_rmse <- RMSE(l_test$Low, predicted_low)
  low_naive_mae  <- MAE(l_test$Low, predicted_low)
  low_naive_mase <- MASE(low_naive_mae, low_naive_mae) 
  
  ResError <-  bind_rows(ResError,
                         data_frame( 
                           Variable = "Low",
                           Currency = currency,
                           Model    = "MODEL 0",
                           Description = "Naïve: Linear Regression",
                           RMSE = low_naive_rmse,
                           MAE  = low_naive_mae,
                           MASE = low_naive_mase)
  )
  
  # Attractivenes
  fit_attract_lm <- lm(Attractiveness ~ Volume+Day+Weekend+Month+Semester,
                       data = l_train)
  predicted_attract <- predict(fit_attract_lm, data= l_test)
  # KPIs
  attract_naive_rmse <- RMSE(l_test$Attractiveness, predicted_attract)
  attract_naive_mae  <- MAE(l_test$Attractiveness, predicted_attract)
  attract_naive_mase <- MASE(attract_naive_mae, attract_naive_mae)
  
  ResError <-  bind_rows(ResError,
                         data_frame(
                           Variable = "Attractiveness",
                           Currency = currency,
                           Model    = "MODEL 0",
                           Description = "Naïve: Linear Regression",
                           RMSE = attract_naive_rmse,
                           MAE  = attract_naive_mae,
                           MASE = attract_naive_mase)
  )
  
  #----------------------------------          
  # MODEL 1: knn
  #----------------------------------
  #--- Price
  grid <- data.frame(k = seq(5, 25, 5))
  fit_low_knn <- train(Low ~ Volume+Attractiveness+
                         Weekend+Month+Semester+Year+Day,
                       data     = l_train,
                       method   = "knn",
                       tuneGrid = grid)
  predicted_low <- predict(fit_low_knn, l_test)
  
  # KPIs  
  low_knn_rmse <- RMSE(l_test$Low, predicted_low)
  low_knn_mae  <- MAE(l_test$Low, predicted_low)
  low_knn_mase <- MASE(low_knn_mae, low_naive_mae) 
  
  ResError <-  bind_rows(ResError,
                         data_frame( 
                           Variable = "Low",
                           Currency = currency,
                           Model    = "MODEL 1",
                           Description = "KNN",
                           RMSE = low_knn_rmse,
                           MAE  = low_knn_mae,
                           MASE = low_knn_mase)
  )
  
  fit_attract_knn <- train(Attractiveness ~ Low+Volume+
                             Weekend+Month+Semester+Year+Day,
                           data     = l_train,
                           method   = "knn",
                           tuneGrid = grid)
  predicted_attract <- predict(fit_attract_knn, l_test)
  
  # KPIs  
  attract_knn_rmse <- RMSE(l_test$Attractiveness, predicted_attract)
  attract_knn_mae  <- MAE(l_test$Attractiveness,  predicted_attract)
  attract_knn_mase <- MASE(attract_knn_mae, attract_naive_mae) 
  
  ResError <-  bind_rows(ResError,
                         data_frame( 
                           Variable = "Attractiveness",
                           Currency = currency,
                           Model    = "MODEL 1",
                           Description = "KNN",
                           RMSE = attract_knn_rmse,
                           MAE  = attract_knn_mae,
                           MASE = attract_knn_mase)
  )   
  
  #----------------------------------          
  # MODEL 2: Regression tree
  #----------------------------------
  grid <- data.frame(cp = seq(0, 0.1, 0.01))
  fit_low_rpart <- train(Low ~ Volume+Attractiveness+
                           Weekend+Month+Semester+Year+Day,
                         data     = l_train,
                         method   = "rpart",
                         tuneGrid = grid)
  predicted_low <- predict(fit_low_rpart, l_test)
  
  # KPIs  
  low_rpart_rmse <- RMSE(l_test$Low, predicted_low)
  low_rpart_mae  <- MAE(l_test$Low, predicted_low)
  low_rpart_mase <- MASE(low_rpart_mae, low_naive_mae) 
  
  ResError <-  bind_rows(ResError,
                         data_frame( 
                           Variable = "Low",
                           Currency = currency,
                           Model    = "MODEL 2",
                           Description = "Regression tree",
                           RMSE = low_rpart_rmse,
                           MAE  = low_rpart_mae,
                           MASE = low_rpart_mase)
  )
  
  fit_attract_rpart <- train(Attractiveness ~ Low+Volume+
                               Weekend+Month+Semester+Year+Day,
                             data     = l_train,
                             method   = "rpart",
                             tuneGrid = grid)
  predicted_attract <- predict(fit_attract_rpart, l_test)
  
  # KPIs  
  attract_rpart_rmse <- RMSE(l_test$Attractiveness, predicted_attract)
  attract_rpart_mae  <- MAE(l_test$Attractiveness,  predicted_attract)
  attract_rpart_mase <- MASE(attract_rpart_mae, attract_naive_mae) 
  
  ResError <-  bind_rows(ResError,
                         data_frame( 
                           Variable = "Attractiveness",
                           Currency = currency,
                           Model    = "MODEL 2",
                           Description = "Regression tree",
                           RMSE = attract_rpart_rmse,
                           MAE  = attract_rpart_mae,
                           MASE = attract_rpart_mase)
  )   
  
  #----------------------------------
  # Random forest
  #----------------------------------
  #--- Price
  fit_low_Rborist <- train(Low ~ Volume+Attractiveness+
                             Weekend+Month+Semester+Year+Day,
                           method   = "Rborist",
                           tuneGrid = data.frame(predFixed = seq(33,45,2),
                                                 minNode   = seq(33, 45, 2)),
                           data     = l_train)
  predicted_low <- predict(fit_low_Rborist, l_test)
  
  # KPIs  
  low_Rborist_rmse <- RMSE(l_test$Low, predicted_low)
  low_Rborist_mae  <- MAE(l_test$Low, predicted_low)
  low_Rborist_mase <- MASE(low_Rborist_mae, low_naive_mae) 
  
  ResError <-  bind_rows(ResError,
                         data_frame( 
                           Variable = "Low",
                           Currency = currency,
                           Model    = "MODEL 3",
                           Description = "Random forest",
                           RMSE = low_Rborist_rmse,
                           MAE  = low_Rborist_mae,
                           MASE = low_Rborist_mase)
  )
  
  #--- Attractiveness
  fit_attract_Rborist <- train(Attractiveness ~ Low+Volume+
                                 Weekend+Month+Semester+Year+Day,
                               method   = "Rborist",
                               tuneGrid = data.frame(predFixed = seq(33,45,2),
                                                     minNode   = seq(33, 45, 2)),
                               data     = l_train)
  predicted_attract <- predict(fit_attract_Rborist, l_test)
  
  # KPIs  
  attract_Rborist_rmse <- RMSE(l_test$Attractiveness, predicted_attract)
  attract_Rborist_mae  <- MAE(l_test$Attractiveness,  predicted_attract)
  attract_Rborist_mase <- MASE(attract_Rborist_mae, attract_naive_mae) 
  
  ResError <-  bind_rows(ResError,
                         data_frame( 
                           Variable = "Attractiveness",
                           Currency = currency,
                           Model    = "MODEL 3",
                           Description = "Random forest",
                           RMSE = attract_Rborist_rmse,
                           MAE  = attract_Rborist_mae,
                           MASE = attract_Rborist_mase)
  )
  #----------------------------------
  # MODEL 4: PCA
  #----------------------------------
  #--- Price
  rctrl <- trainControl(method = "cv", number= 5, returnResamp = "all")
  fit_low_pca  <- train(Low ~ Volume+Attractiveness+
                          Weekend+Month+Semester+Year+Day,
                        method= "pcr",
                        data= l_train,
                        trControl= rctrl,
                        preProc= c("center", "scale"),
                        tuneGrid= data.frame(ncomp= seq(1,10))
  )
  predicted_low <- predict(fit_low_pca, l_test)
  
  # KPIs  
  low_pca_rmse <- RMSE(l_test$Low, predicted_low)
  low_pca_mae  <- MAE(l_test$Low, predicted_low)
  low_pca_mase <- MASE(low_pca_mae, low_naive_mae) 
  
  ResError <-  bind_rows(ResError,
                         data_frame( 
                           Variable = "Low",
                           Currency = currency,
                           Model    = "MODEL 4",
                           Description = "PCA",
                           RMSE = low_pca_rmse,
                           MAE  = low_pca_mae,
                           MASE = low_pca_mase)
  )
  
  #--- Attractiveness
  fit_attract_pca  <- train(Attractiveness ~ Low+Volume+
                              Weekend+Month+Semester+Year+Day,
                            method= "pcr",
                            data= l_train,
                            trControl= rctrl,
                            preProc= c("center", "scale"),
                            tuneGrid= data.frame(ncomp= seq(1,10))
  )
  predicted_attract <- predict(fit_attract_pca, l_test)
  
  # KPIs  
  attract_pca_rmse <- RMSE(l_test$Attractiveness, predicted_attract)
  attract_pca_mae  <- MAE(l_test$Attractiveness,  predicted_attract)
  attract_pca_mase <- MASE(attract_pca_mae, attract_naive_mae) 
  
  ResError <-  bind_rows(ResError,
                         data_frame( 
                           Variable = "Attractiveness",
                           Currency = currency,
                           Model    = "MODEL 4",
                           Description = "PCA",
                           RMSE = attract_pca_rmse,
                           MAE  = attract_pca_mae,
                           MASE = attract_pca_mase))
  
  list(Errors        = ResError, 
       ImpPrice      = varImp(fit_low_Rborist),
       fit_low       = fit_low_knn,
       fit_attract   = fit_attract_Rborist,
       low_naive_mae = low_naive_mae,
       attract_naive_mae = attract_naive_mae)
  
})
#---------------------------------------------------------------
#----------------------------------
#--- Validation
#----------------------------------
# Store "XRP" calculation output
Errors      <- output[[1]]$Errors
imp_low     <- output[[1]]$ImpPrice
fit_low     <- output[[1]]$fit_low
fit_attract <- output[[1]]$fit_attract
low_naive_mae <- output[[1]]$low_naive_mae
attract_naive_mae <- output[[1]]$attract_naive_mae


# " XRP" Validation set 
valid_xrp <- valid_set %>% filter(Symbol== "XRP")

# Best model for Price is Knn - 
predicted_low <- predict(fit_low, valid_xrp)

# KPIs  
low_results <- data.frame(Description = "Knn", 
                          RMSE = RMSE(valid_xrp$Low, predicted_low), 
                          MAE  = MAE(valid_xrp$Low, predicted_low), 
                          MASE = MASE(low_knn_mae, low_naive_mae)
)


# Best model for Attractiveness is random forest - Validation set
predicted_attract <- predict(fit_attract, valid_xrp)
#KPIs

attract_results <- data.frame(Description = "Random forest",
                              RMSE = RMSE(valid_xrp$Attractiveness, predicted_attract),
                              MAE  = MAE(valid_xrp$Attractiveness,  predicted_attract),
                              MASE = MASE(attract_pca_mae, attract_naive_mae))

# Plot preparation
valid_low <- data.frame(predict = predicted_low,
                        Date    = valid_xrp$Date)
valid_attract <- data.frame(predict = predicted_attract,
                            Date    = valid_xrp$Date)


#feat_set 
cryptos_feat %>% filter(Symbol== "XRP" & Date > "2018-01-01") %>%
  ggplot(aes(x = Date, y = Low)) +
  geom_point(alpha = 0.5, color = "red") +
  geom_rect(xmin = as.numeric(ymd("2020-06-01")),
            xmax = as.numeric(ymd("2021-01-01")),
            ymin = 0, ymax = 30000,
            fill = "blue", alpha = 0.01) +
  geom_rect(xmin = as.numeric(ymd("2021-01-01")),
            xmax = as.numeric(ymd("2021-07-06")),
            ymin = 0, ymax = 30000,
            fill = "yellow", alpha = 0.01) +
  geom_point(aes(x= Date, y= predict,), data= valid_low)
