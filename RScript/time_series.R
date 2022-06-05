# Date: 04.06.2022
# Author: Vera Rykalina
# Task: Simple modelling for forecasting

# Citing: Using the mock data, build a simple model to forecast the revenues to expect in 2022. Nothing fancy or very complicated is needed here, the goal is for the model to be easy to use and to understand, not to be 100% accurate.

# Load packages
pacman::p_load(tidyverse, wrappedtools,lubridate, wesanderson, plotly, flextable)

# Load data
rawdata <- read_csv(file="Data/fake_clinic_data_extract_test.csv")

# Glimpse
head(rawdata)
dim(rawdata) # 4071 x 6
str(rawdata)
is.Date(rawdata$appointment_date)
range(rawdata$appointment_date)
glimpse(rawdata)

# Count NAs 
rawdata %>% 
  filter(revenues_from_appointment=="NULL") %>% 
  summarise(n=n()) # 375 raws

# Feature engineering 
rawdata$month <- lubridate::month(rawdata$appointment_date, label=TRUE, abbr=TRUE)
rawdata$week <- lubridate::week(rawdata$appointment_date)
rawdata$day <- lubridate::day(rawdata$appointment_date)
rawdata$wday <- lubridate::wday(rawdata$appointment_date, 
                                label=TRUE, abbr=TRUE,
                                week_start = getOption
                                ("lubridate.week.start", 1))

# Calculation missing values per month
(missing_vals <- rawdata %>% 
  group_by(month) %>% 
  arrange(appointment_date) %>% 
  filter(revenues_from_appointment=="NULL") %>% 
  summarise(n=n()))

# Plotting missing records per month
(missing_vals %>% 
  ggplot(aes(x=month, y=n, fill=month)) + geom_col()+
  labs(x="Month", 
       y="Number of missing revenue records",
       title="Missing values records") +
  scale_fill_manual(values=wes_palette(n=7, 
                    name="BottleRocket1"))+
  theme(legend.position="none"))

# Internal check point
rawdata %>% 
  select(appointment_date,month, day, revenues_from_appointment) %>% 
  arrange(appointment_date) %>% 
  filter(month=="Jun",
         day==29,
         revenues_from_appointment=="NULL") %>%
  summarise(n=n()) 
  


# Check on records depending on clinics proportion
table(rawdata$clinic_name) # clinic_1:clinic_2 = 2429:1642 

# Check on NAs ratio for clinics
rawdata %>% 
  filter(revenues_from_appointment=="NULL") %>% 
  group_by(clinic_name) %>% 
  summarise(n=n()) # 185:190

# Exclude rows with NULL values
rawdata <- rawdata %>% 
  filter(revenues_from_appointment!="NULL")

rawdata %>% select(revenues_from_appointment) %>% tail()

# Calculate length of strings
rawdata %>% 
  mutate(char_length=nchar(revenues_from_appointment)) %>% 
  count(char_length)

# Truncate unnecessary zeros
rawdata$revenues_from_appointment <- rawdata %>% 
  pull(6) %>% 
  str_remove("0+$") %>% 
  str_replace(pattern="(\\d+)\\.$", replace="\\1")


# Check on output control (after truncation)
# Modify slice window to check on random rows
rawdata %>% 
  select(revenues_from_appointment) %>% slice(1:3000)

# Convert revenues_from_appointment variable to numeric
rawdata$revenues_from_appointment <- 
  as.numeric(rawdata$revenues_from_appointment)
glimpse(rawdata)

# Range of revenues
range(rawdata$revenues_from_appointment)

# Some statistics to understand outliers (max/3d Qu.)
summary(rawdata$revenues_from_appointment)


# Density plot
rawdata %>% 
  ggplot(aes(x=revenues_from_appointment,
             fill=as.factor(clinic_name)))+
  geom_density(alpha=0.5) + # right-skewed
  scale_fill_manual(
    values=wes_palette(n=2, 
    name="BottleRocket1"), 
    name="Clinic",
    labels=c("1", "2"))+
  labs(x="Revenue", y="Probability density")

# Density plot of log-transformed data
rawdata %>% 
  ggplot(aes(x=log(revenues_from_appointment),
             fill=as.factor(clinic_name)))+
  geom_density(alpha=0.5) + # right-skewed
  scale_fill_manual(
    values=wes_palette(n=2, 
                       name="BottleRocket1"), 
    name="Clinic",
    labels=c("1", "2"))+
  labs(x="Revenue", y="Probability density(after log-transformation)")
ggsave(filename="Graphs/log_density.png", dpi=200, units="cm", width=22, height=11)

# Visualize revenues with bill not exceeding 500
rawdata %>%
  filter(month !="Jun", revenues_from_appointment<=500) %>%
  ggplot(aes(x=factor(clinic_name), 
             y=revenues_from_appointment,
             color=clinic_name)) + 
  geom_boxplot(coef=3)+
  labs(x="Clinic", y="Revenue") +
  scale_color_manual(values=wes_palette(n=2, 
                                      name="BottleRocket1"))+
  theme(legend.position="none")
ggsave(filename="Graphs/small_invoices.png", dpi=200, units="cm", width=22, height=11) 

# Visualize revenues by bill category
rawdata$revenue_ctg <-  
ifelse(rawdata$revenues_from_appointment <= 50, "\u2264 50",
ifelse(rawdata$revenues_from_appointment <= 100, "\u2264 100",
ifelse(rawdata$revenues_from_appointment <= 200, "\u2264 200",
ifelse(rawdata$revenues_from_appointment <= 500, "\u2264 500", ifelse(rawdata$revenues_from_appointment <= 1000, "\u2264 1000",
       " >1000")))))

# Plot
rawdata %>%
  ggplot(aes(x=factor(revenue_ctg, 
          levels = c("\u2264 50","\u2264 100", 
                     "\u2264 200", "\u2264 500",
                     "\u2264 1000"," >1000")), 
             y=revenues_from_appointment,
             fill=clinic_name)) + 
  geom_col()+
  labs(x="Invoice category", y="Revenue") +
  scale_fill_manual(
    values=wes_palette(n=5, 
    name="BottleRocket1"), 
    labels=c("Clinic 1", "Clinic 2"))+
  facet_wrap(~factor(clinic_name,
                     labels =c("Clinic 1", "Clinic 2"))) +
  theme(legend.position = "none",
        text = element_text(size = 14))
ggsave(filename="Graphs/invoice_category.png", dpi=200, units="cm", width=22, height=11)

# Visualize revenues by month not exceeding 500 bill, excluding June (as it is only two days within a month)
rawdata %>%
  filter(revenues_from_appointment<=500, month!="Jun") %>% 
  ggplot(aes(x=factor(month),
             y=revenues_from_appointment,
             color=month)) + 
  geom_boxplot(coef=3)+
  labs(x="Month", y="Revenue") +
  scale_color_manual(
    values=wes_palette(n=7, 
                       name="BottleRocket1"))+
  facet_wrap(~factor(clinic_name,
                     labels =c("Clinic 1", "Clinic 2"))) +
  theme(text = element_text(size = 14),
        legend.position = "none")+
  geom_hline(yintercept = 100,size=.2,linetype=2)
ggsave(filename="Graphs/revenue_boxplot.png", dpi=200, units="cm", width=22, height=11)

# Visualize revenue by month
month.total <- rawdata %>% 
  filter(month !="Jun")  %>%
  group_by(month=factor(month), clinic_name) %>%
  summarise(sum=sum(revenues_from_appointment)/1000) %>%
  ggplot(aes(x=month, y=sum)) +
  labs(x='Month',
       y='Revenue (x 1000)', 
       title='Total Revenue by Month') +
  geom_bar(aes(fill=clinic_name),
           stat='identity', 
           color='darkgrey') +
  scale_fill_manual(values = wes_palette(
    "BottleRocket1", n = 6)) +
  theme(text = element_text(size = 14), 
        legend.position = "none")
ggsave(filename="Graphs/revenue_month.png", dpi=200, units="cm", width=22, height=11)
ggplotly(month.total)

# Visualize revenue by week
week.total <- rawdata %>% 
  filter(week != 26)  %>%
  group_by(week=factor(week), clinic_name) %>%
  summarise(sum=sum(revenues_from_appointment/1000)) %>%
  ggplot(aes(x=week, y=sum)) +
  labs(x='Week',
       y='Revenue (x 1000)', 
       title='Total Revenue by Week') +
  geom_bar(aes(fill=clinic_name), 
           stat='identity', 
           color='darkgrey') +
  scale_fill_manual(values = wes_palette(
                   "BottleRocket1", n = 7)) +
  theme(text = element_text(size = 14), 
        legend.position = "none")
ggsave(filename="Graphs/revenue_week.png", dpi=200, units="cm", width=22, height=11)
ggplotly(week.total)



# Visualize revenue by month with plotly (line)
week.line <- rawdata %>%
  filter(month !="Jun")  %>%
  group_by(week=floor_date(appointment_date, "week")) %>%
  summarise(revenue = sum(revenues_from_appointment)) %>% 
  plot_ly(x = ~week, y = ~revenue, 
          name = 'Revenue', type = 'scatter', mode="lines") %>%
  layout(title = "Total Revenue by Month",
         xaxis = list(title = "Week"),
         yaxis = list (title = "Total revenue"),
         legend = list(orientation = "h",
                       xanchor = "center",
                       x = 0.5, y= -0.3))

# Visualize revenue by day with plotly (line)
day.line <- rawdata %>%
  group_by(day=floor_date(appointment_date, "day")) %>%
  summarise(revenue = sum(revenues_from_appointment)) %>% 
  plot_ly(x = ~day, y = ~revenue, 
          name = 'Revenue', type = 'scatter', mode="lines") %>%
  layout(title = "Total Revenue by Day",
         xaxis = list(title = "Day"),
         yaxis = list (title = "Total revenue"),
         legend = list(orientation = "h",
                       xanchor = "center",
                       x = 0.5, y= -0.3))

# Visualizations to study granularity using looping
aggregated_df <- c()
plots.sumrev <- c()
granularity <- c("month","week", "day")


for(g in granularity) {
  aggregated_df[[g]] <- rawdata %>%
    group_by(appointment_date=
            floor_date(appointment_date, g)) %>%
    summarise(`revenue (K)`=sum(revenues_from_appointment/1000)) 
  
# Revenue in [[g]] 
  plots.sumrev[[g]] <- 
    ggplot(data = aggregated_df[[g]], 
           aes(x = appointment_date)) +
    geom_line(aes(y = `revenue (K)`, color = "Revenue")) +
    theme_minimal()+
    labs(title = paste("Revenue per ", g),
         x = "Time",
         y = "Revenue (x 1000)") +
    theme(legend.position="bottom", 
          legend.title=element_blank())
}
plots.sumrev[["month"]]
plots.sumrev[["week"]]
plots.sumrev[["day"]]


#### Modelling ####
# Preparing analysis data

# Subset data by month 
Cmonth <- aggregated_df[["month"]] %>%
  filter(month(appointment_date) != 6)

# Save subset for Google spreadsheet 
write_csv(x = Cmonth, file = "Data/cleaned_month.csv")

# Subset data by week 
Cweek <- aggregated_df[["week"]] %>% 
  filter(week(appointment_date) != 52)


# Create ts objects
(ts_month <- ts(Cmonth, frequency = 12, start=c(2021, 7), 
                end = c(2021,12)))
(ts_week <- ts(Cweek, frequency = 52, start=c(2021,26)))
dim(ts_week)

# Data partition
train_month <- window(ts_month[,"revenue (K)"], start = c(2021, 7), end = c(2021,11))
test_month <- window(ts_month[,"revenue (K)"], start= c(2021, 12))


train_week <- window(ts_week[,"revenue (K)"], start = c(2021, 26), end = c(2021, 49))
test_week <- window(ts_week[,"revenue (K)"], start= c(2021, 50),
                    end=c(2021,51))

# Linear regression (month)
TSLMmonth <-tslm(train_month ~ trend)
predictionTSLMmonth = forecast(TSLMmonth, h=length(test_month)) 
accuracy(predictionTSLMmonth, test_month) 

# Linear regression (week)
TSLMweek <-tslm(train_week ~ trend)
predictionTSLMweek = forecast(TSLMweek, h=length(test_week)) 
accuracy(predictionTSLMweek, test_week) 

# Plot (month)
autoplot(predictionTSLMmonth) +
  xlab("Time") + ylab("Revenue (x 1000)") +
  ggtitle("TSLM: Revenue (monthly), 2 months ahead") 

# Plot (week)
autoplot(predictionTSLMweek) +
  xlab("Week") + ylab("Revenue (x 1000)") +
  ggtitle("TSLM: Revenue (weekly), 2 weeks ahead") 

# Comparison plot of fitted and actual data
autoplot(ts_month[,"revenue (K)"], series="Actual") +
  autolayer(predictionTSLMmonth, series="TSLM", PI=FALSE) +
  xlab("Time") + ylab("Revenue (x 1000)") +
  ggtitle("Revenue (monthly): Actual vs Fitted") +
  guides(colour=guide_legend(title=" ")) +
  theme(legend.position="bottom", legend.title=element_blank())

autoplot(ts_week[,"revenue (K)"], series="Actual") +
  autolayer(predictionTSLMweek, series="TSLM", PI=FALSE) +
  xlab("Time") + ylab("Revenue (x 1000)") +
  ggtitle("Revenue (weekly): Actual vs Fitted") +
  guides(colour=guide_legend(title=" ")) +
  theme(legend.position="bottom", legend.title=element_blank())



# Arima (month)
ARmonth <-auto.arima(train_month)
predictionARmonth  <- forecast(ARmonth, h=length(test_month))
accuracy(predictionARmonth, test_month)

# Arima (week
ARweek <-auto.arima(train_week)
predictionARweek  <- forecast(ARweek, h=length(test_week))
accuracy(predictionARweek, test_week)


# Comparison plot of fitted and actual data
autoplot(ts_month[,"revenue (K)"], series="Actual") +
  autolayer(predictionARmonth, series="Arima", PI=FALSE) +
  xlab("Time") + ylab("Revenue (x 1000)") +
  ggtitle("Revenue (monthly): Actual vs Fitted") +
  guides(colour=guide_legend(title=" ")) +
  theme(legend.position="bottom", legend.title=element_blank())

autoplot(ts_week[,"revenue (K)"], series="Actual") +
  autolayer(predictionARweek, series="Arima", PI=FALSE) +
  xlab("Time") + ylab("Revenue (x 1000)") +
  ggtitle("Revenue (weekly): Actual vs Fitted") +
  guides(colour=guide_legend(title=" ")) +
  theme(legend.position="bottom", legend.title=element_blank())


# Comparison plot of Fitted and Actual Values (weekly)
autoplot(ts_week[, 'revenue (K)'], series="Actual") +
  autolayer(predictionTSLMweek, series="TSLM", PI=FALSE) +
  autolayer(predictionARweek, series="Arima", PI= FALSE) +
  xlab("Time") + ylab("Revenue (x 1000)") + 
  ggtitle("Comparison of Models: Actual vs Fitted (weekly)") +
  guides(colour=guide_legend(title=" ")) +
  theme(legend.position="bottom", legend.title=element_blank())
ggsave(filename="Graphs/fitted_actual.png", dpi=200, units="cm", width=22, height=11)

# Forecast 3 months ahead)
# TSLM
(predictionTSLMmonth <- forecast(TSLMmonth, h=3) %>%
    autoplot() +
    xlab("Time") + ylab("Revenue (x 1000)") +
    ggtitle("Revenue (TSLM model): 3 months forecasting")) 
(summary(predictionTSLMmonth <- forecast(TSLMmonth, h=3)))
predictionTSLMmonth %>% as_tibble() %>%  
  mutate(Month=c("Dec", "Jan", "Feb"),
         Year=c("2021", "2022", "2022")) %>% 
  select(Month, Year, everything()) %>% 
  rename(`Forecast (x 1000)`= "Point Forecast") %>% 
  flextable()

# Arima (bad performance)
(predictionARmonth <- forecast(ARmonth, h=3) %>%
  autoplot() +
  xlab("Time") + ylab("Revenue (x 1000)") +
  ggtitle("Revenue (ARIMA model): 3 months forecasting"))
(summary(predictionARmonth <- forecast(ARmonth, h=3)))
predictionARmonth
