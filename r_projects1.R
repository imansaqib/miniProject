# Mini-Project 5 Validation

# clear workspace
rm(list = ls())

# load packages
library(tidyverse)
library(magrittr)
library (ggtext)

# load data
data <- read.csv('rrData.csv') # data should be 200 obss x 4 variables
data$participant <- factor(data$participant) # make participant variable a factor
table(data$participant) # should be 10 repeats per participant
# LINE PLOT ----
# reshape the data into long format so that there are 4 columns: participant, time, feature (rr or rr_fft), and value
data_long <- data %>% gather(key = "feature", value = "value", -participant, -time) # data_long should be 400 obs. x 4 variables

# line plot
ggplot(data_long, aes(x = time, y = value, color = feature)) +
  facet_wrap(vars(participant), nrow=4, ncol=5,scales="free")+
  geom_point()+geom_line()+
  ggtitle("Figure 1: Line Plot") +
  labs(x='time', y = "respiration rate")

# BAR PLOT ----
# find the mean and standard deviation within each participant-feature

summ <- data_long %>% group_by(participant, Feature.iman) %>% summarize(mean=mean(value), se = sd(value)/sqrt(n())max = mean +se, min = mean -se) # summary should be  40 obs. x 4 variables

# bar plot
ggplot(summ, aes(x = participant, y = mean) +
         geom_col()
 # geom_bar(stat = "identity", position = "dodge")+
    
 #ggtitle("Figure 2: Bar Plot")+
  #geom_errorbar(aes(x= participant, color = Feature.iman, ymax = Standard_Deviation))




# SCATTER PLOT ----
# fit linear model to data, y = rr_fft, x = rr)
fit <- lm(data$rr_fft ~ data$rr)
ggplot(data, aes (x = rr, y =rr_fft))+
  geom_point()+
  geom_smooth(method = lm, se = F)+
  labs(x = "RR(bprm)", y = "RR FFT(bprm)", title = "Figure 3: Scatter Plot")+
  theme_minimal()
  
# combine text for equation
eq <- substitute(italic(y) == a + b %.% italic(x)*", "~~italic(r)^2~"="~r2, 
                 list(a = format(unname(coef(fit)[1]), digits = 2),
                      b = format(unname(coef(fit)[2]), digits = 2),
                      r2 = format(summary(fit)$r.squared, digits = 2)))
text <- as.character(as.expression(eq));

# scatter plot
ggplot(data, aes()) +
  ggtitle("Figure 3: Scatter Plot") +
  annotate("text", x = 30, y = 30, label = text, parse = TRUE) 








# BLAND-ALTMAN PLOT ----
# calculate and save the differences between the two measures and the averages of the two measures
#data %<>% mutate()

# Bland-Altman plot
ggplot(data, aes()) +
  ggtitle("Figure 4: Bland-Altman Plot") 
#create new column for average measurement
data$avg <- rowMeans(data) 

#create new column for difference in measurements
data$diff <- data$A - data$B

head (data)

#find average difference
mean_diff <- mean(data$diff)

mean_diff

#[1] 0.5


#find lower 95% confidence interval limits
lower <- mean_diff - 1.96*sd(data$diff)

lower

#[1] -1.921465

#find upper 95% confidence interval limits
upper <- mean_diff + 1.96*sd(data$diff)

upper

#[1] 2.92146

#load ggplot2
library(ggplot2)

#create Bland-Altman plot
ggplot(data, aes(x = rr, y = rr_fft)) +
  geom_point(size=2) +
  geom_hline(yintercept = mean_diff) +
  geom_hline(yintercept = lower, color = "red", linetype="dashed") +
  geom_hline(yintercept = upper, color = "red", linetype="dashed") +
  ggtitle("Bland-Altman Plot") +
  ylab("Difference Between Measures (rr-rr_fft)(brpm)") +
  xlab("Average of Measures (brpm)")




# BOX PLOT ----
# box plot
#library(ggplot2); library(dplyr);
#library(viridis); library(ggExtra)

  
ggplot(data, aes(x = participant, y = rr-rr_fft, col = participant )) +
      geom_boxplot( fill="skyblue", notch=FALSE) +
  xlab("participant")+
  ylab("Difference Between Measures (rr-rr_fft) (bprm) (Saqib)")+
  ggtitle("Figure 5: Box Plot") 
  
  


