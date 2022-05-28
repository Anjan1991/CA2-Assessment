# Loading data set
sleep_df <- read.csv("sleep.csv", na = "")
sleep_df
str(sleep_df)

# renaming the columns as per medical terminologies
names(sleep_df) <- c("Snoring_Range",
                     "Respiration_Rate",
                     "Body_Temp",
                     "Limb_Movement",
                     "Blood_Oxy_Level",
                     "Rapid_Eye_Movement",
                     "Sleeping_Hours",
                     "Heart_Rate",
                     "Stress_Level")

sleep_df_renamed <- sleep_df
sleep_df_renamed

# Finding the number of rows in data frame
nrow(sleep_df_renamed)

# Checking for missing values if any
sum(is.na(sleep_df_renamed))

# ----------------------------------------------------------
# From the initial looks of the dataframe we can see
# --that there are 630 records.
# --variable 1 to 8 is continuous in nature. 
# --variable 9 is categorical.
# --Also there are no missing values.
# ----------------------------------------------------------

# in this data set we have a categorical variable
# which needs to be converted to factor

# Converting stress level to factor variable
sleep_df_renamed$Stress_Level_factored <- factor(sleep_df_renamed$Stress_Level,
                                                 labels = c("low",
                                                            "normal",
                                                            "intermediate",
                                                            "medium",
                                                            "high"),
                                                 ordered = TRUE)

sleep_df_renamed
str(sleep_df_renamed)

# Showing summary of the data set
summary(sleep_df_renamed)

# Sorting the data frame by stress level
sleep_df_sorted <- sleep_df_renamed[order(sleep_df_renamed$Stress_Level_factored),]
sleep_df_sorted

# Initial investigation of data variables
# and their correlations
pairs(sleep_df_sorted)

# Looking at this correlation plot 
# i can say that the data variables 
# are somewhat correlated to each other

# Lets look at the psirs.panel plot for correlation
install.packages("psych")
library(psych)
pairs.panels(sleep_df_sorted,
             smooth = FALSE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = FALSE,    # If TRUE, draws ellipses
             method = "pearson",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals

# Looking at the pairs.panel plot i can say that
# the data variables have corrleation to each other
# Also looking at the histograms and the density plot
# we can say that the data variables seems to not normally distributed

varibles_of_interest <- names(sleep_df_sorted) %in% c("Respiration_Rate",
                                              "Body_Temp",
                                              "Limb_Movement",
                                              "Blood_Oxy_Level",
                                              "Heart_Rate",
                                              "Sleeping_Hours")

new_sleep_df <- sleep_df_sorted[(varibles_of_interest)]

# Rounding the variables to 2 places
new_sleep_df <- round(new_sleep_df, digits = 2)

# Showing pairs plot for the variables of interest
pairs(new_sleep_df)

opar <- par(no.readonly = TRUE)
par(mfrow = c(3,2))

# Showing scatter plot for each of the data variables against heart rate
scatter.smooth(x = new_sleep_df$Respiration_Rate,
               y = new_sleep_df$Blood_Oxy_Level,
               xlab = "Respiration Rate",
               ylab = "Blood Oxygen Level", 
               main = "Correlation of Blood Oxygen Level ~ respiration rate")

scatter.smooth(x = new_sleep_df$Body_Temp,
               y = new_sleep_df$Blood_Oxy_Level,
               xlab = "Body Temperature",
               ylab = "Blood Oxygen Level", 
               main = "Correlation of Blood Oxygen Level ~ body temperature")

scatter.smooth(x = new_sleep_df$Limb_Movement,
               y = new_sleep_df$Blood_Oxy_Level,
               xlab = "Limb Movement",
               ylab = "Blood Oxygen Level", 
               main = "Correlation of Blood Oxygen Level ~ Limb movement")

scatter.smooth(x = new_sleep_df$Sleeping_Hours,
               y = new_sleep_df$Blood_Oxy_Level,
               xlab = "Sleeping Hours",
               ylab = "Blood Oxygen Level", 
               main = "Correlation of Blood Oxygen Level ~ sleeping hours")

scatter.smooth(x = new_sleep_df$Heart_Rate,
               y = new_sleep_df$Blood_Oxy_Level,
               xlab = "Heart Rate",
               ylab = "Blood Oxygen Level", 
               main = "Correlation of blood oxygen level ~ heart rate")

# Showing the correlation values for each of the 
# data variables again the heart rate

paste("Correlation for Body Temperature and Respiration rate: ", cor(new_sleep_df$Blood_Oxy_Level, new_sleep_df$Respiration_Rate))
paste("Correlation for Heart Rate and Body Temperature: ", cor(new_sleep_df$Blood_Oxy_Level, new_sleep_df$Body_Temp))
paste("Correlation for Heart Rate and Limb Movement: ", cor(new_sleep_df$Heart_Rate, new_sleep_df$Limb_Movement))
paste("Correlation for Heart Rate and Blood Oxygen Level: ", cor(new_sleep_df$Blood_Oxy_Level, new_sleep_df$Heart_Rate))
paste("Correlation for Heart Rate and Blood Oxygen Level: ", cor(new_sleep_df$Blood_Oxy_Level, new_sleep_df$Sleeping_Hours))

# Finding outliers in the data variables
attach(new_sleep_df)
opar <- par(no.readonly = TRUE)
par(mfrow = c(3,2))
boxplot(Heart_Rate,
        main = "Heart Rate",
        sub = paste("Outlier rows: ",
                    boxplot.stats(Heart_Rate)$out)) # box plot for 'Heart Rate'
boxplot(Respiration_Rate,
        main = "Respiration Rate",
        sub = paste("Outlier rows: ",
                    boxplot.stats(Respiration_Rate)$out)) # box plot for 'Respiration Rate'
boxplot(Body_Temp,
        main = "Body Temperature",
        sub = paste("Outlier rows: ",
                    boxplot.stats(Body_Temp)$out)) # box plot for 'Body Temperature'
boxplot(Limb_Movement,
        main = "Limb Movement",
        sub = paste("Outlier rows: ",
                    boxplot.stats(Limb_Movement)$out)) # box plot for 'Limb Movement'
boxplot(Blood_Oxy_Level,
        main = "Blood Oxygen Level",
        sub = paste("Outlier rows: ",
                    boxplot.stats(Blood_Oxy_Level)$out)) # box plot for 'Blood Oxygen Level'

boxplot(Sleeping_Hours,
        main = "Sleeping Hours",
        sub = paste("Outlier rows: ",
                    boxplot.stats(Sleeping_Hours)$out)) # box plot for 'Sleeping Hours'

par <- opar

detach(new_sleep_df)

# As per the box plot of the data variables there
# are no outliers in the data variables of interest

# Skewness function to examine the normality
install.packages("e1071")
library(e1071)
opar <- par(no.readonly = TRUE)
par(mfrow = c(4,2))

# Density plot for Snoring Range
plot(density(new_sleep_df$Respiration_Rate),
     main = "Density Plot:Respiration Rate",
     xlab = "Respiration Rate",
     ylab = "Frequency",
     sub = paste("Skewness:", round(e1071::skewness(new_sleep_df$Respiration_Rate), 2)))

polygon(density(new_sleep_df$Respiration_Rate), col = "blue") #fills the area under the curve

plot(density(new_sleep_df$Body_Temp),
     main = "Density Plot:Body Temperature",
     xlab = "Body Temperature",
     ylab = "Frequency",
     sub = paste("Skewness:", round(e1071::skewness(new_sleep_df$Body_Temp), 2)))

polygon(density(new_sleep_df$Body_Temp), col = "blue") #fills the area under the curve

plot(density(new_sleep_df$Limb_Movement),
     main = "Density Plot:Limb Movement",
     xlab = "Limb Movement",
     ylab = "Frequency",
     sub = paste("Skewness:", round(e1071::skewness(new_sleep_df$Limb_Movement), 2)))

polygon(density(new_sleep_df$Limb_Movement), col = "blue") #fills the area under the curve

plot(density(new_sleep_df$Blood_Oxy_Level),
     main = "Density Plot:Blood Oxygen Level",
     xlab = "Blood Oxygen Level",
     ylab = "Frequency",
     sub = paste("Skewness:", round(e1071::skewness(new_sleep_df$Blood_Oxy_Level), 2)))

polygon(density(new_sleep_df$Blood_Oxy_Level), col = "blue") #fills the area under the curve

plot(density(new_sleep_df$Heart_Rate),
     main = "Density Plot:Heart Rate",
     xlab = "Heart Rate",
     ylab = "Frequency",
     sub = paste("Skewness:", round(e1071::skewness(new_sleep_df$Heart_Rate), 2)))

polygon(density(new_sleep_df$Heart_Rate), col = "blue") #fills the area under the curve

plot(density(new_sleep_df$Sleeping_Hours),
     main = "Density Plot:Sleeping Hours",
     xlab = "Sleeping Hours",
     ylab = "Frequency",
     sub = paste("Skewness:", round(e1071::skewness(new_sleep_df$Sleeping_Hours), 2)))

polygon(density(new_sleep_df$Sleeping_Hours), col = "blue") #fills the area under the curve

par <- opar

# Checking normality more formally through the normality test
# Running Shapiro-Wilks test on the data variables of interest

normality_test_Respiration_Rate <- shapiro.test(new_sleep_df$Respiration_Rate)
normality_test_Respiration_Rate

normality_test_Body_Temp <- shapiro.test(new_sleep_df$Body_Temp)
normality_test_Body_Temp

normality_test_Limb_Movement <- shapiro.test(new_sleep_df$Limb_Movement)
normality_test_Limb_Movement

normality_test_Blood_Oxy_Level <- shapiro.test(new_sleep_df$Blood_Oxy_Level)
normality_test_Blood_Oxy_Level

normality_test_Sleeping_Hours <- shapiro.test(new_sleep_df$Sleeping_Hours)
normality_test_Sleeping_Hours

normality_test_Heart_Rate <- shapiro.test(new_sleep_df$Heart_Rate)
normality_test_Heart_Rate

# Let us examine by building the model which variable
# have higher influence on the model
attach(new_sleep_df)
set.seed(1)
no_rows_data <- nrow(new_sleep_df)
sample_data <- sample(1:no_rows_data,
                      size = round(0.8 * no_rows_data),
                      replace = FALSE)

training_data <- new_sleep_df[sample_data, ]
test_data <- new_sleep_df[-sample_data, ]

heart_rate_model <- lm(Blood_Oxy_Level ~
                         Respiration_Rate +
                         Body_Temp +
                         Limb_Movement +
                         Heart_Rate +
                         Sleeping_Hours,
                       data = training_data)

summary(heart_rate_model)

new_sleep_df <- subset(new_sleep_df, select = -c(Heart_Rate, Respiration_Rate))
set.seed(1)
no_rows_data <- nrow(new_sleep_df)
sample_data <- sample(1:no_rows_data,
                      size = round(0.8 * no_rows_data),
                      replace = FALSE)

training_data <- new_sleep_df[sample_data, ]
test_data <- new_sleep_df[-sample_data, ]

heart_rate_model <- lm(Blood_Oxy_Level ~
                         Body_Temp +
                         Limb_Movement +
                         Sleeping_Hours,
                       data = training_data)

summary(heart_rate_model)

opar <- par(no.readonly = TRUE)
par(mfrow = c(1,2))

library(car)
qqPlot(heart_rate_model, labels = row.names(new_sleep_df),
       id.method = "Identity",
       simulated = TRUE,
       main = "Q-Q Plot")


training_data["534",]
training_data["3",]
training_data["88",]
training_data["159",]

fitted(heart_rate_model)["534"]
fitted(heart_rate_model)["3"]
fitted(heart_rate_model)["88"]
fitted(heart_rate_model)["159"]

student_heart_rate_model <- rstudent(heart_rate_model)
hist(student_heart_rate_model, breaks = 10, freq = FALSE,
     xlab = "Studentized residual", main = "Distribution of errors")

curve(dnorm(x, mean = mean(student_heart_rate_model),
            sd = sd(student_heart_rate_model)), add = TRUE,
      col = "blue", lwd = 2)

lines(density(student_heart_rate_model)$x,
      density(student_heart_rate_model)$y,
      col = "red",
      lty = 2,
      lwd = 2)

legend("topright", legend = c("Normal curve", "Kernel density curve"),
       lty = 1:2, col = c("blue", "red"), cex = .7)

outlierTest(heart_rate_model)

crPlots(heart_rate_model)

cutoff <- 4/(nrow(training_data) - length(heart_rate_model$coefficients) - 2)
plot(heart_rate_model, which = 4, cook.levels = cutoff)
abline(h = cutoff, lty = 2, col = "red")

avPlots(heart_rate_model, ask=FALSE)

influencePlot(heart_rate_model, main="Influence Plot",
              sub="Circle size is proportional to Cook's distance")
ncvTest(heart_rate_model)
spreadLevelPlot(heart_rate_model)

install.packages("gvlma")
library(gvlma)
gvlma(heart_rate_model)
summary(heart_rate_model)

library(car)
vif(heart_rate_model)
sqrt(vif(heart_rate_model)) > 2
summary(powerTransform(training_data$Blood_Oxy_Level))

sqrt_transform_blood_oxy_level <- sqrt(training_data$Blood_Oxy_Level)
training_data$blood_oxy_level_sqrt <- sqrt_transform_blood_oxy_level

fit_model1 <- lm(Blood_Oxy_Level ~
                   Body_Temp +
                   Limb_Movement +
                   Sleeping_Hours,
                 data = training_data)

fit_model2 <- lm(blood_oxy_level_sqrt ~
                   Body_Temp +
                   Limb_Movement +
                   Sleeping_Hours,
                 data = training_data)

AIC(fit_model1, fit_model2)

spreadLevelPlot(fit_model2)

fit_model <- lm(Blood_Oxy_Level ~
                  Body_Temp +
                  Limb_Movement +
                  Sleeping_Hours,
                data = training_data)

fit_model_sqrt <- lm(blood_oxy_level_sqrt ~
                       Body_Temp +
                       Limb_Movement +
                       Sleeping_Hours,
                     data = training_data)


predicted_blood_oxy_level <- predict(fit_model, test_data)
predicted_blood_oxy_level_sqrt <- predict(fit_model_sqrt, test_data)
converted_blood_oxy_level_sqrt <- predicted_blood_oxy_level_sqrt ^ 2

actual_predictions <- data.frame(cbind(actuals = test_data$Blood_Oxy_Level, predicted = predicted_blood_oxy_level))
head(actual_predictions)

actual_predictions_sqrt <- data.frame(cbind(actuals = test_data$Blood_Oxy_Level,
                                             predicted = converted_blood_oxy_level_sqrt))
head(actual_predictions_sqrt)

correlation_accuracy <- cor(actual_predictions)
correlation_accuracy

correlation_accuracy <- cor(actual_predictions_sqrt)
correlation_accuracy

min_max_accuracy <- mean(apply(actual_predictions, 1, min) / apply(actual_predictions, 1, max))
min_max_accuracy
  
min_max_accuracy <- mean(apply(actual_predictions_sqrt, 1, min) / apply(actual_predictions_sqrt, 1, max))
min_max_accuracy  

sigma(fit_model) / mean(test_data$Blood_Oxy_Level)  
sigma(fit_model_sqrt) / mean(test_data$Blood_Oxy_Level)

summary(new_sleep_df) 

df <- data.frame(Respiration_Rate = c(18.54), Body_Temp = c(94.54), Limb_Movement = c(8.54), Sleeping_Hours = c(5.54))
predicted_blood_oxy_level <- predict(fit_model, df)
predicted_blood_oxy_level

