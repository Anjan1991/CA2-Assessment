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

# ------------------------------------------
#               Research Question
# ------------------------------------------

# How the heart rate changes with change in several characteristics 
# associated with sleep like respiration rate, body temperature, 
# limb movement, blood oxygen level and sleeping hours

# ----------------------------------------------------------------

interest_variables <- c("Snoring_Range",
                        "Respiration_Rate",
                        "Body_Temp",
                        "Limb_Movement",
                        "Blood_Oxy_Level",
                        "Sleeping_Hours",
                        "Heart_Rate")

# Showing pairs plot for the variables of interest
pairs(sleep_df_sorted[interest_variables])

# Showing scatter plot for each of the data variables against heart rate
scatter.smooth(x = sleep_df_sorted$Snoring_Range,
               y = sleep_df_sorted$Heart_Rate,
               xlab = "Snoring Range",
               ylab = "Heart Rate", 
               main = "Correlation of heart rate ~ snoring range")

scatter.smooth(x = sleep_df_sorted$Respiration_Rate,
               y = sleep_df_sorted$Heart_Rate,
               xlab = "Respiration Rate",
               ylab = "Heart Rate", 
               main = "Correlation of heart rate ~ respiration rate")

scatter.smooth(x = sleep_df_sorted$Body_Temp,
               y = sleep_df_sorted$Heart_Rate,
               xlab = "Body Temperature",
               ylab = "Heart Rate", 
               main = "Correlation of heart rate ~ body temperature")

scatter.smooth(x = sleep_df_sorted$Limb_Movement,
               y = sleep_df_sorted$Heart_Rate,
               xlab = "Limb Movement",
               ylab = "Heart Rate", 
               main = "Correlation of heart rate ~ Limb movement")

scatter.smooth(x = sleep_df_sorted$Blood_Oxy_Level,
               y = sleep_df_sorted$Heart_Rate,
               xlab = "Blood Oxygen Level",
               ylab = "Heart Rate", 
               main = "Correlation of heart rate ~ blood oxygen level")

scatter.smooth(x = sleep_df_sorted$Sleeping_Hours,
               y = sleep_df_sorted$Heart_Rate,
               xlab = "Sleeping Hours",
               ylab = "Heart Rate", 
               main = "Correlation of heart rate ~ sleeping hours")

# Showing the correlation values for each of the 
# data variables again the heart rate

paste("Correlation for Heart Rate and Snoring Range: ", cor(sleep_df_sorted$Heart_Rate, sleep_df_sorted$Snoring_Range))
paste("Correlation for Heart Rate and Respiration rate: ", cor(sleep_df_sorted$Heart_Rate, sleep_df_sorted$Respiration_Rate))
paste("Correlation for Heart Rate and Body Temperature: ", cor(sleep_df_sorted$Heart_Rate, sleep_df_sorted$Body_Temp))
paste("Correlation for Heart Rate and Blood Oxygen Level: ", cor(sleep_df_sorted$Heart_Rate, sleep_df_sorted$Blood_Oxy_Level))
paste("Correlation for Heart Rate and Limb Movement: ", cor(sleep_df_sorted$Heart_Rate, sleep_df_sorted$Limb_Movement))
paste("Correlation for Heart Rate and Sleeping Hours: ", cor(sleep_df_sorted$Heart_Rate, sleep_df_sorted$Sleeping_Hours))

# Finding outliers in the data variables
attach(sleep_df_sorted)
boxplot(Heart_Rate,
        main = "Heart Rate",
        sub = paste("Outlier rows: ",
                    boxplot.stats(Heart_Rate)$out)) # box plot for 'Heart Rate'
boxplot(Snoring_Range,
        main = "Snoring Range",
        sub = paste("Outlier rows: ",
                    boxplot.stats(Snoring_Range)$out)) # box plot for 'Snoring Range'
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
boxplot(Rapid_Eye_Movement,
        main = "Rapid Eye Movement",
        sub = paste("Outlier rows: ",
                    boxplot.stats(Rapid_Eye_Movement)$out)) # box plot for 'Rapid Eye Movement'
boxplot(Sleeping_Hours,
        main = "Sleeping Hours",
        sub = paste("Outlier rows: ",
                    boxplot.stats(Sleeping_Hours)$out)) # box plot for 'Sleeping Hours'

detach(sleep_df_sorted)

# As per the box plot of the data variables there
# are no outliers in the data variables of interest

# Let us examine by building the model which variable
# have higher influence on the model
attach(sleep_df_sorted)
set.seed(1)
heart_rate_model <- lm(formula = Heart_Rate ~
                         Snoring_Range +
                         Respiration_Rate +
                         Body_Temp +
                         Limb_Movement +
                         Blood_Oxy_Level +
                         Rapid_Eye_Movement +
                         Sleeping_Hours +
                         Stress_Level)
heart_rate_model
summary(heart_rate_model)