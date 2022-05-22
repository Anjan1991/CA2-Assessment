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
# we can say that the data variables seems 