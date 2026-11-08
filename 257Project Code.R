library(readr)
winequality_white <- read_delim("Data/winequality-white.csv", 
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(winequality_white)

summary(winequality_white)

install.packages("rstatix")
library(rstatix)

# Identify outliers
outliers <- winequality_white %>% identify_outliers(quality)

hist(winequality_white$quality, main = "Histogram of White Wine Quality Scores" , xlab = "Quality Score", breaks = seq(from = 0, to = 10, by = 1))
