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

winequality_white$quality_group <- cut(winequality_white$quality, 
                        breaks = c(2, 4, 6, 8), 
                        labels = c("Low", "Medium", "High"))


#Levenes Test
install.packages("car")
library(car)

levene_result <- leveneTest(quality ~ quality_group, data = winequality_white)
levene_result
