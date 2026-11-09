library(readr)
winequality_white <- read_delim("Data/winequality-white.csv", 
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(winequality_white)

summary(winequality_white)

install.packages("rstatix")
library(rstatix)

# Identify outliers
outliers <- winequality_white %>% identify_outliers(quality)
wine_clean <- winequality_white[!winequality_white$quality %in% outliers, ]




hist(winequality_white$quality, main = "Histogram of White Wine Quality Scores" , xlab = "Quality Score", breaks = seq(from = 0, to = 10, by = 1))

winequality_white$quality_group <- cut(winequality_white$quality, 
                        breaks = c(2, 4, 6, 8), 
                        labels = c("Low", "Medium", "High"))

#MANOVA
manovaW <- manova(cbind(`fixed acidity`, `volatile acidity`, `citric acid`, `residual sugar`, 
                        chlorides, `free sulfur dioxide`, `total sulfur dioxide`, 
                        density, pH, sulphates, alcohol) ~ quality_group, 
                  data = winequality_white)

summary(manovaW)


#Single Welch ANOVAs
oneway.test(`fixed acidity` ~ quality_group, data = winequality_white, var.equal = FALSE)

oneway.test(`volatile acidity` ~ quality_group, data = winequality_white, var.equal = FALSE)

oneway.test(`citric acid` ~ quality_group, data = winequality_white, var.equal = FALSE)

oneway.test(`residual sugar` ~ quality_group, data = winequality_white, var.equal = FALSE)

oneway.test(chlorides ~ quality_group, data = winequality_white, var.equal = FALSE)

oneway.test(`free sulfur dioxide` ~ quality_group, data = winequality_white, var.equal = FALSE)

oneway.test(density ~ quality_group, data = winequality_white, var.equal = FALSE)

oneway.test(pH ~ quality_group, data = winequality_white, var.equal = FALSE)

oneway.test(sulphates ~ quality_group, data = winequality_white, var.equal = FALSE)

oneway.test(alcohol ~ quality_group, data = winequality_white, var.equal = FALSE)


#Single ANOVAs Standard
summary.aov(manovaW)




