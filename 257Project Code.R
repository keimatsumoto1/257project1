library(readr)
library(knitr)
library(rstatix)

winequality_white <- read_delim("Data/winequality-white.csv", 
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)

#summary(winequality_white)


# Identify outliers
outliers <- winequality_white %>% identify_outliers(quality)

winequality_white$quality_group <- cut(winequality_white$quality, 
                                       breaks = c(2, 4, 6, 8), 
                                       labels = c("Low", "Medium", "High"))

hist(winequality_white$quality, main = "Histogram of White Wine Quality Scores" , xlab = "Quality Score", breaks = seq(from = 0, to = 10, by = 1))


#MANOVA
manovaW <- manova(cbind(`fixed acidity`, `volatile acidity`, `citric acid`, `residual sugar`, 
                        chlorides, `free sulfur dioxide`, `total sulfur dioxide`, 
                        density, pH, sulphates, alcohol) ~ quality_group, 
                  data = winequality_white)

summary(manovaW)


#Single Welch ANOVAs
t1 = oneway.test(`fixed acidity` ~ quality_group, data = winequality_white, var.equal = FALSE)

t2 = oneway.test(`volatile acidity` ~ quality_group, data = winequality_white, var.equal = FALSE)

t3 = oneway.test(`citric acid` ~ quality_group, data = winequality_white, var.equal = FALSE)

t4 = oneway.test(`residual sugar` ~ quality_group, data = winequality_white, var.equal = FALSE)

t5 = oneway.test(chlorides ~ quality_group, data = winequality_white, var.equal = FALSE)

t6 = oneway.test(`free sulfur dioxide` ~ quality_group, data = winequality_white, var.equal = FALSE)

t7 = oneway.test(`total sulfur dioxide` ~ quality_group, data = winequality_white, var.equal = FALSE)

t8 = oneway.test(density ~ quality_group, data = winequality_white, var.equal = FALSE)

t9 = oneway.test(pH ~ quality_group, data = winequality_white, var.equal = FALSE)

t10 = oneway.test(sulphates ~ quality_group, data = winequality_white, var.equal = FALSE)

t11 = oneway.test(alcohol ~ quality_group, data = winequality_white, var.equal = FALSE)

tab <- data.frame(
  Test = c("Fixed Acidity", "Volatile Acidity", "Citric Acid", "Residual Sugar", "Chlorides", "Free Sulfur Dioxide", "Total Sulfur Dioxide",
           "Density", "pH", "Sulphates","Alcohol"),
  P_Value = c(t1$p.value, t2$p.value, t3$p.value, t4$p.value, t5$p.value, t6$p.value,t7$p.value, t8$p.value, t9$p.value, t10$p.value, t11$p.value)
)



#Single ANOVAs Standard
summary.aov(manovaW)


wine_corr_df <- winequality_white[,c("fixed acidity", "volatile acidity", "citric acid", "residual sugar", "chlorides", "free sulfur dioxide", "total sulfur dioxide",
  "density", "pH", "sulphates","alcohol")]
wine_corr_matrix <- cor(wine_corr_df)
print(wine_corr_matrix)

