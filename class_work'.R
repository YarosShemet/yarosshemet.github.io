data <- airquality[124:153, ]
library(psych)
library(ggplot2)
describeBy(x = data$Wind, data$Month)

# asdklf ------------------------------------------------------------------

data <- subset(airquality, airquality %in% c(7,8,9))
result <- aggregate(data$Ozone ~ data$Month, data, FUN = length)

# fjds --------------------------------------------------------------------

data_2 <- iris
describeBy(data_2, data_2$Species, na.rm = T)

# fdsnkj ------------------------------------------------------------------

ggplot(data, aes(Month, Ozone))+
  geom_boxplot()

data_2 <- mtcars
ggplot(data_2, aes(mpg, disp, col=hp))+
  geom_point()

ggplot(iris, aes(Sepal.Length, col = Species)) + geom_histogram()
ggplot(iris, aes(Sepal.Length)) + geom_histogram(aes(col = Species))
ggplot(iris, aes(Sepal.Length)) + geom_histogram(aes(fill = Species))
ggplot(iris, aes(Sepal.Length, fill = Species)) + geom_histogram()

#https://stepik.org/lesson/11787/step/8?unit=2671
ggplot(iris, aes(Sepal.Length, Sepal.Width, col = Species, size=Petal.Length)) +
  geom_point()


# prop.table --------------------------------------------------------------
general_prop <- prop.table(HairEyeColor)  #every single number is a procent from all observations
only_female_prop <- prop.table(HairEyeColor[, , "Female"])   #every single number from WOMEN observations
only_blue_eyes_men_prop <- prop.table(HairEyeColor[, "Blue", "Male"])  #every single number from blue_eyes men

#1-string = 100%
prop.table(HairEyeColor[, , "Male"], 1)
#2-column = 100%
prop.table(HairEyeColor[, , "Male"], 2)
#those men, who have blue eyes
blue_eyes_men <- prop.table(HairEyeColor[, , "Male"], 2)[, "Blue"]
#those men, who have both blue eyes and red hair (percentage from all blue_eyes)
red_men <- prop.table(HairEyeColor[ , ,"Male"], 2)["Red", "Blue"]

#sum of green eyes women 
sum((HairEyeColor[, "Green", "Female"]))


# plot_table --------------------------------------------------------------

library("ggplot2")
mydata <- as.data.frame(HairEyeColor)

ggplot(data = subset(mydata, Sex == "Female"), aes(x = Hair, y = Freq, fill=Eye))+
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_manual(values=c("Brown", "Blue", "Darkgrey", "Darkgreen"))
                                 
?geom_bar   


# testy -------------------------------------------------------------------

#chi
chisq.test(HairEyeColor["Brown", , "Female"])

my_table <- table(diamonds$cut, diamonds$color)
chi <- chisq.test(my_table)
main_test <- chi$statistic

mydata <- as.data.frame(diamonds)
mydata$factor_price <- ifelse(mydata$price >= mean(mydata$price), 1, 0)
mydata$factor_carat <- ifelse(mydata$carat >= mean(mydata$carat), 1, 0)
my_table <- table(mydata$factor_price, mydata$factor_carat)
chi <- chisq.test(my_table)
main_test <- chi$statistic

#fisher
t1 <- table(mtcars$am, mtcars$vs)
chi <- fisher.test(t1)
main_test <- chi$p.value

#t-student
data <- subset(iris, Species != "setosa")
ggplot(data, aes(x = Sepal.Length))+
  geom_histogram(fill = "white", col = "black", binwidth = 0.4)+
  facet_grid(Species ~ .)

ggplot(data, aes(x = Sepal.Length, fill = Species, alpha = 0.5))+
  geom_density()

ggplot(data, aes(Species, Sepal.Length))+
  geom_boxplot()

shapiro.test(data$Sepal.Length)  #normalność rozkładu
shapiro.test(data$Sepal.Length[data$Species == "versicolor"])
shapiro.test(data$Sepal.Length[data$Species == "virginica"])

bartlett.test(Sepal.Length ~ Species, data)  #heteroskedastyczność

t.test(Sepal.Length ~ Species, data)  #t-student

str(ToothGrowth)  
data_hw <- subset(ToothGrowth, (dose == 0.5 & supp == "VC") | (dose == 2 & supp == "OJ"))
data <- subset(ToothGrowth, )
t_test <- t.test(len ~ supp, data_hw)       #dla niezależnych prób
t_stat <- t_test$statistic

mydata <- read.csv("D:/Programowanie/R_projekty/Stepik/lekarstva.csv")
print(t.test(mydata$Pressure_before, mydata$Pressure_after, paired = T)$statistic)    #dla zależnych prób

ggplot(data, aes(Species, Sepal.Length))+
  stat_summary(fun.data = mean_cl_normal, geom="errorbar")           #przedziały ufności

ggplot(data, aes(Species, Sepal.Length))+
  stat_summary(fun.data = mean_cl_normal, geom="pointrange")

final_data <- read.table("D:/Programowanie/R_projekty/Stepik/dataset_11504_15 (1).txt")
if (bartlett.test(V1 ~ V2, final_data)$p.value < 0.05){
  print(wilcox.test(V1 ~ V2, final_data, var.equal = T)$p.value)
} else {
  print(t.test(V1 ~ V2, final_data, var.equal = T)$p.value)
} 

final_data <- read.table("D:/Programowanie/R_projekty/Stepik/dataset_11504_16.txt")
if (t.test(final_data$V1, final_data$V2, paired = T)$p.value < 0.05){
  print(mean(final_data$V1), mean(final_data$V2), t.test(data$V1, data$V2, paired = T)$p.value)
} else {
  print("The difference is not significant")
}
?t.test


# analiza wariancji -------------------------------------------------------

data <- npk
anova <- aov(yield ~ N * P, data = data)
summary(anova)

anova_normal <- aov(yield ~ N + P + K, data = data)
summary(anova_normal)

anova_iris <- aov(Sepal.Width ~ Species, data = iris)
TukeyHSD(anova_iris)


new_data <- read.csv("D:/Programowanie/R_projekty/Stepik/Pillulkin.csv")
new_data$patient <- as.factor(new_data$patient)
str(new_data)
fit_one_factor <- aov(temperature ~ pill + Error(patient/pill), data = new_data)
fit_two_factor <- aov(temperature ~ pill*doctor + Error(patient/(pill*doctor)), data = new_data)
summary(fit_one_factor)
summary(fit_two_factor)

library(ggplot2)
obj <- ggplot(ToothGrowth, aes(x = as.factor(dose), y = len, col = supp, group = supp))+
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width = 0.1, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size = 3, position = position_dodge(0.2))+
  stat_summary(fun.data = mean_cl_boot, geom = 'line', position = position_dodge(0.2))
