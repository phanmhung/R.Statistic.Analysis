#Критерии сравнения групп

#1. Generate X1 and X2 of 100 randoms from normal distribution

N=100

# a.Математические ожидания и дисперсии равны

mean=16
sd=4                 

x11=rnorm(N, mean, sd)
x11

x12= rnorm(N,mean,sd)
x12

# b.Математические ожидания существенно различны, дисперсии равны

mean1=12
mean2=7
sd2=2.34

x21=rnorm(N,mean1,sd2)
x21

x22=rnorm(N,mean2,sd2)
x22

# c.Математические ожидания равны, дисперсии существенно различны
mean3=24
sd31=3.45
sd32=6.3

x31=rnorm(N, mean3, sd31)
x31

x32=rnorm(N,mean3,sd32)
x2

#d. Математические ожидания и дисперсии существенно различны

mean41=13.44
mean42=8.23
sd41=1.952
sd42=3.33

x41=rnorm(N, mean41, sd41)
x41

x42=rnorm(N,mean42,sd42)
x42

# 2. Для каждой ситуации:
# a. Check hypotheses Shapiro–Wilk  
# Критерий Шапиро-Уилка основан на оптимальной линейной несмещённой оценке дисперсии к её обычной оценке
# методом максимального правдоподоби

# Give us W=$ pow(a*x) / $ pow(x- mean) 
#If the p-value is > 0.05, 
#then that distribution is about to the rnorm distribution

shapiro.test(x11)  
shapiro.test(x12)
t.test(x11,x12) #Гипотеза о равенстве средних для двух независимых выборок из нормальных совокупностей в предположении о равенстве дисперсий
var.test(x11,x12) #Функция проверки гипотезы о равенстве дисперсий двух выборок

shapiro.test(x21)
shapiro.test(x22)
t.test(x21,x22)
var.test(x21,x22)

shapiro.test(x31)
shapiro.test(x32)
t.test(x31,x32)
var.test(x31,x32)

shapiro.test(x41)
shapiro.test(x42)
t.test(x41,x42)
var.test(x41,x42)


# b.Build histograms and density for distributions

#1.a

hist(x11, breaks =20, freq=FALSE,
     xlim = c(0,30), density=10, 
     col="green", border="black",
     main ="Histogram MO and Dispersion are equals",
     xlab ="x1 and x2 distribution", ylab="density")

lines(density(x11),col="Blue", lwd=2)

hist(x12,breaks =20, density=10,freq=FALSE,
     include.lowest = TRUE, right = TRUE,
     col = "orange", border="black",
     main ="Математические ожидания и дисперсии равны",
     xlab = "x1 and x2 distribution", ylab="Frequency", add=TRUE)
lines(density(x12), col="yellow", lwd=2)

#1.b

#гистограмм, оценки плотностей для обеих выборок
hist(x21, breaks =20, freq=FALSE,
     xlim = c(0,20), density=10, 
     col="yellow", border="black",
     main ="Математические ожидания существенно различны, дисперсии
равны",
     xlab ="x1 and x2 distribution", ylab="density")
# вертикальными линиями отметить средние значения:
lines(density(x21),col="Blue", lwd=2)


hist(x22,breaks =20, density=10,freq=FALSE,
     include.lowest = TRUE, right = TRUE,
     col = "red", border="black",
     main ="Математические ожидания равны, дисперсии существенно различны",
     xlab = "x1 and x2 distributions", ylab="Frequency", add=TRUE)
lines(density(x22), col="Red", lwd=2)

#1.c
hist(x31, breaks =20, freq=FALSE,
     xlim = c(10,45), density=10, 
     col="cyan", border="black",
     main ="Математические ожидания и дисперсии существенно различны",
     xlab ="x1 and x2 distribution", ylab="density")
lines(density(x31),col="Blue", lwd=2)

hist(x32,breaks =20, density=10,freq=FALSE,
     include.lowest = TRUE, right = TRUE,
     col = "deeppink", border="black",
     main ="Математические ожидания равны, дисперсии существенно различны",
     xlab = "x1 and x2 distributions", ylab="Frequency", add=TRUE)
lines(density(x32), col="Red", lwd=2, add=TRUE)

#1.d

hist(x41, breaks =20, freq=FALSE,
     xlim = c(-2,20), density=10, 
     col="darkorange", border="black",
     main ="Математические ожидания и дисперсии существенно различны",
     xlab ="x1 and x2 distribution", ylab="density")
lines(density(x41),col="Blue", lwd=2)

hist(x42,breaks =20, density=10,freq=FALSE,
     include.lowest = TRUE, right = TRUE,
     col = "green", border="black",
     main ="Математические ожидания и дисперсии существенно различны",
     xlab = "x1 and x2 distributions", ylab="Frequency", add=TRUE)
lines(density(x42), col="Red", lwd=2)


# c. построить коробки с усами для Х1 и Х2.
# компактно изображают одномерное распределение вероятностей.
# показывает медиану , среднее, нижний и верхний квартили, минимальное и максимальное значение выборки и выбросы

# Расстояния между различными частями ящика позволяют определить степень разброса (дисперсии) и асимметрии данных и выявить выбросы.
a=summary(x11)
summary(x12)
boxplot(x11, x12, col="orange")
abline(h=mean(x11), col="green")
abline(h=mean(x12), col="green")

b=summary(x21)
summary(x22)
boxplot(x21, x22, col="red")
abline(h=mean(x21), col="green")
abline(h=mean(x22), col="green")

c=summary(x31)
summary(x32)
boxplot(x31, x32, col="blue")
abline(h=mean(x31), col="green")
abline(h=mean(x32), col="green")

d=summary(x41)
summary(x42)
boxplot(x41, x42, col="blue")
abline(h=mean(x41), col="green")
abline(h=mean(x42), col="green")

