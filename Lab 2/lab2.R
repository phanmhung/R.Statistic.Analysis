# Интервальные оценки параметров нормального распределения
# Estimating parameters from a normal distribution

# continuous probability distribution for a real-valued random variable

#1. N=[100, 500, 1000]

# Параметры задать самостоятельно:
mean=6.72
sd=3.15

N1=100
x1 = rnorm(N1, mean, sd) #generate normal distribution
x1

N2=500
x2 = rnorm(N2, mean, sd)
x2

N3=1000
x3 =rnorm(N3,mean,sd)
x3

#2. Build qqplot and Histograms для построенных выборок
# Quantile-Quantile (Q-Q) Plot

# plot, which is a graphical method for comparing two probability distributions 
# by plotting their quantiles against each other
oldpar=par()
par(mfrow=c(1,2))
qqnorm(x1, col="red")
hist(x1,freq=FALSE,density=10,angle=45, col="gold", 
     xlim = c(0,15), #interval of x
     main = "Normal distribution for 100 numbers",
     xlab = "rNorm X1", border="black",
     ylab="Frequency of variables")
#Эти 2 партии, по-видимому, произошли от популяций с общим распространением.
#Значения пакета 1 совпадают с соответствующими значениями пакета 2.
# Различия
oldpar=par()
par(mfrow=c(1,2))
qqnorm(x2, col="red")

#2nd:
hist(x2,freq=FALSE,density=10,angle=45, col="red", 
     xlim = c(0,15), #interval of x
     main = "Normal distribution for 500 random numbers",
     xlab = "rNorm X2", border="black",
     ylab="Frequency of variables")

#3rd:
oldpar=par()
par(mfrow=c(1,2))
qqnorm(x3, col="blue")
hist(x3,col="green",
     xlim = c(0,15), #interval of x
     main = "Normal distribution for 1000 random numbers",
     xlab = "rNorm X3",
     ylab="Frequency of variables")


#3. Find values of numerical characteristics

#выборочные среднее:
mean(x1)
mean(x2)
mean(x3)

#дисперсию:
var(x1)
var(x2)
var(x3)

#среднее квадратическое отклонение
sd(x1)
sd(x2)
sd(x3)


# 4.Построить доверительные интервалы для математического ожидания при известном значении 
# дисперсии на уровне значимости α = 0,05
#термин при интервальной оценке статистических параметров, более предпочтительной при небольшом объёме выборки, чем точечная
#term for interval estimation of statistical parameters, more preferable with a small sample size than point
summary(x1)
mean(x1)
se=sd(x1)/sqrt(N1)*qnorm(1-0.05/2)	   #standar error  sd(x1)=sqrt(var(x1))
se

ci.upper1=mean(x1)+se
ci.lower1=mean(x1)-se
cat("доверительный интервал for x1 = [",ci.lower1,",",ci.upper1,"]")


summary(x2)
mean(x2)
se=sd(x2)/sqrt(N2)*qnorm(1-0.05/2)	
se
ci.upper2=mean(x2)+se
ci.lower2=mean(x2)-se
cat("доверительный интервал for x2= [",ci.lower2,",",ci.upper2,"]")


summary(x3)
mean(x3)
se=sd(x3)/sqrt(N3)  
se
ci.upper3=mean(x3)+se
ci.lower3=mean(x3)-se
cat("доверительный интервал x3= [",ci.lower3,",",ci.upper3,"]")


#5.Построить доверительные интервалы для математическогоожидания при неизвестном значении 
# дисперсии (оцененном по выборке) на уровне значимости α = 0,05


summary(x1)
mean(x1)
se=mean(x1)/sqrt(N1)* qt(1- (0.05/2), N1-1)   #standar error  unknowing the dispersion
se

c=qnorm(1-0.05/2)		  #trust cofficient
q=qt(1- (0.05/2), N1-1)

ci.upper11=mean(x1)+se
ci.lower11=mean(x1)-se
cat("доверительный интервал for x1 = [",ci.lower11,",",ci.upper11,"]")


summary(x2)
mean(x2)
se=mean(x2)/sqrt(N2)*qt(1- (0.05/2), N2-1)  
se
ci.upper22=mean(x2)+se
ci.lower22=mean(x2)-se
cat("доверительный интервал for x2= [",ci.lower22,",",ci.upper22,"]")


summary(x3)
mean(x3)
se=mean(x3)/sqrt(N3)* qt(1- (0.05/2), N3-1)
se
ci.upper33=mean(x3)+se
ci.lower33=mean(x3)-se
cat("доверительный интервал for x3= [",ci.lower33,",",ci.upper33,"]")


#6. Построить график зависимости точечных оценок математического ожидания, 
# а также левых и правых границ доверительных интервалов от объема выборки

min = min(ci.lower1, ci.lower2, ci.lower3, ci.lower11, ci.lower33, ci.lower33)
min

max = max(ci.upper1, ci.upper2, ci.upper3, ci.upper11 , ci.upper22, ci.upper33)
max

meanN = c(mean, mean, mean)
meanN

MO = c(mean(x1), mean(x2), mean(x3))
MO

n = c(N1, N2, N3)
n


xMin = c(ci.lower1, ci.lower2, ci.lower3)
xMax = c(ci.upper1, ci.upper2, ci.upper3)
yMin = c(ci.lower11, ci.lower22, ci.lower33)
yMax = c(ci.upper11 , ci.upper22, ci.upper33)

ylin = c(min, max)

plot(n, MO ,type="p",   col = 1:6,
	xlab="Elements(N)", xlim=c(100,1000),
      ylab="Math. expectations", ylin)

legend("topleft", legend=c("y1", "y2","y3"), pch=c(1, 2, 3), col=c(2, 3,4))

lines(n, meanN)
lines (n, MO, pch=1, col="blue")
lines (n, xMin, pch=1,col="red")
lines (n, xMax,pch=2, col="red")
lines (n, yMin, pch=3, col="green")
lines (n, yMax, pch=3, col="green")

