
library(moments)
library(MASS)
n=150
shape=2
rate=3
prob=0.16
p=0.01

# полигон частот -один из способов графического представления плотности вероятности случайной величины
# СКО - Standard deviation -характеризует разброс значений относительно среднего (математического ожидания)

# 1. Экспоненциальное распределение
# абсолютно непрерывное распределение, моделирующее время между двумя 
# последовательными свершениями одного и того же события

# Exponential distribution- distribution of the time between events in a process in which events occur continuously and independently at a constant average rate

rate=1
# 1.Сгенерировать выборки
dexp=rexp(n, rate)
dexp

#2.
h4=hist(dexp, col="blue") #строим гистограмму относительных частот.
lines(h4$counts ~ h4$mids, col="red") 
# строим график плотности
em1=density(dexp) #оценки плотности.
plot(em1) #график оценки плотности 

#3.оценки числовых характеристик:
mean(dexp) #выборочные среднее
var(dexp) #дисперсия
sko=sd(dexp) #среднеквадратическое отклонение
moda=density(dexp)$x[which.max(density(dexp)$y)] # мода
median(dexp)
skewness(dexp) # коэффициенты асимметрии
kurtosis(dexp) # эксцесса — мера остроты пика распределения случайной величины.

#4. Найти теоретические мат ожидание и дисперсию
expmatexpect=1/rate
expmatexpect

expdisp=1/(rate*rate)
expdisp

#5.оценки параметров:
library(fitdistrplus)
fitdist(dexp,'exp')
fitdistr(dexp,'exponential') # оценка параметров функцией пакета MASS

#A hypothesis test is a method of statistical inference used to decide whether the data at hand sufficiently support a particular hypothesis
#метод статистического вывода, используемый для определения того, достаточно ли имеющиеся данные подтверждают конкретную гипотезу.
# Критерий хи-квадрат — любая статистическая проверка гипотезы, в которой выборочное распределение критерия 
# имеет распределение хи-квадрат при условии верности нулевой гипотезы
#a statistical hypothesis test that is valid to perform when the test statistic is chi-squared distributed under the null hypothesis
#6.критерия X^2:
xhc=hist(dexp, plot=FALSE)$counts; # сохраняем абсолютные частоты
xhc
xhb=hist(dexp, plot=FALSE)$breaks;  # сохраняем границы интервалов разбиения
xhb
k=length(xhc) # количество интервалов разбиения
k
xhb[1]=-Inf
xhb[k+1]=Inf # раздвигаем границы (только крайние значения интервалов разбиения) до бесконечности
pnth=pexp(xhb,rate) # если теоретические характеристики распределения не известны, можно оценить их по выборке
pnth
thfr=pnth[2:(k+1)]-pnth[1:k]; # рассчитываем теоретические относительные накопленные частоты
thfr
chisq.test(xhc,p=thfr)

# 2. Гамма-распределение
# двухпараметрическое семейство абсолютно непрерывных распределений
# Gamma Distribution-a two-parameter family of continuous probability distributions

dgamma = rgamma(n,shape,rate)
dgamma

h1=hist(dgamma,col = "green")
lines(h1$counts ~ h1$mids,col="red")
plot(density(dgamma))

mean(dgamma) #arithmetic mean
var(dgamma) #variance generic
sd(dgamma) #standard deviation
median(dgamma)
density(dgamma)$x[which.max(density(dgamma)$y)]
skewness(dgamma)# measure the asymmetry of the distribution or data set.
kurtosis(dgamma)# the sharpness of the peak in the data distribution

disp= shape/(rate*rate)
disp
matexpect= shape/rate
matexpect

fitdistr(dgamma,"gamma") #maximum-likelihood Fitting

#6.критерия X^2:
xhc=hist(dgamma, plot=FALSE)$counts; # сохраняем абсолютные частоты
xhc
xhb=hist(dgamma, plot=FALSE)$breaks;  # сохраняем границы интервалов разбиения
xhb
k=length(xhc) # количество интервалов разбиения
k
xhb[1]=-Inf
xhb[k+1]=Inf # раздвигаем границы (только крайние значения интервалов разбиения) до бесконечности
pnth=pgamma(xhb,shape,rate) # если теоретические характеристики распределения не известны, можно оценить их по выборке
pnth
thfr=pnth[2:(k+1)]-pnth[1:k]; # рассчитываем теоретические относительные накопленные частоты
thfr
chisq.test(xhc,p=thfr)

# 3. Биномиальное распределение
# распределение количества «успехов» в последовательности из n независимых случайных экспериментов, 
# таких, что вероятность «успеха» в каждом из них постоянна и равна p

# Binomial distribution -of the number of successes in a sequence of n independent experiments, 
# each asking a yes–no question, and each with its own Boolean-valued outcome: success or failure

size=7
dbin = rbinom(n, size, p)
dbin

var(dbin)
mean(dbin)
sd(dbin)
density(dbin)$x[which.max(density(dbin)$y)]
median(dbin)
#sort(dbin)
#dbin.t<-table(dbin)
#dbin.t
#sort(dbin)[which.max(dbin.t)]
skewness(dbin)
kurtosis(dbin)

binmatexpect=size*p
binmatexpect

bindisp=size*p*(1-p)
bindisp

p=mean(dbin)/size
p
# fitdistr(dbin,"negative binomial") #maximum-likelihood Fitting

h4<-hist(dbin)
lines(h4$counts ~ h4$mids, col="blue")
em2=ecdf(dbin) #эмпирическая функция распределения
em2
plot(em2)
#6.критерия X^2:
xhc=hist(dbin, plot=FALSE)$counts; # сохраняем абсолютные частоты
xhc
xhb=hist(dbin, plot=FALSE)$breaks;  # сохраняем границы интервалов разбиения
xhb
k=length(xhc) # количество интервалов разбиения
k
xhb[1]=-Inf
xhb[k+1]=Inf # раздвигаем границы (только крайние значения интервалов разбиения) до бесконечности
pnth=pnorm(xhb,mean(dbin),sd(dbin)) # если теоретические характеристики распределения не известны, можно оценить их по выборке
pnth
thfr=pnth[2:(k+1)]-pnth[1:k]; # рассчитываем теоретические относительные накопленные частоты
thfr
chisq.test(xhc,p=thfr)
# 4. Геометрическое распределение
#дискретной случайной величины равной количеству испытаний случайного эксперимента до наблюдения первого «успеха»
#Geometric Distribution 

dgeom = rgeom(n,prob)
dgeom
fitdistr(dgeom, "geometric")

var(dgeom)
mean(dgeom)
sd(dgeom)
median(dgeom)
density(dgeom)$x[which.max(density(dgeom)$y)]
skewness(dgeom)
kurtosis(dgeom)

geomatexpect=(1-prob)/prob
geomatexpect
prob
geodisp=(1-prob)/(prob*prob)
geodisp

fitdistr(dgeom, "geometric")

h3=hist(dgeom)
lines(h3$counts ~ h3$mids, col="blue")
em1=ecdf(dgeom)
em1
plot(em1)
#6.критерия X^2:
xhc=hist(dgeom, plot=FALSE)$counts; # сохраняем абсолютные частоты
xhc
xhb=hist(dgeom, plot=FALSE)$breaks;  # сохраняем границы интервалов разбиения
xhb
k=length(xhc) # количество интервалов разбиения
k
xhb[1]=-Inf
xhb[k+1]=Inf # раздвигаем границы (только крайние значения интервалов разбиения) до бесконечности
pnth=pgeom(xhb,prob) # если теоретические характеристики распределения не известны, можно оценить их по выборке
pnth
thfr=pnth[2:(k+1)]-pnth[1:k]; # рассчитываем теоретические относительные накопленные частоты
thfr
chisq.test(xhc,p=thfr)