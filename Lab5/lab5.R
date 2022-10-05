# Критерии сравнения групп и анализ таблиц сопряженности 
# Criteria for comparison of groups and analysis of contingency tables

#            Задание 1:
# 1:Проверить выборки на нормальность графически и с помощью подходящего статистического критерия.

#Opening the file pulse

setwd("/Users/macbook/Documents/university/R/Lab5")


getwd()
x=read.table("pulse.txt", header=TRUE, sep='\t')
x
# Точки указывают на одномерную нормальность набора данных. 
# Если данные распределены нормально, точки будут лежать на опорной линии под углом 45 градусов.
# Если данные не распределены нормально, точки будут отклоняться от опорной линии.
# CB:
oldpar=par()
par(mfrow=c(1,2))
# строит график соответствия квантилей значений вектора x и ожидаемых квантилей
qqnorm(x$CB, main="Quantiles of CB", col="red")
hist(x$CB, col="red",
     density=15, border="black",
     ylab="grades", 
     main="patients before drug use",
     xlab="Patients",)

# EB:
oldpar=par()
par(mfrow=c(1,2))
qqnorm(x$EB, main="Quantiles of EB", col="blue")
hist(x$EB, col="blue",
     density=15, border="black",
     ylab="grades", 
     main="healthy people before drug use",
     xlab="Patients",)

# CA
oldpar=par()
par(mfrow=c(1,2))
qqnorm(x$CA, main="Quantiles of CA", col="green")
hist(x$CA, col="green",
     density=15, border="black",
     ylab="grades", 
     main="patients after drug use",
     xlab="Patients",)

#EA

oldpar=par()
par(mfrow=c(1,2))
qqnorm(x$EA, main="Quantiles of EA", col="gold")
hist(x$EA, col="gold",
     density=15, border="black",
     ylab="grades", 
     main="Healthy people after drug use",
     xlab="Patients",)

#Criteria
#Тест отклоняет гипотезу о нормальности, когда p-значение меньше или равно 0,05. 
# Непрохождение теста на нормальность позволяет с уверенностью 95% утверждать, что данные не соответствуют нормальному распределению.
shapiro.test(x$CB)
shapiro.test(x$EB)
shapiro.test(x$CA)
shapiro.test(x$EA)


# 2. Применив подходящие критерии сравнения групп определить однородность данных «до» и «после» 
# для обеих групп испытуемых: «здоровых» и «пациентов» и построить коробки с усами

# Performs one- and two-sample Wilcoxon tests on vectors of data;

# до

wilcox.test(x$CB,x$EB,var.equal = TRUE) 

#after medicines
wilcox.test(x$CA,x$EA)

boxplot(x$CB,x$EB,col="gold",main="Compare CB and EB",xlab="before drug use")
boxplot(x$CA,x$EA,col="green",main="Compare CA and EA",xlab="after drug use")

# A data set is homogeneous if it is made up of things (i.e. people, cells or traits) that are similar to each other.

# 3. Применив подходящие критерии сравнения групп определить однородность данных по группам испытуемых
# «здоровых» и «пациентов» для обеих случаев «до» и «после» и построить коробки с усами

wilcox.test(x$CB,x$CA,paired = TRUE)

wilcox.test(x$EB,x$EA,paired = TRUE)

boxplot(x$CB,x$CA,col="red",main="Comparison of CB and CA",xlab="Patients ")
boxplot(x$EB,x$EA,col="green",main="Comparison of CA and EA",xlab="Healthy People")

# 4. эффектив, определить однородность, 4 выборки взяты из одного распределения вероятностей

# Задание 2:

# 1.«Группа» и «Оценка».

getwd()
y=read.table("grades.txt", header=TRUE,fileEncodin="utf-16")
y

gr1=c(y$Группа1)
gr2=c(y$Группа2)
gr3=c(y$Группа3)
gr4=c(y$Группа4)

grades1=table(gr1)
grades1
grades2=table(gr2)
grades2
grades3=table(gr3)
grades3
grades4=table(gr4)
grades4
tri=c(17,12,13,4)
four=c(11,15,12,7)
five=c(2,3,5,19)

grades=matrix(c(tri,four,five),4,3,byrow=TRUE,dimnames=list(c("group1","group2","group3","group4"),c("3","4","5")))
grades
#2. Hypotesis критерий Фишера
fisher.test(grades, simulate.p.value=TRUE)
# никакой разницы между группами