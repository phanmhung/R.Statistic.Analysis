# Парная регрессия

#1. Построить диаграмму рассеяния
# показывает вид и тесноту взаимосвязи между парами данных
setwd("/Users/macbook/Documents/university/R/Lab6")
getwd()

x=read.table('Kassovye.txt', header=TRUE)
x

plot(x$money, x$budget, type='p', xlab='Кассовые сборы', ylab='Бюджет фильма')


#2.Найти коэффициент корреляции
# Для выборки budget нормальность принимается, а для money – нет
shapiro.test(x$money);
shapiro.test(x$budget);

# корректнее использовать непараметрический коэффициент ранговой корреляции Спирмена
cor.test(x$money,x$budget,method = "spearman")
# 3. Провести анализ на выбросы с помощью коробок с усами.

boxplot(x$money,  x$budget)

x=x[-39,]
x
shapiro.test(x$money)
shapiro.test( x$budget)
cor.test(x$money, x$budget)

#4. Построить регрессионную модель:

x=x[order(x$budget),]
x
#найти оценки коэффициентов модели:
regresion=lm(money~budget, data=x)
regresion$coefficient
#зависимости
summary(regresion)
fisher.test(x, simulate.p.value = TRUE)

# 5.Построить графики линии регрессии и границ доверительных интервалов
# Графическое выражение регрессионного уравнения называют линией регрессии. 
# Линия регрессии выражает наилучшее предсказание зависимой переменной (Y)
# по независимым переменным (X).
# relationship between a scalar response and one or more explanatory variables


plot(money~budget, data=x)
abline(regresion)
# Доверительный интервал для линии регрессии:
test=predict(regresion,interval='conf', level=0.95)
matlines(x$budget, cbind(test), lty=c(1,2,2))


# 6. Провести анализ остатков модели

residuals=regresion$residuals
shapiro.test(residuals)
qqnorm(residuals)


# 7. Задать произвольное значение бюджета и построить прогнозное значение кассовых сборов

budget = 70
p = c(1, budget)
prognos = regresion$coefficients%*%p
prognos

