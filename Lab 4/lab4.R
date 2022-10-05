# Генерация и анализ коррелированных выборок
# Tạo và phân tích các mẫu tương quan

# 1.Сгенерировать выборку X1 из нормального распределения
# Normal distribution X1 with N=200

a1= mean
S1= sd

N=200
a1=14.34
s1=3.87

x1=rnorm(N,a1,s1)
x1


# 2.Give 2 коэффициентов корреляции:

r1=0.125
r2=0.7

# 3. Build X2 and X3

a2=16.897
s2=2.687

x2= r1*x1 + sqrt(1-r1*r1)*rnorm(N,a2,s2)
x3= r2*x1 + sqrt(1-r2*r2)*rnorm(N,a2,s2)

x2
x3

# 4. Scatter plot
# математическая диаграмма, изображающая значения двух переменных
# в виде точек на декартовой плоскости
#Scatter plots are used to plot data points on a horizontal and a vertical axis
# in the attempt to show how much one variable is affected by another
plot(x1,x2,type='p',col="red",main="диаграммы рассеяния x1, x2")
#низкая положительная корреляция

plot(x1,x3,type='p',col="blue",main="диаграммы рассеяния  x1, x3")
#высокая положительная корреляция

# 5. Построить выборочные парные коэффициенты корреляции r1,r2 for X1 and X2, X1 and X3
# it is used to see the connection between distributions(используется, чтобы увидеть связь между дистрибутивами)

dx1=x1-mean(x1)
dx2=x2-mean(x2)
dx3=x3-mean(x3)

powdx1=dx1*dx1
powdx2=dx2*dx2
powdx3=dx3*dx3

rx1x2=sum(dx1*dx2)/sqrt(sum(powdx1)*sum(powdx2))
rx1x3=sum(dx1*dx3)/sqrt(sum(powdx1)*sum(powdx3))
rx1x2
rx1x3


# 6. Рассчитать статистики Стьюдента and проверить гипотезу

cor.test(x1,x2)
cor.test(x1,x3)
#вычисляет значение t- статистики Стьюдента для проверки значимости корреляционного коэффициента.

#статистики Стьюдента:
stdx1x2 =rx1x2/sqrt(1 - rx1x2*rx1x2)*sqrt(N-2)
stdx1x3=rx1x3/sqrt(1-rx1x3*rx1x3)*sqrt(N-2)

p=0.95
toComp=qt(p,df= N-2)# find the t-score of the 190th quantile of the Student t distribution with df = 198

if(abs(stdx1x2)<toComp){
print("There is a correlation between x1 and x2")
}else{print("There is not correlation between X1 and X2")} 

if(abs(stdx1x3)<toComp){
print("There is a correlation between distributions x1 and x3")
}else{print("The is not correlation between X1 and X3")}


# 7.Построить доверительный интервал
# Fisher Z-transform
#  стандартная ошибка:
t=1/sqrt(N-3)
#ковариацию между переменными:
z2=0.5*log((1+rx1x2)/(1-rx1x2))
z3=0.5*log((1+rx1x3)/(1-rx1x3))
#Рассчитайте доверительный интервал, используя статистику Z:
z2.lower=z2-qnorm(p)*t
z2.upper=z2+qnorm(p)*t

z3.lower=z3-qnorm(p)*t
z3.upper=z3+qnorm(p)*t

cat("Z2 [",z2.lower," , ",z2.upper,"]")
cat("Z3 [",z3.lower," , ",z3.upper,"]")

# Вычисление доверительного интервала корреляции с интервалом Фишера.
# (Calculating the correlation confidence interval with fisher interval)

cov1.lower=(exp(2*z2.lower)-1)/(exp(2*z2.lower)+1)
cov1.upper=(exp(2*z2.upper)-1)/(exp(2*z2.upper)+1)

cov2.lower=(exp(2*z3.lower)-1)/(exp(2*z3.lower)+1)
cov2.upper=(exp(2*z3.upper)-1)/(exp(2*z3.upper)+1)

cat("cov1 = [",cov1.lower," , ",cov1.upper,"]")
cat("cov1 = [",cov2.lower," , ",cov2.upper,"]")
