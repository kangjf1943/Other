# 1
f<- c(136, 458, 289, 72, 72, 51)
m<- c(125, 426, 264, 64, 70, 45)
 # 2
names(f)<- names(m)<- c("京都","大阪","兵庫","奈良","滋賀","和歌山")
f
m
# 3
f + m
# 4
round(f/m, 2)
sum(f)
sum(m)
# 5
sort(f)
# 6
f[1:3]
# 7
m.r<- m
cbind(m, m.r)
m.r["大阪"]<- 425
m.r
# 8
mean(f)
sd(f)
var(f)
# 9
fm<- cbind(female=f, male=m)
fm

#2 行列
# 1
p20<- matrix(c(78, 271, 168, 41, 42, 28,
               76, 264, 160, 38,43, 27),
             2,
             6,
             byrow= TRUE)
p20
colnames(p20)<- c("京都","大阪","兵庫","奈良","滋賀","和歌山")
rownames(p20)<- c("男","女")
p20
# 2
rowSums(p20)
colSums(p20)
colMeans(p20)
rowMeans(p20)
# 3
ratio.fm<- p20["女",]/p20["男",]
ratio.fm
round(ratio.fm, 1)
# 4
p20[,"大阪"]-p20[,"兵庫"]
# 5
p20[, c(-3, -5)]
# 6
p20t<- t(p20)
p20t
# 7
p20t[order(p20t [, "女"]), ]
# 8
apply(p20, 1, median)

# 3 文字列と論理値
# 1
paste(f, "万人", sep="")
# 2
m> 100
# 3
m<= 200
# 4
f[f> 200]
# 5
p20[, p20["女",]> 50]

# 4 サンプルデータの利用
# 1
WorldPhones
apply(WorldPhones, 2, median)
# 2
apply(WorldPhones, 1, sd)
# 3
library(car)
head(Duncan)
dim(Duncan)
# 4
tapply(Duncan$prestige, Duncan$type, mean)
# 5
Duncan[Duncan$type=="prof", ]
# 6
dim(USArrests)
head(USArrests)
deviation.value<- matrix(NA,
                         nrow(USArrests),
                         ncol(USArrests)
                         )
colnames(deviation.value)<- colnames(USArrests)
rownames(deviation.value)<- rownames(USArrests)
head(deviation.value)
for(i in 1 : ncol(USArrests)){
  deviation.value[, i]<- scale(USArrests[, i])* 10 + 50
}
head(deviation.value)
summary(deviation.value)
apply(deviation.value, 2, sd)

# 5グラフなど
# 1
plot(Assault ~ UrbanPop, data = USArrests)
# 2
hist(USArrests$Assault)
# 3
library(car)
plot(prestige ~ type, data = Duncan)
# 4
Bfox
plot(rownames(Bfox),
     Bfox$partic,
     type = "l",
     xlab="year",
     ylab="労働力中の女性率")
# 5
matplot(rownames(Bfox),
        Bfox[, c("menwage", "womwage")],
        type = "l",
        xlab = "",
        ylab = "平均週給")
legend("topleft",
       lty = 1 : 2,
       col = 1 : 2,
       legend = c("男性", "女性"))
# 6 データの読み込みと欠損値、記述統計の答え

# 原来的代码 ----
# 开始
# 1
d0<- read.csv("ass26.csv")
d0
# 2
d1<- data.frame(
  year = seq(1885, 2005, by = 20),
  male = c(19, 23, 30, NA, 48, 59, 62),
  female = c(19, 23, 30, NA, 50, 62, 65)
)
d1
d0 == d1
# 结束 ----

# 添加的分析代码 ----
# 开始
# 1
d0<- read.csv("ass26.csv")
d0

# 检验一下d0数据中每一列都是什么类型数据
for (i in 1:ncol(d0)) {
  print(class(d0[, i]))
}
# 可见第一列是integer，第二和第三列是character

# 2
d1<- data.frame(
  year = seq(1885, 2005, by = 20),
  male = c(19, 23, 30, NA, 48, 59, 62),
  female = c(19, 23, 30, NA, 50, 62, 65)
)
d1

# 同样地看看d1各列都是什么类型的数据
for (i in 1:ncol(d1)) {
  print(class(d1[, i]))
}
# 可见各列都是numeric

# 因为d0和d1各列数据类型不同，自然就判断为对应元素不相等
d0 == d1

# 如果你把d0的第二列和第三列都改为numeric类型，就跟d1一样了
d0$male <- as.numeric(d0$male)
d0$female <- as.numeric(d0$female)
d0 == d1
# 结束 ----

# 3
summary(d0)
# 4
colMeans(d0)
colMeans(d0, na.rm = TRUE)
# 5
library(car)
head(Duncan)
tabl<- xtabs(~ type, data = Duncan)
tabl
# 6
barplot(tab1)

# 7
prop.table(tab1)
# 8
library(AER)
data(CPS1985)
# 9
plot(wage ~ married, data = CPS1985)
