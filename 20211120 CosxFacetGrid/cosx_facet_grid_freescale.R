# 构建数据
mydata <- data.frame(city = rep(c("A", "B"), each = 4),
                     var = rep(rep(c("a", "b"), each = 2), 2),
                     time = rep(1:2, 4),
                     value = c(1, 2, 10, 20, 3, 4, 300, 400))

# 作图
ggplot(mydata) + geom_col(aes(time, value)) +
  facet_grid(city ~ var)

# facet_grid()的scales = "free"可让各列横纵坐标轴范围按需而定
ggplot(mydata) + geom_col(aes(time, value)) +
  facet_grid(city ~ var, scales = "free")

# 想要实现的效果
library(patchwork)
p1 <- ggplot(mydata[which(mydata$city == "A"), ]) + geom_col(aes(time, value)) +
  facet_wrap(.~ var, scales = "free") +
  labs(title = "(city A)")
p2 <- ggplot(mydata[which(mydata$city == "B"), ]) + geom_col(aes(time, value)) +
  facet_wrap(.~ var, scales = "free") +
  labs(title = "(city B)")
p1 / p2
