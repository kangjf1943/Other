library(ggplot2)
library(patchwork)
library(showtext)
showtext.auto()

# 数据清理函数：目标国家和目标年份
fun_clean <- function(x) {
  # 选取目标国家
  x <- x[which(x$Entity %in% c("China", "India", 
                               "United States", "Germany", "United Kingdom")), ]
  # 只选取1800年之后的数据
  x <- x[which(x$Year >= 1800), ]
}

# 各国各年份总排放量
tot_emis <- read.csv("annual-co2-emissions-per-country.csv")
tot_emis <- fun_clean(tot_emis)
# 单位化为亿吨
tot_emis$Annual.CO2.emissions <- tot_emis$Annual.CO2.emissions/1e8

# 各国各年份人均排放量
percap_emis <- read.csv("co-emissions-per-capita.csv")
percap_emis <- fun_clean(percap_emis)

# 作图
p1 <- ggplot(tot_emis) + 
  geom_line(aes(Year, Annual.CO2.emissions, color = Entity)) + 
  labs(x = "", y = "总排放量（吨）") + 
  scale_color_discrete(name = "国家") + 
  theme_bw()
p2 <- ggplot(percap_emis) + 
  geom_line(aes(Year, Annual.CO2.emissions..per.capita., color = Entity)) + 
  labs(x = "", y = "人均排放量（吨/人）") + 
  scale_color_discrete(name = "国家") + 
  theme_bw()
p3 <- p1 / p2 & theme(legend.position = "right")
p3 + plot_layout(guides = "collect")

# 截取1950年之后的数据作图
tot_emis <- tot_emis[which(tot_emis$Year > 1950), ]
percap_emis <- percap_emis[which(percap_emis$Year > 1950), ]

p1 <- ggplot(tot_emis) + 
  geom_line(aes(Year, Annual.CO2.emissions, color = Entity)) + 
  labs(x = "", y = "总排放量（吨）") + 
  guides(color = "none") + 
  theme_bw()
p2 <- ggplot(percap_emis) + 
  geom_line(aes(Year, Annual.CO2.emissions..per.capita., color = Entity)) + 
  labs(x = "", y = "人均排放量（吨/人）") + 
  guides(color = "none") + 
  theme_bw()
p3 <- p1 / p2 & theme(legend.position = "right")
p3 + plot_layout(guides = "collect")

# 中国1990年之后的碳排放
totemis_china <- tot_emis[which(tot_emis$Entity == "China" & 
                                  tot_emis$Year >= 1990), ]
percapemis_china <- percap_emis[which(percap_emis$Entity == "China" & 
                                        percap_emis$Year >= 1990), ]

p1 <- ggplot(totemis_china) + 
  geom_line(aes(Year, Annual.CO2.emissions), color = "red") + 
  labs(x = "", y = "总排放") + 
  theme_bw()
p2 <- ggplot(percapemis_china) + 
  geom_line(aes(Year, Annual.CO2.emissions..per.capita.), color = "red") + 
  labs(x = "", y = "人均排放") + 
  theme_bw()
p1 + p2


