# 关于问题 ----
# 载入所需包
# 用于读取Excel文件
library(openxlsx)
# 用于转化成data.table格式，据说运行起来比较快
library(data.table)
# 用于进行模糊匹配
library(sqldf)

# 读取原始条目
origin <- read.xlsx("Original_trait.xlsx")
# 转化成data.table格式
origin <- data.table(origin)

# 读取目标条目
target <- read.csv("Target_trait.txt", sep = "\t", header = TRUE)
# 选择目标列并重命名
target <- target[, "Trait"]
names(target) <- "target"
# 转化成data.table格式
target <- data.table(target)

# 基于模糊匹配进行数据匹配
join <- data.table(sqldf("select * 
           from origin a inner join target b
                      on a.origin like '%' || b.target || '%'", drv = "SQLite"))
# 查看结果
join
# 可见40条原始条目中，只有7条能够找到模糊对应的目标条目，而且是一一对应的关系

# 采取另一种方法进行模糊匹配
library(fuzzyjoin)

# 将原始条目和目标条目的列名统一
names(origin) <- "trait"
names(target) <- "trait"

# 进行模糊匹配
stringdist_inner_join(origin, target, by = "trait")
# 提示如下错误：
# Error in stri_length(string) : 
# invalid UTF-8 byte sequence detected; try calling stri_enc_toutf8()

# 按照提示更改了编码再匹配一次
target_fix <- target
target_fix$trait <- stringi::stri_enc_toutf8(target$trait)
stringdist_inner_join(origin, target_fix, by = "trait")
# 结果还是出现了同样的错误提示：
# Error in stri_length(string) : 
# invalid UTF-8 byte sequence detected; try calling stri_enc_toutf8()

# 而且查查转化前后的数据，其实并没有变化
identical(target, target_fix)

# 解决方案 ----
# 思路：用adist()得到两个字符串之间的模糊相似度，然后筛选相似度较高的条目，再手动筛选

# 载入所需包
# 用于读取Excel文件
library(openxlsx)
# 用于转化成data.table格式，据说运行起来比较快
library(data.table)

# 读取原始条目
origin <- read.xlsx("Original_trait.xlsx")
# 转化成data.table格式
origin <- data.table(origin)

# 读取目标条目
target <- read.csv("Target_trait.txt", sep = "\t", header = TRUE)
# 选择目标列并重命名
target <- target[, "Trait"]
names(target) <- "target"
# 转化成data.table格式
target <- data.table(target)

# 测试相似度度量中不同参数的结果
adist("Leaf nitrogen content", 
      "Leaf nitrogen (N) content organic per leaf dry mass")
adist("Leaf nitrogen content", "Leaf water content total")
# 我希望第一条测试得到的相似度更高（即得分更小），但是目前测试结果显然不是这样
# 那么如果把“partial”参数改为“TRUE”呢？
adist("Leaf nitrogen content", 
      "Leaf nitrogen (N) content organic per leaf dry mass", 
      partial = TRUE)
adist("Leaf nitrogen content", "Leaf water content total", partial = TRUE)
# 这次得到的结果比较符合预期了，不知道其他条目如何，但暂时就把“partial”参数设置为“TRUE”进行函数构建吧

# 函数：一对多字符串相似度计算
# 输入：x，原始条目向量；y，目标条目向量；number，本次要评估的原始条目序号；threshold，范围为0-1的数字，用于筛选和原始条目字符串相似度最高的目标条目
fun_fuzzyjoin <- function(x, y, number, threshold = 0.5) {
  # 评估原始条目字符串和目标条目的相似度
  score <- adist(x[number], y, partial = TRUE)
  
  # 构建数据框：相似度得分和对应的目标条目字符创
  score_df <- data.frame(
    score = score[1, ], 
    names = y
  )
  
  # 提取符合条件的目标条目字符串
  tar_name <- subset(score_df, score < quantile(score, threshold))
  
  # 加入原始条目字符串列
  tar_name_df <- data.frame(
    tar = tar_name$names, 
    origin = x[number]
  )
  tar_name_df <- tar_name_df[, c("origin", "tar")]
  
  # 返回结果
  return(tar_name_df)
}

# 函数：进行循环并将结果合并成一个数据框
# 输入：x，原始条目向量；y，目标条目向量
fun_loopmer <- function(x, y, ...) {
  # 构建列表用于暂存每次循环的输出结果
  res_ls <- vector("list", length = length(x))
  
  # 进行循环并且存储结果
  for (i in 1:length(x)) {
    res_ls[[i]] <- fun_fuzzyjoin(x, y, i, ...)
  }
  
  # 将结果合并为数据框
  res_df <- Reduce(rbind, res_ls)
  return(res_df)
}

# 进行分析
# 这里的threshold参数选择其实很主观。我有40个原始条目字符串，之后还要手动筛选，那么平均每个原始条目字符串对应的候选目标字符串在20个左右，筛选起来比较不费劲。所以就持续调整threshold值，直到达到这个数量为止，最终选择了相似度排名前2%的目标条目。
join <- fun_loopmer(origin$origin, target$target, threshold = 0.02)
dim(join)
