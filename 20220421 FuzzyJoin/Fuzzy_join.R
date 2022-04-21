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
c <- data.table(sqldf("select * 
           from origin a inner join target b
                      on a.origin like '%' || b.target || '%'", drv = "SQLite"))
# 查看结果
c
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

