library(openxlsx)
library(reshape2)
library(showtext)
showtext_auto()
library(ggplot2)

# 函数：读取并处理每个工作表
# 输入：工作表名称
# 输出：两列数据框，包含消费项目和分类
fun_get_train_data <- function(x) {
  # 读取所要求的工作表数据
  sht_data <- read.xlsx("History_bill.xlsx", sheet = c(x), startRow = 2)
  # 转化成所需数据类型
  sht_data <- sht_data[c("-", "饮食", "交通", "固定资产", "其他消费")]
  sht_data_lng <- melt(sht_data, id = "-")
  sht_data_lng <- sht_data_lng[which(is.na(sht_data_lng$value) == FALSE), ]
  # 输出所需数据
  sht_data_out <- sht_data_lng[c("-", "variable")]
  # 为了使用“Test.R”中写好的代码，命名为“product”和“class”
  names(sht_data_out) <- c("product", "class")
  return(sht_data_out)
}

# 生成训练数据集和测试数据集
mydata <- 
  lapply(grep("^2021", getSheetNames("History_bill.xlsx"), value = TRUE), 
         fun_get_train_data)
mydata <- Reduce(rbind, mydata)

# 生成文档-词项矩阵
doc_matrix<-
  corp_or_dtm(mydata$product, from = "v", type = "d")

# 创建container，这个数据中会将输入数据划分成训练数据集和测试数据集
container<-
  create_container(doc_matrix,
                   mydata$class, trainSize = 1: 568,
                   testSize = 501: 568, 
                   virgin = FALSE)

# 通过支持向量机构建训练文本分类模型
SVM <- train_model(container,"SVM")

# 使用训练好的文本分类模型进行文本分类
SVM_classify <- classify_model(container, SVM)
# 查看下输出结果是什么样
str(SVM_classify)

# 测试分类结果的准确性
# 应用create_analytics()函数出问题，就直接把自动分类结果合并到
mydata_test <- mydata[501: 568, ]
mydata_test <- cbind(mydata_test, SVM_classify)
# 看看预测结果是否一致
mydata_test$correct <- mydata_test$class == mydata_test$SVM_LABEL
# 计算分类准确率
sum(mydata_test$correct) / nrow(mydata_test)
# 查看分类正误是如何分布的：纵轴为手工分类，横轴为自动分类
mydata_test_analytics <- mydata_test
mydata_test_analytics$assis <- 1
mydata_test_analytics <- 
  dcast(class ~ SVM_LABEL, data = mydata_test_analytics)
# 查看结果
View(mydata_test_analytics)
# 可视化
mydata_test_analytics <- melt(mydata_test_analytics, id = "class")
ggplot(mydata_test_analytics) + 
  geom_tile(aes(x = class, y = variable, fill = value))
# 可见对饮食的判断比较准确，其他消费也还可以
