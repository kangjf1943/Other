# 载入所需的R包
library(RTextTools)

# 读取我问题中提到的产品分类训练数据和待分类数据
# 这里的训练数据集只是个示例，手动编写，体量较小，产品名称可能也比较单调…
mydata_train <- data.frame(
  product = c("湖南大米", "上海大米5千克", "干豆", "蔬菜1千克", "水果外卖", 
              "奶", "蒙牛酸奶", "不二家棒棒糖", "杂粮饼", "麦片", "燕麦", 
              "面包", "玉米饼", "奥利奥饼干", "硬面包圈", "奥利奥餐包", 
              "豌豆2斤", "小扁豆", "如土豆", "玉米", "蒙牛牛奶", "伊利牛奶1升", 
              "地铁", "长青路到上海路公交", "旅行飞机票", "北京到上海航空", 
              "上海到四川火车", "回家地铁"), 
  class = c(rep("饮食", 22), rep("交通", 6))
)
mydata_test <- data.frame(
  product = c("福建大米", "蔬菜豆子", "奶制品", "上海郊区地铁")
)
# 但是因为我不知道如何处理这些数据，所以可以暂时放一边，先看看别人是如何处理英文文本分类问题的

# 导入英文文本方案的示例数据
# 这个数据和我问题中的“产品分类”数据可能差异较大，是一个美国议会议案的分类数据
data(USCongress)
example_data <- USCongress[c("text", "major")]
# 这个数据的“text”是议案内容，对标我问题数据中的产品，而“major”是对应的分类，对标我问题数据中的产品分类

# 看看数据长啥样
str(example_data)
# 看看类别个数
length(unique(example_data$major))

# 创建一个文档-词项矩阵
doc_matrix<-
  create_matrix(example_data$text, language ="english", removeNumbers= TRUE,
                stemWords = TRUE, removeSparseTerms = .998)
# 查看数据类型
class(doc_matrix)
# 查看数据中的各元素
View(doc_matrix)
# 我的问题就在这里：一般文档-词项矩阵是一个行列分别为文档和所包含词语，内含各词语在各文档中分布情况数值的矩阵。但是这里生成的这个doc_matrix是一个被称为Simple Triplet Matrix的类似列表的数据类型，这是稀疏矩阵的一种，这个数据中各元素的定义可以查看slam::Simple Triplet Matrix()的帮助文档。我的问题就是，不知道如何把一个普通的文档-词项矩阵转化成这种数据结构？

# 不管怎样，接着往下走
# 接下来创建一个在RTextTools中被称为container的数据，这个数据中会将输入数据划分成训练数据集和测试数据集
# 比如我们输入上一步生成的文档-词项矩阵，将前面4000条设置成训练数据集，后面的都设置成测试数据：
container<-
  create_container(doc_matrix,example_data$major,trainSize = 1:4000,
                   testSize = 4001:4449,virgin = FALSE)

# 通过支持向量机构建训练文本分类模型
SVM <- train_model(container,"SVM")

# 使用训练好的文本分类模型进行文本分类
SVM_classify <- classify_model(container, SVM)
# 查看下输出结果是什么样
str(SVM_classify)
# 这个结果共有449行，对应上面container数据中的449条测试数据，两列分别是“SVM_LABEL”是判断该条数据所属类别，而“SVM_RPOB”应该是判断正确的概率

# 测试分类结果的准确性
analytics <- create_analytics(container, SVM_classify)
# 这个类似列表的结果中，document_summary这个元素展示了测试结果是否准确，查看一下，注意这里选择数据中的特定元素要用@而不是$：
head(analytics@document_summary)
# 稍微检测一下，数据框中“CONSENSUS_INCORRECT”这个变量等于1的地方应该就是模型分类判断错误的地方
# 所以在测试的449条中：
table(analytics@document_summary$CONSENSUS_INCORRECT)
# 有333条分类是正确的，正确率为74%
summary(analytics)

# 对问题数据采取类似的操作
# 输入生成中文文档-词项矩阵所需的包
library(chinese.misc)

# 手动给测试数据添加标签，然后合并训练数据和测试数据
mydata_test$class <- c("饮食", "饮食", "饮食", "交通")
mydata <- rbind(mydata_train, mydata_test)

# 生成文档-词项矩阵
doc_matrix<-
  corp_or_dtm(mydata$product, from = "v", type = "d")
# 查看数据类型
class(doc_matrix)

# 创建container，这个数据中会将输入数据划分成训练数据集和测试数据集
container<-
  create_container(doc_matrix,mydata$class,trainSize = 1:28,
                   testSize = 29:32,virgin = FALSE)

# 通过支持向量机构建训练文本分类模型
SVM <- train_model(container,"SVM")

# 使用训练好的文本分类模型进行文本分类
SVM_classify <- classify_model(container, SVM)
# 查看下输出结果是什么样
str(SVM_classify)

# 测试分类结果的准确性
# 应用create_analytics()函数出问题，就直接把自动分类结果合并到
mydata_test <- cbind(mydata_test, SVM_classify)
# 看看预测结果是否一致
mydata_test$correct <- mydata_test$class == mydata_test$SVM_LABEL
# 可见所有结果都是一致的
mydata_test
