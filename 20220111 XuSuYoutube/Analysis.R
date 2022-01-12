# 读取Youtube播放量靠前的视频信息
youtube_input <- 
  scan("Youtube_res.txt", what = character(0), sep = "\n", encoding = "UTF-8")
# 留下需要的行
youtube_input <- youtube_input[grepl("万次观看", youtube_input)]
# 提取播放量信息
youtube_times <- gsub("[^0-9]", "", youtube_input)
youtube_times <- as.numeric(youtube_times)
# 统计播放总数
cat("超过万次的视频条数为：", length(youtube_times), 
    "这些视频播放次数总计：", sum(youtube_times))
