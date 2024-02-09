# hw2
# 使用R中的 数据读写，文件路径，for循环语句，
# 读入路径“/assignment_idaccuracy/Aminer”中的所有文件
# 并将数据合并成为一个data.frame输出
getwd()
setwd(dir = "/Users/wangdian/Downloads/Study/2024_BigData史东波/data/assignment_idaccuracy/Aminer")
# 1. 要求data.frame中至少要包括论文的doi号，发表年份，杂志，标题；
dir = list.files()
# dir <- paste("./",a,sep = "")
n = length(dir)
merge.data <- read.csv(file = dir[1], head=T, sep = ",")
for (i in 2:n) {
  new.data = read.csv(file = dir[i], header = T, sep = ",")
  merge.data = rbind(merge.data,new.data)
}
head(merge.data)

write.csv(merge.data,file = "/Users/wangdian/Downloads/Study/2024_BigData史东波/data/assignment_idaccuracy/merge_all.csv",row.names=FALSE)  

# 2. 使用apply家族函数替代上述步骤中的for循环
# 将这些数据框进行行合并，并将结果存储在result2中
data_list <- lapply(dir, read.csv)
result2 <- do.call(rbind, data_list)
head(result2)
# 3. 将2中代码封装成为一个可以在命令行运行的脚本
# 脚本的唯一一个参数为aminer论文文件所在的路径。
a = getwd()

rcsv_append <- function(a){
  setwd(a)
  dir = list.files()
  data_list <- lapply(dir, read.csv)
  result2 <- do.call(rbind, data_list)
  return(result2)
}

result2 <- rcsv_append(a)
head(result2)
# hw4
library(readr)
library(stringr)
library(dplyr)
scientist_pub <- read_csv("~/Downloads/Study/2024_BigData史东波/data/assignment_idaccuracy/scientist_pub.csv")
files <- list.files(path = "/Users/wangdian/Downloads/Study/2024_BigData史东波/data/assignment_idaccuracy/Aminer", 
                    pattern = "\\.csv$", full.names = TRUE)

cal <- function(file_path){
  aminer_data <- read_csv(file_path)
  
  uniqueid <- str_extract(file_path, "0_[0-9]+")
  
  scientist_pub_filter <- filter(scientist_pub, uniqueID == uniqueid)
  
  scientist_pub_filter$doi <- toupper(scientist_pub_filter$doi)
  scientist_pub_filter$title <- toupper(scientist_pub_filter$title)
  scientist_pub_filter$journal <- toupper(scientist_pub_filter$journal)
  
  aminer_data$doi <- toupper(aminer_data$doi)
  aminer_data$标题 <- toupper(aminer_data$标题)
  aminer_data$期刊 <- toupper(aminer_data$期刊)
  
  match <- inner_join(aminer_data, scientist_pub_filter, by = c("doi"="doi","标题"="title","期刊"="journal","年份"="pub_year"))
  # 计算每一个作者ID的精准度和查全率
  jzd <- nrow(match) / nrow(aminer_data)
  cql <- nrow(match) / nrow(scientist_pub_filter)
  return(data.frame(file_name = basename(file_path), uniqueID, jzd, cql))
}

results <- lapply(files, cal)
# 合并结果
final_results <- bind_rows(results)

# 保存结果到文件
write_csv(final_results, "/Users/wangdian/Downloads/Study/2024_BigData史东波/data/assignment_idaccuracy/accuracy_recall_results.csv")

# 计算整体准确率和召回率
mean_jzd <- mean(final_results$jzd)
mean_cql <- mean(final_results$cql)

# 打印整体准确率和召回率
print(paste("Overall Precision: ", mean_jzd))
print(paste("Overall Recall: ", mean_cql))

