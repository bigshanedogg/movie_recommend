library(xlsx)
library(KoNLP)
library(stats)

#---------------------#file input#---------------------##전체 세트
#setwd("/Users/hodong/Desktop/data_project/naver_movie/second_raw")
#mp <- data.frame(read.csv("movie_prepared.csv",header=TRUE))
#dp <- data.frame(read.csv("data_prepared.csv",header=TRUE))
#---------------------#file input#---------------------#

extract_size <- 30
#---------------------#file input#---------------------##훈련 세트
setwd("/Users/hodong/Desktop/data_project/naver_movie/fr25to135p20")
mp <- data.frame(read.csv("movie_prepared.csv",header=TRUE))
dp <- data.frame(read.csv("data_train.csv",header=TRUE))
#---------------------#file input#---------------------#


#---------------------#preparing#---------------------#
ar_table <- data.frame(cbind(unique(dp[with(dp,order(user_pk)),1:3]),matrix(nrow=length(unique(dp$user_pk)),ncol=max(dp$review_count))))
names(ar_table)[1:3] <- c("user_pk","id","review_count")

for(i in 1:nrow(ar_trans)){
  temp <- 0
  temp <- ifelse(as.character(mp$title) %in% dp[which(dp$user_pk==ar_table[i,]$user_pk),]$title,1,0)
  temp_t <- unique(as.vector(t(matrix(as.character(mp$title)[(which(temp==1))]))))
  if(length(temp_t)){
    ar_table[i,4:(length(temp_t)+3)] <- temp_t
  }
}
View(ar_table)

ar_trans <- ar_table[,-c(1:3)]
View(ar_trans)
for(i in 1:ncol(ar_trans)){
  if(sum(is.na(ar_trans[,i]))==nrow(ar_trans)){break}
}
ar_trans <- ar_trans[,-c(108:ncol(ar_trans))]
View(ar_trans)
#View(ar_trans)
#View(ar_trans)
write.xlsx(ar_trans,                # R데이터명
           file="/Users/hodong/Desktop/movie_transactions.xlsx",  # 여기서는 기존의 엑셀 파일이라고 설정함
           sheetName="new", # 기존의 엑셀 파일에 new라는 시트에 데이터를 넣음
           col.names=TRUE,  # 변수이름을 그대로 사용
           row.names=FALSE,# 행이름은 사용하지 않음
           append=FALSE)      # 기존의 엑셀 파일이 있으면 그곳에 추가해서 저장

#write.csv(ar_trans, file="/Users/hodong/Desktop/movie_transactions.csv",row.names=FALSE)
#---------------------#preparing#---------------------#

#---------------------#Association Rule#---------------------#
#install.packages("arules")
library(arules)
trans <- read.transactions("movie_transactions.csv",sep=",",quote=NULL)
trans <- trans[-1]
#---------------------#Checking transaction data#---------------------#
summary(trans)
inspect(trans[1:5]) #showing items of each instances in set data structure of python
itemFrequency(trans[,1:5]) #columns of sparse matrix shows alphabetically-ordered items
#---------------------#Checking transaction data#---------------------#

#---------------------#General Rules#---------------------#

#---------------------#Making each user's Rules#---------------------#
'%!in%' <- function(x,y)!('%in%'(x,y))
temp_rule <- apriori(trans, parameter=list(support=0.01,confidence=0.4,minlen=2))
temp_rule
inspect(temp_rule)
summary(temp_rule)
itemLabels(temp_rule)

score_avg <- mean(dp$score)
count_avg <- mean(dp$review_count)
tt <- aggregate(data=dp,score~title,mean)
tt3<-aggregate(data=dp,review_count~title,length)
mv_list <- data.frame(cbind(tt,tt3$review_count))
names(mv_list) <- c("title","score","review_count")

ar_result <- data.frame(cbind(sort(unique(dp$user_pk),decreasing=FALSE),matrix(nrow=length(unique(dp$user_pk)),ncol=(extract_size+2))))
names(ar_result)[1:3] <- c("user_pk","id","review_count")
for(i in sort(unique(dp$user_pk),decreasing=FALSE)){
  ar_result[which(ar_result$user_pk==i),2] <- as.character((dp[which(dp$user_pk==i),]$id)[1])
  ar_result[which(ar_result$user_pk==i),3] <- as.character((dp[which(dp$user_pk==i),]$review_count)[1])
  user_movie <- unique(as.vector(dp[which(dp$user_pk==i),]$title))
  user_movie<-user_movie[user_movie %in% itemLabels(temp_rule)]
  
  user_rules <- sort(subset(temp_rule, lhs %in% user_movie & rhs %!in% user_movie),by="lift")
  tttt <- unique(as.vector(inspect(user_rules)$rhs))
  tttt <- gsub("[\\{\\}]","",tttt)
  #rem <- vector()
  #if(length(tttt)>0){
    #for(j in 1:length(tttt)){
    #  if(!((mv_list[which(mv_list$title==tttt[j]),]$score>=score_avg) & (mv_list[which(mv_list$title==tttt[j]),]$review_count>=count_avg/2))){
    #    rem <- Reduce(c,list(rem,j))
    #  }  
    #}
    #tttt <- tttt[-rem]
  #}
  if(length(tttt)>0){
    for(j in 1:length(tttt)){
      ar_result[which(ar_result$user_pk==i),j+3] <- tttt[j]
    }
  }
}
View(ar_result)

#---------------------#Making each user's Rules#---------------------#
#---------------------#Association Rule#---------------------#

#<-------------------------file output--------------------------> 
write.csv(inspect(temp_rule), file="/Users/hodong/Desktop/general_movie_ar.csv",row.names=FALSE)
write.csv(ar_result, file="/Users/hodong/Desktop/user_ar_result.csv",row.names=FALSE)
#write(temp_rule, file="/Users/hodong/Desktop/general_movie_ar.csv", sep=",",quote=TRUE,row.names=FALSE)
#<-------------------------file output--------------------------> 

'''
write.xlsx(ar_result,                # R데이터명
           file="/Users/hodong/Desktop/user_ar_result.xlsx",  # 여기서는 기존의 엑셀 파일이라고 설정함
           sheetName="new", # 기존의 엑셀 파일에 new라는 시트에 데이터를 넣음
           col.names=TRUE,  # 변수이름을 그대로 사용
           row.names=FALSE,# 행이름은 사용하지 않음
           append=FALSE)      # 기존의 엑셀 파일이 있으면 그곳에 추가해서 저장
'''
