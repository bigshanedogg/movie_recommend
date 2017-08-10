library(xlsx)
library(KoNLP)
library(stats)
library(proxy)

#---------------------#file input#---------------------##훈련세트
setwd("/Users/hodong/Desktop/data_project/naver_movie/fr25to135p20")
gea <- data.frame(read.csv("genre_analysis.csv",header=TRUE))
gra <- data.frame(read.csv("grade_analysis.csv",header=TRUE))
naa <- data.frame(read.csv("nation_analysis.csv",header=TRUE))
mp <- data.frame(read.csv("movie_prepared.csv",header=TRUE))
dp <- data.frame(read.csv("data_train.csv",header=TRUE))
#---------------------#file input#---------------------#

#---------------------#Collaborative Filtering#---------------------#
dist_size <- 100
extract_size <- 30
first_extract <- extract_size+max(dp$review_count)

gea[is.na(gea)] <- 0
gra[is.na(gra)] <- 0
naa[is.na(naa)] <- 0

#---------------------#CF by all#---------------------#
full_table <- cbind(gea[,-grep("count",names(gea))],gra[,-c(1:2,grep("count",names(gra)))],naa[,-c(1:2,grep("count",names(naa)))])
identifier <- full_table[,c(1:2)]
contents <- full_table[,-c(1:2)]
#View(contents)
all_cos <- as.matrix(dist(contents, method="cosine"))
all_euc <- as.matrix(dist(contents, method="Euclidean"))

all_cf <- data.frame(cbind(sort(unique(dp$user_pk),decreasing=FALSE),matrix(nrow=length(unique(dp$user_pk)),ncol=(extract_size+2))))
names(all_cf)[1:3] <- c("user_pk","id","review_count")
all_cf_score <- data.frame(cbind(sort(unique(dp$user_pk),decreasing=FALSE),matrix(nrow=length(unique(dp$user_pk)),ncol=(extract_size+2))))
names(all_cf_score)[1:3] <- c("user_pk","id","review_count")

for(i in 1:nrow(contents)){
  all_cf[which(all_cf$user_pk==identifier[i,]$user_pk),2] <- as.character((dp[which(dp$user_pk==identifier[i,]$user_pk),]$id)[1])
  all_cf[which(all_cf$user_pk==identifier[i,]$user_pk),3] <- as.character((dp[which(dp$user_pk==identifier[i,]$user_pk),]$review_count)[1])
  all_cf_score[which(all_cf_score$user_pk==identifier[i,]$user_pk),2] <- as.character((dp[which(dp$user_pk==identifier[i,]$user_pk),]$id)[1])
  all_cf_score[which(all_cf_score$user_pk==identifier[i,]$user_pk),3] <- as.character((dp[which(dp$user_pk==identifier[i,]$user_pk),]$review_count)[1])
    
  cos<-as.numeric(names(head(sort(all_cos[i,],decreasing=FALSE),n=dist_size)))
  euc<-as.numeric(names(head(sort(all_euc[i,],decreasing=FALSE),n=dist_size)))
  euc <- setdiff(euc,i)
  dist_vec <- head(intersect(cos,euc),n=31)
  
  if(!(length(dist_vec)>0)){
    dist_vec <- head(euc,n=31)
  }
  
  temp <- dp[which(dp$user_pk %in% full_table[dist_vec,]$user_pk),] #각 클러스터의 회원&영화 리뷰 데이터
  tt <- table(temp$title)[table(temp$title)>0]
  recom <- setdiff(names(tt),dp[which(dp$user_pk==identifier[i,]$user_pk),]$title)
  recom <- names(head(sort(tt[names(tt) %in% recom],decreasing=TRUE),n=extract_size))
  
  if(length(recom)>0){
    for(j in 1:length(recom)){
      all_cf[which(all_cf$user_pk==identifier[i,]$user_pk),j+3] <- recom[j]
      temp_score <- mean(dp[which(dp$user_pk %in% unique(temp$user_pk) & dp$title==recom[j]),]$score) #해당 클러스터 내 유저의 영화에 대한 평균 평점
      all_cf_score[which(all_cf_score$user_pk==identifier[i,]$user_pk),j+3] <- ifelse(is.nan(temp_score),NA,temp_score)
    }
  }
}
View(all_cf)
View(all_cf_score)
#---------------------#CF by all#---------------------#

#---------------------#CF by genre#---------------------#
full_table <- gea[,-grep("count",names(gea))]
identifier <- full_table[,c(1:2)]
contents <- full_table[,-c(1:2)]
gen_cos <- as.matrix(dist(contents, method="cosine"))
gen_euc <- as.matrix(dist(contents, method="Euclidean"))

genre_cf <- data.frame(cbind(sort(unique(dp$user_pk),decreasing=FALSE),matrix(nrow=length(unique(dp$user_pk)),ncol=(extract_size+2))))
names(genre_cf)[1:3] <- c("user_pk","id","review_count")
genre_cf_score <- data.frame(cbind(sort(unique(dp$user_pk),decreasing=FALSE),matrix(nrow=length(unique(dp$user_pk)),ncol=(extract_size+2))))
names(genre_cf_score)[1:3] <- c("user_pk","id","review_count")

for(i in 1:nrow(contents)){
  genre_cf[which(genre_cf$user_pk==identifier[i,]$user_pk),2] <- as.character((dp[which(dp$user_pk==identifier[i,]$user_pk),]$id)[1])
  genre_cf[which(genre_cf$user_pk==identifier[i,]$user_pk),3] <- as.character((dp[which(dp$user_pk==identifier[i,]$user_pk),]$review_count)[1])
  genre_cf_score[which(genre_cf_score$user_pk==identifier[i,]$user_pk),2] <- as.character((dp[which(dp$user_pk==identifier[i,]$user_pk),]$id)[1])
  genre_cf_score[which(genre_cf_score$user_pk==identifier[i,]$user_pk),3] <- as.character((dp[which(dp$user_pk==identifier[i,]$user_pk),]$review_count)[1])
  
  cos<-as.numeric(names(head(sort(gen_cos[i,],decreasing=FALSE),n=dist_size)))
  euc<-as.numeric(names(head(sort(gen_euc[i,],decreasing=FALSE),n=dist_size)))
  euc <- setdiff(euc,i)
  dist_vec <- head(intersect(cos,euc),n=31)
  
  if(!(length(dist_vec)>0)){
    dist_vec <- head(euc,n=31)
  }
  
  temp <- dp[which(dp$user_pk %in% full_table[dist_vec,]$user_pk),] #각 클러스터의 회원&영화 리뷰 데이터
  tt <- table(temp$title)[table(temp$title)>0]
  recom <- setdiff(names(tt),dp[which(dp$user_pk==identifier[i,]$user_pk),]$title)
  recom <- names(head(sort(tt[names(tt) %in% recom],decreasing=TRUE),n=extract_size))
  
  if(length(recom)>0){
    for(j in 1:length(recom)){
      genre_cf[which(genre_cf$user_pk==identifier[i,]$user_pk),j+3] <- recom[j]
      temp_score <- mean(dp[which(dp$user_pk %in% unique(temp$user_pk) & dp$title==recom[j]),]$score) #해당 클러스터 내 유저의 영화에 대한 평균 평점
      genre_cf_score[which(genre_cf_score$user_pk==identifier[i,]$user_pk),j+3] <- temp_score
    }
  }
}
View(genre_cf)
#write.csv(genre_cf, file="/Users/hodong/Desktop/genre_cf.csv",row.names=FALSE)
#---------------------#CF by genre#---------------------#

#---------------------#CF by grade#---------------------#
full_table <- gra[,-grep("count",names(gra))]
identifier <- full_table[,c(1:2)]
contents <- full_table[,-c(1:2)]
gra_cos <- as.matrix(dist(contents, method="cosine"))
gra_euc <- as.matrix(dist(contents, method="Euclidean"))

grade_cf <- data.frame(cbind(sort(unique(dp$user_pk),decreasing=FALSE),matrix(nrow=length(unique(dp$user_pk)),ncol=(extract_size+2))))
names(grade_cf)[1:3] <- c("user_pk","id","review_count")
grade_cf_score <- data.frame(cbind(sort(unique(dp$user_pk),decreasing=FALSE),matrix(nrow=length(unique(dp$user_pk)),ncol=(extract_size+2))))
names(grade_cf_score)[1:3] <- c("user_pk","id","review_count")

for(i in 1:nrow(contents)){
  grade_cf[which(grade_cf$user_pk==identifier[i,]$user_pk),2] <- as.character((dp[which(dp$user_pk==identifier[i,]$user_pk),]$id)[1])
  grade_cf[which(grade_cf$user_pk==identifier[i,]$user_pk),3] <- as.character((dp[which(dp$user_pk==identifier[i,]$user_pk),]$review_count)[1])
  grade_cf_score[which(grade_cf_score$user_pk==identifier[i,]$user_pk),2] <- as.character((dp[which(dp$user_pk==identifier[i,]$user_pk),]$id)[1])
  grade_cf_score[which(grade_cf_score$user_pk==identifier[i,]$user_pk),3] <- as.character((dp[which(dp$user_pk==identifier[i,]$user_pk),]$review_count)[1])
  
  cos<-as.numeric(names(head(sort(gra_cos[i,],decreasing=FALSE),n=dist_size)))
  euc<-as.numeric(names(head(sort(gra_euc[i,],decreasing=FALSE),n=dist_size)))
  euc <- setdiff(euc,i)
  dist_vec <- head(intersect(cos,euc),n=31)
  
  if(!(length(dist_vec)>0)){
    dist_vec <- head(euc,n=31)
  }
  
  temp <- dp[which(dp$user_pk %in% full_table[dist_vec,]$user_pk),] #각 클러스터의 회원&영화 리뷰 데이터
  tt <- table(temp$title)[table(temp$title)>0]
  recom <- setdiff(names(tt),dp[which(dp$user_pk==identifier[i,]$user_pk),]$title)
  recom <- names(head(sort(tt[names(tt) %in% recom],decreasing=TRUE),n=extract_size))
  
  if(length(recom)>0){
    for(j in 1:length(recom)){
      grade_cf[which(grade_cf$user_pk==identifier[i,]$user_pk),j+3] <- recom[j]
      temp_score <- mean(dp[which(dp$user_pk %in% unique(temp$user_pk) & dp$title==recom[j]),]$score) #해당 클러스터 내 유저의 영화에 대한 평균 평점
      grade_cf_score[which(grade_cf_score$user_pk==identifier[i,]$user_pk),j+3] <- temp_score
    }
  }
}
View(grade_cf)
#---------------------#CF by grade#---------------------#

#---------------------#CF by nation#---------------------#
full_table <- naa[,-grep("count",names(naa))]
identifier <- full_table[,c(1:2)]
contents <- full_table[,-c(1:2)]
nat_cos <- as.matrix(dist(contents, method="cosine"))
nat_euc <- as.matrix(dist(contents, method="Euclidean"))

nation_cf <- data.frame(cbind(sort(unique(dp$user_pk),decreasing=FALSE),matrix(nrow=length(unique(dp$user_pk)),ncol=(extract_size+2))))
names(nation_cf)[1:3] <- c("user_pk","id","review_count")
nation_cf_score <- data.frame(cbind(sort(unique(dp$user_pk),decreasing=FALSE),matrix(nrow=length(unique(dp$user_pk)),ncol=(extract_size+2))))
names(nation_cf_score)[1:3] <- c("user_pk","id","review_count")

for(i in 1:nrow(contents)){
  nation_cf[which(nation_cf$user_pk==identifier[i,]$user_pk),2] <- as.character((dp[which(dp$user_pk==identifier[i,]$user_pk),]$id)[1])
  nation_cf[which(nation_cf$user_pk==identifier[i,]$user_pk),3] <- as.character((dp[which(dp$user_pk==identifier[i,]$user_pk),]$review_count)[1])
  nation_cf_score[which(nation_cf_score$user_pk==identifier[i,]$user_pk),2] <- as.character((dp[which(dp$user_pk==identifier[i,]$user_pk),]$id)[1])
  nation_cf_score[which(nation_cf_score$user_pk==identifier[i,]$user_pk),3] <- as.character((dp[which(dp$user_pk==identifier[i,]$user_pk),]$review_count)[1])
  
  cos<-as.numeric(names(head(sort(nat_cos[i,],decreasing=FALSE),n=dist_size)))
  euc<-as.numeric(names(head(sort(nat_euc[i,],decreasing=FALSE),n=dist_size)))
  euc <- setdiff(euc,i)
  dist_vec <- head(intersect(cos,euc),n=31)
  
  if(!(length(dist_vec)>0)){
    dist_vec <- head(euc,n=31)
  }
  
  temp <- dp[which(dp$user_pk %in% full_table[dist_vec,]$user_pk),] #각 클러스터의 회원&영화 리뷰 데이터
  tt <- table(temp$title)[table(temp$title)>0]
  recom <- setdiff(names(tt),dp[which(dp$user_pk==identifier[i,]$user_pk),]$title)
  recom <- names(head(sort(tt[names(tt) %in% recom],decreasing=TRUE),n=extract_size))
  
  if(length(recom)>0){
    for(j in 1:length(recom)){
      nation_cf[which(nation_cf$user_pk==identifier[i,]$user_pk),j+3] <- recom[j]
      temp_score <- mean(dp[which(dp$user_pk %in% unique(temp$user_pk) & dp$title==recom[j]),]$score) #해당 클러스터 내 유저의 영화에 대한 평균 평점
      nation_cf_score[which(nation_cf_score$user_pk==identifier[i,]$user_pk),j+3] <- temp_score
    }
  }
}
View(nation_cf)
#---------------------#CF by nation#---------------------#
#---------------------#Collaborative Filtering#---------------------#


#<-------------------------file output--------------------------> 
write.csv(all_cf, file="/Users/hodong/Desktop/all_cf.csv",row.names=FALSE)
write.csv(genre_cf, file="/Users/hodong/Desktop/genre_cf.csv",row.names=FALSE)
write.csv(grade_cf, file="/Users/hodong/Desktop/grade_cf.csv",row.names=FALSE)
write.csv(nation_cf, file="/Users/hodong/Desktop/nation_cf.csv",row.names=FALSE)
write.csv(all_cf_score, file="/Users/hodong/Desktop/all_cf_score.csv",row.names=FALSE)
write.csv(genre_cf_score, file="/Users/hodong/Desktop/genre_cf_score.csv",row.names=FALSE)
write.csv(grade_cf_score, file="/Users/hodong/Desktop/grade_cf_score.csv",row.names=FALSE)
write.csv(nation_cf_score, file="/Users/hodong/Desktop/nation_cf_score.csv",row.names=FALSE)
#<-------------------------file output--------------------------> 

'''
write.xlsx(genre_cf,                # R데이터명
           file="/Users/hodong/Desktop/genre_cf.xlsx",  # 여기서는 기존의 엑셀 파일이라고 설정함
           sheetName="new", # 기존의 엑셀 파일에 new라는 시트에 데이터를 넣음
           col.names=TRUE,  # 변수이름을 그대로 사용
           row.names=FALSE,# 행이름은 사용하지 않음
           append=FALSE)      # 기존의 엑셀 파일이 있으면 그곳에 추가해서 저장

write.xlsx(grade_cf,                # R데이터명
           file="/Users/hodong/Desktop/grade_cf.xlsx",  # 여기서는 기존의 엑셀 파일이라고 설정함
           sheetName="new", # 기존의 엑셀 파일에 new라는 시트에 데이터를 넣음
           col.names=TRUE,  # 변수이름을 그대로 사용
           row.names=FALSE,# 행이름은 사용하지 않음
           append=FALSE)      # 기존의 엑셀 파일이 있으면 그곳에 추가해서 저장

write.xlsx(nation_cf,                # R데이터명
           file="/Users/hodong/Desktop/nation_cf.xlsx",  # 여기서는 기존의 엑셀 파일이라고 설정함
           sheetName="new", # 기존의 엑셀 파일에 new라는 시트에 데이터를 넣음
           col.names=TRUE,  # 변수이름을 그대로 사용
           row.names=FALSE,# 행이름은 사용하지 않음
           append=FALSE)      # 기존의 엑셀 파일이 있으면 그곳에 추가해서 저장

write.xlsx(all_cf,                # R데이터명
           file="/Users/hodong/Desktop/all_cf.xlsx",  # 여기서는 기존의 엑셀 파일이라고 설정함
           sheetName="new", # 기존의 엑셀 파일에 new라는 시트에 데이터를 넣음
           col.names=TRUE,  # 변수이름을 그대로 사용
           row.names=FALSE,# 행이름은 사용하지 않음
           append=FALSE)      # 기존의 엑셀 파일이 있으면 그곳에 추가해서 저장
'''



