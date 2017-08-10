library(xlsx)
library(KoNLP)
library(stats)

#---------------------#file input#---------------------##전체 세트
#setwd("/Users/hodong/Desktop/data_project/naver_movie/second_raw")
#gea <- data.frame(read.csv("genre_analysis.csv",header=TRUE))
#gra <- data.frame(read.csv("grade_analysis.csv",header=TRUE))
#naa <- data.frame(read.csv("nation_analysis.csv",header=TRUE))
#mp <- data.frame(read.csv("movie_prepared.csv",header=TRUE))
#dp <- data.frame(read.csv("data_prepared.csv",header=TRUE))
#---------------------#file input#---------------------#

#---------------------#file input#---------------------##훈련 세트
setwd("/Users/hodong/Desktop/data_project/naver_movie/fr25to135p20")
gea <- data.frame(read.csv("genre_analysis.csv",header=TRUE))
gra <- data.frame(read.csv("grade_analysis.csv",header=TRUE))
naa <- data.frame(read.csv("nation_analysis.csv",header=TRUE))
mp <- data.frame(read.csv("movie_prepared.csv",header=TRUE))
dp <- data.frame(read.csv("data_train.csv",header=TRUE))
#---------------------#file input#---------------------#
sqrt(length(unique(dp$user_pk)))

#---------------------#Clustering#---------------------#
size <- 15 #cluster_size
#size <- 40 #cluster_size
#---------------------#Genre Clustering#---------------------#
gen_pre <- gea[,1:2]

j <- 3
for(i in 3:(ncol(gea)/3+2)){ #선호도 지수만 모으기
  gen_pre <- data.frame(cbind(gen_pre,gea[,3+(i-3)*3]))
  names(gen_pre)[j]<-names(gea)[3+(i-3)*3]
  j <- j+1
}
gen_pre2 <- gen_pre
gen_pre[is.na(gen_pre)]<-0
gen_temp <- gen_pre[,-c(1:2)]
#View(gen_pre)

#genre_cl$size #size of each clustered group
#genre_cl$center #각 클러스터 centroid의 좌표

gen_res <- data.frame(cbind(gen_pre$user_pk,as.character(gen_pre$id)))
names(gen_res) <- c("user_pk","id")
genre_cl <- kmeans(gen_temp,size)
gen_cl_fin <- data.frame(cbind(gen_res,genre_cl$cluster))

names(gen_cl_fin)[3] <- "cluster"
gen_cl_fin$user_pk <- as.numeric(as.character((gen_cl_fin$user_pk)))
#View(gen_cl_fin) #최종 클러스터 분류
#table(gen_cl_fin$final_cluster) #cluster size

#cluster checking
#gen_cl_info <- data.frame(matrix(nrow=size,ncol=1))
#names(gen_cl_info) <- "cluster"
#for(i in 1:size){ gen_cl_info[i,1]<-i }
#j <- 2
#i <- 5
#View(gen_cl_fin)
#for(i in 4:length(names(gen_cl_fin))){
#  temp <- aggregate(data=gen_cl_fin, gen_cl_fin[,i]~cluster, mean)[,2]
#  if(length(temp)==nrow(gen_cl_info)){
#    gen_cl_info <- data.frame(cbind(gen_cl_info,aggregate(data=gen_cl_fin, gen_cl_fin[,i]~cluster, mean)[,2]))
#    names(gen_cl_info)[j] <- names(gen_cl_fin)[i]
#    j <- j+1
#  }
#}
#View(gen_cl_info)

#---------------------#Genre Clustering#---------------------#

#---------------------#Grade Clustering#---------------------#
gra_pre <- gra[,1:2]
j <- 3
for(i in 3:(ncol(gra)/3+2)){ #선호도 지수만 모으기
  gra_pre <- data.frame(cbind(gra_pre,gra[,3+(i-3)*3]))
  names(gra_pre)[j]<-names(gra)[3+(i-3)*3]
  j <- j+1
}
gra_pre2 <- gra_pre
gra_pre[is.na(gra_pre)]<-0
gra_temp <- gra_pre[,-c(1:2)]

gra_res <- data.frame(cbind(gra_pre$user_pk,as.character(gra_pre$id)))
names(gra_res) <- c("user_pk","id")
grade_cl <- kmeans(gra_temp,size)
gra_cl_fin <- data.frame(cbind(gra_res,grade_cl$cluster))
names(gra_cl_fin)[3] <- "cluster"
gra_cl_fin$user_pk <- as.numeric(as.character((gra_cl_fin$user_pk)))

#View(gra_cl_fin) #최종 클러스터 분류
#---------------------#Grade Clustering#---------------------#

#---------------------#Nation Clustering#---------------------#
nat_pre <- naa[,1:2]
j <- 3
for(i in 3:(ncol(naa)/3+2)){ #선호도 지수만 모으기
  nat_pre <- data.frame(cbind(nat_pre,naa[,3+(i-3)*3]))
  names(nat_pre)[j]<-names(naa)[3+(i-3)*3]
  j <- j+1
}
nat_pre2 <- nat_pre
nat_pre[is.na(nat_pre)]<-0
nat_temp <- nat_pre[,-c(1:2)]

nat_res <- data.frame(cbind(nat_pre$user_pk,as.character(nat_pre$id)))
names(nat_res) <- c("user_pk","id")
nation_cl <- kmeans(nat_temp,size)
nat_cl_fin <- data.frame(cbind(nat_res,nation_cl$cluster))
names(nat_cl_fin)[3] <- "cluster"
nat_cl_fin$user_pk <- as.numeric(as.character((nat_cl_fin$user_pk)))
#View(nat_cl_fin) #최종 클러스터 분류
#---------------------#Nation Clustering#---------------------#


#---------------------#Clustering#---------------------#

#---------------------#Recommendation within Cluster#---------------------#
nrow(dp)
extract_size <- 30
first_extract <- extract_size+max(dp$review_count)
#---------------------#Genre Cluster#---------------------#
#View(gen_cl_fin)
table(gen_cl_fin$cluster) #클러스터의 사이즈 확인

#gen_cl_fin$cluster
gen_cl_set <- list() #각 클러스터의 영화 중 상위 30개의 제목만 추출

for(i in 1:size){
  temp <- dp[which(dp$user_pk %in% gen_cl_fin[which(gen_cl_fin$cluster==i),]$user_pk),] #각 클러스터의 회원&영화 리뷰 데이터
  tt <- table(temp$title)[table(temp$title)>0]
  recom <- head(sort(tt,decreasing=TRUE),n=first_extract)
  #recom <- setdiff(names(tt),dp[which(dp$user_pk==identifier[i,]$user_pk),]$title)
  #recom <- names(head(sort(tt[names(tt) %in% recom],decreasing=TRUE),n=first_extract))
  gen_cl_set[[i]]<-recom
}
#Reduce(intersect, gen_cl_set) #모든 클러스터에 속한 영화 - 당시에 핫한 영화일 가능성이 높다. 

#장르 클러스터 분석
genre_k_means <- data.frame(cbind(sort(unique(dp$user_pk),decreasing=FALSE),matrix(nrow=length(unique(dp$user_pk)),ncol=(extract_size+2))))
names(genre_k_means)[1:3] <- c("user_pk","id","review_count")
genre_k_means_score <- data.frame(cbind(sort(unique(dp$user_pk),decreasing=FALSE),matrix(nrow=length(unique(dp$user_pk)),ncol=(extract_size+2))))
names(genre_k_means_score)[1:3] <- c("user_pk","id","review_count")
for(i in sort(unique(dp$user_pk),decreasing=FALSE)){
  user_cluster <- gen_cl_fin[which(gen_cl_fin$user_pk==i),]$cluster #확인할 유저의 클러스터
  #
  recom <- gen_cl_set[[user_cluster]]
  tt <- setdiff(names(recom),dp[which(dp$user_pk==i),]$title) #속한 클러스터의 top (extract_size)개의 영화 중 유저가 안 본 영화 
  recom <- recom[which(names(recom) %in% tt)]
  recom <- names(head(sort(recom,decreasing=TRUE),n=extract_size))
  #
  genre_k_means[which(genre_k_means$user_pk==i),2] <- as.character((dp[which(dp$user_pk==i),]$id)[1])
  genre_k_means[which(genre_k_means$user_pk==i),3] <- as.character((dp[which(dp$user_pk==i),]$review_count)[1])
  genre_k_means_score[which(genre_k_means_score$user_pk==i),2] <- as.character((dp[which(dp$user_pk==i),]$id)[1])
  genre_k_means_score[which(genre_k_means_score$user_pk==i),3] <- as.character((dp[which(dp$user_pk==i),]$review_count)[1])
  
  temp <- gen_cl_fin[which(gen_cl_fin$cluster==user_cluster),]$user_pk
  for(j in 1:length(recom)){
    genre_k_means[which(genre_k_means$user_pk==i),j+3] <- recom[j]
    temp_score <- mean(dp[which(dp$user_pk %in% temp & dp$title==recom[j]),]$score,na.rm=TRUE) #해당 클러스터 내 유저의 영화에 대한 평균 평점
    genre_k_means_score[which(genre_k_means_score$user_pk==i),j+3] <- ifelse(is.nan(temp_score),NA,temp_score)
  }
}
genre_k_means$review_count <- as.numeric(genre_k_means$review_count)
genre_k_means_score$review_count <- as.numeric(genre_k_means_score$review_count)
View(genre_k_means)
View(genre_k_means_score)
#---------------------#Genre Cluster#---------------------#

#---------------------#Grade Cluster#---------------------#
#View(gra_cl_fin)
table(gra_cl_fin$cluster) #클러스터의 사이즈 확인

gra_cl_set <- list() #각 클러스터의 영화 중 상위 30개의 제목만 추출
for(i in 1:size){
  temp <- dp[which(dp$user_pk %in% gra_cl_fin[which(gra_cl_fin$cluster==i),]$user_pk),] #각 클러스터의 회원&영화 리뷰 데이터
  tt <- table(temp$title)[table(temp$title)>0]
  recom <- head(sort(tt,decreasing=TRUE),n=first_extract)
  gra_cl_set[[i]]<-recom
}
#Reduce(intersect, gra_cl_set) #모든 클러스터에 속한 영화 - 당시에 핫한 영화일 가능성이 높다. 

#등급 클러스터 분석
grade_k_means <- data.frame(cbind(sort(unique(dp$user_pk),decreasing=FALSE),matrix(nrow=length(unique(dp$user_pk)),ncol=(extract_size+2))))
names(grade_k_means)[1:3] <- c("user_pk","id","review_count")
grade_k_means_score <- data.frame(cbind(sort(unique(dp$user_pk),decreasing=FALSE),matrix(nrow=length(unique(dp$user_pk)),ncol=(extract_size+2))))
names(grade_k_means_score)[1:3] <- c("user_pk","id","review_count")
for(i in sort(unique(dp$user_pk),decreasing=FALSE)){
  user_cluster <- gra_cl_fin[which(gra_cl_fin$user_pk==i),]$cluster #확인할 유저의 클러스터
  
  recom <- gra_cl_set[[user_cluster]]
  tt <- setdiff(names(recom),dp[which(dp$user_pk==i),]$title) #속한 클러스터의 top (extract_size)개의 영화 중 유저가 안 본 영화 
  recom <- recom[names(recom) %in% tt]
  recom <- names(head(sort(recom,decreasing=TRUE),n=extract_size))
  
  grade_k_means[which(grade_k_means$user_pk==i),2] <- as.character((dp[which(dp$user_pk==i),]$id)[1])
  grade_k_means[which(grade_k_means$user_pk==i),3] <- as.character((dp[which(dp$user_pk==i),]$review_count)[1])
  grade_k_means_score[which(grade_k_means_score$user_pk==i),2] <- as.character((dp[which(dp$user_pk==i),]$id)[1])
  grade_k_means_score[which(grade_k_means_score$user_pk==i),3] <- as.character((dp[which(dp$user_pk==i),]$review_count)[1])
  
  temp <- gra_cl_fin[which(gra_cl_fin$cluster==user_cluster),]$user_pk
  for(j in 1:length(recom)){
    grade_k_means[which(grade_k_means$user_pk==i),j+3] <- recom[j]
    temp_score <- mean(dp[which(dp$user_pk %in% temp & dp$title==recom[j]),]$score,na.rm=TRUE) #해당 클러스터 내 유저의 영화에 대한 평균 평점
    grade_k_means_score[which(grade_k_means_score$user_pk==i),j+3] <- ifelse(is.nan(temp_score),NA,temp_score)
  }
}
grade_k_means$review_count <- as.numeric(grade_k_means$review_count)
grade_k_means_score$review_count <- as.numeric(grade_k_means_score$review_count)
#View(grade_k_means)

#---------------------#Grade Cluster#---------------------#

#---------------------#Nation Cluster#---------------------#
#View(nat_cl_fin)
table(nat_cl_fin$cluster) #클러스터의 사이즈 확인

nat_cl_set <- list() #각 클러스터의 영화 중 상위 20개의 제목만 추출
for(i in 1:size){
  temp <- dp[which(dp$user_pk %in% nat_cl_fin[which(nat_cl_fin$cluster==i),]$user_pk),] #각 클러스터의 회원&영화 리뷰 데이터
  tt <- table(temp$title)[table(temp$title)>0]
  recom <- head(sort(tt,decreasing=TRUE),n=first_extract)
  nat_cl_set[[i]]<-recom
}#Reduce(intersect, nat_cl_set) #모든 클러스터에 속한 영화 - 당시에 핫한 영화일 가능성이 높다. 

#국가 클러스터 분석
nation_k_means <- data.frame(cbind(sort(unique(dp$user_pk),decreasing=FALSE),matrix(nrow=length(unique(dp$user_pk)),ncol=(extract_size+2))))
names(nation_k_means)[1:3] <- c("user_pk","id","review_count")
nation_k_means_score <- data.frame(cbind(sort(unique(dp$user_pk),decreasing=FALSE),matrix(nrow=length(unique(dp$user_pk)),ncol=(extract_size+2))))
names(nation_k_means_score)[1:3] <- c("user_pk","id","review_count")
for(i in sort(unique(dp$user_pk),decreasing=FALSE)){
  user_cluster <- nat_cl_fin[which(nat_cl_fin$user_pk==i),]$cluster #확인할 유저의 클러스터
  
  recom <- nat_cl_set[[user_cluster]]
  tt <- setdiff(names(recom),dp[which(dp$user_pk==i),]$title) #속한 클러스터의 top (extract_size)개의 영화 중 유저가 안 본 영화 
  recom <- recom[names(recom) %in% tt]
  recom <- names(head(sort(recom,decreasing=TRUE),n=extract_size))
  
  nation_k_means[which(nation_k_means$user_pk==i),2] <- as.character((dp[which(dp$user_pk==i),]$id)[1])
  nation_k_means[which(nation_k_means$user_pk==i),3] <- as.character((dp[which(dp$user_pk==i),]$review_count)[1])
  nation_k_means_score[which(nation_k_means_score$user_pk==i),2] <- as.character((dp[which(dp$user_pk==i),]$id)[1])
  nation_k_means_score[which(nation_k_means_score$user_pk==i),3] <- as.character((dp[which(dp$user_pk==i),]$review_count)[1])
  
  temp <- nat_cl_fin[which(nat_cl_fin$cluster==user_cluster),]$user_pk
  for(j in 1:length(recom)){
    nation_k_means[which(nation_k_means$user_pk==i),j+3] <- recom[j]
    temp_score <- mean(dp[which(dp$user_pk %in% temp & dp$title==recom[j]),]$score,na.rm=TRUE) #해당 클러스터 내 유저의 영화에 대한 평균 평점
    nation_k_means_score[which(nation_k_means_score$user_pk==i),j+3] <- ifelse(is.nan(temp_score),NA,temp_score)
  }
}
nation_k_means$review_count <- as.numeric(nation_k_means$review_count)
nation_k_means_score$review_count <- as.numeric(nation_k_means_score$review_count)
#View(nation_k_means)
#---------------------#Nation Cluster#---------------------#

#---------------------#Combining Result example#---------------------#
genres<-setdiff(gen_cl_set[[15]],dp[which(dp$user_pk==1824),]$title) #속한 클러스터의 top 20 영화 중 유저가 안 본 영화 
grares<-setdiff(gra_cl_set[[5]],dp[which(dp$user_pk==1824),]$title) #속한 클러스터의 top 20 영화 중 유저가 안 본 영화
natres<-setdiff(nat_cl_set[[11]],dp[which(dp$user_pk==1824),]$title) #속한 클러스터의 top 20 영화 중 유저가 안 본 영화
Reduce(intersect,list(genres,grares,natres)) #3가지 클러스터 결과의 교집합 출력 
Reduce(intersect,list(genres,grares)) #3가지 클러스터 결과의 교집합 출력 
#View(gea[which(gea$user_pk==1824),])


cl_result <- data.frame(cbind(gen_cl_fin,gra_cl_fin$cluster,nat_cl_fin$cluster))
names(cl_result) <- c("user_pk","id","genre_cluster","grade_cluster","nation_cluster")
#---------------------#Combining Result example#---------------------#

gen_check <- genre_k_means[is.na(genre_k_means)]
gra_check <- grade_k_means[is.na(grade_k_means)]
nat_check <- nation_k_means[is.na(nation_k_means)]
gen_check
gra_check
nat_check

#<-------------------------file output--------------------------> 
write.csv(genre_k_means, file="/Users/hodong/Desktop/genre_k_means.csv",row.names=FALSE)
write.csv(grade_k_means, file="/Users/hodong/Desktop/grade_k_means.csv",row.names=FALSE)
write.csv(nation_k_means, file="/Users/hodong/Desktop/nation_k_means.csv",row.names=FALSE)
write.csv(genre_k_means_score, file="/Users/hodong/Desktop/genre_k_means_score.csv",row.names=FALSE)
write.csv(grade_k_means_score, file="/Users/hodong/Desktop/grade_k_means_score.csv",row.names=FALSE)
write.csv(nation_k_means_score, file="/Users/hodong/Desktop/nation_k_means_score.csv",row.names=FALSE)
write.csv(cl_result, file="/Users/hodong/Desktop/k_means_result.csv",row.names=FALSE)
#<-------------------------file output--------------------------> 


#---------------------#Visullizing#---------------------#
#클러스터 파이차트
layout(matrix(c(1,2),nrow=1), widths=c(1,1))
pie(prop.table(head(cluster[[1]],n=20)))
pie(prop.table(head(cluster[[2]],n=20)))

'''
write.xlsx(genre_k_means,                # R데이터명
           file="/Users/hodong/Desktop/genre_k_means.xlsx",  # 여기서는 기존의 엑셀 파일이라고 설정함
           sheetName="new", # 기존의 엑셀 파일에 new라는 시트에 데이터를 넣음
           col.names=TRUE,  # 변수이름을 그대로 사용
           row.names=FALSE,# 행이름은 사용하지 않음
           append=FALSE)      # 기존의 엑셀 파일이 있으면 그곳에 추가해서 저장

write.xlsx(grade_k_means,                # R데이터명
           file="/Users/hodong/Desktop/grade_k_means.xlsx",  # 여기서는 기존의 엑셀 파일이라고 설정함
           sheetName="new", # 기존의 엑셀 파일에 new라는 시트에 데이터를 넣음
           col.names=TRUE,  # 변수이름을 그대로 사용
           row.names=FALSE,# 행이름은 사용하지 않음
           append=FALSE)      # 기존의 엑셀 파일이 있으면 그곳에 추가해서 저장

write.xlsx(nation_k_means,                # R데이터명
           file="/Users/hodong/Desktop/nation_k_means.xlsx",  # 여기서는 기존의 엑셀 파일이라고 설정함
           sheetName="new", # 기존의 엑셀 파일에 new라는 시트에 데이터를 넣음
           col.names=TRUE,  # 변수이름을 그대로 사용
           row.names=FALSE,# 행이름은 사용하지 않음
           append=FALSE)      # 기존의 엑셀 파일이 있으면 그곳에 추가해서 저장
'''