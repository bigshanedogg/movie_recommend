library(xlsx)
library(KoNLP)

#---------------------#file input#---------------------##전체 세트
#setwd("/Users/hodong/Desktop/data_project/naver_movie/second_raw")
#nat <- data.frame(read.csv("nation_table.csv",header=TRUE))
#grt <- data.frame(read.csv("grade_table.csv",header=TRUE))
#get <- data.frame(read.csv("genre_table.csv",header=TRUE))
#mp <- data.frame(read.csv("movie_prepared.csv",header=TRUE))
#dp <- data.frame(read.csv("data_prepared.csv",header=TRUE))
#---------------------#file input#---------------------#

#---------------------#file input#---------------------##훈련 세트
setwd("/Users/hodong/Desktop/data_project/naver_movie/fr25to135p20")
nat <- data.frame(read.csv("nation_table.csv",header=TRUE))
grt <- data.frame(read.csv("grade_table.csv",header=TRUE))
get <- data.frame(read.csv("genre_table.csv",header=TRUE))
mp <- data.frame(read.csv("movie_prepared.csv",header=TRUE))
dp <- data.frame(read.csv("data_train.csv",header=TRUE))
#---------------------#file input#---------------------#

#---------------------#관객/네티즌/비평가 평점에 따른 영화 정렬#---------------------#
mp_by_auds<-mp[order(mp$aud_score, mp$net_score, mp$cri_score, decreasing=TRUE),]
mp_by_nets<-mp[order(mp$net_score, mp$aud_score, mp$cri_score, decreasing=TRUE),]
mp_by_cris<-mp[order(mp$cri_score, mp$aud_score, mp$net_score, decreasing=TRUE),]
#---------------------#관객/네티즌/비평가 평점에 따른 영화 정렬#---------------------#

#---------------------#장르/등급/국가별 선호도/신뢰도 테이블 생성#---------------------#
#장르 테이블 생성
genre_analysis <- data.frame(matrix(nrow=nrow(get),ncol=(ncol(get))))
names(genre_analysis)[1:3] <- names(get)[1:3]
genre_analysis[,1:3] <- get[,1:3]
for(i in 4:(ncol(genre_analysis)/3+2)){
  names(genre_analysis)[(i-3)*3+1] <- paste0(strsplit(names(get)[(i-3)*3+1],'_')[[1]][1],"_prefer")
  names(genre_analysis)[(i-3)*3+2] <- paste0(strsplit(names(get)[(i-3)*3+1],'_')[[1]][1],"_credit")
  names(genre_analysis)[(i-3)*3+3] <- paste0(strsplit(names(get)[(i-3)*3+1],'_')[[1]][1],"_count")
}

#등급 테이블 생성
grade_analysis <- data.frame(matrix(nrow=nrow(grt),ncol=(ncol(grt))))
names(grade_analysis)[1:3] <- names(grt)[1:3]
grade_analysis[,1:3] <- grt[,1:3]
for(i in 4:(ncol(grade_analysis)/3+2)){
  names(grade_analysis)[(i-3)*3+1] <- paste0(strsplit(names(grt)[(i-3)*3+1],'_')[[1]][1],"_prefer")
  names(grade_analysis)[(i-3)*3+2] <- paste0(strsplit(names(grt)[(i-3)*3+1],'_')[[1]][1],"_credit")
  names(grade_analysis)[(i-3)*3+3] <- paste0(strsplit(names(grt)[(i-3)*3+1],'_')[[1]][1],"_count")
}

#국가 테이블 생성
nation_analysis <- data.frame(matrix(nrow=nrow(nat),ncol=(ncol(nat))))
names(nation_analysis)[1:3] <- names(nat)[1:3]
nation_analysis[,1:3] <- nat[,1:3]
for(i in 4:(ncol(nation_analysis)/3+2)){
  names(nation_analysis)[(i-3)*3+1] <- paste0(strsplit(names(nat)[(i-3)*3+1],'_')[[1]][1],"_prefer")
  names(nation_analysis)[(i-3)*3+2] <- paste0(strsplit(names(nat)[(i-3)*3+1],'_')[[1]][1],"_credit")
  names(nation_analysis)[(i-3)*3+3] <- paste0(strsplit(names(nat)[(i-3)*3+1],'_')[[1]][1],"_count")
}

#각 유저의 전체 영화의 평균과 표준편차
user_stat<-data.frame(cbind(sort(unique(dp$user_pk)),aggregate(score~user_pk,dp,mean)$score,aggregate(score~user_pk,dp,sd)$score))
names(user_stat) <- c("user_pk","avg","std")

#인스턴스별로 각 테이블에 선호도/신뢰도를 계산하는 함수 작성
#선호도 : 전체 장르 평균값을 정규분포화하여, 각 장르의 0~1사이의 percentile값으로 전환
#신뢰도 : 전체 영화 관람수 중 각 장르의 영화를 얼마나 봤는지(DF)를 수치화하고 log & min-max로 0~1사이 값으로 전환  

create_idx <- function(x,y){ #x : data instance, y : table type
  table <- data.frame(matrix(nrow=1,ncol=ncol(x)))
  table[,1:3] <- x[,1:3]
  D<-0
  for(i in 4:(ncol(y)/3+2)){ if(!is.na(x[,3+(i-3)*3])){ D <- D+x[,3+(i-3)*3] } }
  user_stat
  avg <- user_stat[which(user_stat$user_pk==x$user_pk),]$avg
  std <- ifelse(is.na(user_stat[which(user_stat$user_pk==x$user_pk),]$std),0,user_stat[which(user_stat$user_pk==x$user_pk),]$std)
  for(i in 4:(ncol(y)/3+2)){
    if(!is.na(x[,3+(i-3)*3])){
      g <- x[,3+(i-3)*3]
      temp <- ifelse(log(g/D)/log(D)+1>0,round(log(g/D)/log(D)+1,3),0)
      table[,(2+(i-3)*3)]<-temp
      table[,(3+(i-3)*3)]<-g
      table[,(1+(i-3)*3)]<-ifelse(std!=0,round(pnorm((x[,1+(i-3)*3]-avg)/std),3),round(avg,3))
    }
  }
  return(table)
}

#모든 인스턴스에 대해 get_cred_idx 함수 실행
for(i in 1:nrow(get)){
  genre_analysis[i,]<-create_idx(get[i,],genre_analysis)
}

for(i in 1:nrow(grt)){
  grade_analysis[i,]<-create_idx(grt[i,],grade_analysis)
}

for(i in 1:nrow(nat)){
  nation_analysis[i,]<-create_idx(nat[i,],nation_analysis)
}

genre_analysis <- genre_analysis[,-1]
grade_analysis <- grade_analysis[,-1]
nation_analysis <- nation_analysis[,-1]

View(genre_analysis)
View(grade_analysis)
View(nation_analysis)
#---------------------#장르/등급/국가별 선호도/신뢰도 테이블 생성#---------------------#


#<-------------------------file output--------------------------> 
write.csv(genre_analysis, file="/Users/hodong/Desktop/genre_analysis.csv",row.names=FALSE)
write.csv(grade_analysis, file="/Users/hodong/Desktop/grade_analysis.csv",row.names=FALSE)
write.csv(nation_analysis, file="/Users/hodong/Desktop/nation_analysis.csv",row.names=FALSE)
#<-------------------------file output--------------------------> 

'''
write.xlsx(genre_analysis,                # R데이터명
           file="/Users/hodong/Desktop/genre_analysis.xlsx",  # 여기서는 기존의 엑셀 파일이라고 설정함
           sheetName="new", # 기존의 엑셀 파일에 new라는 시트에 데이터를 넣음
           col.names=TRUE,  # 변수이름을 그대로 사용
           row.names=FALSE,# 행이름은 사용하지 않음
           append=FALSE)      # 기존의 엑셀 파일이 있으면 그곳에 추가해서 저장

write.xlsx(grade_analysis,                # R데이터명
           file="/Users/hodong/Desktop/grade_analysis.xlsx",  # 여기서는 기존의 엑셀 파일이라고 설정함
           sheetName="new", # 기존의 엑셀 파일에 new라는 시트에 데이터를 넣음
           col.names=TRUE,  # 변수이름을 그대로 사용
           row.names=FALSE,# 행이름은 사용하지 않음
           append=FALSE)      # 기존의 엑셀 파일이 있으면 그곳에 추가해서 저장

write.xlsx(nation_analysis,                # R데이터명
           file="/Users/hodong/Desktop/nation_analysis.xlsx",  # 여기서는 기존의 엑셀 파일이라고 설정함
           sheetName="new", # 기존의 엑셀 파일에 new라는 시트에 데이터를 넣음
           col.names=TRUE,  # 변수이름을 그대로 사용
           row.names=FALSE,# 행이름은 사용하지 않음
           append=FALSE)      # 기존의 엑셀 파일이 있으면 그곳에 추가해서 저장
'''


