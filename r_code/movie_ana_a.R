install.packages("xlsx")
install.packages("KoNLP")
require(xlsx)
require(KoNLP)

#<-------------------------파일 입력--------------------------> 
setwd("/Users/hodong/Desktop/data_project/naver_movie")
data <- data.frame(read.csv("movie_parse3.csv",header=TRUE))
data <- data[,-1]
names(data) <- c('user_pk','id','reveiw_count','review_id','title','score','content')
movie <- data.frame(read.csv("movie_list2.csv",header=FALSE))
names(movie)<-c('title','genre','nation','time','date','grade','aud_score','cri_score','net_score')
View(data)
nrow(data)
View(movie)
#<-------------------------파일 입력--------------------------> 

#진짜 데이터 분석 시작이다....
#알고리즘 구성을 위한 reduced-set과 프로젝트용 full-set을 구분할 것.
