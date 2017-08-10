library(xlsx)
library(KoNLP)
library(ggplot2)
library(gridExtra)
library(grid)
library(gtable)
library(reshape2)
library(scales)

#---------------------#file input#---------------------#
setwd("/Users/hodong/Desktop/data_project/naver_movie/fr25to135p20")
#old_data <- data.frame(read.csv("movie_parse_u.csv",header=TRUE))
#names(old_data) <- names(dp)
mp <- data.frame(read.csv("movie_prepared.csv",header=TRUE))
dp <- data.frame(read.csv("data_prepared.csv",header=TRUE))
dtr <- data.frame(read.csv("data_train.csv",header=TRUE))
dte <- data.frame(read.csv("data_test.csv",header=TRUE))
#---------------------#file input#---------------------#

#---------------------#file input#---------------------#
setwd("/Users/hodong/Desktop/data_project/naver_movie/fr25to135p20/result")
dib <- data.frame(read.csv("director_board.csv",header=TRUE))
geb <- data.frame(read.csv("genre_board.csv",header=TRUE))
grb <- data.frame(read.csv("grade_board.csv",header=TRUE))
nab <- data.frame(read.csv("nation_board.csv",header=TRUE))
#---------------------#file input#---------------------#

#---------------------#file input#---------------------#
setwd("/Users/hodong/Desktop/data_project/naver_movie/fr25to135p20/result")
rr <- data.frame(read.csv("result_report.csv",header=TRUE))
ar <- data.frame(read.csv("ana_report.csv",header=TRUE))
#---------------------#file input#---------------------#


#---------------------#Review interval별 user 수 분포#---------------------#
uc_graph <- function(dtr){
  temp <- data.frame(matrix(nrow=length(unique(dtr$user_pk)),ncol=3))
  names(temp) <- c("user_pk","review_count","review_interval")
  temp[,1] <- sort(unique(dtr$user_pk),decreasing=FALSE)
  for(i in 1:nrow(temp)){
    temp[i,]$review_count <- dtr[which(dtr$user_pk==temp[i,]$user_pk),]$review_count[1]
    temp[i,]$review_interval <- (floor(temp[i,]$review_count/10))*10
  }
  
  user_count <- aggregate(data=temp,user_pk~review_interval,length)
  names(user_count) <- c("review_interval","user_count")
  
  ggplot(data=user_count,aes(y=user_count,x=review_interval)) + geom_bar(stat="identity") + scale_x_continuous(limits=c(0, 500), labels=comma)
  return(user_count)
  #ggplot(data=user_count,aes(y=user_count,x=review_interval)) + geom_bar(stat="identity") + scale_x_continuous(breaks=seq(0, 1000, 10),limits=c(0,300), labels=comma)
  #write.csv(user_count, file="/Users/hodong/Desktop/after_sample.csv",row.names=FALSE)
}

#before_user <- uc_graph(old_data) #샘플링 후 데이터의 유저 분포 그래프
#View(before_user)
#ggplot(data=before_user,aes(y=user_count,x=review_interval)) + geom_bar(stat="identity") + scale_x_continuous(breaks=seq(0, 1000, 10),labels=comma)
#write.csv(before_user, file="/Users/hodong/Desktop/before_sampling.csv",row.names=FALSE) #0.77033
after_user <- uc_graph(dtr) #샘플링 후 데이터의 유저 분포 그래프
ggplot(data=after_usert,aes(y=user_count,x=review_interval)) + geom_bar(stat="identity") + scale_x_continuous(breaks=seq(0, 1000, 10),labels=comma)
write.csv(after_user, file="/Users/hodong/Desktop/after_sampling.csv",row.names=FALSE) #0.77033
#setwd("/Users/hodong/Desktop/data_project/naver_movie/half_million")
#data <- data.frame(read.csv("movie_parse_u.csv",header=FALSE))
#names(data) <- c('user_pk','id','review_count','review_id','title','score','content')
#---------------------#Review interval별 user 수 분포#---------------------#


#---------------------#board별 score & review count 분포#---------------------#
board_graph <- function(data, x1, x2, x3, count, y, max_y, ylabel, xlabel){
  ggplot(data, aes(y, group=1)) + 
    geom_bar(stat="identity",aes(x=y, y=count)) +
    geom_line(aes(y=x1, colour="aud_mean")) + 
    geom_line(aes(y=x2, colour="cri_mean")) + 
    geom_line(aes(y=x3, colour="net_mean")) + 
    scale_y_continuous(limits=c(0, max_y), breaks=pretty_breaks(), labels=comma) + 
    ylab(ylabel) + 
    xlab(xlabel)
}


#top/bottom 10 review count director analysis
dib_max <- head(dib[order(-dib$count),], n=10)
dib_min <- head(dib[order(dib$count),], n=10)

board_graph(dib_max, dib_max$aud_mean, dib_max$cri_mean, dib_max$net_mean, dib_max$count, dib_max$director, 30, "count", "director")
board_graph(dib_min, dib_min$aud_mean, dib_min$cri_mean, dib_min$net_mean, dib_min$count, dib_min$director, 30, "count", "director")


#top/bottom 10 review count genre analysis
geb_max <- head(geb[order(-geb$count),], n=10)
geb_min <- head(geb[order(geb$count),], n=10)
geb_max$count <- geb_max$count/100
geb_min$count <- geb_min$count/100

board_graph(geb_max, geb_max$aud_mean, geb_max$cri_mean, geb_max$net_mean, geb_max$count, geb_max$genre, 30, "count/100", "genre")
board_graph(geb_min, geb_min$aud_mean, geb_min$cri_mean, geb_min$net_mean, geb_min$count, geb_min$genre, 30, "count/100", "genre")


#top/bottom 10 review count grade analysis
grb_max <- head(grb[order(-grb$count),], n=10)
grb_min <- head(grb[order(grb$count),], n=10)
grb_max$count <- grb_max$count/100
grb_min$count <- grb_min$count/100

board_graph(grb_max, grb_max$aud_mean, grb_max$cri_mean, grb_max$net_mean, grb_max$count, grb_max$grade, 30, "count/100", "grade")
board_graph(grb_min, grb_min$aud_mean, grb_min$cri_mean, grb_min$net_mean, grb_min$count, grb_min$grade, 30, "count/100", "grade")


#top/bottom 10 review count nation analysis
nab_max <- head(nab[order(-nab$count),], n=10)
nab_min <- head(nab[order(nab$count),], n=10)
nab_max$count <- nab_max$count/100
nab_min$count <- nab_min$count/100

board_graph(nab_max, nab_max$aud_mean, nab_max$cri_mean, nab_max$net_mean, nab_max$count, nab_max$nation, 30, "count/100", "nation")
board_graph(nab_min, nab_min$aud_mean, nab_min$cri_mean, nab_min$net_mean, nab_min$count, nab_min$nation, 30, "count/100", "nation")
#---------------------#board별 score & review count 분포#---------------------#


#---------------------#result report의 예측률 성과 비교#---------------------#
names(rr)
rr$pre_ac_rate <- round((rr$predicted_mean/rr$actual_mean/10),2)
rr$NOT0_ratio <- 1-rr$X0_ratio
rrs <- data.frame(rbind(rr[which(rr$method %in% c("random_rec","count_rec","score_rec")),],head(rr[order(-rr$rate_mean),],n=7)))

#rr$NOT0_ratio
#rr$rate_mean
#rr$rate....0.
rr$pre_ac_rate

View(rr)
rr_before <- rr[1:30,]
rr_after <- rr[31:42,]

#before ensemble method의 그래프
ggplot(rr_before, aes(method, group=1)) + 
  #geom_bar(stat="identity",aes(x=y, y=count)) +
  geom_line(aes(y=NOT0_ratio, colour="NOT0_ratio")) + 
  geom_line(aes(y=rate_mean, colour="rate_mean")) + 
  geom_line(aes(y=rate....0., colour="rate....0.")) + 
  geom_line(aes(y=pre_ac_rate, colour="pre_ac_rate")) + 
  scale_y_continuous(limits=c(0, 1.1), breaks=pretty_breaks(), labels=comma) + 
  theme(axis.text.x = element_text(size=5))

#after ensemble method의 그래프
ggplot(rr_after, aes(method, group=1)) + 
  #geom_bar(stat="identity",aes(x=y, y=count)) +
  geom_line(aes(y=NOT0_ratio, colour="NOT0_ratio")) + 
  geom_line(aes(y=rate_mean, colour="rate_mean")) + 
  geom_line(aes(y=rate....0., colour="rate....0.")) + 
  geom_line(aes(y=pre_ac_rate, colour="pre_ac_rate")) + 
  scale_y_continuous(limits=c(0, 1.1), breaks=pretty_breaks(), labels=comma) + 
  theme(axis.text.x = element_text(size=6))

ggplot(rr_before, aes(method, group=1)) + 
  #geom_bar(stat="identity",aes(x=y, y=count)) +
  geom_line(aes(y=rate_mean, colour="rate_mean")) + 
  geom_line(aes(y=pre_ac_rate, colour="pre_ac_rate")) + 
  scale_y_continuous(limits=c(0, 1.1), breaks=pretty_breaks(), labels=comma) + 
  theme(axis.text.x = element_text(size=5))
#View(rr)
ggplot(rr_after, aes(method, group=1)) + 
  #geom_bar(stat="identity",aes(x=y, y=count)) +
  geom_line(aes(y=rate_mean, colour="rate_mean")) + 
  geom_line(aes(y=pre_ac_rate, colour="pre_ac_rate")) + 
  scale_y_continuous(limits=c(0, 1.1), breaks=pretty_breaks(), labels=comma) + 
  theme(axis.text.x = element_text(size=6))



#NOT0_ratio와 rate_mean의 상관 관계 #0.9217809
cor(rr$NOT0_ratio[!is.na(rr$NOT0_ratio)],rr$rate_mean[!is.na(rr$rate_mean)])

#rate_mean이 높은 top 10 method의 그래프
ggplot(rrs, aes(method, group=1)) + 
  #geom_bar(stat="identity",aes(x=y, y=count)) +
  geom_line(aes(y=NOT0_ratio, colour="NOT0_ratio")) + 
  geom_line(aes(y=rate_mean, colour="rate_mean")) + 
  geom_line(aes(y=pre_ac_rate, colour="pre_ac_rate")) + 
  scale_y_continuous(limits=c(0, 1.1), breaks=pretty_breaks(), labels=comma)

write.csv(rr_before, file="/Users/hodong/Desktop/rr_before.csv",row.names=FALSE) #0.77033
write.csv(rr_after, file="/Users/hodong/Desktop/rr_after.csv",row.names=FALSE) #0.77033

#---------------------#result report의 예측률 성과 비교#---------------------#


#---------------------#result report의 pre_ac_cor & mean_abs (저조한) 성과 비교#---------------------#
ggplot(rr, aes(method, group=1)) + 
  #geom_bar(stat="identity",aes(x=y, y=count)) +
  geom_line(aes(y=pre_ac_cor, colour="pre_ac_cor")) + 
  geom_line(aes(y=mean_abs, colour="mean_abs")) + 
  scale_y_continuous(limits=c(0, 3.5), breaks=pretty_breaks(), labels=comma)
#---------------------#result report의 pre_ac_cor & mean_abs (저조한) 성과 비교#---------------------#


#---------------------#result report의 pre_ac_cor & mean_abs (저조한) 성과 비교#---------------------#




















