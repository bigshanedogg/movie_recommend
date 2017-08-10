library(xlsx)
library(KoNLP)

#---------------------#file input#---------------------#
setwd("/Users/hodong/Desktop/data_project/naver_movie/fr25to135p20")
mp <- data.frame(read.csv("movie_prepared.csv",header=TRUE))
dp <- data.frame(read.csv("data_train.csv",header=TRUE))
#---------------------#file input#---------------------#

extract_size <- 30
temp_df <- data.frame(cbind(sort(unique(dp$user_pk),decreasing=FALSE),matrix(nrow=length(unique(dp$user_pk)),ncol=(extract_size+2))))
names(temp_df)[1:3] <- c("user_pk","id","review_count")

for(i in sort(unique(dp$user_pk),decreasing=FALSE)){
  temp_df[which(temp_df$user_pk==i),2] <- as.character((dp[which(dp$user_pk==i),]$id)[1])
  temp_df[which(temp_df$user_pk==i),3] <- as.character((dp[which(dp$user_pk==i),]$review_count)[1])
}

#---------------------#Random recommendation#---------------------#
random_recom <- temp_df
m_t <- unique(as.character(mp$title))

for(i in 1:nrow(random_recom)){
  u_t <- dp[which(dp$user_pk==random_recom[i,]$user_pk),]$title
  temp_mv <- setdiff(m_t,u_t)
  recom <- temp_mv[sample(length(temp_mv),extract_size)]
  
  for(j in 1:length(recom)){
    random_recom[which(random_recom$user_pk==i),j+3] <- recom[j]
  }
}
View(random_recom)
#---------------------#Random recommendation#---------------------#


#---------------------#Top30 frequency recommendation & Top30 score recommendatio#---------------------#
score_recom <- temp_df
count_recom <- temp_df

m_t <- data.frame(cbind(unique(as.character(mp$title))))
names(m_t) <- c("title")
mlist <- data.frame(matrix(nrow=length(unique(as.character(mp$title))),ncol=2))
names(mlist) <- c("score","count")

for(i in 1:nrow(m_t)){
  mlist[i,]$score <- as.numeric(ifelse(is.nan(mean(dp[which(dp$title==as.character(m_t[i,])),]$score)),NA,mean(dp[which(dp$title==as.character(m_t[i,])),]$score)))
  mlist[i,]$count <- nrow(dp[which(dp$title==as.character(m_t[i,])),])
}
m_t <- data.frame(cbind(m_t,mlist))
m_t <- m_t[-which(m_t$count==0),]
#names(m_t) <- c("title","score","count")
#View(m_t)

for(i in 1:nrow(temp_df)){
  u_t <- dp[which(dp$user_pk==temp_df[i,]$user_pk),]$title
  temp_mv <- m_t[-which(m_t$title %in% u_t),]
  
  score_mv <- temp_mv[order(temp_mv$score,decreasing=TRUE),]
  count_mv <- temp_mv[order(temp_mv$count,decreasing=TRUE),]

  score_list <- as.vector(head(score_mv,n=extract_size)$title)
  count_list <- as.vector(head(count_mv,n=extract_size)$title)
  
  for(j in 1:length(score_list)){
    score_recom[which(score_recom$user_pk==temp_df[i,]$user_pk),j+3] <- score_list[j]
    count_recom[which(count_recom$user_pk==temp_df[i,]$user_pk),j+3] <- count_list[j]
  }
}
View(score_recom)
View(count_recom)
#---------------------#Top30 frequency recommendation & Top30 score recommendatio#---------------------#


#---------------------#File Output#---------------------#
write.csv(random_recom, file="/Users/hodong/Desktop/random_recom.csv",row.names=FALSE)
write.csv(score_recom, file="/Users/hodong/Desktop/score_recom.csv",row.names=FALSE)
write.csv(count_recom, file="/Users/hodong/Desktop/count_recom.csv",row.names=FALSE)
#---------------------#Top30 score recommendation#---------------------#