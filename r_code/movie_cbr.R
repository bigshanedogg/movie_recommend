library(xlsx)
library(KoNLP)
library(proxy)

#---------------------#file input#---------------------#
setwd("/Users/hodong/Desktop/data_project/naver_movie/fr25to135p20")
mp <- data.frame(read.csv("movie_prepared.csv",header=TRUE))
dp <- data.frame(read.csv("data_train.csv",header=TRUE))
#---------------------#file input#---------------------#

#---------------------#presetting#---------------------#
first_ext <- 31
extract_size <- first_ext -1
score_avg <- mean(dp$score)

mb <- data.frame(cbind(mp$title,mp$nation,mp$grade,mp[,12:ncol(mp)]))
names(mb) <- c("title","nation","grade",names(mp)[12:ncol(mp)])
temp1 <- mb[,-c(2:3)]
temp1[,1] <- as.character(temp1[,1])
for(j in 2:ncol(temp1)){
  temp1[,j] <- as.numeric(as.character(temp1[,j]))
}
#---------------------#presetting#---------------------#


#---------------------#cbr based user_characteristic#---------------------#
#euc_cbr <- data.frame(cbind(sort(unique(dp$user_pk),decreasing=FALSE),matrix(nrow=length(unique(dp$user_pk)),ncol=(extract_size+2))))
#names(euc_cbr)[1:3] <- c("user_pk","id","review_count")
cos_cbr <- data.frame(cbind(sort(unique(dp$user_pk),decreasing=FALSE),matrix(nrow=length(unique(dp$user_pk)),ncol=(extract_size+2))))
names(cos_cbr)[1:3] <- c("user_pk","id","review_count")

#이전 파일 불러오기
setwd("/Users/hodong/Desktop")
euc_cbr2 <- data.frame(read.csv("euc_cbrf.csv",header=TRUE))
cos_cbr2 <- data.frame(read.csv("cos_cbrf.csv",header=TRUE))
#euc_cbr <- data.frame(rbind(euc_cbr2[1:3780,],euc_cbr[3781:nrow(euc_cbr),]))
#cos_cbr <- data.frame(rbind(cos_cbr2[1:3780,],cos_cbr[3781:nrow(cos_cbr),]))


temp_vec <- sort(unique(dp$user_pk),decreasing=FALSE)
temp_vec
temp_vec <- temp_vec[temp_vec<=5300]
View(euc_cbr[which(euc_cbr$user_pk %in% temp_vec),])

#temp_vec
#i <- sort(unique(dp$user_pk),decreasing=FALSE)

#for(i in temp_vec){
for(i in sort(unique(dp$user_pk),decreasing=FALSE)){
  #euc_cbr[which(euc_cbr$user_pk==i),2] <- as.character((dp[which(dp$user_pk==i),]$id)[1])
  #euc_cbr[which(euc_cbr$user_pk==i),3] <- as.character((dp[which(dp$user_pk==i),]$review_count)[1])
  cos_cbr[which(cos_cbr$user_pk==i),2] <- as.character((dp[which(dp$user_pk==i),]$id)[1])
  cos_cbr[which(cos_cbr$user_pk==i),3] <- as.character((dp[which(dp$user_pk==i),]$review_count)[1])
  
  #avg <- mean(dp[which(dp$user_pk==i),]$score)
  temp <- mb[which(mb$title %in% dp[which(dp$user_pk==i & dp$score>=score_avg),]$title),][,4:ncol(mb)]
  user_stat <- data.frame(cbind("user",t(apply(temp,2,mean))))
  names(user_stat) <- names(temp1)
  user_stat[,1] <- as.character(user_stat[,1])
  for(j in 2:ncol(user_stat)){
    user_stat[,j] <- as.numeric(as.character((user_stat[,j])))
  }
  full_table <- rbind(user_stat,temp1)
  identifier <- full_table[,1]
  contents <- full_table[,-1]
  
  #euc_mat <- as.matrix(dist(contents, method="Euclidean"))
  cos_mat <- as.matrix(dist(contents, method="cosine"))
  #jac_mat <- as.matrix(dist(contents, method="Jaccard"))
  
  #print(head(sort(cos_mat[1,],decreasing=FALSE),n=10))
  
  cos_list <- as.numeric(names(head(sort(cos_mat[1,],decreasing=FALSE),n=first_ext)))
  cos_list <- cos_list[-1]
  #euc_list <- as.numeric(names(head(sort(euc_mat[1,],decreasing=FALSE),n=first_ext)))
  #euc_list <- euc_list[-1]
  #jac_list <- as.numeric(names(head(sort(jac_mat[1,],decreasing=FALSE),n=first_ext)))
  #jac_list <- jac_list[-1]
  
  #recom_euc <- full_table[euc_list,]$title
  recom_cos <- full_table[cos_list,]$title

  #if("user" %in% recom_euc){ recom_euc <- recom_euc[-which(recom_euc=="user")] }
  if("user" %in% recom_cos){ recom_cos <- recom_cos[-which(recom_cos=="user")] }
  #recom_euc <- recom_euc[1:first_ext]
  recom_cos <- recom_cos[1:first_ext]
  
  #if(length(recom_euc)>0){
  #  for(j in 1:length(recom_euc)){
  #    euc_cbr[which(euc_cbr$user_pk==i),j+3] <- recom_euc[j]
  #  }
  #}
  
  if(length(recom_cos)>0){
    for(j in 1:length(recom_cos)){
      cos_cbr[which(cos_cbr$user_pk==i),j+3] <- recom_cos[j]
    }
  }
  
  print(round((i/max(dp$user_pk))*100,2))
}

#View(euc_cbr2)
#View(cos_cbr2)

#View(euc_cbr)
View(cos_cbr)

#write.csv(euc_cbr, file="/Users/hodong/Desktop/euc_cbr.csv",row.names=FALSE)
write.csv(cos_cbr, file="/Users/hodong/Desktop/cos_cbr.csv",row.names=FALSE)
#---------------------#cbr based user_characteristic#---------------------#


#---------------------#cbr based user's movie list#---------------------#

full_table <- mb
identifier <- full_table[,1:3]
contents <- full_table[,-c(1:3)]
euc_mat <- as.matrix(dist(contents, method="Euclidean"))
cos_mat <- as.matrix(dist(contents, method="cosine"))


i<-532

#avg <- mean(dp[which(dp$user_pk==i),]$score)
temp <- mb[which(mb$title %in% dp[which(dp$user_pk==i & dp$score>=score_avg),]$title),][,4:ncol(mb)]
euc_sel <- euc_mat[as.numeric(rownames(temp)),]

euc_list <- vector()
if(nrow(euc_sel)>0){
  for(j in 1:nrow(euc_sel)){
    te <- as.numeric(names(euc_sel[j,][which(euc_sel[j,]==0)]))
    print(length(te))
    euc_list <- Reduce(c,list(euc_list,te))
  }
}
length(euc_list)
table(euc_list)

cos_sel <- cos_mat[as.numeric(rownames(temp)),]

cos_list <- vector()
if(nrow(cos_sel)>0){
  for(j in 1:nrow(cos_sel)){
    te <- as.numeric(names(cos_sel[j,][which(cos_sel[j,]==0)]))
    print(length(te))
    cos_list <- Reduce(c,list(cos_list,te))
  }
}
length(cos_list)
table(cos_list)

cos_te <- as.numeric(names(table(cos_list)[table(cos_list)==max(table(cos_list))]))
euc_te <- as.numeric(names(table(euc_list)[table(euc_list)==max(table(euc_list))]))

qqq <- intersect(cos_te,euc_te)
length(qqq)

taa <- c(euc_list,cos_list)
sort(table(taa),decreasing=TRUE)
uni <- union(euc_list,cos_list)
table(ints)
table(uni)



















