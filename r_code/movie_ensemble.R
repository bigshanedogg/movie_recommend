library(xlsx)
library(KoNLP)

#---------------------#file input#---------------------#
setwd("/Users/hodong/Desktop/data_project/naver_movie/fr25to135p20")
mp <- data.frame(read.csv("movie_prepared.csv",header=TRUE))
dp <- data.frame(read.csv("data_prepared.csv",header=TRUE))
dtr <- data.frame(read.csv("data_train.csv",header=TRUE))
dte <- data.frame(read.csv("data_test.csv",header=TRUE))
#---------------------#file input#---------------------#
nrow(dte)
length(unique(dte$user_pk))
#---------------------#file input2#---------------------#
setwd("/Users/hodong/Desktop/data_project/naver_movie/fr25to135p20/ensemble")
alcf <- data.frame(read.csv("all_cf.csv",header=TRUE))
alcfs <- data.frame(read.csv("all_cf_score.csv",header=TRUE))
gecf <- data.frame(read.csv("genre_cf.csv",header=TRUE))
gecfs <- data.frame(read.csv("genre_cf_score.csv",header=TRUE))
grcf <- data.frame(read.csv("grade_cf.csv",header=TRUE))
grcfs <- data.frame(read.csv("grade_cf_score.csv",header=TRUE))
nacf <- data.frame(read.csv("nation_cf.csv",header=TRUE))
nacfs <- data.frame(read.csv("nation_cf_score.csv",header=TRUE))
gek <- data.frame(read.csv("genre_k_means.csv",header=TRUE))
geks <- data.frame(read.csv("genre_k_means_score.csv",header=TRUE))
grk <- data.frame(read.csv("grade_k_means.csv",header=TRUE))
grks <- data.frame(read.csv("grade_k_means_score.csv",header=TRUE))
nak <- data.frame(read.csv("nation_k_means.csv",header=TRUE))
naks <- data.frame(read.csv("nation_k_means_score.csv",header=TRUE))
gek2 <- data.frame(read.csv("genre_k_means2.csv",header=TRUE))
gek2s <- data.frame(read.csv("genre_k_means_score2.csv",header=TRUE))
grk2 <- data.frame(read.csv("grade_k_means2.csv",header=TRUE))
grk2s <- data.frame(read.csv("grade_k_means_score2.csv",header=TRUE))
nak2 <- data.frame(read.csv("nation_k_means2.csv",header=TRUE))
nak2s <- data.frame(read.csv("nation_k_means_score2.csv",header=TRUE))
uar <- data.frame(read.csv("user_ar_result.csv",header=TRUE))
#ecbr <- data.frame(read.csv("euc_cbr.csv",header=TRUE))
ccbr <- data.frame(read.csv("cos_cbr.csv",header=TRUE))
rar <- data.frame(read.csv("random_recom.csv",header=TRUE))
scr <- data.frame(read.csv("score_recom.csv",header=TRUE))
cor <- data.frame(read.csv("count_recom.csv",header=TRUE))
#---------------------#file input2#---------------------#


#---------------------#Calculating correlation#---------------------#
get_gek_score <- function(x,pk){ #x : 1개의 predicted title
  tt <- t(gek[which(gek$user_pk==pk),-c(1:3)])[!is.na(t(gek[which(gek$user_pk==pk),-c(1:3)]))]
  if(x %in% tt){
    score <- geks[which(gek$user_pk==pk),(which(tt==x)+3)]
  }else{
    score <- NA
  }
  return(score)
}
get_grk_score <- function(x,pk){ #x : 1개의 predicted title
  tt <- t(grk[which(grk$user_pk==pk),-c(1:3)])[!is.na(t(grk[which(grk$user_pk==pk),-c(1:3)]))]
  if(x %in% tt){
    score <- grks[which(grk$user_pk==pk),(which(tt==x)+3)]
  }else{
    score <- NA
  }
  return(score)
}
get_nak_score <- function(x,pk){ #x : 1개의 predicted title
  tt <- t(nak[which(nak$user_pk==pk),-c(1:3)])[!is.na(t(nak[which(nak$user_pk==pk),-c(1:3)]))]
  if(x %in% tt){
    score <- naks[which(nak$user_pk==pk),(which(tt==x)+3)]
  }else{
    score <- NA
  }
  return(score)
}

cal_cor <- function(x,y,pk){ #x : predicted, y : actual, pk : user_pk
  cor_temp <- data.frame(cbind(dte[which(dte$user_pk==pk),1:3],matrix(nrow=nrow(dte[which(dte$user_pk==pk),1:3]),ncol=5)))
  names(cor_temp) <- c("user_pk","id","review_count","title","predicted","actual","ints","abs")
  
  for(j in 1:length(y)){
    cor_temp[j,]$title <- y[j]
    ints <- intersect(x,y[j])
    cor_temp[j,]$ints <- ifelse((length(ints)!=0),1,0)
    tt <- dte[which(dte$user_pk==cor_temp[j,]$user_pk),]
    tt <- tt[which(tt$title==y[j]),]
    cor_temp[j,]$actual <- tt$score[1]
    
    if(length(ints)!=0){
      v_temp <- get_gek_score(ints,pk)
      v_temp <- Reduce(c,list(v_temp,get_grk_score(ints,pk)))
      v_temp <- Reduce(c,list(v_temp,get_nak_score(ints,pk)))
      v_temp <- v_temp[!is.na(v_temp)]
      cor_temp[j,]$predicted <- mean(v_temp)
      
      if(!is.nan(cor_temp[j,]$predicted)){
        cor_temp[j,]$abs <- abs(cor_temp[j,]$predicted-cor_temp[j,]$actual)
      }else{
        cor_temp[j,]$abs <- NA
        cor_temp[j,]$predicted <- NA
      }
    }
  }
  return(cor_temp)
}
#---------------------#Calculating correlation#---------------------#


#---------------------#Predicting rate calculation function#---------------------#
#유저의 리스트 중 맞출 확률 p(a&p)/p(a) || p(a1)+p(a2)+...+p(an)/sum(p(a&b))
#해당 방법의 각 회원에 대한 추천리스트와 테스트 데이터셋에 존재하는 회원의 실제 리스트의 예측률
accuracy_cal <- function(x,y){
  #eg1. x : dte, y : gek
  acc_list <- data.frame(cbind(unique(x[order(x$user_pk),1:2]),matrix(nrow=length(unique(x$user_pk)),ncol=8)))
  names(acc_list) <- c("user_pk","id","review_count","predicted","inter","actual","rate","ints_sum","cor","mean_abs")
  c_temp <- data.frame(cbind(dte[,1:3],matrix(nrow=nrow(dte),ncol=5)))
  names(c_temp) <- c("user_pk","id","review_count","title","predicted_score","actual_score","ints","abs")
  
  cor_pre <- vector()
  cor_ac <- vector()
  for(i in 1:nrow(acc_list)){
    acc_list[i,]$review_count <- dtr[which(dtr$user_pk==acc_list[i,]$user_pk),]$review_count[1]
    actual <- as.character(x[which(x$user_pk==acc_list[i,]$user_pk),]$title)
    predicted <- as.vector(t(y[i,-c(1:3,which(y[i,]==""))]))
    
    c_temp[which(c_temp$user_pk==acc_list[i,]$user_pk),] <- cal_cor(predicted,actual,acc_list[i,]$user_pk)
    
    ints <- intersect(actual,predicted)
    rate <- length(ints)/length(actual)
    acc_list[i,]$predicted <- length(predicted)
    acc_list[i,]$inter <- length(ints)
    acc_list[i,]$actual <- length(actual)
    acc_list[i,]$rate <- rate
    
    ct <- c_temp[which(c_temp$user_pk==acc_list[i,]$user_pk),]
    acc_list[i,]$ints_sum <- sum(ct$ints)
    
    if(acc_list[i,]$ints_sum!=0){
      cor_pre <- Reduce(c,list(cor_pre,ct$predicted_score[which(ct$ints==1)]))
      cor_ac <- Reduce(c,list(cor_ac,ct$actual_score[which(ct$ints==1)]))
      acc_list[i,]$mean_abs <- mean(ct$abs[which(ct$ints==1)])
    }
  }
  if(length(which(is.na(cor_pre)))!=0){
    cor_ac <- cor_ac[-which(is.na(cor_pre))]
    cor_pre <- cor_pre[-which(is.na(cor_pre))]
  }
  acc_list$cor[which(acc_list$ints_sum!=0)] <- cor(cor_pre,cor_ac)
  return(acc_list)
}

#메소드별 kmeans/cf/cbr 기반 리스트 재조정 후 예측률
#km1
accuracy_km1_intersect <- function(x){ 
  acc_list <- data.frame(cbind(unique(x[order(x$user_pk),1:2]),matrix(nrow=length(unique(x$user_pk)),ncol=8)))
  names(acc_list) <- c("user_pk","id","review_count","predicted","inter","actual","rate","ints_sum","cor","mean_abs")
  c_temp <- data.frame(cbind(dte[,1:3],matrix(nrow=nrow(dte),ncol=5)))
  names(c_temp) <- c("user_pk","id","review_count","title","predicted_score","actual_score","ints","abs")
  
  x_1 <- gek; x_2 <- grk; x_3 <- nak;
  
  cor_pre <- vector()
  cor_ac <- vector()
  for(i in 1:nrow(acc_list)){
    acc_list[i,]$review_count <- dtr[which(dtr$user_pk==acc_list[i,]$user_pk),]$review_count[1]
    actual <- as.character(x[which(x$user_pk==acc_list[i,]$user_pk),]$title)
    
    predicted <- as.vector(t(x_1[i,-c(1:3,which(x_1[i,]==""))]))
    temp <- as.vector(t(x_2[i,-c(1:3,which(x_2[i,]==""))]))
    predicted <- Reduce(intersect,list(predicted,temp))
    temp <- as.vector(t(x_3[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(intersect,list(predicted,temp))
    
    c_temp[which(c_temp$user_pk==acc_list[i,]$user_pk),] <- cal_cor(predicted,actual,acc_list[i,]$user_pk)
    
    ints <- intersect(actual,predicted)
    rate <- length(ints)/length(actual)
    acc_list[i,]$predicted <- length(predicted)
    acc_list[i,]$inter <- length(ints)
    acc_list[i,]$actual <- length(actual)
    acc_list[i,]$rate <- rate
    
    ct <- c_temp[which(c_temp$user_pk==acc_list[i,]$user_pk),]
    acc_list[i,]$ints_sum <- sum(ct$ints)
    
    if(acc_list[i,]$ints_sum!=0){
      cor_pre <- Reduce(c,list(cor_pre,ct$predicted_score[which(ct$ints==1)]))
      cor_ac <- Reduce(c,list(cor_ac,ct$actual_score[which(ct$ints==1)]))
      acc_list[i,]$mean_abs <- mean(ct$abs[which(ct$ints==1)])
    }
  }
  if(length(which(is.na(cor_pre)))!=0){
    cor_ac <- cor_ac[-which(is.na(cor_pre))]
    cor_pre <- cor_pre[-which(is.na(cor_pre))]
  }
  acc_list$cor[which(acc_list$ints_sum!=0)] <- cor(cor_pre,cor_ac)
  return(acc_list)
}
accuracy_km1_union <- function(x){ 
  acc_list <- data.frame(cbind(unique(x[order(x$user_pk),1:2]),matrix(nrow=length(unique(x$user_pk)),ncol=8)))
  names(acc_list) <- c("user_pk","id","review_count","predicted","inter","actual","rate","ints_sum","cor","mean_abs")
  c_temp <- data.frame(cbind(dte[,1:3],matrix(nrow=nrow(dte),ncol=5)))
  names(c_temp) <- c("user_pk","id","review_count","title","predicted_score","actual_score","ints","abs")
  
  x_1 <- gek; x_2 <- grk; x_3 <- nak;
  
  cor_pre <- vector()
  cor_ac <- vector()
  for(i in 1:nrow(acc_list)){
    acc_list[i,]$review_count <- dtr[which(dtr$user_pk==acc_list[i,]$user_pk),]$review_count[1]
    actual <- as.character(x[which(x$user_pk==acc_list[i,]$user_pk),]$title)
    
    predicted <- as.vector(t(x_1[i,-c(1:3,which(x_1[i,]==""))]))
    temp <- as.vector(t(x_2[i,-c(1:3,which(x_2[i,]==""))]))
    predicted <- Reduce(union,list(predicted,temp))
    temp <- as.vector(t(x_3[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(union,list(predicted,temp))
    
    c_temp[which(c_temp$user_pk==acc_list[i,]$user_pk),] <- cal_cor(predicted,actual,acc_list[i,]$user_pk)
    
    ints <- intersect(actual,predicted)
    rate <- length(ints)/length(actual)
    acc_list[i,]$predicted <- length(predicted)
    acc_list[i,]$inter <- length(ints)
    acc_list[i,]$actual <- length(actual)
    acc_list[i,]$rate <- rate
    
    ct <- c_temp[which(c_temp$user_pk==acc_list[i,]$user_pk),]
    acc_list[i,]$ints_sum <- sum(ct$ints)
    
    if(acc_list[i,]$ints_sum!=0){
      cor_pre <- Reduce(c,list(cor_pre,ct$predicted_score[which(ct$ints==1)]))
      cor_ac <- Reduce(c,list(cor_ac,ct$actual_score[which(ct$ints==1)]))
      acc_list[i,]$mean_abs <- mean(ct$abs[which(ct$ints==1)])
    }
  }
  if(length(which(is.na(cor_pre)))!=0){
    cor_ac <- cor_ac[-which(is.na(cor_pre))]
    cor_pre <- cor_pre[-which(is.na(cor_pre))]
  }
  acc_list$cor[which(acc_list$ints_sum!=0)] <- cor(cor_pre,cor_ac)
  return(acc_list)
}
accuracy_km1_frequency <- function(x){ 
  acc_list <- data.frame(cbind(unique(x[order(x$user_pk),1:2]),matrix(nrow=length(unique(x$user_pk)),ncol=8)))
  names(acc_list) <- c("user_pk","id","review_count","predicted","inter","actual","rate","ints_sum","cor","mean_abs")
  c_temp <- data.frame(cbind(dte[,1:3],matrix(nrow=nrow(dte),ncol=5)))
  names(c_temp) <- c("user_pk","id","review_count","title","predicted_score","actual_score","ints","abs")
  
  x_1 <- gek; x_2 <- grk; x_3 <- nak;
  
  cor_pre <- vector()
  cor_ac <- vector()
  for(i in 1:nrow(acc_list)){
    acc_list[i,]$review_count <- dtr[which(dtr$user_pk==acc_list[i,]$user_pk),]$review_count[1]
    actual <- as.character(x[which(x$user_pk==acc_list[i,]$user_pk),]$title)
    
    predicted <- as.vector(t(x_1[i,-c(1:3,which(x_1[i,]==""))]))
    temp <- as.vector(t(x_2[i,-c(1:3,which(x_2[i,]==""))]))
    predicted <- Reduce(c,list(predicted,temp))
    temp <- as.vector(t(x_3[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(c,list(predicted,temp))
    predicted <- names(head(sort(table(predicted),decreasing=TRUE),n=30))
    
    c_temp[which(c_temp$user_pk==acc_list[i,]$user_pk),] <- cal_cor(predicted,actual,acc_list[i,]$user_pk)
    
    ints <- intersect(actual,predicted)
    rate <- length(ints)/length(actual)
    acc_list[i,]$predicted <- length(predicted)
    acc_list[i,]$inter <- length(ints)
    acc_list[i,]$actual <- length(actual)
    acc_list[i,]$rate <- rate
    
    ct <- c_temp[which(c_temp$user_pk==acc_list[i,]$user_pk),]
    acc_list[i,]$ints_sum <- sum(ct$ints)
    
    if(acc_list[i,]$ints_sum!=0){
      cor_pre <- Reduce(c,list(cor_pre,ct$predicted_score[which(ct$ints==1)]))
      cor_ac <- Reduce(c,list(cor_ac,ct$actual_score[which(ct$ints==1)]))
      acc_list[i,]$mean_abs <- mean(ct$abs[which(ct$ints==1)])
    }
  }
  if(length(which(is.na(cor_pre)))!=0){
    cor_ac <- cor_ac[-which(is.na(cor_pre))]
    cor_pre <- cor_pre[-which(is.na(cor_pre))]
  }
  acc_list$cor[which(acc_list$ints_sum!=0)] <- cor(cor_pre,cor_ac)
  return(acc_list)
}
#km2
accuracy_km2_intersect <- function(x){ 
  acc_list <- data.frame(cbind(unique(x[order(x$user_pk),1:2]),matrix(nrow=length(unique(x$user_pk)),ncol=8)))
  names(acc_list) <- c("user_pk","id","review_count","predicted","inter","actual","rate","ints_sum","cor","mean_abs")
  c_temp <- data.frame(cbind(dte[,1:3],matrix(nrow=nrow(dte),ncol=5)))
  names(c_temp) <- c("user_pk","id","review_count","title","predicted_score","actual_score","ints","abs")
  
  x_1 <- gek2; x_2 <- grk2; x_3 <- nak2;
  
  cor_pre <- vector()
  cor_ac <- vector()
  for(i in 1:nrow(acc_list)){
    acc_list[i,]$review_count <- dtr[which(dtr$user_pk==acc_list[i,]$user_pk),]$review_count[1]
    actual <- as.character(x[which(x$user_pk==acc_list[i,]$user_pk),]$title)
    
    predicted <- as.vector(t(x_1[i,-c(1:3,which(x_1[i,]==""))]))
    temp <- as.vector(t(x_2[i,-c(1:3,which(x_2[i,]==""))]))
    predicted <- Reduce(intersect,list(predicted,temp))
    temp <- as.vector(t(x_3[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(intersect,list(predicted,temp))
    
    c_temp[which(c_temp$user_pk==acc_list[i,]$user_pk),] <- cal_cor(predicted,actual,acc_list[i,]$user_pk)
    
    ints <- intersect(actual,predicted)
    rate <- length(ints)/length(actual)
    acc_list[i,]$predicted <- length(predicted)
    acc_list[i,]$inter <- length(ints)
    acc_list[i,]$actual <- length(actual)
    acc_list[i,]$rate <- rate
    
    ct <- c_temp[which(c_temp$user_pk==acc_list[i,]$user_pk),]
    acc_list[i,]$ints_sum <- sum(ct$ints)
    
    if(acc_list[i,]$ints_sum!=0){
      cor_pre <- Reduce(c,list(cor_pre,ct$predicted_score[which(ct$ints==1)]))
      cor_ac <- Reduce(c,list(cor_ac,ct$actual_score[which(ct$ints==1)]))
      acc_list[i,]$mean_abs <- mean(ct$abs[which(ct$ints==1)])
    }
  }
  if(length(which(is.na(cor_pre)))!=0){
    cor_ac <- cor_ac[-which(is.na(cor_pre))]
    cor_pre <- cor_pre[-which(is.na(cor_pre))]
  }
  
  acc_list$cor[which(acc_list$ints_sum!=0)] <- cor(cor_pre,cor_ac)
  return(acc_list)
}
accuracy_km2_union <- function(x){ 
  acc_list <- data.frame(cbind(unique(x[order(x$user_pk),1:2]),matrix(nrow=length(unique(x$user_pk)),ncol=8)))
  names(acc_list) <- c("user_pk","id","review_count","predicted","inter","actual","rate","ints_sum","cor","mean_abs")
  c_temp <- data.frame(cbind(dte[,1:3],matrix(nrow=nrow(dte),ncol=5)))
  names(c_temp) <- c("user_pk","id","review_count","title","predicted_score","actual_score","ints","abs")
  
  x_1 <- gek2; x_2 <- grk2; x_3 <- nak2;
  
  cor_pre <- vector()
  cor_ac <- vector()
  for(i in 1:nrow(acc_list)){
    acc_list[i,]$review_count <- dtr[which(dtr$user_pk==acc_list[i,]$user_pk),]$review_count[1]
    actual <- as.character(x[which(x$user_pk==acc_list[i,]$user_pk),]$title)
    
    predicted <- as.vector(t(x_1[i,-c(1:3,which(x_1[i,]==""))]))
    temp <- as.vector(t(x_2[i,-c(1:3,which(x_2[i,]==""))]))
    predicted <- Reduce(union,list(predicted,temp))
    temp <- as.vector(t(x_3[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(union,list(predicted,temp))
    
    c_temp[which(c_temp$user_pk==acc_list[i,]$user_pk),] <- cal_cor(predicted,actual,acc_list[i,]$user_pk)
    
    ints <- intersect(actual,predicted)
    rate <- length(ints)/length(actual)
    acc_list[i,]$predicted <- length(predicted)
    acc_list[i,]$inter <- length(ints)
    acc_list[i,]$actual <- length(actual)
    acc_list[i,]$rate <- rate
    
    ct <- c_temp[which(c_temp$user_pk==acc_list[i,]$user_pk),]
    acc_list[i,]$ints_sum <- sum(ct$ints)
    
    if(acc_list[i,]$ints_sum!=0){
      cor_pre <- Reduce(c,list(cor_pre,ct$predicted_score[which(ct$ints==1)]))
      cor_ac <- Reduce(c,list(cor_ac,ct$actual_score[which(ct$ints==1)]))
      acc_list[i,]$mean_abs <- mean(ct$abs[which(ct$ints==1)])
    }
  }
  if(length(which(is.na(cor_pre)))!=0){
    cor_ac <- cor_ac[-which(is.na(cor_pre))]
    cor_pre <- cor_pre[-which(is.na(cor_pre))]
  }
  acc_list$cor[which(acc_list$ints_sum!=0)] <- cor(cor_pre,cor_ac)
  return(acc_list)
}
accuracy_km2_frequency <- function(x){ 
  acc_list <- data.frame(cbind(unique(x[order(x$user_pk),1:2]),matrix(nrow=length(unique(x$user_pk)),ncol=8)))
  names(acc_list) <- c("user_pk","id","review_count","predicted","inter","actual","rate","ints_sum","cor","mean_abs")
  c_temp <- data.frame(cbind(dte[,1:3],matrix(nrow=nrow(dte),ncol=5)))
  names(c_temp) <- c("user_pk","id","review_count","title","predicted_score","actual_score","ints","abs")
  
  x_1 <- gek2; x_2 <- grk2; x_3 <- nak2;
  
  cor_pre <- vector()
  cor_ac <- vector()
  for(i in 1:nrow(acc_list)){
    acc_list[i,]$review_count <- dtr[which(dtr$user_pk==acc_list[i,]$user_pk),]$review_count[1]
    actual <- as.character(x[which(x$user_pk==acc_list[i,]$user_pk),]$title)
    
    predicted <- as.vector(t(x_1[i,-c(1:3,which(x_1[i,]==""))]))
    temp <- as.vector(t(x_2[i,-c(1:3,which(x_2[i,]==""))]))
    predicted <- Reduce(c,list(predicted,temp))
    temp <- as.vector(t(x_3[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(c,list(predicted,temp))
    predicted <- names(head(sort(table(predicted),decreasing=TRUE),n=30))
    
    c_temp[which(c_temp$user_pk==acc_list[i,]$user_pk),] <- cal_cor(predicted,actual,acc_list[i,]$user_pk)
    
    ints <- intersect(actual,predicted)
    rate <- length(ints)/length(actual)
    acc_list[i,]$predicted <- length(predicted)
    acc_list[i,]$inter <- length(ints)
    acc_list[i,]$actual <- length(actual)
    acc_list[i,]$rate <- rate
    
    ct <- c_temp[which(c_temp$user_pk==acc_list[i,]$user_pk),]
    acc_list[i,]$ints_sum <- sum(ct$ints)
    
    if(acc_list[i,]$ints_sum!=0){
      cor_pre <- Reduce(c,list(cor_pre,ct$predicted_score[which(ct$ints==1)]))
      cor_ac <- Reduce(c,list(cor_ac,ct$actual_score[which(ct$ints==1)]))
      acc_list[i,]$mean_abs <- mean(ct$abs[which(ct$ints==1)])
    }
  }
  if(length(which(is.na(cor_pre)))!=0){
    cor_ac <- cor_ac[-which(is.na(cor_pre))]
    cor_pre <- cor_pre[-which(is.na(cor_pre))]
  }
  
  acc_list$cor[which(acc_list$ints_sum!=0)] <- cor(cor_pre,cor_ac)
  return(acc_list)
}

#kma
accuracy_kma_intersect <- function(x){ 
  acc_list <- data.frame(cbind(unique(x[order(x$user_pk),1:2]),matrix(nrow=length(unique(x$user_pk)),ncol=8)))
  names(acc_list) <- c("user_pk","id","review_count","predicted","inter","actual","rate","ints_sum","cor","mean_abs")
  c_temp <- data.frame(cbind(dte[,1:3],matrix(nrow=nrow(dte),ncol=5)))
  names(c_temp) <- c("user_pk","id","review_count","title","predicted_score","actual_score","ints","abs")
  
  x_1 <- gek; x_2 <- grk; x_3 <- nak; x_4 <- gek2; x_5 <- grk2; x_6 <- nak2;
  
  cor_pre <- vector()
  cor_ac <- vector()
  for(i in 1:nrow(acc_list)){
    acc_list[i,]$review_count <- dtr[which(dtr$user_pk==acc_list[i,]$user_pk),]$review_count[1]
    actual <- as.character(x[which(x$user_pk==acc_list[i,]$user_pk),]$title)
    
    predicted <- as.vector(t(x_1[i,-c(1:3,which(x_1[i,]==""))]))
    temp <- as.vector(t(x_2[i,-c(1:3,which(x_2[i,]==""))]))
    predicted <- Reduce(intersect,list(predicted,temp))
    temp <- as.vector(t(x_3[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(intersect,list(predicted,temp))
    temp <- as.vector(t(x_4[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(intersect,list(predicted,temp))
    temp <- as.vector(t(x_5[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(intersect,list(predicted,temp))
    temp <- as.vector(t(x_6[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(intersect,list(predicted,temp))
    
    c_temp[which(c_temp$user_pk==acc_list[i,]$user_pk),] <- cal_cor(predicted,actual,acc_list[i,]$user_pk)
    
    ints <- intersect(actual,predicted)
    rate <- length(ints)/length(actual)
    acc_list[i,]$predicted <- length(predicted)
    acc_list[i,]$inter <- length(ints)
    acc_list[i,]$actual <- length(actual)
    acc_list[i,]$rate <- rate
    
    ct <- c_temp[which(c_temp$user_pk==acc_list[i,]$user_pk),]
    acc_list[i,]$ints_sum <- sum(ct$ints)
    
    if(acc_list[i,]$ints_sum!=0){
      cor_pre <- Reduce(c,list(cor_pre,ct$predicted_score[which(ct$ints==1)]))
      cor_ac <- Reduce(c,list(cor_ac,ct$actual_score[which(ct$ints==1)]))
      acc_list[i,]$mean_abs <- mean(ct$abs[which(ct$ints==1)])
    }
  }
  if(length(which(is.na(cor_pre)))!=0){
    cor_ac <- cor_ac[-which(is.na(cor_pre))]
    cor_pre <- cor_pre[-which(is.na(cor_pre))]
  }
  
  acc_list$cor[which(acc_list$ints_sum!=0)] <- cor(cor_pre,cor_ac)
  return(acc_list)
}
accuracy_kma_union <- function(x){ 
  acc_list <- data.frame(cbind(unique(x[order(x$user_pk),1:2]),matrix(nrow=length(unique(x$user_pk)),ncol=8)))
  names(acc_list) <- c("user_pk","id","review_count","predicted","inter","actual","rate","ints_sum","cor","mean_abs")
  c_temp <- data.frame(cbind(dte[,1:3],matrix(nrow=nrow(dte),ncol=5)))
  names(c_temp) <- c("user_pk","id","review_count","title","predicted_score","actual_score","ints","abs")
  
  x_1 <- gek; x_2 <- grk; x_3 <- nak; x_4 <- gek2; x_5 <- grk2; x_6 <- nak2;
  
  cor_pre <- vector()
  cor_ac <- vector()
  for(i in 1:nrow(acc_list)){
    acc_list[i,]$review_count <- dtr[which(dtr$user_pk==acc_list[i,]$user_pk),]$review_count[1]
    actual <- as.character(x[which(x$user_pk==acc_list[i,]$user_pk),]$title)
    
    predicted <- as.vector(t(x_1[i,-c(1:3,which(x_1[i,]==""))]))
    temp <- as.vector(t(x_2[i,-c(1:3,which(x_2[i,]==""))]))
    predicted <- Reduce(union,list(predicted,temp))
    temp <- as.vector(t(x_3[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(union,list(predicted,temp))
    temp <- as.vector(t(x_4[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(union,list(predicted,temp))
    temp <- as.vector(t(x_5[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(union,list(predicted,temp))
    temp <- as.vector(t(x_6[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(union,list(predicted,temp))
    
    c_temp[which(c_temp$user_pk==acc_list[i,]$user_pk),] <- cal_cor(predicted,actual,acc_list[i,]$user_pk)
    
    ints <- intersect(actual,predicted)
    rate <- length(ints)/length(actual)
    acc_list[i,]$predicted <- length(predicted)
    acc_list[i,]$inter <- length(ints)
    acc_list[i,]$actual <- length(actual)
    acc_list[i,]$rate <- rate
    
    ct <- c_temp[which(c_temp$user_pk==acc_list[i,]$user_pk),]
    acc_list[i,]$ints_sum <- sum(ct$ints)
    
    if(acc_list[i,]$ints_sum!=0){
      cor_pre <- Reduce(c,list(cor_pre,ct$predicted_score[which(ct$ints==1)]))
      cor_ac <- Reduce(c,list(cor_ac,ct$actual_score[which(ct$ints==1)]))
      acc_list[i,]$mean_abs <- mean(ct$abs[which(ct$ints==1)])
    }
  }
  if(length(which(is.na(cor_pre)))!=0){
    cor_ac <- cor_ac[-which(is.na(cor_pre))]
    cor_pre <- cor_pre[-which(is.na(cor_pre))]
  }
  acc_list$cor[which(acc_list$ints_sum!=0)] <- cor(cor_pre,cor_ac)
  return(acc_list)
}
accuracy_kma_frequency <- function(x){ 
  acc_list <- data.frame(cbind(unique(x[order(x$user_pk),1:2]),matrix(nrow=length(unique(x$user_pk)),ncol=8)))
  names(acc_list) <- c("user_pk","id","review_count","predicted","inter","actual","rate","ints_sum","cor","mean_abs")
  c_temp <- data.frame(cbind(dte[,1:3],matrix(nrow=nrow(dte),ncol=5)))
  names(c_temp) <- c("user_pk","id","review_count","title","predicted_score","actual_score","ints","abs")
  
  x_1 <- gek; x_2 <- grk; x_3 <- nak; x_4 <- gek2; x_5 <- grk2; x_6 <- nak2;
  
  cor_pre <- vector()
  cor_ac <- vector()
  for(i in 1:nrow(acc_list)){
    acc_list[i,]$review_count <- dtr[which(dtr$user_pk==acc_list[i,]$user_pk),]$review_count[1]
    actual <- as.character(x[which(x$user_pk==acc_list[i,]$user_pk),]$title)
    
    predicted <- as.vector(t(x_1[i,-c(1:3,which(x_1[i,]==""))]))
    temp <- as.vector(t(x_2[i,-c(1:3,which(x_2[i,]==""))]))
    predicted <- Reduce(c,list(predicted,temp))
    temp <- as.vector(t(x_3[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(c,list(predicted,temp))
    temp <- as.vector(t(x_4[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(c,list(predicted,temp))
    temp <- as.vector(t(x_5[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(c,list(predicted,temp))
    temp <- as.vector(t(x_6[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(c,list(predicted,temp))
    predicted <- names(head(sort(table(predicted),decreasing=TRUE),n=30))
    
    c_temp[which(c_temp$user_pk==acc_list[i,]$user_pk),] <- cal_cor(predicted,actual,acc_list[i,]$user_pk)
    
    ints <- intersect(actual,predicted)
    rate <- length(ints)/length(actual)
    acc_list[i,]$predicted <- length(predicted)
    acc_list[i,]$inter <- length(ints)
    acc_list[i,]$actual <- length(actual)
    acc_list[i,]$rate <- rate
    
    ct <- c_temp[which(c_temp$user_pk==acc_list[i,]$user_pk),]
    acc_list[i,]$ints_sum <- sum(ct$ints)
    
    if(acc_list[i,]$ints_sum!=0){
      cor_pre <- Reduce(c,list(cor_pre,ct$predicted_score[which(ct$ints==1)]))
      cor_ac <- Reduce(c,list(cor_ac,ct$actual_score[which(ct$ints==1)]))
      acc_list[i,]$mean_abs <- mean(ct$abs[which(ct$ints==1)])
    }
  }
  if(length(which(is.na(cor_pre)))!=0){
    cor_ac <- cor_ac[-which(is.na(cor_pre))]
    cor_pre <- cor_pre[-which(is.na(cor_pre))]
  }
  
  acc_list$cor[which(acc_list$ints_sum!=0)] <- cor(cor_pre,cor_ac)
  return(acc_list)
}

#cf
accuracy_cf_intersect <- function(x){ 
  acc_list <- data.frame(cbind(unique(x[order(x$user_pk),1:2]),matrix(nrow=length(unique(x$user_pk)),ncol=8)))
  names(acc_list) <- c("user_pk","id","review_count","predicted","inter","actual","rate","ints_sum","cor","mean_abs")
  c_temp <- data.frame(cbind(dte[,1:3],matrix(nrow=nrow(dte),ncol=5)))
  names(c_temp) <- c("user_pk","id","review_count","title","predicted_score","actual_score","ints","abs")
  
  x_1 <- alcf; x_2 <- gecf; x_3 <- grcf; x_4 <- nacf;
  
  cor_pre <- vector()
  cor_ac <- vector()
  for(i in 1:nrow(acc_list)){
    acc_list[i,]$review_count <- dtr[which(dtr$user_pk==acc_list[i,]$user_pk),]$review_count[1]
    actual <- as.character(x[which(x$user_pk==acc_list[i,]$user_pk),]$title)
    
    predicted <- as.vector(t(x_1[i,-c(1:3,which(x_1[i,]==""))]))
    temp <- as.vector(t(x_2[i,-c(1:3,which(x_2[i,]==""))]))
    predicted <- Reduce(intersect,list(predicted,temp))
    temp <- as.vector(t(x_3[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(intersect,list(predicted,temp))
    temp <- as.vector(t(x_4[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(intersect,list(predicted,temp))
    
    c_temp[which(c_temp$user_pk==acc_list[i,]$user_pk),] <- cal_cor(predicted,actual,acc_list[i,]$user_pk)
    
    ints <- intersect(actual,predicted)
    rate <- length(ints)/length(actual)
    acc_list[i,]$predicted <- length(predicted)
    acc_list[i,]$inter <- length(ints)
    acc_list[i,]$actual <- length(actual)
    acc_list[i,]$rate <- rate
    
    ct <- c_temp[which(c_temp$user_pk==acc_list[i,]$user_pk),]
    acc_list[i,]$ints_sum <- sum(ct$ints)
    
    if(acc_list[i,]$ints_sum!=0){
      cor_pre <- Reduce(c,list(cor_pre,ct$predicted_score[which(ct$ints==1)]))
      cor_ac <- Reduce(c,list(cor_ac,ct$actual_score[which(ct$ints==1)]))
      acc_list[i,]$mean_abs <- mean(ct$abs[which(ct$ints==1)])
    }
  }
  if(length(which(is.na(cor_pre)))!=0){
    cor_ac <- cor_ac[-which(is.na(cor_pre))]
    cor_pre <- cor_pre[-which(is.na(cor_pre))]
  }
  
  acc_list$cor[which(acc_list$ints_sum!=0)] <- cor(cor_pre,cor_ac)
  return(acc_list)
}
accuracy_cf_union <- function(x){ 
  acc_list <- data.frame(cbind(unique(x[order(x$user_pk),1:2]),matrix(nrow=length(unique(x$user_pk)),ncol=8)))
  names(acc_list) <- c("user_pk","id","review_count","predicted","inter","actual","rate","ints_sum","cor","mean_abs")
  c_temp <- data.frame(cbind(dte[,1:3],matrix(nrow=nrow(dte),ncol=5)))
  names(c_temp) <- c("user_pk","id","review_count","title","predicted_score","actual_score","ints","abs")
  
  x_1 <- alcf; x_2 <- gecf; x_3 <- grcf; x_4 <- nacf;
  
  cor_pre <- vector()
  cor_ac <- vector()
  for(i in 1:nrow(acc_list)){
    acc_list[i,]$review_count <- dtr[which(dtr$user_pk==acc_list[i,]$user_pk),]$review_count[1]
    actual <- as.character(x[which(x$user_pk==acc_list[i,]$user_pk),]$title)
    
    predicted <- as.vector(t(x_1[i,-c(1:3,which(x_1[i,]==""))]))
    temp <- as.vector(t(x_2[i,-c(1:3,which(x_2[i,]==""))]))
    predicted <- Reduce(union,list(predicted,temp))
    temp <- as.vector(t(x_3[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(union,list(predicted,temp))
    temp <- as.vector(t(x_4[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(union,list(predicted,temp))
    
    c_temp[which(c_temp$user_pk==acc_list[i,]$user_pk),] <- cal_cor(predicted,actual,acc_list[i,]$user_pk)
    
    ints <- intersect(actual,predicted)
    rate <- length(ints)/length(actual)
    acc_list[i,]$predicted <- length(predicted)
    acc_list[i,]$inter <- length(ints)
    acc_list[i,]$actual <- length(actual)
    acc_list[i,]$rate <- rate
    
    ct <- c_temp[which(c_temp$user_pk==acc_list[i,]$user_pk),]
    acc_list[i,]$ints_sum <- sum(ct$ints)
    
    if(acc_list[i,]$ints_sum!=0){
      cor_pre <- Reduce(c,list(cor_pre,ct$predicted_score[which(ct$ints==1)]))
      cor_ac <- Reduce(c,list(cor_ac,ct$actual_score[which(ct$ints==1)]))
      acc_list[i,]$mean_abs <- mean(ct$abs[which(ct$ints==1)])
    }
  }
  if(length(which(is.na(cor_pre)))!=0){
    cor_ac <- cor_ac[-which(is.na(cor_pre))]
    cor_pre <- cor_pre[-which(is.na(cor_pre))]
  }
  acc_list$cor[which(acc_list$ints_sum!=0)] <- cor(cor_pre,cor_ac)
  return(acc_list)
}
accuracy_cf_frequency <- function(x){ 
  acc_list <- data.frame(cbind(unique(x[order(x$user_pk),1:2]),matrix(nrow=length(unique(x$user_pk)),ncol=8)))
  names(acc_list) <- c("user_pk","id","review_count","predicted","inter","actual","rate","ints_sum","cor","mean_abs")
  c_temp <- data.frame(cbind(dte[,1:3],matrix(nrow=nrow(dte),ncol=5)))
  names(c_temp) <- c("user_pk","id","review_count","title","predicted_score","actual_score","ints","abs")
  
  x_1 <- alcf; x_2 <- gecf; x_3 <- grcf; x_4 <- nacf;
  
  cor_pre <- vector()
  cor_ac <- vector()
  for(i in 1:nrow(acc_list)){
    acc_list[i,]$review_count <- dtr[which(dtr$user_pk==acc_list[i,]$user_pk),]$review_count[1]
    actual <- as.character(x[which(x$user_pk==acc_list[i,]$user_pk),]$title)
    
    predicted <- as.vector(t(x_1[i,-c(1:3,which(x_1[i,]==""))]))
    temp <- as.vector(t(x_2[i,-c(1:3,which(x_2[i,]==""))]))
    predicted <- Reduce(c,list(predicted,temp))
    temp <- as.vector(t(x_3[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(c,list(predicted,temp))
    temp <- as.vector(t(x_4[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(c,list(predicted,temp))
    predicted <- names(head(sort(table(predicted),decreasing=TRUE),n=30))
    
    c_temp[which(c_temp$user_pk==acc_list[i,]$user_pk),] <- cal_cor(predicted,actual,acc_list[i,]$user_pk)
    
    ints <- intersect(actual,predicted)
    rate <- length(ints)/length(actual)
    acc_list[i,]$predicted <- length(predicted)
    acc_list[i,]$inter <- length(ints)
    acc_list[i,]$actual <- length(actual)
    acc_list[i,]$rate <- rate
    
    ct <- c_temp[which(c_temp$user_pk==acc_list[i,]$user_pk),]
    acc_list[i,]$ints_sum <- sum(ct$ints)
    
    if(acc_list[i,]$ints_sum!=0){
      cor_pre <- Reduce(c,list(cor_pre,ct$predicted_score[which(ct$ints==1)]))
      cor_ac <- Reduce(c,list(cor_ac,ct$actual_score[which(ct$ints==1)]))
      acc_list[i,]$mean_abs <- mean(ct$abs[which(ct$ints==1)])
    }
  }
  if(length(which(is.na(cor_pre)))!=0){
    cor_ac <- cor_ac[-which(is.na(cor_pre))]
    cor_pre <- cor_pre[-which(is.na(cor_pre))]
  }
  
  acc_list$cor[which(acc_list$ints_sum!=0)] <- cor(cor_pre,cor_ac)
  return(acc_list)
}

#메소드별 intersect/union/frequency 기반 리스트 재조정 후 예측률
accuracy_all_intersect <- function(x){ 
  acc_list <- data.frame(cbind(unique(x[order(x$user_pk),1:2]),matrix(nrow=length(unique(x$user_pk)),ncol=8)))
  names(acc_list) <- c("user_pk","id","review_count","predicted","inter","actual","rate","ints_sum","cor","mean_abs")
  c_temp <- data.frame(cbind(dte[,1:3],matrix(nrow=nrow(dte),ncol=5)))
  names(c_temp) <- c("user_pk","id","review_count","title","predicted_score","actual_score","ints","abs")
  
  x_1 <- gek; x_2 <- grk; x_3 <- nak; x_4 <- gek2; x_5 <- grk2; x_6 <- nak2;
  x_7 <- alcf; x_8 <- gecf; x_9 <- grcf; x_10 <- nacf; x_11 <- uar; x_12 <- ccbr;
  
  cor_pre <- vector()
  cor_ac <- vector()
  for(i in 1:nrow(acc_list)){
    acc_list[i,]$review_count <- dtr[which(dtr$user_pk==acc_list[i,]$user_pk),]$review_count[1]
    actual <- as.character(x[which(x$user_pk==acc_list[i,]$user_pk),]$title)
    
    predicted <- as.vector(t(x_1[i,-c(1:3,which(x_1[i,]==""))]))
    temp <- as.vector(t(x_2[i,-c(1:3,which(x_2[i,]==""))]))
    predicted <- Reduce(intersect,list(predicted,temp))
    temp <- as.vector(t(x_3[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(intersect,list(predicted,temp))
    temp <- as.vector(t(x_4[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(intersect,list(predicted,temp))
    temp <- as.vector(t(x_5[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(intersect,list(predicted,temp))
    temp <- as.vector(t(x_6[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(intersect,list(predicted,temp))
    temp <- as.vector(t(x_7[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(intersect,list(predicted,temp))
    temp <- as.vector(t(x_8[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(intersect,list(predicted,temp))
    temp <- as.vector(t(x_9[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(intersect,list(predicted,temp))
    temp <- as.vector(t(x_10[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(intersect,list(predicted,temp))
    temp <- as.vector(t(x_11[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(intersect,list(predicted,temp))
    temp <- as.vector(t(x_12[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(intersect,list(predicted,temp))
    
    c_temp[which(c_temp$user_pk==acc_list[i,]$user_pk),] <- cal_cor(predicted,actual,acc_list[i,]$user_pk)
    
    ints <- intersect(actual,predicted)
    rate <- length(ints)/length(actual)
    acc_list[i,]$predicted <- length(predicted)
    acc_list[i,]$inter <- length(ints)
    acc_list[i,]$actual <- length(actual)
    acc_list[i,]$rate <- rate
    
    ct <- c_temp[which(c_temp$user_pk==acc_list[i,]$user_pk),]
    acc_list[i,]$ints_sum <- sum(ct$ints)
    
    if(acc_list[i,]$ints_sum!=0){
      cor_pre <- Reduce(c,list(cor_pre,ct$predicted_score[which(ct$ints==1)]))
      cor_ac <- Reduce(c,list(cor_ac,ct$actual_score[which(ct$ints==1)]))
      acc_list[i,]$mean_abs <- mean(ct$abs[which(ct$ints==1)])
    }
  }
  if(length(which(is.na(cor_pre)))!=0){
    cor_ac <- cor_ac[-which(is.na(cor_pre))]
    cor_pre <- cor_pre[-which(is.na(cor_pre))]
  }
  
  acc_list$cor[which(acc_list$ints_sum!=0)] <- cor(cor_pre,cor_ac)
  return(acc_list)
}
accuracy_all_union <- function(x){ 
  acc_list <- data.frame(cbind(unique(x[order(x$user_pk),1:2]),matrix(nrow=length(unique(x$user_pk)),ncol=8)))
  names(acc_list) <- c("user_pk","id","review_count","predicted","inter","actual","rate","ints_sum","cor","mean_abs")
  c_temp <- data.frame(cbind(dte[,1:3],matrix(nrow=nrow(dte),ncol=5)))
  names(c_temp) <- c("user_pk","id","review_count","title","predicted_score","actual_score","ints","abs")
  
  x_1 <- gek; x_2 <- grk; x_3 <- nak; x_4 <- gek2; x_5 <- grk2; x_6 <- nak2;
  x_7 <- alcf; x_8 <- gecf; x_9 <- grcf; x_10 <- nacf; x_11 <- uar; x_12 <- ccbr;
  
  cor_pre <- vector()
  cor_ac <- vector()
  for(i in 1:nrow(acc_list)){
    acc_list[i,]$review_count <- dtr[which(dtr$user_pk==acc_list[i,]$user_pk),]$review_count[1]
    actual <- as.character(x[which(x$user_pk==acc_list[i,]$user_pk),]$title)
    
    predicted <- as.vector(t(x_1[i,-c(1:3,which(x_1[i,]==""))]))
    temp <- as.vector(t(x_2[i,-c(1:3,which(x_2[i,]==""))]))
    predicted <- Reduce(union,list(predicted,temp))
    temp <- as.vector(t(x_3[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(union,list(predicted,temp))
    temp <- as.vector(t(x_4[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(union,list(predicted,temp))
    temp <- as.vector(t(x_5[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(union,list(predicted,temp))
    temp <- as.vector(t(x_6[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(union,list(predicted,temp))
    temp <- as.vector(t(x_7[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(union,list(predicted,temp))
    temp <- as.vector(t(x_8[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(union,list(predicted,temp))
    temp <- as.vector(t(x_9[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(union,list(predicted,temp))
    temp <- as.vector(t(x_10[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(union,list(predicted,temp))
    temp <- as.vector(t(x_11[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(union,list(predicted,temp))
    temp <- as.vector(t(x_12[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(union,list(predicted,temp))
    
    c_temp[which(c_temp$user_pk==acc_list[i,]$user_pk),] <- cal_cor(predicted,actual,acc_list[i,]$user_pk)
    
    ints <- intersect(actual,predicted)
    rate <- length(ints)/length(actual)
    acc_list[i,]$predicted <- length(predicted)
    acc_list[i,]$inter <- length(ints)
    acc_list[i,]$actual <- length(actual)
    acc_list[i,]$rate <- rate
    
    ct <- c_temp[which(c_temp$user_pk==acc_list[i,]$user_pk),]
    acc_list[i,]$ints_sum <- sum(ct$ints)
    
    if(acc_list[i,]$ints_sum!=0){
      cor_pre <- Reduce(c,list(cor_pre,ct$predicted_score[which(ct$ints==1)]))
      cor_ac <- Reduce(c,list(cor_ac,ct$actual_score[which(ct$ints==1)]))
      acc_list[i,]$mean_abs <- mean(ct$abs[which(ct$ints==1)])
    }
  }
  if(length(which(is.na(cor_pre)))!=0){
    cor_ac <- cor_ac[-which(is.na(cor_pre))]
    cor_pre <- cor_pre[-which(is.na(cor_pre))]
  }
  acc_list$cor[which(acc_list$ints_sum!=0)] <- cor(cor_pre,cor_ac)
  return(acc_list)
}
accuracy_all_frequency <- function(x){ 
  acc_list <- data.frame(cbind(unique(x[order(x$user_pk),1:2]),matrix(nrow=length(unique(x$user_pk)),ncol=8)))
  names(acc_list) <- c("user_pk","id","review_count","predicted","inter","actual","rate","ints_sum","cor","mean_abs")
  c_temp <- data.frame(cbind(dte[,1:3],matrix(nrow=nrow(dte),ncol=5)))
  names(c_temp) <- c("user_pk","id","review_count","title","predicted_score","actual_score","ints","abs")
  
  x_1 <- gek; x_2 <- grk; x_3 <- nak; x_4 <- gek2; x_5 <- grk2; x_6 <- nak2;
  x_7 <- alcf; x_8 <- gecf; x_9 <- grcf; x_10 <- nacf; x_11 <- uar; x_12 <- ccbr;
  
  cor_pre <- vector()
  cor_ac <- vector()
  for(i in 1:nrow(acc_list)){
    acc_list[i,]$review_count <- dtr[which(dtr$user_pk==acc_list[i,]$user_pk),]$review_count[1]
    actual <- as.character(x[which(x$user_pk==acc_list[i,]$user_pk),]$title)
    
    predicted <- as.vector(t(x_1[i,-c(1:3,which(x_1[i,]==""))]))
    temp <- as.vector(t(x_2[i,-c(1:3,which(x_2[i,]==""))]))
    predicted <- Reduce(c,list(predicted,temp))
    temp <- as.vector(t(x_3[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(c,list(predicted,temp))
    temp <- as.vector(t(x_4[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(c,list(predicted,temp))
    temp <- as.vector(t(x_5[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(c,list(predicted,temp))
    temp <- as.vector(t(x_6[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(c,list(predicted,temp))
    temp <- as.vector(t(x_7[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(c,list(predicted,temp))
    temp <- as.vector(t(x_8[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(c,list(predicted,temp))
    temp <- as.vector(t(x_9[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(c,list(predicted,temp))
    temp <- as.vector(t(x_10[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(c,list(predicted,temp))
    temp <- as.vector(t(x_11[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(c,list(predicted,temp))
    temp <- as.vector(t(x_12[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(c,list(predicted,temp))
    
    predicted <- names(head(sort(table(predicted),decreasing=TRUE),n=30))
    
    c_temp[which(c_temp$user_pk==acc_list[i,]$user_pk),] <- cal_cor(predicted,actual,acc_list[i,]$user_pk)
    
    ints <- intersect(actual,predicted)
    rate <- length(ints)/length(actual)
    acc_list[i,]$predicted <- length(predicted)
    acc_list[i,]$inter <- length(ints)
    acc_list[i,]$actual <- length(actual)
    acc_list[i,]$rate <- rate
    
    ct <- c_temp[which(c_temp$user_pk==acc_list[i,]$user_pk),]
    acc_list[i,]$ints_sum <- sum(ct$ints)
    
    if(acc_list[i,]$ints_sum!=0){
      cor_pre <- Reduce(c,list(cor_pre,ct$predicted_score[which(ct$ints==1)]))
      cor_ac <- Reduce(c,list(cor_ac,ct$actual_score[which(ct$ints==1)]))
      acc_list[i,]$mean_abs <- mean(ct$abs[which(ct$ints==1)])
    }
  }
  if(length(which(is.na(cor_pre)))!=0){
    cor_ac <- cor_ac[-which(is.na(cor_pre))]
    cor_pre <- cor_pre[-which(is.na(cor_pre))]
  }
  
  acc_list$cor[which(acc_list$ints_sum!=0)] <- cor(cor_pre,cor_ac)
  return(acc_list)
}


ensemble_n <- 30
#kmeans, cf, uar 별로 union 후 frequency 적용
accuracy_ens1_frequency <- function(x){ 
  acc_list <- data.frame(cbind(unique(x[order(x$user_pk),1:2]),matrix(nrow=length(unique(x$user_pk)),ncol=8)))
  names(acc_list) <- c("user_pk","id","review_count","predicted","inter","actual","rate","ints_sum","cor","mean_abs")
  c_temp <- data.frame(cbind(dte[,1:3],matrix(nrow=nrow(dte),ncol=5)))
  names(c_temp) <- c("user_pk","id","review_count","title","predicted_score","actual_score","ints","abs")
  
  x_1 <- gek; x_2 <- grk; x_3 <- nak; x_4 <- gek2; x_5 <- grk2; x_6 <- nak2;
  x_7 <- alcf; x_8 <- gecf; x_9 <- grcf; x_10 <- nacf; x_11 <- uar; x_12 <- ccbr;
  
  cor_pre <- vector()
  cor_ac <- vector()
  for(i in 1:nrow(acc_list)){
    acc_list[i,]$review_count <- dtr[which(dtr$user_pk==acc_list[i,]$user_pk),]$review_count[1]
    actual <- as.character(x[which(x$user_pk==acc_list[i,]$user_pk),]$title)
    
    predicted <- vector()
    predicted <- as.vector(t(x_1[i,-c(1:3,which(x_1[i,]==""))]))
    temp <- as.vector(t(x_2[i,-c(1:3,which(x_2[i,]==""))]))
    predicted <- Reduce(union,list(predicted,temp))
    temp <- as.vector(t(x_3[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(union,list(predicted,temp))
    temp <- as.vector(t(x_4[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(union,list(predicted,temp))
    temp <- as.vector(t(x_5[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(union,list(predicted,temp))
    temp <- as.vector(t(x_6[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(union,list(predicted,temp))
    predicted1 <- predicted[!is.na(predicted)]
    
    predicted <- vector()
    predicted <- as.vector(t(x_7[i,-c(1:3,which(x_1[i,]==""))]))
    temp <- as.vector(t(x_8[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(union,list(predicted,temp))
    temp <- as.vector(t(x_9[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(union,list(predicted,temp))
    temp <- as.vector(t(x_10[i,-c(1:3,which(x_3[i,]==""))]))
    predicted2 <- Reduce(union,list(predicted,temp))
    predicted2 <- predicted2[!is.na(predicted2)]
    
    predicted <- vector()
    predicted3 <- as.vector(t(x_11[i,-c(1:3,which(x_1[i,]==""))]))
    predicted3 <- predicted3[!is.na(predicted3)]
    
    predicted_fin <- Reduce(c, list(predicted1,predicted2,predicted3))
    #predicted_fin <- Reduce(c, list(predicted1,predicted2))
    
    predicted <- vector()
    predicted <- names(head(sort(table(predicted_fin),decreasing=TRUE),n=ensemble_n))
    
    c_temp[which(c_temp$user_pk==acc_list[i,]$user_pk),] <- cal_cor(predicted,actual,acc_list[i,]$user_pk)
    
    ints <- intersect(actual,predicted)
    rate <- length(ints)/length(actual)
    acc_list[i,]$predicted <- length(predicted)
    acc_list[i,]$inter <- length(ints)
    acc_list[i,]$actual <- length(actual)
    acc_list[i,]$rate <- rate
    
    ct <- c_temp[which(c_temp$user_pk==acc_list[i,]$user_pk),]
    acc_list[i,]$ints_sum <- sum(ct$ints)
    
    if(acc_list[i,]$ints_sum!=0){
      cor_pre <- Reduce(c,list(cor_pre,ct$predicted_score[which(ct$ints==1)]))
      cor_ac <- Reduce(c,list(cor_ac,ct$actual_score[which(ct$ints==1)]))
      acc_list[i,]$mean_abs <- mean(ct$abs[which(ct$ints==1)])
    }
  }
  if(length(which(is.na(cor_pre)))!=0){
    cor_ac <- cor_ac[-which(is.na(cor_pre))]
    cor_pre <- cor_pre[-which(is.na(cor_pre))]
  }
  
  acc_list$cor[which(acc_list$ints_sum!=0)] <- cor(cor_pre,cor_ac)
  return(acc_list)
}
#kmeans, cf, uar 별로 vector를 합친 후 frequency를 한번에 적용
accuracy_ens2_frequency <- function(x){ 
  acc_list <- data.frame(cbind(unique(x[order(x$user_pk),1:2]),matrix(nrow=length(unique(x$user_pk)),ncol=8)))
  names(acc_list) <- c("user_pk","id","review_count","predicted","inter","actual","rate","ints_sum","cor","mean_abs")
  c_temp <- data.frame(cbind(dte[,1:3],matrix(nrow=nrow(dte),ncol=5)))
  names(c_temp) <- c("user_pk","id","review_count","title","predicted_score","actual_score","ints","abs")
  
  x_1 <- gek; x_2 <- grk; x_3 <- nak; x_4 <- gek2; x_5 <- grk2; x_6 <- nak2;
  x_7 <- alcf; x_8 <- gecf; x_9 <- grcf; x_10 <- nacf; x_11 <- uar; x_12 <- ccbr;
  
  cor_pre <- vector()
  cor_ac <- vector()
  for(i in 1:nrow(acc_list)){
    acc_list[i,]$review_count <- dtr[which(dtr$user_pk==acc_list[i,]$user_pk),]$review_count[1]
    actual <- as.character(x[which(x$user_pk==acc_list[i,]$user_pk),]$title)
    
    predicted <- vector()
    predicted <- as.vector(t(x_1[i,-c(1:3,which(x_1[i,]==""))]))
    temp <- as.vector(t(x_2[i,-c(1:3,which(x_2[i,]==""))]))
    predicted <- Reduce(c,list(predicted,temp))
    temp <- as.vector(t(x_3[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(c,list(predicted,temp))
    temp <- as.vector(t(x_4[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(c,list(predicted,temp))
    temp <- as.vector(t(x_5[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(c,list(predicted,temp))
    temp <- as.vector(t(x_6[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(union,list(predicted,temp))
    predicted1 <- predicted[!is.na(predicted)]
    
    predicted <- vector()
    predicted <- as.vector(t(x_7[i,-c(1:3,which(x_1[i,]==""))]))
    temp <- as.vector(t(x_8[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(c,list(predicted,temp))
    temp <- as.vector(t(x_9[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(c,list(predicted,temp))
    temp <- as.vector(t(x_10[i,-c(1:3,which(x_3[i,]==""))]))
    predicted2 <- Reduce(c,list(predicted,temp))
    predicted2 <- predicted2[!is.na(predicted2)]
    
    predicted <- vector()
    predicted3 <- as.vector(t(x_11[i,-c(1:3,which(x_1[i,]==""))]))
    predicted3 <- predicted3[!is.na(predicted3)]
    
    predicted_fin <- Reduce(c, list(predicted1,predicted2,predicted3))
    #predicted_fin <- Reduce(c, list(predicted1,predicted2))
    
    predicted <- vector()
    predicted <- names(head(sort(table(predicted_fin),decreasing=TRUE),n=ensemble_n))
    
    c_temp[which(c_temp$user_pk==acc_list[i,]$user_pk),] <- cal_cor(predicted,actual,acc_list[i,]$user_pk)
    
    ints <- intersect(actual,predicted)
    rate <- length(ints)/length(actual)
    acc_list[i,]$predicted <- length(predicted)
    acc_list[i,]$inter <- length(ints)
    acc_list[i,]$actual <- length(actual)
    acc_list[i,]$rate <- rate
    
    ct <- c_temp[which(c_temp$user_pk==acc_list[i,]$user_pk),]
    acc_list[i,]$ints_sum <- sum(ct$ints)
    
    if(acc_list[i,]$ints_sum!=0){
      cor_pre <- Reduce(c,list(cor_pre,ct$predicted_score[which(ct$ints==1)]))
      cor_ac <- Reduce(c,list(cor_ac,ct$actual_score[which(ct$ints==1)]))
      acc_list[i,]$mean_abs <- mean(ct$abs[which(ct$ints==1)])
    }
  }
  if(length(which(is.na(cor_pre)))!=0){
    cor_ac <- cor_ac[-which(is.na(cor_pre))]
    cor_pre <- cor_pre[-which(is.na(cor_pre))]
  }
  
  acc_list$cor[which(acc_list$ints_sum!=0)] <- cor(cor_pre,cor_ac)
  return(acc_list)
}

#kmeans, cf 별로 union 후 frequency 적용
accuracy_ens3_frequency <- function(x){ 
  acc_list <- data.frame(cbind(unique(x[order(x$user_pk),1:2]),matrix(nrow=length(unique(x$user_pk)),ncol=8)))
  names(acc_list) <- c("user_pk","id","review_count","predicted","inter","actual","rate","ints_sum","cor","mean_abs")
  c_temp <- data.frame(cbind(dte[,1:3],matrix(nrow=nrow(dte),ncol=5)))
  names(c_temp) <- c("user_pk","id","review_count","title","predicted_score","actual_score","ints","abs")
  
  x_1 <- gek; x_2 <- grk; x_3 <- nak; x_4 <- gek2; x_5 <- grk2; x_6 <- nak2;
  x_7 <- alcf; x_8 <- gecf; x_9 <- grcf; x_10 <- nacf; x_11 <- uar; x_12 <- ccbr;
  
  cor_pre <- vector()
  cor_ac <- vector()
  for(i in 1:nrow(acc_list)){
    acc_list[i,]$review_count <- dtr[which(dtr$user_pk==acc_list[i,]$user_pk),]$review_count[1]
    actual <- as.character(x[which(x$user_pk==acc_list[i,]$user_pk),]$title)
    
    predicted <- vector()
    predicted <- as.vector(t(x_1[i,-c(1:3,which(x_1[i,]==""))]))
    temp <- as.vector(t(x_2[i,-c(1:3,which(x_2[i,]==""))]))
    predicted <- Reduce(union,list(predicted,temp))
    temp <- as.vector(t(x_3[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(union,list(predicted,temp))
    temp <- as.vector(t(x_4[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(union,list(predicted,temp))
    temp <- as.vector(t(x_5[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(union,list(predicted,temp))
    temp <- as.vector(t(x_6[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(union,list(predicted,temp))
    predicted1 <- predicted[!is.na(predicted)]
    
    predicted <- vector()
    predicted <- as.vector(t(x_7[i,-c(1:3,which(x_1[i,]==""))]))
    temp <- as.vector(t(x_8[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(union,list(predicted,temp))
    temp <- as.vector(t(x_9[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(union,list(predicted,temp))
    temp <- as.vector(t(x_10[i,-c(1:3,which(x_3[i,]==""))]))
    predicted2 <- Reduce(union,list(predicted,temp))
    predicted2 <- predicted2[!is.na(predicted2)]
    
    #predicted <- vector()
    #predicted3 <- as.vector(t(x_11[i,-c(1:3,which(x_1[i,]==""))]))
    #predicted3 <- predicted3[!is.na(predicted3)]
    
    #predicted_fin <- Reduce(c, list(predicted1,predicted2,predicted3))
    predicted_fin <- Reduce(c, list(predicted1,predicted2))
    
    predicted <- vector()
    predicted <- names(head(sort(table(predicted_fin),decreasing=TRUE),n=ensemble_n))
    
    c_temp[which(c_temp$user_pk==acc_list[i,]$user_pk),] <- cal_cor(predicted,actual,acc_list[i,]$user_pk)
    
    ints <- intersect(actual,predicted)
    rate <- length(ints)/length(actual)
    acc_list[i,]$predicted <- length(predicted)
    acc_list[i,]$inter <- length(ints)
    acc_list[i,]$actual <- length(actual)
    acc_list[i,]$rate <- rate
    
    ct <- c_temp[which(c_temp$user_pk==acc_list[i,]$user_pk),]
    acc_list[i,]$ints_sum <- sum(ct$ints)
    
    if(acc_list[i,]$ints_sum!=0){
      cor_pre <- Reduce(c,list(cor_pre,ct$predicted_score[which(ct$ints==1)]))
      cor_ac <- Reduce(c,list(cor_ac,ct$actual_score[which(ct$ints==1)]))
      acc_list[i,]$mean_abs <- mean(ct$abs[which(ct$ints==1)])
    }
  }
  if(length(which(is.na(cor_pre)))!=0){
    cor_ac <- cor_ac[-which(is.na(cor_pre))]
    cor_pre <- cor_pre[-which(is.na(cor_pre))]
  }
  
  acc_list$cor[which(acc_list$ints_sum!=0)] <- cor(cor_pre,cor_ac)
  return(acc_list)
}
#kmeans, cf 별로 vector를 합친 후 frequency를 한번에 적용
accuracy_ens4_frequency <- function(x){ 
  acc_list <- data.frame(cbind(unique(x[order(x$user_pk),1:2]),matrix(nrow=length(unique(x$user_pk)),ncol=8)))
  names(acc_list) <- c("user_pk","id","review_count","predicted","inter","actual","rate","ints_sum","cor","mean_abs")
  c_temp <- data.frame(cbind(dte[,1:3],matrix(nrow=nrow(dte),ncol=5)))
  names(c_temp) <- c("user_pk","id","review_count","title","predicted_score","actual_score","ints","abs")
  
  x_1 <- gek; x_2 <- grk; x_3 <- nak; x_4 <- gek2; x_5 <- grk2; x_6 <- nak2;
  x_7 <- alcf; x_8 <- gecf; x_9 <- grcf; x_10 <- nacf; x_11 <- uar; x_12 <- ccbr;
  
  cor_pre <- vector()
  cor_ac <- vector()
  for(i in 1:nrow(acc_list)){
    acc_list[i,]$review_count <- dtr[which(dtr$user_pk==acc_list[i,]$user_pk),]$review_count[1]
    actual <- as.character(x[which(x$user_pk==acc_list[i,]$user_pk),]$title)
    
    predicted <- vector()
    predicted <- as.vector(t(x_1[i,-c(1:3,which(x_1[i,]==""))]))
    temp <- as.vector(t(x_2[i,-c(1:3,which(x_2[i,]==""))]))
    predicted <- Reduce(c,list(predicted,temp))
    temp <- as.vector(t(x_3[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(c,list(predicted,temp))
    temp <- as.vector(t(x_4[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(c,list(predicted,temp))
    temp <- as.vector(t(x_5[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(c,list(predicted,temp))
    temp <- as.vector(t(x_6[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(union,list(predicted,temp))
    predicted1 <- predicted[!is.na(predicted)]
    
    predicted <- vector()
    predicted <- as.vector(t(x_7[i,-c(1:3,which(x_1[i,]==""))]))
    temp <- as.vector(t(x_8[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(c,list(predicted,temp))
    temp <- as.vector(t(x_9[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(c,list(predicted,temp))
    temp <- as.vector(t(x_10[i,-c(1:3,which(x_3[i,]==""))]))
    predicted2 <- Reduce(c,list(predicted,temp))
    predicted2 <- predicted2[!is.na(predicted2)]
    
    #predicted <- vector()
    #predicted3 <- as.vector(t(x_11[i,-c(1:3,which(x_1[i,]==""))]))
    #predicted3 <- predicted3[!is.na(predicted3)]
    
    #predicted_fin <- Reduce(c, list(predicted1,predicted2,predicted3))
    predicted_fin <- Reduce(c, list(predicted1,predicted2))
    
    predicted <- vector()
    predicted <- names(head(sort(table(predicted_fin),decreasing=TRUE),n=ensemble_n))
    
    c_temp[which(c_temp$user_pk==acc_list[i,]$user_pk),] <- cal_cor(predicted,actual,acc_list[i,]$user_pk)
    
    ints <- intersect(actual,predicted)
    rate <- length(ints)/length(actual)
    acc_list[i,]$predicted <- length(predicted)
    acc_list[i,]$inter <- length(ints)
    acc_list[i,]$actual <- length(actual)
    acc_list[i,]$rate <- rate
    
    ct <- c_temp[which(c_temp$user_pk==acc_list[i,]$user_pk),]
    acc_list[i,]$ints_sum <- sum(ct$ints)
    
    if(acc_list[i,]$ints_sum!=0){
      cor_pre <- Reduce(c,list(cor_pre,ct$predicted_score[which(ct$ints==1)]))
      cor_ac <- Reduce(c,list(cor_ac,ct$actual_score[which(ct$ints==1)]))
      acc_list[i,]$mean_abs <- mean(ct$abs[which(ct$ints==1)])
    }
  }
  if(length(which(is.na(cor_pre)))!=0){
    cor_ac <- cor_ac[-which(is.na(cor_pre))]
    cor_pre <- cor_pre[-which(is.na(cor_pre))]
  }
  
  acc_list$cor[which(acc_list$ints_sum!=0)] <- cor(cor_pre,cor_ac)
  return(acc_list)
}

#km1, cf 별로 union 후 frequency 적용
accuracy_ens5_frequency <- function(x){ 
  acc_list <- data.frame(cbind(unique(x[order(x$user_pk),1:2]),matrix(nrow=length(unique(x$user_pk)),ncol=8)))
  names(acc_list) <- c("user_pk","id","review_count","predicted","inter","actual","rate","ints_sum","cor","mean_abs")
  c_temp <- data.frame(cbind(dte[,1:3],matrix(nrow=nrow(dte),ncol=5)))
  names(c_temp) <- c("user_pk","id","review_count","title","predicted_score","actual_score","ints","abs")
  
  x_1 <- gek; x_2 <- grk; x_3 <- nak; x_4 <- gek2; x_5 <- grk2; x_6 <- nak2;
  x_7 <- alcf; x_8 <- gecf; x_9 <- grcf; x_10 <- nacf; x_11 <- uar; x_12 <- ccbr;
  
  cor_pre <- vector()
  cor_ac <- vector()
  for(i in 1:nrow(acc_list)){
    acc_list[i,]$review_count <- dtr[which(dtr$user_pk==acc_list[i,]$user_pk),]$review_count[1]
    actual <- as.character(x[which(x$user_pk==acc_list[i,]$user_pk),]$title)
    
    predicted <- vector()
    predicted <- as.vector(t(x_1[i,-c(1:3,which(x_1[i,]==""))]))
    temp <- as.vector(t(x_2[i,-c(1:3,which(x_2[i,]==""))]))
    predicted <- Reduce(union,list(predicted,temp))
    temp <- as.vector(t(x_3[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(union,list(predicted,temp))
    #temp <- as.vector(t(x_4[i,-c(1:3,which(x_3[i,]==""))]))
    #predicted <- Reduce(union,list(predicted,temp))
    #temp <- as.vector(t(x_5[i,-c(1:3,which(x_3[i,]==""))]))
    #predicted <- Reduce(union,list(predicted,temp))
    #temp <- as.vector(t(x_6[i,-c(1:3,which(x_3[i,]==""))]))
    #predicted <- Reduce(union,list(predicted,temp))
    predicted1 <- predicted[!is.na(predicted)]
    
    predicted <- vector()
    predicted <- as.vector(t(x_7[i,-c(1:3,which(x_1[i,]==""))]))
    temp <- as.vector(t(x_8[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(union,list(predicted,temp))
    temp <- as.vector(t(x_9[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(union,list(predicted,temp))
    temp <- as.vector(t(x_10[i,-c(1:3,which(x_3[i,]==""))]))
    predicted2 <- Reduce(union,list(predicted,temp))
    predicted2 <- predicted2[!is.na(predicted2)]
    
    predicted <- vector()
    predicted3 <- as.vector(t(x_11[i,-c(1:3,which(x_1[i,]==""))]))
    predicted3 <- predicted3[!is.na(predicted3)]
    
    predicted_fin <- Reduce(c, list(predicted1,predicted2,predicted3))
    #predicted_fin <- Reduce(c, list(predicted1,predicted2))
    
    predicted <- vector()
    predicted <- names(head(sort(table(predicted_fin),decreasing=TRUE),n=ensemble_n))
    
    c_temp[which(c_temp$user_pk==acc_list[i,]$user_pk),] <- cal_cor(predicted,actual,acc_list[i,]$user_pk)
    
    ints <- intersect(actual,predicted)
    rate <- length(ints)/length(actual)
    acc_list[i,]$predicted <- length(predicted)
    acc_list[i,]$inter <- length(ints)
    acc_list[i,]$actual <- length(actual)
    acc_list[i,]$rate <- rate
    
    ct <- c_temp[which(c_temp$user_pk==acc_list[i,]$user_pk),]
    acc_list[i,]$ints_sum <- sum(ct$ints)
    
    if(acc_list[i,]$ints_sum!=0){
      cor_pre <- Reduce(c,list(cor_pre,ct$predicted_score[which(ct$ints==1)]))
      cor_ac <- Reduce(c,list(cor_ac,ct$actual_score[which(ct$ints==1)]))
      acc_list[i,]$mean_abs <- mean(ct$abs[which(ct$ints==1)])
    }
  }
  if(length(which(is.na(cor_pre)))!=0){
    cor_ac <- cor_ac[-which(is.na(cor_pre))]
    cor_pre <- cor_pre[-which(is.na(cor_pre))]
  }
  
  acc_list$cor[which(acc_list$ints_sum!=0)] <- cor(cor_pre,cor_ac)
  return(acc_list)
}
#km1, cf 별로 vector를 합친 후 frequency를 한번에 적용
accuracy_ens6_frequency <- function(x){ 
  acc_list <- data.frame(cbind(unique(x[order(x$user_pk),1:2]),matrix(nrow=length(unique(x$user_pk)),ncol=8)))
  names(acc_list) <- c("user_pk","id","review_count","predicted","inter","actual","rate","ints_sum","cor","mean_abs")
  c_temp <- data.frame(cbind(dte[,1:3],matrix(nrow=nrow(dte),ncol=5)))
  names(c_temp) <- c("user_pk","id","review_count","title","predicted_score","actual_score","ints","abs")
  
  x_1 <- gek; x_2 <- grk; x_3 <- nak; x_4 <- gek2; x_5 <- grk2; x_6 <- nak2;
  x_7 <- alcf; x_8 <- gecf; x_9 <- grcf; x_10 <- nacf; x_11 <- uar; x_12 <- ccbr;
  
  cor_pre <- vector()
  cor_ac <- vector()
  for(i in 1:nrow(acc_list)){
    acc_list[i,]$review_count <- dtr[which(dtr$user_pk==acc_list[i,]$user_pk),]$review_count[1]
    actual <- as.character(x[which(x$user_pk==acc_list[i,]$user_pk),]$title)
    
    predicted <- vector()
    predicted <- as.vector(t(x_1[i,-c(1:3,which(x_1[i,]==""))]))
    temp <- as.vector(t(x_2[i,-c(1:3,which(x_2[i,]==""))]))
    predicted <- Reduce(c,list(predicted,temp))
    temp <- as.vector(t(x_3[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(c,list(predicted,temp))
    #temp <- as.vector(t(x_4[i,-c(1:3,which(x_3[i,]==""))]))
    #predicted <- Reduce(c,list(predicted,temp))
    #temp <- as.vector(t(x_5[i,-c(1:3,which(x_3[i,]==""))]))
    #predicted <- Reduce(c,list(predicted,temp))
    #temp <- as.vector(t(x_6[i,-c(1:3,which(x_3[i,]==""))]))
    #predicted <- Reduce(union,list(predicted,temp))
    predicted1 <- predicted[!is.na(predicted)]
    
    predicted <- vector()
    predicted <- as.vector(t(x_7[i,-c(1:3,which(x_1[i,]==""))]))
    temp <- as.vector(t(x_8[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(c,list(predicted,temp))
    temp <- as.vector(t(x_9[i,-c(1:3,which(x_3[i,]==""))]))
    predicted <- Reduce(c,list(predicted,temp))
    temp <- as.vector(t(x_10[i,-c(1:3,which(x_3[i,]==""))]))
    predicted2 <- Reduce(c,list(predicted,temp))
    predicted2 <- predicted2[!is.na(predicted2)]
    
    predicted <- vector()
    predicted3 <- as.vector(t(x_11[i,-c(1:3,which(x_1[i,]==""))]))
    predicted3 <- predicted3[!is.na(predicted3)]
    
    predicted_fin <- Reduce(c, list(predicted1,predicted2,predicted3))
    #predicted_fin <- Reduce(c, list(predicted1,predicted2))
    
    predicted <- vector()
    predicted <- names(head(sort(table(predicted_fin),decreasing=TRUE),n=ensemble_n))
    
    c_temp[which(c_temp$user_pk==acc_list[i,]$user_pk),] <- cal_cor(predicted,actual,acc_list[i,]$user_pk)
    
    ints <- intersect(actual,predicted)
    rate <- length(ints)/length(actual)
    acc_list[i,]$predicted <- length(predicted)
    acc_list[i,]$inter <- length(ints)
    acc_list[i,]$actual <- length(actual)
    acc_list[i,]$rate <- rate
    
    ct <- c_temp[which(c_temp$user_pk==acc_list[i,]$user_pk),]
    acc_list[i,]$ints_sum <- sum(ct$ints)
    
    if(acc_list[i,]$ints_sum!=0){
      cor_pre <- Reduce(c,list(cor_pre,ct$predicted_score[which(ct$ints==1)]))
      cor_ac <- Reduce(c,list(cor_ac,ct$actual_score[which(ct$ints==1)]))
      acc_list[i,]$mean_abs <- mean(ct$abs[which(ct$ints==1)])
    }
  }
  if(length(which(is.na(cor_pre)))!=0){
    cor_ac <- cor_ac[-which(is.na(cor_pre))]
    cor_pre <- cor_pre[-which(is.na(cor_pre))]
  }
  
  acc_list$cor[which(acc_list$ints_sum!=0)] <- cor(cor_pre,cor_ac)
  return(acc_list)
}
#---------------------#Predicting rate calculation function#---------------------#


#---------------------#making Result report#---------------------#
input_result <- function(x){
  temp <- data.frame(matrix(nrow=1,ncol=20))
  if(TRUE %in% is.nan(x$rate)){
    x$rate[is.nan(x$rate)] <- NA
    temp[1,17] <- "Y"
  }
  else{temp[1,17] <- "N"}
  
  temp[1,1] <- round(mean(x$predicted),6)
  temp[1,2] <- min(x$predicted)
  temp[1,3] <- max(x$predicted)
  temp[1,4] <- mean(x$inter)
  temp[1,5] <- min(x$inter)
  temp[1,6] <- max(x$inter)
  temp[1,7] <- mean(x$actual)
  temp[1,8] <- min(x$actual)
  temp[1,9] <- max(x$actual)
  temp[1,10] <- length(x$rate[x$rate!=0])
  temp[1,11] <- length(x$rate[x$rate==0])
  temp[1,12] <- round(length(x$rate[x$rate==0])/length(x$rate),3)
  temp[1,13] <- mean(x$rate, na.rm=TRUE)
  temp[1,14] <- mean(x$rate[x$rate>0],na.rm=TRUE)
  temp[1,15] <- max(x$rate,na.rm=TRUE)
  temp[1,16] <- min(x$rate,na.rm=TRUE)
  
  temp[1,18] <- sum(x$ints_sum)
  temp[1,19] <- x$cor[!is.na(x$cor)][1]
  temp[1,20] <- mean(x$mean_abs[!is.na(x$mean_abs)])
  
  return(temp)
}

result_report <- data.frame(matrix(nrow=45,ncol=21))
names(result_report) <- c("method","predicted_mean","predicted_min","predicted_max","intersect_mean","intersect_min","intersect_max","actual_mean","actual_min","actual_max","length( !=0)","length ( ==0)","0_ratio","rate_mean","rate ( >0)","rate_max","rate_min","NaN","ints_length","pre_ac_cor","mean_abs")
method_names <- c("random_rec","score_rec","count_rec","alcf","gecf","grcf","nacf","gek","gek2","grk","grk2","nak","nak2",
                  "uar","ccbr","k_m_inter","k_m_union","k_m_freq","k_m2_inter","k_m2_union","k_m2_freq",
                  "k_m_all_inter","k_m_all_union","k_m_all_freq","cf_inter","cf_union","cf_freq",
                  "all_intersect","all_union","all_freq","30_u_freq","30_c_freq","30_u_freq_wo_uar","30_c_freq_wo_uar",
                  "30_u_freq_wo_uar_km2","30_c_freq_wo_uar_km2","60_u_freq","60_c_freq","60_u_freq_wo_uar","60_c_freq_wo_uar",
                  "60_u_freq_wo_uar_km2","60_c_freq_wo_uar_km2")
for(i in 1:length(method_names)){
  result_report[i,1] <- method_names[i]
}
#View(result_report)
result_report[1,2:21] <- input_result(accuracy_cal(dte,rar))
result_report[2,2:21] <- input_result(accuracy_cal(dte,scr))
result_report[3,2:21] <- input_result(accuracy_cal(dte,cor))
result_report[4,2:21] <- input_result(accuracy_cal(dte,alcf))
result_report[5,2:21] <- input_result(accuracy_cal(dte,gecf))
result_report[6,2:21] <- input_result(accuracy_cal(dte,grcf))
result_report[7,2:21] <- input_result(accuracy_cal(dte,nacf))
result_report[8,2:21] <- input_result(accuracy_cal(dte,gek))
result_report[9,2:21] <- input_result(accuracy_cal(dte,gek2))
result_report[10,2:21] <- input_result(accuracy_cal(dte,grk))
result_report[11,2:21] <- input_result(accuracy_cal(dte,grk2))
result_report[12,2:21] <- input_result(accuracy_cal(dte,nak))
result_report[13,2:21] <- input_result(accuracy_cal(dte,nak2))
result_report[14,2:21] <- input_result(accuracy_cal(dte,uar))
result_report[15,2:21] <- input_result(accuracy_cal(dte,ccbr))
#View(result_report)

#km1 accuracy report
result_report[16,2:21] <- input_result(accuracy_km1_intersect(dte))
result_report[17,2:21] <- input_result(accuracy_km1_union(dte))
result_report[18,2:21] <- input_result(accuracy_km1_frequency(dte))

#km2 accuracy report
result_report[19,2:21] <- input_result(accuracy_km2_intersect(dte))
result_report[20,2:21] <- input_result(accuracy_km2_union(dte))
result_report[21,2:21] <- input_result(accuracy_km2_frequency(dte))

#kma accuracy report
result_report[22,2:21] <- input_result(accuracy_kma_intersect(dte))
result_report[23,2:21] <- input_result(accuracy_kma_union(dte))
result_report[24,2:21] <- input_result(accuracy_kma_frequency(dte))
#View(result_report)

#cf accuracy report
result_report[25,2:21] <- input_result(accuracy_kma_intersect(dte))
result_report[26,2:21] <- input_result(accuracy_kma_union(dte))
result_report[27,2:21] <- input_result(accuracy_kma_frequency(dte))

#all ensemble accuracy report
result_report[28,2:21] <- input_result(accuracy_all_intersect(dte))
result_report[29,2:21] <- input_result(accuracy_all_union(dte))
result_report[30,2:21] <- input_result(accuracy_all_frequency(dte))

#other ensemble
#ensemble 30
ensemble_n <- 30
result_report[31,2:21] <- input_result(accuracy_ens1_frequency(dte)) #union 후 freq
result_report[32,2:21] <- input_result(accuracy_ens2_frequency(dte)) #c 결합 후 freq
result_report[33,2:21] <- input_result(accuracy_ens3_frequency(dte)) #union 후 freq w/o uar
result_report[34,2:21] <- input_result(accuracy_ens4_frequency(dte)) #c 결합 후 freq w/o uar
result_report[35,2:21] <- input_result(accuracy_ens5_frequency(dte)) #union 후 freq w/o uar,km2
result_report[36,2:21] <- input_result(accuracy_ens6_frequency(dte)) #c 결합 후 freq w/o uar,km2

#ensemble 60
ensemble_n <- 60
result_report[37,2:21] <- input_result(accuracy_ens1_frequency(dte)) #union 후 freq
result_report[38,2:21] <- input_result(accuracy_ens2_frequency(dte)) #c 결합 후 freq
result_report[39,2:21] <- input_result(accuracy_ens3_frequency(dte)) #union 후 freq w/o uar
result_report[40,2:21] <- input_result(accuracy_ens4_frequency(dte)) #c 결합 후 freq w/o uar
result_report[41,2:21] <- input_result(accuracy_ens5_frequency(dte)) #union 후 freq w/o uar,km2
result_report[42,2:21] <- input_result(accuracy_ens6_frequency(dte)) #c 결합 후 freq w/o uar,km2

View(result_report)
write.csv(result_report, file="/Users/hodong/Desktop/result_report.csv",row.names=FALSE)
#---------------------#making Result report#---------------------#

#---------------------#Cor Result report#---------------------#
making_cor <- function(x,xs){
  cor_report <- data.frame(matrix(nrow=nrow(dte),ncol=7))
  names(cor_report) <- c("user_pk","id","review_count","title","predicted_score","actual_score","abs")
  #for(i in 1:nrow(dte)){
  for(i in 1:nrow(dte)){
    cor_report[i,]$user_pk <- dte[i,]$user_pk
    cor_report[i,]$id <- as.character(dte[i,]$id)
    cor_report[i,]$review_count <- dte[i,]$review_count
    cor_report[i,]$title <- as.character(dte[i,]$title)
    cor_report[i,]$actual_score <- dp[which(dp$user_pk==cor_report[i,]$user_pk & dp$title==cor_report[i,]$title),]$score
    actual <- cor_report[i,]$title
    temp <- as.vector(t(x[which(x$user_pk==i),-c(1:3)]))
    predicted <- temp[!is.na(temp)]
    ints <- Reduce(intersect,list(actual,predicted))
    
    if(length(ints)!=0){
      cor_report[i,]$predicted_score <- xs[which(x$user_pk==i),(which(predicted==actual)+3)]
      cor_report[i,]$abs <- abs(cor_report[i,]$actual_score-cor_report[i,]$predicted_score)
    }
  }
  return(cor_report)
}
input_cor <- function(x,xs){
  cor_report <- making_cor(x,xs)
  temp <- cor_report[!is.na(cor_report$abs),]
  score_cor <- cor(temp$predicted_score,temp$actual_score)
  return(score_cor)
}

cor_report <- making_cor(alcf,alcfs)
temp <- cor_report[!is.na(cor_report$abs),]
cor(temp$predicted_score,temp$actual_score)
View(cor_report)
View(temp)
#---------------------#Cor Result report#---------------------#

#---------------------#making Analysis report#---------------------#
#각 방법을 이용했을 때, training_set 구간에 따른 예측률 평균값 확인
making_ana_report <- function(acc_list){
  tam2 <- data.frame(cbind(acc_list$review_count,matrix(nrow=nrow(acc_list),ncol=1),acc_list$rate))
  names(tam2) <- c("review_count","review_interval","rate")
  for(i in 1:nrow(tam2)){
    tam2[i,]$review_interval <- floor((tam2[i,]$review_count)/10)*10
  }
  tam3 <- tam2
  count_analysis2 <- aggregate(data=tam2,rate~review_interval,mean)
  tam3$rate[tam3$rate==0] <- NA
  count_analysis3 <- aggregate(data=tam3,rate~review_interval,mean)
  ana_report <- data.frame(cbind(count_analysis2,count_analysis3$rate),matrix(nrow=nrow(count_analysis2),ncol=1))
  names(ana_report) <- c("review_interval","rate","rate [>0]","rate_0")
  for(i in as.vector(sort(unique(tam2$review_interval),decreasing=FALSE))){
    tca <- tam2[which(tam2$review_interval==i),]$rate
    #print(length(tca[tca==0])/length(tca))
    ana_report[which(ana_report$review_interval==i),]$rate_0 <- length(tca[tca==0])/length(tca)
  }
  temp_report <- data.frame(matrix(nrow=1,ncol=3))
  temp_report[,1] <- cor(ana_report$review_interval,ana_report$rate)
  temp_report[,2] <- cor(ana_report$review_interval,ana_report$`rate [>0]`)
  temp_report[,3] <- cor(ana_report$review_interval,ana_report$rate_0)
  
  return(temp_report)
}

ana_report <- data.frame(matrix(nrow=45,ncol=4))
names(ana_report) <- c("method","interval&rate_cor","interval&rate(>0)_cor","interval&0_rate_cor")
method_names <- c("rar","scr","cor","alcf","gecf","grcf","nacf","gek","gek2","grk","grk2","nak","nak2",
                  "uar","ccbr","k_m_inter","k_m_union","k_m_freq","k_m2_inter","k_m2_union","k_m2_freq",
                  "k_m_all_inter","k_m_all_union","k_m_all_freq","cf_inter","cf_union","cf_freq",
                  "all_intersect","all_union","all_freq","30_u_freq","30_c_freq","30_u_freq_wo_uar","30_c_freq_wo_uar",
                  "30_u_freq_wo_uar_km2","30_c_freq_wo_uar_km2","60_u_freq","60_c_freq","60_u_freq_wo_uar","60_c_freq_wo_uar",
                  "60_u_freq_wo_uar_km2","60_c_freq_wo_uar_km2")
for(i in 1:length(method_names)){
  ana_report[i,1] <- method_names[i]
}

#View(result_report)
ana_report[1,2:4] <- making_ana_report(accuracy_cal(dte,rar))
tt <- accuracy_cal(dte,scr)
making_ana_rep
ana_report[2,2:4] <- making_ana_report(accuracy_cal(dte,scr))
ana_report[3,2:4] <- making_ana_report(accuracy_cal(dte,cor))
ana_report[4,2:4] <- making_ana_report(accuracy_cal(dte,alcf))
ana_report[5,2:4] <- making_ana_report(accuracy_cal(dte,gecf))
ana_report[6,2:4] <- making_ana_report(accuracy_cal(dte,grcf))
ana_report[7,2:4] <- making_ana_report(accuracy_cal(dte,nacf))
ana_report[8,2:4] <- making_ana_report(accuracy_cal(dte,gek))
ana_report[9,2:4] <- making_ana_report(accuracy_cal(dte,gek2))
ana_report[10,2:4] <- making_ana_report(accuracy_cal(dte,grk))
ana_report[11,2:4] <- making_ana_report(accuracy_cal(dte,grk2))
ana_report[12,2:4] <- making_ana_report(accuracy_cal(dte,nak))
ana_report[13,2:4] <- making_ana_report(accuracy_cal(dte,nak2))
ana_report[14,2:4] <- making_ana_report(accuracy_cal(dte,uar))
ana_report[15,2:4] <- making_ana_report(accuracy_cal(dte,ccbr))
#View(ana_report)

#km1 accuracy report
ana_report[16,2:4] <- making_ana_report(accuracy_km1_intersect(dte))
ana_report[17,2:4] <- making_ana_report(accuracy_km1_union(dte))
ana_report[18,2:4] <- making_ana_report(accuracy_km1_frequency(dte))

#km2 accuracy report
ana_report[19,2:4] <- making_ana_report(accuracy_km2_intersect(dte))
ana_report[20,2:4] <- making_ana_report(accuracy_km2_union(dte))
ana_report[21,2:4] <- making_ana_report(accuracy_km2_frequency(dte))

#kma accuracy report
ana_report[22,2:4] <- making_ana_report(accuracy_kma_intersect(dte))
ana_report[23,2:4] <- making_ana_report(accuracy_kma_union(dte))
ana_report[24,2:4] <- making_ana_report(accuracy_kma_frequency(dte))
#View(ana_report)

#cf accuracy report
ana_report[25,2:4] <- making_ana_report(accuracy_kma_intersect(dte))
ana_report[26,2:4] <- making_ana_report(accuracy_kma_union(dte))
ana_report[27,2:4] <- making_ana_report(accuracy_kma_frequency(dte))

#all ensemble accuracy report
ana_report[28,2:4] <- making_ana_report(accuracy_all_intersect(dte))
ana_report[29,2:4] <- making_ana_report(accuracy_all_union(dte))
ana_report[30,2:4] <- making_ana_report(accuracy_all_frequency(dte))

#other ensemble
#ensemble 30
ensemble_n <- 30
ana_report[31,2:4] <- making_ana_report(accuracy_ens1_frequency(dte)) #union 후 freq
ana_report[32,2:4] <- making_ana_report(accuracy_ens2_frequency(dte)) #c 결합 후 freq
ana_report[33,2:4] <- making_ana_report(accuracy_ens3_frequency(dte)) #union 후 freq w/o uar
ana_report[34,2:4] <- making_ana_report(accuracy_ens4_frequency(dte)) #c 결합 후 freq w/o uar
ana_report[35,2:4] <- making_ana_report(accuracy_ens5_frequency(dte)) #union 후 freq w/o uar,km2
ana_report[36,2:4] <- making_ana_report(accuracy_ens6_frequency(dte)) #c 결합 후 freq w/o uar,km2

#ensemble 60
ensemble_n <- 60
ana_report[37,2:4] <- making_ana_report(accuracy_ens1_frequency(dte)) #union 후 freq
ana_report[38,2:4] <- making_ana_report(accuracy_ens2_frequency(dte)) #c 결합 후 freq
ana_report[39,2:4] <- making_ana_report(accuracy_ens3_frequency(dte)) #union 후 freq w/o uar
ana_report[40,2:4] <- making_ana_report(accuracy_ens4_frequency(dte)) #c 결합 후 freq w/o uar
ana_report[41,2:4] <- making_ana_report(accuracy_ens5_frequency(dte)) #union 후 freq w/o uar,km2
ana_report[42,2:4] <- making_ana_report(accuracy_ens6_frequency(dte)) #c 결합 후 freq w/o uar,km2

View(ana_report)

setwd("/Users/hodong/Desktop/data_project/naver_movie/fr25to135p20/result")
rr <- data.frame(read.csv("result_report.csv",header=TRUE))
nams <- rr[which(rr$rate_mean>=0.15),]$method
View(ana_report[which(ana_report$method %in% nams),c(1,2,4)])


write.csv(ana_report, file="/Users/hodong/Desktop/ana_report.csv",row.names=FALSE)
#---------------------#making Analysis report#---------------------#


#---------------------#Review interval별 user 수 분포#---------------------#
#setwd("/Users/hodong/Desktop/data_project/naver_movie/half_million")
#data <- data.frame(read.csv("movie_parse_u.csv",header=FALSE))
#names(data) <- c('user_pk','id','review_count','review_id','title','score','content')

temp <- data.frame(matrix(nrow=length(unique(dtr$user_pk)),ncol=3))
names(temp) <- c("user_pk","review_count","review_interval")
temp[,1] <- sort(unique(dtr$user_pk),decreasing=FALSE)
for(i in 1:nrow(temp)){
  temp[i,]$review_count <- dtr[which(dtr$user_pk==temp[i,]$user_pk),]$review_count[1]
  temp[i,]$review_interval <- (floor(temp[i,]$review_count/10))*10
}

user_count <- aggregate(data=temp,user_pk~review_interval,length)
names(user_count) <- c("review_interval","user_count")
View(user_count)

ggplot(data=user_count,aes(y=user_count,x=review_interval)) + geom_bar(stat="identity") + scale_x_continuous(breaks=seq(0, 1000, 10),labels=comma)
write.csv(user_count, file="/Users/hodong/Desktop/after_sample.csv",row.names=FALSE)
#---------------------#Review interval별 user 수 분포#---------------------#


#---------------------#ETC#---------------------#
#removing movies under mean_score
score_limit <- function(tt){
  score_avg <- mean(dp$score)
  rem <- vector()
  for(i in 1:length(tt)){
    score <- mean(dp[which(dp$title==tt[i]),]$score)
    if(!(score>=score_avg)){
      rem <- Reduce(c,list(rem,i))
    }
  }
  return(tt[-rem])
}
#removing movies under mean_count
count_limit <- function(tt){
  count_avg <- mean(unique(dp$review_count))
  rem <- vector()
  for(i in 1:length(tt)){
    count <- length(dp[which(dp$title==tt[i]),]$score)
    if(!(count>=count_avg)){
      rem <- Reduce(c,list(rem,i))
    }
  }
  return(tt[-rem])
}
#extracting top n movies from tt
score_n <- function(tt,size){
  tt2 <- vector()
  for(i in 1:length(tt)){
    tt2[i] <- as.numeric(mean(dp[which(dp$title==tt[i]),]$score))
  }
  temp <- data.frame(cbind(tt,tt2))
  names(temp) <- c("title","score")
  temp <- head(temp[rev(order(temp$score)),],n=size)
  temp <- as.vector(t(temp$title))
  return(temp)
}
#size <- 20
#score_n(tt,size)
#---------------------#ETC#---------------------#

































