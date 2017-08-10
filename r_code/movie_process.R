library(xlsx)
library(KoNLP)

#<-------------------------file input--------------------------> 
#setwd("/Users/hodong/Desktop/data_project/naver_movie/first_collecting")
setwd("/Users/hodong/Desktop/data_project/naver_movie/half_million")
data <- data.frame(read.csv("movie_parse_u.csv",header=FALSE))
names(data) <- c('user_pk','id','review_count','review_id','title','score','content')
movie <- data.frame(read.csv("movie_list_u.csv",header=FALSE))
names(movie)<-c('title','genre','nation','time','date','grade','director','star','aud_score','cri_score','net_score')
View(data) #nrow(data)
View(movie) #nrow(movie)
#data<-data[-c((nrow(data)/2+9):nrow(data)),] #절반만 빼자
#<-------------------------file input--------------------------> 

#<-------------------------User & review_interval--------------------------> 
nrow(data)
length(unique(data$user_pk))
for(i in sort(unique(as.vector(data$user_pk)),decreasing=FALSE)){
  data[which(data$user_pk==i),]$review_count <- nrow(data[which(data$user_pk==i),])
  print(i/max(data$user_pk))
}

temp <- data.frame(matrix(nrow=length(unique(data$user_pk)),ncol=3))
names(temp) <- c("user_pk","review_count","review_interval")
temp[,1] <- sort(unique(data$user_pk),decreasing=FALSE)

for(i in 1:nrow(temp)){
  temp[i,]$review_count <- data[which(data$user_pk==temp[i,]$user_pk),]$review_count[1]
  temp[i,]$review_interval <- (floor(temp[i,]$review_count/10))*10
}

user_count <- aggregate(data=temp,user_pk~review_interval,length)
View(user_count)
#<-------------------------User & review_interval--------------------------> 

nrow(data)
for(i in sort(unique(as.vector(data$user_pk)),decreasing=FALSE)){
  data[which(data$user_pk==i),]$review_count <- nrow(data[which(data$user_pk==i),])
  print(i/length(sort(unique(as.vector(data$user_pk)),decreasing=FALSE)))
}

data3 <- data
data <- data3

nrow(data[data$review_count>=30,])
#data <- data[data$review_count>=30,]
nrow(data[data$review_count<150,])
#data <- data[data$review_count<150,]
nrow(data)
length(unique(data$user_pk))

#<-------------------------data preparing--------------------------> 
#removing duplicated instances which is reviews about identical movies within each users
num <- c(1:nrow(data))
data <- data.frame(cbind(num,data))

rem <- vector() 
for(i in sort(unique(as.vector(data$user_pk)),decreasing=FALSE)){
  temp <- data[which(data$user_pk==i),]$title
  if(length(temp)!=length(unique(temp))){
    for(j in 1:length(names(which(table(temp)>=2)))){
      tt<-data[which(data$user_pk==i),]
      ar<-tt[which(tt$title==names(which(table(temp)>=2))[j]),]
      arr<-setdiff(ar$review_id,max(ar$review_id))
      rem <- Reduce(c,list(rem,ar[which(ar$review_id %in% arr),]$num))
    } 
  }
}
rem <- unique(rem)
data<-data[-which(data$num %in% rem),-1]


#updating review_count after removing redundant instances
for(i in sort(unique(as.vector(data$user_pk)),decreasing=FALSE)){
  data[which(data$user_pk==i),]$review_count <- nrow(data[which(data$user_pk==i),])
}

#input review_list & movie_list => return review_list without reviews which is not listed in movie_list
rm_missing <- function(data,movie){
  rm_list<-list[]
  for (i in 1:nrow(data)) {if (!(data$title[i] %in% levels(movie$title))) {rm_list<-c(rm_list,-i)}}
  return(data[as.numeric(rm_list),])
}
data <- rm_missing(data,movie)

for(i in sort(unique(as.vector(data$user_pk)),decreasing=FALSE)){
  data[which(data$user_pk==i),]$review_count <- nrow(data[which(data$user_pk==i),])
  print(i/length(sort(unique(as.vector(data$user_pk)),decreasing=FALSE)))
}


#because movies don't have its pk_key, leave the most recent one if there are movies with same title
dup_arr <- function(x){
  dup_li <- vector()
  k <- 0
  for(i in 1:nrow(x)){
    arr <- which(movie$title==as.character(x[i,]$title))
    if(length(arr)>1){
      for(j in 2:length(arr)){
        i_year<-as.numeric(substr(as.character(movie[i,]$date),1,4))
        i_month<-as.numeric(substr(as.character(movie[i,]$date),6,7))
        i_day<-as.numeric(substr(as.character(movie[i,]$date),9,10))
        j_year<-as.numeric(substr(as.character(movie[arr[j],]$date),1,4))
        j_month<-as.numeric(substr(as.character(movie[arr[j],]$date),6,7))
        j_day<-as.numeric(substr(as.character(movie[arr[j],]$date),9,10))
        k<-k+1
        if(i_year>j_year){dup_li[k] <- arr[j]}
        else if(j_year>i_year){dup_li[k] <- i}
        else{
          if(i_month>j_month){dup_li[k] <- arr[j]}
          else if(j_month>i_month){dup_li[k] <- i}
        }
      }        
    }
  }
  dup_li<-unique(sort(dup_li,decreasing = FALSE))
  return(dup_li)
}
vv<-dup_arr(movie)
movie<-movie[-vv,]
movie <- droplevels(movie)

#create function which convert string to int considering where there is NA
string_to_int <- function(x) {return(lapply(x,na_check<-function(z){ifelse(is.na(z),NA,as.numeric(z))}))}
#in movie_list
#convert time/aud_score/cri_score/net_score feature - string data - to numeric data, imputate -1 if there is NA
movie$time <- as.numeric(lapply(movie$time,time_to_int <- function(x){gsub('[^0-9]','',x)}))
movie$aud_score <- as.numeric(string_to_int(movie$aud_score))
movie$cri_score <- as.numeric(string_to_int(movie$cri_score))
movie$net_score <- as.numeric(string_to_int(movie$net_score))

#in review_list
#convert user_pk/review_count/review_id/score feature - string data - to numeric data, imputate -1 if there is NA
data$user_pk <- as.numeric(string_to_int(data$user_pk))
data$review_count <- as.numeric(string_to_int(data$review_count))
data$review_id <- as.numeric(string_to_int(data$review_id)) 
data$score <- as.numeric(string_to_int(data$score))

#Dummy Coding for Categories with creating new features
split_list <- function(l,x){
  x <- as.character(x);x <- gsub('\\[','',x);x <- gsub('\\]','',x);x <- gsub('\'','',x)
  temp <- strsplit(x,',')[[1]]
  for (i in 1:length(temp)){ l <- append(l,gsub(' ','',temp[i])) }
  return(l)
}

extract_genre <- function(lis, movie){
  for (j in 1:nrow(movie)){ lis<-split_list(lis, movie[j,]$genre) }
  return(lis)
}

genre_list = list[]
#genre_list contains the data that which genres movies reviewed on top 1000page are divided into
genre_list <- factor(extract_genre(genre_list,movie)) 
genre_lev <- levels(genre_list) 
#dummy coding with genre_levels
genre_dummy <- function(gl,mv) {
  for(i in 1:length(gl)){
    ll <- list[]
    for(j in 1:nrow(mv)){
      ll <- append(ll,ifelse(gl[i] %in% split_list(list[],mv[j,]$genre),1,0))
    }
    mv<-data.frame(cbind(mv,ll))
    names(mv) <- c(colnames(mv)[1:ncol(mv)-1],gl[i])
  }
  return(mv)
}
movie <- genre_dummy(genre_lev,movie)

#create new primary key for rows of each dataset
#create_pk <- function(x) { return(data.frame(cbind(review_pk=matrix(1:nrow(x)),x[,-which(names(x) %in% c('review_id'))]))) }
create_pk <- function(x) { return(data.frame(cbind(pk=matrix(1:nrow(x)),x))) }
movie<-create_pk(movie)
data<-create_pk(data)
data<-data[-5]
movie$title<-gsub(',','',movie$title)
data$title<-gsub(',','',data$title)


#View(data)
#View(movie)
#<-------------------------data preparing--------------------------> 
movie2 <- movie
data2 <- data

#---------------------#Dividing train_set & test_set#---------------------#
data<-data[,-1]

for(i in sort(unique(as.vector(data$user_pk)),decreasing=FALSE)){
  data[which(data$user_pk==i),]$review_count <- nrow(data[which(data$user_pk==i),])
  print(i/max(data$user_pk))
}

test <- vector()
for(i in sort(unique(data$user_pk),decreasing=FALSE)){
  if((data[which(data$user_pk==i),]$review_count)[1]>=20){
    n <- round((data[which(data$user_pk==i),]$review_count[1])*0.2,0)
    test <- Reduce(c,list(test,sample(which(data$user_pk==i),n)))
  }
}
length(test)
length(test)/nrow(data)

data_test <- data[test,]
data_train <- data[-test,]

for(i in sort(unique(as.vector(data_test$user_pk)),decreasing=FALSE)){
  data_test[which(data_test$user_pk==i),]$review_count <- nrow(data_test[which(data_test$user_pk==i),])
}

for(i in sort(unique(as.vector(data_train$user_pk)),decreasing=FALSE)){
  data_train[which(data_train$user_pk==i),]$review_count <- nrow(data_train[which(data_train$user_pk==i),])
}
#nrow(data_train)
#nrow(data_test)
#nrow(data2)
#nrow(movie)
#length(unique(data2$user_pk))
#min(data2$review_count)
#max(data2$review_count)

min(data$review_count)
max(data$review_count)

#View(data2)
#View(data_test)
#View(data_train)
#---------------------#Dividing train_set & test_set#---------------------#

#<-------------------------only by the movie_list data--------------------------> 
data <- data_train
nrow(data)
#the score board per director calculating without NA
director_board <- data.frame(matrix(nrow=0,ncol=5))
len<-length(levels(movie$director))
for(i in 1:len){
  dir_name <- levels(movie$director)[i]
  ddd<-movie[which(movie$director==dir_name),]
  count <- nrow(ddd)
  director_score <- cbind(as.character(ddd$title),ddd$aud_score,ddd$cri_score,ddd$net_score) #director's the score board per movie
  #aud_mean <- round(mean(sapply(ddd$aud_score,ie <- function(x){ifelse(x==-1,NA,x)}),na.rm=TRUE),3)
  aud_mean <- round(mean(ddd$aud_score,na.rm=TRUE),3)
  cri_mean <- round(mean(ddd$cri_score,na.rm=TRUE),3)
  net_mean <- round(mean(ddd$net_score,na.rm=TRUE),3)
  #cri_mean <- round(mean(sapply(ddd$cri_score,ie <- function(x){ifelse(x==-1,NA,x)}),na.rm=TRUE),3)
  #net_mean <- round(mean(sapply(ddd$net_score,ie <- function(x){ifelse(x==-1,NA,x)}),na.rm=TRUE),3)
  row <- cbind(dir_name,aud_mean,cri_mean,net_mean,count)
  director_board <- data.frame(rbind(director_board,row))
}
names(director_board)<-c('director','aud_mean','cri_mean','net_mean','count')
director_board$aud_mean <- as.numeric(as.character(director_board$aud_mean))
director_board$cri_mean <- as.numeric(as.character(director_board$cri_mean))
director_board$net_mean <- as.numeric(as.character(director_board$net_mean))
director_board$count <- as.numeric(as.character(director_board$count))
View(director_board)

#show director board which directed movies more than n times
view_db <- function(n){
  View(director_board[which(director_board$count>n),])
}
view_db(10)


#the score board per nation calculating without NA
aud_mean<-aggregate(aud_score~nation,movie,mean)
cri_mean<-aggregate(cri_score~nation,movie,mean)
net_mean<-aggregate(net_score~nation,movie,mean)
li_aud <- list[] ; li_cri <- list[] ; li_net <- list[]
for(i in 1:length(levels(movie$nation))){
  li_aud<-append(li_aud,ifelse(levels(movie$nation)[i] %in% aud_mean[,1],round(aud_mean[which(aud_mean$nation==levels(movie$nation)[i]),][,2],3),NA))
  li_cri<-append(li_cri,ifelse(levels(movie$nation)[i] %in% cri_mean[,1],round(cri_mean[which(cri_mean$nation==levels(movie$nation)[i]),][,2],3),NA))
  li_net<-append(li_net,ifelse(levels(movie$nation)[i] %in% net_mean[,1],round(net_mean[which(net_mean$nation==levels(movie$nation)[i]),][,2],3),NA))
}
count<-aggregate(title~nation,movie,length)
nation_board <- data.frame(cbind(levels(movie$nation),li_aud,li_cri,li_net,count))
nation_board <- nation_board[-5]
names(nation_board) <- c('nation','aud_mean','cri_mean','net_mean','count')
View(nation_board)


#the score board per grade calculating without NA
aud_mean<-aggregate(aud_score~grade,movie,mean)
cri_mean<-aggregate(cri_score~grade,movie,mean)
net_mean<-aggregate(net_score~grade,movie,mean)
li_aud <- list[] ; li_cri <- list[] ; li_net <- list[]
for(i in 1:length(levels(movie$grade))){
  li_aud<-append(li_aud,ifelse(levels(movie$grade)[i] %in% aud_mean[,1],round(aud_mean[which(aud_mean$grade==levels(movie$grade)[i]),][,2],3),NA))
  li_cri<-append(li_cri,ifelse(levels(movie$grade)[i] %in% cri_mean[,1],round(cri_mean[which(cri_mean$grade==levels(movie$grade)[i]),][,2],3),NA))
  li_net<-append(li_net,ifelse(levels(movie$grade)[i] %in% net_mean[,1],round(net_mean[which(net_mean$grade==levels(movie$grade)[i]),][,2],3),NA))
}
count<-aggregate(title~grade,movie,length)
grade_board <- data.frame(cbind(levels(movie$grade),li_aud,li_cri,li_net,count))
grade_board <- grade_board[-5]
names(grade_board) <- c('grade','aud_mean','cri_mean','net_mean','count')
View(grade_board)


#the score board per genre calculating without NA
genre_board <- data.frame(matrix(nrow=0,ncol=5))
for(j in 1:length(genre_lev)){
  count<-0; aud_count<-0; net_count<-0; cri_count<-0; aud_sum<-0; cri_sum<-0; net_sum<-0;
  num <- which(colnames(movie)==genre_lev[j])
  if(j==8){num<-which(colnames(movie)=='멜로.로맨스')}
  for(i in 1:nrow(movie)){
    if(movie[i,num]==1){
      count<-count+1
      aud_sum<-aud_sum+ifelse(is.na(movie[i,]$aud_score),0,movie[i,]$aud_score)
      aud_count<-aud_count+ifelse(is.na(movie[i,]$aud_score),0,1)
      cri_sum<-cri_sum+ifelse(is.na(movie[i,]$cri_score),0,movie[i,]$cri_score)
      cri_count<-cri_count+ifelse(is.na(movie[i,]$cri_score),0,1)
      net_sum<-net_sum+ifelse(is.na(movie[i,]$net_score),0,movie[i,]$net_score)
      net_count<-net_count+ifelse(is.na(movie[i,]$net_score),0,1)
    }
  }
  row <- cbind(genre_lev[j],round(aud_sum/aud_count,3),round(cri_sum/cri_count,3),round(net_sum/net_count,3),count)
  genre_board <- data.frame(rbind(genre_board,row))
}
names(genre_board) <- c('genre','aud_mean','cri_mean','net_mean','count')
genre_board$count <- as.numeric(as.character(genre_board$count))
View(genre_board)


#nation_table consists of data which indicate the mean value and standard deviation value of each nation factor and total number
#genre_table consists of data which indicate the mean value and standard deviation value of each genre factor and total number
#grade_table consists of data which indicate the mean value and standard deviation value of each grade factor and total number

mem_no<-sort(unique(data$user_pk),decreasing=FALSE);mem_movie<-which(data$user_pk==mem_no[i])
genre_lev<-levels(genre_list);nation_lev<-levels(movie$nation);grade_lev<-levels(movie$grade)

find_nation<-function(x){return(as.character(movie[which(movie$title==as.character(x)),]$nation))}
find_genre<-function(x){return(as.character(movie[which(movie$title==as.character(x)),]$genre))}
find_grade<-function(x){return(as.character(movie[which(movie$title==as.character(x)),]$grade))}

nation_table <- data.frame(matrix(nrow=length(mem_no),ncol=(2+length(nation_lev)*3)))
for(i in 1:length(levels(movie$nation))){
  names(nation_table)[3*i]<-paste(as.character(nation_lev[i]),'mean',sep='_')
  names(nation_table)[3*i+1]<-paste(as.character(nation_lev[i]),'std',sep='_')
  names(nation_table)[3*i+2]<-paste(as.character(nation_lev[i]),'count',sep='_')
}
names(nation_table)[1:2]<-c('user_pk','id')

genre_table <- data.frame(matrix(nrow=length(mem_no),ncol=(2+length(genre_lev)*3)))
for(i in 1:length(genre_lev)){
  names(genre_table)[3*i]<-paste(as.character(genre_lev[i]),'mean',sep='_')
  names(genre_table)[3*i+1]<-paste(as.character(genre_lev[i]),'std',sep='_')
  names(genre_table)[3*i+2]<-paste(as.character(genre_lev[i]),'count',sep='_')
}
names(genre_table)[1:2]<-c('user_pk','id')


grade_table <- data.frame(matrix(nrow=length(mem_no),ncol=(2+length(grade_lev)*3)))
for(i in 1:length(grade_lev)){
  names(grade_table)[3*i]<-paste(as.character(grade_lev[i]),'mean',sep='_')
  names(grade_table)[3*i+1]<-paste(as.character(grade_lev[i]),'std',sep='_')
  names(grade_table)[3*i+2]<-paste(as.character(grade_lev[i]),'count',sep='_')
}
names(grade_table)[1:2]<-c('user_pk','id')

length(mem_no)
for(i in 1:length(mem_no)){
  mem_movie<-which(data$user_pk==mem_no[i])
  for(j in 1:length(mem_movie)){
    t<-as.character(data[mem_movie[j],]$title)
    s<-as.numeric(as.character(data[mem_movie[j],]$score))
    nl<-data.frame(nation_lev)
    nation_table[i,which(nl$nation_lev==find_nation(t))*3]<-ifelse(is.na(nation_table[i,which(nl$nation_lev==find_nation(t))*3]),s,nation_table[i,which(nl$nation_lev==find_nation(t))*3]+s)
    nation_table[i,which(nl$nation_lev==find_nation(t))*3+1]<-ifelse(is.na(nation_table[i,which(nl$nation_lev==find_nation(t))*3+1]),s*s,nation_table[i,which(nl$nation_lev==find_nation(t))*3+1]+s*s)
    nation_table[i,which(nl$nation_lev==find_nation(t))*3+2]<-ifelse(is.na(nation_table[i,which(nl$nation_lev==find_nation(t))*3+2]),1,nation_table[i,which(nl$nation_lev==find_nation(t))*3+2]+1)
    l<-split_list(list[],find_genre(t))
    rl<-data.frame(genre_lev)
    for(k in 1:length(l)){
      genre_table[i,which(rl$genre_lev==l[k])*3]<-ifelse(is.na(genre_table[i,which(rl$genre_lev==l[k])*3]),s,genre_table[i,which(rl$genre_lev==l[k])*3]+s)
      genre_table[i,which(rl$genre_lev==l[k])*3+1]<-ifelse(is.na(genre_table[i,which(rl$genre_lev==l[k])*3+1]),s*s,genre_table[i,which(rl$genre_lev==l[k])*3+1]+s*s)
      genre_table[i,which(rl$genre_lev==l[k])*3+2]<-ifelse(is.na(genre_table[i,which(rl$genre_lev==l[k])*3+2]),1,genre_table[i,which(rl$genre_lev==l[k])*3+2]+1)
    }
    gl<-data.frame(grade_lev)
    grade_table[i,which(gl$grade_lev==find_grade(t))*3]<-ifelse(is.na(grade_table[i,which(gl$grade_lev==find_grade(t))*3]),s,grade_table[i,which(gl$grade_lev==find_grade(t))*3]+s)
    grade_table[i,which(gl$grade_lev==find_grade(t))*3+1]<-ifelse(is.na(grade_table[i,which(gl$grade_lev==find_grade(t))*3+1]),s*s,grade_table[i,which(gl$grade_lev==find_grade(t))*3+1]+s*s)
    grade_table[i,which(gl$grade_lev==find_grade(t))*3+2]<-ifelse(is.na(grade_table[i,which(gl$grade_lev==find_grade(t))*3+2]),1,grade_table[i,which(gl$grade_lev==find_grade(t))*3+2]+1)
  }
}

for(i in 1:length(mem_no)){
  nation_table[i,]$user_pk<-data[which(data$user_pk==mem_no[i])[1],]$user_pk
  nation_table[i,]$id<-as.character(data[which(data$user_pk==mem_no[i])[1],]$id)
  genre_table[i,]$user_pk<-data[which(data$user_pk==mem_no[i])[1],]$user_pk
  genre_table[i,]$id<-as.character(data[which(data$user_pk==mem_no[i])[1],]$id)
  grade_table[i,]$user_pk<-data[which(data$user_pk==mem_no[i])[1],]$user_pk
  grade_table[i,]$id<-as.character(data[which(data$user_pk==mem_no[i])[1],]$id)
  n_m<-0;n_ms<-0;r_m<-0;r_ms<-0;g_m<-0;g_ms<-0
  for(j in 1:length(nation_lev)){
    if(!is.na(nation_table[i,j*3])){
      nation_table[i,j*3]<-nation_table[i,j*3]/nation_table[i,j*3+2]
    }
    if(!is.na(nation_table[i,j*3+1])){
      nation_table[i,j*3+1]<-nation_table[i,j*3+1]/nation_table[i,j*3+2]
      nation_table[i,j*3+1]<-sqrt(nation_table[i,j*3+1]-nation_table[i,j*3]*nation_table[i,j*3])
    }
  }
  for(j in 1:length(genre_lev)){
    if(!is.na(genre_table[i,j*3])){
      genre_table[i,j*3]<-genre_table[i,j*3]/genre_table[i,j*3+2]
    }
    if(!is.na(genre_table[i,j*3+1])){
      genre_table[i,j*3+1]<-genre_table[i,j*3+1]/genre_table[i,j*3+2]
      genre_table[i,j*3+1]<-sqrt(genre_table[i,j*3+1]-genre_table[i,j*3]*genre_table[i,j*3])
    }
  }
  for(j in 1:length(grade_lev)){
    if(!is.na(grade_table[i,j*3])){
      grade_table[i,j*3]<-grade_table[i,j*3]/grade_table[i,j*3+2]
    }
    if(!is.na(grade_table[i,j*3+1])){
      grade_table[i,j*3+1]<-grade_table[i,j*3+1]/grade_table[i,j*3+2]
      grade_table[i,j*3+1]<-sqrt(grade_table[i,j*3+1]-grade_table[i,j*3]*grade_table[i,j*3])
    }
  }
}
View(genre_table)

sort(unique(dp$user_pk),decreasing=FALSE)

#<-------------------------file output--------------------------> 
write.xlsx(director_board,                # R데이터명
           file="/Users/hodong/Desktop/director_board.xlsx",  # 여기서는 기존의 엑셀 파일이라고 설정함
           sheetName="new", # 기존의 엑셀 파일에 new라는 시트에 데이터를 넣음
           col.names=TRUE,  # 변수이름을 그대로 사용
           row.names=FALSE,# 행이름은 사용하지 않음
           append=FALSE)      # 기존의 엑셀 파일이 있으면 그곳에 추가해서 저장

write.xlsx(nation_board,                # R데이터명
           file="/Users/hodong/Desktop/nation_board.xlsx",  # 여기서는 기존의 엑셀 파일이라고 설정함
           sheetName="new", # 기존의 엑셀 파일에 new라는 시트에 데이터를 넣음
           col.names=TRUE,  # 변수이름을 그대로 사용
           row.names=FALSE,# 행이름은 사용하지 않음
           append=FALSE)      # 기존의 엑셀 파일이 있으면 그곳에 추가해서 저장

write.xlsx(genre_board,                # R데이터명
           file="/Users/hodong/Desktop/genre_board.xlsx",  # 여기서는 기존의 엑셀 파일이라고 설정함
           sheetName="new", # 기존의 엑셀 파일에 new라는 시트에 데이터를 넣음
           col.names=TRUE,  # 변수이름을 그대로 사용
           row.names=FALSE,# 행이름은 사용하지 않음
           append=FALSE)      # 기존의 엑셀 파일이 있으면 그곳에 추가해서 저장

write.xlsx(grade_board,                # R데이터명
           file="/Users/hodong/Desktop/grade_board.xlsx",  # 여기서는 기존의 엑셀 파일이라고 설정함
           sheetName="new", # 기존의 엑셀 파일에 new라는 시트에 데이터를 넣음
           col.names=TRUE,  # 변수이름을 그대로 사용
           row.names=FALSE,# 행이름은 사용하지 않음
           append=FALSE)      # 기존의 엑셀 파일이 있으면 그곳에 추가해서 저장

write.csv(genre_table, file="/Users/hodong/Desktop/genre_table.csv")
write.csv(grade_table, file="/Users/hodong/Desktop/grade_table.csv")
write.csv(nation_table, file="/Users/hodong/Desktop/nation_table.csv")

movie <- movie[,-1]
write.csv(movie, file="/Users/hodong/Desktop/movie_prepared.csv",row.names=FALSE)
write.csv(data2, file="/Users/hodong/Desktop/data_prepared.csv",row.names=FALSE)
write.csv(data_train, file="/Users/hodong/Desktop/data_train.csv",row.names=FALSE)
write.csv(data_test, file="/Users/hodong/Desktop/data_test.csv",row.names=FALSE)

'''
write.xlsx(genre_table,                # R데이터명
           file="/Users/hodong/Desktop/genre_table.xlsx",  # 여기서는 기존의 엑셀 파일이라고 설정함
           sheetName="new", # 기존의 엑셀 파일에 new라는 시트에 데이터를 넣음
           col.names=TRUE,  # 변수이름을 그대로 사용
           row.names=TRUE,# 행이름은 사용하지 않음
           append=FALSE)      # 기존의 엑셀 파일이 있으면 그곳에 추가해서 저장

write.xlsx(nation_table,                # R데이터명
           file="/Users/hodong/Desktop/nation_table.xlsx",  # 여기서는 기존의 엑셀 파일이라고 설정함
           sheetName="new", # 기존의 엑셀 파일에 new라는 시트에 데이터를 넣음
           col.names=TRUE,  # 변수이름을 그대로 사용
           row.names=TRUE,# 행이름은 사용하지 않음
           append=FALSE)      # 기존의 엑셀 파일이 있으면 그곳에 추가해서 저장

write.xlsx(grade_table,                # R데이터명
           file="/Users/hodong/Desktop/grade_table.xlsx",  # 여기서는 기존의 엑셀 파일이라고 설정함
           sheetName="new", # 기존의 엑셀 파일에 new라는 시트에 데이터를 넣음
           col.names=TRUE,  # 변수이름을 그대로 사용
           row.names=TRUE,# 행이름은 사용하지 않음
           append=FALSE)      # 기존의 엑셀 파일이 있으면 그곳에 추가해서 저장



write.xlsx(movie,                # R데이터명
           file="/Users/hodong/Desktop/movie_prepared.xlsx",  # 여기서는 기존의 엑셀 파일이라고 설정함
           sheetName="new", # 기존의 엑셀 파일에 new라는 시트에 데이터를 넣음
           col.names=TRUE,  # 변수이름을 그대로 사용
           row.names=FALSE,# 행이름은 사용하지 않음
           append=FALSE)      # 기존의 엑셀 파일이 있으면 그곳에 추가해서 저장

write.xlsx(data,                # R데이터명
           file="/Users/hodong/Desktop/data_prepared.xlsx",  # 여기서는 기존의 엑셀 파일이라고 설정함
           sheetName="new", # 기존의 엑셀 파일에 new라는 시트에 데이터를 넣음
           col.names=TRUE,  # 변수이름을 그대로 사용
           row.names=FALSE,# 행이름은 사용하지 않음
           append=FALSE)      # 기존의 엑셀 파일이 있으면 그곳에 추가해서 저장

write.xlsx(data_test,                # R데이터명
           file="/Users/hodong/Desktop/data_test.xlsx",  # 여기서는 기존의 엑셀 파일이라고 설정함
           sheetName="new", # 기존의 엑셀 파일에 new라는 시트에 데이터를 넣음
           col.names=TRUE,  # 변수이름을 그대로 사용
           row.names=FALSE,# 행이름은 사용하지 않음
           append=FALSE)      # 기존의 엑셀 파일이 있으면 그곳에 추가해서 저장

write.xlsx(data_train,                # R데이터명
           file="/Users/hodong/Desktop/data_train.xlsx",  # 여기서는 기존의 엑셀 파일이라고 설정함
           sheetName="new", # 기존의 엑셀 파일에 new라는 시트에 데이터를 넣음
           col.names=TRUE,  # 변수이름을 그대로 사용
           row.names=FALSE,# 행이름은 사용하지 않음
           append=FALSE)      # 기존의 엑셀 파일이 있으면 그곳에 추가해서 저장
'''
#<-------------------------file output--------------------------> 