
library(readr)
library(stringr)
library(caret)
library(car)
library(pracma)

set.seed(1)

setwd(".../Kaggle")

# load data
df_session = read_csv("sessions2.csv")
my_id      = unique(df_session[,1], incomparables = FALSE)
col_2_uni  = c(unique(df_session[,2], incomparables = FALSE))
col_3_uni  = c(unique(df_session[,3], incomparables = FALSE))
col_4_uni  = c(unique(df_session[,4], incomparables = FALSE))
col_5_uni  = c(unique(df_session[,5], incomparables = FALSE))
col_uni = c('id',col_2_uni,col_3_uni,col_4_uni,col_5_uni,'time')
uni_length = length(col_2_uni)+length(col_3_uni)+length(col_4_uni)+length(col_5_uni);

my_session = matrix(0,nrow=length(my_id),ncol=1+uni_length+1) ## 1 for id, 1 for staying time sum
colnames(my_session)<-col_uni
#nrow(df_session)

for(x in c(1:nrow(df_session))){
  buffer   <- df_session[x,]
  id       <- buffer[[1]]
  id_index <- which(my_id %in% id)
  
  for (y in c(2:5)){
    action   <- buffer[[y]]
    if(is.na(action)){}                   ## I ignore missing data
    else if(strcmp(action,'NA')==TRUE){}  ## I also ignore those label as 'NA'
    else{
      action_index <- which(col_uni %in% action)
      my_session[id_index,action_index]<-my_session[id_index,action_index]+1
    }
  }
  
  action  <-'time'
  timer   <- buffer[[6]]
  if(is.na(timer)){}                        ## rule out the missing data scenario
  else{
    action_index <- which(col_uni %in% action)
    my_session[id_index,action_index]<-my_session[id_index,action_index]+timer
  }
  
}

my_session[,1]<-my_id   ## for some unknown reason, string array shouldnt be assigned to early, or ZERO matrix will be defined as string matrix
session_stat <- NULL
session_stat$id <- my_id
# generate submission file
session_stat <- as.data.frame(my_session)
write.csv(session_stat, "session_stat.csv", quote=FALSE, row.names = FALSE)