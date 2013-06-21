# 入門機械学習読書会　写経とwiki
## ４章　順位付け：優先トレイ

***

 #Load libraries
library('tm')  
library('ggplot2')  
library('plyr')  

 #ご自分の環境にあわせてwdを04-Rankingに設定してください。  
getwd()  
setwd("ML_for_Hackers-master/04-Ranking")  
 #グローバルパスの設定  
data.path <- file.path("..", "03-Classification", "data")  
easyham.path <- file.path(data.path, "easy_ham")  

 #msgの取得。encoding="latin1"→endoding="native.nec"→encodingオプション無しと諸説有り  
msg.full <- function(path)  
{  
  con <- file(path, open = "rt")  
  msg <- readLines(con)  
  close(con)  
  return(msg)  
}  
  
get.from <- function(msg.vec)  
{  
  from <- msg.vec[grepl("From: ", msg.vec)]  
  from <- strsplit(from, '[":<> ]')[[1]]  
  from <- from[which(from  != "" & from != " ")]  
  return(from[grepl("@", from)][1])  
}  
  
get.subject <- function(msg.vec)  
{  
  subj <- msg.vec[grepl("Subject: ", msg.vec)]  
  if(length(subj) > 0)  
  {  
    return(strsplit(subj, "Subject: ")[[1]][2])  
  }  
  else  
  {  
    return("")  
  }  
}  
  
get.msg <- function(msg.vec)  
{  
  msg <- msg.vec[seq(which(msg.vec == "")[1] + 1, length(msg.vec), 1)]  
  return(paste(msg, collapse = "\n"))  
}  
  
get.date <- function(msg.vec)  
{  
  date.grep <- grepl("^Date: ", msg.vec)  
  date.grep <- which(date.grep == TRUE)  
  date <- msg.vec[date.grep[1]]  
  date <- strsplit(date, "\\+|\\-|: ")[[1]][2]  
  date <- gsub("^\\s+|\\s+$", "", date)  
  return(strtrim(date, 25))  
}  
  
parse.email <- function(path)  
{  
  full.msg <- msg.full(path)  
  date <- get.date(full.msg)  
  from <- get.from(full.msg)  
  subj <- get.subject(full.msg)  
  msg <- get.msg(full.msg)  
  return(c(date, from, subj, msg, path))  
}  
  
easyham.docs <- dir(easyham.path)  
easyham.docs <- easyham.docs[which(easyham.docs != "cmds")]  
easyham.parse <- lapply(easyham.docs,  
                        function(p) parse.email(file.path(easyham.path, p)))  

ehparse.matrix <- do.call(rbind, easyham.parse)  
allparse.df <- data.frame(ehparse.matrix, stringsAsFactors = FALSE)  
names(allparse.df) <- c("Date", "From.EMail", "Subject", "Message", "Path")  
  
date.converter <- function(dates, pattern1, pattern2)  
{  
  pattern1.convert <- strptime(dates, pattern1)  
  pattern2.convert <- strptime(dates, pattern2)  
  pattern1.convert[is.na(pattern1.convert)] <- pattern2.convert[is.na(pattern1.convert)]  
  return(pattern1.convert)  
}  

  #POSIX形式の月の表記は日本語環境と英語環境で異なるため　　
  #日本語環境対策にSys.setlocaleを下記のように設定します。
Sys.setlocale("LC_TIME","C") 
  
pattern1 <- "%a, %d %b %Y %H:%M:%S"  
pattern2 <- "%d %b %Y %H:%M:%S"  
  
allparse.df$Date <- date.converter(allparse.df$Date, pattern1, pattern2)  
  
allparse.df$Subject <- tolower(allparse.df$Subject)  
allparse.df$From.EMail <- tolower(allparse.df$From.EMail)  
  
priority.df <- allparse.df[with(allparse.df, order(Date)), ]  
priority.train <- priority.df[1:(round(nrow(priority.df) / 2)), ]  