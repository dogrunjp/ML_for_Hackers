# 入門機械学習読書会　写経とwiki
## ４章　順位付け：優先トレイ

***

 #Load libraries  
library('tm')  
library('ggplot2')  
library('plyr')  

 **#ご自分の環境にあわせてwdを04-Rankingに設定してください。**  
getwd()  
setwd("ML_for_Hackers-master/04-Ranking")  
 #グローバルパスの設定  
data.path <- file.path("..", "03-Classification", "data")  
easyham.path <- file.path(data.path, "easy_ham")  

 **#msgの取得。encoding="latin1"→endoding="native.nec"→encodingオプション無しと諸説有り**  
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

  **#POSIX形式の月の表記は日本語環境と英語環境で異なるため**  
  **#日本語環境対策にSys.setlocaleを下記のように設定します。**
Sys.setlocale("LC_TIME","C") 
  
pattern1 <- "%a, %d %b %Y %H:%M:%S"  
pattern2 <- "%d %b %Y %H:%M:%S"  
  
allparse.df$Date <- date.converter(allparse.df$Date, pattern1, pattern2)  
  
allparse.df$Subject <- tolower(allparse.df$Subject)  
allparse.df$From.EMail <- tolower(allparse.df$From.EMail)  
  
priority.df <- allparse.df[with(allparse.df, order(Date)), ]  
priority.train <- priority.df[1:(round(nrow(priority.df) / 2)), ]  

 **#重みつけ ** 
priority.train$Date <- as.POSIXct(priority.train$Date)  
from.weight <- ddply(priority.train, .(From.EMail),summarise, Freq=length(Subject))  
from.weight <- from.weight[with(from.weight, order(Freq)), ] 

 **#図4-2の描画**  
  
from.ex <- subset(from.weight, Freq > 6)  
  
ggplot(from.ex) +  
  geom_rect(aes(xmin = 1:nrow(from.ex) - 0.5,  
                xmax = 1:nrow(from.ex) + 0.5,  
                ymin = 0,  
                ymax = Freq,  
                fill = "lightgrey",  
                color = "darkblue")) +  
  scale_x_continuous(breaks = 1:nrow(from.ex), labels = from.ex$From.EMail) +  
  coord_flip() +  
  scale_fill_manual(values = c("lightgrey" = "lightgrey"), guide = "none") +  
  scale_color_manual(values = c("darkblue" = "darkblue"), guide = "none") +  
  ylab("Number of Emails Received (truncated at 6)") +  
  xlab("Sender Address") +  
  theme_bw() +  
  theme(axis.text.y = element_text(size = 5, hjust = 1))  


 #対数による重みつけをし、平坦化した電子メール受信数  
from.weight <- transform(from.weight,  
                         Weight = log(Freq + 1),  
                         log10Weight = log10(Freq + 1))  
  
ggplot(from.weight, aes(x = 1:nrow(from.weight))) +  
  geom_line(aes(y = Weight, linetype = "ln")) +  
  geom_line(aes(y = log10Weight, linetype = "log10")) +  
  geom_line(aes(y = Freq, linetype = "Absolute")) +  
  scale_linetype_manual(values = c("ln" = 1,  
                                   "log10" = 2,  
                                   "Absolute" = 3),  
                        name = "Scaling") +  
  xlab("") +  
  ylab("Number of emails Receieved") +  
  theme_bw() +  
  theme(axis.text.y = element_blank(), axis.text.x = element_blank())  

 **#電子メールのスレッド活動量を重み付けする**  
find.threads <- function(email.df){  
  response.threads <- strsplit(email.df$Subject, "re: ")  
  is.thread <- sapply(response.threads, function(subj)  
    ifelse(subj[1] == "",TRUE,FALSE))  
  threads <- response.threads[is.thread]  
  senders <- email.df$From.EMail[is.thread]  
  threads <- sapply(threads, function(t) paste(t[2:length(t)],  
                                               collapse="re: "))  
  return(cbind(senders, threads))  
}  

threads.matrix <- find.threads(priority.train)  

 **#最も活動的な送信者にあわせて重み付け**
email.thread <- function(thread.matrix){  
  senders <- threads.matrix[,1]  
  senders.freq <- table(senders)  
  senders.matrix <- cbind(names(senders.freq),senders.freq, log(senders.freq +1))  
  senders.df <- data.frame(senders.matrix,stringsAsFactors=FALSE)  
  row.names(senders.df) <- 1:nrow(senders.df)  
  names(senders.df) <- c("From.EMail","Freq","Weight")  
  senders.df$Freq <- as.numeric(senders.df$Freq)  
  senders.df$Weight <- as.numeric(senders.df$Weight)  
  return(senders.df)  
}  
senders.df <-email.thread(thread.matrix)  

 **#活動的と認識されたスレッドに基づく重み付け**
 **#trans.weights 一定の時間に送られたスレッドのメッセージ頻度による重み付け**
get.threads <- function(thrads.matrix, email.df){  
  threads <- unique(threads.matrix[,2])  
  thread.counts <- lapply(threads, function(t) thread.counts(t, email.df))  
  thread.matrix <- do.call(rbind, thread.counts)  
  return(cbind(threads, thread.matrix))  
}  
  
thread.counts <- function(thread, email.df){  
  thread.times <- email.df$Date[which(email.df$Subject== thread  
  | email.df$Subject == paste("re:", thread))]  
  freq <- length(thread.times)  
  min.time <- min(thread.times)  
  max.time <- max(thread.times)  
  time.span <- as.numeric(difftime(max.time, min.time, units="secs"))  
  if(freq < 2){  
    return(c(NA,NA,NA))  
  }  
  else{  
    trans.weight <- freq /time.span  
    log.trans.weight <- 10 + log(trans.weight, base=10)  
    return(c(freq,time.span, log.trans.weight))  
  }  
}  
thread.weights <- get.threads(threads.matrix, priority.train)  
thread.weights <- data.frame(thread.weights, stringsAsFactors=FALSE)  
names(thread.weights) <- c("Thread", "Freq", "Response", "Weight")  
thread.weights$Freq <- as.numeric(thread.weights$Freq)  
thread.weights$Response <- as.numeric(thread.weights$Response)  
thread.weights$Weight <- as.numeric(thread.weights$Weight)  
thread.weights <- subset(thread.weights, is.na(thread.weights$Freq) == FALSE)  
  
head(thread.weights)  

 **# 活動的なスレッドに頻出する単語による重み付け**
term.counts <- function(term.vec, control){  
  vec.corpus <- Corpus(VectorSource(term.vec))  
  vec.tdm <- TermDocumentMatrix(vec.corpus, control=control)  
  return(rowSums(as.matrix(vec.tdm)))  
}  
  
thread.terms <- term.counts(thread.weights$Thread,  
                            control= list(stopwords=TRUE))  
  
thread.terms <- names(thread.terms)  
                              
term.weights <- sapply(thread.terms,  
                       function(t) mean(thread.weights$Weight[grepl(t, thread.weights$Thread, fixed = TRUE)]))
term.weights <- data.frame(list(Term = names(term.weights),  
                                Weight = term.weights),  
                           stringsAsFactors = FALSE,  
                           row.names = 1:length(term.weights))  

