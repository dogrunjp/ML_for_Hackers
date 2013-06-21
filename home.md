# 入門機械学習読書会　写経とwiki
## ４章　順位付け：優先トレイ

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

##msgの取得。encoding="latin1"→endoding="native.nec"→encodingオプション無しと諸説有り
msg.full <- function(path)
{
  con <- file(path, open = "rt")
  msg <- readLines(con)
  close(con)
  return(msg)
}

