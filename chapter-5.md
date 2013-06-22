## ５章　回帰：ページビューの予測
  
getwd()  
setwd('Dropbox/flask/r/ML_for_Hackers-master/05-Regression')  
  
library('ggplot2')  
  
 **#ペースラインモデル**  
ages <- read.csv(file.path('data', 'longevity.csv'))  
  
ggplot(ages, aes(x = AgeAtDeath, fill = factor(Smokes))) +  
  geom_density() +  
  facet_grid(Smokes ~ .)  
  
 **#平均二乗誤差**  
guess <- 73  
with(ages, mean((AgeAtDeath - guess) ^ 2))  
  
guess.accuracy <- data.frame()  
  
for (guess in seq(63, 83, by = 1))  
{  
  prediction.error <- with(ages,  
                           mean((AgeAtDeath - guess) ^ 2))  
  guess.accuracy <- rbind(guess.accuracy,  
                          data.frame(Guess = guess,  
                                     Error = prediction.error))  
}  
 **#図５−２ MS**  
ggplot(guess.accuracy, aes(x = Guess, y = Error)) +  
  geom_point() +  
  geom_line()  

  
constant.guess <- with(ages, mean(AgeAtDeath))  
  
with(ages, sqrt(mean((AgeAtDeath - constant.guess) ^ 2)))  
  
smokers.guess <- with(subset(ages, Smokes == 1),  
                      mean(AgeAtDeath))  
  
non.smokers.guess <- with(subset(ages, Smokes == 0),  
                          mean(AgeAtDeath))  
  
ages <- transform(ages,  
                  NewPrediction = ifelse(Smokes == 0,  
                                         non.smokers.guess,  
                                         smokers.guess))  
  
with(ages, sqrt(mean((AgeAtDeath - NewPrediction) ^ 2)))  

  
 **#線形回帰入門**  
 **#身長と体重の散布図**  
heights.weights <- read.csv('data/01_heights_weights_genders.csv',  
                          header = TRUE,sep = ',')

ggplot(heights.weights, aes(x = Height, y= Weight)) +  
  geom_point() +  
  geom_smooth(method = 'lm')  
  
fitted.regression <- lm(Weight ~ Height,  
                        data = heights.weights)  
  
coef(fitted.regression)  
  
intercept <- coef(fitted.regression)[1]  
slope <- coef(fitted.regression)[2]  
  
predict(fitted.regression)  
  
true.values <- with(heights.weights,Weight)  
errors <- true.values - predict(fitted.regression)  