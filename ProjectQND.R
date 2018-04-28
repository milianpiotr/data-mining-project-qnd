setwd("~/UCD/DataMining/Project/")
library(ggplot2)
library(ggfortify)

library(gridExtra)
library(randomForest)

# library(partykit)
# library(nnet)
library(caret)
library(rpart)

df <- read.csv("bank.csv")
# https://archive.ics.uci.edu/ml/datasets/bank+marketing
# df_small <- read.csv("bank 2.csv", sep = ";")
# df_small <- df_small[names(df)]
# df_big <- read.csv("bank-full.csv", sep = ";")
# df_big <- df_big[names(df)]

df$new_contact <- ifelse(df$previous==0,"new_contact", "existing_contact")
df$month <- paste(df$new_contact, df$month, sep = "_")
df$new_contact <- NULL

df$month <- as.factor(df$month)


yeses <- ifelse(df$y=="yes",TRUE,FALSE)
coloring <- ifelse(yeses,"green","red")
nums <- unlist(lapply(df, is.numeric))  

autoplot(prcomp(df[,nums]), data = df, colour = 'y')

plot(x = df$y, y = df$day)

df$last_contact[df$pdays<0] <- "never"
df$last_contact[df$pdays>365] <- "old"
df$last_contact[is.na(df$last_contact)] <- "recent"
df$last_contact <- as.factor(df$last_contact)

prop.table(table(df$last_contact, df$y),1)
prop.table(table(df$month, df$y),1)


df$campaign
hist(df$age)


pairs(df, col=coloring)




pairs.panels(df)



df[!complete.cases(df)]



mean(yeses)

get_tvt_inds <- function(n_examples, train = .6, test = .2) {
  randomized <- sample(n_examples)
  tresholds <- round(cumsum(c(train,test)) * n_examples)
  train_ids <- randomized <= tresholds[1]
  test_ids <- randomized > tresholds[2]
  validate_ids <- !(train_ids + test_ids)
  return(list("train" = train_ids, "test" = test_ids, "validate" = validate_ids))
}
split <- get_tvt_inds(nrow(df))
split

fit.rf<-randomForest(y~.,data=df[split$train,], importance=TRUE)


pred<-predict(fit.rf,type="class",newdata=df)
tab_rfrst <- confusionMatrix(data=pred[split$validate], mode="prec_recall",
                             reference=df$y[split$validate], 
                             positive = 'yes')
tab_rfrst

varImpPlot(fit.rf,type=1)


df$month <- NULL








tab_rfrst <- confusionMatrix(data=pred[split$test], mode="prec_recall",
                             reference=df$y[split$test], 
                             positive = 'yes')


tab_rfrst
tab_rfrst <- confusionMatrix(data=pred[split$validate], mode="prec_recall",
                             reference=df$y[split$validate], 
                             positive = 'yes')


tab_rfrst


(out$train + out$validate + out$test)











n_examples <- nrow(df)
n_examples <- 11
train = .6
test = .2

round(cumsum(c(0.6, 0.2, 0.2)) * n_examples)


?sample()


tresholds[2]
sum(train_ids)
sum(validate_ids)
sum(test_ids)



spec = c(train = .6, test = .2)
n_train = round(n_examples*spec['train'])
n_test = round(n_examples*spec['test'])

randomized <- sample(1:n_examples) / n_examples

(randomized <= spec['train']) + (randomized >= (spec['test'] + spec['train']))
(randomized <= spec['train']) + (randomized >= (spec['test'] + spec['train'])

sample(1:11)

<

cut(sample(1:n_examples), n_examples*cumsum(c(0,spec)))


nrow(df)*cumsum(c(0,spec))

sample(seq(nrow(df))
    
    
    
spec = c(train = .6, test = .2, validate = .2)
g = sample(cut(seq(nrow(df)), nrow(df)*cumsum(c(0,spec)),labels = names(spec)))
res = split(df, g)









yes_Age <- ggplot(df[yeses,], aes(age)) + geom_histogram(binwidth = 5) + labs(title = "Term Deposits Yes by Age Count", x="age", y="Count of Yes")
no_Age <- ggplot(df[!yeses,], aes(age)) + geom_histogram(binwidth = 5) + labs(title = "Term Deposits No by Age Count", x="age", y="Count of No")
grid.arrange(yes_Age, no_Age)

ggplot(df, aes(education, y)) + geom_jitter()
ggplot(df, aes(job, y)) + geom_jitter()











df_small[names(df)]

mean(complete.cases(df))






pairs(df[,c(1,2,6)], col=coloring)






pairs(df[,c(1:5)], col=coloring)






dim(df)
is.factor(df$Class_att)
summary(df)
str(df)
sum(is.na(df))




