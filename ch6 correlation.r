setwd("C:/Users/Vanessa/Documents/R/Discovering Statistics Using R")

adverts <- c(5,4,4,6,8)
packets <- c(8,9,10,13,15)
advertData <- data.frame(adverts, packets)

install.packages("polycor")
install.packages("Hmisc")
install.packages("ggm")
install.packages("Rcmdr")

library(Hmisc)
library(polycor)
library(boot)
library(ggplot2)
library(ggm)
library(Rcmdr)

examData <- read.delim("Exam Anxiety.dat", header = TRUE)
examData2 <- examData[,c("Exam", "Anxiety", "Revise")]

#pearson's corelation coefficient####
cor(examData[,c("Exam", "Anxiety", "Revise")])
cor(examData2, use = "complete.obs", method = "pearson")

examMatrix <- as.matrix(examData[, c("Exam", "Anxiety", "Revise")])
Hmisc::rcorr(examMatrix)

cor.test(examData$Exam, examData$Anxiety, alternative = "two.sided", method = "pearson")
cor.test(examData$Exam, examData$Anxiety, method = "pearson")
cor.test(examData$Exam, examData$Anxiety, alternative = "less", method = "pearson")
cor.test(examData$Exam, examData$Anxiety, alternative = "greater", method = "pearson")
cor.test(examData$Exam, examData$Anxiety, alternative = "two.sided", method = "spearman")

ggplot(data = examData) +
  geom_point(mapping = aes(Anxiety, Exam, color = Revise))

#spearman's correlation coefficient####
liarData <- read.delim("The Biggest Liar.dat", header = TRUE)
cor(liarData$Creativity,liarData$Position, method = "spearman")
cor.test(liarData$Creativity, liarData$Position,alternative = "less", method = "spearman")
cor.test(liarData$Creativity, liarData$Position,alternative = "less", method = "pearson")
cor.test(liarData$Creativity, liarData$Position,alternative = "two.sided", method = "spearman")

Hmisc::rcorr(as.matrix(liarData[,c("Position", "Creativity")]), type = "spearman")
Hmisc::rcorr(as.matrix(liarData[,c("Position", "Creativity")]))
Hmisc::rcorr(as.matrix(liarData[,c("Position", "Creativity")]), type = "pearson")

