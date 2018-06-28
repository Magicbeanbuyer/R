setwd("C:/Users/Vanessa/Documents/Discovering Statistics Using R")
getwd()

library(car)
library(ggplot2)
library(pastecs)
library(psych)
library(Rcmdr)

#5.5.1 plot graphs ####
#Read in the download data:
dlf <- read.delim("DownloadFestival.dat", header = TRUE)

#Remove the outlier from the day1 hygiene score
dlf$day1 <- ifelse(dlf$day1 > 20, NA, dlf$day1)

#Histograms for hygiene scores on day 1, day 2 and day 3.
#Histogram for day 1:
hist.day1 <-
  ggplot(dlf, aes(day1)) + opts(legend.position = "none") + geom_histogram(aes(y =
                                                                                 ..density..), colour = "black", fill = "white") + labs(x = "Hygiene score on day 1", y = "Density")
hist.day1

#Histogram for day 2:
hist.day2 <-
  ggplot(dlf, aes(day2)) + opts(legend.position = "none") + geom_histogram(aes(y =
                                                                                 ..density..), colour = "black", fill = "white") + labs(x = "Hygiene score on day 2", y = "Density")
hist.day2

#Histogram for day 3:
hist.day3 <-
  ggplot(dlf, aes(day3)) + opts(legend.position = "none") + geom_histogram(aes(y =
                                                                                 ..density..), colour = "black", fill = "white") + labs(x = "Hygiene score on day 3", y = "Density")
hist.day3

#Add the curves to the Histograms:
hist.day1 + stat_function(
  fun = dnorm,
  args = list(
    mean = mean(dlf$day1, na.rm = TRUE),
    sd = sd(dlf$day1, na.rm = TRUE)
  ),
  colour = "black",
  size = 1
)

ggsave(file = paste(imageDirectory, "05 DLF Day 1 Hist.png", sep = "/"))

hist.day2 + stat_function(
  fun = dnorm,
  args = list(
    mean = mean(dlf$day2, na.rm = TRUE),
    sd = sd(dlf$day2, na.rm = TRUE)
  ),
  colour = "black",
  size = 1
)

ggsave(file = paste(imageDirectory, "05 DLF Day 2 Hist.png", sep = "/"))


hist.day3 + stat_function(
  fun = dnorm,
  args = list(
    mean = mean(dlf$day3, na.rm = TRUE),
    sd = sd(dlf$day3, na.rm = TRUE)
  ),
  colour = "black",
  size = 1
)

ggsave(file = paste(imageDirectory, "05 DLF Day 3 Hist.png", sep = "/"))

#Q-Q plot for day 1:
qqplot.day1 <- qplot(sample = dlf$day1, stat = "qq")
qqplot.day1

ggsave(file = paste(imageDirectory, "05 DLF Day 1 QQ.png", sep = "/"))

#Q-Q plot for day 2:
qqplot.day2 <- qplot(sample = dlf$day2, stat = "qq")
qqplot.day2
ggsave(file = paste(imageDirectory, "05 DLF Day 2 QQ.png", sep = "/"))

#Q-Q plot of the hygiene scores on day 3:
qqplot.day3 <- qplot(sample = dlf$day3, stat = "qq")
qqplot.day3
ggsave(file = paste(imageDirectory, "05 DLF Day 3 QQ.png", sep = "/"))


#5.5.2 decriptive statistics####
stat.desc(dlf$day1)
stat.desc(dlf$day1, basic = FALSE, norm = TRUE)
stat.desc(cbind(dlf$day1, dlf$day2, dlf$day3),
          basic = FALSE,
          norm = TRUE)
stat.desc(dlf[, c("day1", "day2", "day3")], basic = FALSE, norm = TRUE)
round(stat.desc(dlf[, c("day1", "day2", "day3")], basic = FALSE, norm = TRUE), digits = 3)


#5.5.3 Exploring groups of data####
rexam <- read.delim("rexam.dat", header = TRUE)
rexam$uni <-
  factor(
    rexam$uni,
    levels = c(0:1),
    labels = c("Duncetown University", "Sussex University")
  )

round(stat.desc(rexam[, c("exam", "computer", "lectures", "numeracy")], basic = FALSE, norm = TRUE), digits = 2)

by(rexam$exam, rexam$uni, describe)
by(rexam$exam,
   rexam$uni,
   stat.desc,
   basic = FALSE,
   norm = TRUE)
by(rexam[, c("exam", "computer", "lectures", "numeracy")],
   rexam$uni,
   stat.desc,
   basic = FALSE,
   norm = TRUE)

dunceData <- subset(rexam, rexam$uni == "Duncetown University")
sussexData <- subset(rexam, rexam$uni == "Sussex University")

hist.numeracy.duncetown <-
  ggplot(dunceData, aes(numeracy)) + labs(legend.position = "none") + geom_histogram(
    aes(y = ..density..),
    fill = "white",
    colour = "black",
    binwidth = 1
  ) + labs(x = "Numeracy Score", y = "Density") + stat_function(
    fun = dnorm,
    args = list(
      mean = mean(dunceData$numeracy, na.rm = TRUE),
      sd = sd(dunceData$numeracy, na.rm = TRUE)
    ),
    colour = "red",
    size = 1
  )
hist.numeracy.duncetown


#5.6.1 shapiro-wilk test####
shapiro.test(rexam$exam)
by(rexam$exam, rexam$uni, shapiro.test)
qplot(sample = rexam$exam, stat = "qq")
qplot(sample = rexam$numeracy, stat = "qq")


qplot(sample = dunceData$exam)
qplot(sample = dunceData$numeracy)


#5.7.1 Levene's test####
leveneTest(rexam$exam, rexam$uni)
leveneTest(rexam$exam, rexam$uni, center = mean)
leveneTest(rexam$numeracy, rexam$uni)

#5.8.3 transforming the data####
dlf$day1plusday2 <- dlf$day1 + dlf$day2
dlf$day1minusday2 <- dlf$day1 - dlf$day2
dlf$day1lessthanone <- dlf$day1 < 1
dlf$male <- dlf$gender == "Male"

dlf$missingday2 <- is.na(dlf$day2)
sum(dlf$missingday2)
sum(is.na(dlf$day2))
mean(is.na(dlf$day2))

dlf$logday1 <- log(dlf$day1 + 1)
dlf$logday2 <- log(dlf$day2 + 1)
dlf$logday3 <- log(dlf$day3 + 1)


#Histogram for logday1:
hist.logday1 <-
  ggplot(dlf, aes(logday1)) + labs(legend.position = "none") + geom_histogram(aes(y =
                                                                                    ..density..), colour = "black", fill = "white") + labs(x = "Log Transformed Hygiene Score on Day 1", y = "Density") + stat_function(
                                                                                      fun = dnorm,
                                                                                      args = list(
                                                                                        mean = mean(dlf$logday1, na.rm = TRUE),
                                                                                        sd = sd(dlf$logday1, na.rm = TRUE)
                                                                                      ),
                                                                                      colour = "red",
                                                                                      size = 1
                                                                                    )
hist.logday1
ggsave(file = paste(imageDirectory, "05 DLF Log Day 1 Hist.png", sep = "/"))

#Histogram for logday2:
hist.logday2 <-
  ggplot(dlf, aes(logday2)) + opts(legend.position = "none") + geom_histogram(aes(y =
                                                                                    ..density..), colour = "black", fill = "white") + labs(x = "Log Transformed Hygiene Score on Day 2", y = "Density") + stat_function(
                                                                                      fun = dnorm,
                                                                                      args = list(
                                                                                        mean = mean(dlf$logday2, na.rm = TRUE),
                                                                                        sd = sd(dlf$logday2, na.rm = TRUE)
                                                                                      ),
                                                                                      colour = "red",
                                                                                      size = 1
                                                                                    )
hist.logday2
ggsave(file = paste(imageDirectory, "05 DLF Log Day 2 Hist.png", sep = "/"))

#Histogram for logday3:
hist.logday3 <-
  ggplot(dlf, aes(logday3)) + opts(legend.position = "none") + geom_histogram(aes(y =
                                                                                    ..density..), colour = "black", fill = "white") + labs(x = "Log Transformed Hygiene Score on Day 3", y = "Density") + stat_function(
                                                                                      fun = dnorm,
                                                                                      args = list(
                                                                                        mean = mean(dlf$logday3, na.rm = TRUE),
                                                                                        sd = sd(dlf$logday3, na.rm = TRUE)
                                                                                      ),
                                                                                      colour = "red",
                                                                                      size = 1
                                                                                    )
hist.logday3
ggsave(file = paste(imageDirectory, "05 DLF Log Day 3 Hist.png", sep = "/"))

dlf$sqrtday1 <- sqrt(dlf$day1)
dlf$sqrtday2 <- sqrt(dlf$day2)
dlf$sqrtday3 <- sqrt(dlf$day3)

dlf$recday1 <- 1 / (dlf$day1 + 1)
dlf$recday2 <- 1 / (dlf$day2 + 1)
dlf$recday3 <- 1 / (dlf$day3 + 1)

dlf$day1nooutliner <- ifelse(dlf$day1 > 4, NA, dlf$day1)
sum(is.na(dlf$day1nooutliner))

dlf$meanHygieneNoMissing <-
  rowMeans(cbind(dlf$day1, dlf$day2, dlf$day3))
dlf$meanHygiene <-
  rowMeans(cbind(dlf$day1, dlf$day2, dlf$day3), na.rm = TRUE)
sum(is.na(dlf$meanHygiene))

dlf$daysMissing <- rowSums(cbind(is.na(dlf$day1),
                                 is.na(dlf$day2),
                                 is.na(dlf$day3)))

dlf$meanHygieneOneMissing <- ifelse(dlf$daysMissing > 1, NA,
                                    rowMeans(cbind(dlf$day1,
                                                   dlf$day2,
                                                   dlf$day3),
                                             na.rm = TRUE))
