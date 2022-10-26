attend <- read.csv("data-raw/CFBeattendance.csv")

library(caTools)


# fill.rate variable distribution
hist(attend$Fill.Rate)
qqnorm(attend$Fill.Rate)
qqline(attend$Fill.Rate)

attend$Fill.Rate[attend$Fill.Rate > 1] <- 1
attend$Team <- as.factor(attend$Team)
attend$Time <- as.factor(attend$Time)
levels(attend$Time) <- list("Morning" = c("10:00 AM", "10:30 AM",
                                         "11:00 AM", "11:05 AM",
                                         "11:10 AM", "11:21 AM",
                                         "11:30 AM", "11:40 AM"),
                           "Early Afternoon" = c("12:00 PM",
                                                 "12:05 PM", "12:10 PM",
                                                 "12:15 PM", "12:20 PM",
                                                 "12:30 PM", "12:35 PM",
                                                 "12:45 PM", "1:00 PM",
                                                 "1:05 PM", "1:10 PM",
                                                 "1:15 PM", "1:30 PM",
                                                 "1:45 PM", "2:00 PM",
                                                 "2:05 PM", "2:15 PM",
                                                 "2:30 PM", "2:35 PM",
                                                 "2:45 PM"),
                           "Late Afternoon" = c("3:00 PM", "3:05 PM",
                                                "3:15 PM", "3:30 PM",
                                                "3:36 PM", "3:40 PM",
                                                "3:45 PM", "4:00 PM",
                                                "4:05 PM", "4:15 PM",
                                                "4:20 PM", "4:30 PM",
                                                "4:45 PM"),
                           "Evening" = c("5:00 PM", "5:05 PM", "5:10 PM",
                                         "5:15 PM", "5:30 PM", "5:45 PM",
                                         "6:00 PM", "6:05 PM", "6:10 PM",
                                         "6:15 PM", "6:30 PM", "6:35 PM",
                                         "6:45 PM", "6:50 PM"),
                           "Night" = c("7:00 PM", "7:05 PM", "7:06 PM",
                                       "7:15 PM", "7:20 PM", "7:30 PM",
                                       "7:35 PM", "7:45 PM", "8:00 PM",
                                       "8:04 PM", "8:05 PM", "8:15 PM",
                                       "8:30 PM", "9:00 PM"))
attend$Opponent <- as.factor(attend$Opponent)
attend$Rank <- factor(attend$Rank, levels = c("1", "2", "3", "4", "5", "6",
                                                 "7", "8", "9", "10", "11", "12",
                                                 "13", "14", "15", "16", "17",
                                                 "18", "19", "20", "21", "22",
                                                 "23", "24", "25", "NR"))
attend$Rank <- as.ordered(attend$Rank)
attend$Rivalry <- as.factor(attend$Rivalry)
attend$TV <- as.factor(ifelse(attend$TV == "Not on TV", 0, 1))
attend$Win_Pct <- attend$Current.Wins / 
  (attend$Current.Losses + attend$Current.Wins) * 100
attend$Win_Pct[is.nan(attend$Win_Pct)] <- 0
attend$PRCP <- as.factor(ifelse(attend$PRCP == 0, 0, 1))
attend$SNOW <- as.factor(ifelse(attend$SNOW == 0, 0, 1))
attend$Opponent_Rank <- factor(attend$Opponent_Rank,
                               levels = c("1", "2", "3", "4", "5", "6",
                                              "7", "8", "9", "10", "11", "12",
                                              "13", "14", "15", "16", "17",
                                              "18", "19", "20", "21", "22",
                                              "23", "24", "25", "NR"))
attend$Opponent_Rank <- as.ordered(attend$Opponent_Rank)
attend$Conference <- as.factor(attend$Conference)

attend <- attend[, c(3, 5, 7, 8, 14:18, 20, 22, 23, 27)]

attend_split <- sample.split(attend, SplitRatio = 0.3)
attend_test <- subset(attend, attend_split == TRUE)
attend_train <- subset(attend, attend_split == FALSE)

save(attend_train, attend_test, file = "data-raw/attend.RData")
