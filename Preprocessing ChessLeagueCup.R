library(magrittr)
library(plyr)
library(dplyr)
library(stringr)

#The next phase of the preprocessing, after clubs have been geocoded
#This is in preparation of the optimization 

ChessClubs20162017 <- read.csv("input\\OriginalLeagueChessClub20162017.csv", sep=",", header=TRUE)

ChessClubs20162017 <- ChessClubs20162017 %>%
  mutate(
    Klasse2 = as.integer(str_replace(str_replace(str_sub(Klasse, end = 1), "R", "4"), "M", "0")) + 1
  ) %>%
  filter(Beker != "") %>%
  rename(Id = X)

count(ChessClubs20162017, Klasse2)

#ChessClubs20162017 <- ChessClubs20162017[1:20,] #for testing purposes with limited set

n <- nrow(ChessClubs20162017)
c <- 4 #4 LeagueCup groups


#set weights, arbitrary choices
# M = 8
# 1 = 5
# 2 = 3
# 3 = 2
# R = 1

ChessClubs20162017 <- ChessClubs20162017 %>%
  mutate(
    Weight = case_when(
      Klasse2 == 1 ~ 8,
      Klasse2 == 2 ~ 5,
      Klasse2 == 3 ~ 3,
      Klasse2 == 4 ~ 2,
      Klasse2 == 5 ~ 1
    )
  )

gMin <- floor(n/c)
gMax <- ceiling(n/c)



# The following is a possible refinement to create more similarity between the groups
# This data needs to be added within the model

# teams1 <- nrow(ChessClubs20162017[ChessClubs20162017$Klasse2 == 1,])
# teams2 <- nrow(ChessClubs20162017[ChessClubs20162017$Klasse2 == 2,])
# teams3 <- nrow(ChessClubs20162017[ChessClubs20162017$Klasse2 == 3,])
# teams4 <- nrow(ChessClubs20162017[ChessClubs20162017$Klasse2 == 4,])
# teams5 <- nrow(ChessClubs20162017[ChessClubs20162017$Klasse2 == 5,])
# 
# groupSize1Min <- floor(teams1 / c)
# groupSize1Max <- ceiling(teams1 / c)
# 
# groupSize2Min <- floor(teams2 / c)
# groupSize2Max <- ceiling(teams2 / c)
# 
# groupSize3Min <- floor(teams3 / c)
# groupSize3Max <- ceiling(teams3 / c)
# 
# groupSize4Min <- floor(teams4 / c)
# groupSize4Max <- ceiling(teams4 / c)
# 
# groupSize5Min <- floor(teams5 / c)
# groupSize5Max <- ceiling(teams5 / c)
# 
# if (groupSize1Max == groupSize1Min & groupSize1Min != 0) {
#   groupSize1Max <- groupSize1Max + 1
#   groupSize1Min <- groupSize1Min - 1
# }
# 
# if (groupSize2Max == groupSize2Min & groupSize2Min != 0) {
#   groupSize2Max <- groupSize2Max + 1
#   groupSize2Min <- groupSize2Min - 1
# }
# 
# if (groupSize3Max == groupSize3Min & groupSize3Min != 0) {
#   groupSize3Max <- groupSize3Max + 1
#   groupSize3Min <- groupSize3Min - 1
# }
# 
# if (groupSize4Max == groupSize4Min & groupSize4Min != 0) {
#   groupSize4Max <- groupSize4Max + 1
#   groupSize4Min <- groupSize4Min - 1
# }
# 
# if (groupSize5Max == groupSize5Min & groupSize5Min != 0) {
#   groupSize5Max <- groupSize5Max + 1
#   groupSize5Min <- groupSize5Min - 1
# }


totalWeight <-  sum(ChessClubs20162017$Weight)
minTotalWeight <- floor(totalWeight / c) - 2
maxTotalWeight <- ceiling(totalWeight / c) + 2


ChessClubs20162017 <- ChessClubs20162017 %>%
  arrange(desc(FIDE))

ChessClubs20162017 <- ChessClubs20162017 %>%
  mutate(
    Id = row_number()
  )

weightLevel <- ChessClubs20162017$Weight
