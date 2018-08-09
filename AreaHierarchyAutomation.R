library(readxl)
library(tidyverse)
library(sqldf)

project <- 200

hierarchyExample <- read_excel("AreaHierarchyExample.xlsx")

state <- sqldf("SELECT DISTINCT State, 0 AS Parent FROM hierarchyExample ORDER BY State")

state <- state %>%
  rename(Name = State) %>%
  mutate(id = (row_number())) 

val <- max(state$id)

location <- sqldf('SELECT DISTINCT h.Location, s.id AS Paren 
                  FROM hierarchyExample h 
                  INNER JOIN state s ON h.State = s.Name')

location <- location %>%
  rename(Parent = Paren, Name = Location) %>%
  arrange(Parent, Name) %>%
  mutate(id = (row_number() + val))

val2 <- max(location$id)

level <- sqldf('SELECT DISTINCT h.Level, l.id AS Par
                FROM hierarchyExample h 
                INNER JOIN state s ON h.State = s.Name
                INNER JOIN location l ON s.id = l.Parent AND h.Location = l.Name')

level <- level %>%
  rename(Parent = Par, Name = Level) %>%
  arrange(Parent, Name) %>%
  mutate(id = (row_number() + val2))

val3 <- max(level$id)

room <- sqldf('SELECT DISTINCT h.Room, lev.id AS Par
                FROM hierarchyExample h 
                INNER JOIN state s ON h.State = s.Name
                INNER JOIN location l ON s.id = l.Parent AND h.Location = l.Name
                INNER JOIN level lev on l.id = lev.Parent AND h.Level = lev.Name')

room <- room %>%
  rename(Parent = Par, Name = Room) %>%
  arrange(Parent, Name) %>%
  mutate(id = (row_number() + val3))


combined <- rbind(state, location, level, room)

combined$ParentId <- if_else(is.na(combined$Parent), project, 0) 

combined <- combined %>%
  mutate(parentNum = 0) %>%
  mutate(futureId = 0)

prevParent <- 100000

for(i in combined$id) {
  currParent <- combined[i,2]
  
  if(currParent == prevParent) {
    parentNum = parentNum + 1
  } else {
    parentNum = 1
  }
  
  combined[i,5] <- parentNum
  
  prevParent <- currParent

  if(combined[i,2] == 0) {
    ParentId = project
  } else {
    ParentId = combined[combined[i,2],6]
  }
  
  combined[i,4] <- ParentId
  
  combined[i,6] <- paste(ParentId, parentNum, sep = ".")
  
}


combined

