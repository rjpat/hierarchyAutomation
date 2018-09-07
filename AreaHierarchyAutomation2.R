library(dplyr)

setwd("~/Documents/HierarchyAutomation")

data <- read.csv('AreaHierarchyExample.csv', encoding = 'UTF-8', header =  TRUE, na.strings = c("", "NA"))

data <- data %>%
  mutate(parent0 = 'a')


loadFrame <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c('name', 'shortName', 'parent', 'futureId'))

tier1Name <- 5
tier1Short <- 4
tier2Name <- 1
tier2Short <- 0
tier3Name <- 2
tier3Short <- 0
tier4Name <- 3
tier4Short <- 0
    

tierLoops <- function(tierName, tierShort, tierNum) {
  
  #'Intermediate produced here to remove any issues of shortname not being assigned. 
  #'If there is no designated shortname then the name is duplicated in the main table and assigned as short.
  #'The reason for duplication is to prevent a select issue where you cannot select the same column multiple times
  tierShortInt <- if(tierShort == 0) {
    data <- data %>%
      bind_cols(data[tierName])
    
    tierShort <- ncol(data)
  } else {
    tierShort
  }
  
  prevDataParent <- paste0('parent', tierNum-1)
  
  cleansed <- data %>%
    select(tierName, tierShort, prevDataParent) %>%
    unique() %>%
    rename('name' = !!names(.[1]), 
           'shortName' = !!names(.[2]), 
           'parent' = !!names(.[3])) %>%
    #Removes any rows with an na name or parent (where previous tier had an na name)
    filter(!is.na(name)) %>%
    filter(!is.na(parent)) %>%
    #Arranges the data in the correct order then adds the additional background columns required for producing the hierarchy
    arrange(parent, name, shortName) %>%
    mutate(shortName = trimws(substring(shortName, 1, 25)), 
           id = row_number(), 
           parentNum = 0, 
           futureId = 0) 
  
  prevParent <- 0
  
  #Produces the area hierarchy details through the loop
  for(i in cleansed$id) {
    currParent <- cleansed[i, 3]
    
    if(currParent == prevParent) {
      parentNum = parentNum + 1
    } else {
      parentNum = 1 
    }
    
    cleansed[i, 5] <- parentNum
    
    cleansed[i, 6] <- paste0(currParent, '.', parentNum)
    
    prevParent = currParent
  }
  
  #removes non-necessary columns
  cleansedPrep <<- cleansed %>%
    select(name, shortName, parent, futureId)
  
  #Creates the loading dataframe to be utilised at the end
  loadFrame <<- rbind(loadFrame, cleansedPrep)
  
  #Prep and join the data back to the original dataset allowing for the function to be called again
  cleansedPrep2 <- cleansedPrep %>%
    select(name, parent, futureId) 
  
  named <- colnames(data[tierName])
  named2 <- colnames(data[prevDataParent])
  
  data <<- data %>%
    left_join(cleansedPrep2, by = c(setNames('name', named), setNames('parent', named2))) %>%
    rename(!!paste0('parent', tierNum) := futureId)
}


for(i in 1:4) {
  a <- eval(parse(text = (paste0('tier', i, 'Name'))))
  
  b <- eval(parse(text = (paste0('tier', i, 'Short'))))
  
  tierLoops(a, b, i)
}


# tierLoops(eval(tier1Name), eval(tier1Short), 1)
# 
# loadFrame
# 
# data



#Currently only working on one loop
# tierLoops(eval(tier2Name), eval(tier2Short), 2)
# 
# tierLoops(eval(tier3Name), eval(tier3Short), 3)
# 
# tierLoops(eval(tier4Name), eval(tier4Short), 4)
# 
# 





# tier1Short <- if(tier1Short == 0) {
#   tier1Name
# } else {
#   tier1Short
# }
# 
# cleansed <- data %>%
#   select(tier1Name, tier1Short, paste0('parent', 1-1)) %>%
#   unique() %>%
#   rename('name' = !!names(.[1]), 
#          'shortName' = !!names(.[2]), 
#          'parent' = !!names(.[3])) %>%
#   arrange(parent, name, shortName) %>%
#   mutate(shortName = trimws(substring(shortName, 1, 25)), 
#          id = row_number(), 
#          parentNum = 0, 
#          futureId = 0)
# 
# prevParent <- 0
# 
# for(i in cleansed$id) {
#   currParent <- cleansed[i, 3]
#   
#   if(currParent == prevParent) {
#     parentNum = parentNum + 1
#   } else {
#     parentNum = 1 
#   }
#   
#   cleansed[i, 5] <- parentNum
#   
#   cleansed[i, 6] <- paste0(currParent, '.', parentNum)
#   
#   prevParent = currParent
# }
# 
# cleansedPrep <- cleansed %>%
#   select(name, shortName, parent, futureId)
# 
# loadFrame <- rbind(loadFrame, cleansedPrep)
# 
# loadFrame
# 
# cleansedPrep2 <- cleansedPrep %>%
#   select(name, parent, futureId) 
# 
# 
# parentCol <- grep(paste0('parent', 1-1), colnames(data))
# 
# data2 <- merge(data, cleansedPrep2, by.x = c(eval(tier1Name), eval(parentCol)), by.y = c(1, 2), sort = FALSE)
# 
# setNames()
# 
# a <- "name"
# b <- "StateName"
# c <- 5
# 
# named <- colnames(data[c])
# named2 <- colnames(data[6])
# 
#   
# data %>%
#   left_join(cleansedPrep2, by = c(setNames('name', named), setNames('parent', named2)))
# 
# ?left_join
# 
# grep(colnames(data[5]), colnames(data))
# 
# ?merge
# 
# colnames(data[c])
# 
# 
# paste(cleansedPrep2$name, cleansedPrep2$parent)
# 
# data %>%
#   left_join(cleansedPrep2, by = (paste(data$StateName, data$parent0) = paste(cleansedPrep2$name, cleansedPrep2$parent)))

# ?select
# 
# data %>%
#   bind_cols(data[1])
# 
# ?bind_cols
# 
# bind_cols(data, data[1], paste0('trimmed', data[1]))


# n <- c('Name', NA, 'Name2')
# d <- c(1, 2, 3)
# 
# data.frame(n, d) %>%
#   filter(!is.na(n))

# tierLoops(eval(tier1Name), eval(tier1Short), 1)

