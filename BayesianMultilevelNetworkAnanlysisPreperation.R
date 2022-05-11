
########################
# 
# This code is to prepare data for Bayesian Multilevel network analysis in MLwiN with the multiple membership model
# It is not yet cleaned up for the publication that goes along with this code, as I am currently still finishing this project
#
##############################

## author: Brent Hendrickx


## there is a lot of repetition in this code in order for me to quickly analyze the data without having to do more advanced programming such as writing functions
## although this is something I would like to learn in the very near future


library(haven)
library(dplyr)
library(tidyverse)
library("xlsx")
library(data.table)
library('readxl')
library(dplyr)

library(ggcorrplot)
library(semTools)
library(semPlot)
library(lavaan)
library(ltm)
library(psych)



# first run data (09102020_Merged Data_Network.sav) through Yeasle's code
# I saved the output as CILS_generation_fit_with_network.csv

completedata <-
alldata <- read.csv("C:/Users/Brent/Documents/Psychology/KUL/Psychologie/Stage/papers/network analysis/CILS_generation_fit_with_networkNEW.csv")
#egonD2 = read_sav('egonD2.sav')

data <- alldata %>% select(wave1_id,
                           ## best friends in class
                           y1_bfs_0:y1_bfs_20,
                           
                           # pupils they like least
                           y1_wfs_1:y1_wfs_5,
                           
                           # popular
                           y1_pos_1:y1_pos_5,
                           
                           ## usual background variables
                           age,sex, 
                           #group_status,    # 1 = 1st gen   3 = 3th gen  4 = majority
                           
                           # contact with majority
                           y1_bgfr1,  # M13_1:How many of your friends have a Belgian backround
                           y1_rescs1,  # M14_1: How often do you spend time during your school break with students with a Belgian background
                           y1_recn1,  # M15_1: How often do you spend time in the neighbourhood where you live with stud
                           y1_compn,   # M16: How many of the people in your neighbourhood have a Belgian backround
                           

                           
                           # cognitive_score,    # does not exist
                           
                           ## minority/majorities
                           number_minority,
                           number_majority,
                           
                           y1_majority_classsum,
                           y1_minority_classsum,
                           y1_majority_proportion_class,
                           
                           # Yeasle code variables
                           group_status,
                           y1_em_fit,
                           y1_em_fit_1st,
                           y1_clsize

                           ## EPQ
                           
                           # original
                           # # disengaging
                           # y1_disenngem1:y1_disenngem15,
                           # y1_disenpsem1:y1_disenpsem15,
                           # # engaging
                           # y1_enngem1:y1_enngem15,
                           # y1_enpsem1:y1_enpsem15,

                           # computed
                           # y1_fit_fisher_Pos_Sit,
                           # y1_fit_fisher_Neg_Sit,
                           # y1_mean_fit_fisher_min,
                           # y1_mean_fit_fisher_maj
                           #y1_em_fit_min

                           )



pupil0 <- data$wave1_id - round(data$wave1_id, -2)
pupil0 <- as.data.frame(pupil0)
pupil <- table(pupil0)

#View(pupil)
datan <- data.frame(data,pupil0)


####################################
###     prepare adjency matrix
####################################

### we will transform the y1_bfs_ variables to indicate if they are friends with a certain person
### currently these variables just list the pupils they are friends with in order of which persons they wrote down first
### we will change this in a way that number 12 in the row of a person (in the columns y1_bfs_1 to y1_bfs_20)
### becomes a 1 in variable y1_bfs_12 (indicator that that person is friend with number 12)

### first we need to make sure there are enough for the amount of pupils in a class, the largest class has 29 pupils
### make it 30 so it looks pretty

# length = length of observations
length <- length(alldata$wave1_id)


y1_bfs_21 <- rep(0,length)
y1_bfs_22 <- rep(0,length)
y1_bfs_23 <- rep(0,length)
y1_bfs_24 <- rep(0,length)
y1_bfs_25 <- rep(0,length)
y1_bfs_26 <- rep(0,length)
y1_bfs_27 <- rep(0,length)
y1_bfs_28 <- rep(0,length)
y1_bfs_29 <- rep(0,length)
y1_bfs_30 <- rep(0,length)


datan <- data.frame(datan,y1_bfs_21)
datan <- data.frame(datan,y1_bfs_22)
datan <- data.frame(datan,y1_bfs_23)
datan <- data.frame(datan,y1_bfs_24)
datan <- data.frame(datan,y1_bfs_25)
datan <- data.frame(datan,y1_bfs_26)
datan <- data.frame(datan,y1_bfs_27)
datan <- data.frame(datan,y1_bfs_28)
datan <- data.frame(datan,y1_bfs_29)
datan <- data.frame(datan,y1_bfs_30)

### make separate document for adjacency matrix
### first also include class and pupil number to make sure the code makes sense

#adja <- datan%>% select(wave1_id)

adja <- datan

group_status_table <- data.frame(table(adja$group_status))
group_status_table

adja <- data.frame(adja,pupil0)   # you have to manually add this one, the one that is already in datan will give an error when using the select() command

adja <- data.frame(adja,datan%>% select(y1_bfs_0:y1_bfs_20,y1_bfs_21:y1_bfs_30))


adja <- data.frame(adja,y1_bfs_0n    <- rep(0,length))
adja <- data.frame(adja,y1_bfs_1n    <- rep(0,length))
adja <- data.frame(adja,y1_bfs_2n    <- rep(0,length))
adja <- data.frame(adja,y1_bfs_3n    <- rep(0,length))
adja <- data.frame(adja,y1_bfs_4n    <- rep(0,length))
adja <- data.frame(adja,y1_bfs_5n    <- rep(0,length))
adja <- data.frame(adja,y1_bfs_6n    <- rep(0,length))
adja <- data.frame(adja,y1_bfs_7n    <- rep(0,length))
adja <- data.frame(adja,y1_bfs_8n    <- rep(0,length))
adja <- data.frame(adja,y1_bfs_9n    <- rep(0,length))
adja <- data.frame(adja,y1_bfs_10n   <- rep(0,length))
adja <- data.frame(adja,y1_bfs_11n   <- rep(0,length))
adja <- data.frame(adja,y1_bfs_12n   <- rep(0,length))
adja <- data.frame(adja,y1_bfs_13n   <- rep(0,length))
adja <- data.frame(adja,y1_bfs_14n   <- rep(0,length))
adja <- data.frame(adja,y1_bfs_15n   <- rep(0,length))
adja <- data.frame(adja,y1_bfs_16n   <- rep(0,length))
adja <- data.frame(adja,y1_bfs_17n   <- rep(0,length))
adja <- data.frame(adja,y1_bfs_18n   <- rep(0,length))
adja <- data.frame(adja,y1_bfs_19n   <- rep(0,length))
adja <- data.frame(adja,y1_bfs_20n   <- rep(0,length))
adja <- data.frame(adja,y1_bfs_21n   <- rep(0,length))
adja <- data.frame(adja,y1_bfs_22n   <- rep(0,length))
adja <- data.frame(adja,y1_bfs_23n   <- rep(0,length))
adja <- data.frame(adja,y1_bfs_24n   <- rep(0,length))
adja <- data.frame(adja,y1_bfs_25n   <- rep(0,length))
adja <- data.frame(adja,y1_bfs_26n   <- rep(0,length))
adja <- data.frame(adja,y1_bfs_27n   <- rep(0,length))
adja <- data.frame(adja,y1_bfs_28n   <- rep(0,length))
adja <- data.frame(adja,y1_bfs_29n   <- rep(0,length))
adja <- data.frame(adja,y1_bfs_30n   <- rep(0,length))

adja[,2:32][is.na(adja[,2:32])]<- 0

## can't like themselves (takes 2-5 min)
## ignore the warnings, the code works
for (i in 1:nrow(adja)){
  for (b in 2:ncol(adja)){
    if (as.numeric(adja[i,2]) == as.numeric(adja$wave1_id[i])){
      
      adja[i,2] <- 0
      i <- i+1
      # b <- b+1
      # if (b == 26){break}
    }
  }
}
for (i in 1:nrow(adja)){
  for (b in 2:ncol(adja)){
    if (as.numeric(adja[i,3]) == as.numeric(adja$wave1_id[i])){
      
      adja[i,3] <- 0
      i <- i+1
      # b <- b+1
      # if (b == 26){break}
    }
  }
}
for (i in 1:nrow(adja)){
  for (b in 2:ncol(adja)){
    if (as.numeric(adja[i,4]) == as.numeric(adja$wave1_id[i])){
      
      adja[i,4] <- 0
      i <- i+1
      # b <- b+1
      # if (b == 26){break}
    }
  }
}
for (i in 1:nrow(adja)){
  for (b in 2:ncol(adja)){
    if (as.numeric(adja[i,5]) == as.numeric(adja$wave1_id[i])){
      
      adja[i,5] <- 0
      i <- i+1
      # b <- b+1
      # if (b == 26){break}
    }
  }
}
for (i in 1:nrow(adja)){
  for (b in 2:ncol(adja)){
    if (as.numeric(adja[i,6]) == as.numeric(adja$wave1_id[i])){
      
      adja[i,6] <- 0
      i <- i+1
      # b <- b+1
      # if (b == 26){break}
    }
  }
}
for (i in 1:nrow(adja)){
  for (b in 2:ncol(adja)){
    if (as.numeric(adja[i,7]) == as.numeric(adja$wave1_id[i])){
      
      adja[i,7] <- 0
      i <- i+1
      # b <- b+1
      # if (b == 26){break}
    }
  }
}
for (i in 1:nrow(adja)){
  for (b in 2:ncol(adja)){
    if (as.numeric(adja[i,8]) == as.numeric(adja$wave1_id[i])){
      
      adja[i,8] <- 0
      i <- i+1
      # b <- b+1
      # if (b == 26){break}
    }
  }
}
for (i in 1:nrow(adja)){
  for (b in 2:ncol(adja)){
    if (as.numeric(adja[i,9]) == as.numeric(adja$wave1_id[i])){
      
      adja[i,9] <- 0
      i <- i+1
      # b <- b+1
      # if (b == 26){break}
    }
  }
}
for (i in 1:nrow(adja)){
  for (b in 2:ncol(adja)){
    if (as.numeric(adja[i,10]) == as.numeric(adja$wave1_id[i])){
      
      adja[i,10] <- 0
      i <- i+1
      # b <- b+1
      # if (b == 26){break}
    }
  }
}
for (i in 1:nrow(adja)){
  for (b in 2:ncol(adja)){
    if (as.numeric(adja[i,11]) == as.numeric(adja$wave1_id[i])){
      
      adja[i,11] <- 0
      i <- i+1
      # b <- b+1
      # if (b == 26){break}
    }
  }
}
for (i in 1:nrow(adja)){
  for (b in 2:ncol(adja)){
    if (as.numeric(adja[i,12]) == as.numeric(adja$wave1_id[i])){
      
      adja[i,12] <- 0
      i <- i+1
      # b <- b+1
      # if (b == 26){break}
    }
  }
}
for (i in 1:nrow(adja)){
  for (b in 2:ncol(adja)){
    if (as.numeric(adja[i,13]) == as.numeric(adja$wave1_id[i])){
      
      adja[i,13] <- 0
      i <- i+1
      # b <- b+1
      # if (b == 26){break}
    }
  }
}
for (i in 1:nrow(adja)){
  for (b in 2:ncol(adja)){
    if (as.numeric(adja[i,14]) == as.numeric(adja$wave1_id[i])){
      
      adja[i,14] <- 0
      i <- i+1
      # b <- b+1
      # if (b == 26){break}
    }
  }
}
for (i in 1:nrow(adja)){
  for (b in 2:ncol(adja)){
    if (as.numeric(adja[i,15]) == as.numeric(adja$wave1_id[i])){
      
      adja[i,15] <- 0
      i <- i+1
      # b <- b+1
      # if (b == 26){break}
    }
  }
}
for (i in 1:nrow(adja)){
  for (b in 2:ncol(adja)){
    if (as.numeric(adja[i,16]) == as.numeric(adja$wave1_id[i])){
      
      adja[i,16] <- 0
      i <- i+1
      # b <- b+1
      # if (b == 26){break}
    }
  }
}
for (i in 1:nrow(adja)){
  for (b in 2:ncol(adja)){
    if (as.numeric(adja[i,17]) == as.numeric(adja$wave1_id[i])){
      
      adja[i,17] <- 0
      i <- i+1
      # b <- b+1
      # if (b == 26){break}
    }
  }
}
for (i in 1:nrow(adja)){
  for (b in 2:ncol(adja)){
    if (as.numeric(adja[i,18]) == as.numeric(adja$wave1_id[i])){
      
      adja[i,18] <- 0
      i <- i+1
      # b <- b+1
      # if (b == 26){break}
    }
  }
}

for (i in 1:nrow(adja)){
  for (b in 2:ncol(adja)){
    if (as.numeric(adja[i,19]) == as.numeric(adja$wave1_id[i])){
      
      adja[i,19] <- 0
      i <- i+1
      # b <- b+1
      # if (b == 26){break}
    }
  }
}
for (i in 1:nrow(adja)){
  for (b in 2:ncol(adja)){
    if (as.numeric(adja[i,20]) == as.numeric(adja$wave1_id[i])){
      
      adja[i,20] <- 0
      i <- i+1
      # b <- b+1
      # if (b == 26){break}
    }
  }
}
for (i in 1:nrow(adja)){
  for (b in 2:ncol(adja)){
    if (as.numeric(adja[i,21]) == as.numeric(adja$wave1_id[i])){
      
      adja[i,21] <- 0
      i <- i+1
      # b <- b+1
      # if (b == 26){break}
    }
  }
}
for (i in 1:nrow(adja)){
  for (b in 2:ncol(adja)){
    if (as.numeric(adja[i,22]) == as.numeric(adja$wave1_id[i])){
      
      adja[i,22] <- 0
      i <- i+1
      # b <- b+1
      # if (b == 26){break}
    }
  }
}


### code says: if person (row) mentions anywhere a specific pupil as a friend, indicated by their class number (listed in y1_bfs_1 to y1_bfs_20), then make a column for that pupil and put a one in his/her column



adja$y1_bfs_1n = ifelse(adja$y1_bfs_0 == 1 | adja$y1_bfs_1 == 1 | adja$y1_bfs_2 == 1 | adja$y1_bfs_3 == 1 | adja$y1_bfs_4 == 1 | adja$y1_bfs_5 == 1 | adja$y1_bfs_6 == 1 | adja$y1_bfs_7 == 1 | adja$y1_bfs_8 == 1 |
                          adja$y1_bfs_9 == 1 | adja$y1_bfs_10 == 1 | adja$y1_bfs_11 == 1 | adja$y1_bfs_1 == 1 | adja$y1_bfs_13 == 1 | adja$y1_bfs_14 == 1 | adja$y1_bfs_15 == 1 | adja$y1_bfs_16 == 1 |
                          adja$y1_bfs_17 == 1  | adja$y1_bfs_18 == 1 | adja$y1_bfs_19 == 1 | adja$y1_bfs_20 == 1, 1,0)

adja$y1_bfs_2n = ifelse(adja$y1_bfs_0 == 2 | adja$y1_bfs_1 == 2 | adja$y1_bfs_2 == 2 | adja$y1_bfs_3 == 2 | adja$y1_bfs_4 == 2 | adja$y1_bfs_5 == 2 | adja$y1_bfs_6 == 2 | adja$y1_bfs_7 == 2 | adja$y1_bfs_8 == 2 |
                          adja$y1_bfs_9 == 2 | adja$y1_bfs_10 == 2 | adja$y1_bfs_11 == 2 | adja$y1_bfs_2 == 2 | adja$y1_bfs_13 == 2 | adja$y1_bfs_14 == 2 | adja$y1_bfs_15 == 2 | adja$y1_bfs_16 == 2 |
                          adja$y1_bfs_17 == 2  | adja$y1_bfs_18 == 2 | adja$y1_bfs_19 == 2 | adja$y1_bfs_20 == 2, 1,0)

adja$y1_bfs_3n = ifelse(adja$y1_bfs_0 == 3 | adja$y1_bfs_1 == 3 | adja$y1_bfs_2 == 3 | adja$y1_bfs_3 == 3 | adja$y1_bfs_4 == 3 | adja$y1_bfs_5 == 3 | adja$y1_bfs_6 == 3 | adja$y1_bfs_7 == 3 | adja$y1_bfs_8 == 3 |
                          adja$y1_bfs_9 == 3 | adja$y1_bfs_10 == 3 | adja$y1_bfs_11 == 3 | adja$y1_bfs_3 == 3 | adja$y1_bfs_13 == 3 | adja$y1_bfs_14 == 3 | adja$y1_bfs_15 == 3 | adja$y1_bfs_16 == 3 |
                          adja$y1_bfs_17 == 3  | adja$y1_bfs_18 == 3 | adja$y1_bfs_19 == 3 | adja$y1_bfs_20 == 3, 1,0)

adja$y1_bfs_4n = ifelse(adja$y1_bfs_0 == 4 | adja$y1_bfs_1 == 4 | adja$y1_bfs_2 == 4 | adja$y1_bfs_3 == 4 | adja$y1_bfs_4 == 4 | adja$y1_bfs_5 == 4 | adja$y1_bfs_6 == 4 | adja$y1_bfs_7 == 4 | adja$y1_bfs_8 == 4 |
                          adja$y1_bfs_9 == 4 | adja$y1_bfs_10 == 4 | adja$y1_bfs_11 == 4 | adja$y1_bfs_4 == 4 | adja$y1_bfs_13 == 4 | adja$y1_bfs_14 == 4 | adja$y1_bfs_15 == 4 | adja$y1_bfs_16 == 4 |
                          adja$y1_bfs_17 == 4  | adja$y1_bfs_18 == 4 | adja$y1_bfs_19 == 4 | adja$y1_bfs_20 == 4, 1,0)

adja$y1_bfs_5n = ifelse(adja$y1_bfs_0 == 5 | adja$y1_bfs_1 == 5 | adja$y1_bfs_2 == 5 | adja$y1_bfs_3 == 5 | adja$y1_bfs_4 == 5 | adja$y1_bfs_5 == 5 | adja$y1_bfs_6 == 5 | adja$y1_bfs_7 == 5 | adja$y1_bfs_8 == 5 |
                          adja$y1_bfs_9 == 5 | adja$y1_bfs_10 == 5 | adja$y1_bfs_11 == 5 | adja$y1_bfs_5 == 5 | adja$y1_bfs_13 == 5 | adja$y1_bfs_14 == 5 | adja$y1_bfs_15 == 5 | adja$y1_bfs_16 == 5 |
                          adja$y1_bfs_17 == 5  | adja$y1_bfs_18 == 5 | adja$y1_bfs_19 == 5 | adja$y1_bfs_20 == 5, 1,0)

adja$y1_bfs_6n = ifelse(adja$y1_bfs_0 == 6 | adja$y1_bfs_1 == 6 | adja$y1_bfs_2 == 6 | adja$y1_bfs_3 == 6 | adja$y1_bfs_4 == 6 | adja$y1_bfs_5 == 6 | adja$y1_bfs_6 == 6 | adja$y1_bfs_7 == 6 | adja$y1_bfs_8 == 6 |
                          adja$y1_bfs_9 == 6 | adja$y1_bfs_10 == 6 | adja$y1_bfs_11 == 6 | adja$y1_bfs_6 == 6 | adja$y1_bfs_13 == 6 | adja$y1_bfs_14 == 6 | adja$y1_bfs_15 == 6 | adja$y1_bfs_16 == 6 |
                          adja$y1_bfs_17 == 6  | adja$y1_bfs_18 == 6 | adja$y1_bfs_19 == 6 | adja$y1_bfs_20 == 6, 1,0)

adja$y1_bfs_7n = ifelse(adja$y1_bfs_0 == 7 | adja$y1_bfs_1 == 7 | adja$y1_bfs_2 == 7 | adja$y1_bfs_3 == 7 | adja$y1_bfs_4 == 7 | adja$y1_bfs_5 == 7 | adja$y1_bfs_6 == 7 | adja$y1_bfs_7 == 7 | adja$y1_bfs_8 == 7 |
                          adja$y1_bfs_9 == 7 | adja$y1_bfs_10 == 7 | adja$y1_bfs_11 == 7 | adja$y1_bfs_7 == 7 | adja$y1_bfs_13 == 7 | adja$y1_bfs_14 == 7 | adja$y1_bfs_15 == 7 | adja$y1_bfs_16 == 7 |
                          adja$y1_bfs_17 == 7  | adja$y1_bfs_18 == 7 | adja$y1_bfs_19 == 7 | adja$y1_bfs_20 == 7, 1,0)

adja$y1_bfs_8n = ifelse(adja$y1_bfs_0 == 8 | adja$y1_bfs_1 == 8 | adja$y1_bfs_2 == 8 | adja$y1_bfs_3 == 8 | adja$y1_bfs_4 == 8 | adja$y1_bfs_5 == 8 | adja$y1_bfs_6 == 8 | adja$y1_bfs_7 == 8 | adja$y1_bfs_8 == 8 |
                          adja$y1_bfs_9 == 8 | adja$y1_bfs_10 == 8 | adja$y1_bfs_11 == 8 | adja$y1_bfs_8 == 8 | adja$y1_bfs_13 == 8 | adja$y1_bfs_14 == 8 | adja$y1_bfs_15 == 8 | adja$y1_bfs_16 == 8 |
                          adja$y1_bfs_17 == 8  | adja$y1_bfs_18 == 8 | adja$y1_bfs_19 == 8 | adja$y1_bfs_20 == 8, 1,0)

adja$y1_bfs_9n = ifelse(adja$y1_bfs_0 == 9 | adja$y1_bfs_1 == 9 | adja$y1_bfs_2 == 9 | adja$y1_bfs_3 == 9 | adja$y1_bfs_4 == 9 | adja$y1_bfs_5 == 9 | adja$y1_bfs_6 == 9 | adja$y1_bfs_7 == 9 | adja$y1_bfs_8 == 9 |
                          adja$y1_bfs_9 == 9 | adja$y1_bfs_10 == 9 | adja$y1_bfs_11 == 9 | adja$y1_bfs_9 == 9 | adja$y1_bfs_13 == 9 | adja$y1_bfs_14 == 9 | adja$y1_bfs_15 == 9 | adja$y1_bfs_16 == 9 |
                          adja$y1_bfs_17 == 9  | adja$y1_bfs_18 == 9 | adja$y1_bfs_19 == 9 | adja$y1_bfs_20 == 9, 1,0)

adja$y1_bfs_10n = ifelse(adja$y1_bfs_0 == 10 | adja$y1_bfs_1 == 10 | adja$y1_bfs_2 == 10 | adja$y1_bfs_3 == 10 | adja$y1_bfs_4 == 10 | adja$y1_bfs_5 == 10 | adja$y1_bfs_6 == 10 | adja$y1_bfs_7 == 10 | adja$y1_bfs_8 == 10 |
                           adja$y1_bfs_9 == 10 | adja$y1_bfs_10 == 10 | adja$y1_bfs_11 == 10 | adja$y1_bfs_10 == 10 | adja$y1_bfs_13 == 10 | adja$y1_bfs_14 == 10 | adja$y1_bfs_15 == 10 | adja$y1_bfs_16 == 10 |
                           adja$y1_bfs_17 == 10  | adja$y1_bfs_18 == 10 | adja$y1_bfs_19 == 10 | adja$y1_bfs_20 == 10, 1,0)

adja$y1_bfs_11n = ifelse(adja$y1_bfs_0 == 11 | adja$y1_bfs_1 == 11 | adja$y1_bfs_2 == 11 | adja$y1_bfs_3 == 11 | adja$y1_bfs_4 == 11 | adja$y1_bfs_5 == 11 | adja$y1_bfs_6 == 11 | adja$y1_bfs_7 == 11 | adja$y1_bfs_8 == 11 |
                           adja$y1_bfs_9 == 11 | adja$y1_bfs_10 == 11 | adja$y1_bfs_11 == 11 | adja$y1_bfs_11 == 11 | adja$y1_bfs_13 == 11 | adja$y1_bfs_14 == 11 | adja$y1_bfs_15 == 11 | adja$y1_bfs_16 == 11 |
                           adja$y1_bfs_17 == 11  | adja$y1_bfs_18 == 11 | adja$y1_bfs_19 == 11 | adja$y1_bfs_20 == 11, 1,0)

adja$y1_bfs_12n = ifelse(adja$y1_bfs_0 == 12 | adja$y1_bfs_1 == 12 | adja$y1_bfs_2 == 12 | adja$y1_bfs_3 == 12 | adja$y1_bfs_4 == 12 | adja$y1_bfs_5 == 12 | adja$y1_bfs_6 == 12 | adja$y1_bfs_7 == 12 | adja$y1_bfs_8 == 12 |
                           adja$y1_bfs_9 == 12 | adja$y1_bfs_10 == 12 | adja$y1_bfs_11 == 12 | adja$y1_bfs_12 == 12 | adja$y1_bfs_13 == 12 | adja$y1_bfs_14 == 12 | adja$y1_bfs_15 == 12 | adja$y1_bfs_16 == 12 |
                           adja$y1_bfs_17 == 12  | adja$y1_bfs_18 == 12 | adja$y1_bfs_19 == 12 | adja$y1_bfs_20 == 12, 1,0)

adja$y1_bfs_13n = ifelse(adja$y1_bfs_0 == 13 | adja$y1_bfs_1 == 13 | adja$y1_bfs_2 == 13 | adja$y1_bfs_3 == 13 | adja$y1_bfs_4 == 13 | adja$y1_bfs_5 == 13 | adja$y1_bfs_6 == 13 | adja$y1_bfs_7 == 13 | adja$y1_bfs_8 == 13 |
                           adja$y1_bfs_9 == 13 | adja$y1_bfs_10 == 13 | adja$y1_bfs_11 == 13 | adja$y1_bfs_13 == 13 | adja$y1_bfs_13 == 13 | adja$y1_bfs_14 == 13 | adja$y1_bfs_15 == 13 | adja$y1_bfs_16 == 13 |
                           adja$y1_bfs_17 == 13  | adja$y1_bfs_18 == 13 | adja$y1_bfs_19 == 13 | adja$y1_bfs_20 == 13, 1,0)

adja$y1_bfs_14n = ifelse(adja$y1_bfs_0 == 14 | adja$y1_bfs_1 == 14 | adja$y1_bfs_2 == 14 | adja$y1_bfs_3 == 14 | adja$y1_bfs_4 == 14 | adja$y1_bfs_5 == 14 | adja$y1_bfs_6 == 14 | adja$y1_bfs_7 == 14 | adja$y1_bfs_8 == 14 |
                           adja$y1_bfs_9 == 14 | adja$y1_bfs_10 == 14 | adja$y1_bfs_11 == 14 | adja$y1_bfs_14 == 14 | adja$y1_bfs_13 == 14 | adja$y1_bfs_14 == 14 | adja$y1_bfs_15 == 14 | adja$y1_bfs_16 == 14 |
                           adja$y1_bfs_17 == 14  | adja$y1_bfs_18 == 14 | adja$y1_bfs_19 == 14 | adja$y1_bfs_20 == 14, 1,0)

adja$y1_bfs_15n = ifelse(adja$y1_bfs_0 == 15 | adja$y1_bfs_1 == 15 | adja$y1_bfs_2 == 15 | adja$y1_bfs_3 == 15 | adja$y1_bfs_4 == 15 | adja$y1_bfs_5 == 15 | adja$y1_bfs_6 == 15 | adja$y1_bfs_7 == 15 | adja$y1_bfs_8 == 15 |
                           adja$y1_bfs_9 == 15 | adja$y1_bfs_10 == 15 | adja$y1_bfs_11 == 15 | adja$y1_bfs_15 == 15 | adja$y1_bfs_13 == 15 | adja$y1_bfs_14 == 15 | adja$y1_bfs_15 == 15 | adja$y1_bfs_16 == 15 |
                           adja$y1_bfs_17 == 15  | adja$y1_bfs_18 == 15 | adja$y1_bfs_19 == 15 | adja$y1_bfs_20 == 15, 1,0)

adja$y1_bfs_16n = ifelse(adja$y1_bfs_0 == 16 | adja$y1_bfs_1 == 16 | adja$y1_bfs_2 == 16 | adja$y1_bfs_3 == 16 | adja$y1_bfs_4 == 16 | adja$y1_bfs_5 == 16 | adja$y1_bfs_6 == 16 | adja$y1_bfs_7 == 16 | adja$y1_bfs_8 == 16 |
                           adja$y1_bfs_9 == 16 | adja$y1_bfs_10 == 16 | adja$y1_bfs_11 == 16 | adja$y1_bfs_16 == 16 | adja$y1_bfs_13 == 16 | adja$y1_bfs_14 == 16 | adja$y1_bfs_15 == 16 | adja$y1_bfs_16 == 16 |
                           adja$y1_bfs_17 == 16  | adja$y1_bfs_18 == 16 | adja$y1_bfs_19 == 16 | adja$y1_bfs_20 == 16, 1,0)

adja$y1_bfs_17n = ifelse(adja$y1_bfs_0 == 17 | adja$y1_bfs_1 == 17 | adja$y1_bfs_2 == 17 | adja$y1_bfs_3 == 17 | adja$y1_bfs_4 == 17 | adja$y1_bfs_5 == 17 | adja$y1_bfs_6 == 17 | adja$y1_bfs_7 == 17 | adja$y1_bfs_8 == 17 |
                           adja$y1_bfs_9 == 17 | adja$y1_bfs_10 == 17 | adja$y1_bfs_11 == 17 | adja$y1_bfs_17 == 17 | adja$y1_bfs_13 == 17 | adja$y1_bfs_14 == 17 | adja$y1_bfs_15 == 17 | adja$y1_bfs_16 == 17 |
                           adja$y1_bfs_17 == 17  | adja$y1_bfs_18 == 17 | adja$y1_bfs_19 == 17 | adja$y1_bfs_20 == 17, 1,0)

adja$y1_bfs_18n = ifelse(adja$y1_bfs_0 == 18 | adja$y1_bfs_1 == 18 | adja$y1_bfs_2 == 18 | adja$y1_bfs_3 == 18 | adja$y1_bfs_4 == 18 | adja$y1_bfs_5 == 18 | adja$y1_bfs_6 == 18 | adja$y1_bfs_7 == 18 | adja$y1_bfs_8 == 18 |
                           adja$y1_bfs_9 == 18 | adja$y1_bfs_10 == 18 | adja$y1_bfs_11 == 18 | adja$y1_bfs_18 == 18 | adja$y1_bfs_13 == 18 | adja$y1_bfs_14 == 18 | adja$y1_bfs_15 == 18 | adja$y1_bfs_16 == 18 |
                           adja$y1_bfs_17 == 18  | adja$y1_bfs_18 == 18 | adja$y1_bfs_19 == 18 | adja$y1_bfs_20 == 18, 1,0)

adja$y1_bfs_19n = ifelse(adja$y1_bfs_0 == 19 | adja$y1_bfs_1 == 19 | adja$y1_bfs_2 == 19 | adja$y1_bfs_3 == 19 | adja$y1_bfs_4 == 19 | adja$y1_bfs_5 == 19 | adja$y1_bfs_6 == 19 | adja$y1_bfs_7 == 19 | adja$y1_bfs_8 == 19 |
                           adja$y1_bfs_9 == 19 | adja$y1_bfs_10 == 19 | adja$y1_bfs_11 == 19 | adja$y1_bfs_19 == 19 | adja$y1_bfs_13 == 19 | adja$y1_bfs_14 == 19 | adja$y1_bfs_15 == 19 | adja$y1_bfs_16 == 19 |
                           adja$y1_bfs_17 == 19  | adja$y1_bfs_18 == 19 | adja$y1_bfs_19 == 19 | adja$y1_bfs_20 == 19, 1,0)

adja$y1_bfs_20n = ifelse(adja$y1_bfs_0 == 20 | adja$y1_bfs_1 == 20 | adja$y1_bfs_2 == 20 | adja$y1_bfs_3 == 20 | adja$y1_bfs_4 == 20 | adja$y1_bfs_5 == 20 | adja$y1_bfs_6 == 20 | adja$y1_bfs_7 == 20 | adja$y1_bfs_8 == 20 |
                           adja$y1_bfs_9 == 20 | adja$y1_bfs_10 == 20 | adja$y1_bfs_11 == 20 | adja$y1_bfs_20 == 20 | adja$y1_bfs_13 == 20 | adja$y1_bfs_14 == 20 | adja$y1_bfs_15 == 20 | adja$y1_bfs_16 == 20 |
                           adja$y1_bfs_17 == 20  | adja$y1_bfs_18 == 20 | adja$y1_bfs_19 == 20 | adja$y1_bfs_20 == 20, 1,0)



adja$y1_bfs_21n = ifelse(adja$y1_bfs_0 == 21 | adja$y1_bfs_1 == 21 | adja$y1_bfs_2 == 21 | adja$y1_bfs_3 == 21 | adja$y1_bfs_4 == 21 | adja$y1_bfs_5 == 21 | adja$y1_bfs_6 == 21 | adja$y1_bfs_7 == 21 | adja$y1_bfs_8 == 21 |
                           adja$y1_bfs_9 == 21 | adja$y1_bfs_10 == 21 | adja$y1_bfs_11 == 21 | adja$y1_bfs_21 == 21 | adja$y1_bfs_13 == 21 | adja$y1_bfs_14 == 21 | adja$y1_bfs_15 == 21 | adja$y1_bfs_16 == 21 |
                           adja$y1_bfs_17 == 21  | adja$y1_bfs_18 == 21 | adja$y1_bfs_19 == 21 | adja$y1_bfs_20 == 21, 1,0)

adja$y1_bfs_22n = ifelse(adja$y1_bfs_0 == 22 | adja$y1_bfs_1 == 22 | adja$y1_bfs_2 == 22 | adja$y1_bfs_3 == 22 | adja$y1_bfs_4 == 22 | adja$y1_bfs_5 == 22 | adja$y1_bfs_6 == 22 | adja$y1_bfs_7 == 22 | adja$y1_bfs_8 == 22 |
                           adja$y1_bfs_9 == 22 | adja$y1_bfs_10 == 22 | adja$y1_bfs_11 == 22 | adja$y1_bfs_22 == 22 | adja$y1_bfs_13 == 22 | adja$y1_bfs_14 == 22 | adja$y1_bfs_15 == 22 | adja$y1_bfs_16 == 22 |
                           adja$y1_bfs_17 == 22  | adja$y1_bfs_18 == 22 | adja$y1_bfs_19 == 22 | adja$y1_bfs_20 == 22, 1,0)

adja$y1_bfs_23n = ifelse(adja$y1_bfs_0 == 23 | adja$y1_bfs_1 == 23 | adja$y1_bfs_2 == 23 | adja$y1_bfs_3 == 23 | adja$y1_bfs_4 == 23 | adja$y1_bfs_5 == 23 | adja$y1_bfs_6 == 23 | adja$y1_bfs_7 == 23 | adja$y1_bfs_8 == 23 |
                           adja$y1_bfs_9 == 23 | adja$y1_bfs_10 == 23 | adja$y1_bfs_11 == 23 | adja$y1_bfs_23 == 23 | adja$y1_bfs_13 == 23 | adja$y1_bfs_14 == 23 | adja$y1_bfs_15 == 23 | adja$y1_bfs_16 == 23 |
                           adja$y1_bfs_17 == 23  | adja$y1_bfs_18 == 23 | adja$y1_bfs_19 == 23 | adja$y1_bfs_20 == 23, 1,0)

adja$y1_bfs_24n = ifelse(adja$y1_bfs_0 == 24 | adja$y1_bfs_1 == 24 | adja$y1_bfs_2 == 24 | adja$y1_bfs_3 == 24 | adja$y1_bfs_4 == 24 | adja$y1_bfs_5 == 24 | adja$y1_bfs_6 == 24 | adja$y1_bfs_7 == 24 | adja$y1_bfs_8 == 24 |
                           adja$y1_bfs_9 == 24 | adja$y1_bfs_10 == 24 | adja$y1_bfs_11 == 24 | adja$y1_bfs_24 == 24 | adja$y1_bfs_13 == 24 | adja$y1_bfs_14 == 24 | adja$y1_bfs_15 == 24 | adja$y1_bfs_16 == 24 |
                           adja$y1_bfs_17 == 24  | adja$y1_bfs_18 == 24 | adja$y1_bfs_19 == 24 | adja$y1_bfs_20 == 24, 1,0)

adja$y1_bfs_25n = ifelse(adja$y1_bfs_0 == 25 | adja$y1_bfs_1 == 25 | adja$y1_bfs_2 == 25 | adja$y1_bfs_3 == 25 | adja$y1_bfs_4 == 25 | adja$y1_bfs_5 == 25 | adja$y1_bfs_6 == 25 | adja$y1_bfs_7 == 25 | adja$y1_bfs_8 == 25 |
                           adja$y1_bfs_9 == 25 | adja$y1_bfs_10 == 25 | adja$y1_bfs_11 == 25 | adja$y1_bfs_25 == 25 | adja$y1_bfs_13 == 25 | adja$y1_bfs_14 == 25 | adja$y1_bfs_15 == 25 | adja$y1_bfs_16 == 25 |
                           adja$y1_bfs_17 == 25  | adja$y1_bfs_18 == 25 | adja$y1_bfs_19 == 25 | adja$y1_bfs_20 == 25, 1,0)

adja$y1_bfs_26n = ifelse(adja$y1_bfs_0 == 26 | adja$y1_bfs_1 == 26 | adja$y1_bfs_2 == 26 | adja$y1_bfs_3 == 26 | adja$y1_bfs_4 == 26 | adja$y1_bfs_5 == 26 | adja$y1_bfs_6 == 26 | adja$y1_bfs_7 == 26 | adja$y1_bfs_8 == 26 |
                           adja$y1_bfs_9 == 26 | adja$y1_bfs_10 == 26 | adja$y1_bfs_11 == 26 | adja$y1_bfs_26 == 26 | adja$y1_bfs_13 == 26 | adja$y1_bfs_14 == 26 | adja$y1_bfs_15 == 26 | adja$y1_bfs_16 == 26 |
                           adja$y1_bfs_17 == 26  | adja$y1_bfs_18 == 26 | adja$y1_bfs_19 == 26 | adja$y1_bfs_20 == 26, 1,0)

adja$y1_bfs_27n = ifelse(adja$y1_bfs_0 == 27 | adja$y1_bfs_1 == 27 | adja$y1_bfs_2 == 27 | adja$y1_bfs_3 == 27 | adja$y1_bfs_4 == 27 | adja$y1_bfs_5 == 27 | adja$y1_bfs_6 == 27 | adja$y1_bfs_7 == 27 | adja$y1_bfs_8 == 27 |
                           adja$y1_bfs_9 == 27 | adja$y1_bfs_10 == 27 | adja$y1_bfs_11 == 27 | adja$y1_bfs_27 == 27 | adja$y1_bfs_13 == 27 | adja$y1_bfs_14 == 27 | adja$y1_bfs_15 == 27 | adja$y1_bfs_16 == 27 |
                           adja$y1_bfs_17 == 27  | adja$y1_bfs_18 == 27 | adja$y1_bfs_19 == 27 | adja$y1_bfs_20 == 27, 1,0)

adja$y1_bfs_28n = ifelse(adja$y1_bfs_0 == 28 | adja$y1_bfs_1 == 28 | adja$y1_bfs_2 == 28 | adja$y1_bfs_3 == 28 | adja$y1_bfs_4 == 28 | adja$y1_bfs_5 == 28 | adja$y1_bfs_6 == 28 | adja$y1_bfs_7 == 28 | adja$y1_bfs_8 == 28 |
                           adja$y1_bfs_9 == 28 | adja$y1_bfs_10 == 28 | adja$y1_bfs_11 == 28 | adja$y1_bfs_28 == 28 | adja$y1_bfs_13 == 28 | adja$y1_bfs_14 == 28 | adja$y1_bfs_15 == 28 | adja$y1_bfs_16 == 28 |
                           adja$y1_bfs_17 == 28  | adja$y1_bfs_18 == 28 | adja$y1_bfs_19 == 28 | adja$y1_bfs_20 == 28, 1,0)

adja$y1_bfs_29n = ifelse(adja$y1_bfs_0 == 29 | adja$y1_bfs_1 == 29 | adja$y1_bfs_2 == 29 | adja$y1_bfs_3 == 29 | adja$y1_bfs_4 == 29 | adja$y1_bfs_5 == 29 | adja$y1_bfs_6 == 29 | adja$y1_bfs_7 == 29 | adja$y1_bfs_8 == 29 |
                           adja$y1_bfs_9 == 29 | adja$y1_bfs_10 == 29 | adja$y1_bfs_11 == 29 | adja$y1_bfs_29 == 29 | adja$y1_bfs_13 == 29 | adja$y1_bfs_14 == 29 | adja$y1_bfs_15 == 29 | adja$y1_bfs_16 == 29 |
                           adja$y1_bfs_17 == 29  | adja$y1_bfs_18 == 29 | adja$y1_bfs_19 == 29 | adja$y1_bfs_20 == 29, 1,0)

adja$y1_bfs_30n = ifelse(adja$y1_bfs_0 == 30 | adja$y1_bfs_1 == 30 | adja$y1_bfs_2 == 30 | adja$y1_bfs_3 == 30 | adja$y1_bfs_4 == 30 | adja$y1_bfs_5 == 30 | adja$y1_bfs_6 == 30 | adja$y1_bfs_7 == 30 | adja$y1_bfs_8 == 30 |
                           adja$y1_bfs_9 == 30 | adja$y1_bfs_10 == 30 | adja$y1_bfs_11 == 30 | adja$y1_bfs_30 == 30 | adja$y1_bfs_13 == 30 | adja$y1_bfs_14 == 30 | adja$y1_bfs_15 == 30 | adja$y1_bfs_16 == 30 |
                           adja$y1_bfs_17 == 30  | adja$y1_bfs_18 == 30 | adja$y1_bfs_19 == 30 | adja$y1_bfs_20 == 30, 1,0)


######################################################################################################################
######################################################################################################################
###
###                     MM models
###
######################################################################################################################
######################################################################################################################

egonetsB <- adja


# class variable
egonetsB$class <- as.numeric(substring(egonetsB$wave1_id, 1,nchar(egonetsB$wave1_id)-2))

egonetsB[,2:22]<- 0

## now make a matrix with the identifiers   ==> this is important!!!
for (i in 1:nrow(egonetsB)){
  if (egonetsB$y1_bfs_1n[i] != 0){
    egonetsB$y1_bfs_1[i] <- as.numeric(paste(egonetsB$class[i],'01', sep = "", collapse=""))
    i <- i+1
  }
}
for (i in 1:nrow(egonetsB)){
  if (egonetsB$y1_bfs_2n[i] != 0){
    egonetsB$y1_bfs_2[i] <- as.numeric(paste(egonetsB$class[i],'02', sep = "", collapse=""))
    i <- i+1
  }
}
for (i in 1:nrow(egonetsB)){
  if (egonetsB$y1_bfs_3n[i] != 0){
    egonetsB$y1_bfs_3[i] <- as.numeric(paste(egonetsB$class[i],'03', sep = "", collapse=""))
    i <- i+1
  }
}
for (i in 1:nrow(egonetsB)){
  if (egonetsB$y1_bfs_4n[i] != 0){
    egonetsB$y1_bfs_4[i] <- as.numeric(paste(egonetsB$class[i],'04', sep = "", collapse=""))
    i <- i+1
  }
}
for (i in 1:nrow(egonetsB)){
  if (egonetsB$y1_bfs_5n[i] != 0){
    egonetsB$y1_bfs_5[i] <- as.numeric(paste(egonetsB$class[i],'05', sep = "", collapse=""))
    i <- i+1
  }
}

for (i in 1:nrow(egonetsB)){
  if (egonetsB$y1_bfs_6n[i] != 0){
    egonetsB$y1_bfs_6[i] <- as.numeric(paste(egonetsB$class[i],'06', sep = "", collapse=""))
    i <- i+1
  }
}
for (i in 1:nrow(egonetsB)){
  if (egonetsB$y1_bfs_7n[i] != 0){
    egonetsB$y1_bfs_7[i] <- as.numeric(paste(egonetsB$class[i],'07', sep = "", collapse=""))
    i <- i+1
  }
}
for (i in 1:nrow(egonetsB)){
  if (egonetsB$y1_bfs_8n[i] != 0){
    egonetsB$y1_bfs_8[i] <- as.numeric(paste(egonetsB$class[i],'08', sep = "", collapse=""))
    i <- i+1
  }
}
for (i in 1:nrow(egonetsB)){
  if (egonetsB$y1_bfs_9n[i] != 0){
    egonetsB$y1_bfs_9[i] <- as.numeric(paste(egonetsB$class[i],'09', sep = "", collapse=""))
    i <- i+1
  }
}
for (i in 1:nrow(egonetsB)){
  if (egonetsB$y1_bfs_10n[i] != 0){
    egonetsB$y1_bfs_10[i] <- as.numeric(paste(egonetsB$class[i],'10', sep = "", collapse=""))
    i <- i+1
  }
}

for (i in 1:nrow(egonetsB)){
  if (egonetsB$y1_bfs_11n[i] != 0){
    egonetsB$y1_bfs_11[i] <- as.numeric(paste(egonetsB$class[i],'11', sep = "", collapse=""))
    i <- i+1
  }
}
for (i in 1:nrow(egonetsB)){
  if (egonetsB$y1_bfs_12n[i] != 0){
    egonetsB$y1_bfs_12[i] <- as.numeric(paste(egonetsB$class[i],'12', sep = "", collapse=""))
    i <- i+1
  }
}
for (i in 1:nrow(egonetsB)){
  if (egonetsB$y1_bfs_13n[i] != 0){
    egonetsB$y1_bfs_13[i] <- as.numeric(paste(egonetsB$class[i],'13', sep = "", collapse=""))
    i <- i+1
  }
}
for (i in 1:nrow(egonetsB)){
  if (egonetsB$y1_bfs_14n[i] != 0){
    egonetsB$y1_bfs_14[i] <- as.numeric(paste(egonetsB$class[i],'14', sep = "", collapse=""))
    i <- i+1
  }
}
for (i in 1:nrow(egonetsB)){
  if (egonetsB$y1_bfs_15n[i] != 0){
    egonetsB$y1_bfs_15[i] <- as.numeric(paste(egonetsB$class[i],'15', sep = "", collapse=""))
    i <- i+1
  }
}

for (i in 1:nrow(egonetsB)){
  if (egonetsB$y1_bfs_16n[i] != 0){
    egonetsB$y1_bfs_16[i] <- as.numeric(paste(egonetsB$class[i],'16', sep = "", collapse=""))
    i <- i+1
  }
}
for (i in 1:nrow(egonetsB)){
  if (egonetsB$y1_bfs_17n[i] != 0){
    egonetsB$y1_bfs_17[i] <- as.numeric(paste(egonetsB$class[i],'17', sep = "", collapse=""))
    i <- i+1
  }
}
for (i in 1:nrow(egonetsB)){
  if (egonetsB$y1_bfs_18n[i] != 0){
    egonetsB$y1_bfs_18[i] <- as.numeric(paste(egonetsB$class[i],'18', sep = "", collapse=""))
    i <- i+1
  }
}
for (i in 1:nrow(egonetsB)){
  if (egonetsB$y1_bfs_19n[i] != 0){
    egonetsB$y1_bfs_19[i] <- as.numeric(paste(egonetsB$class[i],'19', sep = "", collapse=""))
    i <- i+1
  }
}
for (i in 1:nrow(egonetsB)){
  if (egonetsB$y1_bfs_20n[i] != 0){
    egonetsB$y1_bfs_20[i] <- as.numeric(paste(egonetsB$class[i],'20', sep = "", collapse=""))
    i <- i+1
  }
}

for (i in 1:nrow(egonetsB)){
  if (egonetsB$y1_bfs_21n[i] != 0){
    egonetsB$y1_bfs_21[i] <- as.numeric(paste(egonetsB$class[i],'21', sep = "", collapse=""))
    i <- i+1
  }
}
for (i in 1:nrow(egonetsB)){
  if (egonetsB$y1_bfs_22n[i] != 0){
    egonetsB$y1_bfs_22[i] <- as.numeric(paste(egonetsB$class[i],'22', sep = "", collapse=""))
    i <- i+1
  }
}
for (i in 1:nrow(egonetsB)){
  if (egonetsB$y1_bfs_23n[i] != 0){
    egonetsB$y1_bfs_23[i] <- as.numeric(paste(egonetsB$class[i],'23', sep = "", collapse=""))
    i <- i+1
  }
}
for (i in 1:nrow(egonetsB)){
  if (egonetsB$y1_bfs_24n[i] != 0){
    egonetsB$y1_bfs_24[i] <- as.numeric(paste(egonetsB$class[i],'24', sep = "", collapse=""))
    i <- i+1
  }
}
for (i in 1:nrow(egonetsB)){
  if (egonetsB$y1_bfs_25n[i] != 0){
    egonetsB$y1_bfs_25[i] <- as.numeric(paste(egonetsB$class[i],'25', sep = "", collapse=""))
    i <- i+1
  }
}

for (i in 1:nrow(egonetsB)){
  if (egonetsB$y1_bfs_26n[i] != 0){
    egonetsB$y1_bfs_26[i] <- as.numeric(paste(egonetsB$class[i],'26', sep = "", collapse=""))
    i <- i+1
  }
}
for (i in 1:nrow(egonetsB)){
  if (egonetsB$y1_bfs_27n[i] != 0){
    egonetsB$y1_bfs_27[i] <- as.numeric(paste(egonetsB$class[i],'27', sep = "", collapse=""))
    i <- i+1
  }
}
for (i in 1:nrow(egonetsB)){
  if (egonetsB$y1_bfs_28n[i] != 0){
    egonetsB$y1_bfs_28[i] <- as.numeric(paste(egonetsB$class[i],'28', sep = "", collapse=""))
    i <- i+1
  }
}
for (i in 1:nrow(egonetsB)){
  if (egonetsB$y1_bfs_29n[i] != 0){
    egonetsB$y1_bfs_29[i] <- as.numeric(paste(egonetsB$class[i],'29', sep = "", collapse=""))
    i <- i+1
  }
}
for (i in 1:nrow(egonetsB)){
  if (egonetsB$y1_bfs_30n[i] != 0){
    egonetsB$y1_bfs_30[i] <- as.numeric(paste(egonetsB$class[i],'30', sep = "", collapse=""))
    i <- i+1
  }
}


egonetB <- egonetsB
egonetB <- egonetB[!egonetB$group_status == 0,]   # delete the pupils with questionable group status (computed in Yeasle's code)


################################################
#
#            tie weights
#
##############################################

## equal weighting scheme  -> questionable
## also does not lead to significant effect
# egonetB2$totalties <- rowSums(egonetB2[,23:42])
# egonetB2[,23:42] <- egonetB2[,23:42]/egonetB2$totalties
# egonetB2[,23:42][is.na(egonetB2[,23:42])]<- 0

## 'correcting' for amount of ties 
egonetB$totalties <- rowSums(abs(egonetB[,122:151]))
egonetB[,122:151] <- egonetB[,122:151]/sqrt(egonetB$totalties)
egonetB[,122:151][is.na(egonetB[,122:151])]<- 0


## 3th option: leave ties '1' as one
# do nothing
# egonetB[,122:151][is.na(egonetB[,122:151])]<- 0


################################################
#
#            CFA majority contact
#
##############################################

#items <- egonC1 %>% select(y1_bgfr1,y1_rescs1,y1_recn1,y1_compn)  # test for when only minorities in the data
items <- egonetB %>% dplyr::select(y1_bgfr1,y1_rescs1,y1_recn1,y1_compn)
corr <- round(cor(items, use = "complete.obs"),1)
ggcorrplot(corr)


model <- '
contact =~ y1_rescs1+y1_bgfr1+y1_recn1+y1_compn
'

fit <- cfa(model,data=items, estimator = 'MLF', , missing = "ML")
summary(fit, standardized=T, fit.measures=T) 
semPaths(fit,"model","stand",style="LISREL",rotation=1, 
         edge.color="black",sizeLat=4,layout = 'tree',
         edge.label.cex=0.6,mar=c(4,1,4,1),sizeMan = 4)
mi<-inspect(fit,"mi")
mi.sorted<- mi[order(-mi$mi),]  # sort from high to low
mi.sorted[1:7,]



## put the factor scores in the data
idx <- lavInspect(fit, "case.idx")
fscores <- lavPredict(fit,method = "regression")
## loop over factors
for (fs in colnames(fscores)) {
  egonetB[idx, fs] <- fscores[ , fs]
}

# check if it is correct
items2 <- egonetB %>% select(y1_bgfr1,y1_rescs1,y1_recn1,y1_compn,contact)
corr <- round(cor(items2, use = "complete.obs"),1)
ggcorrplot(corr) # yes they correlate very well



################################################
#
#            class frequency
#
##############################################

## problem: pupils with only 2 other people in their class -> their weights soley depend on who completed questionnaire
## take out classes have less than 50% of their class participating + minimum 5 people per class
## small egonets will cause estimation problems in MLwiN with the MM method!!
table(alldata$group_status)
table(egonetB$group_status)

class <- as.numeric(egonetB$class)

classfreq <- data.frame(table(class))
classfreq
sum(classfreq$Freq)

lengthegonetB <- length(egonetB$wave1_id)
egonetB$classFreq <- rep(0,lengthegonetB)
classfreq$class <- as.numeric(as.character(classfreq$class))

for (k in 1:nrow(egonetB)){
  for (i in 1:nrow(classfreq)){
    if (egonetB$class[k] == classfreq[i,1]){
      egonetB[k,155] <- classfreq[i,2]
      i <- i+1
      k <- k+1
    }
  }
}

egonetB$classinsample <- (egonetB[,155]/egonetB[,47])


classfreq50 <- subset(egonetB, classinsample>0.5)

# also exclude classes smaller then 5
classfreq50 <- subset(classfreq50, y1_clsize>5)

# before exclusion
group_status_table <- data.frame(table(egonetB$group_status))
group_status_table
sum(group_status_table$Freq)

# after exclusion
group_status_table <- data.frame(table(classfreq50$group_status))
group_status_table
sum(group_status_table$Freq)

#total
table(alldata$group_status)
table(egonetB$group_status)
table(classfreq50$group_status) 

table(classfreq50$sex)
summary(classfreq50$age)

#######################################################
#
#            popularity
#
#######################################################

###################
# y1_pos network popularity
####################

pop1 <- data.frame(table(classfreq50$y1_pos_1))

pop1 <- (classfreq50$y1_pos_1)
pop1 <- append(pop1, classfreq50$y1_pos_2)
pop1 <- append(pop1, classfreq50$y1_pos_3)
pop1 <- append(pop1, classfreq50$y1_pos_4)
pop1 <- append(pop1, classfreq50$y1_pos_5)

pop1 <- as.data.frame(pop1)
#pop1 <- as.numeric(as.character(pop1$))
popular <- data.frame(table(pop1))
popular <- popular[c(-1),]

popular$pop1 <- as.numeric(as.character(popular$pop1))   # warning doesn't matter
popular[,1][is.na(popular[,1])]<- 0



lengthclassfreq50 <- length(classfreq50$wave1_id)
classfreq50$popular <- rep(0,lengthclassfreq50)


for (k in 1:nrow(classfreq50)){
  for (i in 1:nrow(popular)){
    if (classfreq50$wave1_id[k] == popular[i,1]){
      classfreq50[k,157] <- popular[i,2]
      i <- i+1
      k <- k+1
    }
  }
}


popularity <- classfreq50




###########################
##  indegrees per pupil
##########################



#test
egonetM <- popularity[1:20,]


#Do not use all the for loops, it will take forever again
#frequency of the pupils mentioned as who other like
indegree1 <- as.data.frame(table(popularity$y1_bfs_1))
indegree2 <- as.data.frame(table(popularity$y1_bfs_2))
indegree3 <- as.data.frame(table(popularity$y1_bfs_3))
indegree4 <- as.data.frame(table(popularity$y1_bfs_4))
indegree5 <- as.data.frame(table(popularity$y1_bfs_5))
indegree6 <- as.data.frame(table(popularity$y1_bfs_6))
indegree7 <- as.data.frame(table(popularity$y1_bfs_7))
indegree8 <- as.data.frame(table(popularity$y1_bfs_8))
indegree9 <- as.data.frame(table(popularity$y1_bfs_9))
indegree10 <- as.data.frame(table(popularity$y1_bfs_10))

indegree11 <- as.data.frame(table(popularity$y1_bfs_11))
indegree12 <- as.data.frame(table(popularity$y1_bfs_12))
indegree13 <- as.data.frame(table(popularity$y1_bfs_13))
indegree14 <- as.data.frame(table(popularity$y1_bfs_14))
indegree15 <- as.data.frame(table(popularity$y1_bfs_15))
indegree16 <- as.data.frame(table(popularity$y1_bfs_16))
indegree17 <- as.data.frame(table(popularity$y1_bfs_17))
indegree18 <- as.data.frame(table(popularity$y1_bfs_18))
indegree19 <- as.data.frame(table(popularity$y1_bfs_19))
indegree20 <- as.data.frame(table(popularity$y1_bfs_20))

indegree21 <- as.data.frame(table(popularity$y1_bfs_21))
indegree22 <- as.data.frame(table(popularity$y1_bfs_22))
indegree23 <- as.data.frame(table(popularity$y1_bfs_23))
indegree24 <- as.data.frame(table(popularity$y1_bfs_24))
indegree25 <- as.data.frame(table(popularity$y1_bfs_25))
indegree26 <- as.data.frame(table(popularity$y1_bfs_26))
indegree27 <- as.data.frame(table(popularity$y1_bfs_27))
indegree28 <- as.data.frame(table(popularity$y1_bfs_28))
indegree29 <- as.data.frame(table(popularity$y1_bfs_29))

# put in one dataset
indegreeN <- rbind(indegree1,indegree2,indegree3,indegree4,indegree5,indegree6,indegree7,indegree8,indegree9,indegree10,
                   indegree11,indegree12,indegree13,indegree14,indegree15,indegree16,indegree17,indegree18,indegree19,indegree20,
                   indegree21,indegree22,indegree23,indegree24,indegree25,indegree26,indegree27,indegree28,indegree29)

#construct new variable in datafile, match value of first column indegreeN (id column) to the wave1_id
# and put values of second column in this new variable

lengthpopularity <- length(popularity$wave1_id)
popularity$indegree <- rep(0,lengthpopularity)
indegreeN2 <- indegreeN[!(indegreeN$Var1==0),]
indegreeN2$Var1 <- as.numeric(as.character(indegreeN2$Var1))

## not all people from the list will get into the data because we don't have any responses from them (so also no info if they were majority or not)
for (k in 1:nrow(popularity)){
  for (i in 1:nrow(indegreeN2)){
    if (popularity$wave1_id[k] == indegreeN2[i,1]){
      popularity[k,158] <- indegreeN2[i,2]
      i <- i+1
      k <- k+1
    }
  }
}

popularityitems <- popularity %>% select(popular, indegree)
corr <- round(cor(popularityitems, use = "complete.obs"),1)
ggcorrplot(corr)
corr
summary(popularity$indegree)
summary(popularity$popular)












###############################################################################################
## making network predictor variables
## multiply network weights with the according values in a variable
###############################################################################################


## gneral code to do it across columns, but doesnt work
#for (i in 1:nrow(egonetM)){
#    for (k in 1:nrow(egonetM)){
#      for (b in 2:ncol(egonetM)){
#         if (egonetM$wave1_id[i] == egonetM[k,b]){
#           egonetM[k,b+21] <- egonetM[k,b+21]*egonetM$y1_em_fit[i]
#           i <- i+1
#           k <- k+1
#           b <- b+1
#    }
#   }
#  }
# }


# clean dataset and add weight matrix (to make predictors) of egonB to dataset
egonC <- popularity %>% select(1,3:22,49:58, #wave1_id and network
                               y1_bfs_1n:y1_bfs_30n,
                               age:pupil0,
                               class:indegree,
                               )

predictorw <- popularity %>% select(1, y1_bfs_1n:y1_bfs_30n)

names(predictorw)[2:31] <- paste0("P", names(predictorw[,2:31]))

egonD <- data.frame(egonC,predictorw)



################################################ test
# test code
egonD <- popularity[1:20,]


#for real
egonD <- popularity


##########################################################
## problem: if an alter (y1_bfs_1 - ...) is not in our data as a responder (wave1_id), delete this person from
##          weight matrix 
##########################################################

# Prototype code:   (this works but only for every column)
#for (k in 1:nrow(egonetM2)){if ((!any(egonetM2[k,2] %in% egonetM2$wave1_id))){egonetM2[k,2+30] <- 0 
#k <- k+1}}

## for all columns  (does not work)
# for (k in 1:nrow(egonetM2)){
#   for (i in 2:ncol(egonetM2)){
#     if ((!any(egonetM2[k,i] %in% egonetM2$wave1_id))){
#       egonetM2[k,i+121] <- 0
#       k <- k+1
#       i <- i+1
#       if (i > 30){
#         break
#       }
#     }
#   }
# }

### now do it for all 30 columns on the real data
### egonB[k,2+84]  -> the place where your weight matrix starts

for (k in 1:nrow(egonD)){if ((!any(egonD[k,2] %in% egonD$wave1_id))){egonD[k,2+84] <- 0
k <- k+1}}
for (k in 1:nrow(egonD)){if ((!any(egonD[k,3] %in% egonD$wave1_id))){egonD[k,3+84] <- 0
k <- k+1}}
for (k in 1:nrow(egonD)){if ((!any(egonD[k,4] %in% egonD$wave1_id))){egonD[k,4+84] <- 0
k <- k+1}}
for (k in 1:nrow(egonD)){if ((!any(egonD[k,5] %in% egonD$wave1_id))){egonD[k,5+84] <- 0
k <- k+1}}
for (k in 1:nrow(egonD)){if ((!any(egonD[k,6] %in% egonD$wave1_id))){egonD[k,6+84] <- 0
k <- k+1}}

for (k in 1:nrow(egonD)){if ((!any(egonD[k,7] %in% egonD$wave1_id))){egonD[k,7+84] <- 0
k <- k+1}}
for (k in 1:nrow(egonD)){if ((!any(egonD[k,8] %in% egonD$wave1_id))){egonD[k,8+84] <- 0
k <- k+1}}
for (k in 1:nrow(egonD)){if ((!any(egonD[k,9] %in% egonD$wave1_id))){egonD[k,9+84] <- 0
k <- k+1}}
for (k in 1:nrow(egonD)){if ((!any(egonD[k,10] %in% egonD$wave1_id))){egonD[k,10+84] <- 0
k <- k+1}}
for (k in 1:nrow(egonD)){if ((!any(egonD[k,11] %in% egonD$wave1_id))){egonD[k,11+84] <- 0
k <- k+1}}

for (k in 1:nrow(egonD)){if ((!any(egonD[k,12] %in% egonD$wave1_id))){egonD[k,12+84] <- 0
k <- k+1}}
for (k in 1:nrow(egonD)){if ((!any(egonD[k,13] %in% egonD$wave1_id))){egonD[k,13+84] <- 0
k <- k+1}}
for (k in 1:nrow(egonD)){if ((!any(egonD[k,14] %in% egonD$wave1_id))){egonD[k,14+84] <- 0
k <- k+1}}
for (k in 1:nrow(egonD)){if ((!any(egonD[k,15] %in% egonD$wave1_id))){egonD[k,15+84] <- 0
k <- k+1}}
for (k in 1:nrow(egonD)){if ((!any(egonD[k,16] %in% egonD$wave1_id))){egonD[k,16+84] <- 0
k <- k+1}}

for (k in 1:nrow(egonD)){if ((!any(egonD[k,17] %in% egonD$wave1_id))){egonD[k,17+84] <- 0
k <- k+1}}
for (k in 1:nrow(egonD)){if ((!any(egonD[k,18] %in% egonD$wave1_id))){egonD[k,18+84] <- 0
k <- k+1}}
for (k in 1:nrow(egonD)){if ((!any(egonD[k,19] %in% egonD$wave1_id))){egonD[k,19+84] <- 0
k <- k+1}}
for (k in 1:nrow(egonD)){if ((!any(egonD[k,20] %in% egonD$wave1_id))){egonD[k,20+84] <- 0
k <- k+1}}
for (k in 1:nrow(egonD)){if ((!any(egonD[k,21] %in% egonD$wave1_id))){egonD[k,21+84] <- 0
k <- k+1}}

for (k in 1:nrow(egonD)){if ((!any(egonD[k,22] %in% egonD$wave1_id))){egonD[k,22+84] <- 0
k <- k+1}}
for (k in 1:nrow(egonD)){if ((!any(egonD[k,23] %in% egonD$wave1_id))){egonD[k,23+84] <- 0
k <- k+1}}
for (k in 1:nrow(egonD)){if ((!any(egonD[k,24] %in% egonD$wave1_id))){egonD[k,24+84] <- 0
k <- k+1}}
for (k in 1:nrow(egonD)){if ((!any(egonD[k,25] %in% egonD$wave1_id))){egonD[k,25+84] <- 0
k <- k+1}}
for (k in 1:nrow(egonD)){if ((!any(egonD[k,26] %in% egonD$wave1_id))){egonD[k,26+84] <- 0
k <- k+1}}

for (k in 1:nrow(egonD)){if ((!any(egonD[k,27] %in% egonD$wave1_id))){egonD[k,27+84] <- 0
k <- k+1}}
for (k in 1:nrow(egonD)){if ((!any(egonD[k,28] %in% egonD$wave1_id))){egonD[k,28+84] <- 0
k <- k+1}}
for (k in 1:nrow(egonD)){if ((!any(egonD[k,29] %in% egonD$wave1_id))){egonD[k,29+84] <- 0
k <- k+1}}
for (k in 1:nrow(egonD)){if ((!any(egonD[k,30] %in% egonD$wave1_id))){egonD[k,30+84] <- 0
k <- k+1}}
for (k in 1:nrow(egonD)){if ((!any(egonD[k,31] %in% egonD$wave1_id))){egonD[k,31+84] <- 0
k <- k+1}}

# save data in seperate frame before making interaction variable
egon1 <- egonD


############################################################
## make interaction variables
##     multiply Pweights with variables of interests
##     example: does being friends with majority members (binary) or generation to majority (ordinal) increase chances of having a better emotional fit
##     -> make a copy of the weight matrix
##     -> multiply weighs with ('group_status') variable, sum the weighs for all the rows into a new variable
##     -> is this variable significantly related to emotional fit (y)?
###########################################################


## this works but not also for each column automatically
## code below with 'y1_em_fit' is prototype, change to variable you want!!

## prototype
# for (k in 1:nrow(egonD)){
#   for (i in 1:nrow(egonD)){
#     if (egonD$wave1_id[i] == egonD[k,2]){
#       egonD[k,1+31] <- egonD[k,1+31]*egonD$y1_em_fit[i]
#       i <- i+1
#       k <- k+1}}}



#################################################
## group_status (ordinal)
##           groupstatus -> (0) first gen minority (84)
##                          (1) 2nd gen minority (569)
##                          (2) 3th gen minority (10) +  majority (1637)
#################################################

## test first
egonetM2 <- egonD[1:20,]


for (k in 1:nrow(egonetM2)){
  for (i in 1:nrow(egonetM2)){
    if (egonetM2$wave1_id[i] == egonetM2[k,2]){
      egonetM2[k,1+85] <- egonetM2[k,1+85]*egonetM2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
## seems about right

## how large is the difference in emofit scores between pupils of different background?
tapply(egonD$y1_em_fit, egonD$group_status, summary)   
# seems like there is a nice linear ordinal relationship between 1st gen, 2nd gen and majority in mean emofit scores
# so put majority and 3th gen together, there are only 10 3th gen after all...

# # make a binary group status variable 0-1-2:
# egonD$group <- egonD$group_status
# table(egonD$group)
# egonD$group[egonD$group == 4] <- 3
# egonD$group[egonD$group == 1] <- 0
# egonD$group[egonD$group == 2] <- 1
# egonD$group[egonD$group == 3] <- 2
# table(egonD$group)





# add categorical variable to indicate minority or majority
egonD$minority <- egonD$group
egonD$minority[egonD$minority == 1 | egonD$minority == 2 ] <- 1
egonD$minority[egonD$minority == 3 ] <- 0

# add original weights back into data, but on another datafile
egonB2 <- egonD

#egonB2[,86:114] <- egonetB2_5C[,32:61]


write_sav(egonD,'egonD.sav')

#write_sav(egonB2,'egonB2.sav')


##############################
# relation contact - indegree
##############################

itemsindcon <- egonD %>% select(contact,wgroup,y1_majority_proportion_class)
corr <- round(cor(itemsindcon, use = "complete.obs"),2)
ggcorrplot(corr)
corr




############################################################
####### relation between dependent variables
############################################################
itemsindcon <- egonD %>% select(y1_em_fit,contact,wgroup,y1_majority_proportion_class,net_group_indegree,popular,indegree,y1_clsize,group_status)
corr <- round(cor(itemsindcon, use = "complete.obs"),2)
ggcorrplot(corr)
corr

#Take out uncorrelated ones (popular)
itemsindcon <- egonD2 %>% select(y1_em_fit,contact,wgroup,y1_majority_proportion_class,indegree,group_status,
                                 #net_group_indegree
                                 )
corr <- round(cor(itemsindcon, use = "complete.obs"),2)
ggcorrplot(corr)
corr

itemsindcon <- egonD2 %>% select(contact,wgroup,y1_majority_proportion_class,indegree,group_status)
corr <- round(cor(itemsindcon, use = "complete.obs"),2)
ggcorrplot(corr)
corr


items <- egonD2 %>% select(contact,wgroup,y1_majority_proportion_class,indegree,group_status)
model <- '
total =~ contact+group_status+wgroup+indegree+y1_majority_proportion_class
wgroup ~~ indegree
contact ~~ group_status
'

fit <- cfa(model,data=items, estimator = 'MLM')
summary(fit, standardized=T, fit.measures=T) 
semPaths(fit,"model","stand",style="LISREL",rotation=1, 
         edge.color="black",sizeLat=4,layout = 'tree',
         edge.label.cex=0.6,mar=c(4,1,4,1),sizeMan = 4)
mi<-inspect(fit,"mi")
mi.sorted<- mi[order(-mi$mi),]  # sort from high to low
mi.sorted[1:7,]


items <- egonD2 %>% select(y1_em_fit,contact,wgroup,y1_majority_proportion_class,indegree,group_status)
model <- '
total =~ contact+group_status+wgroup+indegree+y1_majority_proportion_class
wgroup ~~ indegree
contact ~~ group_status
y1_em_fit ~ contact+group_status+wgroup+indegree+y1_majority_proportion_class
'

fit <- sem(model,data=items, estimator = 'MLM')
summary(fit, standardized=T, fit.measures=T) 
semPaths(fit,"model","stand",style="LISREL",rotation=1, 
         edge.color="black",sizeLat=4,layout = 'tree',
         edge.label.cex=0.6,mar=c(4,1,4,1),sizeMan = 4)
mi<-inspect(fit,"mi")
mi.sorted<- mi[order(-mi$mi),]  # sort from high to low
mi.sorted[1:7,]


#####################

write_sav(egonD2,'egonD2.sav')


egonC1 = read_sav('egonC1.sav')  # only minority data
egonD = read_sav('egonD.sav')



########################################################
##   select on only minority for analysis in MLwiN
########################################################

# keep the majority members in for the network effects!
# How to deal with this for the analysis in MLwiN  (?)
# I suggest: keep everyone in, but put NA for all the variables of the majority members but the 2*30 network variables (y1_bfs_1 - y1_bfs_30 and y1_bfs_1n - y1_bfs_30n)


egonC1 <- egonD2[egonD2$group != 3,]
egonC2 <- egonD2
for (i in 1:nrow(egonC2)){
  if (egonC2[i,116] == 3){    # group
    egonC2[i,74] <- NA     # y1_em_fit
}}



# by simply removing the majority members
egonC1 <- egonD2[egonD2$group != 3,]
write_sav(egonC1,'egonC1.sav')
write_sav(egonC2,'egonC2.sav')




##################################################
##        check CFA again
##################################################

items <- egonC1 %>% dplyr::select(y1_bgfr1,y1_rescs1,y1_recn1,y1_compn)  # test for when only minorities in the data
corr <- round(cor(items, use = "complete.obs"),1)
ggcorrplot(corr)


model <- '
contact =~ y1_rescs1+y1_bgfr1+y1_recn1+y1_compn
'


fit1 <- cfa(model,data=items, estimator = 'MLM')
fit1 <- cfa(model,data=items, estimator = 'MLF', missing = "ML")
summary(fit1, standardized=T, fit.measures=T) 
semPaths(fit1,"model","stand",style="LISREL",rotation=1, 
         edge.color="black",sizeLat=4,layout = 'tree',
         edge.label.cex=0.6,mar=c(4,1,4,1),sizeMan = 4)
mi<-inspect(fit1,"mi")
mi.sorted<- mi[order(-mi$mi),]  # sort from high to low
mi.sorted[1:7,]



## put the factor scores in the data
idx <- lavInspect(fit1, "case.idx")
fscores <- lavPredict(fit1,method = "regression")
## loop over factors
for (fs in colnames(fscores)) {
  egonC1[idx, fs] <- fscores[ , fs]
}

# check if it is correct
items2 <- egonC1 %>% dplyr::select(y1_bgfr1,y1_rescs1,y1_recn1,y1_compn,contact)
corr <- round(cor(items2, use = "complete.obs"),1)
ggcorrplot(corr) # yes they correlate very well


## check if Yeasle's emotional fit scores are about the same as alba's
items2 <- alldata %>% select(y1_em_fit, y1_em_fit_min, y1_em_fit_1st)
corr <- round(cor(items2, use = "complete.obs"),2)
ggcorrplot(corr) # yes they correlate very well
corr


########################################################################
#
#            peer effect: emotions network
#
########################################################################

# put emotional fit for everyone back in
egonC2$emofit <- egonD$y1_em_fit

# replace the old weight matrix by the first one
egonC2[,86:114]<-egonC2[,32:60]

for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,2]){
      egonC2[k,1+85] <- egonC2[k,1+85]*egonC2$emofit[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 3
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,3]){
      egonC2[k,2+85] <- egonC2[k,2+85]*egonC2$emofit[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 4
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,4]){
      egonC2[k,3+85] <- egonC2[k,3+85]*egonC2$emofit[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 5
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,5]){
      egonC2[k,4+85] <- egonC2[k,4+85]*egonC2$emofit[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 6
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,6]){
      egonC2[k,5+85] <- egonC2[k,5+85]*egonC2$emofit[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 7
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,7]){
      egonC2[k,6+85] <- egonC2[k,6+85]*egonC2$emofit[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 8
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,8]){
      egonC2[k,7+85] <- egonC2[k,7+85]*egonC2$emofit[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 9
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,9]){
      egonC2[k,8+85] <- egonC2[k,8+85]*egonC2$emofit[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 10
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,10]){
      egonC2[k,9+85] <- egonC2[k,9+85]*egonC2$emofit[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 11
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,11]){
      egonC2[k,10+85] <- egonC2[k,10+85]*egonC2$emofit[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 12
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,12]){
      egonC2[k,11+85] <- egonC2[k,11+85]*egonC2$emofit[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 13
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,13]){
      egonC2[k,12+85] <- egonC2[k,12+85]*egonC2$emofit[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 14
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,14]){
      egonC2[k,13+85] <- egonC2[k,13+85]*egonC2$emofit[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 15
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,15]){
      egonC2[k,14+85] <- egonC2[k,14+85]*egonC2$emofit[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 16
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,16]){
      egonC2[k,15+85] <- egonC2[k,15+85]*egonC2$emofit[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 17
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,17]){
      egonC2[k,16+85] <- egonC2[k,16+85]*egonC2$emofit[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 18
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,18]){
      egonC2[k,17+85] <- egonC2[k,17+85]*egonC2$emofit[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 19
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,19]){
      egonC2[k,18+85] <- egonC2[k,18+85]*egonC2$emofit[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 20
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,20]){
      egonC2[k,19+85] <- egonC2[k,19+85]*egonC2$emofit[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 21
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,21]){
      egonC2[k,20+85] <- egonC2[k,20+85]*egonC2$emofit[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 22
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,22]){
      egonC2[k,21+85] <- egonC2[k,21+85]*egonC2$emofit[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 23
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,23]){
      egonC2[k,22+85] <- egonC2[k,22+85]*egonC2$emofit[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 24
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,24]){
      egonC2[k,23+85] <- egonC2[k,23+85]*egonC2$emofit[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 25
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,25]){
      egonC2[k,24+85] <- egonC2[k,24+85]*egonC2$emofit[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 26
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,26]){
      egonC2[k,25+85] <- egonC2[k,25+85]*egonC2$emofit[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 27
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,27]){
      egonC2[k,26+85] <- egonC2[k,26+85]*egonC2$emofit[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 28
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,28]){
      egonC2[k,27+85] <- egonC2[k,27+85]*egonC2$emofit[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 29
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,29]){
      egonC2[k,28+85] <- egonC2[k,28+85]*egonC2$emofit[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 30
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,30]){
      egonC2[k,29+85] <- egonC2[k,29+85]*egonC2$emofit[i]
      i <- i+1
      k <- k+1
    }
  }
}

egonC2$emofitnetwork <- rowSums(egonC2[,86:114])

egonC3 <- egonC2
write_sav(egonC3,'egonC3.sav')



########################################################################
#
#            peer effect: emotions network*majority
#
########################################################################


for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,2]){
      egonC2[k,1+85] <- egonC2[k,1+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 3
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,3]){
      egonC2[k,2+85] <- egonC2[k,2+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 4
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,4]){
      egonC2[k,3+85] <- egonC2[k,3+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 5
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,5]){
      egonC2[k,4+85] <- egonC2[k,4+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 6
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,6]){
      egonC2[k,5+85] <- egonC2[k,5+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 7
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,7]){
      egonC2[k,6+85] <- egonC2[k,6+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 8
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,8]){
      egonC2[k,7+85] <- egonC2[k,7+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 9
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,9]){
      egonC2[k,8+85] <- egonC2[k,8+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 10
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,10]){
      egonC2[k,9+85] <- egonC2[k,9+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 11
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,11]){
      egonC2[k,10+85] <- egonC2[k,10+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 12
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,12]){
      egonC2[k,11+85] <- egonC2[k,11+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 13
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,13]){
      egonC2[k,12+85] <- egonC2[k,12+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 14
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,14]){
      egonC2[k,13+85] <- egonC2[k,13+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 15
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,15]){
      egonC2[k,14+85] <- egonC2[k,14+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 16
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,16]){
      egonC2[k,15+85] <- egonC2[k,15+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 17
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,17]){
      egonC2[k,16+85] <- egonC2[k,16+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 18
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,18]){
      egonC2[k,17+85] <- egonC2[k,17+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 19
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,19]){
      egonC2[k,18+85] <- egonC2[k,18+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 20
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,20]){
      egonC2[k,19+85] <- egonC2[k,19+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 21
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,21]){
      egonC2[k,20+85] <- egonC2[k,20+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 22
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,22]){
      egonC2[k,21+85] <- egonC2[k,21+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 23
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,23]){
      egonC2[k,22+85] <- egonC2[k,22+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 24
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,24]){
      egonC2[k,23+85] <- egonC2[k,23+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 25
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,25]){
      egonC2[k,24+85] <- egonC2[k,24+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 26
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,26]){
      egonC2[k,25+85] <- egonC2[k,25+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 27
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,27]){
      egonC2[k,26+85] <- egonC2[k,26+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 28
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,28]){
      egonC2[k,27+85] <- egonC2[k,27+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 29
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,29]){
      egonC2[k,28+85] <- egonC2[k,28+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 30
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,30]){
      egonC2[k,29+85] <- egonC2[k,29+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}

egonC2$emofitXgroup <- rowSums(egonC2[,86:114])


########################################################################
#
#            peer effect: emotions network*group
#
########################################################################


for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,2]){
      egonC2[k,1+85] <- egonC2[k,1+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 3
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,3]){
      egonC2[k,2+85] <- egonC2[k,2+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 4
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,4]){
      egonC2[k,3+85] <- egonC2[k,3+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 5
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,5]){
      egonC2[k,4+85] <- egonC2[k,4+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 6
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,6]){
      egonC2[k,5+85] <- egonC2[k,5+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 7
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,7]){
      egonC2[k,6+85] <- egonC2[k,6+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 8
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,8]){
      egonC2[k,7+85] <- egonC2[k,7+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 9
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,9]){
      egonC2[k,8+85] <- egonC2[k,8+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 10
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,10]){
      egonC2[k,9+85] <- egonC2[k,9+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 11
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,11]){
      egonC2[k,10+85] <- egonC2[k,10+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 12
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,12]){
      egonC2[k,11+85] <- egonC2[k,11+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 13
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,13]){
      egonC2[k,12+85] <- egonC2[k,12+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 14
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,14]){
      egonC2[k,13+85] <- egonC2[k,13+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 15
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,15]){
      egonC2[k,14+85] <- egonC2[k,14+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 16
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,16]){
      egonC2[k,15+85] <- egonC2[k,15+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 17
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,17]){
      egonC2[k,16+85] <- egonC2[k,16+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 18
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,18]){
      egonC2[k,17+85] <- egonC2[k,17+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 19
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,19]){
      egonC2[k,18+85] <- egonC2[k,18+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 20
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,20]){
      egonC2[k,19+85] <- egonC2[k,19+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 21
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,21]){
      egonC2[k,20+85] <- egonC2[k,20+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 22
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,22]){
      egonC2[k,21+85] <- egonC2[k,21+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 23
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,23]){
      egonC2[k,22+85] <- egonC2[k,22+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 24
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,24]){
      egonC2[k,23+85] <- egonC2[k,23+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 25
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,25]){
      egonC2[k,24+85] <- egonC2[k,24+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 26
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,26]){
      egonC2[k,25+85] <- egonC2[k,25+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 27
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,27]){
      egonC2[k,26+85] <- egonC2[k,26+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 28
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,28]){
      egonC2[k,27+85] <- egonC2[k,27+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 29
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,29]){
      egonC2[k,28+85] <- egonC2[k,28+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 30
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,30]){
      egonC2[k,29+85] <- egonC2[k,29+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}

egonC2$emofitXgroup <- rowSums(egonC2[,86:114])

egonX1 <- egonC2


########################################################################
#
#            peer effect: emotions network*majority
#
########################################################################


for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,2]){
      egonC2[k,1+85] <- egonC2[k,1+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 3
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,3]){
      egonC2[k,2+85] <- egonC2[k,2+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 4
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,4]){
      egonC2[k,3+85] <- egonC2[k,3+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 5
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,5]){
      egonC2[k,4+85] <- egonC2[k,4+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 6
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,6]){
      egonC2[k,5+85] <- egonC2[k,5+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 7
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,7]){
      egonC2[k,6+85] <- egonC2[k,6+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 8
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,8]){
      egonC2[k,7+85] <- egonC2[k,7+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 9
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,9]){
      egonC2[k,8+85] <- egonC2[k,8+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 10
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,10]){
      egonC2[k,9+85] <- egonC2[k,9+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 11
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,11]){
      egonC2[k,10+85] <- egonC2[k,10+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 12
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,12]){
      egonC2[k,11+85] <- egonC2[k,11+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 13
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,13]){
      egonC2[k,12+85] <- egonC2[k,12+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 14
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,14]){
      egonC2[k,13+85] <- egonC2[k,13+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 15
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,15]){
      egonC2[k,14+85] <- egonC2[k,14+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 16
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,16]){
      egonC2[k,15+85] <- egonC2[k,15+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 17
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,17]){
      egonC2[k,16+85] <- egonC2[k,16+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 18
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,18]){
      egonC2[k,17+85] <- egonC2[k,17+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 19
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,19]){
      egonC2[k,18+85] <- egonC2[k,18+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 20
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,20]){
      egonC2[k,19+85] <- egonC2[k,19+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 21
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,21]){
      egonC2[k,20+85] <- egonC2[k,20+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 22
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,22]){
      egonC2[k,21+85] <- egonC2[k,21+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 23
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,23]){
      egonC2[k,22+85] <- egonC2[k,22+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 24
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,24]){
      egonC2[k,23+85] <- egonC2[k,23+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 25
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,25]){
      egonC2[k,24+85] <- egonC2[k,24+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 26
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,26]){
      egonC2[k,25+85] <- egonC2[k,25+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 27
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,27]){
      egonC2[k,26+85] <- egonC2[k,26+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 28
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,28]){
      egonC2[k,27+85] <- egonC2[k,27+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 29
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,29]){
      egonC2[k,28+85] <- egonC2[k,28+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 30
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,30]){
      egonC2[k,29+85] <- egonC2[k,29+85]*egonC2$group[i]
      i <- i+1
      k <- k+1
    }
  }
}

egonC2$emofitXgroup <- rowSums(egonC2[,86:114])


########################################################################
#
#            peer effect minority in majority: wgroup*groupminority
#
########################################################################

egonC2$minorityfriends <- egonC2$group
egonC2$minorityfriends[egonC2$minorityfriends == 3 ] <- 0
egonC2$minorityfriends[egonC2$minorityfriends == 1 ] <- 5    # '1' needs to be 2 but first make sure it doesn't overlap
egonC2$minorityfriends[egonC2$minorityfriends == 2 ] <- 1
egonC2$minorityfriends[egonC2$minorityfriends == 5 ] <- 2

# 1 = second gen
# 2 = first gen
# 0 = majority + (10 pupils) 3th gen

# replace the old weight matrix by the first one
egonC2[,86:114]<-egonC2[,32:60]


for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,2]){
      egonC2[k,1+85] <- egonC2[k,1+85]*egonC2$minorityfriends[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 3
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,3]){
      egonC2[k,2+85] <- egonC2[k,2+85]*egonC2$minorityfriends[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 4
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,4]){
      egonC2[k,3+85] <- egonC2[k,3+85]*egonC2$minorityfriends[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 5
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,5]){
      egonC2[k,4+85] <- egonC2[k,4+85]*egonC2$minorityfriends[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 6
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,6]){
      egonC2[k,5+85] <- egonC2[k,5+85]*egonC2$minorityfriends[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 7
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,7]){
      egonC2[k,6+85] <- egonC2[k,6+85]*egonC2$minorityfriends[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 8
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,8]){
      egonC2[k,7+85] <- egonC2[k,7+85]*egonC2$minorityfriends[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 9
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,9]){
      egonC2[k,8+85] <- egonC2[k,8+85]*egonC2$minorityfriends[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 10
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,10]){
      egonC2[k,9+85] <- egonC2[k,9+85]*egonC2$minorityfriends[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 11
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,11]){
      egonC2[k,10+85] <- egonC2[k,10+85]*egonC2$minorityfriends[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 12
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,12]){
      egonC2[k,11+85] <- egonC2[k,11+85]*egonC2$minorityfriends[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 13
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,13]){
      egonC2[k,12+85] <- egonC2[k,12+85]*egonC2$minorityfriends[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 14
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,14]){
      egonC2[k,13+85] <- egonC2[k,13+85]*egonC2$minorityfriends[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 15
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,15]){
      egonC2[k,14+85] <- egonC2[k,14+85]*egonC2$minorityfriends[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 16
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,16]){
      egonC2[k,15+85] <- egonC2[k,15+85]*egonC2$minorityfriends[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 17
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,17]){
      egonC2[k,16+85] <- egonC2[k,16+85]*egonC2$minorityfriends[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 18
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,18]){
      egonC2[k,17+85] <- egonC2[k,17+85]*egonC2$minorityfriends[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 19
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,19]){
      egonC2[k,18+85] <- egonC2[k,18+85]*egonC2$minorityfriends[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 20
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,20]){
      egonC2[k,19+85] <- egonC2[k,19+85]*egonC2$minorityfriends[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 21
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,21]){
      egonC2[k,20+85] <- egonC2[k,20+85]*egonC2$minorityfriends[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 22
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,22]){
      egonC2[k,21+85] <- egonC2[k,21+85]*egonC2$minorityfriends[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 23
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,23]){
      egonC2[k,22+85] <- egonC2[k,22+85]*egonC2$minorityfriends[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 24
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,24]){
      egonC2[k,23+85] <- egonC2[k,23+85]*egonC2$minorityfriends[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 25
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,25]){
      egonC2[k,24+85] <- egonC2[k,24+85]*egonC2$minorityfriends[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 26
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,26]){
      egonC2[k,25+85] <- egonC2[k,25+85]*egonC2$minorityfriends[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 27
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,27]){
      egonC2[k,26+85] <- egonC2[k,26+85]*egonC2$minorityfriends[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 28
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,28]){
      egonC2[k,27+85] <- egonC2[k,27+85]*egonC2$minorityfriends[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 29
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,29]){
      egonC2[k,28+85] <- egonC2[k,28+85]*egonC2$minorityfriends[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 30
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,30]){
      egonC2[k,29+85] <- egonC2[k,29+85]*egonC2$minorityfriends[i]
      i <- i+1
      k <- k+1
    }
  }
}



egonC2$wgroupMAJORITY_min <- rowSums(egonC2[,86:114])

write_sav(egonC2,'egonC2.sav')




########################################################################
#
#            peer effect minority in majority: wgroup*groupminority01 
#
########################################################################


egonC2$wave1_idEXTRA <- egonC2$wave1_id
egonC2$minorityfriends01 <- egonC2$minorityfriends
egonC2$minorityfriends01[egonC2$minorityfriends01 == 2 ] <- 1


# 1 = second gen &  first gen
# 0 = majority + (10 pupils) 3th gen

# replace the old weight matrix by the first one
egonC2[,86:114]<-egonC2[,32:60]


for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,2]){
      egonC2[k,1+85] <- egonC2[k,1+85]*egonC2$minorityfriends01[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 3
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,3]){
      egonC2[k,2+85] <- egonC2[k,2+85]*egonC2$minorityfriends01[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 4
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,4]){
      egonC2[k,3+85] <- egonC2[k,3+85]*egonC2$minorityfriends01[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 5
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,5]){
      egonC2[k,4+85] <- egonC2[k,4+85]*egonC2$minorityfriends01[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 6
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,6]){
      egonC2[k,5+85] <- egonC2[k,5+85]*egonC2$minorityfriends01[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 7
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,7]){
      egonC2[k,6+85] <- egonC2[k,6+85]*egonC2$minorityfriends01[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 8
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,8]){
      egonC2[k,7+85] <- egonC2[k,7+85]*egonC2$minorityfriends01[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 9
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,9]){
      egonC2[k,8+85] <- egonC2[k,8+85]*egonC2$minorityfriends01[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 10
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,10]){
      egonC2[k,9+85] <- egonC2[k,9+85]*egonC2$minorityfriends01[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 11
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,11]){
      egonC2[k,10+85] <- egonC2[k,10+85]*egonC2$minorityfriends01[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 12
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,12]){
      egonC2[k,11+85] <- egonC2[k,11+85]*egonC2$minorityfriends01[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 13
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,13]){
      egonC2[k,12+85] <- egonC2[k,12+85]*egonC2$minorityfriends01[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 14
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,14]){
      egonC2[k,13+85] <- egonC2[k,13+85]*egonC2$minorityfriends01[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 15
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,15]){
      egonC2[k,14+85] <- egonC2[k,14+85]*egonC2$minorityfriends01[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 16
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,16]){
      egonC2[k,15+85] <- egonC2[k,15+85]*egonC2$minorityfriends01[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 17
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,17]){
      egonC2[k,16+85] <- egonC2[k,16+85]*egonC2$minorityfriends01[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 18
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,18]){
      egonC2[k,17+85] <- egonC2[k,17+85]*egonC2$minorityfriends01[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 19
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,19]){
      egonC2[k,18+85] <- egonC2[k,18+85]*egonC2$minorityfriends01[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 20
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,20]){
      egonC2[k,19+85] <- egonC2[k,19+85]*egonC2$minorityfriends01[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 21
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,21]){
      egonC2[k,20+85] <- egonC2[k,20+85]*egonC2$minorityfriends01[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 22
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,22]){
      egonC2[k,21+85] <- egonC2[k,21+85]*egonC2$minorityfriends01[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 23
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,23]){
      egonC2[k,22+85] <- egonC2[k,22+85]*egonC2$minorityfriends01[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 24
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,24]){
      egonC2[k,23+85] <- egonC2[k,23+85]*egonC2$minorityfriends01[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 25
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,25]){
      egonC2[k,24+85] <- egonC2[k,24+85]*egonC2$minorityfriends01[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 26
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,26]){
      egonC2[k,25+85] <- egonC2[k,25+85]*egonC2$minorityfriends01[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 27
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,27]){
      egonC2[k,26+85] <- egonC2[k,26+85]*egonC2$minorityfriends01[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 28
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,28]){
      egonC2[k,27+85] <- egonC2[k,27+85]*egonC2$minorityfriends01[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 29
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,29]){
      egonC2[k,28+85] <- egonC2[k,28+85]*egonC2$minorityfriends01[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 30
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,30]){
      egonC2[k,29+85] <- egonC2[k,29+85]*egonC2$minorityfriends01[i]
      i <- i+1
      k <- k+1
    }
  }
}



egonC2$wgroupMAJORITY_min01 <- rowSums(egonC2[,86:114])

write_sav(egonC2,'egonC2.sav')



####################################
##
##     binary majority members
##
#######################################


# replace the old weight matrix by the first one
egonC2[,86:114]<-egonC2[,32:60]
# make a binary group status variable 0-1:
egonC2$group01 <- egonC2$group_status
table(egonC2$group01)

egonC2$group01[egonC2$group01 == 1] <- 0
egonC2$group01[egonC2$group01 == 2] <- 0
egonC2$group01[egonC2$group01 == 3] <- 1
egonC2$group01[egonC2$group01 == 4] <- 1
table(egonC2$group01)



############## this code may take an hour to an hour and a half to run
# column 2
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,2]){
      egonC2[k,1+85] <- egonC2[k,1+85]*egonC2$group01[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 3
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,3]){
      egonC2[k,2+85] <- egonC2[k,2+85]*egonC2$group01[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 4
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,4]){
      egonC2[k,3+85] <- egonC2[k,3+85]*egonC2$group01[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 5
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,5]){
      egonC2[k,4+85] <- egonC2[k,4+85]*egonC2$group01[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 6
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,6]){
      egonC2[k,5+85] <- egonC2[k,5+85]*egonC2$group01[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 7
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,7]){
      egonC2[k,6+85] <- egonC2[k,6+85]*egonC2$group01[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 8
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,8]){
      egonC2[k,7+85] <- egonC2[k,7+85]*egonC2$group01[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 9
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,9]){
      egonC2[k,8+85] <- egonC2[k,8+85]*egonC2$group01[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 10
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,10]){
      egonC2[k,9+85] <- egonC2[k,9+85]*egonC2$group01[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 11
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,11]){
      egonC2[k,10+85] <- egonC2[k,10+85]*egonC2$group01[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 12
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,12]){
      egonC2[k,11+85] <- egonC2[k,11+85]*egonC2$group01[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 13
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,13]){
      egonC2[k,12+85] <- egonC2[k,12+85]*egonC2$group01[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 14
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,14]){
      egonC2[k,13+85] <- egonC2[k,13+85]*egonC2$group01[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 15
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,15]){
      egonC2[k,14+85] <- egonC2[k,14+85]*egonC2$group01[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 16
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,16]){
      egonC2[k,15+85] <- egonC2[k,15+85]*egonC2$group01[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 17
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,17]){
      egonC2[k,16+85] <- egonC2[k,16+85]*egonC2$group01[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 18
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,18]){
      egonC2[k,17+85] <- egonC2[k,17+85]*egonC2$group01[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 19
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,19]){
      egonC2[k,18+85] <- egonC2[k,18+85]*egonC2$group01[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 20
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,20]){
      egonC2[k,19+85] <- egonC2[k,19+85]*egonC2$group01[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 21
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,21]){
      egonC2[k,20+85] <- egonC2[k,20+85]*egonC2$group01[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 22
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,22]){
      egonC2[k,21+85] <- egonC2[k,21+85]*egonC2$group01[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 23
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,23]){
      egonC2[k,22+85] <- egonC2[k,22+85]*egonC2$group01[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 24
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,24]){
      egonC2[k,23+85] <- egonC2[k,23+85]*egonC2$group01[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 25
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,25]){
      egonC2[k,24+85] <- egonC2[k,24+85]*egonC2$group01[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 26
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,26]){
      egonC2[k,25+85] <- egonC2[k,25+85]*egonC2$group01[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 27
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,27]){
      egonC2[k,26+85] <- egonC2[k,26+85]*egonC2$group01[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 28
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,28]){
      egonC2[k,27+85] <- egonC2[k,27+85]*egonC2$group01[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 29
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,29]){
      egonC2[k,28+85] <- egonC2[k,28+85]*egonC2$group01[i]
      i <- i+1
      k <- k+1
    }
  }
}
# column 30
for (k in 1:nrow(egonC2)){
  for (i in 1:nrow(egonC2)){
    if (egonC2$wave1_id[i] == egonC2[k,30]){
      egonC2[k,29+85] <- egonC2[k,29+85]*egonC2$group01[i]
      i <- i+1
      k <- k+1
    }
  }
}

## now sum the rows of the weight matrix
egonC2$wgroup01 <- rowSums(egonC2[,86:114])







##################################################
##              minority contact
##################################################




datan <- alldata %>% select(wave1_id,
                           # contact with majority
                           y1_bgfr1:y1_bgfr6,  # M13_1:How many of your friends have a Belgian backround
                           y1_rescs1:y1_rescs6,  # M14_1: How often do you spend time during your school break with students with a Belgian background
                           y1_recn1:y1_recn6,  # M15_1: How often do you spend time in the neighbourhood where you live with stud
                           y1_compn,   # M16: How many of the people in your neighbourhood have a Belgian backround

                           group_status #group_status,    # 1 = 1st gen   3 = 3th gen  4 = majority
)

egonA<-datan

# make sure you only calculate for majority members (and 3th gen)
for (i in 1:nrow(egonA)){
  if (egonA$group_status[i] == 1 | egonA$group_status[i] == 2){
    egonA[i,2] <- NA
    egonA[i,3] <- NA
    egonA[i,4] <- NA
    egonA[i,5] <- NA
    egonA[i,6] <- NA
    egonA[i,7] <- NA
    egonA[i,8] <- NA
    egonA[i,9] <- NA
    egonA[i,10] <- NA
    egonA[i,11] <- NA
    egonA[i,12] <- NA
    egonA[i,13] <- NA
    egonA[i,14] <- NA
    egonA[i,15] <- NA
    egonA[i,16] <- NA
    egonA[i,17] <- NA
    egonA[i,18] <- NA
    egonA[i,19] <- NA
}}


egonE1 <- merge(egonA,egonC2,by="wave1_id")



items <- egonE1 %>% select(y1_bgfr2:y1_bgfr6,y1_rescs2:y1_rescs6,y1_recn2:y1_recn6,)  # test for when only minorities in the data
corr <- round(cor(items, use = "complete.obs"),1)
ggcorrplot(corr)

data <- items
data <- data[complete.cases(data), ]
cronbach.alpha(data, CI=TRUE)
alpha(data)



model <- '
minoritycontact =~ Italian + Turkish + Morroccan + Polish + Other
Italian =~ y1_rescs2+y1_bgfr2+y1_recn2
Turkish =~ y1_rescs3+y1_bgfr3+y1_recn3
Morroccan =~ y1_rescs4+y1_bgfr4+y1_recn4
Polish =~ y1_rescs5+y1_bgfr5+y1_recn5
Other =~ y1_rescs6+y1_bgfr6+y1_recn6
       
'


#fit <- cfa(model,data=items,  missing = "ML")
fit <- cfa(model,data=items, estimator = 'MLM')
summary(fit, standardized=T, fit.measures=T) 
semPaths(fit,"model","stand",style="LISREL",rotation=1, 
         edge.color="black",sizeLat=4,layout = 'tree',
         edge.label.cex=0.6,mar=c(4,1,4,1),sizeMan = 4)
mi<-inspect(fit,"mi")
mi.sorted<- mi[order(-mi$mi),]  # sort from high to low
mi.sorted[1:15,]

model <- '
minoritycontact =~ Italian + Polish + Turkish + Morroccan + Other
Italian =~ y1_rescs2+y1_bgfr2+y1_recn2
Turkish =~ y1_rescs3+y1_bgfr3+y1_recn3
Morroccan =~ y1_rescs4+y1_bgfr4+y1_recn4
Polish =~ y1_rescs5+y1_bgfr5+y1_recn5
Other =~ y1_rescs6+y1_bgfr6+y1_recn6

#Turkish ~~ Morroccan  
y1_rescs3 ~~ y1_rescs4
y1_recn3 ~~  y1_recn4
y1_bgfr3 ~~ y1_bgfr4

# italian - polish association
y1_bgfr2 ~~ y1_bgfr5
y1_recn2 ~~ y1_recn5
'


# fit1 <- cfa(model,data=items,  missing = "ML")
fit1 <- cfa(model,data=items, estimator = 'MLM')
summary(fit1, standardized=T, fit.measures=T) 
semPaths(fit1,"model","stand",style="LISREL",rotation=1, 
         edge.color="black",sizeLat=4,layout = 'tree',
         edge.label.cex=0.6,mar=c(4,1,4,1),sizeMan = 4)
mi<-inspect(fit1,"mi")
mi.sorted<- mi[order(-mi$mi),]  # sort from high to low
mi.sorted[1:15,]


model <- '
minoritycontact =~ Italian + Polish + Turkish + Morroccan
Italian =~ y1_rescs2+y1_bgfr2+y1_recn2
Turkish =~ y1_rescs3+y1_bgfr3+y1_recn3
Morroccan =~ y1_rescs4+y1_bgfr4+y1_recn4
Polish =~ y1_rescs5+y1_bgfr5+y1_recn5

#Turkish ~~ Morroccan  
y1_rescs3 ~~ y1_rescs4
y1_recn3 ~~  y1_recn4
y1_bgfr3 ~~ y1_bgfr4

# italian - polish association
#Italian ~~ Polish
y1_bgfr2 ~~ y1_bgfr5
y1_recn2 ~~ y1_recn5
y1_rescs2 ~~ y1_rescs5
'


fit1 <- cfa(model,data=items, estimator = 'MLM')
# fit1 <- cfa(model,data=items, missing = "ML")
summary(fit1, standardized=T, fit.measures=T) 
semPaths(fit1,"model","stand",style="LISREL",rotation=1, 
         edge.color="black",sizeLat=4,layout = 'tree',
         edge.label.cex=0.6,mar=c(4,1,4,1),sizeMan = 4)
mi<-inspect(fit1,"mi")
mi.sorted<- mi[order(-mi$mi),]  # sort from high to low
mi.sorted[1:15,]


# ## put the factor scores in the data
# idx <- lavInspect(fit1, "case.idx")
# fscores <- lavPredict(fit1,method = "regression")
# ## loop over factors
# for (fs in colnames(fscores)) {
#   egonE1[idx, fs] <- fscores[ , fs]
# }


## with the missigness data
model <- '
A_minoritycontact =~ A_Italian + A_Polish + A_Turkish + A_Morroccan
A_Italian =~ y1_rescs2+y1_bgfr2+y1_recn2
A_Turkish =~ y1_rescs3+y1_bgfr3+y1_recn3
A_Morroccan =~ y1_rescs4+y1_bgfr4+y1_recn4
A_Polish =~ y1_rescs5+y1_bgfr5+y1_recn5

#Turkish ~~ Morroccan  
y1_rescs3 ~~ y1_rescs4
y1_recn3 ~~  y1_recn4
y1_bgfr3 ~~ y1_bgfr4

# italian - polish association
#Italian ~~ Polish
y1_bgfr2 ~~ y1_bgfr5
y1_recn2 ~~ y1_recn5
y1_rescs2 ~~ y1_rescs5
'


fit1 <- cfa(model,data=items, estimator = 'MLM')
fit1 <- cfa(model,data=items, estimator = 'MLF', missing = "ML")
# fit1 <- cfa(model,data=items, missing = "ML")
summary(fit1, standardized=T, fit.measures=T) 
semPaths(fit1,"model","stand",style="LISREL",rotation=1, 
         edge.color="black",sizeLat=4,layout = 'tree',
         edge.label.cex=0.6,mar=c(4,1,4,1),sizeMan = 4)
mi<-inspect(fit1,"mi")
mi.sorted<- mi[order(-mi$mi),]  # sort from high to low
mi.sorted[1:15,]

## with the missigness data + turkish morroccan only
model <- '
#A_minoritycontact =~ A_Turkish + A_Morroccan
B_Turkish =~ y1_rescs3+y1_bgfr3+y1_recn3
B_Morroccan =~ y1_rescs4+y1_bgfr4+y1_recn4
#A_Morroccan ~~ A_Turkish

#Turkish ~~ Morroccan  
y1_rescs3 ~~ y1_rescs4
y1_recn3 ~~  y1_recn4
y1_bgfr3 ~~ y1_bgfr4

'


fit1 <- cfa(model,data=items, estimator = 'MLM')
fit1 <- cfa(model,data=items, estimator = 'MLF', missing = "ML") # warning: "some cases are empty..." -> this can be ignored.
# fit1 <- cfa(model,data=items, missing = "ML")
summary(fit1, standardized=T, fit.measures=T) 
semPaths(fit1,"model","stand",style="LISREL",rotation=1, 
         edge.color="black",sizeLat=4,layout = 'tree',
         edge.label.cex=0.6,mar=c(4,1,4,1),sizeMan = 4)
mi<-inspect(fit1,"mi")
mi.sorted<- mi[order(-mi$mi),]  # sort from high to low
mi.sorted[1:15,]

## put the factor scores in the data
idx <- lavInspect(fit1, "case.idx")
fscores <- lavPredict(fit1,method = "regression")
## loop over factors
for (fs in colnames(fscores)) {
  egonE1[idx, fs] <- fscores[ , fs]
}

# take average of Turkish and Moroccan
items4 <- egonE1 %>% dplyr::select(B_Morroccan,B_Turkish)
egonE1$MinorityContact <- rowMeans(items4, na.rm=T)

# check if it is correct
items2 <- egonE1 %>% dplyr::select(y1_bgfr2:y1_bgfr5,y1_rescs2:y1_rescs5,y1_recn2:y1_recn5,minoritycontact)
items2 <- egonE1 %>% dplyr::select(y1_bgfr3:y1_bgfr4,y1_rescs3:y1_rescs4,y1_recn3:y1_recn4,B_Morroccan,B_Turkish,MinorityContact)
corr <- round(cor(items2, use = "complete.obs"),2)
ggcorrplot(corr) # yes they correlate very well
corr


egonE1$meanMinorityContact <- rowMeans(items, na.rm=T)  # cronbach's alpha version = just average everything
write_sav(egonE1,'egonE1.sav')

egonE2 <- egonE1
# egonE2 <- egonE1[!egonE1$group == 1,]
# egonE2 <- egonE1[!egonE1$group == 2,]
write_sav(egonE2,'egonE2.sav')

items3 <- egonE2 %>% dplyr::select(A_minoritycontact,wgroupMAJORITY_min)
corr <- round(cor(items3, use = "complete.obs"),2)
ggcorrplot(corr)
corr


##########################################
#
#           contact majority (again)
#
############################################

datan <- alldata %>% dplyr::select(wave1_id,
                            # contact with majority
                            y1_bgfr1, # M13_1:How many of your friends have a Belgian backround
                            y1_rescs1,  # M14_1: How often do you spend time during your school break with students with a Belgian background
                            y1_recn1, # M15_1: How often do you spend time in the neighbourhood where you live with stud
                            y1_compn,   # M16: How many of the people in your neighbourhood have a Belgian backround
                            
                            group_status #group_status,    # 1 = 1st gen   3 = 3th gen  4 = majority
)

egonA<-datan

# make sure you only calculate for majority members (and 3th gen)
for (i in 1:nrow(egonA)){
  if (egonA$group_status[i] == 3){
    egonA[i,2] <- NA
    egonA[i,3] <- NA
    egonA[i,4] <- NA
    egonA[i,5] <- NA
  }}


egonF1 <- merge(egonA,egonE2,by="wave1_id")





items <- egonF1 %>% dplyr::select(y1_bgfr1,y1_rescs1,y1_recn1,y1_compn)
corr <- round(cor(items, use = "complete.obs"),2)
ggcorrplot(corr)


model <- '
contactmajority =~ y1_rescs1+y1_bgfr1+y1_recn1+y1_compn
'

fit <- cfa(model,data=items, estimator = 'MLF', missing = "ML")
summary(fit, standardized=T, fit.measures=T) 
semPaths(fit,"model","stand",style="LISREL",rotation=1, 
         edge.color="black",sizeLat=4,layout = 'tree',
         edge.label.cex=0.6,mar=c(4,1,4,1),sizeMan = 4)
mi<-inspect(fit,"mi")
mi.sorted<- mi[order(-mi$mi),]  # sort from high to low
mi.sorted[1:7,]



## put the factor scores in the data
idx <- lavInspect(fit, "case.idx")
fscores <- lavPredict(fit,method = "regression")
## loop over factors
for (fs in colnames(fscores)) {
  egonF1[idx, fs] <- fscores[ , fs]
}
summary(egonE2$contactmajority)


egonF1 <- egonE2
egonF1 <- egonE2[!egonE1$group == 1,]
egonF1 <- egonE2[!egonE1$group == 2,]
write_sav(egonF1,'egonF1.sav')


egonE2$wgroup01 <- egonC2$wgroup01
egonE2$wgroup01 <- egonC1$contact
write_sav(egonE2,'egonE2.sav')

#without majority
egonF1 <- egonE2
egonF1 <- egonE2[!egonE2$group == 3,]
write_sav(egonF1,'egonF1.sav')

egonE1n <- egonE1[!egonE1$group == 3,]
write_sav(egonE1n,'egonE1n.sav')


items <- egonE2 %>% dplyr::select(emofitnetwork,wgroupMAJORITY_min,wgroup01)
corr <- round(cor(items, use = "complete.obs"),2)
ggcorrplot(corr)
corr



schoolID <- alldata %>% dplyr::select(wave1_id,schno_revised)

school <- merge(egonF1,schoolID,by="wave1_id")
write_sav(school,'school.sav')

schoolID<-school
schoolID <- schoolID[!schoolID$group == 1,]
schoolID <- schoolID[!schoolID$group == 2,]
write_sav(schoolID,'ALL.sav')


dagt <- data.frame(table(egonE2$class))
dagt
length(dagt$Freq)



##### in preperation of my lab presentation 3-05-2022


presentation <- egonC2 %>% dplyr::select(wave1_id, y1_bfs_1:y1_bfs_5,y1_bfs_29,y1_bfs_1n:y1_bfs_5n,y1_bfs_29n, group01,emofit)
presentation  <- rename(presentation, classmate1 = y1_bfs_1)
presentation  <- rename(presentation, classmate2 = y1_bfs_2)
presentation  <- rename(presentation, classmate3 = y1_bfs_3)
presentation  <- rename(presentation, classmate4 = y1_bfs_4)
presentation  <- rename(presentation, classmate5 = y1_bfs_5)
presentation  <- rename(presentation, classmate29 = y1_bfs_29)

presentation  <- rename(presentation, Wclassmate1 = y1_bfs_1n)
presentation  <- rename(presentation, wclassmate2 = y1_bfs_2n)
presentation  <- rename(presentation, wclassmate3 = y1_bfs_3n)
presentation  <- rename(presentation, wclassmate4 = y1_bfs_4n)
presentation  <- rename(presentation, wclassmate5 = y1_bfs_5n)
presentation  <- rename(presentation, wclassmate29 = y1_bfs_29n)

presentation  <- rename(presentation, majoritymember = group01)

