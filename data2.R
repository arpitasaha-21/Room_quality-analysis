setwd("~/ds_project/datsets")
data_2 <- read.csv("~/ds_project/datsets/Dataset_2.csv")
print(data_2)

num_of_missingValue =colSums(is.na(data_2))
num_of_missingValue

missingRoom = which(is.na(data_2$Rooms))
missingRoom
missingType = which(is.na(data_2$Type))
missingType
missingPrice= which(is.na(data_2$Price))
missingPrice

data_2$Price[c(18)] <- NA


get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

frequnt_type = get_mode(data_2$Type)
frequnt_type

data_2$Type[17] <- frequnt_type
data_2


data_2$Type <- factor(data_2$Type,levels = c("h","m","l"), labels = c(1,2,3))
data_2

remove_val <-na.omit(data_2)
remove_val

rm_mean =mean(data_2$Rooms,na.rm = T)
recover_missingRm_mean=data_2$Rooms[is.na(data_2$Rooms)] <-rm_mean
recover_missingRm_mean

Pr_mean =mean(data_2$Price,na.rm = T)
recover_missingPr_mean=data_2$Price[is.na(data_2$Price)] <-Pr_mean
recover_missingPr_mean
data_2


rm_median =median(data_2$Rooms,na.rm = T)
recover_missingRm_median=data_2$Rooms[is.na(data_2$Rooms)] <-rm_median
recover_missingRm_median

pr_median =median(data_2$Price,na.rm = T)
recover_missingPr_median=data_2$Price[is.na(data_2$Price)] <-pr_median
recover_missingPr_median
data_2


rm_mode =get_mode(data_2$Rooms)
recover_missingRm_mode=data_2$Rooms[is.na(data_2$Rooms)] <-rm_mode
recover_missingRm_mode

get_mode2 <- function(x) {
  ux <- na.omit(unique(x) )
  ux[which.max(tabulate(match(x, ux)))]
  
}
pr_mode =get_mode2(data_2$Price)
recover_missingPr_mode=data_2$Price[is.na(data_2$Price)] <-pr_mode
recover_missingPr_mode
data_2

