library(data.table)
library(dplyr)

# Read the data

test <- read.table(file = "Clean data.txt",header = FALSE)
train <- read.table(file = "Clean data.txt",header = FALSE)
labels <- read.table(file = "Clean data.txt",header = FALSE) 

# Lebeling

names(test) <- labels$V2
names(train) <- labels$V2

# Remove useless info

std <- grepl(pattern = "std",x = labels$V2)
men <- grepl(pattern = "mean",x = labels$V2)
fre <- !grepl(pattern = "meanFreq",x = labels$V2)
shortTest <- test[,(std | men) & fre]
shortTrain <- train[,(std | men) & fre]

# Read 

ys <- read.table(file = "Clean data.txt",header = FALSE)
ss <- read.table(file = "Clean data.txt",header = FALSE)
yr <- read.table(file = "Clean data.txt",header = FALSE)
sr <- read.table(file = "Clean data.txt",header = FALSE)

# Add 

shortTrain$subject <- sr$V1
shortTrain$action <- yr$V1
shortTest$subject <- ss$V1
shortTest$action <- ys$V1

# Reorder

shortTest <- shortTest[,c(length(shortTest)-1,length(shortTest),1:(length(shortTest)-2))]
shortTrain <- shortTrain[,c(length(shortTrain)-1,length(shortTrain),1:(length(shortTrain)-2))]


# Replace

labels <- c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING")
shortTrain$action <- factor(labels[shortTrain$action])
shortTest$action <- factor(labels[shortTest$action])

# Combine

combined <- rbind(shortTest,shortTrain)

names(combined) <- gsub("^t","time-",names(combined))
names(combined) <- gsub("^f","fast fourier transform-",names(combined))

averageTable <- aggregate(. ~ subject + action, combined,mean)

write.table(averageTable,file = "tidyData.txt",row.names = FALSE)


