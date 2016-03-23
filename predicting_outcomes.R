setwd("~/Google Drive/Grad School/Programming Practice/Shelter Outcomes")

library(plyr)
library(randomForest)

train <- read.csv("train")
test <- read.csv("test")
output <- read.csv("sample_submission")

head(train)
head(test)

summary(train)
summary(test)

#########################################
## FEATURE CREATION/CLEANUP: TRAIN SET ##
#########################################

train$is_named <- "No"
train$is_named[train$Name != ""] <- "Yes"
train$is_named <- as.factor(train$is_named)

age <- strsplit(as.character(train$AgeuponOutcome), " ")
age <- ldply(age, rbind)
names(age) <- c("num", "size")
age$num <- as.numeric(as.character(age$num))
age$size <- as.character(age$size)
for(i in 1:dim(age)[1]){
        if(is.na(age$num[i])){
                next
        } else {
                age$yearsOld[i] <- if(age$size[i] == "day" | age$size[i] == "days") {
                        age$num[i] / 365
                } else if(age$size[i] == "week" | age$size[i] == "weeks") {
                        (age$num[i] * 7) / 365
                } else if(age$size[i] == "month" | age$size[i] == "months") {
                        age$num[i] / 12
                } else {
                        age$num[i]
                } 
        }
}
age$yearsOld[is.na(age$num)] <- NA
train$yearsOld <- age$yearsOld

color <- strsplit(as.character(train$Color), "/")
color <- ldply(color, rbind)
names(color) <- c("color1", "color2")

color$color1 <- as.character(color$color1)
color$color1[color$color1 == "Silver Lynx Point"] <- "Silver Point"

color1 <- strsplit(color$color1, " ")
color1 <- ldply(color1, rbind)
names(color1) <- c("color1a", "color1b")
color1$color1b <- as.character(color1$color1b)
color1$color1b[is.na(color1$color1b)] <- "None"
color1$color1b <- as.factor(color1$color1b)

color$color2 <- as.character(color$color2)
color$color2[is.na(color$color2)] <- "None"

color2 <- strsplit(color$color2, " ")
color2 <- ldply(color2, rbind)
names(color2) <- c("color2a", "color2b")
color2$color2b <- as.character(color2$color2b)
color2$color2b[is.na(color2$color2b)] <- "None"
color2$color2b <- as.factor(color2$color2b)

color <- cbind(color1, color2)

train <- cbind(train,color)

train$sexKnown <- "Yes"
train$sexKnown[train$SexuponOutcome == "Unknown" | train$SexuponOutcome == ""] <- "No"
train$sexKnown <- as.factor(train$sexKnown)

sex_fixed <- strsplit(as.character(train$SexuponOutcome), " ")
sex_fixed <- ldply(sex_fixed, rbind)
head(sex_fixed)
sex_fixed$fixed <- "No"
sex_fixed$fixed[sex_fixed$`1` == "Neutered" | sex_fixed$`1` == "Spayed"] <- "Yes"
sex_fixed$fixed[is.na(sex_fixed$`2`)] <- "Unknown"
sex_fixed$`1` <- NULL
names(sex_fixed) <- c("sex", "fixed")
sex_fixed$fixed <- as.factor(sex_fixed$fixed)
train <- cbind(train, sex_fixed)
train$sex <- as.character(train$sex)
train$sex[is.na(train$sex)] <- "Unknown"
train$sex <- as.factor(train$sex)

train$DateTime <- as.Date(strftime(as.character(train$DateTime), "%Y-%m-%d %H:%M:%S"))
train$yearOutcome <- as.numeric(format(train$DateTime, "%Y"))
train$monthOutcome <- as.factor(months(train$DateTime))
train$weekdayOutcome <- as.factor(weekdays(train$DateTime))
train$dayOutcome <- as.numeric(format(train$DateTime, "%m"))
train$hourOutcome <- as.numeric(format(train$DateTime, "%H"))

train$purebreed <- "Yes"
train$purebreed[grep("/", as.character(train$Breed))] <- "No"
train$purebreed[grep("Mix", as.character(train$Breed))] <- "No"
train$purebreed <- as.factor(train$purebreed)

train$newOutcome <- as.factor(paste(train$OutcomeType, train$OutcomeSubtype, sep = "-"))

train <- train[, -c(2, 3, 7, 8, 9, 10)]

########################################
## FEATURE CREATION/CLEANUP: TEST SET ##
########################################

test$is_named <- "No"
test$is_named[test$Name != ""] <- "Yes"
test$is_named <- as.factor(test$is_named)

age <- strsplit(as.character(test$AgeuponOutcome), " ")
age <- ldply(age, rbind)
names(age) <- c("num", "size")
age$num <- as.numeric(as.character(age$num))
age$size <- as.character(age$size)
for(i in 1:dim(age)[1]){
        if(is.na(age$num[i])){
                next
        } else {
                age$yearsOld[i] <- if(age$size[i] == "day" | age$size[i] == "days") {
                        age$num[i] / 365
                } else if(age$size[i] == "week" | age$size[i] == "weeks") {
                        (age$num[i] * 7) / 365
                } else if(age$size[i] == "month" | age$size[i] == "months") {
                        age$num[i] / 12
                } else {
                        age$num[i]
                } 
        }
}
age$yearsOld[is.na(age$num)] <- NA
test$yearsOld <- age$yearsOld

color <- strsplit(as.character(test$Color), "/")
color <- ldply(color, rbind)
names(color) <- c("color1", "color2")

color$color1 <- as.character(color$color1)
color$color1[color$color1 == "Silver Lynx Point"] <- "Silver Point"

color1 <- strsplit(color$color1, " ")
color1 <- ldply(color1, rbind)
names(color1) <- c("color1a", "color1b")
color1$color1b <- as.character(color1$color1b)
color1$color1b[is.na(color1$color1b)] <- "None"
color1$color1b <- as.factor(color1$color1b)

color$color2 <- as.character(color$color2)
color$color2[is.na(color$color2)] <- "None"

color2 <- strsplit(color$color2, " ")
color2 <- ldply(color2, rbind)
names(color2) <- c("color2a", "color2b")
color2$color2b <- as.character(color2$color2b)
color2$color2b[is.na(color2$color2b)] <- "None"
color2$color2b <- as.factor(color2$color2b)

color <- cbind(color1, color2)

test <- cbind(test,color)

test$sexKnown <- "Yes"
test$sexKnown[test$SexuponOutcome == "Unknown" | test$SexuponOutcome == ""] <- "No"
test$sexKnown <- as.factor(test$sexKnown)

sex_fixed <- strsplit(as.character(test$SexuponOutcome), " ")
sex_fixed <- ldply(sex_fixed, rbind)
head(sex_fixed)
sex_fixed$fixed <- "No"
sex_fixed$fixed[sex_fixed$`1` == "Neutered" | sex_fixed$`1` == "Spayed"] <- "Yes"
sex_fixed$fixed[is.na(sex_fixed$`2`)] <- "Unknown"
sex_fixed$`1` <- NULL
names(sex_fixed) <- c("sex", "fixed")
sex_fixed$fixed <- as.factor(sex_fixed$fixed)
test <- cbind(test, sex_fixed)
test$sex <- as.character(test$sex)
test$sex[is.na(test$sex)] <- "Unknown"
test$sex <- as.factor(test$sex)

test$DateTime <- as.Date(strftime(as.character(test$DateTime), "%Y-%m-%d %H:%M:%S"))
test$yearOutcome <- as.numeric(format(test$DateTime, "%Y"))
test$monthOutcome <- as.factor(months(test$DateTime))
test$weekdayOutcome <- as.factor(weekdays(test$DateTime))
test$dayOutcome <- as.numeric(format(test$DateTime, "%m"))
test$hourOutcome <- as.numeric(format(test$DateTime, "%H"))

test$purebreed <- "Yes"
test$purebreed[grep("/", as.character(test$Breed))] <- "No"
test$purebreed[grep("Mix", as.character(test$Breed))] <- "No"
test$purebreed <- as.factor(test$purebreed)

test <- test[, -c(2, 3, 5, 6, 7, 8)]

######################################################
## BUILD A FOREST TO PREDICT THE IMPORTANT FEATURES ##
######################################################

forest <- randomForest(OutcomeType ~ AnimalType + is_named + yearsOld + color1a +
                               color1b + color2a + color2b + sexKnown + sex + 
                               fixed + yearOutcome + monthOutcome + 
                               weekdayOutcome + dayOutcome + hourOutcome + 
                               purebreed, 
                       data = train[!is.na(train$yearsOld), ], ntree = 100, 
                       importance = TRUE)

varImpPlot(forest)

Prediction <- predict(forest, test)
