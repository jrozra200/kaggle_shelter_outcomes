setwd("~/Google Drive/Grad School/Programming Practice/Shelter Outcomes")

library(plyr)
library(randomForest)
library(rpart)
library(e1071)
library(gbm)
library(lubridate)

train <- read.csv("train")
test <- read.csv("test")
dogpopularity <- read.csv("popularity.csv")
dogpopularity$BREED <- tolower(dogpopularity$BREED)
dogpopularity$BREED <- gsub(pattern = "s$", replacement = "", dogpopularity$BREED)
catpopularity <- read.csv("popular_cat_breeds.csv")
catpopularity$BREED <- tolower(catpopularity$BREED)
catpopularity$BREED <- gsub(pattern = " cat", replacement = "", x = catpopularity$BREED)
output <- read.csv("sample_submission")

head(train)
head(test)

summary(train)
summary(test)

#########################################
## FEATURE CREATION/CLEANUP: TRAIN SET ##
#########################################

train$Breed1 <- tolower(train$Breed)
train$Breed1 <- gsub(pattern = " mix", replacement = "", x = train$Breed1)
train$Breed1 <- gsub(pattern = "black/tan", replacement = "black-tan", x = train$Breed1)
breed <- strsplit(as.character(train$Breed1), "/")
breed <- ldply(breed, rbind)
names(breed) <- c("breed1a", "breed1b")

for(i in 1:dim(breed)[1]) {
        if(train$AnimalType[i] == "Dog"){
                pop1 <- dogpopularity$Average[dogpopularity$BREED == breed$breed1a[i]]
                if(!is.na(breed$breed1b[i])) {
                        pop2 <- dogpopularity$Average[dogpopularity$BREED == breed$breed1b[i]]
                } else {
                        pop2 <- NA
                }
        } else {
                pop1 <- catpopularity$RANK[catpopularity$BREED == breed$breed1a[i]]
                if(!is.na(breed$breed1b[i])) {
                        pop2 <- catpopularity$RANK[catpopularity$BREED == breed$breed1b[i]]
                } else {
                        pop2 <- NA
                }
        }
        
        pop1 <- if(length(pop1) == 0) {
                NA
        } else {
                pop1
        }
        
        pop2 <- if(length(pop2) == 0) {
                NA
        } else {
                pop2
        }
        
        train$popularity[i] <- if((is.na(pop1) & is.na(pop2)) == TRUE){
                0
        } else if ((!is.na(pop1) & is.na(pop2)) == TRUE) {
                pop1
        } else if ((is.na(pop1) & !is.na(pop2)) == TRUE) {
                pop2
        } else {
                (pop1 + pop2) / 2
        }
        
}

train$popularity[train$Breed1 == "pit bull"] <- 11

train$hourOutcome <- hour(train$DateTime)

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

train$purebreed <- "Yes"
train$purebreed[grep("/", as.character(train$Breed))] <- "No"
train$purebreed[grep("Mix", as.character(train$Breed))] <- "No"
train$purebreed <- as.factor(train$purebreed)

for(i in 1:dim(train)[1]){
        train$nameLength[i] <- nchar(as.character(train$Name[i]))
}

namelist <- ddply(train, .(Name), summarize, count = length(Name))
train$nameNormalness <- namelist$count[train$Name]
train$nameNormalness[train$Name == ""] <- 0

tree <- rpart(yearsOld ~ AnimalType + is_named + color1a + color1b + color2a +
                      color2b + sexKnown + sex + fixed + purebreed, data = train)
guessed_age <- predict(tree, train[is.na(train$yearsOld), ])
train$yearsOld[is.na(train$yearsOld)] <- guessed_age

train <- train[, -c(2, 3, 7, 8, 9, 10, 11)]

########################################
## FEATURE CREATION/CLEANUP: TEST SET ##
########################################

test$Breed1 <- tolower(test$Breed)
test$Breed1 <- gsub(pattern = " mix", replacement = "", x = test$Breed1)
test$Breed1 <- gsub(pattern = "black/tan", replacement = "black-tan", x = test$Breed1)
breed <- strsplit(as.character(test$Breed1), "/")
breed <- ldply(breed, rbind)
names(breed) <- c("breed1a", "breed1b")

for(i in 1:dim(breed)[1]) {
        if(test$AnimalType[i] == "Dog"){
                pop1 <- dogpopularity$Average[dogpopularity$BREED == breed$breed1a[i]]
                if(!is.na(breed$breed1b[i])) {
                        pop2 <- dogpopularity$Average[dogpopularity$BREED == breed$breed1b[i]]
                } else {
                        pop2 <- NA
                }
        } else {
                pop1 <- catpopularity$RANK[catpopularity$BREED == breed$breed1a[i]]
                if(!is.na(breed$breed1b[i])) {
                        pop2 <- catpopularity$RANK[catpopularity$BREED == breed$breed1b[i]]
                } else {
                        pop2 <- NA
                }
        }
        
        pop1 <- if(length(pop1) == 0) {
                NA
        } else {
                pop1
        }
        
        pop2 <- if(length(pop2) == 0) {
                NA
        } else {
                pop2
        }
        
        test$popularity[i] <- if((is.na(pop1) & is.na(pop2)) == TRUE){
                0
        } else if ((!is.na(pop1) & is.na(pop2)) == TRUE) {
                pop1
        } else if ((is.na(pop1) & !is.na(pop2)) == TRUE) {
                pop2
        } else {
                (pop1 + pop2) / 2
        }
        
}

test$popularity[test$Breed1 == "pit bull"] <- 11

test$hourOutcome <- hour(test$DateTime)

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

test$purebreed <- "Yes"
test$purebreed[grep("/", as.character(test$Breed))] <- "No"
test$purebreed[grep("Mix", as.character(test$Breed))] <- "No"
test$purebreed <- as.factor(test$purebreed)

for(i in 1:dim(test)[1]){
        test$nameLength[i] <- nchar(as.character(test$Name[i]))
}

namelist <- ddply(test, .(Name), summarize, count = length(Name))
test$nameNormalness <- namelist$count[test$Name]
test$nameNormalness[test$Name == ""] <- 0

tree <- rpart(yearsOld ~ AnimalType + is_named + color1a + color1b + color2a +
                      color2b + sexKnown + sex + fixed + purebreed, data = test)
guessed_age <- predict(tree, test[is.na(test$yearsOld), ])
test$yearsOld[is.na(test$yearsOld)] <- guessed_age


test <- test[, -c(2, 3, 5, 6, 7, 8, 9)]

levels(test$color1a) <- levels(train$color1a)
levels(test$color1b) <- levels(train$color1b)
levels(test$color2a) <- levels(train$color2a)
levels(test$color2b) <- levels(train$color2b)

######################################################
## BUILD A FOREST TO PREDICT THE IMPORTANT FEATURES ##
######################################################

## THIS IS THE WINNER SO FAR - DON'T CHANGE FROM THIS! ## 
forest <- randomForest(OutcomeType ~ AnimalType + is_named + yearsOld + color1a +
                               color1b + color2a + color2b + sexKnown + sex + 
                               fixed + yearOutcome + monthOutcome + 
                               weekdayOutcome + dayOutcome + hourOutcome + 
                               purebreed + popularity, 
                       data = train, ntree = 1000, 
                       importance = TRUE, type = "classification")

## MONKEY AWAY ## 

forest <- randomForest(OutcomeType ~ AnimalType + is_named + yearsOld + color1a +
                               color1b + color2a + color2b + sexKnown + sex + 
                               fixed + yearOutcome + monthOutcome + 
                               weekdayOutcome + dayOutcome + hourOutcome + 
                               purebreed + popularity, 
                       data = train, ntree = 1000, 
                       importance = TRUE, type = "classification")

varImpPlot(forest)

Prediction <- predict(forest, test, type = "prob")

submission <- as.data.frame(cbind(test$ID, Prediction))
names(submission) <- c("ID", "Adoption", "Died", "Euthanasia", 
                       "Return_to_owner", "Transfer")
write.csv(submission, "seventhsub.csv", row.names = FALSE)

svm_mod <- svm(OutcomeType ~ yearsOld + fixed + weekdayOutcome + AnimalType + 
                      color1a + monthOutcome, data = train, probability = TRUE)

svmPred <- predict(svm_mod, test, type = "prob")


gbm1 <- gbm(OutcomeType ~ AnimalType + popularity + hourOutcome + 
                    is_named + yearsOld + color1a + color1b + 
                    color2a + color2b + sexKnown + sex + 
                    fixed + yearOutcome + monthOutcome + 
                    weekdayOutcome + dayOutcome + hourOutcome + 
                    purebreed + nameLength + nameNormalness, data = train, 
            distribution = "multinomial", verbose=TRUE, shrinkage = 0.01, 
            n.trees = 500, interaction.depth = 15, train.fraction=0.8, 
            keep.data=FALSE)

submission2 <- predict(gbm1, test, type = "response")
dim(submission2) <- c(nrow(test),5)
colnames(submission2) <- levels(train$OutcomeType)

options(scipen=100)

sub2 <- data.frame(ID=test$ID)
sub2 <- cbind(sub2,submission2)
write.csv(sub2, "eighththsub.csv", row.names = FALSE)
