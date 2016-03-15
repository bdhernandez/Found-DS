str(titanic3)

#replace NA in embarked column w/ S
titanic3$embarked[titanic3$embarked == NA] <- "S"

#checking that it worked
sum(is.na(titanic3$embarked))
sum(!is.na(titanic3$embarked))

#calculate the mean of the Age column aand use to populate missing values
AgeAv <- mean(titanic3$age, na.rm = TRUE) 
titanic3$age[is.na(titanic3$age)] <- AgeAv

#other ways to populate the NA in Age column: average age by sex
#AgeFem <- mean(titanic3$age[titanic3$sex== "female"])
#AgeMale <- mean(titanic3$age[titanic3$sex== "male"])
#titanic3$age[is.na(titanic3$age)] <AgeAv

#Fill empty slots in boat column.
titanic3$boat[titanic3$boat==""] <- NA

#create has_cabin_number dummy value column
titanic3$has_cabin_number <-  ifelse(titanic3$cabin == "", 0, 1)
