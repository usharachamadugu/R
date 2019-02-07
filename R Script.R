########################################################################################################

#install.packages("dplyr")
library(dplyr)
data()
data(mtcars)
head(mtcars)
str(mtcars)
View(mtcars)

#local_df <- tbl_df(mtcars)
#View(local_df)

#1. Filter or subset
#Base R approach to filter dataset

mtcars[mtcars$cyl==8 & mtcars$gear==5,]

#dplyr approach
#You can use "," or "&" to use and condition
filter(mtcars,cyl==8,gear==5)


filter(mtcars,cyl==8&gear==5)



filter(mtcars,cyl==8 |cyl==6)


##You can use the %in% operator
filter(mtcars,cyl %in% c(6,8))


##Converting row names into column
temp <- mtcars
temp$myNames <- rownames(temp)
filter(temp,cyl==8,gear==5)

#2. Select: Pick columns by name
#Base R approach
mtcars[,c("mpg","cyl","gear")]

#dplyr Approach
select(mtcars,mpg,cyl,gear)

# Use ":" to select multiple contiguous columns, 
#and use "contains" to match columns by name

select(mtcars,"carb",mpg:disp,"gear")


select(mtcars,mpg:disp,contains("ge"),
       contains("carb"))


#Exclude a particular column 
select(mtcars,c(-gear,-carb))


select(mtcars,-contains("ge"))


filter(select(mtcars,gear,carb,cyl),
       cyl==8|cyl==6)

#library(stringr)
#str_detect("New York","New")

#To select all columns that start with the character string "c", 
#use the function starts_with()
head(select(mtcars, starts_with("c")))

##Some additional options to select columns based on specific criteria:
#ends_with() : Select columns that end with a character string
#contains() : Select columns that contain a character string


#3. Arrange : Reorder rows
#base Approach
mtcars[order(mtcars$cyl),
       c("cyl","disp","gear")]


mtcars[order(mtcars$cyl,decreasing = T),
       c("cyl","gear")]

mtcars[order(mtcars$cyl,mtcars$gear,decreasing = T),
       c("cyl","gear")]

mtcars[order(mtcars$cyl,-(mtcars$gear),decreasing = F),
       c("cyl","gear")]


mtcars[order(-(mtcars$cyl),mtcars$gear,
             decreasing = T),c("cyl","gear")]
mtcars[order(-(mtcars$cyl),mtcars$gear),c("cyl","gear")]



#dplyr Approach
#Syntax:
#arrange(dataframe,orderby)
arrange(mtcars,cyl)
arrange(select(mtcars,"cyl","gear"),cyl)
arrange(select(mtcars,"cyl","gear"),cyl,gear)
arrange(select(mtcars,"cyl","gear"),desc(cyl))
arrange(select(mtcars,"cyl","gear"),cyl,desc(gear))


#mutate: Add new variable
#Base R Approach
temp <- mtcars
temp$new_variable <- temp$hp + temp$wt
str(temp)

temp$new_variable <- NULL
str(temp)

##dplyr Approach
temp <- mutate(temp,mutate_new = temp$hp + temp$wt)
str(temp)

# Fetch the unique values in dataframe

#Base Package approach - unique function
#unique()

unique(mtcars$cyl)
unique(mtcars[["cyl"]])
unique(mtcars["cyl"])
unique(mtcars[c("cyl","gear")])

#dplyr approach

#distinct() 
distinct(mtcars["cyl"])
distinct(mtcars[c("cyl","gear")])


#aggregate()
##base R approach (package:stats)
aggregate(mtcars$mpg, by=list(mtcars$cyl), 
          FUN=mean, na.rm=TRUE)

c(1,2,3,NA,NULL)
sum(10,20,NULL)
mean(c(2,2,2,2,NA),na.rm = T)
sum(10,20,NA)
sum(10,20,NA,na.rm = T)


aggregate(mtcars[,c("mpg","disp","hp")], 
          by=list(mtcars$cyl,mtcars$gear), 
          FUN=mean, na.rm=TRUE)



#dplyr approach
#Summarise : Reduce variable to values
summarise(mtcars,avg_mpg = mean(mpg))

summarise(mtcars,avg_mpg = mean(mpg),
          avg_disp = mean(disp))



group_by(mtcars,cyl)

summarise(group_by(mtcars,cyl),
          avg_mpg = mean(mpg))
summarise(group_by(mtcars,cyl,gear),
          avg_mpg = mean(mpg))

#Table is very handy to find the frequencies (mode)
#Base Package Approach 
table(mtcars$cyl)


#dplyr approach
#Helper Function n() count the number of rows in a group 
#Helper Function n_distinct(vector) counts the number of
#unique item in that vector
summarise(group_by(mtcars,cyl),freq=n())

summarise(group_by(mtcars,gear),freq=n())

summarise(group_by(mtcars,cyl),freq=n(),
          n_distinct(gear))

summarise(group_by(mtcars,gear),freq=n())


