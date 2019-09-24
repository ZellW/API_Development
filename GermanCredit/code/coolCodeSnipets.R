#nice way to identidy missing values:
apply(data,2,function(x) sum(is.na(x)))

beets <- c(41, 40, 41, 42, 44, 35, 41, 36, 47, 45)
no_beets <- c(51, 51, 50, 42, 40, 31, 43, 45)
#Stack data: p 99 of UsingR
b <- list("beets" = beets, "no beets"=no_beets)
b
stacked <- stack(b) #results in a data frame
plot(values ~ ind, data=stacked)

# Split function
speeds <- split(michelson$Speed, michelson$Expt)# date followed by factor to split by
speeds <- stack(speeds) #puts it back together

seatbelts <- matrix(c(56, 2, 8, 16), nrow=2)
rownames(seatbelts)  <- c("buckled", "unbuckled")
colnames(seatbelts) <- c("buckled", "unbuckled")
#better way:
dimnames(seatbelts) <- list(parent=c("buckled", "unbuckled"), child=c("buckled", "unbuckled"))

gradeTable <- table(grades$prev, grades$grade)
margin.table(seatbelts, margin=1)# margin 1 is rows; 2 for columns - this adds the row numbers "
margin.table(gradeTable, margin=2)
addmargins(seatbelts)

#Automatically chage table data into a proportion by column or row:
table(grades$prev, grades$grade)
prop.table(table(grades$prev, grades$grade), margin=1)*100 #Devides each cell by the total count for the row where the cell is like 15/28 for cell (1,1)

xtabs(count ~ Whorls + Loops, Fingerprints)
plot(count ~ Whorls + Loops, Fingerprints) #did it just for fun
xtabs(~ Origin + Type, Cars93)#since nothing on left hand side or formula, it automatically tallies the data!
# the period - . - can be usedas shorthand notation for all variables in the data set not speficied on theleft hand side.
xtabs(count ~ ., Fingerprints)#since there are only 3 firleds in the dataset and count is specified then the 2 remaining fields (Whorls and Loops) 
#are assumed by the period shorthand

#Funtions UsingR p 169
lst1 <- with(ToothGrowth, split(len, supp))#splits the length col by the supp column
sapply(lst1, sd)
#can do this in 1 step using tapply:
with(ToothGrowth, tapply( len, supp, sd))
with(ToothGrowth, tapply(len, list(supp, dose), mean))
#can use aggregate function instead - suggests irt is easier than using the with construct
aggregate(len~supp, ToothGrowth, mean)

#create a data set with 5 cols
replicate(5, rnorm(3))

#Multivariate Graphics p 189 UsingR
#Lattice
xyplot(MPG.highway ~ Weight | Price, Cars93) #not very helpful so do this:
#need to bucket the prices:
prices <- equal.count(Cars93$Price, number=3, overlap=0)
xyplot(MPG.highway ~ Weight | prices, Cars93, layout=c(3,1))
#Could also specify the buckets - might be more useful. Transform adds a column for Cars93 called price
Cars93_1 <- transform(Cars93, price=cut(Price, c(0, 15, 30, 75), labels=c("Cheap", "Middle", "Expensive")))
#dplyr mutate would produce the same result:
Cars93_2 <- mutate(Cars93, price=cut(Price,c(0, 15, 30, 75),labels=c("Cheap", "Moderate", "Expensive")))
#the type=p and r used below stand for point and regression line
xyplot(MPG.highway ~ Weight | price, Cars93_1, layout=c(3,1), type=c("p", "r")) 
xyplot(MPG.highway ~ Weight | price, Cars93_2, layout=c(3,1), type=c("p", "r")) 

#ggplot2 p 202 UsingR
f <- function(x) x^2
x <- seq(-2,2, length=100)
p <- ggplot(data.frame(x=x, y=f(x)), aes(x=x, y=y)) + geom_line()
#R has a function to do this easily:
curve(f, -2,2)

p2 <- ggplot(Cars93, aes(x=Cylinders, y=MPG.highway)) + geom_boxplot()
p2 <- p2 + geom_jitter(position=position_jitter(w=.1), alha=.15) #pretty cool - shows all the data and jitter avoids overalp
#Grouping is easy in ggplot:
exp1_2 <- subset(morley, Expt %in% 1:2)#Use data for experiements 1 or 2 only
#These do the same thing:
exp1_2_1 <- morley[Expt==1 | Expt==2,]
exp1_2_1 <- morley[Expt<3,]

p3 <- ggplot(exp1_2_1, aes(x=Run, y=Speed, group=Expt, color=Expt)) + geom_line()

