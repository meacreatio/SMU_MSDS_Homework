# Live session Unit 05 assignment
Matt Pavlovich  
10/01/2017  



# Link to homework files:
#### https://github.com/meacreatio/SMU_MSDS_Homework/tree/master/Homework/6306/week5

# Question 1: Data Munging 
#### a. Import text file and rename colmns

```r
df <- read.table("yob2016.txt", sep = ";", header = F)
names(df) <- c("Name", "Sex", "Count")
```

#### b. Data Summary and Structure

```r
summary(df)
```

```
##       Name       Sex           Count        
##  Aalijah:    2   F:18758   Min.   :    5.0  
##  Aaliyan:    2   M:14111   1st Qu.:    7.0  
##  Aamari :    2             Median :   12.0  
##  Aarian :    2             Mean   :  110.7  
##  Aarin  :    2             3rd Qu.:   30.0  
##  Aaris  :    2             Max.   :19414.0  
##  (Other):32857
```

```r
str(df)
```

```
## 'data.frame':	32869 obs. of  3 variables:
##  $ Name : Factor w/ 30295 levels "Aaban","Aabha",..: 9317 22546 3770 26409 12019 20596 6185 339 9298 11222 ...
##  $ Sex  : Factor w/ 2 levels "F","M": 1 1 1 1 1 1 1 1 1 1 ...
##  $ Count: int  19414 19246 16237 16070 14722 14366 13030 11699 10926 10733 ...
```

```r
dim(df)
```

```
## [1] 32869     3
```

#### c. Display name that ends with 'yyy'

```r
df.mispelled.name <- df[grep("yyy$", df$Name), ]
as.character(df.mispelled.name$Name[1])
```

```
## [1] "Fionayyy"
```

#### d. Remove invalid name and create new dataframe reference

```r
index.misspelled.name <- which(df$Name == df.mispelled.name$Name[1])
y2016 <- df[-index.misspelled.name, ]
```

# Question 2: Data Merging
#### a. Import text file and rename colmns

```r
y2015 <- read.table("yob2015.txt", sep = ",", header = F)
names(y2015) <- c("Name", "Sex", "Count")
```

#### b. Display last ten rows

```r
tail(y2015, n = 10)
```

```
##         Name Sex Count
## 33054   Ziyu   M     5
## 33055   Zoel   M     5
## 33056  Zohar   M     5
## 33057 Zolton   M     5
## 33058   Zyah   M     5
## 33059 Zykell   M     5
## 33060 Zyking   M     5
## 33061  Zykir   M     5
## 33062  Zyrus   M     5
## 33063   Zyus   M     5
```

```r
# all names are male and the dataset is ordered by sex ascending and count descending
```

#### c. Merge 2016 and 2015

```r
final <- merge(y2015, y2016, by = c("Name", "Sex"), all = T)
final <- final[complete.cases(final), ]
```

# Question 3: Data Summary
#### a. Amount of children

```r
final$Total <- rowSums(final[, c("Count.x", "Count.y")])
sum(final$Total)
```

```
## [1] 7239231
```

#### b. Sort by Total and display top ten names

```r
df.names.sorted.total <- final[order(-final$Total), ]
head(df.names.sorted.total, n = 10)
```

```
##           Name Sex Count.x Count.y Total
## 10406     Emma   F   20415   19414 39829
## 25135   Olivia   F   19638   19246 38884
## 24768     Noah   M   19594   19015 38609
## 20350     Liam   M   18330   18138 36468
## 29447   Sophia   F   17381   16070 33451
## 4074       Ava   F   16340   16237 32577
## 22375    Mason   M   16591   15192 31783
## 31995  William   M   15863   15668 31531
## 13786    Jacob   M   15914   14416 30330
## 13402 Isabella   F   15574   14722 30296
```

##### c. Display top ten girls' names

```r
df.names.sorted.total.girls <- df.names.sorted.total[df.names.sorted.total$Sex == "F", ]
girl <- df.names.sorted.total.girls[1:10, ]
girl
```

```
##            Name Sex Count.x Count.y Total
## 10406      Emma   F   20415   19414 39829
## 25135    Olivia   F   19638   19246 38884
## 29447    Sophia   F   17381   16070 33451
## 4074        Ava   F   16340   16237 32577
## 13402  Isabella   F   15574   14722 30296
## 23052       Mia   F   14871   14366 29237
## 6878  Charlotte   F   11381   13030 24411
## 358     Abigail   F   12371   11699 24070
## 10384     Emily   F   11766   10926 22692
## 12542    Harper   F   10283   10733 21016
```

#### d. Write top 10 girls names to a csv file

```r
write.csv(girl[, c("Name", "Total")], file = "itsagirl.csv", row.names = F)
```
