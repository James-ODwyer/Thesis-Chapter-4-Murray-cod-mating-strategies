
# Supplementary material C-7 
# R code to calculate the number of offspring each adult contributed to

#Novel use of sibship and parental reconstruction sheds light on the mating system of an iconic Australian freshwater fish 


#O'Dwyer J E1-2, Harrisson KA1-3, Tonkin Z3, Lyon J3, Zampatti B4, Koster W3, Raymond S3 Dawson D3, Bice C5, Murphy N1-2


#All locations

bestcluster <-read.csv("C:/Users/18088076/Dropbox/PhD/Chapters/Recruitment over time/analysis/all rivers/larvae only/colony/newnamed/long/MC_all_larvae_details.BestCluster", sep="")

bestcluster <-read.csv("D:/Dropbox/PhD/Chapters/Recruitment over time/analysis/all rivers/larvae only/colony/newnamed/long/MC_all_larvae_details.BestCluster", sep="")

bestcluster <- subset(bestcluster,bestcluster$Probability>=0.99)

library(plyr)

library(ggplot2)


bestcluster$OffspringID <- gsub("-", "",bestcluster$OffspringID)
bestcluster$OffspringID <- gsub(".", "_",bestcluster$OffspringID,fixed = TRUE)


for (i in c(1:nrow(bestcluster))) {
  
  # substring to year 
  sub1  <- substr(bestcluster$OffspringID[i], 3,4)
  
  # substring to river 
  sub2  <- substr(bestcluster$OffspringID[i], 5,5)
  
  #substring to site
  sub3  <- substr(bestcluster$OffspringID[i], 6,6)
  
  #substring to week
  sub4 <- substr(bestcluster$OffspringID[i], 10,10)
  
  #substring to length
  sub5 <- substr(bestcluster$OffspringID[i], 11,16)
  
  
  bestcluster$year1[i] <- sub1
  bestcluster$river1[i] <- sub2
  bestcluster$site1[i] <- sub3
  bestcluster$week1[i] <- sub4
  bestcluster$length1[i] <- sub5
  
  
}

bestcluster$length1 <- gsub("_", ".",bestcluster$length1, fixed = TRUE)
bestcluster$length1 <- gsub("L", "",bestcluster$length1, fixed = TRUE)
bestcluster$length1 <- as.numeric(bestcluster$length1)


bestcluster2014 <- subset(bestcluster, bestcluster$year1==14)
bestcluster2015 <- subset(bestcluster, bestcluster$year1==15)
bestcluster2016 <- subset(bestcluster, bestcluster$year1==16)
bestcluster2017 <- subset(bestcluster, bestcluster$year1==17)
bestcluster2018 <- subset(bestcluster, bestcluster$year1==18)






##########Year by year################ 
# analyse the frequency each generated adult pops up for each offspring
# Bestcluster all rivers
# 



#get father clusters
dad_cluster_year <- matrix(nrow =nrow(bestcluster), ncol=2)
dad_cluster_year <- as.data.frame(dad_cluster_year)
dad_cluster_year[,1] <- bestcluster$FatherID
dad_cluster_year[,2] <- bestcluster$year1
dad_cluster_year[,2] <- as.character(dad_cluster_year[,2])

#split into years
dad_cluster_year2014 <- subset(dad_cluster_year, dad_cluster_year$V2=="14")
dad_cluster_year2015 <- subset(dad_cluster_year, dad_cluster_year$V2=="15")
dad_cluster_year2016 <- subset(dad_cluster_year, dad_cluster_year$V2=="16")
dad_cluster_year2017 <- subset(dad_cluster_year, dad_cluster_year$V2=="17")
dad_cluster_year2018 <- subset(dad_cluster_year, dad_cluster_year$V2=="18")


# get seperate counts for each year
dad_cluster_year_count2014 <- count(dad_cluster_year2014,'V1')
rownames(dad_cluster_year_count2014) <- dad_cluster_year_count2014$V1
dad_cluster_year_count2014$V1 <- NULL
colnames(dad_cluster_year_count2014) <- "2014"

dad_cluster_year_count2015 <- count(dad_cluster_year2015,'V1')
rownames(dad_cluster_year_count2015) <- dad_cluster_year_count2015$V1
dad_cluster_year_count2015$V1 <- NULL
colnames(dad_cluster_year_count2015) <- "2015"

dad_cluster_year_count2016 <- count(dad_cluster_year2016,'V1')
rownames(dad_cluster_year_count2016) <- dad_cluster_year_count2016$V1
dad_cluster_year_count2016$V1 <- NULL
colnames(dad_cluster_year_count2016) <- "2016"

dad_cluster_year_count2017 <- count(dad_cluster_year2017,'V1')
rownames(dad_cluster_year_count2017) <- dad_cluster_year_count2017$V1
dad_cluster_year_count2017$V1 <- NULL
colnames(dad_cluster_year_count2017) <- "2017"

dad_cluster_year_count2018 <- count(dad_cluster_year2018,'V1')
rownames(dad_cluster_year_count2018) <- dad_cluster_year_count2018$V1
dad_cluster_year_count2018$V1 <- NULL
colnames(dad_cluster_year_count2018) <- "2018"

#prepare summary table to take in data
dad_cluster_year_count <- count(dad_cluster_year,'V1')

dad_summary_years <- matrix(nrow =nrow(dad_cluster_year_count), ncol=6)

dad_summary_years <- as.data.frame(dad_summary_years)
rownames(dad_summary_years) <- dad_cluster_year_count$V1
colnames(dad_summary_years) <- c("individual", "2014", "2015", "2016", "2017", "2018")
dad_summary_years$individual <- dad_cluster_year_count$V1

# add each year in progressively

#2014 added
dad_summary_years2 <- merge(data.frame(dad_summary_years, row.names=rownames(dad_summary_years)), data.frame(dad_cluster_year_count2014, row.names=rownames(dad_cluster_year_count2014)), by = 0, all = TRUE)[-1]
#2015 added
dad_summary_years2 <- merge(data.frame(dad_summary_years2, row.names=rownames(dad_summary_years)), data.frame(dad_cluster_year_count2015, row.names=rownames(dad_cluster_year_count2015)), by = 0, all = TRUE)[-1]
#2016 added
dad_summary_years2 <- merge(data.frame(dad_summary_years2, row.names=rownames(dad_summary_years)), data.frame(dad_cluster_year_count2016, row.names=rownames(dad_cluster_year_count2016)), by = 0, all = TRUE)[-1]
#2017 added
dad_summary_years2 <- merge(data.frame(dad_summary_years2, row.names=rownames(dad_summary_years)), data.frame(dad_cluster_year_count2017, row.names=rownames(dad_cluster_year_count2017)), by = 0, all = TRUE)[-1]
#2018 added
dad_summary_years2 <- merge(data.frame(dad_summary_years2, row.names=rownames(dad_summary_years)), data.frame(dad_cluster_year_count2018, row.names=rownames(dad_cluster_year_count2018)), by = 0, all = TRUE)[-1]


#rename columns
colnames(dad_summary_years2) <- c("Individual", "2014","2015", "2016", "2017", "2018", "2014x","2015x", "2016x", "2017x", "2018x")

#compress down data
dad_summary_years2$`2014` <- dad_summary_years2$`2014x`
dad_summary_years2$`2015` <- dad_summary_years2$`2015x`
dad_summary_years2$`2016` <- dad_summary_years2$`2016x`
dad_summary_years2$`2017` <- dad_summary_years2$`2017x`
dad_summary_years2$`2018` <- dad_summary_years2$`2018x`

dad_summary_years2$`2014x` <- NULL
dad_summary_years2$`2015x` <- NULL
dad_summary_years2$`2016x` <- NULL
dad_summary_years2$`2017x` <- NULL
dad_summary_years2$`2018x` <- NULL

#replace NA with 0
dad_summary_years2[is.na(dad_summary_years2)] <- 0



# bestcluster code
# Repeat for mothers

#get mother clusters
mom_cluster_year <- matrix(nrow =nrow(bestcluster), ncol=2)
mom_cluster_year <- as.data.frame(mom_cluster_year)
mom_cluster_year[,1] <- bestcluster$MotherID
mom_cluster_year[,2] <- bestcluster$year1
mom_cluster_year[,2] <- as.character(mom_cluster_year[,2])

#split into years
mom_cluster_year2014 <- subset(mom_cluster_year, mom_cluster_year$V2=="14")
mom_cluster_year2015 <- subset(mom_cluster_year, mom_cluster_year$V2=="15")
mom_cluster_year2016 <- subset(mom_cluster_year, mom_cluster_year$V2=="16")
mom_cluster_year2017 <- subset(mom_cluster_year, mom_cluster_year$V2=="17")
mom_cluster_year2018 <- subset(mom_cluster_year, mom_cluster_year$V2=="18")


# get seperate counts for each year
mom_cluster_year_count2014 <- count(mom_cluster_year2014,'V1')
rownames(mom_cluster_year_count2014) <- mom_cluster_year_count2014$V1
mom_cluster_year_count2014$V1 <- NULL
colnames(mom_cluster_year_count2014) <- "2014"

mom_cluster_year_count2015 <- count(mom_cluster_year2015,'V1')
rownames(mom_cluster_year_count2015) <- mom_cluster_year_count2015$V1
mom_cluster_year_count2015$V1 <- NULL
colnames(mom_cluster_year_count2015) <- "2015"

mom_cluster_year_count2016 <- count(mom_cluster_year2016,'V1')
rownames(mom_cluster_year_count2016) <- mom_cluster_year_count2016$V1
mom_cluster_year_count2016$V1 <- NULL
colnames(mom_cluster_year_count2016) <- "2016"

mom_cluster_year_count2017 <- count(mom_cluster_year2017,'V1')
rownames(mom_cluster_year_count2017) <- mom_cluster_year_count2017$V1
mom_cluster_year_count2017$V1 <- NULL
colnames(mom_cluster_year_count2017) <- "2017"

mom_cluster_year_count2018 <- count(mom_cluster_year2018,'V1')
rownames(mom_cluster_year_count2018) <- mom_cluster_year_count2018$V1
mom_cluster_year_count2018$V1 <- NULL
colnames(mom_cluster_year_count2018) <- "2018"


mom_cluster_year_count <- count(mom_cluster_year,'V1')

#prepare summary table to take in data
mom_summary_years <- matrix(nrow =nrow(mom_cluster_year_count), ncol=6)

mom_summary_years <- as.data.frame(mom_summary_years)
rownames(mom_summary_years) <- mom_cluster_year_count$V1
colnames(mom_summary_years) <- c("individual", "2014", "2015", "2016", "2017", "2018")
mom_summary_years$individual <- mom_cluster_year_count$V1

# bestcluster code
# add each year in progressively

#2014 added
mom_summary_years2 <- merge(data.frame(mom_summary_years, row.names=rownames(mom_summary_years)), data.frame(mom_cluster_year_count2014, row.names=rownames(mom_cluster_year_count2014)), by = 0, all = TRUE)[-1]
#2015 added
mom_summary_years2 <- merge(data.frame(mom_summary_years2, row.names=rownames(mom_summary_years)), data.frame(mom_cluster_year_count2015, row.names=rownames(mom_cluster_year_count2015)), by = 0, all = TRUE)[-1]
#2016 added
mom_summary_years2 <- merge(data.frame(mom_summary_years2, row.names=rownames(mom_summary_years)), data.frame(mom_cluster_year_count2016, row.names=rownames(mom_cluster_year_count2016)), by = 0, all = TRUE)[-1]
#2017 added
mom_summary_years2 <- merge(data.frame(mom_summary_years2, row.names=rownames(mom_summary_years)), data.frame(mom_cluster_year_count2017, row.names=rownames(mom_cluster_year_count2017)), by = 0, all = TRUE)[-1]
#2018 added
mom_summary_years2 <- merge(data.frame(mom_summary_years2, row.names=rownames(mom_summary_years)), data.frame(mom_cluster_year_count2018, row.names=rownames(mom_cluster_year_count2018)), by = 0, all = TRUE)[-1]


#rename columns
colnames(mom_summary_years2) <- c("Individual", "2014","2015", "2016", "2017", "2018", "2014x","2015x", "2016x", "2017x", "2018x")

#compress down data
mom_summary_years2$`2014` <- mom_summary_years2$`2014x`
mom_summary_years2$`2015` <- mom_summary_years2$`2015x`
mom_summary_years2$`2016` <- mom_summary_years2$`2016x`
mom_summary_years2$`2017` <- mom_summary_years2$`2017x`
mom_summary_years2$`2018` <- mom_summary_years2$`2018x`

mom_summary_years2$`2014x` <- NULL
mom_summary_years2$`2015x` <- NULL
mom_summary_years2$`2016x` <- NULL
mom_summary_years2$`2017x` <- NULL
mom_summary_years2$`2018x` <- NULL

#replace NA with 0
mom_summary_years2[is.na(mom_summary_years2)] <- 0

#parent summary containing dads and moms
parent_summary_years <- rbind(mom_summary_years2,dad_summary_years2)

rownames(parent_summary_years) <- parent_summary_years[,1]

parent_summary_years$sum <- NA

for (i in (1:nrow(parent_summary_years))) {
  
  
  parent_summary_years$sum[i] <- sum(parent_summary_years[i,2:6])
  
  
}


# gg plot the resulting sum columns
parent_summary_yearscount <- count(parent_summary_years, "sum")
parentsummary_yearssum <- parent_summary_years[,c(1,7)]
ggplot(parent_summary_yearscount, aes(sum, freq)) + geom_bar(stat="identity") +xlab("Number of sequenced offspring each parent produced") + ylab("Number of parents identified") +ylim(0,315) + theme_bw()

parent_summary_yearscountall <- parent_summary_yearscount

# Now Goulburn


bestclustergoulburn <- subset(bestcluster, bestcluster$river1=="G")




#get father clusters
dad_cluster_year <- matrix(nrow =nrow(bestclustergoulburn), ncol=2)
dad_cluster_year <- as.data.frame(dad_cluster_year)
dad_cluster_year[,1] <- bestclustergoulburn$FatherID
dad_cluster_year[,2] <- bestclustergoulburn$year1
dad_cluster_year[,2] <- as.character(dad_cluster_year[,2])

#split into years
dad_cluster_year2014 <- subset(dad_cluster_year, dad_cluster_year$V2=="14")
dad_cluster_year2015 <- subset(dad_cluster_year, dad_cluster_year$V2=="15")
dad_cluster_year2016 <- subset(dad_cluster_year, dad_cluster_year$V2=="16")
dad_cluster_year2017 <- subset(dad_cluster_year, dad_cluster_year$V2=="17")
dad_cluster_year2018 <- subset(dad_cluster_year, dad_cluster_year$V2=="18")


# get seperate counts for each year
dad_cluster_year_count2014 <- count(dad_cluster_year2014,'V1')
rownames(dad_cluster_year_count2014) <- dad_cluster_year_count2014$V1
dad_cluster_year_count2014$V1 <- NULL
colnames(dad_cluster_year_count2014) <- "2014"

dad_cluster_year_count2015 <- count(dad_cluster_year2015,'V1')
rownames(dad_cluster_year_count2015) <- dad_cluster_year_count2015$V1
dad_cluster_year_count2015$V1 <- NULL
colnames(dad_cluster_year_count2015) <- "2015"

dad_cluster_year_count2016 <- count(dad_cluster_year2016,'V1')
rownames(dad_cluster_year_count2016) <- dad_cluster_year_count2016$V1
dad_cluster_year_count2016$V1 <- NULL
colnames(dad_cluster_year_count2016) <- "2016"

dad_cluster_year_count2017 <- count(dad_cluster_year2017,'V1')
rownames(dad_cluster_year_count2017) <- dad_cluster_year_count2017$V1
dad_cluster_year_count2017$V1 <- NULL
colnames(dad_cluster_year_count2017) <- "2017"

dad_cluster_year_count2018 <- count(dad_cluster_year2018,'V1')
rownames(dad_cluster_year_count2018) <- dad_cluster_year_count2018$V1
dad_cluster_year_count2018$V1 <- NULL
colnames(dad_cluster_year_count2018) <- "2018"

#prepare summary table to take in data
dad_cluster_year_count <- count(dad_cluster_year,'V1')

dad_summary_years <- matrix(nrow =nrow(dad_cluster_year_count), ncol=6)

dad_summary_years <- as.data.frame(dad_summary_years)
rownames(dad_summary_years) <- dad_cluster_year_count$V1
colnames(dad_summary_years) <- c("individual", "2014", "2015", "2016", "2017", "2018")
dad_summary_years$individual <- dad_cluster_year_count$V1

# add each year in progressively

#2014 added
dad_summary_years2 <- merge(data.frame(dad_summary_years, row.names=rownames(dad_summary_years)), data.frame(dad_cluster_year_count2014, row.names=rownames(dad_cluster_year_count2014)), by = 0, all = TRUE)[-1]
#2015 added
dad_summary_years2 <- merge(data.frame(dad_summary_years2, row.names=rownames(dad_summary_years)), data.frame(dad_cluster_year_count2015, row.names=rownames(dad_cluster_year_count2015)), by = 0, all = TRUE)[-1]
#2016 added
dad_summary_years2 <- merge(data.frame(dad_summary_years2, row.names=rownames(dad_summary_years)), data.frame(dad_cluster_year_count2016, row.names=rownames(dad_cluster_year_count2016)), by = 0, all = TRUE)[-1]
#2017 added
dad_summary_years2 <- merge(data.frame(dad_summary_years2, row.names=rownames(dad_summary_years)), data.frame(dad_cluster_year_count2017, row.names=rownames(dad_cluster_year_count2017)), by = 0, all = TRUE)[-1]
#2018 added
dad_summary_years2 <- merge(data.frame(dad_summary_years2, row.names=rownames(dad_summary_years)), data.frame(dad_cluster_year_count2018, row.names=rownames(dad_cluster_year_count2018)), by = 0, all = TRUE)[-1]


#rename columns
colnames(dad_summary_years2) <- c("Individual", "2014","2015", "2016", "2017", "2018", "2014x","2015x", "2016x", "2017x", "2018x")

#compress down data
dad_summary_years2$`2014` <- dad_summary_years2$`2014x`
dad_summary_years2$`2015` <- dad_summary_years2$`2015x`
dad_summary_years2$`2016` <- dad_summary_years2$`2016x`
dad_summary_years2$`2017` <- dad_summary_years2$`2017x`
dad_summary_years2$`2018` <- dad_summary_years2$`2018x`

dad_summary_years2$`2014x` <- NULL
dad_summary_years2$`2015x` <- NULL
dad_summary_years2$`2016x` <- NULL
dad_summary_years2$`2017x` <- NULL
dad_summary_years2$`2018x` <- NULL

#replace NA with 0
dad_summary_years2[is.na(dad_summary_years2)] <- 0



# bestcluster code
# Repeat for mothers

#get mother clusters
mom_cluster_year <- matrix(nrow =nrow(bestclustergoulburn), ncol=2)
mom_cluster_year <- as.data.frame(mom_cluster_year)
mom_cluster_year[,1] <- bestclustergoulburn$MotherID
mom_cluster_year[,2] <- bestclustergoulburn$year1
mom_cluster_year[,2] <- as.character(mom_cluster_year[,2])

#split into years
mom_cluster_year2014 <- subset(mom_cluster_year, mom_cluster_year$V2=="14")
mom_cluster_year2015 <- subset(mom_cluster_year, mom_cluster_year$V2=="15")
mom_cluster_year2016 <- subset(mom_cluster_year, mom_cluster_year$V2=="16")
mom_cluster_year2017 <- subset(mom_cluster_year, mom_cluster_year$V2=="17")
mom_cluster_year2018 <- subset(mom_cluster_year, mom_cluster_year$V2=="18")


# get seperate counts for each year
mom_cluster_year_count2014 <- count(mom_cluster_year2014,'V1')
rownames(mom_cluster_year_count2014) <- mom_cluster_year_count2014$V1
mom_cluster_year_count2014$V1 <- NULL
colnames(mom_cluster_year_count2014) <- "2014"

mom_cluster_year_count2015 <- count(mom_cluster_year2015,'V1')
rownames(mom_cluster_year_count2015) <- mom_cluster_year_count2015$V1
mom_cluster_year_count2015$V1 <- NULL
colnames(mom_cluster_year_count2015) <- "2015"

mom_cluster_year_count2016 <- count(mom_cluster_year2016,'V1')
rownames(mom_cluster_year_count2016) <- mom_cluster_year_count2016$V1
mom_cluster_year_count2016$V1 <- NULL
colnames(mom_cluster_year_count2016) <- "2016"

mom_cluster_year_count2017 <- count(mom_cluster_year2017,'V1')
rownames(mom_cluster_year_count2017) <- mom_cluster_year_count2017$V1
mom_cluster_year_count2017$V1 <- NULL
colnames(mom_cluster_year_count2017) <- "2017"

mom_cluster_year_count2018 <- count(mom_cluster_year2018,'V1')
rownames(mom_cluster_year_count2018) <- mom_cluster_year_count2018$V1
mom_cluster_year_count2018$V1 <- NULL
colnames(mom_cluster_year_count2018) <- "2018"


mom_cluster_year_count <- count(mom_cluster_year,'V1')

#prepare summary table to take in data
mom_summary_years <- matrix(nrow =nrow(mom_cluster_year_count), ncol=6)

mom_summary_years <- as.data.frame(mom_summary_years)
rownames(mom_summary_years) <- mom_cluster_year_count$V1
colnames(mom_summary_years) <- c("individual", "2014", "2015", "2016", "2017", "2018")
mom_summary_years$individual <- mom_cluster_year_count$V1

# bestcluster code
# add each year in progressively

#2014 added
mom_summary_years2 <- merge(data.frame(mom_summary_years, row.names=rownames(mom_summary_years)), data.frame(mom_cluster_year_count2014, row.names=rownames(mom_cluster_year_count2014)), by = 0, all = TRUE)[-1]
#2015 added
mom_summary_years2 <- merge(data.frame(mom_summary_years2, row.names=rownames(mom_summary_years)), data.frame(mom_cluster_year_count2015, row.names=rownames(mom_cluster_year_count2015)), by = 0, all = TRUE)[-1]
#2016 added
mom_summary_years2 <- merge(data.frame(mom_summary_years2, row.names=rownames(mom_summary_years)), data.frame(mom_cluster_year_count2016, row.names=rownames(mom_cluster_year_count2016)), by = 0, all = TRUE)[-1]
#2017 added
mom_summary_years2 <- merge(data.frame(mom_summary_years2, row.names=rownames(mom_summary_years)), data.frame(mom_cluster_year_count2017, row.names=rownames(mom_cluster_year_count2017)), by = 0, all = TRUE)[-1]
#2018 added
mom_summary_years2 <- merge(data.frame(mom_summary_years2, row.names=rownames(mom_summary_years)), data.frame(mom_cluster_year_count2018, row.names=rownames(mom_cluster_year_count2018)), by = 0, all = TRUE)[-1]


#rename columns
colnames(mom_summary_years2) <- c("Individual", "2014","2015", "2016", "2017", "2018", "2014x","2015x", "2016x", "2017x", "2018x")

#compress down data
mom_summary_years2$`2014` <- mom_summary_years2$`2014x`
mom_summary_years2$`2015` <- mom_summary_years2$`2015x`
mom_summary_years2$`2016` <- mom_summary_years2$`2016x`
mom_summary_years2$`2017` <- mom_summary_years2$`2017x`
mom_summary_years2$`2018` <- mom_summary_years2$`2018x`

mom_summary_years2$`2014x` <- NULL
mom_summary_years2$`2015x` <- NULL
mom_summary_years2$`2016x` <- NULL
mom_summary_years2$`2017x` <- NULL
mom_summary_years2$`2018x` <- NULL

#replace NA with 0
mom_summary_years2[is.na(mom_summary_years2)] <- 0

#parent summary containing dads and moms
parent_summary_years <- rbind(mom_summary_years2,dad_summary_years2)

# Before continued need to count number of parents who contributed for each year specifically
# Best cluster
# polygamous parents 
# Goulburn
# For table 
# FS HS 

# number of parents which contributed to each year for Goulburn
# 2014
sum(parent_summary_years[,2] !=0)
# 2015
sum(parent_summary_years[,3] !=0)
# 2016
sum(parent_summary_years[,4] !=0)
#2017
sum(parent_summary_years[,5] !=0)
#2018
sum(parent_summary_years[,6] !=0)


rownames(parent_summary_years) <- parent_summary_years[,1]

parent_summary_years$sum <- NA

for (i in (1:nrow(parent_summary_years))) {
  
  
  parent_summary_years$sum[i] <- sum(parent_summary_years[i,2:6])
  
  
}


# gg plot the resulting sum columns
parent_summary_yearscount <- count(parent_summary_years, "sum")
parentsummary_yearssum <- parent_summary_years[,c(1,7)]
ggplot(parent_summary_yearscount, aes(sum, freq)) + geom_bar(stat="identity") +xlab("Number of sequenced offspring each parent produced") + ylab("Number of parents identified") +ylim(0,315) + theme_bw()

parent_summary_yearscountgoulburn <- parent_summary_yearscount

# Now Barmah

bestclusterbarmah <- subset(bestcluster, bestcluster$river1=="B")
bestclusterbarmah <- subset(bestclusterbarmah, bestclusterbarmah$year1 == "17" | bestclusterbarmah$year1 == "18")
nrow(bestclusterbarmah)

#get father clusters
dad_cluster_year <- matrix(nrow =nrow(bestclusterbarmah), ncol=2)
dad_cluster_year <- as.data.frame(dad_cluster_year)
dad_cluster_year[,1] <- bestclusterbarmah$FatherID
dad_cluster_year[,2] <- bestclusterbarmah$year1
dad_cluster_year[,2] <- as.character(dad_cluster_year[,2])

#split into years

dad_cluster_year2017 <- subset(dad_cluster_year, dad_cluster_year$V2=="17")
dad_cluster_year2018 <- subset(dad_cluster_year, dad_cluster_year$V2=="18")


# get seperate counts for each year

dad_cluster_year_count2017 <- count(dad_cluster_year2017,'V1')
rownames(dad_cluster_year_count2017) <- dad_cluster_year_count2017$V1
dad_cluster_year_count2017$V1 <- NULL
colnames(dad_cluster_year_count2017) <- "2017"

dad_cluster_year_count2018 <- count(dad_cluster_year2018,'V1')
rownames(dad_cluster_year_count2018) <- dad_cluster_year_count2018$V1
dad_cluster_year_count2018$V1 <- NULL
colnames(dad_cluster_year_count2018) <- "2018"

#prepare summary table to take in data
dad_cluster_year_count <- count(dad_cluster_year,'V1')

dad_summary_years <- matrix(nrow =nrow(dad_cluster_year_count), ncol=3)

dad_summary_years <- as.data.frame(dad_summary_years)
rownames(dad_summary_years) <- dad_cluster_year_count$V1
colnames(dad_summary_years) <- c("individual",  "2017", "2018")
dad_summary_years$individual <- dad_cluster_year_count$V1

# add each year in progressively

#2017 added
dad_summary_years2 <- merge(data.frame(dad_summary_years, row.names=rownames(dad_summary_years)), data.frame(dad_cluster_year_count2017, row.names=rownames(dad_cluster_year_count2017)), by = 0, all = TRUE)[-1]
#2018 added
dad_summary_years2 <- merge(data.frame(dad_summary_years2, row.names=rownames(dad_summary_years)), data.frame(dad_cluster_year_count2018, row.names=rownames(dad_cluster_year_count2018)), by = 0, all = TRUE)[-1]


#rename columns
colnames(dad_summary_years2) <- c("Individual", "2017", "2018",  "2017x", "2018x")

#compress down data

dad_summary_years2$`2017` <- dad_summary_years2$`2017x`
dad_summary_years2$`2018` <- dad_summary_years2$`2018x`


dad_summary_years2$`2017x` <- NULL
dad_summary_years2$`2018x` <- NULL

#replace NA with 0
dad_summary_years2[is.na(dad_summary_years2)] <- 0



# bestcluster code
# Repeat for mothers

#get mother clusters
mom_cluster_year <- matrix(nrow =nrow(bestclusterbarmah), ncol=2)
mom_cluster_year <- as.data.frame(mom_cluster_year)
mom_cluster_year[,1] <- bestclusterbarmah$MotherID
mom_cluster_year[,2] <- bestclusterbarmah$year1
mom_cluster_year[,2] <- as.character(mom_cluster_year[,2])

#split into years

mom_cluster_year2017 <- subset(mom_cluster_year, mom_cluster_year$V2=="17")
mom_cluster_year2018 <- subset(mom_cluster_year, mom_cluster_year$V2=="18")


# get seperate counts for each year

mom_cluster_year_count2017 <- count(mom_cluster_year2017,'V1')
rownames(mom_cluster_year_count2017) <- mom_cluster_year_count2017$V1
mom_cluster_year_count2017$V1 <- NULL
colnames(mom_cluster_year_count2017) <- "2017"

mom_cluster_year_count2018 <- count(mom_cluster_year2018,'V1')
rownames(mom_cluster_year_count2018) <- mom_cluster_year_count2018$V1
mom_cluster_year_count2018$V1 <- NULL
colnames(mom_cluster_year_count2018) <- "2018"


mom_cluster_year_count <- count(mom_cluster_year,'V1')

#prepare summary table to take in data
mom_summary_years <- matrix(nrow =nrow(mom_cluster_year_count), ncol=3)

mom_summary_years <- as.data.frame(mom_summary_years)
rownames(mom_summary_years) <- mom_cluster_year_count$V1
colnames(mom_summary_years) <- c("individual", "2017", "2018")
mom_summary_years$individual <- mom_cluster_year_count$V1

# bestcluster code
# add each year in progressively

#2017 added
mom_summary_years2 <- merge(data.frame(mom_summary_years, row.names=rownames(mom_summary_years)), data.frame(mom_cluster_year_count2017, row.names=rownames(mom_cluster_year_count2017)), by = 0, all = TRUE)[-1]
#2018 added
mom_summary_years2 <- merge(data.frame(mom_summary_years2, row.names=rownames(mom_summary_years)), data.frame(mom_cluster_year_count2018, row.names=rownames(mom_cluster_year_count2018)), by = 0, all = TRUE)[-1]


#rename columns
colnames(mom_summary_years2) <- c("Individual", "2017", "2018", "2017x", "2018x")

#compress down data

mom_summary_years2$`2017` <- mom_summary_years2$`2017x`
mom_summary_years2$`2018` <- mom_summary_years2$`2018x`


mom_summary_years2$`2017x` <- NULL
mom_summary_years2$`2018x` <- NULL

#replace NA with 0
mom_summary_years2[is.na(mom_summary_years2)] <- 0

#parent summary containing dads and moms
parent_summary_years <- rbind(mom_summary_years2,dad_summary_years2)

rownames(parent_summary_years) <- parent_summary_years[,1]
nrow(parent_summary_years)

# Number of adults which contributed to offspring for each year for Barmah
sum(parent_summary_years[,2] !=0)
sum(parent_summary_years[,3] !=0)

parent_summary_years$sum <- NA

for (i in (1:nrow(parent_summary_years))) {
  
  
  parent_summary_years$sum[i] <- sum(parent_summary_years[i,2:3])
  
  
}


# gg plot the resulting sum columns
parent_summary_yearscount <- count(parent_summary_years, "sum")
parentsummary_yearssum <- parent_summary_years[,c(1,4)]
ggplot(parent_summary_yearscount, aes(sum, freq)) + geom_bar(stat="identity") +xlab("Number of sequenced offspring each parent produced") + ylab("Number of parents identified") +ylim(0,315) + theme_bw()

parent_summary_yearscountbarmah <- parent_summary_yearscount

# now Chowilla



bestclusterchowilla <- subset(bestcluster, bestcluster$river1=="C")




#get father clusters
dad_cluster_year <- matrix(nrow =nrow(bestclusterchowilla), ncol=2)
dad_cluster_year <- as.data.frame(dad_cluster_year)
dad_cluster_year[,1] <- bestclusterchowilla$FatherID
dad_cluster_year[,2] <- bestclusterchowilla$year1
dad_cluster_year[,2] <- as.character(dad_cluster_year[,2])

#split into years
dad_cluster_year2014 <- subset(dad_cluster_year, dad_cluster_year$V2=="14")
dad_cluster_year2015 <- subset(dad_cluster_year, dad_cluster_year$V2=="15")
dad_cluster_year2016 <- subset(dad_cluster_year, dad_cluster_year$V2=="16")
dad_cluster_year2017 <- subset(dad_cluster_year, dad_cluster_year$V2=="17")
dad_cluster_year2018 <- subset(dad_cluster_year, dad_cluster_year$V2=="18")


# get seperate counts for each year
dad_cluster_year_count2014 <- count(dad_cluster_year2014,'V1')
rownames(dad_cluster_year_count2014) <- dad_cluster_year_count2014$V1
dad_cluster_year_count2014$V1 <- NULL
colnames(dad_cluster_year_count2014) <- "2014"

dad_cluster_year_count2015 <- count(dad_cluster_year2015,'V1')
rownames(dad_cluster_year_count2015) <- dad_cluster_year_count2015$V1
dad_cluster_year_count2015$V1 <- NULL
colnames(dad_cluster_year_count2015) <- "2015"

dad_cluster_year_count2016 <- count(dad_cluster_year2016,'V1')
rownames(dad_cluster_year_count2016) <- dad_cluster_year_count2016$V1
dad_cluster_year_count2016$V1 <- NULL
colnames(dad_cluster_year_count2016) <- "2016"

dad_cluster_year_count2017 <- count(dad_cluster_year2017,'V1')
rownames(dad_cluster_year_count2017) <- dad_cluster_year_count2017$V1
dad_cluster_year_count2017$V1 <- NULL
colnames(dad_cluster_year_count2017) <- "2017"

dad_cluster_year_count2018 <- count(dad_cluster_year2018,'V1')
rownames(dad_cluster_year_count2018) <- dad_cluster_year_count2018$V1
dad_cluster_year_count2018$V1 <- NULL
colnames(dad_cluster_year_count2018) <- "2018"

#prepare summary table to take in data
dad_cluster_year_count <- count(dad_cluster_year,'V1')

dad_summary_years <- matrix(nrow =nrow(dad_cluster_year_count), ncol=6)

dad_summary_years <- as.data.frame(dad_summary_years)
rownames(dad_summary_years) <- dad_cluster_year_count$V1
colnames(dad_summary_years) <- c("individual", "2014", "2015", "2016", "2017", "2018")
dad_summary_years$individual <- dad_cluster_year_count$V1

# add each year in progressively

#2014 added
dad_summary_years2 <- merge(data.frame(dad_summary_years, row.names=rownames(dad_summary_years)), data.frame(dad_cluster_year_count2014, row.names=rownames(dad_cluster_year_count2014)), by = 0, all = TRUE)[-1]
#2015 added
dad_summary_years2 <- merge(data.frame(dad_summary_years2, row.names=rownames(dad_summary_years)), data.frame(dad_cluster_year_count2015, row.names=rownames(dad_cluster_year_count2015)), by = 0, all = TRUE)[-1]
#2016 added
dad_summary_years2 <- merge(data.frame(dad_summary_years2, row.names=rownames(dad_summary_years)), data.frame(dad_cluster_year_count2016, row.names=rownames(dad_cluster_year_count2016)), by = 0, all = TRUE)[-1]
#2017 added
dad_summary_years2 <- merge(data.frame(dad_summary_years2, row.names=rownames(dad_summary_years)), data.frame(dad_cluster_year_count2017, row.names=rownames(dad_cluster_year_count2017)), by = 0, all = TRUE)[-1]
#2018 added
dad_summary_years2 <- merge(data.frame(dad_summary_years2, row.names=rownames(dad_summary_years)), data.frame(dad_cluster_year_count2018, row.names=rownames(dad_cluster_year_count2018)), by = 0, all = TRUE)[-1]


#rename columns
colnames(dad_summary_years2) <- c("Individual", "2014","2015", "2016", "2017", "2018", "2014x","2015x", "2016x", "2017x", "2018x")

#compress down data
dad_summary_years2$`2014` <- dad_summary_years2$`2014x`
dad_summary_years2$`2015` <- dad_summary_years2$`2015x`
dad_summary_years2$`2016` <- dad_summary_years2$`2016x`
dad_summary_years2$`2017` <- dad_summary_years2$`2017x`
dad_summary_years2$`2018` <- dad_summary_years2$`2018x`

dad_summary_years2$`2014x` <- NULL
dad_summary_years2$`2015x` <- NULL
dad_summary_years2$`2016x` <- NULL
dad_summary_years2$`2017x` <- NULL
dad_summary_years2$`2018x` <- NULL

#replace NA with 0
dad_summary_years2[is.na(dad_summary_years2)] <- 0



# bestcluster code
# Repeat for mothers

#get mother clusters
mom_cluster_year <- matrix(nrow =nrow(bestclusterchowilla), ncol=2)
mom_cluster_year <- as.data.frame(mom_cluster_year)
mom_cluster_year[,1] <- bestclusterchowilla$MotherID
mom_cluster_year[,2] <- bestclusterchowilla$year1
mom_cluster_year[,2] <- as.character(mom_cluster_year[,2])

#split into years
mom_cluster_year2014 <- subset(mom_cluster_year, mom_cluster_year$V2=="14")
mom_cluster_year2015 <- subset(mom_cluster_year, mom_cluster_year$V2=="15")
mom_cluster_year2016 <- subset(mom_cluster_year, mom_cluster_year$V2=="16")
mom_cluster_year2017 <- subset(mom_cluster_year, mom_cluster_year$V2=="17")
mom_cluster_year2018 <- subset(mom_cluster_year, mom_cluster_year$V2=="18")


# get seperate counts for each year
mom_cluster_year_count2014 <- count(mom_cluster_year2014,'V1')
rownames(mom_cluster_year_count2014) <- mom_cluster_year_count2014$V1
mom_cluster_year_count2014$V1 <- NULL
colnames(mom_cluster_year_count2014) <- "2014"

mom_cluster_year_count2015 <- count(mom_cluster_year2015,'V1')
rownames(mom_cluster_year_count2015) <- mom_cluster_year_count2015$V1
mom_cluster_year_count2015$V1 <- NULL
colnames(mom_cluster_year_count2015) <- "2015"

mom_cluster_year_count2016 <- count(mom_cluster_year2016,'V1')
rownames(mom_cluster_year_count2016) <- mom_cluster_year_count2016$V1
mom_cluster_year_count2016$V1 <- NULL
colnames(mom_cluster_year_count2016) <- "2016"

mom_cluster_year_count2017 <- count(mom_cluster_year2017,'V1')
rownames(mom_cluster_year_count2017) <- mom_cluster_year_count2017$V1
mom_cluster_year_count2017$V1 <- NULL
colnames(mom_cluster_year_count2017) <- "2017"

mom_cluster_year_count2018 <- count(mom_cluster_year2018,'V1')
rownames(mom_cluster_year_count2018) <- mom_cluster_year_count2018$V1
mom_cluster_year_count2018$V1 <- NULL
colnames(mom_cluster_year_count2018) <- "2018"


mom_cluster_year_count <- count(mom_cluster_year,'V1')

#prepare summary table to take in data
mom_summary_years <- matrix(nrow =nrow(mom_cluster_year_count), ncol=6)

mom_summary_years <- as.data.frame(mom_summary_years)
rownames(mom_summary_years) <- mom_cluster_year_count$V1
colnames(mom_summary_years) <- c("individual", "2014", "2015", "2016", "2017", "2018")
mom_summary_years$individual <- mom_cluster_year_count$V1

# bestcluster code
# add each year in progressively

#2014 added
mom_summary_years2 <- merge(data.frame(mom_summary_years, row.names=rownames(mom_summary_years)), data.frame(mom_cluster_year_count2014, row.names=rownames(mom_cluster_year_count2014)), by = 0, all = TRUE)[-1]
#2015 added
mom_summary_years2 <- merge(data.frame(mom_summary_years2, row.names=rownames(mom_summary_years)), data.frame(mom_cluster_year_count2015, row.names=rownames(mom_cluster_year_count2015)), by = 0, all = TRUE)[-1]
#2016 added
mom_summary_years2 <- merge(data.frame(mom_summary_years2, row.names=rownames(mom_summary_years)), data.frame(mom_cluster_year_count2016, row.names=rownames(mom_cluster_year_count2016)), by = 0, all = TRUE)[-1]
#2017 added
mom_summary_years2 <- merge(data.frame(mom_summary_years2, row.names=rownames(mom_summary_years)), data.frame(mom_cluster_year_count2017, row.names=rownames(mom_cluster_year_count2017)), by = 0, all = TRUE)[-1]
#2018 added
mom_summary_years2 <- merge(data.frame(mom_summary_years2, row.names=rownames(mom_summary_years)), data.frame(mom_cluster_year_count2018, row.names=rownames(mom_cluster_year_count2018)), by = 0, all = TRUE)[-1]

# Chowilla
#rename columns
colnames(mom_summary_years2) <- c("Individual", "2014","2015", "2016", "2017", "2018", "2014x","2015x", "2016x", "2017x", "2018x")

#compress down data
mom_summary_years2$`2014` <- mom_summary_years2$`2014x`
mom_summary_years2$`2015` <- mom_summary_years2$`2015x`
mom_summary_years2$`2016` <- mom_summary_years2$`2016x`
mom_summary_years2$`2017` <- mom_summary_years2$`2017x`
mom_summary_years2$`2018` <- mom_summary_years2$`2018x`

mom_summary_years2$`2014x` <- NULL
mom_summary_years2$`2015x` <- NULL
mom_summary_years2$`2016x` <- NULL
mom_summary_years2$`2017x` <- NULL
mom_summary_years2$`2018x` <- NULL

#replace NA with 0
mom_summary_years2[is.na(mom_summary_years2)] <- 0
# Chowilla

#parent summary containing dads and moms
parent_summary_years <- rbind(mom_summary_years2,dad_summary_years2)
nrow(parent_summary_years)
sum(parent_summary_years[,5] >0)
sum(parent_summary_years[,6] >0)

rownames(parent_summary_years) <- parent_summary_years[,1]

parent_summary_years$sum <- NA

for (i in (1:nrow(parent_summary_years))) {
  
  
  parent_summary_years$sum[i] <- sum(parent_summary_years[i,2:6])
  
  
}


# gg plot the resulting sum columns
parent_summary_yearscount <- count(parent_summary_years, "sum")
parentsummary_yearssum <- parent_summary_years[,c(1,7)]
ggplot(parent_summary_yearscount, aes(sum, freq)) + geom_bar(stat="identity") +xlab("Number of sequenced offspring each parent produced") + ylab("Number of parents identified") +ylim(0,315) + theme_bw()

parent_summary_yearscountchowilla <- parent_summary_yearscount

# Create a combined graph of all figures

parent_summary_yearscountall
parent_summary_yearscountgoulburn
parent_summary_yearscountbarmah
parent_summary_yearscountchowilla


gpall <- ggplot(parent_summary_yearscountall, aes(sum, freq)) + geom_bar(stat="identity") +xlab("No. sequenced offspring  \neach parent produced") + ylab("No. parents identified") +ylim(0,330) + theme_bw()

gpall <- gpall + theme(axis.text = element_text(size = 10), axis.title = element_text(size = 13.0),legend.position = "none", plot.background = element_blank(), 
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank())
gpall <- gpall + scale_x_continuous(limits=c(0,22))

gpgoulburn <- ggplot(parent_summary_yearscountgoulburn, aes(sum, freq)) + geom_bar(stat="identity") +xlab("No. sequenced offspring  \neach parent produced") + ylab("No. parents identified") +ylim(0,330) + theme_bw()
gpgoulburn <- gpgoulburn + theme(axis.text = element_text(size = 10), axis.title = element_text(size = 13.0),legend.position = "none", plot.background = element_blank(),
                                 panel.grid.major = element_blank(),
                                 panel.grid.minor = element_blank())
gpgoulburn <- gpgoulburn + scale_x_continuous(limits=c(0,22))

gpbarmah <- ggplot(parent_summary_yearscountbarmah, aes(sum, freq)) + geom_bar(stat="identity") +xlab("No. sequenced offspring  \neach parent produced") + ylab("No. parents identified") +ylim(0,330) + theme_bw()
gpbarmah <- gpbarmah + theme(axis.text = element_text(size = 10), axis.title = element_text(size = 13.0),legend.position = "none", plot.background = element_blank(),
                             panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank())
gpbarmah <- gpbarmah + scale_x_continuous(limits=c(0,22))

gpchowilla <- ggplot(parent_summary_yearscountchowilla, aes(sum, freq)) + geom_bar(stat="identity") +xlab("No. sequenced offspring  \neach parent produced") + ylab("No. parents identified") +ylim(0,330) + theme_bw()
gpchowilla <- gpchowilla + theme(axis.text = element_text(size = 10), axis.title = element_text(size = 13.0),legend.position = "none", plot.background = element_blank(),
                                 panel.grid.major = element_blank(),
                                 panel.grid.minor = element_blank())
gpchowilla <- gpchowilla + scale_x_continuous(limits=c(0,22))


library(ggpubr)
#combine into 1 ggplot

tiff("number_of_offspring_from_each parent_all_graphs.tif",units='cm', width =14,height=14, res=400)
combinedplot <- ggarrange(gpall, gpgoulburn, gpbarmah, gpchowilla,
                          labels = c("A", "B", "C", "D"),label.x =0.38, font.label = list(size = 22),
                          ncol = 2, nrow = 2)
dev.off()


combinedplot <- combinedplot +theme(panel.border = element_rect(colour = "black", fill=NA, size=1) )

tiff("number_of_offspring_from_each parent_all_graphs_revised_scales.tif",units='cm', width =14,height=14, res=400)
combinedplot
dev.off()


