#Supplementary material C-5
# R code to filter and analyse sibling relationships for degrees of monogamy and polygamy


#Novel use of sibship and parental reconstruction sheds light on the mating system of an iconic Australian freshwater fish 


#O'Dwyer J E1-2, Harrisson KA1-3, Tonkin Z3, Lyon J3, Zampatti B4, Koster W3, Raymond S3 Dawson D3, Bice C5, Murphy N1-2

#Supplementary material 4

# Code to determine full sibling and half sibling relationships


#Related analysis

#related analyses of data 


# Create density plots and subet data
# from Related output, determine the range of relatedness values which represent each class of relationship based
# on the simulated relatioships. 

#tiff("Density_plot_simulated_familiy_relationships_all_larvae.tif",units='cm', width =14,height=14, res=600)
qplot (relvalues , ..density.., data = newdata , geom ="density", colour =as.factor(Relationship), xlab =" Relatedness Value ", ylab =" Density ")
dev.off()

#determine cut offs so incorrect relationships aren't included
#changing values to see how many of each class are included in different cut offs
sum(newdata$relvalues >= 0.14 & newdata$Relationship=="Unrelated")

sum(newdata$relvalues >= 0.4 & newdata$Relationship=="Half" )

sum(newdata$relvalues <= 0.14 & newdata$Relationship=="Half" )

sum(newdata$relvalues <= 0.4 & newdata$Relationship=="Full")

#save the new subsets of data for full siblings and half siblings for related

full_sibtable <- subset(output[["relatedness"]], (output[["relatedness"]]$dyadml>= 0.40 ))
half_sibtable <- subset(output[["relatedness"]], (output[["relatedness"]]$dyadml<= 0.39) & (output[["relatedness"]]$dyadml>= 0.15) )



#######
setwd("C:/Users/18088076/Dropbox/PhD/Chapters/Recruitment over time/analysis/all rivers/larvae only/colony/newnamed/long")
# filter Colony full sibs and half sibs to 99% and over certainty for assignment
Colony_fullsib_table <- read.table("MC_all_larvae_details.FullSibDyad", header = TRUE,sep="\t")

Colony_fullsib_table <- subset(Colony_fullsib_table, Colony_fullsib_table$Probability >= 0.99)

Colony_half_table <- read.table("MC_all_larvae_details.HalfSibDyad", header = TRUE, sep="\t")

Colony_half_table <- subset(Colony_half_table, Colony_half_table$Probability >= 0.99)


#Remove excess columns Related data

cols<-!(colnames(full_sibtable) %in% c("pair.no","group","trioml","wang", "lynchli", "lynchrd", "ritland","quellergt" ))
Related_fullsib_table <- subset(full_sibtable,,cols)

cols<-!(colnames(half_sibtable) %in% c("pair.no","group","trioml","wang", "lynchli", "lynchrd", "ritland","quellergt" ))
Related_half_table <- subset(half_sibtable,,cols)

# Remove formatting differences between tables
# prepare data to be combined
Related_fullsib_table$ind1.id <- gsub("_", "", Related_fullsib_table$ind1.id)
Related_fullsib_table$ind2.id <- gsub("_", "", Related_fullsib_table$ind2.id)
Related_fullsib_table$ind1.id <- gsub("-", "", Related_fullsib_table$ind1.id)
Related_fullsib_table$ind2.id <- gsub("-", "", Related_fullsib_table$ind2.id)

Related_half_table$ind1.id <- gsub("_", "", Related_half_table$ind1.id)
Related_half_table$ind2.id <- gsub("_", "", Related_half_table$ind2.id)
Related_half_table$ind1.id <- gsub("-", "", Related_half_table$ind1.id)
Related_half_table$ind2.id <- gsub("-", "", Related_half_table$ind2.id)

Colony_fullsib_table$OffspringID1 <- gsub("-", "",Colony_fullsib_table$OffspringID1)
Colony_fullsib_table$OffspringID2 <- gsub("-", "",Colony_fullsib_table$OffspringID2)
Colony_half_table$OffspringID1 <- gsub("-", "",Colony_half_table$OffspringID1)
Colony_half_table$OffspringID2 <- gsub("-", "",Colony_half_table$OffspringID2)

#generate new combined dataset that only includes individuals found in both related and Colony
dataframe <- matrix(nrow=500, ncol=2)
n=0

# For loop, scans through each cell of each table looking for the identical match for a dyad from both Related 
# and colony
for (j in c(1:nrow(Related_fullsib_table))) {
  
  for (i in c(1:nrow(Colony_fullsib_table))) {
    
    if ((Colony_fullsib_table$OffspringID1[i] == Related_fullsib_table$ind1.id[j] & Colony_fullsib_table$OffspringID2[i] == Related_fullsib_table$ind2.id[j])  | (Colony_fullsib_table$OffspringID1[i] == Related_fullsib_table$ind2.id[j] & Colony_fullsib_table$OffspringID2[i] == Related_fullsib_table$ind1.id[j])  ) {
      
      n =n+1
      print(n)
      dataframe[n,1]  <- as.character(Colony_fullsib_table$OffspringID1[i])
      dataframe[n,2]  <- as.character(Colony_fullsib_table$OffspringID2[i])
    }
  }
}

# Convert to a easy to read combined table
combined_fullsib_dyads_both_tests <- dataframe 
combined_fullsib_dyads_both_tests <- as.data.frame(combined_fullsib_dyads_both_tests)  
combined_fullsib_dyads_both_tests <-    combined_fullsib_dyads_both_tests[complete.cases(combined_fullsib_dyads_both_tests), ]

colnames(combined_fullsib_dyads_both_tests) <- c("OffspringID1", "OffspringID2")


# Repeat for Half siblings
Colony_half_table$OffspringID1 <- gsub("-", "",Colony_half_table$OffspringID1)
Colony_half_table$OffspringID2 <- gsub("-", "",Colony_half_table$OffspringID2)

dataframe <- matrix(nrow=800, ncol=2)
n=0


for (j in c(1:nrow(Related_half_table))) {
  
  for (i in c(1:nrow(Colony_half_table))) {
    
    if ((Colony_half_table$OffspringID1[i] == Related_half_table$ind1.id[j] & Colony_half_table$OffspringID2[i] == Related_half_table$ind2.id[j])  | (Colony_half_table$OffspringID1[i] == Related_half_table$ind2.id[j] & Colony_half_table$OffspringID2[i] == Related_half_table$ind1.id[j])  ) {
      
      n =n+1
      print(n)
      dataframe[n,1]  <- as.character(Colony_half_table$OffspringID1[i])
      dataframe[n,2]  <- as.character(Colony_half_table$OffspringID2[i])
    }
  }
}



combined_halfsib_dyads_both_tests <- dataframe 
combined_halfsib_dyads_both_tests <- as.data.frame(combined_halfsib_dyads_both_tests)  
combined_halfsib_dyads_both_tests <-    combined_halfsib_dyads_both_tests[complete.cases(combined_halfsib_dyads_both_tests), ]


colnames(combined_halfsib_dyads_both_tests) <- c("OffspringID1", "OffspringID2")

#rename shorter
combined_halfsib <- combined_halfsib_dyads_both_tests
combined_fullsib <- combined_fullsib_dyads_both_tests

# Full siblings, use naming scheme in Id to separate data so correct location, sites, years, larval lengths, and week of sampling 
# are included in the tables
combined_fullsib$OffspringID1 <- gsub("-", "",combined_fullsib$OffspringID1)
combined_fullsib$OffspringID2 <- gsub("-", "",combined_fullsib$OffspringID2)
combined_fullsib$OffspringID1 <- gsub(".", "_",combined_fullsib$OffspringID1,fixed = TRUE)
combined_fullsib$OffspringID2 <- gsub(".", "_",combined_fullsib$OffspringID2, fixed = TRUE)

#FS info
for (i in c(1:nrow(combined_fullsib))) {
  
  # substring to year 
  sub1  <- substr(combined_fullsib$OffspringID1[i], 3,4)
  sub1_2  <- substr(combined_fullsib$OffspringID2[i], 3,4)    
  
  # substring to river 
  sub2  <- substr(combined_fullsib$OffspringID1[i], 5,5)
  sub2_2  <- substr(combined_fullsib$OffspringID2[i], 5,5)
  
  #substring to site
  sub3  <- substr(combined_fullsib$OffspringID1[i], 6,6)
  sub3_2  <- substr(combined_fullsib$OffspringID2[i], 6,6)
  
  #substring to week
  sub4 <- substr(combined_fullsib$OffspringID1[i], 10,10)
  sub4_2 <- substr(combined_fullsib$OffspringID2[i], 10,10)
  
  #substring to length
  sub5 <- substr(combined_fullsib$OffspringID1[i], 11,16)
  sub5_2 <- substr(combined_fullsib$OffspringID2[i], 11,16)
  
  
  combined_fullsib$year1[i] <- sub1
  combined_fullsib$year2[i] <- sub1_2
  combined_fullsib$river1[i] <- sub2
  combined_fullsib$river2[i] <- sub2_2
  combined_fullsib$site1[i] <- sub3
  combined_fullsib$site2[i] <- sub3_2
  combined_fullsib$week1[i] <- sub4
  combined_fullsib$week2[i] <- sub4_2
  combined_fullsib$length1[i] <- sub5
  combined_fullsib$length2[i] <- sub5_2
  
  
  
  combined_fullsib$yeardiff[i] <- (as.numeric(combined_fullsib$year1[i]) - as.numeric(combined_fullsib$year2[i]))
  
  
}
#Make data easier to work with and consistent

combined_fullsib$OffspringID1 <- gsub("_", ".",combined_fullsib$OffspringID1, fixed = TRUE)
combined_fullsib$OffspringID2 <- gsub("_", ".",combined_fullsib$OffspringID2, fixed = TRUE)
combined_fullsib$length1 <- gsub("_", ".",combined_fullsib$length1, fixed = TRUE)
combined_fullsib$length2 <- gsub("_", ".",combined_fullsib$length2, fixed = TRUE)
combined_fullsib$length1 <- gsub("L", "",combined_fullsib$length1, fixed = TRUE)
combined_fullsib$length2 <- gsub("L", "",combined_fullsib$length2, fixed = TRUE)
combined_fullsib$length1 <- as.numeric(combined_fullsib$length1)
combined_fullsib$length2 <- as.numeric(combined_fullsib$length2)
combined_fullsib$yeardiff <- abs(combined_fullsib$yeardiff)

#calculate length difference between all larvae in dyad pairs
for (i in c(1:nrow(combined_fullsib))) {
  combined_fullsib$lengthdiff[i] <-abs((as.numeric(combined_fullsib$length1[i]) - as.numeric(combined_fullsib$length2[i])))
}

#Full sibling analysis
# Subset into locations
combined_fullsib_goulburn  <- subset(combined_fullsib, combined_fullsib$river1=="G")
combined_fullsib_barmah  <- subset(combined_fullsib, combined_fullsib$river1=="B")
combined_fullsib_chowilla  <- subset(combined_fullsib, combined_fullsib$river1=="C")

#calculate Goulburn information
#Monogamous pairings for goulburn
monogamous_pairings_goulburn <- subset(combined_fullsib_goulburn, (combined_fullsib_goulburn$year1 == combined_fullsib_goulburn$year2))


monogamous_pairings_2014 <- subset(combined_fullsib_goulburn, ((combined_fullsib_goulburn$year1 == combined_fullsib_goulburn$year2) & combined_fullsib_goulburn$year1 ==14 ))
monogamous_pairings_2015 <- subset(combined_fullsib_goulburn, ((combined_fullsib_goulburn$year1 == combined_fullsib_goulburn$year2) & combined_fullsib_goulburn$year1 ==15 ))
monogamous_pairings_2016 <- subset(combined_fullsib_goulburn, ((combined_fullsib_goulburn$year1 == combined_fullsib_goulburn$year2) & combined_fullsib_goulburn$year1 ==16 ))
monogamous_pairings_2017 <- subset(combined_fullsib_goulburn, ((combined_fullsib_goulburn$year1 == combined_fullsib_goulburn$year2) & combined_fullsib_goulburn$year1 ==17 ))
monogamous_pairings_2018 <- subset(combined_fullsib_goulburn, ((combined_fullsib_goulburn$year1 == combined_fullsib_goulburn$year2) & combined_fullsib_goulburn$year1 ==18 ))

# information per year for monogamous pairings
# count the unique number of monogamous parings so no double ups occur
#length unique = number of offspring per year who had one or more full sibs
#still need to know how many have just full sibs and no half sibs
fullsiblings2014 <-  monogamous_pairings_2014[,1:2]
fullsiblings2014 <- as.vector(c((fullsiblings2014[,1]),(fullsiblings2014[,2])))
length(unique(fullsiblings2014))

fullsiblings2015 <-  monogamous_pairings_2015[,1:2]
fullsiblings2015 <- as.vector(c((fullsiblings2015[,1]),(fullsiblings2015[,2])))
length(unique(fullsiblings2015))

fullsiblings2016 <-  monogamous_pairings_2016[,1:2]
fullsiblings2016 <- as.vector(c((fullsiblings2016[,1]),(fullsiblings2016[,2])))
length(unique(fullsiblings2016))


fullsiblings2017 <-  monogamous_pairings_2017[,1:2]
fullsiblings2017 <- as.vector(c((fullsiblings2017[,1]),(fullsiblings2017[,2])))
length(unique(fullsiblings2017))

fullsiblings2018 <-  monogamous_pairings_2018[,1:2]
fullsiblings2018 <- as.vector(c((fullsiblings2018[,1]),(fullsiblings2018[,2])))
length(unique(fullsiblings2018))


# Now Barmah
monogamous_pairings_barmah <- subset(combined_fullsib_barmah, (combined_fullsib_barmah$year1 == combined_fullsib_barmah$year2))

monogamous_pairings_2017 <- subset(combined_fullsib_barmah, ((combined_fullsib_barmah$year1 == combined_fullsib_barmah$year2) & combined_fullsib_barmah$year1 ==17 ))
monogamous_pairings_2018 <- subset(combined_fullsib_barmah, ((combined_fullsib_barmah$year1 == combined_fullsib_barmah$year2) & combined_fullsib_barmah$year1 ==18 ))


fullsiblings2017 <-  monogamous_pairings_2017[,1:2]
fullsiblings2017 <- as.vector(c((fullsiblings2017[,1]),(fullsiblings2017[,2])))
length(unique(fullsiblings2017))

fullsiblings2018 <-  monogamous_pairings_2018[,1:2]
fullsiblings2018 <- as.vector(c((fullsiblings2018[,1]),(fullsiblings2018[,2])))
length(unique(fullsiblings2018))


#Now Chowilla
monogamous_pairings_chowilla <- subset(combined_fullsib_chowilla, (combined_fullsib_chowilla$year1 == combined_fullsib_chowilla$year2))

monogamous_pairings_2017 <- subset(combined_fullsib_chowilla, ((combined_fullsib_chowilla$year1 == combined_fullsib_chowilla$year2) & combined_fullsib_chowilla$year1 ==17 ))
monogamous_pairings_2018 <- subset(combined_fullsib_chowilla, ((combined_fullsib_chowilla$year1 == combined_fullsib_chowilla$year2) & combined_fullsib_chowilla$year1 ==18 ))

#FS info 

fullsiblings2017 <-  monogamous_pairings_2017[,1:2]
fullsiblings2017 <- as.vector(c((fullsiblings2017[,1]),(fullsiblings2017[,2])))
length(unique(fullsiblings2017))

fullsiblings2018 <-  monogamous_pairings_2018[,1:2]
fullsiblings2018 <- as.vector(c((fullsiblings2018[,1]),(fullsiblings2018[,2])))
length(unique(fullsiblings2018))


# Now have how many full sibling pairs were found across each location and across each year.

# Now to analyse half siblings

combined_halfsib$OffspringID1 <- gsub(".", "_",combined_halfsib$OffspringID1,fixed = TRUE)
combined_halfsib$OffspringID2 <- gsub(".", "_",combined_halfsib$OffspringID2,fixed = TRUE)


#HS info 
#use naming scheme in Id to separate data so correct location, sites, years, larval lengths, and week of sampling 
# are included in the tables
#what are the year and length differences between inds of each dyad?

for (i in c(1:nrow(combined_halfsib))) {
  
  # substring to year 
  sub1  <- substr(combined_halfsib$OffspringID1[i], 3,4)
  sub1_2  <- substr(combined_halfsib$OffspringID2[i], 3,4)    
  
  # substring to river 
  sub2  <- substr(combined_halfsib$OffspringID1[i], 5,5)
  sub2_2  <- substr(combined_halfsib$OffspringID2[i], 5,5)
  
  #substring to site
  sub3  <- substr(combined_halfsib$OffspringID1[i], 6,6)
  sub3_2  <- substr(combined_halfsib$OffspringID2[i], 6,6)
  
  #substring to week
  sub4 <- substr(combined_halfsib$OffspringID1[i], 10,10)
  sub4_2 <- substr(combined_halfsib$OffspringID2[i], 10,10)
  
  #substring to length
  sub5 <- substr(combined_halfsib$OffspringID1[i], 11,16)
  sub5_2 <- substr(combined_halfsib$OffspringID2[i], 11,16)
  
  
  combined_halfsib$year1[i] <- sub1
  combined_halfsib$year2[i] <- sub1_2
  combined_halfsib$river1[i] <- sub2
  combined_halfsib$river2[i] <- sub2_2
  combined_halfsib$site1[i] <- sub3
  combined_halfsib$site2[i] <- sub3_2
  combined_halfsib$week1[i] <- sub4
  combined_halfsib$week2[i] <- sub4_2
  combined_halfsib$length1[i] <- sub5
  combined_halfsib$length2[i] <- sub5_2
  
  
  
  combined_halfsib$yeardiff[i] <- (as.numeric(combined_halfsib$year1[i]) - as.numeric(combined_halfsib$year2[i]))
  
}

#fix up formatting to make everything identically formatted 
combined_halfsib$OffspringID1 <- gsub("_", ".",combined_halfsib$OffspringID1, fixed = TRUE)
combined_halfsib$OffspringID2 <- gsub("_", ".",combined_halfsib$OffspringID2, fixed = TRUE)
combined_halfsib$length1 <- gsub("_", ".",combined_halfsib$length1, fixed = TRUE)
combined_halfsib$length2 <- gsub("_", ".",combined_halfsib$length2, fixed = TRUE)
combined_halfsib$length1 <- gsub("L", "",combined_halfsib$length1, fixed = TRUE)
combined_halfsib$length2 <- gsub("L", "",combined_halfsib$length2, fixed = TRUE)
combined_halfsib$length1 <- as.numeric(combined_halfsib$length1)
combined_halfsib$length2 <- as.numeric(combined_halfsib$length2)



for (i in c(1:nrow(combined_halfsib))) {
  combined_halfsib$lengthdiff[i] <-(as.numeric(combined_halfsib$length1[i]) - as.numeric(combined_halfsib$length2[i]))
}

combined_halfsib$yeardiff <- abs(combined_halfsib$yeardiff)
combined_halfsib$lengthdiff <- abs(combined_halfsib$lengthdiff)

# what half sibling pairs were found within the same season (indicative of within seasonal polygamy) vs other seasons
sum(combined_halfsib$yeardiff == 0)
sum(combined_halfsib$yeardiff == 1)
sum(combined_halfsib$yeardiff == 2)
sum(combined_halfsib$yeardiff == 3)
sum(combined_halfsib$yeardiff == 4)

# year by year information on half sibling pairs found within the same season
# how many half sibs from each year (from the same season)


# investigate the relationship between sites, year and river for full and half sib detection.
# Indicates potential bias in sampling (heightened probability of detecting relatives close by to each other)
# Half siblings first
nrow(combined_halfsib)
# 365 HS
# all data
sum(combined_halfsib$year1 == combined_halfsib$year2)
sum(combined_halfsib$site1 == combined_halfsib$site2 & combined_halfsib$year1 == combined_halfsib$year2)
sum(combined_halfsib$site1 == combined_halfsib$site2 & combined_halfsib$year1 == combined_halfsib$year2 & combined_halfsib$week1 == combined_halfsib$week2)

sum(combined_halfsib$river1 == "G")
# 168 HS
sum(combined_halfsib$year1 == combined_halfsib$year2 & combined_halfsib$river1 == "G")
sum(combined_halfsib$site1 == combined_halfsib$site2 & combined_halfsib$year1 == combined_halfsib$year2 & combined_halfsib$river1 == "G")
sum(combined_halfsib$site1 == combined_halfsib$site2 & combined_halfsib$year1 == combined_halfsib$year2 & combined_halfsib$week1 == combined_halfsib$week2 & combined_halfsib$river1 == "G")

sum(combined_halfsib$river1 == "B")
# 63 HS
sum(combined_halfsib$year1 == combined_halfsib$year2 & combined_halfsib$river1 == "B")
sum(combined_halfsib$site1 == combined_halfsib$site2 & combined_halfsib$year1 == combined_halfsib$year2 & combined_halfsib$river1 == "B")
sum(combined_halfsib$site1 == combined_halfsib$site2 & combined_halfsib$year1 == combined_halfsib$year2 & combined_halfsib$week1 == combined_halfsib$week2 & combined_halfsib$river1 == "B")

sum(combined_halfsib$river1 == "C")
# 144 HS
sum(combined_halfsib$year1 == combined_halfsib$year2 & combined_halfsib$river1 == "C")
sum(combined_halfsib$site1 == combined_halfsib$site2 & combined_halfsib$year1 == combined_halfsib$year2 & combined_halfsib$river1 == "C")
sum(combined_halfsib$site1 == combined_halfsib$site2 & combined_halfsib$year1 == combined_halfsib$year2 & combined_halfsib$week1 == combined_halfsib$week2 & combined_halfsib$river1 == "C")

# Now investigate full siblings

nrow(combined_fullsib)
# 254 FS
# all data
sum(combined_fullsib$year1 == combined_fullsib$year2)
sum(combined_fullsib$site1 == combined_fullsib$site2 & combined_fullsib$year1 == combined_fullsib$year2)
sum(combined_fullsib$site1 == combined_fullsib$site2 & combined_fullsib$year1 == combined_fullsib$year2 & combined_fullsib$week1 == combined_fullsib$week2)

# Now specific rivers
sum(combined_fullsib$river1 == "G")
# 92

sum(combined_fullsib$year1 == combined_fullsib$year2 & combined_fullsib$river1 == "G")
sum(combined_fullsib$site1 == combined_fullsib$site2 & combined_fullsib$year1 == combined_fullsib$year2 & combined_fullsib$river1 == "G")
sum(combined_fullsib$site1 == combined_fullsib$site2 & combined_fullsib$year1 == combined_fullsib$year2 & combined_fullsib$week1 == combined_fullsib$week2 & combined_fullsib$river1 == "G")


sum(combined_fullsib$river1 == "B")
# 85
sum(combined_fullsib$year1 == combined_fullsib$year2 & combined_fullsib$river1 == "B")
sum(combined_fullsib$site1 == combined_fullsib$site2 & combined_fullsib$year1 == combined_fullsib$year2 & combined_fullsib$river1 == "B")
sum(combined_fullsib$site1 == combined_fullsib$site2 & combined_fullsib$year1 == combined_fullsib$year2 & combined_fullsib$week1 == combined_fullsib$week2 & combined_fullsib$river1 == "B")

sum(combined_fullsib$river1 == "C")
# 77
sum(combined_fullsib$year1 == combined_fullsib$year2 & combined_fullsib$river1 == "C")
sum(combined_fullsib$site1 == combined_fullsib$site2 & combined_fullsib$year1 == combined_fullsib$year2 & combined_fullsib$river1 == "C")
sum(combined_fullsib$site1 == combined_fullsib$site2 & combined_fullsib$year1 == combined_fullsib$year2 & combined_fullsib$week1 == combined_fullsib$week2 & combined_fullsib$river1 == "C")



# Analyse pairs separately for each location now

combined_halfsib_goulburn <- subset(combined_halfsib, combined_halfsib$river1=="G" & combined_halfsib$river2 == "G")
combined_halfsib_barmah <- subset(combined_halfsib, combined_halfsib$river1=="B" & combined_halfsib$river2 == "B")
combined_halfsib_chowilla <- subset(combined_halfsib, combined_halfsib$river1=="C" & combined_halfsib$river2 == "C")

# Within seasonalpolygamous pairings all sites
polygamous_pairings_2014 <- subset(combined_halfsib, ((combined_halfsib$year1 == combined_halfsib$year2) & combined_halfsib$year1 ==14 ))
polygamous_pairings_2015 <- subset(combined_halfsib, ((combined_halfsib$year1 == combined_halfsib$year2) & combined_halfsib$year1 ==15 ))
polygamous_pairings_2016 <- subset(combined_halfsib, ((combined_halfsib$year1 == combined_halfsib$year2) & combined_halfsib$year1 ==16 ))
polygamous_pairings_2017 <- subset(combined_halfsib, ((combined_halfsib$year1 == combined_halfsib$year2) & combined_halfsib$year1 ==17 ))
polygamous_pairings_2018 <- subset(combined_halfsib, ((combined_halfsib$year1 == combined_halfsib$year2) & combined_halfsib$year1 ==18 ))
polygamous_pairings <- subset(combined_halfsib, (combined_halfsib$year1 == combined_halfsib$year2))
polygamous_pairings$OffspringID1 <- as.factor(polygamous_pairings$OffspringID1)
polygamous_pairings$OffspringID2 <- as.factor(polygamous_pairings$OffspringID2)



# Goulburn river
polygamous_pairings_2014 <- subset(combined_halfsib_goulburn, ((combined_halfsib_goulburn$year1 == combined_halfsib_goulburn$year2) & combined_halfsib_goulburn$year1 ==14 ))
polygamous_pairings_2015 <- subset(combined_halfsib_goulburn, ((combined_halfsib_goulburn$year1 == combined_halfsib_goulburn$year2) & combined_halfsib_goulburn$year1 ==15 ))
polygamous_pairings_2016 <- subset(combined_halfsib_goulburn, ((combined_halfsib_goulburn$year1 == combined_halfsib_goulburn$year2) & combined_halfsib_goulburn$year1 ==16 ))
polygamous_pairings_2017 <- subset(combined_halfsib_goulburn, ((combined_halfsib_goulburn$year1 == combined_halfsib_goulburn$year2) & combined_halfsib_goulburn$year1 ==17 ))
polygamous_pairings_2018 <- subset(combined_halfsib_goulburn, ((combined_halfsib_goulburn$year1 == combined_halfsib_goulburn$year2) & combined_halfsib_goulburn$year1 ==18 ))
polygamous_pairings <- subset(combined_halfsib_goulburn, (combined_halfsib_goulburn$year1 == combined_halfsib_goulburn$year2))
polygamous_pairings$OffspringID1 <- as.factor(polygamous_pairings$OffspringID1)
polygamous_pairings$OffspringID2 <- as.factor(polygamous_pairings$OffspringID2)

#Length unique shows the number of larvae which had at least one half sibling in the data
halfsibs<- cbind((c(as.character(polygamous_pairings$OffspringID1), as.character(polygamous_pairings$OffspringID2))))
halfsibs <- as.vector(halfsibs)
halfsibs <- as.factor(halfsibs)
length(unique(halfsibs))

halfsibs2014 <- cbind((c(as.character(polygamous_pairings_2014$OffspringID1), as.character(polygamous_pairings_2014$OffspringID2))))
halfsibs2014 <- as.vector(halfsibs2014)
halfsibs2014 <- as.factor(halfsibs2014)
length(unique(halfsibs2014))

halfsibs2015 <- cbind((c(as.character(polygamous_pairings_2015$OffspringID1), as.character(polygamous_pairings_2015$OffspringID2))))
halfsibs2015 <- as.vector(halfsibs2015)
halfsibs2015 <- as.factor(halfsibs2015)
length(unique(halfsibs2015))

halfsibs2016 <- cbind((c(as.character(polygamous_pairings_2016$OffspringID1), as.character(polygamous_pairings_2016$OffspringID2))))
halfsibs2016 <- as.vector(halfsibs2016)
halfsibs2016 <- as.factor(halfsibs2016)
length(unique(halfsibs2016))

halfsibs2017 <- cbind((c(as.character(polygamous_pairings_2017$OffspringID1), as.character(polygamous_pairings_2017$OffspringID2))))
halfsibs2017 <- as.vector(halfsibs2017)
halfsibs2017 <- as.factor(halfsibs2017)
length(unique(halfsibs2017))

halfsibs2018 <- cbind((c(as.character(polygamous_pairings_2018$OffspringID1), as.character(polygamous_pairings_2018$OffspringID2))))
halfsibs2018 <- as.vector(halfsibs2018)
halfsibs2018 <- as.factor(halfsibs2018)
length(unique(halfsibs2018))



#Barmah
#HS info
polygamous_pairings_2017 <- subset(combined_halfsib_barmah, ((combined_halfsib_barmah$year1 == combined_halfsib_barmah$year2) & combined_halfsib_barmah$year1 ==17 ))
polygamous_pairings_2018 <- subset(combined_halfsib_barmah, ((combined_halfsib_barmah$year1 == combined_halfsib_barmah$year2) & combined_halfsib_barmah$year1 ==18 ))
polygamous_pairings <- subset(combined_halfsib_barmah, (combined_halfsib_barmah$year1 == combined_halfsib_barmah$year2))
polygamous_pairings$OffspringID1 <- as.factor(polygamous_pairings$OffspringID1)


polygamous_pairings$OffspringID2 <- as.factor(polygamous_pairings$OffspringID2)

halfsibs2017 <- cbind((c(as.character(polygamous_pairings_2017$OffspringID1), as.character(polygamous_pairings_2017$OffspringID2))))
halfsibs2017 <- as.vector(halfsibs2017)
halfsibs2017 <- as.factor(halfsibs2017)
length(unique(halfsibs2017))

halfsibs2018 <- cbind((c(as.character(polygamous_pairings_2018$OffspringID1), as.character(polygamous_pairings_2018$OffspringID2))))
halfsibs2018 <- as.vector(halfsibs2018)
halfsibs2018 <- as.factor(halfsibs2018)
length(unique(halfsibs2018))


# Chowilla
# HS info

polygamous_pairings_2017 <- subset(combined_halfsib_chowilla, ((combined_halfsib_chowilla$year1 == combined_halfsib_chowilla$year2) & combined_halfsib_chowilla$year1 ==17 ))
polygamous_pairings_2018 <- subset(combined_halfsib_chowilla, ((combined_halfsib_chowilla$year1 == combined_halfsib_chowilla$year2) & combined_halfsib_chowilla$year1 ==18 ))
polygamous_pairings <- subset(combined_halfsib_chowilla, (combined_halfsib_chowilla$year1 == combined_halfsib_chowilla$year2))
polygamous_pairings$OffspringID1 <- as.factor(polygamous_pairings$OffspringID1)
polygamous_pairings$OffspringID2 <- as.factor(polygamous_pairings$OffspringID2)


halfsibs<- cbind((c(as.character(polygamous_pairings$OffspringID1), as.character(polygamous_pairings$OffspringID2))))
halfsibs <- as.vector(halfsibs)
halfsibs <- as.factor(halfsibs)
length(unique(halfsibs))

halfsibs2017 <- cbind((c(as.character(polygamous_pairings_2017$OffspringID1), as.character(polygamous_pairings_2017$OffspringID2))))
halfsibs2017 <- as.vector(halfsibs2017)
halfsibs2017 <- as.factor(halfsibs2017)
length(unique(halfsibs2017))

halfsibs2018 <- cbind((c(as.character(polygamous_pairings_2018$OffspringID1), as.character(polygamous_pairings_2018$OffspringID2))))
halfsibs2018 <- as.vector(halfsibs2018)
halfsibs2018 <- as.factor(halfsibs2018)
length(unique(halfsibs2018))


# At this point the number of half sibs and full sibs per year and per population is found but it is not known how many had both full and 
# half siblings during the same year

# To do this we combine the two previously created vectors and subtract unique from totals this time.

# goulburn first

monogamous_pairings_2014 <- subset(combined_fullsib_goulburn, ((combined_fullsib_goulburn$year1 == combined_fullsib_goulburn$year2) & combined_fullsib_goulburn$year1 ==14 ))
monogamous_pairings_2015 <- subset(combined_fullsib_goulburn, ((combined_fullsib_goulburn$year1 == combined_fullsib_goulburn$year2) & combined_fullsib_goulburn$year1 ==15 ))
monogamous_pairings_2016 <- subset(combined_fullsib_goulburn, ((combined_fullsib_goulburn$year1 == combined_fullsib_goulburn$year2) & combined_fullsib_goulburn$year1 ==16 ))
monogamous_pairings_2017 <- subset(combined_fullsib_goulburn, ((combined_fullsib_goulburn$year1 == combined_fullsib_goulburn$year2) & combined_fullsib_goulburn$year1 ==17 ))
monogamous_pairings_2018 <- subset(combined_fullsib_goulburn, ((combined_fullsib_goulburn$year1 == combined_fullsib_goulburn$year2) & combined_fullsib_goulburn$year1 ==18 ))

# information per year for monogamous pairings
# count the unique number of monogamous parings so no double ups occur
#length unique = number of offspring per year who had one or more full sibs
#still need to know how many have just full sibs and no half sibs
fullsiblings2014 <-  monogamous_pairings_2014[,1:2]
fullsiblings2014 <- as.vector(c((fullsiblings2014[,1]),(fullsiblings2014[,2])))
length(unique(fullsiblings2014))

fullsiblings2015 <-  monogamous_pairings_2015[,1:2]
fullsiblings2015 <- as.vector(c((fullsiblings2015[,1]),(fullsiblings2015[,2])))
length(unique(fullsiblings2015))

fullsiblings2016 <-  monogamous_pairings_2016[,1:2]
fullsiblings2016 <- as.vector(c((fullsiblings2016[,1]),(fullsiblings2016[,2])))
length(unique(fullsiblings2016))


fullsiblings2017 <-  monogamous_pairings_2017[,1:2]
fullsiblings2017 <- as.vector(c((fullsiblings2017[,1]),(fullsiblings2017[,2])))
length(unique(fullsiblings2017))

fullsiblings2018 <-  monogamous_pairings_2018[,1:2]
fullsiblings2018 <- as.vector(c((fullsiblings2018[,1]),(fullsiblings2018[,2])))
length(unique(fullsiblings2018))

polygamous_pairings_2014 <- subset(combined_halfsib_goulburn, ((combined_halfsib_goulburn$year1 == combined_halfsib_goulburn$year2) & combined_halfsib_goulburn$year1 ==14 ))
polygamous_pairings_2015 <- subset(combined_halfsib_goulburn, ((combined_halfsib_goulburn$year1 == combined_halfsib_goulburn$year2) & combined_halfsib_goulburn$year1 ==15 ))
polygamous_pairings_2016 <- subset(combined_halfsib_goulburn, ((combined_halfsib_goulburn$year1 == combined_halfsib_goulburn$year2) & combined_halfsib_goulburn$year1 ==16 ))
polygamous_pairings_2017 <- subset(combined_halfsib_goulburn, ((combined_halfsib_goulburn$year1 == combined_halfsib_goulburn$year2) & combined_halfsib_goulburn$year1 ==17 ))
polygamous_pairings_2018 <- subset(combined_halfsib_goulburn, ((combined_halfsib_goulburn$year1 == combined_halfsib_goulburn$year2) & combined_halfsib_goulburn$year1 ==18 ))
polygamous_pairings <- subset(combined_halfsib_goulburn, (combined_halfsib_goulburn$year1 == combined_halfsib_goulburn$year2))
polygamous_pairings$OffspringID1 <- as.factor(polygamous_pairings$OffspringID1)
polygamous_pairings$OffspringID2 <- as.factor(polygamous_pairings$OffspringID2)

#Length unique shows the number of larvae which had at least one half sibling in the data
halfsibs<- cbind((c(as.character(polygamous_pairings$OffspringID1), as.character(polygamous_pairings$OffspringID2))))
halfsibs <- as.vector(halfsibs)
halfsibs <- as.factor(halfsibs)
length(unique(halfsibs))

halfsibs2014 <- cbind((c(as.character(polygamous_pairings_2014$OffspringID1), as.character(polygamous_pairings_2014$OffspringID2))))
halfsibs2014 <- as.vector(halfsibs2014)
halfsibs2014 <- as.factor(halfsibs2014)
length(unique(halfsibs2014))

halfsibs2015 <- cbind((c(as.character(polygamous_pairings_2015$OffspringID1), as.character(polygamous_pairings_2015$OffspringID2))))
halfsibs2015 <- as.vector(halfsibs2015)
halfsibs2015 <- as.factor(halfsibs2015)
length(unique(halfsibs2015))

halfsibs2016 <- cbind((c(as.character(polygamous_pairings_2016$OffspringID1), as.character(polygamous_pairings_2016$OffspringID2))))
halfsibs2016 <- as.vector(halfsibs2016)
halfsibs2016 <- as.factor(halfsibs2016)
length(unique(halfsibs2016))

halfsibs2017 <- cbind((c(as.character(polygamous_pairings_2017$OffspringID1), as.character(polygamous_pairings_2017$OffspringID2))))
halfsibs2017 <- as.vector(halfsibs2017)
halfsibs2017 <- as.factor(halfsibs2017)
length(unique(halfsibs2017))

halfsibs2018 <- cbind((c(as.character(polygamous_pairings_2018$OffspringID1), as.character(polygamous_pairings_2018$OffspringID2))))
halfsibs2018 <- as.vector(halfsibs2018)
halfsibs2018 <- as.factor(halfsibs2018)
length(unique(halfsibs2018))

#how many larvae sequenced had both full and half siblings within the same year
combined_fullsibs_halfsibs2014 <- cbind((c(as.character(fullsiblings2014), as.character(halfsibs2014))))
combined_fullsibs_halfsibs2014 <- as.vector(combined_fullsibs_halfsibs2014)
combined_fullsibs_halfsibs2014 <- as.factor(combined_fullsibs_halfsibs2014)
length(unique(combined_fullsibs_halfsibs2014))
(length(unique(fullsiblings2014))+length(unique(halfsibs2014)))-length(unique(combined_fullsibs_halfsibs2014))

combined_fullsibs_halfsibs2015 <- cbind((c(as.character(fullsiblings2015), as.character(halfsibs2015))))
combined_fullsibs_halfsibs2015 <- as.vector(combined_fullsibs_halfsibs2015)
combined_fullsibs_halfsibs2015 <- as.factor(combined_fullsibs_halfsibs2015)
length(unique(combined_fullsibs_halfsibs2015))
(length(unique(fullsiblings2015))+length(unique(halfsibs2015)))-length(unique(combined_fullsibs_halfsibs2015))

combined_fullsibs_halfsibs2016 <- cbind((c(as.character(fullsiblings2016), as.character(halfsibs2016))))
combined_fullsibs_halfsibs2016 <- as.vector(combined_fullsibs_halfsibs2016)
combined_fullsibs_halfsibs2016 <- as.factor(combined_fullsibs_halfsibs2016)
length(unique(combined_fullsibs_halfsibs2016))
(length(unique(fullsiblings2016))+length(unique(halfsibs2016)))-length(unique(combined_fullsibs_halfsibs2016))

combined_fullsibs_halfsibs2017 <- cbind((c(as.character(fullsiblings2017), as.character(halfsibs2017))))
combined_fullsibs_halfsibs2017 <- as.vector(combined_fullsibs_halfsibs2017)
combined_fullsibs_halfsibs2017 <- as.factor(combined_fullsibs_halfsibs2017)
length(unique(combined_fullsibs_halfsibs2017))
(length(unique(fullsiblings2017))+length(unique(halfsibs2017)))-length(unique(combined_fullsibs_halfsibs2017))

combined_fullsibs_halfsibs2018 <- cbind((c(as.character(fullsiblings2018), as.character(halfsibs2018))))
combined_fullsibs_halfsibs2018 <- as.vector(combined_fullsibs_halfsibs2018)
combined_fullsibs_halfsibs2018 <- as.factor(combined_fullsibs_halfsibs2018)
length(unique(combined_fullsibs_halfsibs2018))
(length(unique(fullsiblings2018))+length(unique(halfsibs2018)))-length(unique(combined_fullsibs_halfsibs2018))


# Now Barmah

monogamous_pairings_2017 <- subset(combined_fullsib_barmah, ((combined_fullsib_barmah$year1 == combined_fullsib_barmah$year2) & combined_fullsib_barmah$year1 ==17 ))
monogamous_pairings_2018 <- subset(combined_fullsib_barmah, ((combined_fullsib_barmah$year1 == combined_fullsib_barmah$year2) & combined_fullsib_barmah$year1 ==18 ))


fullsiblings2017 <-  monogamous_pairings_2017[,1:2]
fullsiblings2017 <- as.vector(c((fullsiblings2017[,1]),(fullsiblings2017[,2])))
length(unique(fullsiblings2017))

fullsiblings2018 <-  monogamous_pairings_2018[,1:2]
fullsiblings2018 <- as.vector(c((fullsiblings2018[,1]),(fullsiblings2018[,2])))
length(unique(fullsiblings2018))

polygamous_pairings_2017 <- subset(combined_halfsib_barmah, ((combined_halfsib_barmah$year1 == combined_halfsib_barmah$year2) & combined_halfsib_barmah$year1 ==17 ))
polygamous_pairings_2018 <- subset(combined_halfsib_barmah, ((combined_halfsib_barmah$year1 == combined_halfsib_barmah$year2) & combined_halfsib_barmah$year1 ==18 ))


halfsibs2017 <- cbind((c(as.character(polygamous_pairings_2017$OffspringID1), as.character(polygamous_pairings_2017$OffspringID2))))
halfsibs2017 <- as.vector(halfsibs2017)
halfsibs2017 <- as.factor(halfsibs2017)
length(unique(halfsibs2017))

halfsibs2018 <- cbind((c(as.character(polygamous_pairings_2018$OffspringID1), as.character(polygamous_pairings_2018$OffspringID2))))
halfsibs2018 <- as.vector(halfsibs2018)
halfsibs2018 <- as.factor(halfsibs2018)
length(unique(halfsibs2018))


combined_fullsibs_halfsibs2017 <- cbind((c(as.character(fullsiblings2017), as.character(halfsibs2017))))
combined_fullsibs_halfsibs2017 <- as.vector(combined_fullsibs_halfsibs2017)
combined_fullsibs_halfsibs2017 <- as.factor(combined_fullsibs_halfsibs2017)
length(unique(combined_fullsibs_halfsibs2017))
(length(unique(fullsiblings2017))+length(unique(halfsibs2017)))-length(unique(combined_fullsibs_halfsibs2017))

combined_fullsibs_halfsibs2018 <- cbind((c(as.character(fullsiblings2018), as.character(halfsibs2018))))
combined_fullsibs_halfsibs2018 <- as.vector(combined_fullsibs_halfsibs2018)
combined_fullsibs_halfsibs2018 <- as.factor(combined_fullsibs_halfsibs2018)
length(unique(combined_fullsibs_halfsibs2018))
(length(unique(fullsiblings2018))+length(unique(halfsibs2018)))-length(unique(combined_fullsibs_halfsibs2018))

# Now chowilla
monogamous_pairings_chowilla <- subset(combined_fullsib_chowilla, (combined_fullsib_chowilla$year1 == combined_fullsib_chowilla$year2))

monogamous_pairings_2017 <- subset(combined_fullsib_chowilla, ((combined_fullsib_chowilla$year1 == combined_fullsib_chowilla$year2) & combined_fullsib_chowilla$year1 ==17 ))
monogamous_pairings_2018 <- subset(combined_fullsib_chowilla, ((combined_fullsib_chowilla$year1 == combined_fullsib_chowilla$year2) & combined_fullsib_chowilla$year1 ==18 ))

fullsiblings2017 <-  monogamous_pairings_2017[,1:2]
fullsiblings2017 <- as.vector(c((fullsiblings2017[,1]),(fullsiblings2017[,2])))
length(unique(fullsiblings2017))

fullsiblings2018 <-  monogamous_pairings_2018[,1:2]
fullsiblings2018 <- as.vector(c((fullsiblings2018[,1]),(fullsiblings2018[,2])))
length(unique(fullsiblings2018))


polygamous_pairings_2017 <- subset(combined_halfsib_chowilla, ((combined_halfsib_chowilla$year1 == combined_halfsib_chowilla$year2) & combined_halfsib_chowilla$year1 ==17 ))
polygamous_pairings_2018 <- subset(combined_halfsib_chowilla, ((combined_halfsib_chowilla$year1 == combined_halfsib_chowilla$year2) & combined_halfsib_chowilla$year1 ==18 ))

halfsibs2017 <- cbind((c(as.character(polygamous_pairings_2017$OffspringID1), as.character(polygamous_pairings_2017$OffspringID2))))
halfsibs2017 <- as.vector(halfsibs2017)
halfsibs2017 <- as.factor(halfsibs2017)
length(unique(halfsibs2017))

halfsibs2018 <- cbind((c(as.character(polygamous_pairings_2018$OffspringID1), as.character(polygamous_pairings_2018$OffspringID2))))
halfsibs2018 <- as.vector(halfsibs2018)
halfsibs2018 <- as.factor(halfsibs2018)
length(unique(halfsibs2018))

combined_fullsibs_halfsibs2017 <- cbind((c(as.character(fullsiblings2017), as.character(halfsibs2017))))
combined_fullsibs_halfsibs2017 <- as.vector(combined_fullsibs_halfsibs2017)
combined_fullsibs_halfsibs2017 <- as.factor(combined_fullsibs_halfsibs2017)
length(unique(combined_fullsibs_halfsibs2017))
(length(unique(fullsiblings2017))+length(unique(halfsibs2017)))-length(unique(combined_fullsibs_halfsibs2017))

combined_fullsibs_halfsibs2018 <- cbind((c(as.character(fullsiblings2018), as.character(halfsibs2018))))
combined_fullsibs_halfsibs2018 <- as.vector(combined_fullsibs_halfsibs2018)
combined_fullsibs_halfsibs2018 <- as.factor(combined_fullsibs_halfsibs2018)
length(unique(combined_fullsibs_halfsibs2018))
(length(unique(fullsiblings2018))+length(unique(halfsibs2018)))-length(unique(combined_fullsibs_halfsibs2018))


# End of sibling analysis code