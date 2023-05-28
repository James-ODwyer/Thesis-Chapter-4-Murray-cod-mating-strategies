

#Genetic sibship and parental reconstruction sheds light on the mating systems and larval dispersal of an iconic Australian freshwater fish


#O'Dwyer J E1-2, Harrisson KA1-3, Tonkin Z3, Lyon J3, Zampatti B4, Koster W3, Raymond S3 Dawson D3, Bice C5, Murphy N1-2


#Supplementary code. R code to calculate the number of offspring each adult contributed to.
# R code to calculate the proportion of full and half sibling relationships found across/among seasons per reach, and within 
# each reach.

###################

library(dplyr)

# datasets here have an additional 11 larvae from 2016. These were not part of the main study looking into mating strategies 
# (were only 11 sample post a blackwater event. these were removed)

# This is the equivalent of Supp materials 4 but improved. 

#Related analysis

#related analyses of data 


#
#######
setwd("C:/Users/18088076/Dropbox/PhD/Chapters/Recruitment over time/analysis/all rivers/larvae only/colony/newnamed/long")

Colony_fullsib_table <- read.table("MC_all_larvae_details.FullSibDyad", header = TRUE,sep="\t")

Colony_fullsib_table <- subset(Colony_fullsib_table, Colony_fullsib_table$Probability >= 0.99)



colnames(full_sibtable)

cols<-!(colnames(full_sibtable) %in% c("pair.no","group","trioml","wang", "lynchli", "lynchrd", "ritland","quellergt" ))
Related_fullsib_table <- subset(full_sibtable,,cols)

cols<-!(colnames(half_sibtable) %in% c("pair.no","group","trioml","wang", "lynchli", "lynchrd", "ritland","quellergt" ))
Related_half_table <- subset(half_sibtable,,cols)



Colony_half_table <- read.table("MC_all_larvae_details.HalfSibDyad", header = TRUE, sep="\t")

Colony_half_table <- subset(Colony_half_table, Colony_half_table$Probability >= 0.99)

setwd("C:/Users/18088076/Dropbox/PhD/Chapters/Recruitment over time")

### Colony tables and related tables gsub out the - and the _ 

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

## Is actually very fast, this method will work 


dataframe <- matrix(nrow=300, ncol=2)
n=0


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


combined_fullsib_dyads_both_tests <- dataframe 
combined_fullsib_dyads_both_tests <- as.data.frame(combined_fullsib_dyads_both_tests)  
combined_fullsib_dyads_both_tests <-    combined_fullsib_dyads_both_tests[complete.cases(combined_fullsib_dyads_both_tests), ]

colnames(combined_fullsib_dyads_both_tests) <- c("OffspringID1", "OffspringID2")

Colony_half_table <- read.table("Goulburn_MC_larvae.HalfSibDyad", header = TRUE, sep=",")

Colony_half_table <- subset(Colony_half_table, Colony_half_table$Probability >= 0.99)
colnames(half_sibtable)

Colony_half_table$OffspringID1 <- gsub("-", "",Colony_half_table$OffspringID1)
Colony_half_table$OffspringID2 <- gsub("-", "",Colony_half_table$OffspringID2)


dataframe <- matrix(nrow=900, ncol=2)
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



#254/254 = 100% of full siblings from Colony confirmed
#355/371 = 98% of half siblings
355/371
rm(outputsim,output,dataframe,sim,simrel,relatednessmeasures,input,newdata)


#############

# full half sibling and full sibling data 
combined_halfsib <- combined_halfsib_dyads_both_tests
combined_fullsib <- combined_fullsib_dyads_both_tests

#Analysis of basic recruitment patterns 

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
combined_fullsib$OffspringID1 <- gsub("_", ".",combined_fullsib$OffspringID1, fixed = TRUE)
combined_fullsib$OffspringID2 <- gsub("_", ".",combined_fullsib$OffspringID2, fixed = TRUE)
combined_fullsib$length1 <- gsub("_", ".",combined_fullsib$length1, fixed = TRUE)
combined_fullsib$length2 <- gsub("_", ".",combined_fullsib$length2, fixed = TRUE)
combined_fullsib$length1 <- gsub("L", "",combined_fullsib$length1, fixed = TRUE)
combined_fullsib$length2 <- gsub("L", "",combined_fullsib$length2, fixed = TRUE)
combined_fullsib$length1 <- as.numeric(combined_fullsib$length1)
combined_fullsib$length2 <- as.numeric(combined_fullsib$length2)
combined_fullsib$yeardiff <- abs(combined_fullsib$yeardiff)

for (i in c(1:nrow(combined_fullsib))) {
  combined_fullsib$lengthdiff[i] <-abs((as.numeric(combined_fullsib$length1[i]) - as.numeric(combined_fullsib$length2[i])))
}


# removing the 11 sampels from barmah from 2015 + 2016 (not part of study due to very low sample size and result of black water event
# There were 8 from 2015 and 3 from 2016)
barmah15ind1 <- combined_fullsib[combined_fullsib$river1 == "B" & combined_fullsib$year1 == "15", ]
barmah15ind2<- combined_fullsib[combined_fullsib$river2 == "B" & combined_fullsib$year2 == "15", ]
barmah16ind1 <- combined_fullsib[combined_fullsib$river1 == "B" & combined_fullsib$year1 == "16", ]
barmah16ind2<- combined_fullsib[combined_fullsib$river2 == "B" & combined_fullsib$year2 == "16", ]
FSremovelist <- c(barmah15ind1$OffspringID1,barmah15ind2$OffspringID2,barmah16ind1$OffspringID1,barmah16ind2$OffspringID2)


combined_fullsib <- combined_fullsib %>% filter(!OffspringID1 %in% FSremovelist & !OffspringID2 %in% FSremovelist)

barmah15ind1half <- combined_halfsib[combined_halfsib$river1 == "B" & combined_halfsib$year1 == "15", ]
barmah15ind2half<- combined_halfsib[combined_halfsib$river2 == "B" & combined_halfsib$year2 == "15", ]
barmah16ind1half <- combined_halfsib[combined_halfsib$river1 == "B" & combined_halfsib$year1 == "16", ]
barmah16ind2half<- combined_halfsib[combined_halfsib$river2 == "B" & combined_halfsib$year2 == "16", ]
HSremovelist <- c(barmah15ind1half$OffspringID1,barmah15ind2half$OffspringID2,barmah16ind1half$OffspringID1,barmah16ind2half$OffspringID2)
combined_halfsib <- combined_halfsib %>%  filter(!OffspringID1 %in% HSremovelist & !OffspringID2 %in% HSremovelist)


### Here onward we want all the details combined

# First, How many offspring had at least one full sibling. 
all_fullsibsunique <- unique(c(combined_fullsib$OffspringID1,combined_fullsib$OffspringID2))
length(unique(c(combined_fullsib$OffspringID1,combined_fullsib$OffspringID2)))
# 201 individuals



# Now to do each site. Subset the FS is simple as all FS were from the same river but half sibs will be done site by site
combined_fullsib_goulburn <- subset(combined_fullsib,combined_fullsib$river1=="G")
combined_fullsib_barmah <- subset(combined_fullsib,combined_fullsib$river1=="B")
combined_fullsib_chowilla <- subset(combined_fullsib,combined_fullsib$river1=="C")

# number of Goulburn offspring with one or more full siblings
Goulburn_fullsibsall <- unique(c(combined_fullsib_goulburn$OffspringID1,combined_fullsib_goulburn$OffspringID2))
length(unique(c(combined_fullsib_goulburn$OffspringID1,combined_fullsib_goulburn$OffspringID2)))

# number of Barmah offspring with one or more full siblings
Barmahfullsibsall <- unique(c(combined_fullsib_barmah$OffspringID1,combined_fullsib_barmah$OffspringID2))
length(unique(c(combined_fullsib_barmah$OffspringID1,combined_fullsib_barmah$OffspringID2)))

# number of Chowilla offspring with one or more full siblings
chowillafullsibsall <- unique(c(combined_fullsib_chowilla$OffspringID1,combined_fullsib_chowilla$OffspringID2))
length(unique(c(combined_fullsib_chowilla$OffspringID1,combined_fullsib_chowilla$OffspringID2)))


Goulburnhalfsibsallfulldata <- filter(combined_halfsib_goulburncombined, OffspringID1 %in% Goulburnhalfsibsall | OffspringID2 %in% Goulburnhalfsibsall)


#half siblings
# Goulburn 
combined_halfsib_goulburnid1 <- subset(combined_halfsib,combined_halfsib$river1=="G")
combined_halfsib_goulburnid2 <- subset(combined_halfsib,combined_halfsib$river2=="G")

combined_halfsib_goulburncombined <- rbind(combined_halfsib_goulburnid1,combined_halfsib_goulburnid2)


alluniquesgoulburnhalfs <-unique(c(combined_halfsib_goulburncombined$OffspringID1,combined_halfsib_goulburncombined$OffspringID2))
#goulburnhalfsuniques_only <- vector()
#  j=1
#  for (i in c(1:length(alluniquesgoulburnhalfs))) {
#    
#    if(grepl("GY|GM|GP|GL",alluniquesgoulburnhalfs[i])) {
#      
#      goulburnhalfsuniques_only[j] <- alluniquesgoulburnhalfs[i]
#     
#      j= j+1
 #     
 #   }
  #  
   # 
  #}
  # Number pfof inds with half siblings total
# This line does the above loop.
  length(grep(pattern = "GL|GM|GP|GY", alluniquesgoulburnhalfs))
  
  # To get the index for actual inds 
  grep(pattern = "GL|GM|GP|GY", alluniquesgoulburnhalfs) -> idx
  
  Goulburnhalfsibsall <- alluniquesgoulburnhalfs[idx]
 
  # Only Goulburn half siblings now but with full data
 Goulburnhalfsibsallfulldata <- filter(combined_halfsib_goulburncombined, OffspringID1 %in% Goulburnhalfsibsall | OffspringID2 %in% Goulburnhalfsibsall)
  
  # to generate estimates of number of half siblings within and across seasons will need to re filter the goulburn subset 
  # the full data and remove barmah again 

 
 # Now how many are from a different year vs same year
 #totaldyads
 sum(Goulburnhalfsibsallfulldata$year1 !=Goulburnhalfsibsallfulldata$year2)
 sum(Goulburnhalfsibsallfulldata$year1 ==Goulburnhalfsibsallfulldata$year2)
 
 # individuals
# Gets a bit confusing here so this stat won't be included in the paper
 # Will report dyads. with inds there is the possibility of having both etc
 goulburnindssdiffyearhalfsibs <- subset(Goulburnhalfsibsallfulldata, Goulburnhalfsibsallfulldata$year1 !=Goulburnhalfsibsallfulldata$year2)
 goulburnindssdiffyearhalfsibsunique <- unique(c(goulburnindssdiffyearhalfsibs$OffspringID1,goulburnindssdiffyearhalfsibs$OffspringID2))
 length(grep(pattern = "GL|GM|GP|GY", goulburnindssdiffyearhalfsibsunique))
  
 # To count how many sibs with both FS and HS, will us unique inds of halfs and
 # then filter by names in the FS.
 
 Goulburnhalfsibsall 
 Goulburn_fullsibsalldf <- as.data.frame(matrix(nrow=length(Goulburn_fullsibsall),ncol=1))
 Goulburn_fullsibsalldf$V1 <- Goulburn_fullsibsall
 fullsibsgoulburnwithhalfsibs <- filter(Goulburn_fullsibsalldf, V1 %in% Goulburnhalfsibsall)
 unique(fullsibsgoulburnwithhalfsibs$V1)
 
 # get offspring sequenced with no siblings from
 
 #percent with HS + percent with FS -percent with both
 
 
 #half siblings
 # Barmah 
 combined_halfsib_barmahid1 <- subset(combined_halfsib,combined_halfsib$river1=="B")
 combined_halfsib_barmahid2 <- subset(combined_halfsib,combined_halfsib$river2=="B")
 
 combined_halfsib_barmahcombined <- rbind(combined_halfsib_barmahid1,combined_halfsib_barmahid2)
 
 
 alluniquesbarmahhalfs <-unique(c(combined_halfsib_barmahcombined$OffspringID1,combined_halfsib_barmahcombined$OffspringID2))
 #goulburnhalfsuniques_only <- vector()
 #  j=1
 #  for (i in c(1:length(alluniquesgoulburnhalfs))) {
 #    
 #    if(grepl("GY|GM|GP|GL",alluniquesgoulburnhalfs[i])) {
 #      
 #      goulburnhalfsuniques_only[j] <- alluniquesgoulburnhalfs[i]
 #     
 #      j= j+1
 #     
 #   }
 #  
 # 
 #}
 # Number pfof inds with half siblings total
 # This line does the above loop.
 length(grep(pattern = "BM|BL|BC", alluniquesbarmahhalfs))
 
 # To get the index for actual inds 
 grep(pattern = "BM|BL|BC", alluniquesbarmahhalfs) -> idx
 
 Barmahhalfsibsall <- alluniquesbarmahhalfs[idx]
 
 # Only Barmah half siblings now but with full data
 Barmahhalfsibsallfulldata <- filter(combined_halfsib_barmahcombined, OffspringID1 %in% Barmahhalfsibsall | OffspringID2 %in% Barmahhalfsibsall)
 
 # to generate estimates of number of half siblings within and across seasons will need to re filter the goulburn subset 
 # the full data and remove barmah again 
 
 
 # Now how many are from a different year vs same year
 #totaldyads
 sum(Barmahhalfsibsallfulldata$year1 !=Barmahhalfsibsallfulldata$year2)
 sum(Barmahhalfsibsallfulldata$year1 ==Barmahhalfsibsallfulldata$year2)
 
 
 
 # Get barmah inds with both
 Barmahhalfsibsall 
 fullsibsbarmahwithhalfsibs <- filter(combined_fullsib_barmah, OffspringID1 %in% Barmahhalfsibsall | OffspringID2 %in% Barmahhalfsibsall)
 unique(c(fullsibsbarmahwithhalfsibs$OffspringID1,fullsibsbarmahwithhalfsibs$OffspringID2))
 
 
 Barmahhalfsibsall 
 Barmah_fullsibsalldf <- as.data.frame(matrix(nrow=length(Barmahfullsibsall),ncol=1))
 Barmah_fullsibsalldf$V1 <- Barmahfullsibsall
 fullsibsbarmahwithhalfsibs <- filter(Barmah_fullsibsalldf, V1 %in% Barmahhalfsibsall)
 unique(fullsibsbarmahwithhalfsibs$V1)
 
 
 
 
 #half siblings
 # Chowilla  
 combined_halfsib_chowillaid1 <- subset(combined_halfsib,combined_halfsib$river1=="C")
 combined_halfsib_chowillaid2 <- subset(combined_halfsib,combined_halfsib$river2=="C")
 
 combined_halfsib_chowillacombined <- rbind(combined_halfsib_chowillaid1,combined_halfsib_chowillaid2)
 
 
 alluniqueschowillahalfs <-unique(c(combined_halfsib_chowillacombined$OffspringID1,combined_halfsib_chowillacombined$OffspringID2))
 #goulburnhalfsuniques_only <- vector()
 #  j=1
 #  for (i in c(1:length(alluniquesgoulburnhalfs))) {
 #    
 #    if(grepl("GY|GM|GP|GL",alluniquesgoulburnhalfs[i])) {
 #      
 #      goulburnhalfsuniques_only[j] <- alluniquesgoulburnhalfs[i]
 #     
 #      j= j+1
 #     
 #   }
 #  
 # 
 #}
 # Number pfof inds with half siblings total
 # This line does the above loop.
 length(grep(pattern = "C1|C2|C3|C4|C5|C6", alluniqueschowillahalfs))
 
 # To get the index for actual inds 
 grep(pattern = "C1|C2|C3|C4|C5|C6", alluniqueschowillahalfs) -> idx
 
 Chowillahhalfsibsall <- alluniqueschowillahalfs[idx]
 
 # Only Barmah half siblings now but with full data
 Chowillahalfsibsallfulldata <- filter(combined_halfsib_chowillacombined, OffspringID1 %in% Chowillahhalfsibsall | OffspringID2 %in% Chowillahhalfsibsall)
 
 # to generate estimates of number of half siblings within and across seasons will need to re filter the goulburn subset 
 # the full data and remove barmah again 
 
 
 # Now how many are from a different year vs same year
 #totaldyads
 sum(Chowillahalfsibsallfulldata$year1 !=Chowillahalfsibsallfulldata$year2)
 sum(Chowillahalfsibsallfulldata$year1 ==Chowillahalfsibsallfulldata$year2)
 
 Chowillahhalfsibsall 
 Chowilla_fullsibsalldf <- as.data.frame(matrix(nrow=length(chowillafullsibsall),ncol=1))
 Chowilla_fullsibsalldf$V1 <- chowillafullsibsall
 fullsibschowillawithhalfsibs <- filter(Chowilla_fullsibsalldf, V1 %in% Chowillahhalfsibsall)
 unique(fullsibschowillawithhalfsibs$V1)
 
 
 
 
 
 # Table 3. half and fulls done now
 # Need to do all reaches 
 
 # all reaches 
 
 # Offspring with full siblings all reaches
  all_fullsibsunique <- unique(c(combined_fullsib$OffspringID1,combined_fullsib$OffspringID2))
  unique(c(combined_fullsib$OffspringID1,combined_fullsib$OffspringID2))
 
 # Offspring with half siblings all reaches 
all_halfsibsunique <- unique(c(combined_halfsib$OffspringID1,combined_halfsib$OffspringID2))
 unique(c(combined_halfsib$OffspringID1,combined_halfsib$OffspringID2))
 
 # To figure out inds that have both. Simply subset the group of either FS or HS using dplyr filter to
 # Only contain unique inds also found in the other group (subset FS by HS or subset HS by FS)
 all_halfsibsunique 
 all_fullsibsuniquedf <- as.data.frame(matrix(nrow=length(all_fullsibsunique),ncol=1))
 all_fullsibsuniquedf$V1 <- all_fullsibsunique
 allfullsibswithhalfsibs <- filter(all_fullsibsuniquedf, V1 %in% all_halfsibsunique)
 unique(allfullsibswithhalfsibs$V1)
 
 
 # 
 
# To do table 4. will need to effectively subset into single years
 
 
# Starting table 4. Summary of full-sibling and half-sibling relationships
 # Half sibs. Half sibs were generated based on more complex filtering becuase relationships did cross locations
 # See above for details
 Goulburnhalfsibsallfulldata
 Barmahhalfsibsallfulldata
 Chowillahalfsibsallfulldata
 
 
 # Full sibs. This wasn't required for Full sibs because they were all within the same location
 combined_fullsib_goulburn
 combined_fullsib_barmah
 combined_fullsib_chowilla
 



#Goulburn
 # QUick background calculations for supp material breaking down FS/HS relationships
length(unique(c(combined_fullsib_goulburn$OffspringID1,combined_fullsib_goulburn$OffspringID2)))
#111 offspring sequenced from goulburn had at least one full sibling out of 351 sequenced
# Percentage of goulburn offspring with at least 1 full sibling 31.6%

#Sum of all full sibs that are from the same river and the same week and the same site across all larvae
sum(combined_fullsib$river1==combined_fullsib$river2 & combined_fullsib$week1==combined_fullsib$week2 & combined_fullsib$site1==combined_fullsib$site2)

# 167 are from the same river, site and week. Therefore 66% of all larvae are FS potentially from sampling error


#85 full sib pairs are from Barmah
sum(combined_fullsib$river1=="B")
sum(combined_fullsib$river1==combined_fullsib$river2 & combined_fullsib$week1==combined_fullsib$week2 & combined_fullsib$site1==combined_fullsib$site2 & combined_fullsib$river1=="B")

#58 are from the same site + river+week. therefore 68% of FS pairs are potentially sampling error



#90 full sib pairs are from Goulburn
sum(combined_fullsib$river1=="G")
sum(combined_fullsib$river1==combined_fullsib$river2)
sum(combined_fullsib$river1==combined_fullsib$river2 & combined_fullsib$week1==combined_fullsib$week2)
sum(combined_fullsib$river1==combined_fullsib$river2 & combined_fullsib$week1==combined_fullsib$week2 & combined_fullsib$site1==combined_fullsib$site2 & combined_fullsib$river1=="G")
#77 are from the same site + river + week. Therefore 84% of FS pairs are potentially sampling error


#77 full sib pairs are from chowilla
sum(combined_fullsib$river1=="C")
sum(combined_fullsib$river1==combined_fullsib$river2 & combined_fullsib$week1==combined_fullsib$week2 & combined_fullsib$site1==combined_fullsib$site2 & combined_fullsib$river1=="C")
#32 full siblings are from the same site + river + week. Therefore 42% of FS pairs are potentially sampling error

# Lets subset this data to see what the patterns are for between sites/ years only
# This is alll sites.

#FS info for Table 4
# 2018 is so much more than all the others. What happened then?
# Table 4 all reaches
sum((combined_fullsib$year1 == combined_fullsib$year2) & combined_fullsib$year1 ==14 )

sum((combined_fullsib$year1 == combined_fullsib$year2) & combined_fullsib$year1 ==15 )
sum((combined_fullsib$year1 == combined_fullsib$year2) & combined_fullsib$year1 ==16 )
sum((combined_fullsib$year1 == combined_fullsib$year2) & combined_fullsib$year1 ==17 )
sum((combined_fullsib$year1 == combined_fullsib$year2) & combined_fullsib$year1 ==18 )

# (FS pairings in the same year)
monogamous_pairings_2014 <- subset(combined_fullsib, ((combined_fullsib$year1 == combined_fullsib$year2) & combined_fullsib$year1 ==14 ))
monogamous_pairings_2015 <- subset(combined_fullsib, ((combined_fullsib$year1 == combined_fullsib$year2) & combined_fullsib$year1 ==15 ))
monogamous_pairings_2016 <- subset(combined_fullsib, ((combined_fullsib$year1 == combined_fullsib$year2) & combined_fullsib$year1 ==16 ))
monogamous_pairings_2017 <- subset(combined_fullsib, ((combined_fullsib$year1 == combined_fullsib$year2) & combined_fullsib$year1 ==17 ))
monogamous_pairings_2018 <- subset(combined_fullsib, ((combined_fullsib$year1 == combined_fullsib$year2) & combined_fullsib$year1 ==18 ))

# Number of offspring from 2014 with at least 1 full sibling
length(unique(c(monogamous_pairings_2014$OffspringID1,monogamous_pairings_2014$OffspringID2)))
# 26
# Number of offspring from 2014 with at least 1 full sibling
length(unique(c(monogamous_pairings_2015$OffspringID1,monogamous_pairings_2015$OffspringID2)))
# 21
# Number of offspring from 2014 with at least 1 full sibling
length(unique(c(monogamous_pairings_2016$OffspringID1,monogamous_pairings_2016$OffspringID2)))
# 14 
# Number of offspring from 2014 with at least 1 full sibling
length(unique(c(monogamous_pairings_2017$OffspringID1,monogamous_pairings_2017$OffspringID2)))
# 41
# Number of offspring from 2014 with at least 1 full sibling
length(unique(c(monogamous_pairings_2018$OffspringID1,monogamous_pairings_2018$OffspringID2)))
# 91

# 565 total inds sequenced 
# for 2017 158 inds total 
41/158
# 25.9% FS inds
#205 inds for 2018 total
91/205



# HS combined all reaches info for table 4. 
combined_halfsib
Goulburnhalfsibsallfulldata
Barmahhalfsibsallfulldata
Chowillahalfsibsallfulldata

# (HS from the same season)
polygamous_pairings_2014 <- subset(combined_halfsib, ((combined_halfsib$year1 == combined_halfsib$year2) & combined_halfsib$year1 ==14 ))
polygamous_pairings_2015 <- subset(combined_halfsib, ((combined_halfsib$year1 == combined_halfsib$year2) & combined_halfsib$year1 ==15 ))
polygamous_pairings_2016 <- subset(combined_halfsib, ((combined_halfsib$year1 == combined_halfsib$year2) & combined_halfsib$year1 ==16 ))
polygamous_pairings_2017 <- subset(combined_halfsib, ((combined_halfsib$year1 == combined_halfsib$year2) & combined_halfsib$year1 ==17 ))
polygamous_pairings_2018 <- subset(combined_halfsib, ((combined_halfsib$year1 == combined_halfsib$year2) & combined_halfsib$year1 ==18 ))



# Number of offspring from 2014 with at least 1 half sibling
length(unique(c(polygamous_pairings_2014$OffspringID1,polygamous_pairings_2014$OffspringID2)))
# 12
# Number of offspring from 2015 with at least 1 full sibling
length(unique(c(polygamous_pairings_2015$OffspringID1,polygamous_pairings_2015$OffspringID2)))
# 22
# Number of offspring from 2016 with at least 1 full sibling
length(unique(c(polygamous_pairings_2016$OffspringID1,polygamous_pairings_2016$OffspringID2)))
# 4 
# Number of offspring from 2017 with at least 1 full sibling
length(unique(c(polygamous_pairings_2017$OffspringID1,polygamous_pairings_2017$OffspringID2)))
# 30
# Number of offspring from 2018 with at least 1 full sibling
length(unique(c(polygamous_pairings_2018$OffspringID1,polygamous_pairings_2018$OffspringID2)))
# 68


# 565 total inds sequenced 
# for 2017 158 inds total 
30/158
# 18.9% HS inds
#205 inds for 2018 total
68/205

# Table 4. combined reaches completed


# Table 4 reach specific FS and HS (goulburn, barmah, and CHowilla individually)

#FS first


combined_fullsib_goulburn
combined_fullsib_barmah
combined_fullsib_chowilla



# (FS pairings in the same year)
monogamous_pairings_2014Goulburn <- subset(combined_fullsib_goulburn, ((combined_fullsib_goulburn$year1 == combined_fullsib_goulburn$year2) & combined_fullsib_goulburn$year1 ==14 ))
monogamous_pairings_2015Goulburn <- subset(combined_fullsib_goulburn, ((combined_fullsib_goulburn$year1 == combined_fullsib_goulburn$year2) & combined_fullsib_goulburn$year1 ==15 ))
monogamous_pairings_2016Goulburn <- subset(combined_fullsib_goulburn, ((combined_fullsib_goulburn$year1 == combined_fullsib_goulburn$year2) & combined_fullsib_goulburn$year1 ==16 ))
monogamous_pairings_2017Goulburn <- subset(combined_fullsib_goulburn, ((combined_fullsib_goulburn$year1 == combined_fullsib_goulburn$year2) & combined_fullsib_goulburn$year1 ==17 ))
monogamous_pairings_2018Goulburn <- subset(combined_fullsib_goulburn, ((combined_fullsib_goulburn$year1 == combined_fullsib_goulburn$year2) & combined_fullsib_goulburn$year1 ==18 ))

# Number of offspring from 2014 with at least 1 full sibling
length(unique(c(monogamous_pairings_2014Goulburn$OffspringID1,monogamous_pairings_2014Goulburn$OffspringID2)))
# 26
# Number of offspring from 2015 with at least 1 full sibling
length(unique(c(monogamous_pairings_2015Goulburn$OffspringID1,monogamous_pairings_2015Goulburn$OffspringID2)))
# 21
# Number of offspring from 2016 with at least 1 full sibling
length(unique(c(monogamous_pairings_2016Goulburn$OffspringID1,monogamous_pairings_2016Goulburn$OffspringID2)))
# 14 
# Number of offspring from 2017 with at least 1 full sibling
length(unique(c(monogamous_pairings_2017Goulburn$OffspringID1,monogamous_pairings_2017Goulburn$OffspringID2)))
# 18
# Number of offspring from 2018 with at least 1 full sibling
length(unique(c(monogamous_pairings_2018Goulburn$OffspringID1,monogamous_pairings_2018Goulburn$OffspringID2)))
# 30

# totals of sequenced inds
# 2014 = 66 
# 2015 = 72 
# 2016 = 74
# 2017 = 66 
# 2018 = 83

# 2014
26/66
# 2015
21/72
# 2016 
14/74
# 2017 
18/66
# 2018 
30/83

# Barmah FS yearly counts table 4
combined_fullsib_barmah
monogamous_pairings_2017Barmah <- subset(combined_fullsib_barmah, ((combined_fullsib_barmah$year1 == combined_fullsib_barmah$year2) & combined_fullsib_barmah$year1 ==17 ))
monogamous_pairings_2018Barmah <- subset(combined_fullsib_barmah, ((combined_fullsib_barmah$year1 == combined_fullsib_barmah$year2) & combined_fullsib_barmah$year1 ==18 ))

# Number of offspring from 2017 with at least 1 full sibling
length(unique(c(monogamous_pairings_2017Barmah$OffspringID1,monogamous_pairings_2017Barmah$OffspringID2)))
# 11
# Number of offspring from 2018 with at least 1 full sibling
length(unique(c(monogamous_pairings_2018Barmah$OffspringID1,monogamous_pairings_2018Barmah$OffspringID2)))
# 34

# Total sequenced inds
# 2017 = 61
# 2018 = 66

# 2017 
11/61
# 2018 
34/66

# Chowilla FS yearly counts table 4
combined_fullsib_chowilla
monogamous_pairings_2017Chowilla <- subset(combined_fullsib_chowilla, ((combined_fullsib_chowilla$year1 == combined_fullsib_chowilla$year2) & combined_fullsib_chowilla$year1 ==17 ))
monogamous_pairings_2018Chowilla <- subset(combined_fullsib_chowilla, ((combined_fullsib_chowilla$year1 == combined_fullsib_chowilla$year2) & combined_fullsib_chowilla$year1 ==18 ))

# Number of offspring from 2017 with at least 1 full sibling
length(unique(c(monogamous_pairings_2017Chowilla$OffspringID1,monogamous_pairings_2017Chowilla$OffspringID2)))
# 12
# Number of offspring from 2018 with at least 1 full sibling
length(unique(c(monogamous_pairings_2018Chowilla$OffspringID1,monogamous_pairings_2018Chowilla$OffspringID2)))
# 27

# Total sequenced inds
# 2017 = 31
# 2018 = 56

# 2017 
12/31
# 2018 
27/56




# Now HS calls 

Goulburnhalfsibsallfulldata
Barmahhalfsibsallfulldata
Chowillahalfsibsallfulldata

# Starting with Goulburn 

polygamous_pairings_2014Goulburn <- subset(Goulburnhalfsibsallfulldata, ((Goulburnhalfsibsallfulldata$year1 == Goulburnhalfsibsallfulldata$year2) & Goulburnhalfsibsallfulldata$year1 ==14 ))
polygamous_pairings_2015Goulburn <- subset(Goulburnhalfsibsallfulldata, ((Goulburnhalfsibsallfulldata$year1 == Goulburnhalfsibsallfulldata$year2) & Goulburnhalfsibsallfulldata$year1 ==15 ))
polygamous_pairings_2016Goulburn <- subset(Goulburnhalfsibsallfulldata, ((Goulburnhalfsibsallfulldata$year1 == Goulburnhalfsibsallfulldata$year2) & Goulburnhalfsibsallfulldata$year1 ==16 ))
polygamous_pairings_2017Goulburn <- subset(Goulburnhalfsibsallfulldata, ((Goulburnhalfsibsallfulldata$year1 == Goulburnhalfsibsallfulldata$year2) & Goulburnhalfsibsallfulldata$year1 ==17 ))
polygamous_pairings_2018Goulburn <- subset(Goulburnhalfsibsallfulldata, ((Goulburnhalfsibsallfulldata$year1 == Goulburnhalfsibsallfulldata$year2) & Goulburnhalfsibsallfulldata$year1 ==18 ))

# Number of offspring from 2014 with at least 1 half sibling
length(unique(c(polygamous_pairings_2014Goulburn$OffspringID1,polygamous_pairings_2014Goulburn$OffspringID2)))
# 12
# Number of offspring from 2015 with at least 1 half sibling
length(unique(c(polygamous_pairings_2015Goulburn$OffspringID1,polygamous_pairings_2015Goulburn$OffspringID2)))
# 22
# Number of offspring from 2016 with at least 1 half sibling
length(unique(c(polygamous_pairings_2016Goulburn$OffspringID1,polygamous_pairings_2016Goulburn$OffspringID2)))
# 4 
# Number of offspring from 2017 with at least 1 half sibling
length(unique(c(polygamous_pairings_2017Goulburn$OffspringID1,polygamous_pairings_2017Goulburn$OffspringID2)))
# 12
# Number of offspring from 2018 with at least 1 half sibling
length(unique(c(polygamous_pairings_2018Goulburn$OffspringID1,polygamous_pairings_2018Goulburn$OffspringID2)))
# 25

# totals of sequenced inds
# 2014 = 66 
# 2015 = 72 
# 2016 = 74
# 2017 = 66 
# 2018 = 83
# 2014
12/66
# 2015
22/72
# 2016 
4/74
# 2017 
12/66
# 2018 
25/83


# Barmah HS yearly counts table 4
Barmahhalfsibsallfulldata
polygamous_pairings_2017Barmah <- subset(Barmahhalfsibsallfulldata, ((Barmahhalfsibsallfulldata$year1 == Barmahhalfsibsallfulldata$year2) & Barmahhalfsibsallfulldata$year1 ==17 ))
polygamous_pairings_2018Barmah <- subset(Barmahhalfsibsallfulldata, ((Barmahhalfsibsallfulldata$year1 == Barmahhalfsibsallfulldata$year2) & Barmahhalfsibsallfulldata$year1 ==18 ))

# Number of offspring from 2017 with at least 1 half sibling
length(unique(c(polygamous_pairings_2017Barmah$OffspringID1,polygamous_pairings_2017Barmah$OffspringID2)))
# 10
# Number of offspring from 2018 with at least 1 half sibling
length(unique(c(polygamous_pairings_2018Barmah$OffspringID1,polygamous_pairings_2018Barmah$OffspringID2)))
# 20

# Total sequenced inds
# 2017 = 61
# 2018 = 66

# 2017 
10/61
# 2018 
20/66

# Chowilla HS yearly counts table 4
Chowillahalfsibsallfulldata
polygamous_pairings_2017Chowilla <- subset(Chowillahalfsibsallfulldata, ((Chowillahalfsibsallfulldata$year1 == Chowillahalfsibsallfulldata$year2) & Chowillahalfsibsallfulldata$year1 ==17 ))
polygamous_pairings_2018Chowilla <- subset(Chowillahalfsibsallfulldata, ((Chowillahalfsibsallfulldata$year1 == Chowillahalfsibsallfulldata$year2) & Chowillahalfsibsallfulldata$year1 ==18 ))

# Number of offspring from 2017 with at least 1 half sibling
length(unique(c(polygamous_pairings_2017Chowilla$OffspringID1,polygamous_pairings_2017Chowilla$OffspringID2)))
# 8
# Number of offspring from 2018 with at least 1 half sibling
length(unique(c(polygamous_pairings_2018Chowilla$OffspringID1,polygamous_pairings_2018Chowilla$OffspringID2)))
# 23

# Total sequenced inds
# 2017 = 31
# 2018 = 56

# 2017 
8/31
# 2018 
23/56



# completed table 4. All sib sib based FS and HS relationships defined and counted


# To do Tables 5 and 6 will need to target Best cluster file from 
# 



#For number of adults in monogomous pairs per year, unique full sib
# dyads * 2 as there are 2 parents 




vector2014 <- cbind((c(as.character(monogamous_pairings_2014$OffspringID1), as.character(monogamous_pairings_2014$OffspringID2))))
vector2014 <- as.vector(vector2014)
length(unique(vector2014))
table(vector2014)
hist(table(vector2014),breaks= c(1,2,3,4,5) )



length(unique(monogamous_pairings_2015$OffspringID1))
length(unique(monogamous_pairings_2015$OffspringID2))

#FS info
nrow(subset(monogamous_pairings, monogamous_pairings$year1=="14"))
nrow(subset(monogamous_pairings, monogamous_pairings$year1=="15"))
nrow(subset(monogamous_pairings, monogamous_pairings$year1=="16"))
nrow(subset(monogamous_pairings, monogamous_pairings$year1=="17"))
nrow(subset(monogamous_pairings, monogamous_pairings$year1=="18"))

monogamous_pairings_2 <- matrix(nrow=5, ncol=2)

monogamous_pairings_2[1,2] <- as.numeric(nrow(subset(monogamous_pairings, monogamous_pairings$year1=="14")))
monogamous_pairings_2[2,2] <- as.numeric(nrow(subset(monogamous_pairings, monogamous_pairings$year1=="15")))
monogamous_pairings_2[3,2] <- as.numeric(nrow(subset(monogamous_pairings, monogamous_pairings$year1=="16")))
monogamous_pairings_2[4,2] <- as.numeric(nrow(subset(monogamous_pairings, monogamous_pairings$year1=="17")))
monogamous_pairings_2[5,2] <- as.numeric(nrow(subset(monogamous_pairings, monogamous_pairings$year1=="18")))

monogamous_pairings_2[1,1] <- "2014"
monogamous_pairings_2[2,1] <- "2015"
monogamous_pairings_2[3,1] <- "2016"
monogamous_pairings_2[4,1] <- "2017"
monogamous_pairings_2[5,1] <- "2018"


#+ geom_bar(stat="identity")

monogamous_pairings_2 <- as.data.frame(monogamous_pairings_2)
colnames(monogamous_pairings_2) <- c("year", "fullsibling_pairs")
monogamous_pairings_2$year <- as.factor(monogamous_pairings_2$year) 


tiff("fullsib_pairs_per_year_Goulburn.tif",units='cm', width =14,height=14, res=400)
ggplot(monogamous_pairings_2, aes(year, fullsibling_pairs)) + geom_bar(stat="identity") +xlab("Year") + ylab("Number of full sibling pairs") + theme_bw()
dev.off()


#FS info

monogamous_pairings$sib <- 'FS'

vector1 <- as.character(monogamous_pairings$OffspringID1)
vector2 <- as.character(monogamous_pairings$OffspringID2)
vector3 <- c(vector1,vector2)

length(vector1)
length(vector2)
length(vector3)

unique(vector3)
vector4 <- unique(vector3)
length(vector4)
unique_monogamous_pairings <- as.data.frame(vector4, nrow=length(vector4), ncol=1)

colnames(unique_monogamous_pairings) <- "Offspring"

for (i in c(1:nrow(unique_monogamous_pairings))) {
  
  # substring to year 
  sub1  <- substr(unique_monogamous_pairings$Offspring[i], 3,4)
  
  # substring to river 
  sub2  <- substr(unique_monogamous_pairings$Offspring[i], 5,5)
  
  
  #substring to site
  sub3  <- substr(unique_monogamous_pairings$Offspring[i], 6,6)
  
  
  #substring to week.
  sub4 <- substr(unique_monogamous_pairings$Offspring[i], 10,10)

  
  #substring to length
  sub5 <- substr(unique_monogamous_pairings$Offspring[i], 11,16)
  
  
  
  
  unique_monogamous_pairings$year1[i] <- sub1
  
  unique_monogamous_pairings$river1[i] <- sub2
  
  unique_monogamous_pairings$site1[i] <- sub3
  
  unique_monogamous_pairings$week1[i] <- sub4
  
  unique_monogamous_pairings$length1[i] <- sub5
  
}


unique_monogamous_pairings$length1 <- gsub("L", "",unique_monogamous_pairings$length1, fixed = TRUE)









# Lets subset them into river by river dempographics 

fullsib_table_no_sample_error_goulburn <- subset(combined_fullsib_no_sample_error, combined_fullsib_no_sample_error$river1=="G")

fullsib_table_no_sample_error_barmah <- subset(combined_fullsib_no_sample_error, combined_fullsib_no_sample_error$river1=="B")

fullsib_table_no_sample_error_chowilla <- subset(combined_fullsib_no_sample_error, combined_fullsib_no_sample_error$river1=="C")

fullsib_table_no_sample_error_goulburn

#Goulburn first

# FS info

monogamous_pairings_goulburn <- subset(fullsib_table_no_sample_error_goulburn, (fullsib_table_no_sample_error_goulburn$year1 == fullsib_table_no_sample_error_goulburn$year2))

vectorall<- cbind((c(as.character(monogamous_pairings_goulburn$OffspringID1), as.character(monogamous_pairings_goulburn$OffspringID2))))
vectorall <- as.vector(vectorall)
vectorall <- as.factor(vectorall)
length(unique(vectorall))
length(vectorall)
table(vectorall)
hist(table(vectorall),breaks= c(0,1,2,3,4,5) )

#FS info

monogamous_pairings_2014 <- subset(fullsib_table_no_sample_error_goulburn, ((fullsib_table_no_sample_error_goulburn$year1 == Colony_fullsib_table_no_sample_error_goulburn$year2) & Colony_fullsib_table_no_sample_error_goulburn$year1 ==14 ))
monogamous_pairings_2015 <- subset(fullsib_table_no_sample_error_goulburn, ((fullsib_table_no_sample_error_goulburn$year1 == Colony_fullsib_table_no_sample_error_goulburn$year2) & Colony_fullsib_table_no_sample_error_goulburn$year1 ==15 ))
monogamous_pairings_2016 <- subset(fullsib_table_no_sample_error_goulburn, ((fullsib_table_no_sample_error_goulburn$year1 == Colony_fullsib_table_no_sample_error_goulburn$year2) & Colony_fullsib_table_no_sample_error_goulburn$year1 ==16 ))
monogamous_pairings_2017 <- subset(fullsib_table_no_sample_error_goulburn, ((fullsib_table_no_sample_error_goulburn$year1 == Colony_fullsib_table_no_sample_error_goulburn$year2) & Colony_fullsib_table_no_sample_error_goulburn$year1 ==17 ))
monogamous_pairings_2018 <- subset(fullsib_table_no_sample_error_goulburn, ((fullsib_table_no_sample_error_goulburn$year1 == Colony_fullsib_table_no_sample_error_goulburn$year2) & Colony_fullsib_table_no_sample_error_goulburn$year1 ==18 ))


nrow(subset(monogamous_pairings_goulburn, monogamous_pairings_goulburn$year1=="14"))
nrow(subset(monogamous_pairings_goulburn, monogamous_pairings_goulburn$year1=="15"))
nrow(subset(monogamous_pairings_goulburn, monogamous_pairings_goulburn$year1=="16"))
nrow(subset(monogamous_pairings_goulburn, monogamous_pairings_goulburn$year1=="17"))
nrow(subset(monogamous_pairings_goulburn, monogamous_pairings_goulburn$year1=="18"))

monogamous_pairings_2 <- matrix(nrow=5, ncol=2)

monogamous_pairings_2[1,2] <- as.numeric(nrow(subset(monogamous_pairings_goulburn, monogamous_pairings_goulburn$year1=="14")))
monogamous_pairings_2[2,2] <- as.numeric(nrow(subset(monogamous_pairings_goulburn, monogamous_pairings_goulburn$year1=="15")))
monogamous_pairings_2[3,2] <- as.numeric(nrow(subset(monogamous_pairings_goulburn, monogamous_pairings_goulburn$year1=="16")))
monogamous_pairings_2[4,2] <- as.numeric(nrow(subset(monogamous_pairings_goulburn, monogamous_pairings_goulburn$year1=="17")))
monogamous_pairings_2[5,2] <- as.numeric(nrow(subset(monogamous_pairings_goulburn, monogamous_pairings_goulburn$year1=="18")))

monogamous_pairings_2[1,1] <- "2014"
monogamous_pairings_2[2,1] <- "2015"
monogamous_pairings_2[3,1] <- "2016"
monogamous_pairings_2[4,1] <- "2017"
monogamous_pairings_2[5,1] <- "2018"

monogamous_pairings_2goulburn <- monogamous_pairings_2

#### Now Barmah


monogamous_pairings_barmah <- subset(fullsib_table_no_sample_error_barmah, (fullsib_table_no_sample_error_barmah$year1 == fullsib_table_no_sample_error_barmah$year2))

vectorall<- cbind((c(as.character(fullsib_table_no_sample_error_barmah$OffspringID1), as.character(fullsib_table_no_sample_error_barmah$OffspringID2))))
vectorall <- as.vector(vectorall)
vectorall <- as.factor(vectorall)
length(unique(vectorall))
length(vectorall)
table(vectorall)
hist(table(vectorall),breaks= c(0,1,2,3,4,5) )



monogamous_pairings_2014 <- subset(fullsib_table_no_sample_error_barmah, ((fullsib_table_no_sample_error_barmah$year1 == fullsib_table_no_sample_error_barmah$year2) & fullsib_table_no_sample_error_barmah$year1 ==14 ))
monogamous_pairings_2015 <- subset(fullsib_table_no_sample_error_barmah, ((fullsib_table_no_sample_error_barmah$year1 == fullsib_table_no_sample_error_barmah$year2) & fullsib_table_no_sample_error_barmah$year1 ==15 ))
monogamous_pairings_2016 <- subset(fullsib_table_no_sample_error_barmah, ((fullsib_table_no_sample_error_barmah$year1 == fullsib_table_no_sample_error_barmah$year2) & fullsib_table_no_sample_error_barmah$year1 ==16 ))
monogamous_pairings_2017 <- subset(fullsib_table_no_sample_error_barmah, ((fullsib_table_no_sample_error_barmah$year1 == fullsib_table_no_sample_error_barmah$year2) & fullsib_table_no_sample_error_barmah$year1 ==17 ))
monogamous_pairings_2018 <- subset(fullsib_table_no_sample_error_barmah, ((fullsib_table_no_sample_error_barmah$year1 == fullsib_table_no_sample_error_barmah$year2) & fullsib_table_no_sample_error_barmah$year1 ==18 ))



#FS info 

nrow(subset(monogamous_pairings_barmah, monogamous_pairings_barmah$year1=="14"))
nrow(subset(monogamous_pairings_barmah, monogamous_pairings_barmah$year1=="15"))
nrow(subset(monogamous_pairings_barmah, monogamous_pairings_barmah$year1=="16"))
nrow(subset(monogamous_pairings_barmah, monogamous_pairings_barmah$year1=="17"))
nrow(subset(monogamous_pairings_barmah, monogamous_pairings_barmah$year1=="18"))

monogamous_pairings_2 <- matrix(nrow=5, ncol=2)

monogamous_pairings_2[1,2] <- as.numeric(nrow(subset(monogamous_pairings_barmah, monogamous_pairings_barmah$year1=="14")))
monogamous_pairings_2[2,2] <- as.numeric(nrow(subset(monogamous_pairings_barmah, monogamous_pairings_barmah$year1=="15")))
monogamous_pairings_2[3,2] <- as.numeric(nrow(subset(monogamous_pairings_barmah, monogamous_pairings_barmah$year1=="16")))
monogamous_pairings_2[4,2] <- as.numeric(nrow(subset(monogamous_pairings_barmah, monogamous_pairings_barmah$year1=="17")))
monogamous_pairings_2[5,2] <- as.numeric(nrow(subset(monogamous_pairings_barmah, monogamous_pairings_barmah$year1=="18")))

monogamous_pairings_2[1,1] <- "2014"
monogamous_pairings_2[2,1] <- "2015"
monogamous_pairings_2[3,1] <- "2016"
monogamous_pairings_2[4,1] <- "2017"
monogamous_pairings_2[5,1] <- "2018"


monogamous_pairings_2barmah <- monogamous_pairings_2


### Now Chowilla



monogamous_pairings_chowilla <- subset(fullsib_table_no_sample_error_chowilla, (fullsib_table_no_sample_error_chowilla$year1 == fullsib_table_no_sample_error_chowilla$year2))

vectorall<- cbind((c(as.character(fullsib_table_no_sample_error_chowilla$OffspringID1), as.character(fullsib_table_no_sample_error_chowilla$OffspringID2))))
vectorall <- as.vector(vectorall)
vectorall <- as.factor(vectorall)
length(unique(vectorall))
length(vectorall)
table(vectorall)
hist(table(vectorall),breaks= c(0,1,2,3,4,5,6,7,8,9) )



monogamous_pairings_2014 <- subset(fullsib_table_no_sample_error_chowilla, ((fullsib_table_no_sample_error_chowilla$year1 == fullsib_table_no_sample_error_chowilla$year2) & fullsib_table_no_sample_error_chowilla$year1 ==14 ))
monogamous_pairings_2015 <- subset(fullsib_table_no_sample_error_chowilla, ((fullsib_table_no_sample_error_chowilla$year1 == fullsib_table_no_sample_error_chowilla$year2) & fullsib_table_no_sample_error_chowilla$year1 ==15 ))
monogamous_pairings_2016 <- subset(fullsib_table_no_sample_error_chowilla, ((fullsib_table_no_sample_error_chowilla$year1 == fullsib_table_no_sample_error_chowilla$year2) & fullsib_table_no_sample_error_chowilla$year1 ==16 ))
monogamous_pairings_2017 <- subset(fullsib_table_no_sample_error_chowilla, ((fullsib_table_no_sample_error_chowilla$year1 == fullsib_table_no_sample_error_chowilla$year2) & fullsib_table_no_sample_error_chowilla$year1 ==17 ))
monogamous_pairings_2018 <- subset(fullsib_table_no_sample_error_chowilla, ((fullsib_table_no_sample_error_chowilla$year1 == fullsib_table_no_sample_error_chowilla$year2) & fullsib_table_no_sample_error_chowilla$year1 ==18 ))




nrow(subset(monogamous_pairings_chowilla, monogamous_pairings_chowilla$year1=="14"))
nrow(subset(monogamous_pairings_chowilla, monogamous_pairings_chowilla$year1=="15"))
nrow(subset(monogamous_pairings_chowilla, monogamous_pairings_chowilla$year1=="16"))
nrow(subset(monogamous_pairings_chowilla, monogamous_pairings_chowilla$year1=="17"))
nrow(subset(monogamous_pairings_chowilla, monogamous_pairings_chowilla$year1=="18"))

monogamous_pairings_2 <- matrix(nrow=5, ncol=2)

monogamous_pairings_2[1,2] <- as.numeric(nrow(subset(monogamous_pairings_chowilla, monogamous_pairings_chowilla$year1=="14")))
monogamous_pairings_2[2,2] <- as.numeric(nrow(subset(monogamous_pairings_chowilla, monogamous_pairings_chowilla$year1=="15")))
monogamous_pairings_2[3,2] <- as.numeric(nrow(subset(monogamous_pairings_chowilla, monogamous_pairings_chowilla$year1=="16")))
monogamous_pairings_2[4,2] <- as.numeric(nrow(subset(monogamous_pairings_chowilla, monogamous_pairings_chowilla$year1=="17")))
monogamous_pairings_2[5,2] <- as.numeric(nrow(subset(monogamous_pairings_chowilla, monogamous_pairings_chowilla$year1=="18")))

monogamous_pairings_2[1,1] <- "2014"
monogamous_pairings_2[2,1] <- "2015"
monogamous_pairings_2[3,1] <- "2016"
monogamous_pairings_2[4,1] <- "2017"
monogamous_pairings_2[5,1] <- "2018"

# renamed them all so the years and FS pairs for error free are here
monogamous_pairings_2chowilla <- monogamous_pairings_2
monogamous_pairings_2barmah_FSreduced     <-monogamous_pairings_2barmah
monogamous_pairings_2goulburnFSreduced    <-monogamous_pairings_2goulburn
monogamous_pairings_2chowillaFSreduced    <-monogamous_pairings_2chowilla

# Let's try with all data

combined_fullsib_goulburn  <- subset(combined_fullsib, combined_fullsib$river1=="G")
combined_fullsib_barmah  <- subset(combined_fullsib, combined_fullsib$river1=="B")
combined_fullsib_chowilla  <- subset(combined_fullsib, combined_fullsib$river1=="C")


#Goulburn first

monogamous_pairings_goulburn <- subset(combined_fullsib_goulburn, (combined_fullsib_goulburn$year1 == combined_fullsib_goulburn$year2))

vectorall<- cbind((c(as.character(monogamous_pairings_goulburn$OffspringID1), as.character(monogamous_pairings_goulburn$OffspringID2))))
vectorall <- as.vector(vectorall)
vectorall <- as.factor(vectorall)
length(unique(vectorall))
length(vectorall)
table(vectorall)
hist(table(vectorall),breaks= c(0,1,2,3,4,5) )



monogamous_pairings_2014 <- subset(combined_fullsib_goulburn, ((combined_fullsib_goulburn$year1 == combined_fullsib_goulburn$year2) & combined_fullsib_goulburn$year1 ==14 ))
monogamous_pairings_2015 <- subset(combined_fullsib_goulburn, ((combined_fullsib_goulburn$year1 == combined_fullsib_goulburn$year2) & combined_fullsib_goulburn$year1 ==15 ))
monogamous_pairings_2016 <- subset(combined_fullsib_goulburn, ((combined_fullsib_goulburn$year1 == combined_fullsib_goulburn$year2) & combined_fullsib_goulburn$year1 ==16 ))
monogamous_pairings_2017 <- subset(combined_fullsib_goulburn, ((combined_fullsib_goulburn$year1 == combined_fullsib_goulburn$year2) & combined_fullsib_goulburn$year1 ==17 ))
monogamous_pairings_2018 <- subset(combined_fullsib_goulburn, ((combined_fullsib_goulburn$year1 == combined_fullsib_goulburn$year2) & combined_fullsib_goulburn$year1 ==18 ))


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




# FS info
nrow(subset(monogamous_pairings_goulburn, monogamous_pairings_goulburn$year1=="14"))
nrow(subset(monogamous_pairings_goulburn, monogamous_pairings_goulburn$year1=="15"))
nrow(subset(monogamous_pairings_goulburn, monogamous_pairings_goulburn$year1=="16"))
nrow(subset(monogamous_pairings_goulburn, monogamous_pairings_goulburn$year1=="17"))
nrow(subset(monogamous_pairings_goulburn, monogamous_pairings_goulburn$year1=="18"))

monogamous_pairings_2 <- matrix(nrow=5, ncol=2)

monogamous_pairings_2[1,2] <- as.numeric(nrow(subset(monogamous_pairings_goulburn, monogamous_pairings_goulburn$year1=="14")))
monogamous_pairings_2[2,2] <- as.numeric(nrow(subset(monogamous_pairings_goulburn, monogamous_pairings_goulburn$year1=="15")))
monogamous_pairings_2[3,2] <- as.numeric(nrow(subset(monogamous_pairings_goulburn, monogamous_pairings_goulburn$year1=="16")))
monogamous_pairings_2[4,2] <- as.numeric(nrow(subset(monogamous_pairings_goulburn, monogamous_pairings_goulburn$year1=="17")))
monogamous_pairings_2[5,2] <- as.numeric(nrow(subset(monogamous_pairings_goulburn, monogamous_pairings_goulburn$year1=="18")))

monogamous_pairings_2[1,1] <- "2014"
monogamous_pairings_2[2,1] <- "2015"
monogamous_pairings_2[3,1] <- "2016"
monogamous_pairings_2[4,1] <- "2017"
monogamous_pairings_2[5,1] <- "2018"

monogamous_pairings_2goulburn <- monogamous_pairings_2
monogamous_pairings_2goulburnFSreduced
#### Now Barmah


monogamous_pairings_barmah <- subset(combined_fullsib_barmah, (combined_fullsib_barmah$year1 == combined_fullsib_barmah$year2))

vectorall<- cbind((c(as.character(combined_fullsib_barmah$OffspringID1), as.character(combined_fullsib_barmah$OffspringID2))))
vectorall <- as.vector(vectorall)
vectorall <- as.factor(vectorall)
length(unique(vectorall))
length(vectorall)
table(vectorall)
hist(table(vectorall),breaks= c(0,1,2,3,4,5,6,7,8,9) )



monogamous_pairings_2014 <- subset(combined_fullsib_barmah, ((combined_fullsib_barmah$year1 == combined_fullsib_barmah$year2) & combined_fullsib_barmah$year1 ==14 ))
monogamous_pairings_2015 <- subset(combined_fullsib_barmah, ((combined_fullsib_barmah$year1 == combined_fullsib_barmah$year2) & combined_fullsib_barmah$year1 ==15 ))
monogamous_pairings_2016 <- subset(combined_fullsib_barmah, ((combined_fullsib_barmah$year1 == combined_fullsib_barmah$year2) & combined_fullsib_barmah$year1 ==16 ))
monogamous_pairings_2017 <- subset(combined_fullsib_barmah, ((combined_fullsib_barmah$year1 == combined_fullsib_barmah$year2) & combined_fullsib_barmah$year1 ==17 ))
monogamous_pairings_2018 <- subset(combined_fullsib_barmah, ((combined_fullsib_barmah$year1 == combined_fullsib_barmah$year2) & combined_fullsib_barmah$year1 ==18 ))


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





#FS info
nrow(subset(monogamous_pairings_barmah, monogamous_pairings_barmah$year1=="14"))
nrow(subset(monogamous_pairings_barmah, monogamous_pairings_barmah$year1=="15"))
nrow(subset(monogamous_pairings_barmah, monogamous_pairings_barmah$year1=="16"))
nrow(subset(monogamous_pairings_barmah, monogamous_pairings_barmah$year1=="17"))
nrow(subset(monogamous_pairings_barmah, monogamous_pairings_barmah$year1=="18"))

monogamous_pairings_2 <- matrix(nrow=5, ncol=2)

monogamous_pairings_2[1,2] <- as.numeric(nrow(subset(monogamous_pairings_barmah, monogamous_pairings_barmah$year1=="14")))
monogamous_pairings_2[2,2] <- as.numeric(nrow(subset(monogamous_pairings_barmah, monogamous_pairings_barmah$year1=="15")))
monogamous_pairings_2[3,2] <- as.numeric(nrow(subset(monogamous_pairings_barmah, monogamous_pairings_barmah$year1=="16")))
monogamous_pairings_2[4,2] <- as.numeric(nrow(subset(monogamous_pairings_barmah, monogamous_pairings_barmah$year1=="17")))
monogamous_pairings_2[5,2] <- as.numeric(nrow(subset(monogamous_pairings_barmah, monogamous_pairings_barmah$year1=="18")))

monogamous_pairings_2[1,1] <- "2014"
monogamous_pairings_2[2,1] <- "2015"
monogamous_pairings_2[3,1] <- "2016"
monogamous_pairings_2[4,1] <- "2017"
monogamous_pairings_2[5,1] <- "2018"

#FS info
monogamous_pairings_2barmah <- monogamous_pairings_2


### Now Chowilla



monogamous_pairings_chowilla <- subset(combined_fullsib_chowilla, (combined_fullsib_chowilla$year1 == combined_fullsib_chowilla$year2))

vectorall<- cbind((c(as.character(combined_fullsib_chowilla$OffspringID1), as.character(combined_fullsib_chowilla$OffspringID2))))
vectorall <- as.vector(vectorall)
vectorall <- as.factor(vectorall)
length(unique(vectorall))
length(vectorall)
table(vectorall)
hist(table(vectorall),breaks= c(0,1,2,3,4,5) )



monogamous_pairings_2014 <- subset(combined_fullsib_chowilla, ((combined_fullsib_chowilla$year1 == combined_fullsib_chowilla$year2) & combined_fullsib_chowilla$year1 ==14 ))
monogamous_pairings_2015 <- subset(combined_fullsib_chowilla, ((combined_fullsib_chowilla$year1 == combined_fullsib_chowilla$year2) & combined_fullsib_chowilla$year1 ==15 ))
monogamous_pairings_2016 <- subset(combined_fullsib_chowilla, ((combined_fullsib_chowilla$year1 == combined_fullsib_chowilla$year2) & combined_fullsib_chowilla$year1 ==16 ))
monogamous_pairings_2017 <- subset(combined_fullsib_chowilla, ((combined_fullsib_chowilla$year1 == combined_fullsib_chowilla$year2) & combined_fullsib_chowilla$year1 ==17 ))
monogamous_pairings_2018 <- subset(combined_fullsib_chowilla, ((combined_fullsib_chowilla$year1 == combined_fullsib_chowilla$year2) & combined_fullsib_chowilla$year1 ==18 ))

#FS info 

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



nrow(subset(monogamous_pairings_chowilla, monogamous_pairings_chowilla$year1=="14"))
nrow(subset(monogamous_pairings_chowilla, monogamous_pairings_chowilla$year1=="15"))
nrow(subset(monogamous_pairings_chowilla, monogamous_pairings_chowilla$year1=="16"))
nrow(subset(monogamous_pairings_chowilla, monogamous_pairings_chowilla$year1=="17"))
nrow(subset(monogamous_pairings_chowilla, monogamous_pairings_chowilla$year1=="18"))

monogamous_pairings_2 <- matrix(nrow=5, ncol=2)

monogamous_pairings_2[1,2] <- as.numeric(nrow(subset(monogamous_pairings_chowilla, monogamous_pairings_chowilla$year1=="14")))
monogamous_pairings_2[2,2] <- as.numeric(nrow(subset(monogamous_pairings_chowilla, monogamous_pairings_chowilla$year1=="15")))
monogamous_pairings_2[3,2] <- as.numeric(nrow(subset(monogamous_pairings_chowilla, monogamous_pairings_chowilla$year1=="16")))
monogamous_pairings_2[4,2] <- as.numeric(nrow(subset(monogamous_pairings_chowilla, monogamous_pairings_chowilla$year1=="17")))
monogamous_pairings_2[5,2] <- as.numeric(nrow(subset(monogamous_pairings_chowilla, monogamous_pairings_chowilla$year1=="18")))
# FS info
monogamous_pairings_2[1,1] <- "2014"
monogamous_pairings_2[2,1] <- "2015"
monogamous_pairings_2[3,1] <- "2016"
monogamous_pairings_2[4,1] <- "2017"
monogamous_pairings_2[5,1] <- "2018"

monogamous_pairings_2chowilla <- monogamous_pairings_2

monogamous_pairings_2chowilla <- as.data.frame(monogamous_pairings_2chowilla)
colnames(monogamous_pairings_2chowilla) <- c("year", "fullsibling_pairs")
monogamous_pairings_2chowilla$year <- as.factor(monogamous_pairings_2chowilla$year) 


tiff("fullsib_pairs_per_year_Goulburn.tif",units='cm', width =14,height=14, res=400)
ggplot(monogamous_pairings_2, aes(year, fullsibling_pairs)) + geom_bar(stat="identity") +xlab("Year") + ylab("Number of full sibling pairs") + theme_bw()
dev.off()




### Now we have comprehensive full sibling info we can look into half sibling info


### Starting from reading it in


#HS info




combined_halfsib$OffspringID1 <- gsub(".", "_",combined_halfsib$OffspringID1,fixed = TRUE)
combined_halfsib$OffspringID2 <- gsub(".", "_",combined_halfsib$OffspringID2,fixed = TRUE)


#HS info
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

nrow(combined_halfsib)

sum(combined_halfsib$yeardiff == 0)
# 203 out of 371 half siblings were within the same breeding season. (56% of half sibs)
sum(combined_halfsib$yeardiff == 1)
sum(combined_halfsib$yeardiff == 2)
sum(combined_halfsib$yeardiff == 3)
sum(combined_halfsib$yeardiff == 4)
#HS info
# halfsibling pairs over each year
hist(combined_halfsib$yeardiff,breaks= c(0,1,2,3,4))

sum(combined_halfsib$site1 == combined_halfsib$site2)
#184 pairings or around 50.4% of half sibs were found in the same site


sum((combined_halfsib$site1 == combined_halfsib$site2) & (combined_halfsib$year1 == combined_halfsib$year2) )
# 104 pairings or around 28.4% of half sibs were from the same year and site

sum((combined_halfsib$site1 == combined_halfsib$site2) & (combined_halfsib$year1 == combined_halfsib$year2) & (combined_halfsib$week1==combined_halfsib$week2) )
#80 pairings or around 21.9% of half sibs were from the same year, site and week

sum((combined_halfsib$site1 == combined_halfsib$site2) & (combined_halfsib$year1 == combined_halfsib$year2) & (combined_halfsib$week1==combined_halfsib$week2) & (combined_halfsib$lengthdiff <= 1),na.rm = TRUE )
#68 pairings or around 18.6% of half sibs were from the same year, site and week and had a length difference of less than 1mm 


sum((combined_halfsib$site1 == combined_halfsib$site2) & (combined_halfsib$year1 == combined_halfsib$year2) & (combined_halfsib$week1==combined_halfsib$week2) & (combined_halfsib$lengthdiff >= 2),na.rm = TRUE )
# 3 pairings or 1.0% of half sibs were from the same year, site and week and had a length difference of more than 2mm 
test <- subset(combined_halfsib, (combined_halfsib$site1 == combined_halfsib$site2 & (combined_halfsib$year1 == combined_halfsib$year2) & (combined_halfsib$week1==combined_halfsib$week2) & (combined_halfsib$lengthdiff >= 2)))
rm(test)

sum(combined_halfsib$length1==combined_halfsib$length2,na.rm = TRUE)
hist(combined_halfsib$length2)

sum(combined_halfsib$year2== combined_halfsib$year1 & combined_halfsib$year2==18)

sum((combined_halfsib$year1 == combined_halfsib$year2) & combined_halfsib$year1 ==14 )
sum((combined_halfsib$year1 == combined_halfsib$year2) & combined_halfsib$year1 ==15 )
sum((combined_halfsib$year1 == combined_halfsib$year2) & combined_halfsib$year1 ==16 )
sum((combined_halfsib$year1 == combined_halfsib$year2) & combined_halfsib$year1 ==17 )
sum((combined_halfsib$year1 == combined_halfsib$year2) & combined_halfsib$year1 ==18 )
# how many half sibs from each year (from the same season)
halfsibs_yearbyyear <- matrix(nrow=5,ncol=2)
halfsibs_yearbyyear <- as.data.frame(halfsibs_yearbyyear)
halfsibs_yearbyyear[1,2] <- sum((combined_halfsib$year1 == combined_halfsib$year2) & combined_halfsib$year1 ==14 )
halfsibs_yearbyyear[2,2] <- sum((combined_halfsib$year1 == combined_halfsib$year2) & combined_halfsib$year1 ==15 )
halfsibs_yearbyyear[3,2] <- sum((combined_halfsib$year1 == combined_halfsib$year2) & combined_halfsib$year1 ==16 )
halfsibs_yearbyyear[4,2] <- sum((combined_halfsib$year1 == combined_halfsib$year2) & combined_halfsib$year1 ==17 )
halfsibs_yearbyyear[5,2] <- sum((combined_halfsib$year1 == combined_halfsib$year2) & combined_halfsib$year1 ==18 )
halfsibs_yearbyyear[1,1] <- ("2014")
halfsibs_yearbyyear[2,1] <- ("2015")
halfsibs_yearbyyear[3,1] <- ("2016")
halfsibs_yearbyyear[4,1] <- ("2017")
halfsibs_yearbyyear[5,1] <- ("2018")
halfsibs_yearbyyear[1,2] <- as.numeric(halfsibs_yearbyyear[1,2])
halfsibs_yearbyyear[2,2] <- as.numeric(halfsibs_yearbyyear[2,2])
halfsibs_yearbyyear[3,2] <- as.numeric(halfsibs_yearbyyear[3,2])
halfsibs_yearbyyear[4,2] <- as.numeric(halfsibs_yearbyyear[4,2])
halfsibs_yearbyyear[5,2] <- as.numeric(halfsibs_yearbyyear[5,2])

library(ggplot2)

#graph of half siblings per year
# HS info

tiff("halfsib_pairs_per_year_Goulburn.tif",units='cm', width =14,height=14, res=400)
ggplot(halfsibs_yearbyyear, aes(V1,V2)) + geom_bar(stat="identity") +xlab("Year") + ylab("Number of half sibling pairs") +ylim(0,250) + theme_bw()
dev.off()
# very weird. Almost everything is a half sib in 2018

# Lets analyse separately for each river now
sum(combined_halfsib$river1 != combined_halfsib$river2)
combined_halfsib_goulburn <- subset(combined_halfsib, combined_halfsib$river1=="G" & combined_halfsib$river2 == "G")
combined_halfsib_barmah <- subset(combined_halfsib, combined_halfsib$river1=="B" & combined_halfsib$river2 == "B")
combined_halfsib_chowilla <- subset(combined_halfsib, combined_halfsib$river1=="C" & combined_halfsib$river2 == "C")



#starting with goulburn
# HS info


sum((combined_halfsib_goulburn$year1 == combined_halfsib_goulburn$year2) & combined_halfsib_goulburn$year1 ==14 )
sum((combined_halfsib_goulburn$year1 == combined_halfsib_goulburn$year2) & combined_halfsib_goulburn$year1 ==15 )
sum((combined_halfsib_goulburn$year1 == combined_halfsib_goulburn$year2) & combined_halfsib_goulburn$year1 ==16 )
sum((combined_halfsib_goulburn$year1 == combined_halfsib_goulburn$year2) & combined_halfsib_goulburn$year1 ==17 )
sum((combined_halfsib_goulburn$year1 == combined_halfsib_goulburn$year2) & combined_halfsib_goulburn$year1 ==18 )
# how many half sibs from each year (from the same season)
halfsibs_yearbyyear <- matrix(nrow=5,ncol=2)
halfsibs_yearbyyear <- as.data.frame(halfsibs_yearbyyear)
halfsibs_yearbyyear[1,2] <- sum((combined_halfsib_goulburn$year1 == combined_halfsib_goulburn$year2) & combined_halfsib_goulburn$year1 ==14 )
halfsibs_yearbyyear[2,2] <- sum((combined_halfsib_goulburn$year1 == combined_halfsib_goulburn$year2) & combined_halfsib_goulburn$year1 ==15 )
halfsibs_yearbyyear[3,2] <- sum((combined_halfsib_goulburn$year1 == combined_halfsib_goulburn$year2) & combined_halfsib_goulburn$year1 ==16 )
halfsibs_yearbyyear[4,2] <- sum((combined_halfsib_goulburn$year1 == combined_halfsib_goulburn$year2) & combined_halfsib_goulburn$year1 ==17 )
halfsibs_yearbyyear[5,2] <- sum((combined_halfsib_goulburn$year1 == combined_halfsib_goulburn$year2) & combined_halfsib_goulburn$year1 ==18 )
halfsibs_yearbyyear[1,1] <- ("2014")
halfsibs_yearbyyear[2,1] <- ("2015")
halfsibs_yearbyyear[3,1] <- ("2016")
halfsibs_yearbyyear[4,1] <- ("2017")
halfsibs_yearbyyear[5,1] <- ("2018")
halfsibs_yearbyyear[1,2] <- as.numeric(halfsibs_yearbyyear[1,2])
halfsibs_yearbyyear[2,2] <- as.numeric(halfsibs_yearbyyear[2,2])
halfsibs_yearbyyear[3,2] <- as.numeric(halfsibs_yearbyyear[3,2])
halfsibs_yearbyyear[4,2] <- as.numeric(halfsibs_yearbyyear[4,2])
halfsibs_yearbyyear[5,2] <- as.numeric(halfsibs_yearbyyear[5,2])

tiff("halfsib_pairs_per_year_Goulburn.tif",units='cm', width =14,height=14, res=400)
ggplot(halfsibs_yearbyyear, aes(V1,V2)) + geom_bar(stat="identity") +xlab("Year") + ylab("Number of half sibling pairs") +ylim(0,50) + theme_bw()
dev.off()
# goulburn shows a much more even pattern compared to the overall data. 2015 and 2018 are highest but not by huge amounts


### Now lets try Barmah

###

###
# HS info


sum((combined_halfsib_barmah$year1 == combined_halfsib_barmah$year2) & combined_halfsib_barmah$year1 ==14 )
sum((combined_halfsib_barmah$year1 == combined_halfsib_barmah$year2) & combined_halfsib_barmah$year1 ==15 )
sum((combined_halfsib_barmah$year1 == combined_halfsib_barmah$year2) & combined_halfsib_barmah$year1 ==16 )
sum((combined_halfsib_barmah$year1 == combined_halfsib_barmah$year2) & combined_halfsib_barmah$year1 ==17 )
sum((combined_halfsib_barmah$year1 == combined_halfsib_barmah$year2) & combined_halfsib_barmah$year1 ==18 )
# how many half sibs from each year (from the same season)
halfsibs_yearbyyear <- matrix(nrow=5,ncol=2)
halfsibs_yearbyyear <- as.data.frame(halfsibs_yearbyyear)
halfsibs_yearbyyear[1,2] <- sum((combined_halfsib_barmah$year1 == combined_halfsib_barmah$year2) & combined_halfsib_barmah$year1 ==14 )
halfsibs_yearbyyear[2,2] <- sum((combined_halfsib_barmah$year1 == combined_halfsib_barmah$year2) & combined_halfsib_barmah$year1 ==15 )
halfsibs_yearbyyear[3,2] <- sum((combined_halfsib_barmah$year1 == combined_halfsib_barmah$year2) & combined_halfsib_barmah$year1 ==16 )
halfsibs_yearbyyear[4,2] <- sum((combined_halfsib_barmah$year1 == combined_halfsib_barmah$year2) & combined_halfsib_barmah$year1 ==17 )
halfsibs_yearbyyear[5,2] <- sum((combined_halfsib_barmah$year1 == combined_halfsib_barmah$year2) & combined_halfsib_barmah$year1 ==18 )
halfsibs_yearbyyear[1,1] <- ("2014")
halfsibs_yearbyyear[2,1] <- ("2015")
halfsibs_yearbyyear[3,1] <- ("2016")
halfsibs_yearbyyear[4,1] <- ("2017")
halfsibs_yearbyyear[5,1] <- ("2018")
halfsibs_yearbyyear[1,2] <- as.numeric(halfsibs_yearbyyear[1,2])
halfsibs_yearbyyear[2,2] <- as.numeric(halfsibs_yearbyyear[2,2])
halfsibs_yearbyyear[3,2] <- as.numeric(halfsibs_yearbyyear[3,2])
halfsibs_yearbyyear[4,2] <- as.numeric(halfsibs_yearbyyear[4,2])
halfsibs_yearbyyear[5,2] <- as.numeric(halfsibs_yearbyyear[5,2])

tiff("halfsib_pairs_per_year_Goulburn.tif",units='cm', width =14,height=14, res=400)
ggplot(halfsibs_yearbyyear, aes(V1,V2)) + geom_bar(stat="identity") +xlab("Year") + ylab("Number of half sibling pairs") +ylim(0,50) + theme_bw()
dev.off()

# We again see 2018 as the year with the highest number of half siblings. 2015 and 2016 both failed to sequence more than 10 ind each 
# It's not surprising that there were no HS pairs discovered in these time points. It appears Barmah and Chowilla 
# will need to be a 2 year comparison between 2017 and 2018.
# 

# HS info


### So now what about Chowilla


sum((combined_halfsib_chowilla$year1 == combined_halfsib_chowilla$year2) & combined_halfsib_chowilla$year1 ==14 )
sum((combined_halfsib_chowilla$year1 == combined_halfsib_chowilla$year2) & combined_halfsib_chowilla$year1 ==15 )
sum((combined_halfsib_chowilla$year1 == combined_halfsib_chowilla$year2) & combined_halfsib_chowilla$year1 ==16 )
sum((combined_halfsib_chowilla$year1 == combined_halfsib_chowilla$year2) & combined_halfsib_chowilla$year1 ==17 )
sum((combined_halfsib_chowilla$year1 == combined_halfsib_chowilla$year2) & combined_halfsib_chowilla$year1 ==18 )
# how many half sibs from each year (from the same season)
halfsibs_yearbyyear <- matrix(nrow=5,ncol=2)
halfsibs_yearbyyear <- as.data.frame(halfsibs_yearbyyear)
halfsibs_yearbyyear[1,2] <- sum((combined_halfsib_chowilla$year1 == combined_halfsib_chowilla$year2) & combined_halfsib_chowilla$year1 ==14 )
halfsibs_yearbyyear[2,2] <- sum((combined_halfsib_chowilla$year1 == combined_halfsib_chowilla$year2) & combined_halfsib_chowilla$year1 ==15 )
halfsibs_yearbyyear[3,2] <- sum((combined_halfsib_chowilla$year1 == combined_halfsib_chowilla$year2) & combined_halfsib_chowilla$year1 ==16 )
halfsibs_yearbyyear[4,2] <- sum((combined_halfsib_chowilla$year1 == combined_halfsib_chowilla$year2) & combined_halfsib_chowilla$year1 ==17 )
halfsibs_yearbyyear[5,2] <- sum((combined_halfsib_chowilla$year1 == combined_halfsib_chowilla$year2) & combined_halfsib_chowilla$year1 ==18 )
halfsibs_yearbyyear[1,1] <- ("2014")
halfsibs_yearbyyear[2,1] <- ("2015")
halfsibs_yearbyyear[3,1] <- ("2016")
halfsibs_yearbyyear[4,1] <- ("2017")
halfsibs_yearbyyear[5,1] <- ("2018")
halfsibs_yearbyyear[1,2] <- as.numeric(halfsibs_yearbyyear[1,2])
halfsibs_yearbyyear[2,2] <- as.numeric(halfsibs_yearbyyear[2,2])
halfsibs_yearbyyear[3,2] <- as.numeric(halfsibs_yearbyyear[3,2])
halfsibs_yearbyyear[4,2] <- as.numeric(halfsibs_yearbyyear[4,2])
halfsibs_yearbyyear[5,2] <- as.numeric(halfsibs_yearbyyear[5,2])


#tiff("halfsib_pairs_per_year_Goulburn.tif",units='cm', width =14,height=14, res=400)
ggplot(halfsibs_yearbyyear, aes(V1,V2)) + geom_bar(stat="identity") +xlab("Year") + ylab("Number of half sibling pairs") +ylim(0,200) + theme_bw()
#dev.off()


# HS info

# need to pull these apart and figure out how many parents there were 

# polygamous pairings for each year and overall
# All rivers
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

#Barmah
 #HS info
polygamous_pairings_2014 <- subset(combined_halfsib_barmah, ((combined_halfsib_barmah$year1 == combined_halfsib_barmah$year2) & combined_halfsib_barmah$year1 ==14 ))
polygamous_pairings_2015 <- subset(combined_halfsib_barmah, ((combined_halfsib_barmah$year1 == combined_halfsib_barmah$year2) & combined_halfsib_barmah$year1 ==15 ))
polygamous_pairings_2016 <- subset(combined_halfsib_barmah, ((combined_halfsib_barmah$year1 == combined_halfsib_barmah$year2) & combined_halfsib_barmah$year1 ==16 ))
polygamous_pairings_2017 <- subset(combined_halfsib_barmah, ((combined_halfsib_barmah$year1 == combined_halfsib_barmah$year2) & combined_halfsib_barmah$year1 ==17 ))
polygamous_pairings_2018 <- subset(combined_halfsib_barmah, ((combined_halfsib_barmah$year1 == combined_halfsib_barmah$year2) & combined_halfsib_barmah$year1 ==18 ))
polygamous_pairings <- subset(combined_halfsib_barmah, (combined_halfsib_barmah$year1 == combined_halfsib_barmah$year2))
polygamous_pairings$OffspringID1 <- as.factor(polygamous_pairings$OffspringID1)


polygamous_pairings$OffspringID2 <- as.factor(polygamous_pairings$OffspringID2)


# Chowilla
# HS info
polygamous_pairings_2014 <- subset(combined_halfsib_chowilla, ((combined_halfsib_chowilla$year1 == combined_halfsib_chowilla$year2) & combined_halfsib_chowilla$year1 ==14 ))
polygamous_pairings_2015 <- subset(combined_halfsib_chowilla, ((combined_halfsib_chowilla$year1 == combined_halfsib_chowilla$year2) & combined_halfsib_chowilla$year1 ==15 ))
polygamous_pairings_2016 <- subset(combined_halfsib_chowilla, ((combined_halfsib_chowilla$year1 == combined_halfsib_chowilla$year2) & combined_halfsib_chowilla$year1 ==16 ))
polygamous_pairings_2017 <- subset(combined_halfsib_chowilla, ((combined_halfsib_chowilla$year1 == combined_halfsib_chowilla$year2) & combined_halfsib_chowilla$year1 ==17 ))
polygamous_pairings_2018 <- subset(combined_halfsib_chowilla, ((combined_halfsib_chowilla$year1 == combined_halfsib_chowilla$year2) & combined_halfsib_chowilla$year1 ==18 ))
polygamous_pairings <- subset(combined_halfsib_chowilla, (combined_halfsib_chowilla$year1 == combined_halfsib_chowilla$year2))
polygamous_pairings$OffspringID1 <- as.factor(polygamous_pairings$OffspringID1)
polygamous_pairings$OffspringID2 <- as.factor(polygamous_pairings$OffspringID2)


# How many inds had full siblings 
# Grab the monogamous pairs, subset to site and years, combine into a single vector 
# then run unique to determine number of unique individuals. Number of inds with FS = unique value
# Number of Polygamous inds is calculated previously.
# total number of inds = sample size per year. 
# number with FS and HS pairs = number from HS which had a FS siblnig as well. 
# Number of just FS = all FS unique value - FS and HS both pairs.
# refer to families developed from 









# Best cluster stuff
# So we need to grab the full sibling pairs to give them values equal to the parents of all groups 
# we have the dyads so we will need to assign a p 1 and p 2 to the family 
# calculate that for every individual and have a list of full siblings with parent numbers 
# from here we scan the pairwise dyad of half sib and run along all the values for full sibs to see if there is a family
# If there is an identical value in both therefore it is a half sib with one of the parents (we will code it as first parent)
# If there is already a value there for a P already 

#######################################
#########################################
######################################
# Parent analysis
# Bestcluster file
#

# Need to use best cluster file to determine number of parents who contributed to each year.
# step 1. subset for each river and each year
# step 2.Use each subset section to first determine how many adults are in it. Count unique number of males and females.
# step 3. Use best FS family to count how many parents had large FS clusters
# step 4. Use plotted out polygamous relationships to count how many parents undertook polygamy, and polygamy
# from both adults
# step 5.



bestcluster <-read.csv("C:/Users/18088076/Dropbox/PhD/Chapters/Recruitment over time/analysis/all rivers/larvae only/colony/newnamed/long/MC_all_larvae_details.BestCluster", sep="")
bestcluster <-read.csv("D:/Dropbox/PhD/Chapters/Recruitment over time/analysis/all rivers/larvae only/colony/newnamed/long/MC_all_larvae_details.BestCluster", sep="")

bestcluster <- subset(bestcluster,bestcluster$Probability>=0.99)

library("plyr", lib.loc="~/R/win-library/3.5")

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
parent_summary_yearscountalllocations <- parent_summary_yearscount
# There is an adult which produced 20 offspring!
#This adult produced not just the largest full sibling family but the largest 2 in the study, mating with two others
# This adult mated with 5 females over 1 year in Chowilla. Larvae were found in 4 out of the 6 sites!

# There is an adult which produced 14 offspring! 
# This adult prodiced 2 very large sibling families in the study, mating with 1 other female. 
# This adult mated with 2 females over 1 year in Barmah. Larvae were found in 3 out of 3 sites! 


tiff("number_of_offspring_from_each parent_all_rivers.tif",units='cm', width =14,height=14, res=400)
ggplot(parent_summary_yearscount, aes(sum, freq)) + geom_bar(stat="identity") +xlab("Number of sequenced offspring each parent produced") + ylab("Number of parents identified") +ylim(0,315) + theme_bw()
dev.off()

# Completed all rivers at once

# test for random mating
# all rivers

# too many low values 
# round all values 6 and above to a single 6 +
# replace all values larger than 6 with 6
# What about zeros? Without zeros the distrubiton could be completely non poisson just from deviation between zero
# expected and observed. 
# solution = 2 test/2 fold
# test 1. zeros = effective number of breeders - all known to breed e.g. 645-nrow(parent_summary_years)
# test 2. zeros = the number of observed values required for the pearson residual of zeros to = 0 (or close to)
parentsummary_yearssum$sum <- replace(parentsummary_yearssum$sum, parentsummary_yearssum$sum==7,6)


parentvector <- parentsummary_yearssum$sum
vector_zeros <- rep(0,times=(642-nrow(parent_summary_years)))
alladults_Ne_vector <- c(parentvector,vector_zeros)

gf1 <- goodfit(alladults_Ne_vector, type = "poisson",
               method = c("ML"), par = NULL)
summary(gf1)
plot(gf1)


parentvector <- parentsummary_yearssum$sum
vector_zeros <- rep(0,times=(738-nrow(parent_summary_years)))
alladults_Ne_vector <- c(parentvector,vector_zeros)

gf2 <- goodfit(alladults_Ne_vector, type = "poisson",
               method = c("ML"), par = NULL)

summary(gf2)
plot(gf2)





# Now to try separate rivers
# bestcluster
# parent analysis

#Goulburn

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

# 2014
sum(parent_summary_years[,2] >0)
# 2015
sum(parent_summary_years[,3] >0)
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
parent_summary_yearscountgoulburn <- parent_summary_yearscount


tiff("number_of_offspring_from_each parent_Goulburn.tif",units='cm', width =14,height=14, res=400)
ggplot(parent_summary_yearscount, aes(sum, freq)) + geom_bar(stat="identity") +xlab("Number of sequenced offspring each parent produced") + ylab("Number of parents identified") +ylim(0,200) + theme_bw()
dev.off()


parent_summary_years_P_A <- parent_summary_years
is.factor(parent_summary_years_P_A[,1])

#Code to convert numeric into present absent data 1's and 0's

for (j in c(1:ncol(parent_summary_years_P_A))) {
  
  
  for (i in c(1:nrow(parent_summary_years_P_A))){
    
    
    if (is.factor(parent_summary_years_P_A[,j])) {
      
      parent_summary_years_P_A[i,j] <- parent_summary_years_P_A[i,j]
      
      
    } else if (parent_summary_years_P_A[i,j] >0) {
      
      
      parent_summary_years_P_A[i,j] <- 1
      
      
    }  else {
      
      parent_summary_years_P_A[i,j] <- 0
      
    }
    
  }
}


# I think I need the names of the Ind in each cell and not the P/A data

parent_summary_years_ind_names <- parent_summary_years_P_A

for (j in c(1:ncol(parent_summary_years_ind_names))) {
  
  
  for (i in c(1:nrow(parent_summary_years_ind_names))){
    
    
    if (is.factor(parent_summary_years_ind_names[,j])) {
      
      parent_summary_years_ind_names[i,j] <- parent_summary_years_ind_names[i,j]
      
      
    } else if (parent_summary_years_ind_names[i,j] >0) {
      
      
      parent_summary_years_ind_names[i,j] <- as.character(parent_summary_years_ind_names[i,1])
      
      
    }  else {
      
      parent_summary_years_ind_names[i,j] <- 0
      
    }
    
  }
}




parent_summary_years_ind_names[parent_summary_years_ind_names==0] <- NA

parent_summary_years_ind_names2014 <- parent_summary_years_ind_names$`2014`
parent_summary_years_ind_names2014<-parent_summary_years_ind_names2014[!is.na(parent_summary_years_ind_names2014)]
parent_summary_years_ind_names2015 <- parent_summary_years_ind_names$`2015`
parent_summary_years_ind_names2015<-parent_summary_years_ind_names2015[!is.na(parent_summary_years_ind_names2015)]
parent_summary_years_ind_names2016 <- parent_summary_years_ind_names$`2016`
parent_summary_years_ind_names2016<-parent_summary_years_ind_names2016[!is.na(parent_summary_years_ind_names2016)]
parent_summary_years_ind_names2017 <- parent_summary_years_ind_names$`2017`
parent_summary_years_ind_names2017<-parent_summary_years_ind_names2017[!is.na(parent_summary_years_ind_names2017)]
parent_summary_years_ind_names2018 <- parent_summary_years_ind_names$`2018`
parent_summary_years_ind_names2018<-parent_summary_years_ind_names2018[!is.na(parent_summary_years_ind_names2018)]




#Done the Venn diagram

#venn is a  function from package gplot btw

library("gplots", lib.loc="~/R/win-library/3.5")

tiff("Venn_diagram_parents_contributing_to_each_year_Goulburn_full_data.tif",units='cm', width =14,height=14, res=400)
Yearlylist <-venn(list("2014"=parent_summary_years_ind_names2014, "2015"=parent_summary_years_ind_names2015,"2016"=parent_summary_years_ind_names2016,"2017"=parent_summary_years_ind_names2017,"2018"=parent_summary_years_ind_names2018))
dev.off()

#Going off Yearlylist
# 2 years
#2014-2015 = 3 
#2014-2016 = 6 
#2014-2017 = 5 
#2014-2018 = 5 
#2015:2016 = 12 
#2015:2017 = 8 
#2015:2018 = 1 
#2016:2017 = 4 
#2016:2018 = 7 
#2017:2018 = 7 
#3 years
#2014:2015:2016 = 1 

#2014:2016:2017= 1 

#2015:2016:2017= 2 

#4 years

#sums of 2 years that are one year apart, two years apart,  3 years apart
#2014-2015, 2015-2016, 2016-2017, 2017-2018 = 3,12,4,7 ==26  == 26/4 (avg) == 6.5
# 2014-2016, 2015-2017, 2016-2018 = 6,8,7 = 21 = 21/3 (avg) = 7


#1 year
#2014= 59
#2015 = 53
#2016= 48
#2017 = 43
#2018 = 51

#Sum 2 years
58
#Sum 3 years
4
# sum 1 year 
254



#percentage  who bred one year
(254/316)*100
# 80.38% of adults bred only during a single year
(58/316)*100
#18.35% of adults bred in two years
(4/316)*100
#1.27% of adults bred in 3 years



#Are the means of skipping a year/ each consecutive year significantly different? 
#Remember to do this I need to create an analysis of variance first
# Then I can calculate the TukeysHSD 




matrix1 <- matrix(nrow= 7, ncol=2)
# consecutive first
matrix1[1,1] <- 3
matrix1[1,2] <- 1
matrix1[2,1] <- 12
matrix1[2,2] <- 1
matrix1[3,1] <- 4
matrix1[3,2] <- 1
matrix1[4,1] <- 7
matrix1[4,2] <- 1
# skipping a year now
matrix1[5,1] <- 6
matrix1[5,2] <- 2
matrix1[6,1] <- 8
matrix1[6,2] <- 2
matrix1[7,1] <- 7
matrix1[7,2] <- 2

dataframe1 <- as.data.frame(matrix1)
colnames(dataframe1) <- c("N","gap")
dataframe1$gap <- as.factor(dataframe1$gap)
yearsbreeding_anova<-aov(N~gap,data=dataframe1)
TukeyHSD <- TukeyHSD(yearsbreeding_anova) 











# Test for random mating
# Goulburn
# bestcluster
parentsummary_yearssum$sum <- replace(parentsummary_yearssum$sum, parentsummary_yearssum$sum==20,6)


parentvector <- parentsummary_yearssum$sum
vector_zeros <- rep(0,times=(625-nrow(parent_summary_years)))
alladults_Ne_vector <- c(parentvector,vector_zeros)

gf1 <- goodfit(alladults_Ne_vector, type = "poisson",
               method = c("ML"), par = NULL)
summary(gf1)
plot(gf1)


parentvector <- parentsummary_yearssum$sum
vector_zeros <- rep(0,times=(459-nrow(parent_summary_years)))
alladults_Ne_vector <- c(parentvector,vector_zeros)

gf2 <- goodfit(alladults_Ne_vector, type = "poisson",
               method = c("ML"), par = NULL)

summary(gf2)
plot(gf2)


# Goulburn done
# bestcluster
# lets move onto barmah

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
sum(parent_summary_years[,3] >0)
parent_summary_years$sum <- NA

for (i in (1:nrow(parent_summary_years))) {
  
  
  parent_summary_years$sum[i] <- sum(parent_summary_years[i,2:3])
  
  
}


# gg plot the resulting sum columns
parent_summary_yearscount <- count(parent_summary_years, "sum")
parentsummary_yearssum <- parent_summary_years[,c(1,4)]
parent_summary_yearscountbarmah <- parent_summary_yearscount


# need to do venn to though


parent_summary_years_P_A <- parent_summary_years
is.factor(parent_summary_years_P_A[,1])

#Code to convert numeric into present absent data 1's and 0's

for (j in c(1:ncol(parent_summary_years_P_A))) {
  
  
  for (i in c(1:nrow(parent_summary_years_P_A))){
    
    
    if (is.factor(parent_summary_years_P_A[,j])) {
      
      parent_summary_years_P_A[i,j] <- parent_summary_years_P_A[i,j]
      
      
    } else if (parent_summary_years_P_A[i,j] >0) {
      
      
      parent_summary_years_P_A[i,j] <- 1
      
      
    }  else {
      
      parent_summary_years_P_A[i,j] <- 0
      
    }
    
  }
}


# I think I need the names of the Ind in each cell and not the P/A data

parent_summary_years_ind_names <- parent_summary_years_P_A

for (j in c(1:ncol(parent_summary_years_ind_names))) {
  
  
  for (i in c(1:nrow(parent_summary_years_ind_names))){
    
    
    if (is.factor(parent_summary_years_ind_names[,j])) {
      
      parent_summary_years_ind_names[i,j] <- parent_summary_years_ind_names[i,j]
      
      
    } else if (parent_summary_years_ind_names[i,j] >0) {
      
      
      parent_summary_years_ind_names[i,j] <- as.character(parent_summary_years_ind_names[i,1])
      
      
    }  else {
      
      parent_summary_years_ind_names[i,j] <- 0
      
    }
    
  }
}




parent_summary_years_ind_names[parent_summary_years_ind_names==0] <- NA

parent_summary_years_ind_names2014 <- parent_summary_years_ind_names$`2014`
parent_summary_years_ind_names2014<-parent_summary_years_ind_names2014[!is.na(parent_summary_years_ind_names2014)]
parent_summary_years_ind_names2015 <- parent_summary_years_ind_names$`2015`
parent_summary_years_ind_names2015<-parent_summary_years_ind_names2015[!is.na(parent_summary_years_ind_names2015)]
parent_summary_years_ind_names2016 <- parent_summary_years_ind_names$`2016`
parent_summary_years_ind_names2016<-parent_summary_years_ind_names2016[!is.na(parent_summary_years_ind_names2016)]
parent_summary_years_ind_names2017 <- parent_summary_years_ind_names$`2017`
parent_summary_years_ind_names2017<-parent_summary_years_ind_names2017[!is.na(parent_summary_years_ind_names2017)]
parent_summary_years_ind_names2018 <- parent_summary_years_ind_names$`2018`
parent_summary_years_ind_names2018<-parent_summary_years_ind_names2018[!is.na(parent_summary_years_ind_names2018)]




#Done the Venn diagram

#venn is a  function from package gplot btw

library("gplots", lib.loc="~/R/win-library/3.5")

tiff("Venn_diagram_parents_contributing_to_each_year_Goulburn.tif",units='cm', width =14,height=14, res=400)
Yearlylist <-venn(list("2014"=parent_summary_years_ind_names2014, "2015"=parent_summary_years_ind_names2015,"2016"=parent_summary_years_ind_names2016,"2017"=parent_summary_years_ind_names2017,"2018"=parent_summary_years_ind_names2018))
dev.off()







tiff("number_of_offspring_from_each parent_barmah.tif",units='cm', width =14,height=14, res=400)
ggplot(parent_summary_yearscount, aes(sum, freq)) + geom_bar(stat="identity") +xlab("Number of sequenced offspring each parent produced") + ylab("Number of parents identified") +ylim(0,115) + theme_bw()
dev.off()

# Test for random mating
# barmah
# bestcluster
parentsummary_yearssum$sum <- replace(parentsummary_yearssum$sum, parentsummary_yearssum$sum==14,6)


parentvector <- parentsummary_yearssum$sum
vector_zeros <- rep(0,times=(169-nrow(parent_summary_years)))
alladults_Ne_vector <- c(parentvector,vector_zeros)

gf1 <- goodfit(alladults_Ne_vector, type = "poisson",
               method = c("ML"), par = NULL)
summary(gf1)
plot(gf1)


parentvector <- parentsummary_yearssum$sum
vector_zeros <- rep(0,times=(235-nrow(parent_summary_years)))
alladults_Ne_vector <- c(parentvector,vector_zeros)

gf2 <- goodfit(alladults_Ne_vector, type = "poisson",
               method = c("ML"), par = NULL)

summary(gf2)
plot(gf2)

# Barmah is finished now
# lets move onto chowilla
# bestcluster


#Chowilla

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
sum(parent_summary_years[,6] >0)


rownames(parent_summary_years) <- parent_summary_years[,1]

parent_summary_years$sum <- NA

for (i in (1:nrow(parent_summary_years))) {
  
  
  parent_summary_years$sum[i] <- sum(parent_summary_years[i,2:6])
  
  
}


# gg plot the resulting sum columns
parent_summary_yearscount <- count(parent_summary_years, "sum")
parentsummary_yearssum <- parent_summary_years[,c(1,7)]
parent_summary_yearscountchowilla <- parent_summary_yearscount


tiff("number_of_offspring_from_each parent_Chowilla.tif",units='cm', width =14,height=14, res=400)
ggplot(parent_summary_yearscountchowilla, aes(sum, freq)) + geom_bar(stat="identity") +xlab("Number of sequenced offspring each parent produced") + ylab("Number of parents identified") +ylim(0,40) + theme_bw()
dev.off()



#we have all the different parent summaries now
# let us make a combined graph

parent_summary_yearscountalllocations
parent_summary_yearscountgoulburn
parent_summary_yearscountbarmah
parent_summary_yearscountchowilla


gpall <- ggplot(parent_summary_yearscountalllocations, aes(sum, freq)) + geom_bar(stat="identity") +xlab("No. sequenced offspring  \neach parent produced") + ylab("No. parents identified") +ylim(0,330) + theme_bw()

gpall <- gpall + theme(axis.text = element_text(size = 10), axis.title = element_text(size = 13.0),legend.position = "none", plot.background = element_blank(),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank())

gpgoulburn <- ggplot(parent_summary_yearscountgoulburn, aes(sum, freq)) + geom_bar(stat="identity") +xlab("No. sequenced offspring  \neach parent produced") + ylab("No. parents identified") +ylim(0,200) + theme_bw()
gpgoulburn <- gpgoulburn + theme(axis.text = element_text(size = 10), axis.title = element_text(size = 13.0),legend.position = "none", plot.background = element_blank(),
                                 panel.grid.major = element_blank(),
                                 panel.grid.minor = element_blank())

gpbarmah <- ggplot(parent_summary_yearscountbarmah, aes(sum, freq)) + geom_bar(stat="identity") +xlab("No. sequenced offspring  \neach parent produced") + ylab("No. parents identified") +ylim(0,100) + theme_bw()
gpbarmah <- gpbarmah + theme(axis.text = element_text(size = 10), axis.title = element_text(size = 13.0),legend.position = "none", plot.background = element_blank(),
                             panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank())

gpchowilla <- ggplot(parent_summary_yearscountchowilla, aes(sum, freq)) + geom_bar(stat="identity") +xlab("No. sequenced offspring  \neach parent produced") + ylab("No. parents identified") +ylim(0,40) + theme_bw()
gpchowilla <- gpchowilla + theme(axis.text = element_text(size = 10), axis.title = element_text(size = 13.0),legend.position = "none", plot.background = element_blank(),
                                 panel.grid.major = element_blank(),
                                 panel.grid.minor = element_blank())


library(ggpubr)
#combine into 1 ggplot

tiff("number_of_offspring_from_each parent_all_graphs.tif",units='cm', width =14,height=14, res=400)
combinedplot <- ggarrange(gpall, gpgoulburn, gpbarmah, gpchowilla,
          labels = c("A", "B", "C", "D"),label.x =0.38, font.label = list(size = 22),
          ncol = 2, nrow = 2)
dev.off()


combinedplot <- combinedplot +theme(panel.border = element_rect(colour = "black", fill=NA, size=1) )

tiff("number_of_offspring_from_each parent_all_graphs.tif",units='cm', width =14,height=14, res=400)
combinedplot
dev.off()


# Test for random mating
# barmah
# bestcluster
parentsummary_yearssum$sum <- replace(parentsummary_yearssum$sum, parentsummary_yearssum$sum==20,6)


parentvector <- parentsummary_yearssum$sum
vector_zeros <- rep(0,times=(60-nrow(parent_summary_years)))
alladults_Ne_vector <- c(parentvector,vector_zeros)

gf1 <- goodfit(alladults_Ne_vector, type = "poisson",
               method = c("ML"), par = NULL)
summary(gf1)
plot(gf1)


parentvector <- parentsummary_yearssum$sum
vector_zeros <- rep(0,times=(77-nrow(parent_summary_years)))
alladults_Ne_vector <- c(parentvector,vector_zeros)

gf2 <- goodfit(alladults_Ne_vector, type = "poisson",
               method = c("ML"), par = NULL)

summary(gf2)
plot(gf2)

##
####



###########


########

# calculate the number of parents who contributed to full sib families 
# Best Family cluster file
# Polygamy parents results 

######
getwd()

full_sibfamily <- read.delim("C:/Users/18088076/Dropbox/PhD/Chapters/Recruitment over time/analysis/all rivers/larvae only/colony/newnamed/long/MC_all_larvae_details.BestFSFamily")




colnames(full_sibfamily) <- c('FullSibshipIndex' ,'Prob_Inc',       'Prob_Exc'     ,  'Member1'       ,   'Member2'        ,  'Member3',         
                              'Member4' ,         'Member5'   ,       'Member6'     ,     'Member7'     ,     'Member8'     ,     'Member9')


full_sibfamily2 <- subset(full_sibfamily, full_sibfamily$Prob_Inc >= 0.99 & full_sibfamily$Prob_Exc >= 0.99)

full_sibfamily2$year1 <- NA
full_sibfamily2$river1 <- NA
full_sibfamily2$site1 <- NA

full_sibfamily2$Member1 <- gsub("-", "",full_sibfamily2$Member1)
full_sibfamily2$Member2 <- gsub("-", "",full_sibfamily2$Member2)
full_sibfamily2$Member3 <- gsub("-", "",full_sibfamily2$Member3)
full_sibfamily2$Member4 <- gsub("-", "",full_sibfamily2$Member4)
full_sibfamily2$Member5 <- gsub("-", "",full_sibfamily2$Member5)
full_sibfamily2$Member6 <- gsub("-", "",full_sibfamily2$Member6)
full_sibfamily2$Member7 <- gsub("-", "",full_sibfamily2$Member7)
full_sibfamily2$Member8 <- gsub("-", "",full_sibfamily2$Member8)
full_sibfamily2$Member9 <- gsub("-", "",full_sibfamily2$Member9)

# only one family bred across 2 years 

for (i in c(1:nrow(full_sibfamily2))) {
  
  # substring to year 
  sub1  <- substr(full_sibfamily2$Member1[i], 3,4)
  
  
  # substring to river 
  sub2  <- substr(full_sibfamily2$Member1[i], 5,5)
  
  #substring to site
  sub3  <- substr(full_sibfamily2$Member1[i], 6,6)
  
  full_sibfamily2$year1[i] <- sub1
  full_sibfamily2$river1[i] <- sub2
  full_sibfamily2$site1[i] <- sub3
  
  
  
}

# Best FS is now usable 
# need to subset 
# Polygamy results

full_sibfamily_goulburn <- subset(full_sibfamily2, full_sibfamily2$river1=="G")
full_sibfamily_barmah <- subset(full_sibfamily2, full_sibfamily2$river1=="B")
full_sibfamily_chowilla <- subset(full_sibfamily2, full_sibfamily2$river1=="C")

# Goulburn first

full_sibfamily2014 <- subset(full_sibfamily_goulburn, full_sibfamily_goulburn$year1 == 14)
nrow(full_sibfamily2014)


full_sibfamily2014monogamous_pairings <- subset(full_sibfamily2014, full_sibfamily2014$Member2 != "")
nrow(full_sibfamily2014monogamous_pairings) 
# 11 FS families, 22 FS parents

# How many pairings are there per number of offspring 2014. # measure only usefull for offspring. Maybe go back to FS family excel sheet
# create new one with FS family plus the parents from there divide up the full siblings by year etc again run counts as done
# also run counts of how many parents there were that are also unique to the other parents there. 
sum(full_sibfamily2014$Member1 != "" & full_sibfamily2014$Member2 == "")
sum(full_sibfamily2014$Member2 != "" & full_sibfamily2014$Member3 == "")
sum(full_sibfamily2014$Member3 != "" & full_sibfamily2014$Member4 == "")
sum(full_sibfamily2014$Member4 != "" & full_sibfamily2014$Member5 == "")
sum(full_sibfamily2014$Member5 != "")
nrow(full_sibfamily2014)
monogamous_pairings_2014_freq <- matrix(nrow= 5, ncol=2)

monogamous_pairings_2014_freq[1,1] <- "1"
monogamous_pairings_2014_freq[2,1] <- "2"
monogamous_pairings_2014_freq[3,1] <- "3"
monogamous_pairings_2014_freq[4,1] <- "4"
monogamous_pairings_2014_freq[5,1] <- "5"

colnames(monogamous_pairings_2014_freq) <- c("Offspring" , "freq")

monogamous_pairings_2014_freq <- as.data.frame(monogamous_pairings_2014_freq)
monogamous_pairings_2014_freq$freq <- as.numeric(monogamous_pairings_2014_freq$freq)
monogamous_pairings_2014_freq$Offspring <- as.character(monogamous_pairings_2014_freq$Offspring)

monogamous_pairings_2014_freq[1,2] <- sum(full_sibfamily2014$Member1 != "" & full_sibfamily2014$Member2 == "")
monogamous_pairings_2014_freq[2,2] <- sum(full_sibfamily2014$Member2 != "" & full_sibfamily2014$Member3 == "")
monogamous_pairings_2014_freq[3,2] <- sum(full_sibfamily2014$Member3 != "" & full_sibfamily2014$Member4 == "")
monogamous_pairings_2014_freq[4,2] <- sum(full_sibfamily2014$Member4 != "" & full_sibfamily2014$Member5 == "")
monogamous_pairings_2014_freq[5,2] <- sum(full_sibfamily2014$Member5 != "")


ggplot(monogamous_pairings_2014_freq, aes(Offspring, freq)) + geom_bar(stat="identity")

tiff("Fullsib_family_sizes_Goulburn2014.tif",units='cm', width =14,height=14, res=400)
ggplot(monogamous_pairings_2014_freq, aes(Offspring, freq)) + geom_bar(stat="identity") +xlab("size of full sibling families") + ylab("Number of full sibling families") +ylim(0,50) + theme_bw() + ggtitle("2014")
dev.off()

# of all parents from 2014, 41 had a single offspring sampled, 7 had 1 full sibling, 2 had 2 and 1 had 3. 
# 4 polygamous parents were found
# 10 parent pairs = 20 parents produced monogamous pairings with multiple offspring.
# of the 41 with no full siblings 38 had no relation to any other offspring (3 were half sibs of another)
#74.5% of individuals had no relatedness to any other sample. 
# repeat for 2015-2018



full_sibfamily2015 <- subset(full_sibfamily_goulburn, full_sibfamily_goulburn$year1 == 15)
nrow(full_sibfamily2015)


full_sibfamily2015monogamous_pairings <- subset(full_sibfamily2015, full_sibfamily2015$Member2 != "")
# number of adults contributing to full sibling pairs (two adults per offspring pair so 2*nrow)
nrow(full_sibfamily2015monogamous_pairings) 
# 20 adults 


# How many pairings are there per number of offspring 2014. 
sum(full_sibfamily2015$Member1 != "" & full_sibfamily2015$Member2 == "" )
sum(full_sibfamily2015$Member2 != "" & full_sibfamily2015$Member3 == "")
sum(full_sibfamily2015$Member3 != "" & full_sibfamily2015$Member4 == "")
sum(full_sibfamily2015$Member4 != "" & full_sibfamily2015$Member5 == "")
sum(full_sibfamily2015$Member5 != "")

monogamous_pairings_2015_freq <- matrix(nrow= 5, ncol=2)

monogamous_pairings_2015_freq[1,1] <- "1"
monogamous_pairings_2015_freq[2,1] <- "2"
monogamous_pairings_2015_freq[3,1] <- "3"
monogamous_pairings_2015_freq[4,1] <- "4"
monogamous_pairings_2015_freq[5,1] <- "5"

colnames(monogamous_pairings_2015_freq) <- c("Offspring" , "freq")

monogamous_pairings_2015_freq <- as.data.frame(monogamous_pairings_2015_freq)
monogamous_pairings_2015_freq$freq <- as.numeric(monogamous_pairings_2015_freq$freq)
monogamous_pairings_2015_freq$Offspring <- as.character(monogamous_pairings_2015_freq$Offspring)

monogamous_pairings_2015_freq[1,2] <- sum(full_sibfamily2015$Member1 != "" & full_sibfamily2015$Member2 == "")
monogamous_pairings_2015_freq[2,2] <- sum(full_sibfamily2015$Member2 != "" & full_sibfamily2015$Member3 == "")
monogamous_pairings_2015_freq[3,2] <- sum(full_sibfamily2015$Member3 != "" & full_sibfamily2015$Member4 == "")
monogamous_pairings_2015_freq[4,2] <- sum(full_sibfamily2015$Member4 != "" & full_sibfamily2015$Member5 == "")
monogamous_pairings_2015_freq[5,2] <- sum(full_sibfamily2015$Member5 != "")


ggplot(monogamous_pairings_2015_freq, aes(Offspring, freq)) + geom_bar(stat="identity")



# of all parents from 2015, 51 had a single offspring sampled, 8 had 1 full sibling, 1 had 2 and 1 had 3. 
# 4 polygamous parents were found
# 10 parent pairs = 20 parents produced monogamous pairings with multiple offspring.
# of the 51 with no full siblings 45 had no relation to any other offspring (6 were half sibs of another)
#73.7% of individuals had no relatedness to any other sample. 

stop()

full_sibfamily2016 <- subset(full_sibfamily_goulburn, full_sibfamily_goulburn$year1 == 16)
nrow(full_sibfamily2016)


full_sibfamily2016monogamous_pairings <- subset(full_sibfamily2016, full_sibfamily2016$Member2 != "")

# number of adults contributing to full sibling pairs (two adults per offspring pair so 2*nrow)
nrow(full_sibfamily2016monogamous_pairings) 
# 12 adults contributed


# How many pairings are there per number of offspring 2014. 
sum(full_sibfamily2016$Member1 != "" & full_sibfamily2016$Member2 == "" )
sum(full_sibfamily2016$Member2 != "" & full_sibfamily2016$Member3 == "")
sum(full_sibfamily2016$Member3 != "" & full_sibfamily2016$Member4 == "")
sum(full_sibfamily2016$Member4 != "" & full_sibfamily2016$Member5 == "")
sum(full_sibfamily2016$Member5 != "")

monogamous_pairings_2016_freq <- matrix(nrow= 5, ncol=2)

monogamous_pairings_2016_freq[1,1] <- "1"
monogamous_pairings_2016_freq[2,1] <- "2"
monogamous_pairings_2016_freq[3,1] <- "3"
monogamous_pairings_2016_freq[4,1] <- "4"
monogamous_pairings_2016_freq[5,1] <- "5"

colnames(monogamous_pairings_2016_freq) <- c("Offspring" , "freq")

monogamous_pairings_2016_freq <- as.data.frame(monogamous_pairings_2016_freq)
monogamous_pairings_2016_freq$freq <- as.numeric(monogamous_pairings_2016_freq$freq)
monogamous_pairings_2016_freq$Offspring <- as.character(monogamous_pairings_2016_freq$Offspring)

monogamous_pairings_2016_freq[1,2] <- sum(full_sibfamily2016$Member1 != "" & full_sibfamily2016$Member2 == "")
monogamous_pairings_2016_freq[2,2] <- sum(full_sibfamily2016$Member2 != "" & full_sibfamily2016$Member3 == "")
monogamous_pairings_2016_freq[3,2] <- sum(full_sibfamily2016$Member3 != "" & full_sibfamily2016$Member4 == "")
monogamous_pairings_2016_freq[4,2] <- sum(full_sibfamily2016$Member4 != "" & full_sibfamily2016$Member5 == "")
monogamous_pairings_2016_freq[5,2] <- sum(full_sibfamily2016$Member5 != "")


ggplot(monogamous_pairings_2016_freq, aes(Offspring, freq)) + geom_bar(stat="identity")

# of all parents from 2016, 49 had a single offspring sampled, 3 had 1 full sibling, 2 had 2 and 0 had 3. 
# 4 polygamous parents were found
# 5 parent pairs = 10 parents produced monogamous pairings with multiple offspring.
# of the 46 with no full siblings 45 had no relation to any other offspring (3 were half sibs of another)
# 85.1% of individuals had no relatedness to any other sample. 
# 3 plotgamous parents were observed




full_sibfamily2017 <- subset(full_sibfamily_goulburn, full_sibfamily_goulburn$year1 == 17)
nrow(full_sibfamily2017)


full_sibfamily2017monogamous_pairings <- subset(full_sibfamily2017, full_sibfamily2017$Member2 != "")
# number of adults contributing to full sibling pairs (two adults per offspring pair so 2*nrow)
nrow(full_sibfamily2017monogamous_pairings) 
# 14 adults

# How many pairings are there per number of offspring 2014. 
sum(full_sibfamily2017$Member1 != "" & full_sibfamily2017$Member2 == "" )
sum(full_sibfamily2017$Member2 != "" & full_sibfamily2017$Member3 == "")
sum(full_sibfamily2017$Member3 != "" & full_sibfamily2017$Member4 == "")
sum(full_sibfamily2017$Member4 != "" & full_sibfamily2017$Member5 == "")
sum(full_sibfamily2017$Member5 != "")

monogamous_pairings_2017_freq <- matrix(nrow= 5, ncol=2)

monogamous_pairings_2017_freq[1,1] <- "1"
monogamous_pairings_2017_freq[2,1] <- "2"
monogamous_pairings_2017_freq[3,1] <- "3"
monogamous_pairings_2017_freq[4,1] <- "4"
monogamous_pairings_2017_freq[5,1] <- "5"

colnames(monogamous_pairings_2017_freq) <- c("Offspring" , "freq")

monogamous_pairings_2017_freq <- as.data.frame(monogamous_pairings_2017_freq)
monogamous_pairings_2017_freq$freq <- as.numeric(monogamous_pairings_2017_freq$freq)
monogamous_pairings_2017_freq$Offspring <- as.character(monogamous_pairings_2017_freq$Offspring)

monogamous_pairings_2017_freq[1,2] <- sum(full_sibfamily2017$Member1 != "" & full_sibfamily2017$Member2 == "")
monogamous_pairings_2017_freq[2,2] <- sum(full_sibfamily2017$Member2 != "" & full_sibfamily2017$Member3 == "")
monogamous_pairings_2017_freq[3,2] <- sum(full_sibfamily2017$Member3 != "" & full_sibfamily2017$Member4 == "")
monogamous_pairings_2017_freq[4,2] <- sum(full_sibfamily2017$Member4 != "" & full_sibfamily2017$Member5 == "")
monogamous_pairings_2017_freq[5,2] <- sum(full_sibfamily2017$Member5 != "")


ggplot(monogamous_pairings_2017_freq, aes(Offspring, freq)) + geom_bar(stat="identity")




# of all parents from 2017, 44 had a single offspring sampled, 5 had 1 full sibling, 0 had 2 and 2 had 3. 
# 4 polygamous parents were found
# 7 parent pairs = 14 parents produced monogamous pairings with multiple offspring.
# of the 44 with no full siblings 42 had no relation to any other offspring (2 were half sibs of another)
#82.3% of individuals had no relatedness to any other sample. 





full_sibfamily2018 <- subset(full_sibfamily_goulburn, full_sibfamily_goulburn$year1 == 18)
nrow(full_sibfamily2018)


full_sibfamily2018monogamous_pairings <- subset(full_sibfamily2018, full_sibfamily2018$Member2 != "")

# number of adults contributing to full sibling pairs (two adults per offspring pair so 2*nrow)
nrow(full_sibfamily2018monogamous_pairings) 
# 22 adults

# How many pairings are there per number of offspring 2018. 
sum(full_sibfamily2018$Member1 != "" & full_sibfamily2018$Member2 == "" )
sum(full_sibfamily2018$Member2 != "" & full_sibfamily2018$Member3 == "")
sum(full_sibfamily2018$Member3 != "" & full_sibfamily2018$Member4 == "")
sum(full_sibfamily2018$Member4 != "" & full_sibfamily2018$Member5 == "")
sum(full_sibfamily2018$Member5 != "")

monogamous_pairings_2018_freq <- matrix(nrow= 5, ncol=2)

monogamous_pairings_2018_freq[1,1] <- "1"
monogamous_pairings_2018_freq[2,1] <- "2"
monogamous_pairings_2018_freq[3,1] <- "3"
monogamous_pairings_2018_freq[4,1] <- "4"
monogamous_pairings_2018_freq[5,1] <- "5"

colnames(monogamous_pairings_2018_freq) <- c("Offspring" , "freq")

monogamous_pairings_2018_freq <- as.data.frame(monogamous_pairings_2018_freq)
monogamous_pairings_2018_freq$freq <- as.numeric(monogamous_pairings_2018_freq$freq)
monogamous_pairings_2018_freq$Offspring <- as.character(monogamous_pairings_2018_freq$Offspring)

monogamous_pairings_2018_freq[1,2] <- sum(full_sibfamily2018$Member1 != "" & full_sibfamily2018$Member2 == "")
monogamous_pairings_2018_freq[2,2] <- sum(full_sibfamily2018$Member2 != "" & full_sibfamily2018$Member3 == "")
monogamous_pairings_2018_freq[3,2] <- sum(full_sibfamily2018$Member3 != "" & full_sibfamily2018$Member4 == "")
monogamous_pairings_2018_freq[4,2] <- sum(full_sibfamily2018$Member4 != "" & full_sibfamily2018$Member5 == "")
monogamous_pairings_2018_freq[5,2] <- sum(full_sibfamily2018$Member5 != "")


ggplot(monogamous_pairings_2018_freq, aes(Offspring, freq)) + geom_bar(stat="identity")

############
######

# Next up is Barmah
# Best FS family
# polygamous adults results

#########

full_sibfamily2017 <- subset(full_sibfamily_barmah, full_sibfamily_barmah$year1 == 17)
full_sibfamily2017monogamous_pairings <- subset(full_sibfamily2017, full_sibfamily2017$Member2 != "")

nrow(full_sibfamily2017monogamous_pairings) 

# 5 FS families, 10 FS parents

full_sibfamily2018 <- subset(full_sibfamily_barmah, full_sibfamily_barmah$year1 == 18)
full_sibfamily2018monogamous_pairings <- subset(full_sibfamily2018, full_sibfamily2018$Member2 != "")

nrow(full_sibfamily2018monogamous_pairings) 
# 7 FS families, 14 FS parents

#Results polygamous parents
# Best FS family 
# now onto Chowilla





full_sibfamily2017 <- subset(full_sibfamily_chowilla, full_sibfamily_chowilla$year1 == 17)
full_sibfamily2017monogamous_pairings <- subset(full_sibfamily2017, full_sibfamily2017$Member2 != "")

nrow(full_sibfamily2017monogamous_pairings) 
# 4 FS families, 8 FS parents

full_sibfamily2018 <- subset(full_sibfamily_chowilla, full_sibfamily_chowilla$year1 == 18)
full_sibfamily2018monogamous_pairings <- subset(full_sibfamily2018, full_sibfamily2018$Member2 != "")

nrow(full_sibfamily2018monogamous_pairings) 
# 6 FS families, 12 FS parents











##############

# Larvae summary statistics on location plus FS plus HS rates weekly yearly etc
####

### All rivers
#need to removed Goulburn 2015 +2016
#how many individuals had FS and HS total
nrow(combined_fullsib)
#248 Full sib dyads
combined_fullsib_study <- subset(combined_fullsib, (combined_fullsib$river1!="B" | combined_fullsib$year1!=15 ))
combined_fullsib_study <- subset(combined_fullsib_study, (combined_fullsib_study$river1!="B" | combined_fullsib_study$year2!=15 ))
combined_fullsib_study <- subset(combined_fullsib_study, (combined_fullsib_study$river2!="B" | combined_fullsib_study$year1!=15 ))
combined_fullsib_study <- subset(combined_fullsib_study, (combined_fullsib_study$river2!="B" | combined_fullsib_study$year2!=15 ))
combined_fullsib_study <- subset(combined_fullsib_study, (combined_fullsib_study$river1!="B" | combined_fullsib_study$year1!=16 ))
combined_fullsib_study <- subset(combined_fullsib_study, (combined_fullsib_study$river1!="B" | combined_fullsib_study$year2!=16 ))
combined_fullsib_study <- subset(combined_fullsib_study, (combined_fullsib_study$river2!="B" | combined_fullsib_study$year1!=16 ))
combined_fullsib_study <- subset(combined_fullsib_study, (combined_fullsib_study$river2!="B" | combined_fullsib_study$year2!=16 ))

vector1 <- combined_fullsib_study$OffspringID1
vector2 <- combined_fullsib_study$OffspringID2
unique_fullsibs <- c(vector1,vector2)
unique_fullsibs <- unique(unique_fullsibs)
length(unique_fullsibs)
#195 offspring had one or more full siblings across the study 
# 195/591 =33% of offspring sequenced have one or more full siblings

nrow(combined_halfsib)

combined_halfsib_study <- subset(combined_halfsib, (combined_halfsib$river1!="B" | combined_halfsib$year1!=15 ))
combined_halfsib_study <- subset(combined_halfsib_study, (combined_halfsib_study$river1!="B" | combined_halfsib_study$year2!=15 ))
combined_halfsib_study <- subset(combined_halfsib_study, (combined_halfsib_study$river2!="B" | combined_halfsib_study$year1!=15 ))
combined_halfsib_study <- subset(combined_halfsib_study, (combined_halfsib_study$river2!="B" | combined_halfsib_study$year2!=15 ))
combined_halfsib_study <- subset(combined_halfsib_study, (combined_halfsib_study$river1!="B" | combined_halfsib_study$year1!=16 ))
combined_halfsib_study <- subset(combined_halfsib_study, (combined_halfsib_study$river1!="B" | combined_halfsib_study$year2!=16 ))
combined_halfsib_study <- subset(combined_halfsib_study, (combined_halfsib_study$river2!="B" | combined_halfsib_study$year1!=16 ))
combined_halfsib_study <- subset(combined_halfsib_study, (combined_halfsib_study$river2!="B" | combined_halfsib_study$year2!=16 ))

#halfsibs that are from either the same year or from different years
sum(combined_halfsib_study$yeardiff == 0)
sum(combined_halfsib_study$yeardiff != 0)




#355 offspring
vector1 <- combined_halfsib_study$OffspringID1
vector2 <- combined_halfsib_study$OffspringID2
unique_halfsibs <- c(vector1,vector2)
unique_halfsibs <- unique(unique_halfsibs)
length(unique_halfsibs)
#275 offspring had one or more half siblings across the study
#275/565 =47% of offspring sequenced had one or more half siblings



#how many individuals are half sibs from different rivers. 1 need to subset so only different rivers are present
#2. need to remove non uniques
# 3. count

HSdiffrivers <- subset(combined_halfsib_study, combined_halfsib_study$river1!=combined_halfsib_study$river2)
# 9 dyads. All from Goulburn and Barmah, none from same year

vector1 <- HSdiffrivers$OffspringID1
vector2 <- HSdiffrivers$OffspringID2
unique_HSdiffrivers <- c(vector1,vector2)
unique_HSdiffrivers <- unique(unique_HSdiffrivers)
length(unique_HSdiffrivers)
# 5 individuals were found to have parents which crossed locations. All HS pairs are from different years.

# HS of different years vs same year
HSdiffyears <- subset(combined_halfsib_study, combined_halfsib_study$year1!=combined_halfsib_study$year2)

vector1 <- HSdiffyears$OffspringID1
vector2 <- HSdiffyears$OffspringID2
unique_HSdiffyears <- c(vector1,vector2)
unique_HSdiffyears <- unique(unique_HSdiffyears)
length(unique_HSdiffyears)

HSsameyears <- subset(combined_halfsib_study, combined_halfsib_study$year1==combined_halfsib_study$year2)
vector1 <- HSsameyears$OffspringID1
vector2 <- HSsameyears$OffspringID2
unique_HSsameyears <- c(vector1,vector2)
unique_HSsameyears <- unique(unique_HSsameyears)
length(unique_HSsameyears)
unique_HSboth <-  c(unique_HSsameyears,unique_HSdiffyears)
unique_HSboth <- unique(unique_HSboth)
# 318 individuals non unique for Same years plus diff years
# 136 unique individuals for same years
# 182 unique individuals for different years
# 275 unique individuals for Half sibs total
# 40 individuals have same year plus different year HS pairs

#Now how many are unrelated to anything. 
# grab those that are HS and those that are FS. Combine and unique. Then total offspring (565) - unique calculated is
# number with no sibs

related_indivs <- c(unique_halfsibs,unique_fullsibs)
length(related_indivs)
#470 indivs
#indivs with both = 470-364 = 106
#18.8% of indivs had both FS and HS pairs

unique_related_indivs <- unique(related_indivs)
length(unique_related_indivs)
#related indivs = 364
#nonrelated = 565-364 = 201 =35.6
#(275-106)/565 = 29.9% have only half siblings
#(195-106)/565 = 15.8% have only full siblings


# Now the specific locations

# Goulburn 

combined_fullsib_goulburn <- subset(combined_fullsib, combined_fullsib$river1=="G" | combined_fullsib$river2=="G")
combined_fullsib_barmah <- subset(combined_fullsib, combined_fullsib$river1=="B" | combined_fullsib$river2=="B")



vector1 <- combined_fullsib_goulburn$OffspringID1
vector2 <- combined_fullsib_goulburn$OffspringID2
unique_fullsibs <- c(vector1,vector2)
unique_fullsibs <- unique(unique_fullsibs)
length(unique_fullsibs)
#111 unique individuals
#351 goulburn individuals
#111/351 =31.6%

combined_halfsib_goulburn <- subset(combined_halfsib, (combined_halfsib$river1=="G" | combined_halfsib$river2=="G" ))
combined_halfsib_goulburn <- subset(combined_halfsib_goulburn, (combined_halfsib_goulburn$river1!="B" | combined_halfsib_goulburn$year1!=15 ))
combined_halfsib_goulburn <- subset(combined_halfsib_goulburn, (combined_halfsib_goulburn$river1!="B" | combined_halfsib_goulburn$year2!=15 ))
combined_halfsib_goulburn <- subset(combined_halfsib_goulburn, (combined_halfsib_goulburn$river2!="B" | combined_halfsib_goulburn$year1!=15 ))
combined_halfsib_goulburn <- subset(combined_halfsib_goulburn, (combined_halfsib_goulburn$river2!="B" | combined_halfsib_goulburn$year2!=15 ))
combined_halfsib_goulburn <- subset(combined_halfsib_goulburn, (combined_halfsib_goulburn$river1!="B" | combined_halfsib_goulburn$year1!=16 ))
combined_halfsib_goulburn <- subset(combined_halfsib_goulburn, (combined_halfsib_goulburn$river1!="B" | combined_halfsib_goulburn$year2!=16 ))
combined_halfsib_goulburn <- subset(combined_halfsib_goulburn, (combined_halfsib_goulburn$river2!="B" | combined_halfsib_goulburn$year1!=16 ))
combined_halfsib_goulburn <- subset(combined_halfsib_goulburn, (combined_halfsib_goulburn$river2!="B" | combined_halfsib_goulburn$year2!=16 ))


nrow(combined_halfsib_goulburn)

#halfsibs that are from either the same year or from different years
sum(combined_halfsib_goulburn$yeardiff == 0)
sum(combined_halfsib_goulburn$yeardiff != 0)

#355 offspring
vector1 <- combined_halfsib_goulburn$OffspringID1
vector2 <- combined_halfsib_goulburn$OffspringID2
unique_halfsibs <- c(vector1,vector2)
unique_halfsibs <- unique(unique_halfsibs)
length(unique_halfsibs)
#189 offspring had one or more half siblings across Goulburn
#189/351 =53.8% of offspring sequenced had one or more half siblings


# 5 individuals were found to have parents which crossed locations. All HS pairs are from different years.

# HS of different years vs same year
HSdiffyears <- subset(combined_halfsib_goulburn, combined_halfsib_goulburn$year1!=combined_halfsib_goulburn$year2)

vector1 <- HSdiffyears$OffspringID1
vector2 <- HSdiffyears$OffspringID2
unique_HSdiffyears <- c(vector1,vector2)
unique_HSdiffyears <- unique(unique_HSdiffyears)
length(unique_HSdiffyears)
#144 half sibs were from different years

HSsameyears <- subset(combined_halfsib_goulburn, combined_halfsib_goulburn$year1==combined_halfsib_goulburn$year2)
vector1 <- HSsameyears$OffspringID1
vector2 <- HSsameyears$OffspringID2
unique_HSsameyears <- c(vector1,vector2)
unique_HSsameyears <- unique(unique_HSsameyears)
length(unique_HSsameyears)
#75 were from the same year
# 
unique_HSboth <-  c(unique_HSsameyears,unique_HSdiffyears)
unique_HSboth <- unique(unique_HSboth)
length(unique_HSboth)
#189 from either or both
# 

# 318 individuals non unique for Same years plus diff years
# 136 unique individuals for same years
# 182 unique individuals for different years
# 275 unique individuals for Half sibs total
# 40 individuals have same year plus different year HS pairs

#Now how many are unrelated to anything. 
# grab those that are HS and those that are FS. Combine and unique. Then total offspring (565) - unique calculated is
# number with no sibs

related_indivs <- c(unique_halfsibs,unique_fullsibs)
length(related_indivs)
#300 indivs with related non unique
#indivs with both = 300-244 = 56
#15.95% of indivs had both FS and HS pairs

unique_related_indivs <- unique(related_indivs)
length(unique_related_indivs)
#244 unique indivs related 

#nonrelated = 351-244 = 107 =30.4%
#(189-56)/351 = 37.89% have only half siblings
#(111-56)/351 = 15.67% have only full siblings




#Barmah

combined_fullsib_barmah <- subset(combined_fullsib, combined_fullsib$river1=="B" | combined_fullsib$river2=="B")



vector1 <- combined_fullsib_barmah$OffspringID1
vector2 <- combined_fullsib_barmah$OffspringID2
unique_fullsibs <- c(vector1,vector2)
unique_fullsibs <- unique(unique_fullsibs)
length(unique_fullsibs)
#51 unique individuals
#127 goulburn individuals
#51/127 =40.15%

combined_halfsib_barmah <- subset(combined_halfsib, (combined_halfsib$river1=="B" | combined_halfsib$river2=="B" ))
combined_halfsib_barmah <- subset(combined_halfsib_barmah, (combined_halfsib_barmah$river1!="B" | combined_halfsib_barmah$year1!=15 ))
combined_halfsib_barmah <- subset(combined_halfsib_barmah, (combined_halfsib_barmah$river1!="B" | combined_halfsib_barmah$year2!=15 ))
combined_halfsib_barmah <- subset(combined_halfsib_barmah, (combined_halfsib_barmah$river2!="B" | combined_halfsib_barmah$year1!=15 ))
combined_halfsib_barmah <- subset(combined_halfsib_barmah, (combined_halfsib_barmah$river2!="B" | combined_halfsib_barmah$year2!=15 ))
combined_halfsib_barmah <- subset(combined_halfsib_barmah, (combined_halfsib_barmah$river1!="B" | combined_halfsib_barmah$year1!=16 ))
combined_halfsib_barmah <- subset(combined_halfsib_barmah, (combined_halfsib_barmah$river1!="B" | combined_halfsib_barmah$year2!=16 ))
combined_halfsib_barmah <- subset(combined_halfsib_barmah, (combined_halfsib_barmah$river2!="B" | combined_halfsib_barmah$year1!=16 ))
combined_halfsib_barmah <- subset(combined_halfsib_barmah, (combined_halfsib_barmah$river2!="B" | combined_halfsib_barmah$year2!=16 ))



nrow(combined_halfsib_barmah)

#halfsibs that are from either the same year or from different years
sum(combined_halfsib_barmah$yeardiff == 0)
sum(combined_halfsib_barmah$yeardiff != 0)



#355 offspring
vector1 <- combined_halfsib_barmah$OffspringID1
vector2 <- combined_halfsib_barmah$OffspringID2
unique_halfsibs <- c(vector1,vector2)
unique_halfsibs <- unique(unique_halfsibs)
length(unique_halfsibs)
#44 offspring had one or more half siblings across Barmah
#44/127 =34.64% of offspring sequenced had one or more half siblings


# 5 individuals were found to have parents which crossed locations. All HS pairs are from different years.

# HS of different years vs same year
HSdiffyears <- subset(combined_fullsib_barmah, combined_fullsib_barmah$year1!=combined_fullsib_barmah$year2)

vector1 <- HSdiffyears$OffspringID1
vector2 <- HSdiffyears$OffspringID2
unique_HSdiffyears <- c(vector1,vector2)
unique_HSdiffyears <- unique(unique_HSdiffyears)
length(unique_HSdiffyears)
#16 half sibs were from different years

HSsameyears <- subset(combined_fullsib_barmah, combined_fullsib_barmah$year1==combined_fullsib_barmah$year2)
vector1 <- HSsameyears$OffspringID1
vector2 <- HSsameyears$OffspringID2
unique_HSsameyears <- c(vector1,vector2)
unique_HSsameyears <- unique(unique_HSsameyears)
length(unique_HSsameyears)
#30 were from the same year
# 
unique_HSboth <-  c(unique_HSsameyears,unique_HSdiffyears)
unique_HSboth <- unique(unique_HSboth)
length(unique_HSboth)
#44 from either or both


# 44 unique individuals for Half sibs total


#Now how many are unrelated to anything. 
# grab those that are HS and those that are FS. Combine and unique. Then total offspring (565) - unique calculated is
# number with no sibs

related_indivs <- c(unique_halfsibs,unique_fullsibs)
length(related_indivs)
#95 indivs with related non unique
#indivs with both = 95-73 = 22
#17.3% of indivs had both FS and HS pairs

unique_related_indivs <- unique(related_indivs)
length(unique_related_indivs)
#73 unique indivs related 

#nonrelated = 127-73 = 54 =42.5%
#(44-22)/127 = 17.3% have only half siblings
#(51-22)/127 = 22.9% have only full siblings




#Chowilla

combined_fullsib_chowilla <- subset(combined_fullsib, combined_fullsib$river1=="C" | combined_fullsib$river2=="C")



vector1 <- combined_fullsib_chowilla$OffspringID1
vector2 <- combined_fullsib_chowilla$OffspringID2
unique_fullsibs <- c(vector1,vector2)
unique_fullsibs <- unique(unique_fullsibs)
length(unique_fullsibs)
#39 unique individuals
#87 chowilla individuals
#39/87 =44.83%

combined_halfsib_chowilla <- subset(combined_halfsib, (combined_halfsib$river1=="C" | combined_halfsib$river2=="C" ))
combined_halfsib_chowilla <- subset(combined_halfsib_chowilla, (combined_halfsib_chowilla$river1!="B" | combined_halfsib_chowilla$year1!=15 ))
combined_halfsib_chowilla <- subset(combined_halfsib_chowilla, (combined_halfsib_chowilla$river1!="B" | combined_halfsib_chowilla$year2!=15 ))
combined_halfsib_chowilla <- subset(combined_halfsib_chowilla, (combined_halfsib_chowilla$river2!="B" | combined_halfsib_chowilla$year1!=15 ))
combined_halfsib_chowilla <- subset(combined_halfsib_chowilla, (combined_halfsib_chowilla$river2!="B" | combined_halfsib_chowilla$year2!=15 ))
combined_halfsib_chowilla <- subset(combined_halfsib_chowilla, (combined_halfsib_chowilla$river1!="B" | combined_halfsib_chowilla$year1!=16 ))
combined_halfsib_chowilla <- subset(combined_halfsib_chowilla, (combined_halfsib_chowilla$river1!="B" | combined_halfsib_chowilla$year2!=16 ))
combined_halfsib_chowilla <- subset(combined_halfsib_chowilla, (combined_halfsib_chowilla$river2!="B" | combined_halfsib_chowilla$year1!=16 ))
combined_halfsib_chowilla <- subset(combined_halfsib_chowilla, (combined_halfsib_chowilla$river2!="B" | combined_halfsib_chowilla$year2!=16 ))



nrow(combined_halfsib_chowilla)

#halfsibs that are from either the same year or from different years
sum(combined_halfsib_chowilla$yeardiff == 0)
sum(combined_halfsib_chowilla$yeardiff != 0)



vector1 <- combined_halfsib_chowilla$OffspringID1
vector2 <- combined_halfsib_chowilla$OffspringID2
unique_halfsibs <- c(vector1,vector2)
unique_halfsibs <- unique(unique_halfsibs)
length(unique_halfsibs)
#47 offspring had one or more half siblings across Barmah
#47/87 =54.02% of offspring sequenced had one or more half siblings


# 5 individuals were found to have parents which crossed locations. All HS pairs are from different years.

# HS of different years vs same year
HSdiffyears <- subset(combined_halfsib_chowilla, combined_halfsib_chowilla$year1!=combined_halfsib_chowilla$year2)

vector1 <- HSdiffyears$OffspringID1
vector2 <- HSdiffyears$OffspringID2
unique_HSdiffyears <- c(vector1,vector2)
unique_HSdiffyears <- unique(unique_HSdiffyears)
length(unique_HSdiffyears)
#23 half sibs were from different years

HSsameyears <- subset(combined_halfsib_chowilla, combined_halfsib_chowilla$year1==combined_halfsib_chowilla$year2)
vector1 <- HSsameyears$OffspringID1
vector2 <- HSsameyears$OffspringID2
unique_HSsameyears <- c(vector1,vector2)
unique_HSsameyears <- unique(unique_HSsameyears)
length(unique_HSsameyears)
#31 were from the same year
# 
unique_HSboth <-  c(unique_HSsameyears,unique_HSdiffyears)
unique_HSboth <- unique(unique_HSboth)
length(unique_HSboth)
#47 from either or both


# 44 unique individuals for Half sibs total


#Now how many are unrelated to anything. 
# grab those that are HS and those that are FS. Combine and unique. Then total offspring (565) - unique calculated is
# number with no sibs

related_indivs <- c(unique_halfsibs,unique_fullsibs)
length(related_indivs)
#86 indivs with related non unique
#indivs with both = 86-58 = 28
#32.2% of indivs had both FS and HS pairs

unique_related_indivs <- unique(related_indivs)
length(unique_related_indivs)
#58 unique indivs related 

#nonrelated = 87-58 = 29 =33.33%
#(47-28)/87 = 21.83% have only half siblings
#(39-28)/87 = 12.7% have only full siblings







#summary

#all rivers
#nonrelated = 565-364 = 201 =35.6
#(275-106)/565 = 29.9% have only half siblings
#(195-106)/565 = 15.8% have only full siblings
#indivs with both = 470-364 = 106
#18.8% of indivs had both FS and HS pairs

# 108 +54 +28= 190 for unrelated added
#190/565 =33.62%
# 133 +22 + 19 +6 = 180 for half sibs
# 180/565 = 31.85%
# 55 + 29 + 11 = 95 for full sibs
# 95/565 = 16.81%
# 56 +22 + 28 = 100 for both
# 100/565 = 17.69%

# Goulburn
#nonrelated = 351-244 = 107 =30.4%
#(189-56)/351 = 37.89% have only half siblings
#(111-56)/351 = 15.67% have only full siblings
#indivs with both = 300-244 = 56
#15.95% of indivs had both FS and HS pairs


#Barmah
#nonrelated = 127-73 = 54 =42.5%
#(44-22)/127 = 17.3% have only half siblings
#(51-22)/127 = 22.9% have only full siblings
#indivs with both = 95-73 = 22
#17.3% of indivs had both FS and HS pairs


#Chowilla

#indivs with both = 86-58 = 28
#32.2% of indivs had both FS and HS pairs
#nonrelated = 87-58 = 29 =33.33%
#(47-28)/87 = 21.83% have only half siblings
#(39-28)/87 = 12.7% have only full siblings
















