
# Supplementary Material C-6

# R code to determine percentage polygamous + monogamous parents and between seasonal 
# detection rates of adults exhibiting either mating strategy


#Novel use of sibship and parental reconstruction sheds light on the mating system of an iconic Australian freshwater fish 


#O'Dwyer J E1-2, Harrisson KA1-3, Tonkin Z3, Lyon J3, Zampatti B4, Koster W3, Raymond S3 Dawson D3, Bice C5, Murphy N1-2




# First how many parents produced more than one offspring in a full sibling family.

# Determnied from the .BestFSFamily output

full_sibfamily <- read.delim("C:/Users/18088076/Dropbox/PhD/Chapters/Recruitment over time/analysis/all rivers/larvae only/colony/newnamed/long/MC_all_larvae_details.BestFSFamily")

colnames(full_sibfamily) <- c('FullSibshipIndex' ,'Prob_Inc',       'Prob_Exc'     ,  'Member1'       ,   'Member2'        ,  'Member3',         
                              'Member4' ,         'Member5'   ,       'Member6'     ,     'Member7'     ,     'Member8'     ,     'Member9')

# subset to 99% certainty 
full_sibfamily2 <- subset(full_sibfamily, full_sibfamily$Prob_Inc >= 0.99 & full_sibfamily$Prob_Exc >= 0.99)

full_sibfamily2$year1 <- NA
full_sibfamily2$river1 <- NA
full_sibfamily2$site1 <- NA

#format
full_sibfamily2$Member1 <- gsub("-", "",full_sibfamily2$Member1)
full_sibfamily2$Member2 <- gsub("-", "",full_sibfamily2$Member2)
full_sibfamily2$Member3 <- gsub("-", "",full_sibfamily2$Member3)
full_sibfamily2$Member4 <- gsub("-", "",full_sibfamily2$Member4)
full_sibfamily2$Member5 <- gsub("-", "",full_sibfamily2$Member5)
full_sibfamily2$Member6 <- gsub("-", "",full_sibfamily2$Member6)
full_sibfamily2$Member7 <- gsub("-", "",full_sibfamily2$Member7)
full_sibfamily2$Member8 <- gsub("-", "",full_sibfamily2$Member8)
full_sibfamily2$Member9 <- gsub("-", "",full_sibfamily2$Member9)

# Only one family bred across 2 years 
# Add in correct locations years and sites
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


#subset by location now
full_sibfamily_goulburn <- subset(full_sibfamily2, full_sibfamily2$river1=="G")
full_sibfamily_barmah <- subset(full_sibfamily2, full_sibfamily2$river1=="B")
full_sibfamily_chowilla <- subset(full_sibfamily2, full_sibfamily2$river1=="C")

# Goulburn first
#subset years
#2014
full_sibfamily2014 <- subset(full_sibfamily_goulburn, full_sibfamily_goulburn$year1 == 14)
nrow(full_sibfamily2014)
#104 adults
# only include parents where 2+ offspring were found
full_sibfamily2014monogamous_pairings <- subset(full_sibfamily2014, full_sibfamily2014$Member2 != "")
nrow(full_sibfamily2014monogamous_pairings) 
# 11 FS families, 22 FS parents

#2015

full_sibfamily2015 <- subset(full_sibfamily_goulburn, full_sibfamily_goulburn$year1 == 15)
nrow(full_sibfamily2015)


full_sibfamily2015monogamous_pairings <- subset(full_sibfamily2015, full_sibfamily2015$Member2 != "")
# number of adults contributing to full sibling pairs (two adults per offspring pair so 2*nrow)
nrow(full_sibfamily2015monogamous_pairings) 
# 20 adults 

#2016 
full_sibfamily2016 <- subset(full_sibfamily_goulburn, full_sibfamily_goulburn$year1 == 16)
nrow(full_sibfamily2016)


full_sibfamily2016monogamous_pairings <- subset(full_sibfamily2016, full_sibfamily2016$Member2 != "")

# number of adults contributing to full sibling pairs (two adults per offspring pair so 2*nrow)
nrow(full_sibfamily2016monogamous_pairings) 
# 12 adults contributed

# 2017
full_sibfamily2017 <- subset(full_sibfamily_goulburn, full_sibfamily_goulburn$year1 == 17)
nrow(full_sibfamily2017)

full_sibfamily2017monogamous_pairings <- subset(full_sibfamily2017, full_sibfamily2017$Member2 != "")
# number of adults contributing to full sibling pairs (two adults per offspring pair so 2*nrow)
nrow(full_sibfamily2017monogamous_pairings) 
# 14 adults


#2018
full_sibfamily2018 <- subset(full_sibfamily_goulburn, full_sibfamily_goulburn$year1 == 18)
nrow(full_sibfamily2018)

full_sibfamily2018monogamous_pairings <- subset(full_sibfamily2018, full_sibfamily2018$Member2 != "")

# number of adults contributing to full sibling pairs (two adults per offspring pair so 2*nrow)
nrow(full_sibfamily2018monogamous_pairings) 
#22 adults


# now Barmah

# 2017
full_sibfamily2017 <- subset(full_sibfamily_barmah, full_sibfamily_barmah$year1 == 17)
nrow(full_sibfamily2017)

full_sibfamily2017monogamous_pairings <- subset(full_sibfamily2017, full_sibfamily2017$Member2 != "")
# number of adults contributing to full sibling pairs (two adults per offspring pair so 2*nrow)
nrow(full_sibfamily2017monogamous_pairings) 
# 10 adults


#2018
full_sibfamily2018 <- subset(full_sibfamily_barmah, full_sibfamily_barmah$year1 == 18)
nrow(full_sibfamily2018)

full_sibfamily2018monogamous_pairings <- subset(full_sibfamily2018, full_sibfamily2018$Member2 != "")

# number of adults contributing to full sibling pairs (two adults per offspring pair so 2*nrow)
nrow(full_sibfamily2018monogamous_pairings) 
#14 adults

# now chowilla

# 2017
full_sibfamily2017 <- subset(full_sibfamily_chowilla, full_sibfamily_chowilla$year1 == 17)
nrow(full_sibfamily2017)

full_sibfamily2017monogamous_pairings <- subset(full_sibfamily2017, full_sibfamily2017$Member2 != "")
# number of adults contributing to full sibling pairs (two adults per offspring pair so 2*nrow)
nrow(full_sibfamily2017monogamous_pairings) 
# 10 adults


#2018
full_sibfamily2018 <- subset(full_sibfamily_chowilla, full_sibfamily_chowilla$year1 == 18)
nrow(full_sibfamily2018)

full_sibfamily2018monogamous_pairings <- subset(full_sibfamily2018, full_sibfamily2018$Member2 != "")

# number of adults contributing to full sibling pairs (two adults per offspring pair so 2*nrow)
nrow(full_sibfamily2018monogamous_pairings) 
#14 adults



# Adults who produced full siblings have been completed



# Next is to calculate the number of adults who bred producing one or more half siblings
# This step was calculated more by had based on the sibling dyads calculated in Supplementary material 3.
# Each polygamous pairing was mapped out across each year year and the number of adults were calculated each year. 
# This was achieved through viewing each generated polygamous pairings table
# e.g. polygamous_pairings_2014
View(polygamous_pairings_2014)
#Each offspring dyad was then drawn as a triad with a potential parent in the middle. # This was repeated for all offspring dyads
# and the total adults which undertook polygamous relationships were counted and the total number of adult pairs which both mated with 
# a second additional partner were counted as examples of both sexes undergoing polygamy 



# Part 2, repeat adult detection and between seasonal rates of each mating strategy


# Does the sampling design preferentially detect half siblings over long term pair bonds

# Break this down. What parents were found to both breed separately in different years later on? If both were redetected independently they
# should be at least somewhat likely to be detected as a pair together if they actually were also forming long term pair bonds

# change Colony standard symbols to M and F to allow for better grepl and sub commands (let's not use regex expressions in the names)
bestcluster$FatherID <- gsub("*", "M" , bestcluster$FatherID, fixed=TRUE) 

bestcluster$MotherID <- gsub("#", "F" , bestcluster$MotherID, fixed=TRUE)




bestcluster$F_alt1 <- "unknown"
bestcluster$F_alt2 <- "unknown"
bestcluster$F_alt3 <- "unknown"
bestcluster$F_alt4 <- "unknown"
bestcluster$F_alt5 <- "unknown"
bestcluster$F_alt6 <- "unknown"
bestcluster$Female_altname1 <- "unknown"
bestcluster$Female_altname2 <- "unknown"
bestcluster$Female_altname3 <- "unknown"
bestcluster$Female_altname4 <- "unknown"
bestcluster$Female_altname5 <- "unknown"
bestcluster$Female_altname6 <- "unknown"

bestcluster$M_alt1 <- "unknown"
bestcluster$M_alt2 <- "unknown"
bestcluster$M_alt3 <- "unknown"
bestcluster$M_alt4 <- "unknown"
bestcluster$M_alt5 <- "unknown"
bestcluster$M_alt6 <- "unknown"
bestcluster$Male_altname1 <- "unknown"
bestcluster$Male_altname2 <- "unknown"
bestcluster$Male_altname3 <- "unknown"
bestcluster$Male_altname4 <- "unknown"
bestcluster$Male_altname5 <- "unknown"
bestcluster$Male_altname6 <- "unknown"

#(bestcluster$year1[i] != bestcluster$year1[j])

sum(grepl(bestcluster$MotherID[j],bestcluster[i,17:22]))==0

for (i in (c (1:nrow(bestcluster)))){
  j=1
  
  for (j in (c(1:nrow(bestcluster)))) {
    
    
    if((bestcluster$FatherID[i] == bestcluster$FatherID[j]) & (sum(grepl(bestcluster$MotherID[j],bestcluster[i,17:22]))==0) & (bestcluster$F_alt1[i] == "unknown") & n==0 ) {
      
      
      bestcluster$F_alt1[i] <-bestcluster$year1[j]
      bestcluster$Female_altname1[i] <-bestcluster$MotherID[j]
      n=1
    }
    
    if((bestcluster$FatherID[i] == bestcluster$FatherID[j]) & (sum(grepl(bestcluster$MotherID[j],bestcluster[i,17:22]))==0) & (bestcluster$F_alt1[i] != "unknown") & (bestcluster$F_alt2[i] == "unknown") & n==0 ) {
      
      
      bestcluster$F_alt2[i] <-bestcluster$year1[j]
      bestcluster$Female_altname2[i] <-bestcluster$MotherID[j]
      n=1
    }
    
    if((bestcluster$FatherID[i] == bestcluster$FatherID[j]) & (sum(grepl(bestcluster$MotherID[j],bestcluster[i,17:22]))==0) & (bestcluster$F_alt2[i] != "unknown") & (bestcluster$F_alt3[i] == "unknown")& n==0 ) {
      
      
      bestcluster$F_alt3[i] <-bestcluster$year1[j]
      bestcluster$Female_altname3[i] <-bestcluster$MotherID[j]
      n=1
    }
    
    if((bestcluster$FatherID[i] == bestcluster$FatherID[j]) & (sum(grepl(bestcluster$MotherID[j],bestcluster[i,17:22]))==0) & (bestcluster$F_alt3[i] != "unknown") & (bestcluster$F_alt4[i] == "unknown") & n==0 ) {
      
      
      bestcluster$F_alt4[i] <-bestcluster$year1[j]
      bestcluster$Female_altname4[i] <-bestcluster$MotherID[j]
      n=1
    }
    
    
    if((bestcluster$FatherID[i] == bestcluster$FatherID[j]) & (sum(grepl(bestcluster$MotherID[j],bestcluster[i,17:22]))==0) & (bestcluster$F_alt4[i] != "unknown") & (bestcluster$F_alt5[i] == "unknown") & n==0 ) {
      
      
      bestcluster$F_alt5[i] <-bestcluster$year1[j]
      bestcluster$Female_altname5[i] <-bestcluster$MotherID[j]
      n=1
    }
    
    if((bestcluster$FatherID[i] == bestcluster$FatherID[j]) & (sum(grepl(bestcluster$MotherID[j],bestcluster[i,17:22]))==0)  & (bestcluster$F_alt5[i] != "unknown") & (bestcluster$F_alt6[i] == "unknown") & n==0 ) {
      
      
      bestcluster$F_alt6[i] <-bestcluster$year1[j]
      bestcluster$Female_altname6[i] <-bestcluster$MotherID[j]
      n=1
    }
    n=0
  }
  
}


n=0 
(sum(grepl(bestcluster$FatherID[j],bestcluster[i,29:34]))==0)

for (i in (c (1:nrow(bestcluster)))) {
  j=1
  
  for (j in (c(1:nrow(bestcluster)))) {
    
    
    if((bestcluster$MotherID[i] == bestcluster$MotherID[j])  & (sum(grepl(bestcluster$FatherID[j],bestcluster[i,29:34]))==0) & (bestcluster$M_alt1[i] == "unknown") & n==0) {
      
      
      bestcluster$M_alt1[i] <-bestcluster$year1[j]
      bestcluster$Male_altname1[i] <-bestcluster$FatherID[j]
      n=1
    }
    
    if((bestcluster$MotherID[i] == bestcluster$MotherID[j]) & (bestcluster$M_alt1[i] != "unknown") & (sum(grepl(bestcluster$FatherID[j],bestcluster[i,29:34]))==0) & (bestcluster$M_alt2[i] == "unknown") & n==0){
      
      
      bestcluster$M_alt2[i] <-bestcluster$year1[j]
      bestcluster$Male_altname2[i] <-bestcluster$FatherID[j]
      n=1
    }
    
    if((bestcluster$MotherID[i] == bestcluster$MotherID[j]) & (bestcluster$M_alt2[i] != "unknown") & (sum(grepl(bestcluster$FatherID[j],bestcluster[i,29:34]))==0) & (bestcluster$M_alt3[i] == "unknown") & n==0){
      
      
      bestcluster$M_alt3[i] <-bestcluster$year1[j]
      bestcluster$Male_altname3[i] <-bestcluster$FatherID[j]
      n=1
    }
    
    if((bestcluster$MotherID[i] == bestcluster$MotherID[j]) & (bestcluster$M_alt3[i] != "unknown") & (sum(grepl(bestcluster$FatherID[j],bestcluster[i,29:34]))==0) & (bestcluster$M_alt4[i] == "unknown")& n==0 ){
      
      
      bestcluster$M_alt4[i] <-bestcluster$year1[j]
      bestcluster$Male_altname4[i] <-bestcluster$FatherID[j]
      n=1
    }
    
    
    if((bestcluster$MotherID[i] == bestcluster$MotherID[j]) & (bestcluster$M_alt4[i] != "unknown") & (sum(grepl(bestcluster$FatherID[j],bestcluster[i,29:34]))==0) & (bestcluster$M_alt5[i] == "unknown") & n==0){
      
      
      bestcluster$M_alt5[i] <-bestcluster$year1[j]
      bestcluster$Male_altname5[i] <-bestcluster$FatherID[j]
      n=1
    }
    
    if((bestcluster$MotherID[i] == bestcluster$MotherID[j]) & (bestcluster$M_alt5[i] != "unknown") & (sum(grepl(bestcluster$FatherID[j],bestcluster[i,29:34]))==0) & (bestcluster$M_alt6[i] == "unknown") & n==0){
      
      
      bestcluster$M_alt6[i] <-bestcluster$year1[j]
      bestcluster$Male_altname6[i] <-bestcluster$FatherID[j]
      n=1
    }
    n=0
    
  }
  
}

bestcluster[bestcluster == "unknown"] <- NA


bestcluster$yearsummalepairs <- NA
bestcluster$yearsumfemalepairs <- NA


for ( i in (c(1:nrow(bestcluster)))) {
  
  if(sum(unique(as.numeric(bestcluster[i,11:16])),na.rm = TRUE) >=20) {
    
    bestcluster$yearsumfemalepairs[i] <- sum(unique(as.numeric(bestcluster[i,11:16])),na.rm = TRUE)
    
  }
  else {
    bestcluster$yearsumfemalepairs[i] <- 0
  }
  
}
for ( i in (c(1:nrow(bestcluster)))) {
  
  if(sum(unique(as.numeric(bestcluster[i,23:28])),na.rm = TRUE) >=20) {
    
    bestcluster$yearsummalepairs[i] <- sum(unique(as.numeric(bestcluster[i,23:28])),na.rm = TRUE)
    
  }
  else {
    bestcluster$yearsummalepairs[i] <- 0
  }
  
}




Bestcluster_parents_either_breeding <- subset(bestcluster, !is.na(bestcluster$Female_altname2) | !is.na(bestcluster$Male_altname2))
Bestcluster_parents_dual_breeding <- subset(bestcluster, !is.na(bestcluster$Female_altname2) & !is.na(bestcluster$Male_altname2))



Bestcluster_parents_either_breeding_across_years <- subset(Bestcluster_parents_either_breeding, Bestcluster_parents_either_breeding$yearsummalepairs >=20 | Bestcluster_parents_either_breeding$yearsumfemalepairs >=20)
Bestcluster_parents_dual_breeding_across_years <- subset(Bestcluster_parents_dual_breeding, Bestcluster_parents_dual_breeding$yearsummalepairs >=20 & Bestcluster_parents_dual_breeding$yearsumfemalepairs >=20)

dadsstrict <- as.data.frame(Bestcluster_parents_dual_breeding_across_years$FatherID)
mumsstrict <- as.data.frame(Bestcluster_parents_dual_breeding_across_years$MotherID)



rownames(dadsstrict) <- 31:60
rownames(mumsstrict)

colnames(dadsstrict) <- "inds"
colnames(mumsstrict) <- "inds"

both_parstrict <- rbind(mumsstrict, dadsstrict)

length(unique(both_parstrict$inds))
# 41 adults were sampled independently multiple times in different years
# 37 inds were found to breed over multiple years, and had a partner who was detected over multiple years
# 

# From manually counting (Bestcluster_parents_dual_breeding, scanning through the number of pairs and number of years)
# 29 inds had a partner where they only bred once in any detected season.


# Now how many adults bred a second time all up (not both adults in a pair together just adults breeding a second year)

female_multibreed <- subset(Bestcluster_parents_either_breeding_across_years, !is.na(Bestcluster_parents_either_breeding_across_years$Male_altname2))
male_multibreed <- subset(Bestcluster_parents_either_breeding_across_years, !is.na(Bestcluster_parents_either_breeding_across_years$Female_altname2))
dadseither <- as.data.frame(male_multibreed$FatherID)
mumseither <- as.data.frame(female_multibreed$MotherID)

rownames(dadseither) <- 125:245
rownames(mumseither)

colnames(dadseither) <- "inds"
colnames(mumseither) <- "inds"

both_pareither <- rbind(mumseither, dadseither)

length(unique(both_pareither$inds))

# 96 adults were found to have breed in a second year

# How are these distributed in different reaches though.

# first let's sort through all the groups properly
Chowilla_dual_breeding <- subset(Bestcluster_parents_dual_breeding_across_years, Bestcluster_parents_dual_breeding_across_years$river1 == "C")
Barmah_dual_breeding <- subset(Bestcluster_parents_dual_breeding_across_years, Bestcluster_parents_dual_breeding_across_years$river1 == "B")
Goulburn_dual_breeding <- subset(Bestcluster_parents_dual_breeding_across_years, Bestcluster_parents_dual_breeding_across_years$river1 == "G")

# Chowilla doesn't have any. 

dadsstrictbarmah <- as.data.frame(Barmah_dual_breeding$FatherID)
mumsstrictbarmah <- as.data.frame(Barmah_dual_breeding$MotherID)



rownames(dadsstrictbarmah) <- 7:12
rownames(mumsstrictbarmah)

colnames(dadsstrictbarmah) <- "inds"
colnames(mumsstrictbarmah) <- "inds"

both_parstrictbarmah <- rbind(mumsstrictbarmah, dadsstrictbarmah)


length(unique(both_parstrictbarmah$inds))

# 4 inds are from Barmah

Chowilla_dual_breeding <- subset(Bestcluster_parents_dual_breeding_across_years, Bestcluster_parents_dual_breeding_across_years$river1 == "C")
Barmah_dual_breeding <- subset(Bestcluster_parents_dual_breeding_across_years, Bestcluster_parents_dual_breeding_across_years$river1 == "B")
Goulburn_dual_breeding <- subset(Bestcluster_parents_dual_breeding_across_years, Bestcluster_parents_dual_breeding_across_years$river1 == "G")



dadsstrictgoulburn<- as.data.frame(Goulburn_dual_breeding$FatherID)
mumsstrictgoulburn <- as.data.frame(Goulburn_dual_breeding$MotherID)



rownames(dadsstrictgoulburn) <- 25:48
rownames(mumsstrictgoulburn)

colnames(dadsstrictgoulburn) <- "inds"
colnames(mumsstrictgoulburn) <- "inds"

both_parstrictgoulburn <- rbind(mumsstrictgoulburn, dadsstrictgoulburn)


length(unique(both_parstrictgoulburn$inds))



# 33 are from Goulburn.


# Now how about the any of the adults breeding a second year? 

male_multibreed
female_multibreed


male_multibreedchowilla <- subset(male_multibreed,male_multibreed$river1=="C")
female_multibreedchowilla <- subset(female_multibreed,female_multibreed$river1=="C")
male_multibreedbarmah <- subset(male_multibreed,male_multibreed$river1=="B")
female_multibreedbarmah<- subset(female_multibreed,female_multibreed$river1=="B")
male_multibreedgoulburn <- subset(male_multibreed,male_multibreed$river1=="G")
female_multibreedgoulburn<- subset(female_multibreed,female_multibreed$river1=="G")


# chowilla

dadseitherchowilla <- as.data.frame(male_multibreedchowilla$FatherID)
mumseitherchowilla <- as.data.frame(female_multibreedchowilla$MotherID)

rownames(dadseitherchowilla) <- 11:13
rownames(mumseitherchowilla)

colnames(dadseitherchowilla) <- "inds"
colnames(mumseitherchowilla) <- "inds"

both_pareitherchowilla <- rbind(dadseitherchowilla, mumseitherchowilla)

length(unique(both_pareitherchowilla$inds))

# 6 inds from Chowilla bred in multiple across years


# barmah


dadseitherbarmah <- as.data.frame(male_multibreedbarmah$FatherID)
mumseitherbarmah<- as.data.frame(female_multibreedbarmah$MotherID)

rownames(dadseitherbarmah) <- 22:42
rownames(mumseitherbarmah)

colnames(dadseitherbarmah) <- "inds"
colnames(mumseitherbarmah) <- "inds"

both_pareitherbarmah <- rbind(dadseitherbarmah, mumseitherbarmah)

length(unique(both_pareitherbarmah$inds))

# 19 inds from barmah were found to breed over multiple years

# Goulburn


dadseithergoulburn <- as.data.frame(male_multibreedgoulburn$FatherID)
mumseithergoulburn<- as.data.frame(female_multibreedgoulburn$MotherID)

rownames(dadseithergoulburn) <- 94:190
rownames(mumseithergoulburn)

colnames(dadseithergoulburn) <- "inds"
colnames(mumseithergoulburn) <- "inds"

both_pareitherbarmah <- rbind(dadseithergoulburn, mumseithergoulburn)

length(unique(both_pareitherbarmah$inds))


# 76 inds from Goulburn. 

# remembering there were inds that migrated between to remove doubles. This comes down to 
# 71 from Goulburn
# 14 from Barmah
# 6 from Chowilla
# and 5 from Barmah/Goulburn


# what about total adults per reach
length(unique(bestcluster$FatherID))
length(unique(bestcluster$MotherID))
# 509 total

# now each reach

bestclusterGoulburn <- subset(bestcluster,bestcluster$river1=="G")
bestclusterBarmah <- subset(bestcluster,bestcluster$river1=="B")
bestclusterChowilla <- subset(bestcluster,bestcluster$river1=="C")


length(unique(bestclusterGoulburn$FatherID))
length(unique(bestclusterGoulburn$MotherID))

# 316 inds

length(unique(bestclusterBarmah$FatherID))
length(unique(bestclusterBarmah$MotherID))

#143 inds

length(unique(bestclusterChowilla$FatherID))
length(unique(bestclusterChowilla$MotherID))

#58 inds

# But remembering the double ups that occur between Goulburn and Barmah, minus the 4 inds from each for the 8 pairs found.
# Goulburn= 312 adults, Barmah = 143 adults, Chowilla = 58 adults


bestclusterdadssecond <- subset(bestcluster,bestcluster$F_alt1!="unknown")


length(unique(bestclusterdadssecond$FatherID))
# 41 dads

bestclustermumsssecond <- subset(bestcluster,bestcluster$M_alt1!="unknown")


length(unique(bestclustermumsssecond$MotherID))
# 43 mums 


dads_all <- as.data.frame(bestcluster$FatherID)
mums_all <- as.data.frame(bestcluster$MotherID)



colnames(dads_all) <- "inds"
colnames(mums_all) <- "inds"

both_par_all <- rbind(dads_all, mums_all)

length(unique(both_par_all$inds))




# How many adults moved between barmah and Goulburn

#It could be automated but there are 9 pairs so let's just look at the cluster file manually here 

sum(combined_halfsib$river1 != combined_halfsib$river2)

migrants <- subset(combined_halfsib,combined_halfsib$river1 != combined_halfsib$river2)


# *76 #88   *157  #88
# *76 #88   *157  #88
# *60 #69   *60   #309
# *60 #69   *60  #274
# *43 #50   *296 #50
# *43 #50   *296 #50
# *43 #50   *296 #50
# *43 #50    *296 #50
#
# so #88 migrated, *60 migrated, #50 migrated




