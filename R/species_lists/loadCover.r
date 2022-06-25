###############################################
# 
# ##cover data
# my.GR.data <- tbl(con, "turfCommunity") %>%
#   collect() %>% 
#   bind_rows(problems.cover) %>%
#   left_join(tbl(con, "taxon"), copy = TRUE) %>%
#   left_join(tbl(con, "turfs"), copy = TRUE) %>%
#   left_join(tbl(con, "plots"), by = c("destinationPlotID" = "plotID"), copy = TRUE) %>%
#   left_join(tbl(con, "blocks"), by = "blockID", copy = TRUE) %>%
#   left_join(tbl(con, "sites"), by = "siteID", copy = TRUE) %>%
#   left_join(tbl(con, "turfEnvironment"), copy = TRUE) %>%
#   select(siteID, blockID, destinationPlotID, originPlotID, turfID, TTtreat, Year = year, species, cover, Temperature_level, Precipitation_level, recorder, totalVascular, functionalGroup, vegetationHeight) %>%
#   filter(!is.na(TTtreat)) %>% 
#   collect()
# 
# #incorporate these into the code...
# 
# cover$NID.herb <- NULL
# cover <- cover[!is.na(rowSums(cover)),]
# 
# cover$Car.cap <- cover$Car.Cap + cover$Car.cap
# cover$Car.Cap <- NULL
# 
# cover$Gen.sp <- cover$Gen.sp.
# cover$Gen.sp. <- NULL
# 
# cover$Agr.cap <- cover$Agr.cap + cover$Agr.can
# cover$Agr.can <- NULL
# 
# cover$Pyr.sp <- cover$Pyr.min + cover$Pyr.rot
# cover$Pyr.min <- NULL
# cover$Pyr.rot <- NULL
# 
# cover$Epi.sp <- cover$Epi.sp + cover$Epi.ana
# cover$Epi.ana <- NULL

cover.thin <- tbl(con, sql("SELECT sites.siteID, blocks.blockID, turfs.TTtreat,turfs.turfID, dest_blocks.blockID AS destBlockID, (SELECT Count(subTurfEnvironment.bad) AS CountOfbad
FROM subTurfEnvironment where (subTurfEnvironment.year = turfCommunity.year) AND (subTurfEnvironment.turfID = turfCommunity.turfID)
 AND ( (subTurfEnvironment.bad)='')) AS notbad, sites.Temperature_level, sites.Summertemperature_gridded as summerTemperature, sites.Annualprecipitation_gridded as annualPrecipitation, sites.Precipitation_level, turfCommunity.Year, turfCommunity.species, turfCommunity.cover, turfEnvironment.recorder , dest_blocks.siteID as destSiteID
FROM (((blocks AS dest_blocks INNER JOIN plots AS dest_plots ON dest_blocks.blockID = dest_plots.blockID) INNER JOIN (((sites INNER JOIN blocks ON sites.siteID = blocks.siteID) INNER JOIN plots ON blocks.blockID = plots.blockID) 
INNER JOIN turfs ON plots.plotID = turfs.originPlotID) ON dest_plots.plotID = turfs.destinationPlotID) INNER JOIN turfCommunity ON turfs.turfID = turfCommunity.turfID) INNER JOIN turfEnvironment ON (turfEnvironment.year = turfCommunity.Year) AND (turfs.turfID = turfEnvironment.turfID)
WHERE NOT turfs.TTtreat='' AND ((Not (turfCommunity.Year)=2010))")) %>% 
  collect()

cover.thin
                                       
#correct for stomping
cover.thin %>% count(notbad)

# stompingQ<-"SELECT blocks.siteID, blocks.blockID, turfs.turfID, subTurfEnvironment.year, turfs.TTtreat, Count(subTurfEnvironment.bad) AS CountOfbad
# FROM blocks INNER JOIN (plots INNER JOIN (turfs INNER JOIN subTurfEnvironment ON turfs.turfID = subTurfEnvironment.turfID) ON plots.plotID = turfs.destinationPlotID) ON blocks.blockID = plots.blockID
# GROUP BY blocks.siteID, blocks.blockID, turfs.turfID, subTurfEnvironment.year, turfs.TTtreat, subTurfEnvironment.bad
# HAVING (((subTurfEnvironment.bad)='x'))"
# stomping <- tbl(con, sql(stompingQ)) %>% collect()
# #stomping <- filter(stomping, is.na(TTtreat)|TTtreat == "TTC") %>% 
# #  distinct()

#delete turfs with too much stomping  
cover.thin <- cover.thin %>% filter(cover.thin$notbad > 10)


 #correct covers for stomping
max(cover.thin$cover)
cover.thin$cover <- cover.thin$cover*25/cover.thin$notbad
sort(cover.thin$cover, decreasing = TRUE)[1:10]      
cover.thin$cover[cover.thin$cover > 80] <- 80#stop doubtfully high values                                 

#correct for botanist effects
cover.thin$recorder[is.na(cover.thin$recorder)] <- "unknown botanist"
#PM
cover.thin$cover[cover.thin$recorder == "PM"] <- cover.thin$cover[cover.thin$recorder=="PM"] * 1.20
#Siri
siri <- tbl(con, "sites") %>% 
  inner_join(tbl(con, "blocks"), by  = "siteID") %>% 
  inner_join(tbl(con, "plots"), by = "blockID") %>% 
  inner_join(tbl(con, "turfs"), by = c("plotID" = "destinationPlotID")) %>% 
  inner_join(tbl(con, "turfEnvironment"), by = "turfID") %>% 
  inner_join(tbl(con, "TurfCommunity"), by = c("turfID", "year")) %>% 
  filter(recorder == "Siri") %>% 
  group_by(turfID, year, date, totalVascular, TTtreat, Temperature_level) %>% 
  summarise(SumOfCover = sum(cover)) %>% 
  collect()  
  


# tbl(con, sql("SELECT turfs.turfID, TurfCommunity.Year, turfEnvironment.date, Sum(TurfCommunity.cover) AS SumOfcover, turfEnvironment.totalVascular, turfs.TTtreat, sites.Temperature_level
# FROM ((sites INNER JOIN blocks ON sites.siteID = blocks.siteID) INNER JOIN plots ON blocks.blockID = plots.blockID) INNER JOIN ((turfs INNER JOIN turfEnvironment ON turfs.turfID = turfEnvironment.turfID) INNER JOIN TurfCommunity ON (TurfCommunity.Year = turfEnvironment.year) AND (turfs.turfID = TurfCommunity.turfID)) ON plots.plotID = turfs.destinationPlotID
# WHERE (((turfEnvironment.recorder)='Siri'))
# GROUP BY turfs.turfID, TurfCommunity.Year, turfEnvironment.date, turfEnvironment.totalVascular, turfs.TTtreat, sites.Temperature_level
# HAVING ((Not (turfs.TTtreat)=''))
# ORDER BY TurfCommunity.Year, turfEnvironment.date, Sum(TurfCommunity.cover) DESC")) 

siriLOW <- siri[siri$SumOfCover/siri$totalVascular < 1.35,]

siri.fix <- paste(cover.thin$turfID, cover.thin$year) %in% paste(siriLOW$turfID, siriLOW$year)

table(siri.fix,cover.thin$recorder)

cover.thin$cover[siri.fix] <- cover.thin$cover[siri.fix] * 1.3

#John's corrections
cover.thin <- cover.thin %>% 
  mutate(cover = if_else(turfID == '111 TT2 137' & year == 2011 & species == 'Agr.cap', 25, cover)) %>% 
  mutate(cover = if_else(turfID == '32 TT3 109' & year == 2009, cover/2, cover), 
         cover = if_else(turfID == '32 TT3 109' & year == 2012, cover * 2/3, cover), 
         cover = if_else(turfID == '33 TT2 58' & year == 2009, cover * 2/3, cover),
         cover = if_else(turfID == '34 TT1 32' & year == 2009, cover/2, cover), 
         cover = if_else(turfID == '40 TT2 62' & year == 2011, cover * 2/3, cover)) 
  
  
#cover['111 TT2 137_2011', 'Agr.cap'] <- 25
# cover['32 TT3 109_2009', ] <- cover['32 TT3 109_2009', ] / 2
# cover['32 TT3 109_2012', ] <- cover['32 TT3 109_2012', ] * 2 / 3
# cover['33 TT2 58_2009', ] <- cover['33 TT2 58_2009', ] * 2 / 3
# cover['34 TT1 32_2009', ] <- cover['34 TT1 32_2009', ] / 2
# cover['40 TT2 62_2011', ] <- cover['40 TT2 62_2011', ] * 2 / 3 


# make fat table
cover <- cover.thin %>% spread(key = species, value = cover)

#make meta data
cover.meta <- cover %>% select(siteID:destSiteID) %>% 
  mutate(TTtreat = factor(TTtreat, levels = c("TTC","TT1", "TT2", "TT3", "TT4")))
  
  
turfs <- cover.meta[!duplicated(cover.meta$turfID),]

cover <- cover %>% select(-(siteID:destSiteID))

#clear up
rm(siri.fix, siriLOW, siri)
         



#set NID.seedling to  0/1
#cover$NID.seedling <- ifelse(cover$NID.seedling > 0,1,0) # leave this out for the moment
         
table(cover.meta$turfID, cover.meta$year)   
table(cover.meta$year, cover.meta$siteID, cover.meta$TTtreat)         
                                       
alltaxa <- TRUE
propertaxa <- !names(cover) %in% c("NID.seedling", "Car.sp", "Hie.sp", "Luz.sp",  "NID.gram", "NID.herb", "NID.rosett", "Pyr.sp")
noNIDseedlings <- !names(cover) %in% c("NID.seedling")

turfs$newTT <- turfs$TTtreat  #alternative TTtreat with combined controls
levels(turfs$newTT)[1:2] <- "control"

save(cover, cover.thin, cover.meta, turfs, file = "cover.Rdata")
