
# 04 Descriptive bibliometrics plotting ----

# # Import data
cat("loading data...\n")
load(paste(here(), "/literature.RData", sep=""))

literature$AEorERE <- as.factor(ifelse(literature$ISOSourceAbbreviation %in% toupper(c("Agric. Econ.","Appl. Econ. Perspect. Policy","Am. J. Agr. Econ.","Aust. J. Agr. Resour. Econ.","Can. J. Agric. Econ.-Rev. Can. Agroecon.","Eur. Rev. Agric. Econ.","Food Policy","Agribusiness","J. Agric. Econ.","J. Agric. Resour. Econ.")), "AE","ERE"))


# output per year and field ----
yearPlot <- ggplot(meta, aes(year)) +
  geom_histogram(binwidth = 1, fill = "darkgrey") +
  xlab("") +
  ylab("") +
  ggtitle("total") +
  scale_y_continuous(limits = c(0, 1550))
yearPlot_AE <- ggplot(meta[meta$AEorERE=="AE",], aes(year)) +
  geom_histogram(binwidth = 1, fill = "darkgrey") +
  xlab("year") +
  ylab("")+
  ggtitle("AE") +
  scale_y_continuous(limits = c(0, 1550))
yearPlot_ERE <- ggplot(meta[meta$AEorERE=="ERE",], aes(year)) +
  geom_histogram(binwidth = 1, fill = "darkgrey") +
  xlab("") +
  ylab("")+
  ggtitle("ERE") +
  scale_y_continuous(limits = c(0, 1550))

cowplot::plot_grid(yearPlot,yearPlot_AE,yearPlot_ERE, align = "hv", ncol=3)
ggsave(paste(here(), "/figs/year_plots_descript.png", sep=""), width=21, height=7, units = "cm")

# Authors ----
listAU = strsplit(as.character(literature$Authors), split="; ")
listAU = lapply(listAU, function(l) trim(l))
AU = unlist(listAU)
rm("listAU")
Authors = as_tibble(sort(table(AU), decreasing = TRUE))
rm("AU")

# Authors_AE
listAU_AE = strsplit(as.character(literature[literature$AEorERE=="AE","Authors"]), split="; ")
listAU_AE = lapply(listAU_AE, function(l) trim(l))
AU_AE = unlist(listAU_AE)
rm("listAU_AE")
Authors_AE = as_tibble(sort(table(AU_AE), decreasing = TRUE))
rm("AU_AE")

# Authors_ERE
listAU_ERE = strsplit(as.character(literature[literature$AEorERE=="ERE","Authors"]), split="; ")
listAU_ERE = lapply(listAU_ERE, function(l) trim(l))
AU_ERE = unlist(listAU_ERE)
rm("listAU_ERE")
Authors_ERE = as_tibble(sort(table(AU_ERE), decreasing = TRUE))
rm("AU_ERE")

authPlot <- ggplot(Authors[1:10,]) +
  geom_col(aes(x=reorder(AU, n),y=n), fill = "darkgrey")+
  scale_y_continuous(limits = c(0, 81)) +
  coord_flip() +
  xlab("") +
  ylab("") +
  ggtitle("total")

authPlot_AE <- ggplot(Authors_AE[1:10,]) +
  geom_col(aes(x=reorder(AU_AE, n),y=n), fill = "darkgrey")+
  scale_y_continuous(limits = c(0, 81)) +
  coord_flip() +
  xlab("") +
  ylab("# articles") +
  ggtitle("AE")

authPlot_ERE <- ggplot(Authors_ERE[1:10,]) +
  geom_col(aes(x=reorder(AU_ERE, n),y=n), fill = "darkgrey")+
  scale_y_continuous(limits = c(0, 81)) +
  coord_flip() +
  xlab("") +
  ylab("") +
  ggtitle("ERE")

cowplot::plot_grid(authPlot,authPlot_AE,authPlot_ERE, align = "hv", ncol=3)
ggsave(paste(here(), "/figs/auth_plots_descript.png", sep=""), width=21, height=7, units = "cm")


# Locations ----

source("helperFunctions.R")

literature$AuthorAddress[which(is.na(literature$AuthorAddress))] <- ""
literature$Locations <- sapply(literature$AuthorAddress, get_location)

for (i in c("AE","ERE","total")){
  # Split locations by "; "
  if (i == "total"){
    locationList <- unlist(lapply(literature$Locations, function(x) strsplit(x, ";")))
  }
  if (i == "AE"){
    locationList <- unlist(lapply(literature[literature$AEorERE=="AE","Locations"], function(x) strsplit(x, ";")))
  }
  if (i == "ERE"){
    locationList <- unlist(lapply(literature[literature$AEorERE=="ERE","Locations"], function(x) strsplit(x, ";")))
  }
  
  locations <- data.frame(location = locationList)        # Create data frame
  rm("locationList")
  locations$location <- as.character(locations$location)  # To character type
  locations$city <- gsub(",.*", "", locations$location)   # Remove country from location
  locations$country <- gsub(".*,", "", locations$location) # Remove city from location

  for (j in c("USA","AK","AL","AR","AZ","CA","CT","CO","DC","DE","FL","GA","HI","IA","ID","IN","IL","KS","KY","LA","MD","MA","MI","ME","MN","MO","MS","MT","NC","ND","NE","NJ","NH","NM","NV","NY","OH","OK","OR","PA","PR","RI","TN","UT","SC","SD","TX","VA","VT","WA","WI","WV","WY")){
    locations[grep(j, locations$country), "country"] <- "United States"
  }
  locations$country=gsub("Peoples R China", "China", locations$country)
  locations[grep("Scotland|England|Wales|North Ireland", locations$country), "country"] <- "United Kingdom"
  locations[grep("Guadeloupe", locations$country), "country"] <- "Guadeloupe"
  locations[grep("Polynesia", locations$country), "country"] <- "French Polynesia"
  locations[grep("U Arab Emirates", locations$country), "country"] <- "United Arab Emirates"
  locations[grep("Trinid | Trinidad", locations$country), "country"] <- 'Trinidad'
  locations[grep("Tobago", locations$country), "country"] <- 'Tobago'
  locations[grep("Bosnia & Herceg", locations$country), "country"] <- 'Bosnia and Herzegovina'
  locations[grep("Byelarus", locations$country), "country"] <- 'Belarus'
  locations[grep("Cote Ivoire", locations$country), "country"] <- 'Ivory Coast'
  locations[grep("Papua N Guinea", locations$country), "country"] <- 'Mongolia'
  locations[grep("Mongol Peo Rep", locations$country), "country"] <- 'Papua New Guinea'
  locations[grep("Surinam", locations$country), "country"] <- 'Suriname'
  locations[grep("W Ind Assoc St", locations$country), "country"] <- 'Turks and Caicos Islands'
  locations[grep("Dominican", locations$country), "country"] <- 'Dominican Republic'
  locations[grep("Surinam", locations$country), "country"] <- 'Suriname'
  locations[grep("Marshall", locations$country), "country"] <- 'Marshall Islands'
  locations[grep("Martinique", locations$country), "country"] <- 'Martinique'
  locations[grep("Micronesia", locations$country), "country"] <- 'Micronesia'
  locations[grep("Reunion", locations$country), "country"] <- 'Reunion'
  locations[grep("Sao Tome", locations$country), "country"] <- 'Sao Tome and Principe'
  locations[grep("Zaire", locations$country), "country"] <- 'Democratic Republic of the Congo'
  locations[grep("Tajikstan", locations$country), "country"] <- 'Tajikistan'
  locations[grep("Rep of Georgia", locations$country), "country"] <- 'Georgia'
  locations$country = tolower(locations$country)
  locations <- na.omit(locations)
  
  if (i == "total"){
    locations_total <- locations
  }
  else {
    assign(paste("locations", i, sep="_"),locations) 
  }
  rm(list = c("locations"))
}


if (!require("rworldmap")) install.packages("rworldmap", dep=T)

location_map <- joinCountryData2Map(as.data.frame(table(locations_total$country)), joinCode = "NAME", nameJoinColumn = "Var1", verbose=TRUE)
location_map_AE <- joinCountryData2Map(as.data.frame(table(locations_AE$country)), joinCode = "NAME", nameJoinColumn = "Var1", verbose=TRUE)
location_map_ERE <- joinCountryData2Map(as.data.frame(table(locations_ERE$country)), joinCode = "NAME", nameJoinColumn = "Var1", verbose=TRUE)

# graphic parameter
op <- par(mfcol = c(1,3),mar = c(0,0,1,0), pty="m")

Map_total <-
  mapCountryData(location_map, nameColumnToPlot="Freq", mapTitle="\n\n\n\n\ntotal", addLegend=F, colourPalette="white2Black", catMethod="logFixedWidth")
do.call( addMapLegend, c(Map_total, legendWidth=.5, legendShrink=.875, legendMar = 2, legendLabels="all", legendIntervals="page", tcl=-0.25))           

Map_AE <-
  mapCountryData(location_map_AE , nameColumnToPlot="Freq", mapTitle="\n\n\n\n\nAE", addLegend=F, colourPalette="white2Black", catMethod="logFixedWidth")
do.call( addMapLegend, c(Map_AE, legendWidth=.5, legendShrink=.875, legendMar = 2, legendLabels="all", legendIntervals="page", tcl=-0.25))  

Map_ERE <- 
  mapCountryData(location_map_ERE, nameColumnToPlot="Freq", mapTitle="\n\n\n\n\nERE", addLegend=F, colourPalette="white2Black", catMethod="logFixedWidth")
do.call( addMapLegend, c(Map_ERE, legendWidth=.5, legendShrink=.875, legendMar = 2, legendLabels="all", legendIntervals="page", tcl=-0.25))  

dev.print(file= paste(here(), "/figs/locationsPlot.png", sep = ""), device=png, width=3200, height= 900, res=300)

dev.off()

# get the updated citation counts
literature_updated_citations <- literature %>% left_join(final_update %>% select(code, citation) %>% rename(citation_update = citation), by = c("UniqueArticleIdentifier" = "code")) %>% mutate(citation_update = ifelse(is.na(citation_update), TimesCited, citation_update))

# Top citation papers
MostCitedPapers = data.frame(literature_updated_citations$id,paste(literature_updated_citations$Authors, paste("(", literature_updated_citations$YearPublished, ")", sep = ""), literature_updated_citations$ISOSourceAbbreviation, sep = ","), literature_updated_citations$citation_update, literature_updated_citations$AEorERE)
MostCitedPapers = MostCitedPapers[order(literature_updated_citations$citation_update, decreasing = TRUE), ]
names(MostCitedPapers) = c("id", "Paper", "TimesCited", "AEorERE")
MostCitedPapers <- MostCitedPapers %>% mutate(Paper = toupper(Paper))

TopCiteAEplot <- ggplot(MostCitedPapers[MostCitedPapers$AEorERE=="AE",2:3][1:10,]) +
  geom_col(aes(x=reorder(Paper, TimesCited),y=TimesCited), fill = "darkgrey") +
  scale_y_continuous(limits = c(0, 2500)) +
  coord_flip() +
  xlab("") +
  ylab("") +
  ggtitle("AE")

TopCiteEREplot <- ggplot(MostCitedPapers[MostCitedPapers$AEorERE=="ERE",2:3][1:10,]) +
  geom_col(aes(x=reorder(Paper, TimesCited),y=TimesCited), fill = "darkgrey") +
  scale_y_continuous(limits = c(0, 2500)) +
  coord_flip() +
  xlab("") +
  ylab("") +
  ggtitle("ERE")

cowplot::plot_grid(TopCiteAEplot,TopCiteEREplot, align = "hv", ncol=1)
ggsave(paste(here(), "/figs/TopCitedPaper.png", sep=""), width=20, height=15, units = "cm")

# publications per journal ----
literature$ISOSourceAbbreviation[literature$ISOSourceAbbreviation=="Can. J. Agric. Econ.-Rev. Can. Agroecon."] <-
  "Can. J. Agric. Econ."
literature$ISOSourceAbbreviation[literature$ISOSourceAbbreviation=="Annu. Rev. Resour. Econ"] <-
  "Annu. Rev. Resour. Econ."
literature$ISOSourceAbbreviation <- as.factor(literature$ISOSourceAbbreviation)
pubsAE <- literature %>% 
  filter(AEorERE=="AE") %>% 
  count(ISOSourceAbbreviation) %>% 
  arrange(-n) %>% 
  ggplot() +
  geom_col(aes(x=reorder(ISOSourceAbbreviation, n),y=n), fill = "darkgrey") +
  scale_y_continuous(limits = c(0, 5000)) +
  coord_flip() +
  xlab("") +
  ylab("Number of articles") +
  ggtitle("AE")
pubsERE <- literature %>% 
  filter(AEorERE=="ERE") %>% 
  count(ISOSourceAbbreviation) %>% 
  arrange(-n) %>% 
  ggplot() +
  geom_col(aes(x=reorder(ISOSourceAbbreviation, n),y=n), fill = "darkgrey") +
  scale_y_continuous(limits = c(0, 5000)) +
  coord_flip() +
  xlab("") +
  ylab("Number of articles") +
  ggtitle("ERE")
