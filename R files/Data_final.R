#Data Exploration, Preparation, Cleaning

#codebook: https://www.start.umd.edu/gtd/downloads/Codebook.pdf

#set your own wd
wdir <- ""
setwd(wdir)

'I Load Data
II Observation Selection
III Feature Selection
IV Data Cleaning
V Time Frames'

'I Load Data'

data <- read.csv(file="globalterrorismdb_0718dist.csv", header = T, sep = ",")

#show histogram for total amount of incidents in all time periods
png("frequency1970-2017.png", units="in", width=8, height=5, res=300)
plot <- hist(data$iyear, ylab = "Frequency", xlab = "Year", col = "grey",
        border = "black", main = NULL, breaks = 47, xlim = c(1970, 2019), 
        ylim = c(0, 20000))
dev.off()

'II Observation Selection'

#select only var that fulfill all inclusion criteria and/or doubtterr == 0 or -9
crit111 <- data[which((data$crit2 == 1 & data$crit1 == 1 & data$crit3 == 1) & 
                        (data$doubtterr == 0 | data$doubtterr == -9)), ]

#keep only successful attacks
crit_success <- crit111[which(crit111$success == 1), ]

'III Feature Seletion'

#time/space var to keep: iyear, imonth, iday, country_txt, region_txt
#tactic var to keep: vicinity, multiple, suicide, attacktype1, attacktype1_txt,
  #weaptype1, weaptype1_txt, weapsubtype1, weapsubtype1_txt, targtype1, targtype1_txt

data_selected <- crit_success[, c("iyear", "imonth", "country_txt", "region_txt", "vicinity", "multiple", 
                             "suicide", "attacktype1", "attacktype1_txt", "weaptype1",
                             "weaptype1_txt", "weapsubtype1", "weapsubtype1_txt",
                             "targtype1", "targtype1_txt")]

#select only observation that don't have "unknown" in two of the three "main" variables
data_selected_sub <- data_selected[which(
                        (data_selected$attacktype1 != 9 & data_selected$weaptype1 != 13) | 
                        (data_selected$targtype1 != 20 & data_selected$weaptype1 != 13) | 
                        (data_selected$targtype1 != 20 & data_selected$attacktype1 != 9)), ]

data_selected <- data_selected_sub

rm(crit111, data_selected_sub, crit_success, plot)

'IV Data Selection'

#impute year+month var in order to set the time frames t3 und t4 right
data_selected$yearmonth <- ifelse(data_selected$imonth == 12 | data_selected$imonth == 11 | data_selected$imonth == 10, 
                            (paste(data_selected$iyear, data_selected$imonth, sep = "")),
                            (paste(data_selected$iyear, data_selected$imonth, sep = "0")))
data_selected$yearmonth <- as.numeric(data_selected$yearmonth)

#impute Israel/Palastine for Israel and West Bank and Gaza Strip
data_selected$country_txt <- as.character(data_selected$country_txt)
data_selected$country_txt[data_selected$country_txt == "Israel" | 
                          data_selected$country_txt == "West Bank and Gaza Strip"] <- "Israel/Palestine"

data_selected$country_txt <- as.factor(data_selected$country_txt)

#split groups into "soft" and "hard" targets
data_selected$targgroup <- ifelse(data_selected$targtype1_txt == "Abortion Related" |
                                  data_selected$targtype1_txt == "Airports & Aircraft" |
                                  data_selected$targtype1_txt == "Business" |
                                  data_selected$targtype1_txt == "Educational Institution" |
                                  data_selected$targtype1_txt == "Food or Water Supply" |
                                  data_selected$targtype1_txt == "Journalists & Media" |
                                  data_selected$targtype1_txt == "Maritime" |
                                  data_selected$targtype1_txt == "NGO" |
                                  data_selected$targtype1_txt == "Other" |
                                  data_selected$targtype1_txt == "Private Citizens & Property" |
                                  data_selected$targtype1_txt == "Religious Figures/Institutions" |
                                  data_selected$targtype1_txt == "Telecommunication" |
                                  data_selected$targtype1_txt == "Tourists" |
                                  data_selected$targtype1_txt == "Transportation" |
                                  data_selected$targtype1_txt == "Utilities", 
                                  "Civil Targets (social)",
                                  
                                  ifelse(data_selected$targtype1_txt == "Government (Diplomatic)" |
                                         data_selected$targtype1_txt == "Government (General)", 
                                         "Government Targets (symbolic)",
                                         
                                         ifelse(data_selected$targtype1_txt == "Military" |
                                                data_selected$targtype1_txt == "Police" |
                                                data_selected$targtype1_txt == "Terrorists/Non-State Militia" |
                                                data_selected$targtype1_txt == "Violent Political Party",
                                                "Armed Actors", "Unknown Target")))


#impute unknown/NAs of multiple and vicinity with most frequent observed value
table(data_selected$vicinity)
data_selected$vicinity[data_selected$vicinity == -9] <- 0
summary(data_selected$multiple)
data_selected$multiple[is.na(data_selected$multiple)] <- 0

#imputation of NA in weapsubtype1 
  #if "unknown" category in weapsubtype1exists, e.g. weaptype1 == 6 is explosives,
  #weapsubtype1 == 16 is unknown explosives, is variable coded as such
  #if "unknown" category does not exist, it is coded as weaptype1+00, e.g. weapsubtype1 == NA for
  #weaptype1 == 1, turns into weapsubtype1 == 100

for(i in 1:nrow(data_selected)){
  if(data_selected$weaptype1[i] == 1 & is.na(data_selected$weapsubtype1[i])){
    data_selected$weapsubtype1[i] <- 100
  } else if (data_selected$weaptype1[i] == 2 & is.na(data_selected$weapsubtype1[i])) {
    data_selected$weapsubtype1[i] <- 200
  } else if (data_selected$weaptype1[i] == 3 & is.na(data_selected$weapsubtype1[i])) {
    data_selected$weapsubtype1[i] <- 300 
  } else if (data_selected$weaptype1[i] == 5 & is.na(data_selected$weapsubtype1[i])){
    data_selected$weapsubtype1[i] <- 5
  } else if (data_selected$weaptype1[i] == 6 & is.na(data_selected$weapsubtype1[i])){
    data_selected$weapsubtype1[i] <- 16
  } else if (data_selected$weaptype1[i] == 7 & is.na(data_selected$weapsubtype1[i])){
    data_selected$weapsubtype1[i] <- 700
  } else if (data_selected$weaptype1[i] == 8 & is.na(data_selected$weapsubtype1[i])) {
    data_selected$weapsubtype1[i] <- 800
  } else if (data_selected$weaptype1[i] == 9 & is.na(data_selected$weapsubtype1[i])){
    data_selected$weapsubtype1[i] <- 27
  } else if (data_selected$weaptype1[i] == 10 & is.na(data_selected$weapsubtype1[i])){
    data_selected$weapsubtype1[i] <- 1000
  } else if (data_selected$weaptype1[i] == 11 & is.na(data_selected$weapsubtype1[i])){
    data_selected$weapsubtype1[i] <- 1100
  } else if (data_selected$weaptype1[i] == 12 & is.na(data_selected$weapsubtype1[i])){
    data_selected$weapsubtype1[i] <- 1200
  } else if (data_selected$weaptype1[i] == 13 & is.na(data_selected$weapsubtype1[i])){
    data_selected$weapsubtype1[i] <- 1300
  } else { }
}

rm(i)

#imputation of NAs in weapsubtype1_txt
data_selected$weapsubtype1_txt <- as.character(data_selected$weapsubtype1_txt)
data_selected$weaptype1_txt <- as.character(data_selected$weaptype1_txt)

for(i in 1:nrow(data_selected)) {
  if(data_selected$weapsubtype1_txt[i] == "") {
    if(data_selected$weapsubtype1[i] == 500) {
      data_selected$weapsubtype1_txt[i] <- "Unknown Gun Type" 
    } else if(data_selected$weapsubtype1[i] == 1600){
      data_selected$weapsubtype1_txt[i] <- "Unknown Explosive Type"
    } else if (data_selected$weapsubtype1[i] == 2700) {
      data_selected$weapsubtype1_txt[i] <- "Unknown Weapon Type"
    } else {
      data_selected$weapsubtype1_txt[i] <- data_selected$weaptype1_txt[i]
    }
  }
}

rm(i)

'V Time Frames'

#subsetting the data set in either equally sized periods or in periods with 
#different collection methodologies
#1/1/1977-12/31/1987
t1 <- data_selected[which(data_selected$iyear >= 1977 & data_selected$iyear <= 1987), 
                    c("iyear", "country_txt", "region_txt", "vicinity", "multiple",
                      "suicide", "attacktype1_txt", "weaptype1_txt", "weapsubtype1_txt",
                      "targgroup")] 
#before 1977 too less data

#1/1/1988-12/31/1997
t2 <- data_selected[which(data_selected$iyear >= 1988 & data_selected$iyear <= 1997),
                    c("iyear", "country_txt", "region_txt", "vicinity", "multiple",
                      "suicide", "attacktype1_txt", "weaptype1_txt", "weapsubtype1_txt",
                      "targgroup")] 

#1/1/1998-3/31/2008
t3 <- data_selected[which(data_selected$iyear >= 1998 & data_selected$yearmonth <= 200803),
                    c("iyear", "country_txt", "region_txt", "vicinity", "multiple",
                      "suicide", "attacktype1_txt", "weaptype1_txt", "weapsubtype1_txt",
                      "targgroup")] 

#4/1/2008-10/31/2011
t4 <- data_selected[which(data_selected$iyear >=2008 & data_selected$yearmonth >= 200804 & data_selected$yearmonth <= 201110
                          & data_selected$iyear <= 2011),
                    c("iyear", "country_txt", "region_txt", "vicinity", "multiple",
                      "suicide", "attacktype1_txt", "weaptype1_txt", "weapsubtype1_txt",
                      "targgroup")] 

#11/1/2012-12/31/2017 
t5 <- data_selected[which(data_selected$iyear >= 2011 & data_selected$yearmonth >= 201111 & data_selected$iyear <= 2017),
                    c("iyear", "country_txt", "region_txt", "vicinity", "multiple",
                      "suicide", "attacktype1_txt", "weaptype1_txt", "weapsubtype1_txt",
                      "targgroup")] 
