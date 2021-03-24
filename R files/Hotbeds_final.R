#hotbeds (I) & heatmap prep and execution (II)

setwd("/Users/Bisa/Desktop/BachelorThesis2.0/Final Code")



'I Calculate Hotbed
  a) Crit1
  b) Crit2
  c) Sum up Crit1 & Crit2
II Apply Hotbeds
III Preparation for Heatmap'

'I Calculate Hotbeds'

#Def. Hotbeds: 
  #cr1) most incidents in period: threshold >= 95% quantile
  #cr2) highest relative escalation rate within: average escalation; above 75% quantile

z <- list(t1,t2,t3,t4,t5)

'a) Crit1' 
y <- 1

for (i in z) {
  #get frequencies of countries in time period i
  freq_country <- data.frame(table(i$country_txt))
  
  #selete only countries that have at least one incident in time period i
  freq_country <- freq_country[which(freq_country$Freq != 0), ]
  
  #set "relative" threshold (criteria of final hotbed calculation: >1 && <5)
  threshold <- quantile(freq_country$Freq, c(0.95), type = 1)
  
  #select only countries above the given threshold
  hotbeds <- freq_country[which(freq_country$Freq >= threshold),]
  hotbeds <- droplevels(hotbeds)
  
  #dynamic naming
  name <- paste("hb_cr1_t", y, sep ="")
  assign(name, hotbeds)
  
  #make extra list with hotbed names --> needed for cr2
  hb_names <- hotbeds[,1]
  hb_names <- as.character(hb_names)
  
  name <- paste("hb_names_t", y, sep ="")
  assign(name, hb_names)
  
  y <- y+1
}

rm(freq_country, threshold, y, i, name, hotbeds, hb_names)

'b) Crit2'

#overlapping years for growth rate; otherwise, first year always 0
#if data frame ends in the beginning of the year an extra year is not added (e.g. for t3), if period ends in the 
#end of the year, extra year is added (e.g. for t4)
t1_over <- data_selected[which(data_selected$iyear >= 1977 & data_selected$iyear <= 1987), ] 
t2_over <- data_selected[which(data_selected$iyear >= 1987 & data_selected$iyear <= 1997), ] 
t3_over <- data_selected[which(data_selected$iyear >= 1997 & data_selected$iyear <= 20083), ] 
t4_over <- data_selected[which(data_selected$iyear >= 2007 & data_selected$iyear <= 201110), ] 
t5_over <- data_selected[which(data_selected$iyear >= 2011 & data_selected$iyear <= 2017), ] 

z <- list(t1_over,t2_over,t3_over,t4_over,t5_over)

x <- 1

for (i in z) {
  
  #get freqencies of countries per year in time period i
  freq_country_year <- data.frame(table(i$country_txt, i$iyear))
  colnames(freq_country_year) <- c("country", "year", "freq")
  
  #apply growth rate from year a to a+1
  growth <- ddply(freq_country_year,"country",transform,
                  Growth=c(NA,exp(diff(log(freq)))-1))
  
  #apply growth rate only to countries that were selected in hotbeds
  zz <- list(hb_names_t1, hb_names_t2, hb_names_t3, hb_names_t4, hb_names_t5)
  growth_select <- growth[growth$country %in% zz[[x]],]
  growth_select <- droplevels(growth_select)
  growth_select$year <- as.numeric(as.character(growth_select$year))
  
  #remove first year of every time period, because the growth will be 0
  min_year <- min(growth_select$year)
  growth_select <- growth_select[growth_select$year!= min_year, ]
  
  #threshold is 3rd Quantile
  threshold <- quantile(growth_select$Growth, c(0.75), type = 1, na.rm = T) 
  growth_select <- growth_select[which(growth_select$Growth >=threshold), ]
  
  #above threshold and at least twice in the list
  hotbeds <- growth_select %>% group_by(country) %>% filter(n()>=2)
  
  name <- paste("hb_cr2_t", x, sep ="")
  assign(name, hotbeds)
  
  x <- x+1
}

rm(z, zz, i, name, x, threshold, min_year, hotbeds, growth, growth_select, 
   freq_country_year, t1_over, t2_over, t3_over, t4_over, t5_over)

#select only hotbeds that have entries that are not more than two years apart
hotbeds_cr2 <- function(df){
  
  df$year_diff <- 0
  for(r in 2:nrow(df)){
    if(df[r-1, "country"] == df[r, "country"]) {
      df[r, "year_diff"] <- df[r, "year"] - df[r-1, "year"]
    }
  }
  
  #delete all first appearences
  df <- df[df$year_diff != 0, ]
  
  #delete all that are more than two years apart
  df <- df[df$year_diff < 3, ]
  
  #delete duplicates if more than one within 2 years
  df <- aggregate(year_diff~country, df, min)
  
  return(df)
}

#apply hotbeds_cr2 function to all df hb_cr2_tx
zzz <- list(hb_cr2_t1, hb_cr2_t2, hb_cr2_t3, hb_cr2_t4, hb_cr2_t5)
w <- 1

for (i in zzz) {
  hb_cr2_sum <- hotbeds_cr2(i)
  
  name <- paste("hb_cr2_t", w, "_sum", sep ="")
  assign(name, hb_cr2_sum)
  
  w <- w+1

}

#combine hotbeds that fulfill both criteria in one data frame
hotbedsfinal_function <- function(df1, df2){
 
  df <- as.data.frame(intersect(df1$Var1, df2$country))
  colnames(df) <- "Hotbeds"
  df <- droplevels(df)
  
  return(df)
}

#apply function for final hotbeds selection

'c) Sum up Crit1 & Crit2'

hb_final_t1 <- hotbedsfinal_function(hb_cr1_t1, hb_cr2_t1_sum)
hb_final_t2 <- hotbedsfinal_function(hb_cr1_t2, hb_cr2_t2_sum)
hb_final_t3 <- hotbedsfinal_function(hb_cr1_t3, hb_cr2_t3_sum)
hb_final_t4 <- hotbedsfinal_function(hb_cr1_t4, hb_cr2_t4_sum)
hb_final_t5 <- hotbedsfinal_function(hb_cr1_t5, hb_cr2_t5_sum)

rm(zzz, w, i, name, hb_cr1_t1, hb_cr1_t2, hb_cr1_t3, hb_cr1_t4, hb_cr1_t5, hb_cr2_t1_sum, hb_cr2_t2_sum,
   hb_cr2_t3_sum, hb_cr2_t4_sum, hb_cr2_t5_sum, hb_names_t1, hb_names_t2, hb_names_t3,
   hb_names_t4, hb_names_t5, hb_cr2_t1, hb_cr2_t2, hb_cr2_t3, hb_cr2_t4, hb_cr2_t5, hb_cr2_sum)


'II Apply Hotbeds'

#apply hb to df

t1t2 <- rbind(t1, t2)
t2t3 <- rbind(t2, t3)
t3t4t5 <- rbind(t3, t4, t5)
t4t5 <- rbind(t4, t5)

newcol_hotbed <- function(df, df2){
  
  for (i in 1:nrow(df)){
    if(df[i,2] %in% df2[,1]){
      df$hotbed[i] <- 1
    } else {
      df$hotbed[i] <- 0
    }
  }
  
  return(df)
}

t1 <- newcol_hotbed(t1, hb_final_t1)
t1t2 <- newcol_hotbed(t1t2, hb_final_t1)

t2 <- newcol_hotbed(t2, hb_final_t2)
t2t3 <- newcol_hotbed(t2t3, hb_final_t2)

t3 <- newcol_hotbed(t3, hb_final_t3)
t3t4t5 <- newcol_hotbed(t3t4t5, hb_final_t3)

t4 <- newcol_hotbed(t4, hb_final_t4)
t4t5 <- newcol_hotbed(t4t5, hb_final_t4)

t5 <- newcol_hotbed(t5, hb_final_t5)

#make table with regions

adapt_hotbedregionnames <- function(df) {
  
  hotbeds <- unique(df[which(df$hotbed == 1), c(2,3)])
  
  #insert hotbed before region_txt (only needed for overview, not for analysis)
  hotbeds <- cbind(hotbeds, paste(hotbeds$region_txt, "Hotbed", sep = "_"))
  colnames(hotbeds)[3] <- "hotbed_regions"
  
  name <- paste("_hotbedregions", sep ="")
  assign(name, hotbeds)
  
}

hb_regions_t1 <- adapt_hotbedregionnames(t1)
hb_regions_t2 <- adapt_hotbedregionnames(t2)
hb_regions_t3 <- adapt_hotbedregionnames(t3)
hb_regions_t4 <- adapt_hotbedregionnames(t4)
hb_regions_t5 <- adapt_hotbedregionnames(t5)

apply_hb_region <- function(df, df2){
  
  df$region_txt <- as.character(df$region_txt)
  
  for (i in 1:nrow(df)){
    if(df[i,2] %in% df2[,1]){
      df[i,3] <- paste(df[i,3], "(Hotbed)", sep = " ")
      
    }
  }
  
  return(df)
}

t1 <- apply_hb_region(t1, hb_regions_t1)
t2 <- apply_hb_region(t2, hb_regions_t2)
t3 <- apply_hb_region(t3, hb_regions_t3)
t4 <- apply_hb_region(t4, hb_regions_t4)

t1t2 <- apply_hb_region(t1t2, hb_regions_t1)
t2t3 <- apply_hb_region(t2t3, hb_regions_t2)
t3t4t5 <- apply_hb_region(t3t4t5, hb_regions_t3)
t4t5 <- apply_hb_region(t4t5, hb_regions_t4)
t5 <- apply_hb_region(t5, hb_regions_t5)

'III Preparation for Heatmap'

#produce list of character of incidents/tactics in order to compare equal tactics
aggr_tactic_year_region <- function(df) {
  
  tactics <- paste(as.character(df$vicinity), as.character(df$multiple),
                   as.character(df$suicide), as.character(df$attacktype1_txt),
                   as.character(df$weaptype1_txt), as.character(df$weapsubtype1_txt),
                   as.character(df$targgroup),
                   sep = " - ")
  
  df <- cbind(df, tactics)
  
  freq_tactics <- data.frame(table(tactics))
  freq_tactics <- freq_tactics[order(-freq_tactics$Freq),]
  freq_tactics <- freq_tactics[which(freq_tactics$Freq>300), ] 
  
  list <- list(df = df, freq_tactics = freq_tactics)
  
  return(list)
  
}

z <- list(t1,t2,t3,t4,t5,t1t2, t2t3, t3t4t5, t4t5)
y <- 1
i <- 1

for(i in z){
  
  #make list for time period i with tactic column and frequency of tactics
  aggr <- aggr_tactic_year_region(i)
  
  df <- as.data.frame(aggr[[1]])
  freq_tactics <- as.data.frame(aggr[[2]])
  
  name <- paste("t", y, sep ="")
  assign(name, df)
  
  name <- paste("freq_tactics_t", y, sep ="")
  assign(name, freq_tactics)
  
  y <- y+1
  
}

t1t2 <- t6
t2t3 <- t7
t3t4t5 <- t8
t4t5 <- t9

rm(aggr, df, freq_tactics, i, z, y, name, t6, t7, t8, t9)
