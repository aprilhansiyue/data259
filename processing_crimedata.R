library(dplyr)

lemon <- read.csv("C:/Users/hansi/Downloads/lemonade_raw.csv")
crime <- read.csv("C:/Users/hansi/Downloads/Crimes_Map.csv")

lemon <- lemon[complete.cases(lemon),]

for (i in 1:length(lemon$index)) {
  info <- strsplit(lemon$address[i],", ")
  lemon$streets[i] <- info[[1]][2]
  lemon$zipcode[i] <- strsplit(info[[1]][4]," ")[[1]][2]
}

lemon <- lemon[,-1]

## Latitude and Longitude Added
lemon <- read.csv("C:/Users/hansi/Downloads/geocodedlemon.csv")
lemon <- lemon %>% select(-c("altitude","geocode_add","point"))
for (i in 1:length(lemon$X)) {
  info <- strsplit(lemon$geocoded[i],", ")
  lemon$latitude[i] <- gsub("\\(|\\)","",info[[1]][1])
  lemon$longitude[i] <- gsub("\\(|\\)","",info[[1]][2])
}

## Processing crime data
byType <- table(crime$PRIMARY.DESCRIPTION)
byBlock <- table(crime$BLOCK)
number <- c()

## Convert Address to Blocks
formatted_blocks <- c()
for (i in 1:length(lemon$X)) {
  info <- strsplit(lemon$streets[i]," ")
  number <- info[[1]][1]
  if (grepl("\\-",number)){
    number <- strsplit(number,"\\-")[[1]][1]
  }
  formatted_number <- sprintf("%05d", as.numeric(number))
  formatted_number <- sub("..$", "XX", formatted_number)
  formatted_blocks[i] <- paste(formatted_number,info[[1]][2],
                               info[[1]][3],info[[1]][4])
}
lemon$blocks <- formatted_blocks

## Compared Blocks to Crime Data (Frequency Encoding)
formatted_blocks <- toupper(formatted_blocks)
CrimeFreq <- c()
for (i in 1:length(lemon$X)) {
  CrimeFreq[i] <- byBlock[formatted_blocks[i]]
}
CrimeFreq[is.na(CrimeFreq)] <- 0
lemon$CrimeFrequency <- CrimeFreq

