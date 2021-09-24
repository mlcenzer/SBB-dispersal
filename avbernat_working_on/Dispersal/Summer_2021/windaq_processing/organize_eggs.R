
rm(list=ls())

setwd("~/Desktop/git_repositories/SBB-dispersal/avbernat_working_on/Dispersal/Summer_2021/windaq_processing/data/")

masterlist = read.csv("masterlist.csv", header=TRUE, sep=",", quote="", stringsAsFactors=FALSE)
deaddata = masterlist[masterlist$died=="Y",]

motherdata = read.csv("mother_laying_bydate.csv", header=TRUE, sep=",", quote="", stringsAsFactors=FALSE)
eggdata = read.csv("egg_tube_data.csv", header=TRUE, sep=",", quote="", stringsAsFactors=FALSE)
eggdata$`dead?` = ""
head(eggdata)

unique(cbind(eggdata$MID, eggdata$pop))

eggdata = eggdata[!is.na(eggdata$MID),]

last_input = 569
for (i in 1:nrow(eggdata)) {
  mid = eggdata$MID[i]
  if (mid %in% deaddata$ID) {
    eggdata$`dead?`[i] = "yes"
    print(mid)
  }
}

alive_mothers = eggdata[eggdata$`dead?`!="yes",]

# who is left?
unique(cbind(alive_mothers$MID, alive_mothers$pop)) # 3 KL and 1 PK

# what is the date distribution of alive nymphs?

hatched = eggdata[eggdata$hatch_date !="",]
alive_nymphs = hatched[hatched$death_date =="",]
dates = as.Date(alive_nymphs$hatch_date, "%m.%d.%y")
alive_nymphs$date = dates

unique(alive_nymphs$date)
hist(alive_nymphs$date, breaks=14, freq=TRUE)

# by pop
library(ggplot2)
ggplot(alive_nymphs, aes(x=date, fill=pop)) +
  geom_histogram()

# yes/no wing date

alive_nymphs$adult <- "no"
alive_nymphs$adult[alive_nymphs$wing_date !=""] = "yes"

ggplot(alive_nymphs, aes(x=date, fill=adult)) +
  geom_histogram()

# numbers of PK vs KL (pretty even)

paste("PK", nrow(alive_nymphs[alive_nymphs$pop == "PK",]))
paste("KL", nrow(alive_nymphs[alive_nymphs$pop == "KL",]))


# Check how many mothers are left 

# how many mothers do I have in cups right now?

PKmoms = c(317, 103, 329, 9, 339)
LPmoms = c(247, 153, 262, 363, 164, 263, 61)
HSmoms = c(24, 28, 349, 355, 229, 356, 232,
           348, 33, 22, 29, 129, 41)
deaddata$ID

for (id in deaddata$ID) {
  if (247 %in% PKmoms) {
    cat("PK", id)
  }
  if (id %in% LPmoms) {
    cat("LP", id)
  }
  if (id %in% HSmoms) {
    cat("HS", id)
  }
}

# all mothers currently selected are alive
# so that means 5 PK, 7 LP, and 13 HS

