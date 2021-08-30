
rm(list=ls())

setwd("~/Desktop/git_repositories/SBB-dispersal/avbernat_working_on/Dispersal/Summer_2021/windaq_processing/data/")

masterlist = read.csv("masterlist.csv", header=TRUE, sep=",", quote="", stringsAsFactors=FALSE)
deaddata = masterlist[masterlist$died=="Y",]

motherdata = read.csv("mother_laying_bydate.csv", header=TRUE, sep=",", quote="", stringsAsFactors=FALSE)
eggdata = read.csv("egg_tube_data.csv", header=TRUE, sep=",", quote="", stringsAsFactors=FALSE)
eggdata$`dead?` = ""

unique(cbind(eggdata$MID, eggdata$pop))

eggdata = eggdata[!is.na(eggdata$MID),]

last_input = 432
for (i in 1:nrow(eggdata)) {
  mid = eggdata$MID[i]
  if (mid %in% deaddata$ID) {
    eggdata$`dead?`[i] = "yes"
    print(mid)
  }
}

alive_eggs = eggdata[eggdata$`dead?`!="yes",]

# who is left?
unique(cbind(alive_eggs$MID, alive_eggs$pop)) # 3 KL and 1 PK

# what is the date distribution?

hatched = alive_eggs[alive_eggs$hatch_date !="",]
dates = as.Date(hatched$hatch_date, "%m.%d.%y")
hatched$date = dates

unique(hatched$date)
hist(hatched$date, breaks=14, freq=TRUE)

library(ggplot2)
ggplot(hatched, aes(x=date, fill=pop)) +
  geom_histogram()

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

