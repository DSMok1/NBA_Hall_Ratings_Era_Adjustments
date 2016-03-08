
###############

###  This file compiles BBRef CSV dumps into a single file

###############






###  Import Data ####

Stats_Adv_Playoffs <- read.csv("~/ETC/Sports/NBA/Stats_Dump_Advanced_Playoffs.csv", na.strings="\\N")
Stats_Adv_Reg<- read.csv("~/ETC/Sports/NBA/Stats_Dump_Advanced_Reg_Season.csv", na.strings="\\N")
Stats_Basic_Playoffs <- read.csv("~/ETC/Sports/NBA/Stats_Dump_Basic_Playoffs.csv", na.strings="\\N")
Stats_Basic_Reg <- read.csv("~/ETC/Sports/NBA/Stats_Dump_Basic_Reg_Season.csv", na.strings="\\N")


# Add_IDs <- function (raw_data) {
#   
#   raw_data <- within(raw_data, player_year <- paste(player_id, year_id, sep='_'))
#   raw_data <- within(raw_data, player_tm_year <- paste(player_id, team_id, year_id, sep='_'))
#   
#   return(raw_data)
# }
# 
# Stats_Adv_Playoffs <- Add_IDs(Stats_Adv_Playoffs)
# Stats_Adv_Reg<- Add_IDs(Stats_Adv_Reg)
# Stats_Basic_Playoffs <- Add_IDs(Stats_Basic_Playoffs)
# Stats_Basic_Reg <- Add_IDs(Stats_Basic_Reg)

Stats_Playoffs <- merge(Stats_Adv_Playoffs,Stats_Basic_Playoffs)
Stats_Reg <- merge(Stats_Adv_Reg,Stats_Basic_Reg)

Stats_Playoffs$Playoffs <- TRUE
Stats_Reg$Playoffs <- FALSE

Stats <- rbind(Stats_Reg,Stats_Playoffs)

###  Export as GZ ####

write.csv(Stats,file=gzfile("~/ETC/Sports/NBA/Stats_Dump_Compiled.csv.gz"), row.names = FALSE)