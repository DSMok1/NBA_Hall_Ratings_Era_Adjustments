########

## Generate Hall Ratings
## by D. Myers 11/27/15

########

###  Input and Output Information ####

Stats_Dump <- read.csv(gzfile("~/ETC/Sports/NBA/Hall_Rating/Stats_Dump_Compiled.csv.gz"))
Stats_Dump <- Stats_Dump[(!Stats_Dump$team_id == "TOT") & (Stats_Dump$mp>0),]     # Remove summations and 0 minutes players

Playoff_Series <- read.csv("~/ETC/Sports/NBA/Hall_Rating/Playoff_Series.csv")
Season_Length <- read.csv("~/ETC/Sports/NBA/Hall_Rating/Playoff_Length.csv")
Team_Strength <- read.csv("~/ETC/Sports/NBA/Hall_Rating/Team_History.csv")

Output_File <- "~/ETC/Sports/NBA/Hall_Rating/Hall_Rating_5.51.csv"

### Control variables ####

Length_Factor_Wt <- 1.0  # Do we want to adjust shorter seasons or series all the way to equivalency with current seasons?  That's 1.0
Max_MPG <- 40  # Above 40 MPG the relationship between MPG and BPM is all over the place
Playoff_Factor <- 2.0  # Additional weight to playoffs  -- NOT USED --


### Identify Injured Players ####

library(dplyr)
library(magrittr)

Injuries<-merge(Stats_Dump,Team_Strength[,c("year_id","team_id","lg_id","G")],by = c("year_id","lg_id","team_id"),all.x = TRUE) %>%
  .[.$Playoffs == FALSE,] %>%
  group_by(., lg_id,player_id,year_id) %>%
  summarise(.,
            mp = sum(mp),
            g = sum(g),
             G = mean(G)) %>%
  mutate(mpg = mp/g) %>%
  mutate(Injury = (g<0.80*G & mpg>20))

Stats_Dump<-merge(Stats_Dump,Injuries[,c("year_id","player_id","lg_id","Injury","G")],by = c("year_id","player_id","lg_id"),all.x = TRUE)


###  Project BPM from WS/48 and MPG ####

Stats_Dump$mpg <- Stats_Dump$mp/Stats_Dump$g  # Set a maximum MPG for the regression and outputs
Stats_Dump$mpg2 <- pmin(Stats_Dump$mpg,Max_MPG)  # Set a maximum MPG for the regression and outputs

Data_Extract <- Stats_Dump[,c("player_id","year_id","team_id","lg_id","mp","g","mpg","mpg2","ws_per_48","bpm","Playoffs","Injury")] %>%
  .[.$mp>250,] %>%
  .[.$Playoffs == FALSE,] %>%
  .[!is.na(.$bpm),] %>%
  .[.$Injury==FALSE,]

plot(Data_Extract$mpg,Data_Extract$bpm)  # See how mpg and bpm interact


# Projected_BPM_Reg <- lm(bpm ~ ws_per_48 + mpg, data=Data_Extract )

# summary(Projected_BPM_Reg)
# coef(Projected_BPM_Reg)
# plot(Projected_BPM_Reg)

library(MASS)

Robust_BPM_Reg <- rlm(bpm ~ ws_per_48 + mpg2, data=Data_Extract)
summary(Robust_BPM_Reg)
# coef(Robust_BPM_Reg)
# plot(Robust_BPM_Reg)
plot(Data_Extract$mpg,resid(Robust_BPM_Reg))
plot(Data_Extract$mpg,predict(Robust_BPM_Reg))


BPM_Intercept <- coef(Robust_BPM_Reg)[1]
BPM_coef_ws48 <- coef(Robust_BPM_Reg)[2]
BPM_coef_mpg <- coef(Robust_BPM_Reg)[3]

### Playoff Team Data ####

library(dplyr)
Playoff_Series <- Playoff_Series[!(Playoff_Series$series_type == "tiebreak"),]

Playoff_Series$Weights <- (1+0.5*((Playoff_Series$Series_End-1)^2))*0.75  # Add weighting for playoffs

Playoff_Series$Adjusted_Length <- 7*Length_Factor_Wt + Playoff_Series$Games*(1-Length_Factor_Wt)
Playoff_Series$Mult_Factor <- Playoff_Series$Weights*Playoff_Series$Adjusted_Length/Playoff_Series$Games

Playoff_Series<-merge(Playoff_Series,Team_Strength[,c("year_id","team_id","lg_id","PO_Rating")],by = c("year_id","lg_id","team_id"),all.x = TRUE)
Playoff_Series<-merge(Playoff_Series,Team_Strength[,c("year_id","team_id","lg_id","PO_Rating")],by.x = c("year_id","lg_id","OPP"),by.y = c("year_id","lg_id","team_id"),all.x = TRUE)
Playoff_Series$SoS_Actual<-(Playoff_Series$PO_Rating.x +Playoff_Series$PO_Rating.y)/10

Team_ID_Group <- group_by(Playoff_Series, team_id, year_id,lg_id)

Playoff_Team_Data <- summarise(Team_ID_Group,
                               Playoffs = TRUE,
                               g_playoff = sum (Games),
                               win = sum(Wins),
                               loss = sum(Losses),
                               total_length = sum(Series_Length),
                               sos = sum(SoS_Actual*Games)/sum(Games),
                               modern_length = max(Series_End),
                               series_wins = sum(Win_Series),
                               series_played = length(SoS),
                               mult_factor = sum(Mult_Factor*Games)/sum(Games)
                               )

Stats_Dump <- merge(Stats_Dump,Playoff_Team_Data,by = c("year_id","lg_id","team_id","Playoffs"),all.x = TRUE)

### Playoff Average BPM  ####

# Playoff_Data <- Stats_Dump[,c("player_id","year_id","team_id","mp","g","ws_per_48","bpm","Playoffs")]
# Playoff_Data <- Playoff_Data[!is.na(Playoff_Data$year_id),]
# Playoff_Data <- Playoff_Data[Playoff_Data$Playoffs == TRUE,]
# BPM_Playoff_Avg <- with(Playoff_Data[!is.na(Playoff_Data$bpm),], sum(bpm*mp)/sum(mp))
# WS48_Playoff_Avg <- with(Playoff_Data[!is.na(Playoff_Data$ws_per_48),], sum(ws_per_48*mp)/sum(mp))
# Playoff_Data <- Playoff_Data[Playoff_Data$year_id>1995,]
# Playoff_Data <- Playoff_Data[(Playoff_Data$mp/Playoff_Data$g)>24,]
# Playoff_Data <- Playoff_Data[Playoff_Data$g>7,]  # Have to make it past first round
# BPM_Playoff_Starter_Avg <- with(Playoff_Data[!is.na(Playoff_Data$bpm),], sum(bpm*mp)/sum(mp))


### Implied BPM for historical players ####
Stats_Dump$Raw_Implied_BPM <- predict(Robust_BPM_Reg,newdata = Stats_Dump)
Stats_Dump$Implied_BPM <- Stats_Dump$Raw_Implied_BPM + ifelse(Stats_Dump$Playoffs == TRUE,Stats_Dump$sos,0)  

Stats_Dump$Equiv_BPM <- ifelse(is.na(Stats_Dump$bpm),Stats_Dump$Implied_BPM, ((Stats_Dump$bpm + Stats_Dump$Implied_BPM)/2))


### Team Adjustments to Equivalent BPM ####

library(magrittr)
library(dplyr)

Team_Adj_Data <- 
  Stats_Dump[,c("player","player_id","age","year_id","team_id","lg_id","mp","bpm","Implied_BPM","Equiv_BPM","Playoffs")] %>%
  .[.$Playoffs == FALSE,] %>%
  group_by(., lg_id,team_id,year_id) %>%
  summarise(.,
            equiv_bpm = sum(Equiv_BPM*mp)/sum(mp),
            implied_bpm = sum(Implied_BPM*mp)/sum(mp),
            bpm = sum(bpm*mp)/sum(mp),
            mp = sum(mp)) %>%
  merge(.,Team_Strength[,c("lg_id","team_id","year_id","G","MP","Equiv_Eff_Dif")]) %>%
  mutate(Team_Adj = Equiv_Eff_Dif/5 - equiv_bpm) %>%
  mutate(Missing_Minutes = MP - mp)

Stats_Dump <- merge(Stats_Dump,Team_Adj_Data[,c("team_id","year_id","Team_Adj")])

Stats_Dump$Equiv_BPM <- Stats_Dump$Equiv_BPM + Stats_Dump$Team_Adj
Stats_Dump$Equiv_VORP <- (Stats_Dump$Equiv_BPM+2)*Stats_Dump$mp/(Stats_Dump$G*48)


### Aging Curve Estimate ####
library(magrittr)

Aging_Data <- 
  Stats_Dump[,c("player","player_id","age","year_id","team_id","lg_id","mp","g","bpm","Implied_BPM","Equiv_BPM","Playoffs","Injury")] %>%
  .[.$mp>150,] %>%
  .[.$year_id>1999,] %>%
  .[.$Playoffs == FALSE,] %>%
  .[!is.na(.$Equiv_BPM),] %>%
  .[.$Injury==FALSE,]


Aging_Group <- group_by(Aging_Data, player, player_id,year_id,age)
Aging_Data <- summarise(Aging_Group,
                       equiv_bpm = sum(Equiv_BPM*mp)/sum(mp),
                       implied_bpm = sum(Implied_BPM*mp)/sum(mp),
                       bpm = sum(bpm*mp)/sum(mp),
                       mp = sum(mp))

Aging_Data_2 <- Aging_Data

Aging_Data$prior_year <- Aging_Data$year_id - 1

names(Aging_Data_2)[names(Aging_Data_2) == "bpm"] <-
  "prior_bpm"
names(Aging_Data_2)[names(Aging_Data_2) == "implied_bpm"] <-
  "prior_implied_bpm"
names(Aging_Data_2)[names(Aging_Data_2) == "equiv_bpm"] <-
  "prior_equiv_bpm"
names(Aging_Data_2)[names(Aging_Data_2) == "age"] <-
  "prior_age"
names(Aging_Data_2)[names(Aging_Data_2) == "mp"] <-
  "prior_mp"


Aging_Pairs <- merge(Aging_Data,Aging_Data_2, by.x=c("player","player_id","prior_year"), by.y = c("player","player_id","year_id")) 

Aging_Pairs$delta_bpm <- Aging_Pairs$bpm - Aging_Pairs$prior_bpm 
Aging_Pairs$delta_implied_bpm <- Aging_Pairs$implied_bpm - Aging_Pairs$prior_implied_bpm 
Aging_Pairs$delta_equiv_bpm <- Aging_Pairs$equiv_bpm - Aging_Pairs$prior_equiv_bpm 
Aging_Pairs$avg_mp <- 2*(Aging_Pairs$mp * Aging_Pairs$prior_mp)/(Aging_Pairs$mp + Aging_Pairs$prior_mp) 
Aging_Pairs$age_2 <- Aging_Pairs$age^2
Aging_Pairs$age_3 <- Aging_Pairs$age^3 


Age_Group <- group_by(Aging_Pairs, age, age_2, age_3)
Aging <- summarise(Age_Group,
                        delta_bpm = sum(delta_bpm*avg_mp)/sum(avg_mp),
                        delta_implied_bpm = sum(delta_implied_bpm*avg_mp)/sum(avg_mp),
                        delta_equiv_bpm = sum(delta_equiv_bpm*avg_mp)/sum(avg_mp),
                        players = length(player),
                        mp = sum(avg_mp)
                   )
plot(Aging$age,Aging$delta_bpm)

Aging_Pairs <- merge(Aging_Pairs,Aging[,c("age","players")],by = c("age"))
Aging_Pairs <- Aging_Pairs[Aging_Pairs$players > 50,]
Aging_Pairs$players <- NULL



# ### Aging_MPG Curve Estimate ####
# library(magrittr)
# 
# Aging_MPG_Data <- 
#   Stats_Dump[,c("player","player_id","age","year_id","team_id","lg_id","mp","g","bpm","Implied_BPM","Equiv_BPM","Playoffs","Injury")] %>%
#   .[.$g>30,] %>%
#   .[.$year_id>1999,] %>%
#   .[.$Playoffs == FALSE,] %>%
#   .[.$Injury == FALSE,]
# 
# 
# Aging_MPG_Group <- group_by(Aging_MPG_Data, player, player_id,year_id,age)
# Aging_MPG_Data <- summarise(Aging_MPG_Group,
#                         mp = sum(mp),
#                         mpg = sum(mp)/sum(g),
#                         g = sum(g)
#                         )
# 
# Aging_MPG_Data_2 <- Aging_MPG_Data
# 
# Aging_MPG_Data$prior_year <- Aging_MPG_Data$year_id - 1
# 
# names(Aging_MPG_Data_2)[names(Aging_MPG_Data_2) == "mp"] <-
#   "prior_mp"
# names(Aging_MPG_Data_2)[names(Aging_MPG_Data_2) == "g"] <-
#   "prior_g"
# names(Aging_MPG_Data_2)[names(Aging_MPG_Data_2) == "mpg"] <-
#   "prior_mpg"
# names(Aging_MPG_Data_2)[names(Aging_MPG_Data_2) == "age"] <-
#   "prior_age"
# 
# 
# 
# Aging_MPG_Pairs <- merge(Aging_MPG_Data,Aging_MPG_Data_2, by.x=c("player","player_id","prior_year"), by.y = c("player","player_id","year_id")) 
# 
# Aging_MPG_Pairs$delta_mp <- Aging_MPG_Pairs$mp - Aging_MPG_Pairs$prior_mp 
# Aging_MPG_Pairs$delta_g <- Aging_MPG_Pairs$g - Aging_MPG_Pairs$prior_g 
# Aging_MPG_Pairs$delta_mpg <- Aging_MPG_Pairs$mpg - Aging_MPG_Pairs$prior_mpg
# Aging_MPG_Pairs$avg_g <- 2*(Aging_MPG_Pairs$g * Aging_MPG_Pairs$prior_g)/(Aging_MPG_Pairs$g + Aging_MPG_Pairs$prior_g) 
# Aging_MPG_Pairs$age_2 <- Aging_MPG_Pairs$age^2
# Aging_MPG_Pairs$age_3 <- Aging_MPG_Pairs$age^3 
# 
# 
# Age_Min_Group <- group_by(Aging_MPG_Pairs, age, age_2, age_3)
# Aging_MPG <- summarise(Age_Min_Group,
#                    delta_mpg = sum(delta_mpg*avg_g)/sum(avg_g),
#                    delta_g = mean(delta_g),
#                    delta_mp = mean(delta_mp),
#                    players = length(player),
#                    g = sum(avg_g)
# )
# plot(Aging_MPG$age,Aging_MPG$delta_mpg)
# 
# Aging_MPG_Pairs <- merge(Aging_MPG_Pairs,Aging_MPG[,c("age","players")],by = c("age"))
# Aging_MPG_Pairs <- Aging_MPG_Pairs[Aging_MPG_Pairs$players > 50,]
# Aging_MPG_Pairs$players <- NULL



###  Regression Fits for Aging Curves ####

library(MASS)

# Regression aging curve for BPM

Robust_Aging_BPM <- rlm(delta_bpm ~ age + age_2 + age_3 , data=Aging_Pairs, weights = avg_mp)
summary(Robust_Aging_BPM)

# plot(Aging_Pairs$age,resid(Robust_Aging_BPM))

Aging$Fit_Delta_BPM <- predict(Robust_Aging_BPM,newdata=Aging)

# Plot aging curve
plot(Aging_Pairs$age,Aging_Pairs$delta_bpm,pch=19,cex = Aging_Pairs$avg_mp/2000,col=rgb(red=0, green=0, blue=0, alpha=0.05))
lines(Aging$age,Aging$delta_bpm, lwd = 2, col="blue")
lines(Aging$age,Aging$Fit_Delta_BPM, lwd = 2, col="red")
grid(nx = NA, ny = NULL, col = "darkgray")


# Regression aging curve for implied BPM

Robust_Aging_Implied_BPM <- rlm(delta_implied_bpm ~ age + age_2 + age_3, data=Aging_Pairs, weights = avg_mp)
summary(Robust_Aging_Implied_BPM)

# plot(Aging_Pairs$age,resid(Robust_Aging_Implied_BPM))

Aging$Fit_Delta_Implied_BPM <- predict(Robust_Aging_Implied_BPM,newdata=Aging)

# Plot aging curve
plot(Aging_Pairs$age,Aging_Pairs$delta_implied_bpm,pch=19,cex = Aging_Pairs$avg_mp/2000,col=rgb(red=0, green=0, blue=0, alpha=0.05))
lines(Aging$age,Aging$delta_implied_bpm, lwd = 2, col="blue")
lines(Aging$age,Aging$Fit_Delta_Implied_BPM, lwd = 2, col="red")
grid(nx = NA, ny = NULL, col = "darkgray")


# Regression aging curve for equivalent BPM

Robust_Aging_Equiv_BPM <- rlm(delta_equiv_bpm ~ age + age_2 + age_3, data=Aging_Pairs, weights = avg_mp)
summary(Robust_Aging_Equiv_BPM)

# plot(Aging_Pairs$age,resid(Robust_Aging_Equiv_BPM))

Aging$Fit_Delta_Equiv_BPM <- predict(Robust_Aging_Equiv_BPM,newdata=Aging)

# Plot aging curve
plot(Aging_Pairs$age,Aging_Pairs$delta_equiv_bpm,pch=19,cex = Aging_Pairs$avg_mp/2000,col=rgb(red=0, green=0, blue=0, alpha=0.05))
lines(Aging$age,Aging$delta_equiv_bpm, lwd = 2, col="blue")
lines(Aging$age,Aging$Fit_Delta_Equiv_BPM, lwd = 2, col="red")
grid(nx = NA, ny = NULL, col = "darkgray")




# ###  Regression Fits for Aging_MPG Curves ####
# 
# library(MASS)
# 
# # Regression Aging_MPG curve for BPM
# 
# Robust_Aging_MPG <- rlm(delta_mpg ~ age + age_2 + age_3 , data=Aging_MPG_Pairs, weights = avg_g)
# summary(Robust_Aging_MPG)
# 
# # plot(Aging_MPG_Pairs$age,resid(Robust_Aging_MPG_BPM))
# 
# Aging_MPG$Fit_Delta_mpg <- predict(Robust_Aging_MPG,newdata=Aging_MPG)
# 
# # Plot Aging_MPG curve
# plot(Aging_MPG_Pairs$age,Aging_MPG_Pairs$delta_mpg,pch=19,cex = Aging_MPG_Pairs$avg_g/50,col=rgb(red=0, green=0, blue=0, alpha=0.05))
# lines(Aging_MPG$age,Aging_MPG$delta_mpg, lwd = 2, col="blue")
# lines(Aging_MPG$age,Aging_MPG$Fit_Delta_mpg, lwd = 2, col="red")
# grid(nx = NA, ny = NULL, col = "darkgray")



### Era Adjustments ####

Era_Data <- Stats_Dump[,c("player","player_id","age","year_id","team_id","lg_id","g","mp","Equiv_BPM","Playoffs","Injury")]
Era_Data <- Era_Data[Era_Data$mp>150,]
Era_Data <- Era_Data[Era_Data$Playoffs == FALSE,]
Era_Data <- Era_Data[Era_Data$Injury == FALSE,]
Era_Data <- Era_Data[!is.na(Era_Data$Equiv_BPM),]

Era_Group <- group_by(Era_Data, player, player_id,lg_id,year_id,age)
Era_Data <- summarise(Era_Group,
                        equiv_bpm = sum(Equiv_BPM*mp)/sum(mp),
                        mp = sum(mp))

Era_Data_2 <- Era_Data

Era_Data$prior_year <- Era_Data$year_id - 1

names(Era_Data_2)[names(Era_Data_2) == "equiv_bpm"] <-
  "prior_equiv_bpm"
names(Era_Data_2)[names(Era_Data_2) == "age"] <-
  "prior_age"
names(Era_Data_2)[names(Era_Data_2) == "mp"] <-
  "prior_mp"
names(Era_Data_2)[names(Era_Data_2) == "lg_id"] <-
  "prior_lg"

Era_Pairs <- merge(Era_Data,Era_Data_2, by.x=c("player","player_id","prior_year"), by.y = c("player","player_id","year_id"))
Era_Pairs <- Era_Pairs[Era_Pairs$age>23.5 & Era_Pairs$age<31.5,]
Era_Pairs <- merge(Era_Pairs,Aging[,c("age","Fit_Delta_Equiv_BPM")],by = c("age"))
Era_Pairs$Hist_Delta <- Era_Pairs$equiv_bpm - (Era_Pairs$prior_equiv_bpm + Era_Pairs$Fit_Delta_Equiv_BPM)
Era_Pairs$avg_mp <- 2*(Era_Pairs$mp * Era_Pairs$prior_mp)/(Era_Pairs$mp + Era_Pairs$prior_mp)

plot(Era_Pairs[Era$lg_id == "NBA" & Era$prior_lg == "NBA",]$year_id,
     Era_Pairs[Era$lg_id == "NBA" & Era$prior_lg == "NBA",]$Hist_Delta,
     pch=19,cex = Aging_MPG_Pairs$avg_g/50,col=rgb(red=0, green=0, blue=0, alpha=0.05))

Era_Sequence <- group_by(Era_Pairs, year_id,prior_year, lg_id,prior_lg)

Era <- summarise(Era_Sequence,
                   Delta = sum(Hist_Delta*avg_mp)/sum(avg_mp),
                   count = length(player),
                   mp = sum(avg_mp),
                   Avg_Age = mean(age)
)

Era <- Era[Era$count>15,]

Era_NBA <- Era[Era$lg_id == "NBA" & Era$prior_lg == "NBA",]
Era_ABA <- Era[!(Era$lg_id == "NBA" & Era$prior_lg == "NBA"),]

Era_NBA <-
  Era_NBA[order(-Era_NBA$year_id),]

Era_NBA$Era_Adjust <- cumsum(Era_NBA$Delta)
plot(Era_NBA$prior_year,Era_NBA$Era_Adjust)

Era_ABA <-
  Era_ABA[order(-Era_ABA$year_id),]

Era_ABA$Era_Adjust <- cumsum(Era_ABA$Delta) + Era_NBA$Era_Adjust[Era_NBA$prior_year == 1977]
plot(Era_ABA$prior_year,Era_ABA$Era_Adjust)

Era <- rbind(Era_NBA,Era_ABA)

plot(x= NULL, y = NULL,
     xlim=c(1950,2015),ylim=c(-4,2),
     xlab="Year", ylab="Era Adjustment, Equiv. BPM", 
     main="Era Adjustments, NBA and ABA, vs. 2016"
     )
mtext("Leaguewide improvements that apply to all players equally are not captured", cex=0.75, padj = -1)
lines(Era_NBA$prior_year,Era_NBA$Era_Adjust, type= "l",lwd = 2, col="red")
lines(Era_ABA$prior_year,Era_ABA$Era_Adjust, lwd = 2, col="blue")
grid(nx = NULL, ny = NULL, col = "darkgray")
abline(h=0,col = "black")

library(ggplot2)
library(scales)

ggplot() + 
  ggtitle("Era Adjustments, NBA and ABA, vs. 2016") +
  xlab("Year") + ylab("Era Adjustment, Equiv. BPM") +
  
  geom_line(data=Era_NBA,aes(x=prior_year,y=Era_Adjust, color="NBA",size=mp)) +
  geom_line(data=Era_ABA,aes(x=prior_year,y=Era_Adjust, color="ABA",size=mp)) +
  geom_hline(aes(yintercept=0))+
  annotate("text",x=1950,y=-4,label="Note: Leaguewide improvements that apply to all players equally are not captured",hjust=0,vjust=0)+

  scale_x_continuous(breaks=seq(1950,2015,by=10),limits=c(1950,2015)) +
  scale_y_continuous(breaks=seq(-4,2,by=1),limits=c(-4,2)) +
  scale_size(range=c(0.5,3),name="Minutes for Deltas",labels=comma) +
  scale_color_discrete(name="League") +
  
  guides(color = guide_legend(override.aes = list(size=3)))+
  
  theme(
    plot.title = element_text(size=16,face="bold",vjust=0),
    axis.title.x = element_text(size=14, vjust=-0.35),
    axis.title.y = element_text(size=14, vjust=1.35),
    
    legend.title = element_text(size=12),
    legend.position="top",
    legend.box="horizontal",
    legend.key=element_rect(fill=NA),
    panel.grid.minor = element_blank()
    )
# ggsave(filename="~/ETC/Sports/NBA/Hall_Rating/Era_Adjustment_GGPlot.png",width=4,height=3,dpi=300,units="in")




### Era MPG Sum Adjustments ####
library (magrittr)

Era_MPG_Data <- Stats_Dump[,c("player","player_id","age","year_id","G",
                              "team_id","lg_id","g","mp","mpg","Playoffs","Injury")] %>%
  .[.$Playoffs == FALSE,] %>%
  .[.$Injury == FALSE,] %>%
  .[.$age > 24,] %>%
  .[.$age< 31,]

Era_MPG_Group <- group_by(Era_MPG_Data, player, player_id,lg_id,year_id,age,G)
Era_MPG_Data <- summarise(Era_MPG_Group,
                      mpg = sum(mp)/sum(g),
                      mp = sum(mp),
                      g = sum(g),
                      ReMPG = sum(mp)/(sum(g) + 4))

Era_MPG_Data <- Era_MPG_Data[Era_MPG_Data$g > 5, ]

Era_MPG_Data_2 <- Era_MPG_Data

Era_MPG_Data$prior_year <- Era_MPG_Data$year_id - 1

names(Era_MPG_Data_2)[names(Era_MPG_Data_2) == "mpg"] <-
  "prior_mpg"
names(Era_MPG_Data_2)[names(Era_MPG_Data_2) == "ReMPG"] <-
  "prior_ReMPG"
names(Era_MPG_Data_2)[names(Era_MPG_Data_2) == "mp"] <-
  "prior_mp"
names(Era_MPG_Data_2)[names(Era_MPG_Data_2) == "age"] <-
  "prior_age"
names(Era_MPG_Data_2)[names(Era_MPG_Data_2) == "g"] <-
  "prior_g"
names(Era_MPG_Data_2)[names(Era_MPG_Data_2) == "G"] <-
  "prior_G"
names(Era_MPG_Data_2)[names(Era_MPG_Data_2) == "lg_id"] <-
  "prior_lg"

Era_MPG_Pairs <- merge(Era_MPG_Data,Era_MPG_Data_2, 
                       by.x = c("player","player_id","prior_year"), 
                       by.y = c("player","player_id","year_id"))

Era_MPG_Pairs$avg_g <- 2*(Era_MPG_Pairs$g * Era_MPG_Pairs$prior_g)/(Era_MPG_Pairs$g + Era_MPG_Pairs$prior_g)

Era_MPG_Sequence <- group_by(Era_MPG_Pairs, year_id,prior_year, lg_id,prior_lg)

Era_MPG <- summarise(Era_MPG_Sequence,
                 Delta_ReMPG= sum(ReMPG)/sum(prior_ReMPG),
                 Delta_mpg= sum(mpg)/sum(prior_mpg),
                 Delta_min= (sum(mp)/mean(G))/(sum(prior_mp)/mean(prior_G)),
                 count = length(player),
                 g = sum(avg_g)
)

Era_MPG <- Era_MPG[Era_MPG$count>15,]

Era_MPG_NBA <- Era_MPG[Era_MPG$lg_id == "NBA" & Era_MPG$prior_lg == "NBA",]
Era_MPG_ABA <- Era_MPG[!(Era_MPG$lg_id == "NBA" & Era_MPG$prior_lg == "NBA"),]

Era_MPG_NBA <-
  Era_MPG_NBA[order(-Era_MPG_NBA$year_id),]

Era_MPG_NBA <- Era_MPG_NBA[Era_MPG_NBA$year_id<2016,]

Era_MPG_NBA$Era_ReMPG_Adjust <- cumprod(Era_MPG_NBA$Delta_ReMPG)
Era_MPG_NBA$Era_mpg_Adjust <- cumprod(Era_MPG_NBA$Delta_mpg)
Era_MPG_NBA$Era_min_Adjust <- cumprod(Era_MPG_NBA$Delta_min)
plot(Era_MPG_NBA$prior_year,Era_MPG_NBA$Era_ReMPG_Adjust)
plot(Era_MPG_NBA$prior_year,Era_MPG_NBA$Era_mpg_Adjust)

Era_MPG_ABA <-
  Era_MPG_ABA[order(-Era_MPG_ABA$year_id),]

Era_MPG_ABA$Era_ReMPG_Adjust <- cumprod(Era_MPG_ABA$Delta_ReMPG) * Era_MPG_NBA$Era_ReMPG_Adjust[Era_MPG_NBA$prior_year == 1977]
Era_MPG_ABA$Era_mpg_Adjust <- cumprod(Era_MPG_ABA$Delta_mpg) * Era_MPG_NBA$Era_mpg_Adjust[Era_MPG_NBA$prior_year == 1977]
Era_MPG_ABA$Era_min_Adjust <- cumprod(Era_MPG_ABA$Delta_min) * Era_MPG_NBA$Era_min_Adjust[Era_MPG_NBA$prior_year == 1977]
plot(Era_MPG_ABA$prior_year,Era_MPG_ABA$Era_ReMPG_Adjust)

Era_MPG <- rbind(Era_MPG_NBA,Era_MPG_ABA)

plot(x= NULL, y = NULL,
     xlim=c(1950,2015),ylim=c(0.4,1.2),
     xlab="Year", ylab="Era_MPG Adjustment, Equiv. BPM", 
     main="Era_MPG Adjustments, NBA and ABA, vs. 2016"
)
mtext("Leaguewide improvements that apply to all players equally are not captured", cex=0.75, padj = -1)
lines(Era_MPG_NBA$prior_year,Era_MPG_NBA$Era_ReMPG_Adjust, type= "l",lwd = 2, col="red")
lines(Era_MPG_ABA$prior_year,Era_MPG_ABA$Era_ReMPG_Adjust, lwd = 2, col="blue")
grid(nx = NULL, ny = NULL, col = "darkgray")
abline(h=0,col = "black")

library(ggplot2)
library(scales)

ggplot() + 
  ggtitle("Era_MPG Adjustments, NBA and ABA, vs. 2016") +
  xlab("Year") + ylab("Era_MPG Adjustment, Equiv. BPM") +
  
  geom_line(data=Era_MPG_NBA,aes(x=prior_year,y=Era_min_Adjust, color="NBA",size=g)) +
  geom_line(data=Era_MPG_ABA,aes(x=prior_year,y=Era_min_Adjust, color="ABA",size=g)) +
  geom_hline(aes(yintercept=0))+
  annotate("text",x=1950,y=-4,label="Note: Leaguewide improvements that apply to all players equally are not captured",hjust=0,vjust=0)+
  
  scale_x_continuous(breaks=seq(1950,2015,by=10),limits=c(1950,2015)) +
  scale_y_continuous(breaks=seq(0.0,1.4,by=0.1),limits=c(0.0,1.4)) +
  scale_size(range=c(0.5,3),name="Games for Deltas",labels=comma) +
  scale_color_discrete(name="League") +
  
  guides(color = guide_legend(override.aes = list(size=3)))+
  
  theme(
    plot.title = element_text(size=16,face="bold",vjust=0),
    axis.title.x = element_text(size=14, vjust=-0.35),
    axis.title.y = element_text(size=14, vjust=1.35),
    
    legend.title = element_text(size=12),
    legend.position="top",
    legend.box="horizontal",
    legend.key=element_rect(fill=NA),
    panel.grid.minor = element_blank()
  )
# ggsave(filename="~/ETC/Sports/NBA/Hall_Rating/Era_MPG_Adjustment_GGPlot.png",width=4,height=3,dpi=300,units="in")












### Generate Hall Rating ####

Stats_Dump <- merge(Stats_Dump,Era[,c("prior_lg","prior_year","Era_Adjust")],by.x = c("lg_id","year_id"),by.y = c("prior_lg","prior_year"),all.x = TRUE)
Stats_Dump$Era_Adjust[Stats_Dump$year_id==2016] <- 0
Stats_Dump$Adjusted_BPM <- Stats_Dump$Equiv_BPM+Stats_Dump$Era_Adjust

Stats_Dump$Adj_VORP <- with(Stats_Dump,(Adjusted_BPM*mp+2)/(48*82))
Stats_Dump$VOA <- with(Stats_Dump,Adjusted_BPM*mp/(48*82))
Stats_Dump$VO_Finals <- with(Stats_Dump,(Adjusted_BPM-2)*mp/(48*82))

Stats_Dump$Raw_Hall_Rate <- with(Stats_Dump,ifelse(VOA<0,0,VOA)+ifelse(VO_Finals<0,0,VO_Finals))

## Add adjustments to playoffs -- increase value and also length of shorter playoffs and regular seasons

Stats_Dump <- merge(Stats_Dump,Season_Length,by = c("year_id","lg_id"),all.x = TRUE)


Stats_Dump$Adj_Hall_Rate <- with(Stats_Dump,ifelse(Playoffs == TRUE,Raw_Hall_Rate*mult_factor,Raw_Hall_Rate*(82*Length_Factor_Wt+season_length*(1-Length_Factor_Wt))/season_length))


###  Player Rating Horizons ####

# Look at the ith best player in each season, by Equiv_VORP, and compare their Equiv_BPM Rates (Graph)

Rating_Ranks <-   Stats_Dump[,c("player","player_id","age","year_id","team_id","lg_id","mp","g","G","bpm","Equiv_VORP","Equiv_BPM","Playoffs","Injury")] %>%
  .[.$mp>500,] %>%
  .[.$year_id<2016,] %>%
  .[.$Playoffs == FALSE,] %>%
  .[.$lg_id == "NBA",] %>%
  .[!is.na(.$Equiv_BPM),] %>%
  .[.$Injury==FALSE,] %>%
  arrange(year_id, -Equiv_BPM) %>%
  group_by(year_id) %>%
  mutate(Rating_Rank=row_number())


ggplot() + 
  ggtitle("NBA Historical Rating Rank Horizons") +
  xlab("Year") + ylab("Rating Horizon, Equiv. BPM") +
 
  geom_line(data=Rating_Ranks[Rating_Ranks$Rating_Rank == 1,],aes(x=year_id,y=Equiv_BPM, color="1"), size=1.2) +
  geom_line(data=Rating_Ranks[Rating_Ranks$Rating_Rank == 2,],aes(x=year_id,y=Equiv_BPM, color="2"), size=1.2) +
  geom_line(data=Rating_Ranks[Rating_Ranks$Rating_Rank == 5,],aes(x=year_id,y=Equiv_BPM, color="5"), size=1.2) +
  geom_line(data=Rating_Ranks[Rating_Ranks$Rating_Rank == 10,],aes(x=year_id,y=Equiv_BPM, color="10"), size=1.2) +
  geom_line(data=Rating_Ranks[Rating_Ranks$Rating_Rank == 15,],aes(x=year_id,y=Equiv_BPM, color="15"), size=1.2) +
  geom_line(data=Rating_Ranks[Rating_Ranks$Rating_Rank == 20,],aes(x=year_id,y=Equiv_BPM, color="25"), size=1.2) +
  geom_line(data=Rating_Ranks[Rating_Ranks$Rating_Rank == 25,],aes(x=year_id,y=Equiv_BPM, color="35"), size=1.2) +
  # geom_hline(aes(yintercept=0))+
  # annotate("text",x=1950,y=-4,label="Note: Leaguewide improvements that apply to all players equally are not captured",hjust=0,vjust=0)+
  
  scale_x_continuous(breaks=seq(1950,2015,by=10),limits=c(1950,2015)) +
  scale_y_continuous(breaks=seq(-1,11,by=1),limits=c(-1,11)) +
  scale_color_discrete(name="Rating Rank") +
  
  guides(color = guide_legend(override.aes = list(size=3)))+
  
  theme(
    plot.title = element_text(size=16,face="bold",vjust=0),
    axis.title.x = element_text(size=14, vjust=-0.35),
    axis.title.y = element_text(size=14, vjust=1.35),
    
    legend.title = element_text(size=12),
    legend.position="top",
    legend.box="horizontal",
    legend.key=element_rect(fill=NA),
    panel.grid.minor = element_blank()
  )


ggplot(Rating_Ranks,aes(x=year_id,y=Equiv_BPM)) + geom_boxplot(aes(group=year_id))

# ggplot(Rating_Ranks,aes(x=year_id,y=Equiv_BPM)) + stat_density_2d(geom="raster", aes(fill= ..density..), contour = FALSE)
# last_plot()+ scale_fill_gradient(limits=c(1e-4,0.004))
# 
# ggplot(Rating_Ranks,aes(x=year_id,y=Equiv_BPM)) + stat_density_2d(contour = TRUE)
# ggplot(Rating_Ranks,aes(x=year_id,y=Equiv_BPM)) + stat_density_2d(geom="point", aes(size= ..density..), contour = FALSE)
# last_plot()  + stat_density_2d(contour = TRUE)
  


# ggsave(filename="~/ETC/Sports/NBA/Hall_Rating/Era_Adjustment_GGPlot.png",width=4,height=3,dpi=300,units="in")




# write.csv(Stats_Dump, file = "~/ETC/Sports/NBA/Hall_Rating/Hall_Data_Dump.csv", row.names = FALSE)

### Player Sums ####

Stats_Dump <- Stats_Dump[!is.na(Stats_Dump$Implied_BPM),]  # Remove records that were expunged + 1954 round robin losers

library(dplyr)
Player_ID_Group <- group_by(Stats_Dump, player, player_id)
Hall_Rating_Sum <- summarise(Player_ID_Group,
                                  Hall_Rating = sum(Adj_Hall_Rate),
                                  Reg_Season_Hall_Rating = sum(Adj_Hall_Rate[Playoffs == FALSE]),
                                  Playoff_Hall_Rating = sum(Adj_Hall_Rate[Playoffs == TRUE]),
                                  Est_BPM = sum(Adjusted_BPM*mp)/sum(mp),
                                  Seasons = length(unique(year_id)),
                                  Best_Season = max(Adj_Hall_Rate),
                                  Rookie_Yr = min(year_id),
                                  Final_Yr = max(year_id),
                                  Minutes = sum(mp),
                                  Games = sum(g),
                                  MPG = sum(mp)/sum(g),
                                  Teams = length(unique(team_id)),
                                  Primary_Team = names(which.max(table(team_id))),
                                  Position = names(which.max(table(pos))))

Hall_Rating_Sum$Rank <- rank(-Hall_Rating_Sum$Hall_Rating)
Hall_Rating_Sum$Reg_Season_Rank <- rank(-Hall_Rating_Sum$Reg_Season_Hall_Rating)
Hall_Rating_Sum$Playoff_Rank <- rank(-Hall_Rating_Sum$Playoff_Hall_Rating)

Hall_Rating_Sum <-
  Hall_Rating_Sum[,c("Rank",
                     "player",
                     "Hall_Rating",
                     "Reg_Season_Rank",
                     "Reg_Season_Hall_Rating",
                     "Playoff_Rank",
                     "Playoff_Hall_Rating",
                     "Est_BPM",
                     "Best_Season",
                     "Position",
                     "MPG",
                     "Minutes",
                     "Games",
                     "Seasons",
                     "Rookie_Yr",
                     "Final_Yr",
                     "Primary_Team",
                     "Teams",
                     "player_id"
                       )]


Hall_Rating_Sum <-
  Hall_Rating_Sum[order(Hall_Rating_Sum$Rank),]

write.csv(Hall_Rating_Sum, file = Output_File, row.names = FALSE)
