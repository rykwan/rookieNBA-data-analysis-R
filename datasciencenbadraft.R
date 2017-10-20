draftdata <- read.csv("../Desktop/nbadraft.csv")
# explore correlation for these values to see how college shooting translates to nba
dft.shot <- subset(draftdata, select = c("nba_fg_pct", "nba_3pt_fg_pct", "coll_fg_pct", "coll_ft_pct", "coll_3pt_pct"))
#remove '%' from values
dft.shot$nba_fg_pct <- as.numeric(sub("%", "", dft.shot$nba_fg_pct))
dft.shot$nba_3pt_fg_pct <- as.numeric(sub("%", "", dft.shot$nba_3pt_fg_pct))
dft.shot$coll_fg_pct <- as.numeric(sub("%", "", dft.shot$coll_fg_pct))
dft.shot$coll_ft_pct <- as.numeric(sub("%", "", dft.shot$coll_ft_pct))
dft.shot$coll_3pt_pct <- as.numeric(sub("%", "", dft.shot$coll_3pt_pct))
# make missing values 0
dft.shot[is.na(dft.shot)] <- 0
#correlation
cor(dft.shot)
# insight 1: to predict how good a player will be at shooting 3 pointers in the nba, college free throw pct is as good a predictor as college 3pt pct
#logistic regression for chance of all star
draftdata$coll_fg_pct <- as.numeric(sub("%","",draftdata$coll_fg_pct))
allstar.out <- glm(nba_allstar~draft_pick+rk_pp36+coll_ppg+rk_per+coll_fg_pct,data=draftdata, family="binomial")
allstar.out.tab <- coef(summary(allstar.out))
allstar.out.tab[,"Estimate"] <- exp(coef(allstar.out))
allstar.out.tab
#predict based on draft position 1,5,10,15,20, assuming average rookie season and avg drafted college player stats
predictData <- with(draftdata, expand.grid(draft_pick = c(1,5,10,15,20), rk_pp36 = mean(rk_pp36, na.rm = TRUE), coll_ppg = mean(coll_ppg, na.rm=TRUE), rk_per = mean(rk_per), coll_fg_pct = mean(coll_fg_pct, na.rm = TRUE)))
cbind(predictData, predict(allstar.out, type = "response", se.fit = TRUE, interval = "confidence", newdata = predictData))
# insight 2: predictive model suggests 1st overall picks have 27% chance of being all star, then 21%,16%,12%, and 8% for picks5,10,15,20 but assuming an average rookie season
#predict based on some real current draft prospects 
fultz <- with(draftdata, expand.grid(draft_pick = c(1), rk_pp36 = mean(rk_pp36, na.rm = TRUE), coll_ppg = 23.2, rk_per = mean(rk_per), coll_fg_pct = 47.6))
lonzo <- with(draftdata, expand.grid(draft_pick = c(1,2,3), rk_pp36 = mean(rk_pp36, na.rm = TRUE), coll_ppg = 14.6, rk_per = mean(rk_per), coll_fg_pct = 55.1))
dfox <- with(draftdata, expand.grid(draft_pick = c(2,3,5), rk_pp36 = mean(rk_pp36, na.rm = TRUE), coll_ppg = 16.7, rk_per = mean(rk_per), coll_fg_pct = 47.9))
cbind(fultz, predict(allstar.out, type = "response", se.fit = TRUE, interval = "confidence", newdata = fultz))
# insight 3: predicts that Markelle Fultz, the projected number one pick this year, has 45% chance of becoming allstar, better than the 27% for avg prospect
cbind(lonzo, predict(allstar.out, type = "response", se.fit = TRUE, interval = "confidence", newdata = lonzo))
# insight 4: predicts that Lonzo Ball has 24% chance of being all-star if selected 2nd by the Lakers this yr
cbind(dfox, predict(allstar.out, type = "response", se.fit = TRUE, interval = "confidence", newdata = dfox))
# insight 5: predicts that DeAaron Fox has 28% chance of being all-star if selected 2nd instead of Lonzo Ball

