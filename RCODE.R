# Install relevant libraries.

library(haven)
library(data.table)
library(readr)
library(stargazer)
library(jtools)
library(coefplot)
library(ggplot2)
library(ggpubr)
library(sjPlot)
library(sjmisc)
library(sjlabelled)


# Proper working directory for this file is ~/anes_timeseries_2016_dta
# Input the anes file.

df <- read_dta("anes_timeseries_2016.dta")

# Filter out all responses that did not have a post-election interview.

df <- subset(df, df$V160502 == 1)

# Renaming of all relevant variables to the analysis, so that future code is 
# easier to read.

oldnames <- c("V162031","V162110","V162113","V162320","V162321")
newnames <- c("vote","copwarmth","blmwarmth","polbiasbase","polbiasmod")

setnames(df, old=oldnames, new=newnames)

# Begin subsetting the data frame to include only subjects which have all
# relevant questions answered.

df <- subset(df, `>`(df$vote, 0))
df <- subset(df, `>`(df$copwarmth, 0))
df <- subset(df, `>`(df$blmwarmth, 0))
df <- subset(df, `>`(df$polbiasbase, 0))
df <- subset(df, `>`(df$polbiasmod, 0))

df <- subset(df, `<`(df$vote, 5))
df <- subset(df, `<`(df$copwarmth, 101))
df <- subset(df, `<`(df$blmwarmth, 101))

# Begin converting the values of "polbiasbase" and "polbiasmod" into 
# a 7 point scale.

df$polbiasbase <- df$polbiasbase + 2
df$polbiasmod <- df$polbiasmod - 1
for (x in 1:length(df$polbiasbase)){
  if (df$polbiasbase[x] == 3){
    df$polbiasbase[x] <- 3 - df$polbiasmod[x]
  }
  if (df$polbiasbase[x] == 5){
    df$polbiasbase[x] <- 5 + df$polbiasmod[x]
  }
}

# Simplify the voter categories from 4 responses down to 2, as 3 values
# represent the same end result but for different reasons.

df_vote_filter <- (df$vote == 4)
for (x in 1:length(df_vote_filter)){
  if (df_vote_filter[x]) {
    df$vote[x] <- 1
  } else {
    df$vote[x] <- 0
  }
}
# Begin sub-setting into Republic and Democrat subsets.

dem_df <- subset(df, df$V161155 == 1)
rep_df <- subset(df, df$V161155 == 2)

blm_df <- df$blmwarmth
cop_df <- df$copwarmth
pol_df <- df$polbiasbase
# Begin breaking down individual statistics below, now that initial data-frames
# are created + glms that represent each variable independently

m1 <- glm(vote ~ blmwarmth + copwarmth + polbiasbase, data = dem_df, family = binomial)

m2 <- glm(vote ~ blmwarmth + copwarmth + polbiasbase, data = rep_df, family = binomial)

g1<- glm(vote ~ blmwarmth , data = df, family = binomial)
summary(g1)
g2<-glm(vote ~ copwarmth , data = df, family = binomial)
summary(g2)
g3<-glm(vote ~ polbiasbase , data = df, family = binomial)
summary(g3)
g4<-glm(vote ~ polbiasmod , data = df, family = binomial)
summary(g4)

#Summaries of all our binary variables: Frequency bar plots
plot(table(df$blmwarmth),main="blm warmth frequencies",xlab="blm warmth rating",ylab="frequency")
plot(table(df$copwarmth),main="cop warmth frequencies",xlab="cop warmth rating",ylab="frequency")
plot(table(df$vote),main="voting frequencies",xlab="vote(1 is 'I voted' or 0 is 'I did not vote')",ylab="frequency")
plot(table(df$polbiasbase),main="racial bias (1 = participant thinks racial bias towards whites, 7 = racial bias towards blacks)",xlab="racial bias",ylab="frequency")
plot(table(df$polbiasmod),main="how much better do police treat whites than blacks? 1=much 2=moderate 3=a little" ,xlab="police bias",ylab="frequency")

#create a linear regression table of 1 independent variable, and 3 independent variables for our logit model
tab_model(g4)
tab_model(g1,g2,g3)

# rough draft predictive probability plots
blmw <- predict(g1, type = "response")
hist(blmw,main="histogram of predictive probabilities of voting chance based on BLM warmth",xlab="predictive probabilities")
cop <- predict(g2, type = "response")
hist(cop,main="histogram of predictive probabilities of voting chance based on police warmth",xlab="predictive probabilities")
pol <- predict(g3, type = "response")
hist(pol,main="histogram of predictive probabilities of voting chance based on opinion of police bias",xlab="predictive probabilities")
pol2 <- predict(g4, type = "response")
hist(pol2,main="histogram of predictive probabilities of voting chance based on police bias opinions(furthered)",xlab="predictive probabilities")

#plots of log odds for each of our independent variables
plot1 <- effect_plot(g1,pred=blmwarmth,interval=FALSE, x.label = "Warmth Towards BLM",
                     y.label = "Log Odds of Voting",
                     main.title = "BLM Warmth vs. Voting")
plot2 <- effect_plot(g2,pred=copwarmth,interval=FALSE, x.label = "Warmth Towards Police",
                     y.label = "Log Odds of Voting",
                     main.title = "Police Warmth vs. Voting")
plot3 <- effect_plot(g4,pred=polbiasmod,interval=FALSE, x.label = "Police Racial Bias Opinion Level",
                     y.label = "Log Odds of Voting",
                     main.title = "Police Racial Bias Opinion vs. Voting")


#FINAL PREDICTIVE PROBABILITY PLOTS
# COPY EVERYTHING BETWEEN TWO NOTES TO MAKE AN EFFECT PLOT
### START OF A SINGLE GRAPH ###
m1.plot.blm <- effect_plot(m1, pred = blmwarmth, interval = TRUE, 
                           x.label = "Warmth Towards BLM",
                           y.label = "Probablilty of Voting",
                           main.title = "Democrat BLM Warmth vs. Voting")
m1.plot.blm+
  ylim(0, 1)
### END OF A SINGLE GRAPH ###

m1.plot.cop <- effect_plot(m1, pred = copwarmth, interval = TRUE, 
                           x.label = "Warmth Towards Police",
                           y.label = "Probablilty of Voting",
                           main.title = "Democrat Police Warmth vs. Voting")
m1.plot.cop+
  ylim(0, 1)

m1.plot.bias <- effect_plot(m1, pred = polbiasbase, interval = TRUE, 
                            x.label = "Police Racial Bias",
                            y.label = "Probablilty of Voting",
                            main.title = "Democrat Police Racial Bias vs. Voting")
m1.plot.bias+
  ylim(0, 1)

m2.plot.blm <- effect_plot(m2, pred = blmwarmth, interval = TRUE, 
                           x.label = "Warmth Towards BLM",
                           y.label = "Probablilty of Voting",
                           main.title = "Republican BLM Warmth vs. Voting")
m2.plot.blm+
  ylim(0, 1)

m2.plot.cop <- effect_plot(m2, pred = copwarmth, interval = TRUE, 
                           x.label = "Warmth Towards Police",
                           y.label = "Probablilty of Voting",
                           main.title = "Republican Police Warmth vs. Voting")
m2.plot.cop+
  ylim(0, 1)

m2.plot.bias <- effect_plot(m2, pred = polbiasbase, interval = TRUE, 
                            x.label = "Police Racial Bias",
                            y.label = "Probablilty of Voting",
                            main.title = "Republican Police Racial Bias vs. Voting")
m2.plot.bias+
  ylim(0, 1)
