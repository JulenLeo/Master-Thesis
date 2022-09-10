# packages
Sys.unsetenv("GITHUB_PAT")
Sys.getenv("GITHUB_PAT")
devtools::install_github("m-clark/gammit")
pacman::p_load(ggplot2, tibble, dplyr, mgcv, tidyverse, gratia,
               sjPlot, sjmisc, MASS, gamm4, mgcViz, devtools, Metrics, DHARMa,
               glmmTMB, plotly)

# data

players_df <- readRDS("data/players_all.Rds")

# merge observations of the same year

players <- players_df[FALSE,]
ids <- unique(players_df$player_id)
years <- unique(players_df$year)
for(id in ids) {
  for (year2 in years) {
    aux <- subset(players_df, player_id == id & year == year2)
    if (nrow(aux) > 0) {
      aux <- aux[order(-aux$minutes_played), ]
      if (nrow(aux) > 1) {
        for (i in 2:nrow(aux)) {
          aux[1,9:10] <- aux[1,9:10] + aux[i,9:10]
        }
      }
      players <- rbind(players, aux[1,])
    }
  }
}
players_merged <- players


# players with at least 180 minutes in a season

players <- subset(players, minutes_played >= 180)


# players with no NA-s

players <- players[!is.na(players$market_value),]
players <- players[!is.na(players$position),]
players <- players[!is.na(players$foot),]


# players with at least 8 consecutive observations

ids <- unique(players$player_id)
players8 <- players[FALSE,]
for (id in ids) {
  player <- subset(players, player_id == id)
  dif <- player$year[nrow(player)] - player$year[1]
  if (nrow(player) > 8 & nrow(player) == dif + 1) {
    players8 <- rbind(players8, player)
  }
}
players <- players8

saveRDS(players, "analysis/data/players.RDs", compress = TRUE)
rm(list = ls())
players <- readRDS("analysis/data/players.Rds")
options(scipen = 999)


# remove goalkeepers (to consider only field players)

players0 <- players %>% filter(position != 'Goalkeeper')


# Convert to factor and some changes

players0$player_name <- as.factor(players0$player_name)
players0$position <- as.factor(players0$position)
players0$position2 <- as.factor(players0$position2)
players0$player_id <- as.factor(players0$player_id)
players0$club_name <- as.factor(players0$club_name)

players0$league <- factor(players0$league,levels = c('laliga','premier','seriea','bundesliga','ligue1'))

players0$minutes_per_match <- (players0$minutes_played/players0$matches_played)

players0$yellows_per_match <- (players0$yellows/players0$minutes_played)*90
players0$reds_per_match <- (players0$reds/players0$minutes_played)*90

players0$goals_per_match <- (players0$goals/players0$minutes_played)*90
players0$assists_per_match <- (players0$assists/players0$minutes_played)*90


players0$injuries_per_match <- (players0$injuries_total/players0$minutes_played)*90
players0$absence_per_match <- (players0$days_lost_total/players0$minutes_played)*90

## Descriptive plots
# market_Value

ggplot(players0, aes(x = market_value)) +
  geom_histogram(aes(y = ..density..), color = "darkblue", fill = "lightblue") +
  geom_density(colour="darkblue", fill="lightblue", alpha=0.2, adjust=3)

ggplot(players0, aes(sample = market_value) ) +
  stat_qq(color = "darkblue") +
  stat_qq_line(color = "darkblue")

ggplot(players0, aes(x = log10(market_value))) +
  geom_histogram(aes(y = ..density..), color = "darkgreen", fill = "lightgreen") +
  geom_density(colour="darkgreen", fill="lightgreen", alpha=0.2, adjust=3)

ggplot(players0, aes(sample = log10(market_value))) +
  stat_qq(color = "darkgreen") +
  stat_qq_line(color = "darkgreen")

# age

ggplot(players0, aes(age, market_value)) +
  geom_point() +
  stat_smooth()

ggplot(players0, aes(age, log10(market_value))) +
  geom_point() +
  stat_smooth()

first_players<-players0[c(1:243),]

ggplot(first_players, aes(x = age, y = market_value, color = player_name)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  facet_wrap(~ player_name) + 
  theme(legend.position = "none")

ggplot(first_players, aes(x = age, y = log10(market_value), color = player_name)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  facet_wrap(~ player_name) + 
  theme(legend.position = "none")

# year

ggplot(players0, aes(year, market_value)) +
  geom_point() +
  stat_smooth()

# league

ggplot(players0, aes(x= league , y=  log10(market_value), fill = league)) +
  geom_boxplot()


# minutes_played

ggplot(players0, aes(minutes_played, market_value)) +
  geom_point() +
  stat_smooth()

# minutes_per_match

ggplot(players0, aes(minutes_per_match, market_value)) +
  geom_point() +
  stat_smooth()


# goals

ggplot(players0, aes(goals_per_match, market_value)) +
  geom_point() +
  stat_smooth()

ggplot(players0, aes(goals_per_match, market_value, colour = position2)) +
  geom_point() +
  stat_smooth(method = lm)


# assists

ggplot(players0, aes(assists_per_match, market_value)) +
  geom_point() +
  stat_smooth()

ggplot(players0, aes(assists_per_match, market_value, colour = position2)) +
  geom_point() +
  stat_smooth(method = lm)


# yellows_per_match

ggplot(players0, aes(yellows_per_match, market_value)) +
  geom_point() +
  stat_smooth()

ggplot(players0, aes(yellows_per_match, market_value, colour = position2)) +
  geom_point() +
  stat_smooth(method = lm)


# reds_per_match

ggplot(players0, aes(reds_per_match, market_value)) +
  geom_point() +
  stat_smooth()

ggplot(players0, aes(reds_per_match, market_value, colour = position2)) +
  geom_point() +
  stat_smooth(method = lm)


# height

ggplot(players0, aes(height, market_value)) +
  geom_point() +
  stat_smooth()

ggplot(players0, aes(height, market_value, colour = position2)) +
  geom_point() +
  stat_smooth(method = lm)


# position

ggplot(players0, aes(x = position , y =  log10(market_value), fill = position)) +
  geom_boxplot()


# position2

ggplot(players0, aes(x = position2 , y =  log10(market_value), fill = position2)) +
  geom_boxplot()

# foot

ggplot(players0, aes(x = foot , y =  log10(market_value), fill = foot)) +
  geom_boxplot()


# ranking

ggplot(players0, aes(ranking, market_value)) +
  geom_point() +
  stat_smooth()

ggplot(players0, aes(x = as.factor(ranking) , y =  log10(market_value), fill = as.factor(ranking))) +
  geom_boxplot()


# points

ggplot(players0, aes(points, market_value)) +
  geom_point() +
  stat_smooth()



################################################################################
#
# MODELING ---------------------------------------------------------------------
#
################################################################################

# Train & Test

train <- players0 %>% filter(year < 2021)
test <- players0 %>% filter(year >= 2021)

models_info <- data.frame(Model = character(),
                          AIC = double(),
                          BIC = double(),
                          MAE = double())

## lm with no covariates

mod0 <- lm(log10(market_value) ~ 1 , data = train)
summary(mod0)
int <- 6.807270
train$predicted_mod0 <- 10^predict(mod0)
first_players<-train[c(1:297),]


ggplot(first_players, aes(x = age, y = predicted_mod0, color = player_name)) +
  geom_line() +
  geom_point(aes(x = age, y = market_value, color = player_name)) +
  geom_abline(intercept = 10^int, slope = 0, color = "black", linetype = "dashed") +
  theme_bw() +
  facet_wrap(~ player_name) + 
  theme(legend.position = "none")

models_info[1,] <- c("mod0", AIC(mod0), BIC(mod0),
                     mae(train$market_value, train$predicted_mod0))


# mixed model with player as random effect

mod1 <- bam(log10(market_value) ~ s(player_id,bs = 're'),
            data = train, select = TRUE)
summary(mod1)

pp <- predict(mod1,type='response',se.fit = TRUE)
# transformed back the log10
pp.fit <- 10^pp$fit
pp.lower <- 10^(pp$fit-1.96*pp$se.fit)
pp.upper <- 10^(pp$fit+1.96*pp$se.fit)
train$predicted_mod1 <- pp.fit
train$predicted_mod1.l <- pp.lower
train$predicted_mod1.u <- pp.upper

first_players<-train[c(1:297),]

ggplot(first_players, aes(x = age, y = predicted_mod1, color = player_name)) +
  geom_line() +
  geom_ribbon(aes(ymin = predicted_mod1.l, ymax = predicted_mod1.u), alpha = 0.1, linetype = 'dashed',size=0) +
  geom_point(aes(x = age, y = market_value, color = player_name)) +
  theme_bw() +
  facet_wrap(~ player_name) + 
  theme(legend.position = "none") +
  labs(y = "market_value")

models_info[2,] <- c("mod1", AIC(mod1), BIC(mod1),
                     mae(train$market_value, train$predicted_mod1))

#gets better


# league as random effect

mod2 <- bam(log10(market_value) ~
              s(player_id,bs = 're') +
              s(league, bs= 're') ,
            data = train, select = TRUE)
summary(mod2)

pp <- predict(mod2,type='response',se.fit = TRUE)
pp.fit <- 10^pp$fit
pp.lower <- 10^(pp$fit-1.96*pp$se.fit)
pp.upper <- 10^(pp$fit+1.96*pp$se.fit)
train$predicted_mod2 <- pp.fit
train$predicted_mod2.l <- pp.lower
train$predicted_mod2.u <- pp.upper

first_players<-train[c(1:297),]

ggplot(first_players, aes(x = age, y = predicted_mod2, color = player_name)) +
  geom_line() +
  geom_ribbon(aes(ymin = predicted_mod2.l, ymax = predicted_mod2.u), alpha = 0.1, linetype = 'dashed',size=0) +
  geom_point(aes(x = age, y = market_value, color = player_name)) +
  theme_bw() +
  facet_wrap(~ player_name) + 
  theme(legend.position = "none") +
  labs(y = "market_value")

models_info[3,] <- c("mod2", AIC(mod2), BIC(mod2),
                     mae(train$market_value, train$predicted_mod2))


# club as random effect

mod3 <- bam(log10(market_value) ~
              s(player_id,bs = 're') +
              s(league, bs = 're') +
              s(club_name, bs = 're'),
            data = train, select = TRUE)
summary(mod3)

pp <- predict(mod3,type='response',se.fit = TRUE)
pp.fit <- 10^pp$fit
pp.lower <- 10^(pp$fit-1.96*pp$se.fit)
pp.upper <- 10^(pp$fit+1.96*pp$se.fit)
train$predicted_mod3 <- pp.fit
train$predicted_mod3.l <- pp.lower
train$predicted_mod3.u <- pp.upper

first_players<-train[c(1:297),]

ggplot(first_players, aes(x = age, y = predicted_mod3, color = player_name)) +
  geom_line() +
  geom_ribbon(aes(ymin = predicted_mod3.l, ymax = predicted_mod3.u), alpha = 0.1, linetype = 'dashed',size=0) +
  geom_point(aes(x = age, y = market_value, color = player_name)) +
  theme_bw() +
  facet_wrap(~ player_name) + 
  theme(legend.position = "none") +
  labs(y = "market_value")

models_info[4,] <- c("mod3", AIC(mod3), BIC(mod3),
                     mae(train$market_value, train$predicted_mod3))


# season as random effect

mod4 <- bam(log10(market_value) ~
              s(player_id,bs = 're') +
              s(league, bs = 're') +
              s(club_name, bs = 're') +
              s(season, bs = 're'),
            data = train, select = TRUE)
summary(mod4)

pp <- predict(mod4,type='response',se.fit = TRUE)
pp.fit <- 10^pp$fit
pp.lower <- 10^(pp$fit-1.96*pp$se.fit)
pp.upper <- 10^(pp$fit+1.96*pp$se.fit)
train$predicted_mod4 <- pp.fit
train$predicted_mod4.l <- pp.lower
train$predicted_mod4.u <- pp.upper

first_players<-train[c(1:297),]

ggplot(first_players, aes(x = age, y = predicted_mod4, color = player_name)) +
  geom_line() +
  geom_ribbon(aes(ymin = predicted_mod4.l, ymax = predicted_mod4.u), alpha = 0.1, linetype = 'dashed',size=0) +
  geom_point(aes(x = age, y = market_value, color = player_name)) +
  theme_bw() +
  facet_wrap(~ player_name) + 
  theme(legend.position = "none") +
  labs(y = "market_value")

models_info[5,] <- c("mod4", AIC(mod4), BIC(mod4),
                     mae(train$market_value, train$predicted_mod4))


# age as fixed effect

mod5 <- bam(log10(market_value) ~
              s(player_id,bs = 're') +
              s(league, bs = 're') +
              s(club_name, bs = 're') +
              s(season, bs = 're') +
              s(age),
            data = train, select = TRUE)
summary(mod5)
plot_model(mod5, type = 'pred',terms = c('age'),axis.title = 'log10(market_value)',title = 'Predicted values')

pp <- predict(mod5,type='response',se.fit = TRUE)
pp.fit <- 10^pp$fit
pp.lower <- 10^(pp$fit-1.96*pp$se.fit)
pp.upper <- 10^(pp$fit+1.96*pp$se.fit)
train$predicted_mod5 <- pp.fit
train$predicted_mod5.l <- pp.lower
train$predicted_mod5.u <- pp.upper

first_players<-train[c(1:297),]

ggplot(first_players, aes(x = age, y = predicted_mod5, color = player_name)) +
  geom_line() +
  geom_ribbon(aes(ymin = predicted_mod5.l, ymax = predicted_mod5.u), alpha = 0.1, linetype = 'dashed',size=0) +
  geom_point(aes(x = age, y = market_value, color = player_name)) +
  theme_bw() +
  facet_wrap(~ player_name) + 
  theme(legend.position = "none") +
  labs(y = "market_value")

models_info[6,] <- c("mod5", AIC(mod5), BIC(mod5),
                     mae(train$market_value, train$predicted_mod5))


# position2 as fixed effect

mod6 <- bam(log10(market_value) ~
              s(player_id,bs = 're') +
              s(league, bs = 're') +
              s(club_name, bs = 're') +
              s(season, bs = 're') +
              s(age) +
              position2,
            data = train, select = TRUE)
summary(mod6)

pp <- predict(mod6,type='response',se.fit = TRUE)
pp.fit <- 10^pp$fit
pp.lower <- 10^(pp$fit-1.96*pp$se.fit)
pp.upper <- 10^(pp$fit+1.96*pp$se.fit)
train$predicted_mod6 <- pp.fit
train$predicted_mod6.l <- pp.lower
train$predicted_mod6.u <- pp.upper

first_players<-train[c(1:297),]

ggplot(first_players, aes(x = age, y = predicted_mod6, color = player_name)) +
  geom_line() +
  geom_ribbon(aes(ymin = predicted_mod6.l, ymax = predicted_mod6.u), alpha = 0.1, linetype = 'dashed',size=0) +
  geom_point(aes(x = age, y = market_value, color = player_name)) +
  theme_bw() +
  facet_wrap(~ player_name) + 
  theme(legend.position = "none") +
  labs(y = "market_value")

models_info[7,] <- c("mod6", AIC(mod6), BIC(mod6),
                     mae(train$market_value, train$predicted_mod6))


# age by position2 fixed effect

mod7 <- bam(log10(market_value) ~
              s(player_id,bs = 're') +
              s(league, bs = 're') +
              s(club_name, bs = 're') +
              s(season, bs = 're') +
              s(age, by = position2),
            data = train, select = TRUE)
summary(mod7)
plot_model(mod7, type = 'pred',terms = c('age','position2'),axis.title = 'log10(market_value)',title = 'Predicted values')

pp <- predict(mod7,type='response',se.fit = TRUE)
pp.fit <- 10^pp$fit
pp.lower <- 10^(pp$fit-1.96*pp$se.fit)
pp.upper <- 10^(pp$fit+1.96*pp$se.fit)
train$predicted_mod7 <- pp.fit
train$predicted_mod7.l <- pp.lower
train$predicted_mod7.u <- pp.upper

first_players<-train[c(1:297),]

ggplot(first_players, aes(x = age, y = predicted_mod7, color = player_name)) +
  geom_line() +
  geom_ribbon(aes(ymin = predicted_mod7.l, ymax = predicted_mod7.u), alpha = 0.1, linetype = 'dashed',size=0) +
  geom_point(aes(x = age, y = market_value, color = player_name)) +
  theme_bw() +
  facet_wrap(~ player_name) + 
  theme(legend.position = "none") +
  labs(y = "market_value")

models_info[8,] <- c("mod7", AIC(mod7), BIC(mod7),
                     mae(train$market_value, train$predicted_mod7))


# age with random intercept ans slope inside player random effect

mod8 <- bam(log10(market_value) ~
              s(player_id, bs ='re') +
              s(age, player_id, bs = 're') +
              s(league, bs = 're') +
              s(club_name, bs = 're') +
              s(season, bs = 're') +
              s(age, by = position2),
            data = train, select = TRUE)
summary(mod8)

pp <- predict(mod8,type='response',se.fit = TRUE)
pp.fit <- 10^pp$fit
pp.lower <- 10^(pp$fit-1.96*pp$se.fit)
pp.upper <- 10^(pp$fit+1.96*pp$se.fit)
train$predicted_mod8 <- pp.fit
train$predicted_mod8.l <- pp.lower
train$predicted_mod8.u <- pp.upper

first_players<-train[c(1:297),]

ggplot(first_players, aes(x = age, y = predicted_mod8, color = player_name)) +
  geom_line() +
  geom_ribbon(aes(ymin = predicted_mod8.l, ymax = predicted_mod8.u), alpha = 0.1, linetype = 'dashed',size=0) +
  geom_point(aes(x = age, y = market_value, color = player_name)) +
  theme_bw() +
  facet_wrap(~ player_name) + 
  theme(legend.position = "none") +
  labs(y = "market_value")

models_info[9,] <- c("mod8", AIC(mod8), BIC(mod8),
                     mae(train$market_value, train$predicted_mod8))


# minutes_played as fixed effect

mod9 <- bam(log10(market_value) ~
              s(player_id, bs ='re') +
              s(age, player_id, bs = 're') +
              s(league, bs = 're') +
              s(club_name, bs = 're') +
              s(season, bs = 're') +
              s(age, by = position2) +
              minutes_played ,
            data = train, select = TRUE)
summary(mod9)

pp <- predict(mod9,type='response',se.fit = TRUE)
pp.fit <- 10^pp$fit
pp.lower <- 10^(pp$fit-1.96*pp$se.fit)
pp.upper <- 10^(pp$fit+1.96*pp$se.fit)
train$predicted_mod9 <- pp.fit
train$predicted_mod9.l <- pp.lower
train$predicted_mod9.u <- pp.upper

first_players<-train[c(1:297),]

ggplot(first_players, aes(x = age, y = predicted_mod9, color = player_name)) +
  geom_line() +
  geom_ribbon(aes(ymin = predicted_mod9.l, ymax = predicted_mod9.u), alpha = 0.1, linetype = 'dashed',size=0) +
  geom_point(aes(x = age, y = market_value, color = player_name)) +
  theme_bw() +
  facet_wrap(~ player_name) + 
  theme(legend.position = "none") +
  labs(y = "market_value")

models_info[10,] <- c("mod9", AIC(mod9), BIC(mod9),
                      mae(train$market_value, train$predicted_mod9))


# goals as fixed effect

mod10 <- bam(log10(market_value) ~
               s(player_id, bs ='re') +
               s(age, player_id, bs = 're') +
               s(league, bs = 're') +
               s(club_name, bs = 're') +
               s(season, bs = 're') +
               s(age, by = position2) +
               minutes_played +
               goals,
             data = train, select = TRUE)
summary(mod10)

pp <- predict(mod10,type='response',se.fit = TRUE)
pp.fit <- 10^pp$fit
pp.lower <- 10^(pp$fit-1.96*pp$se.fit)
pp.upper <- 10^(pp$fit+1.96*pp$se.fit)
train$predicted_mod10 <- pp.fit
train$predicted_mod10.l <- pp.lower
train$predicted_mod10.u <- pp.upper

first_players<-train[c(1:297),]

ggplot(first_players, aes(x = age, y = predicted_mod10, color = player_name)) +
  geom_line() +
  geom_ribbon(aes(ymin = predicted_mod10.l, ymax = predicted_mod10.u), alpha = 0.1, linetype = 'dashed',size=0) +
  geom_point(aes(x = age, y = market_value, color = player_name)) +
  theme_bw() +
  facet_wrap(~ player_name) + 
  theme(legend.position = "none") +
  labs(y = "market_value")

models_info[11,] <- c("mod10", AIC(mod10), BIC(mod10),
                      mae(train$market_value, train$predicted_mod10))


# assists as fixed effect

mod11 <- bam(log10(market_value) ~
               s(player_id, bs ='re') +
               s(age, player_id, bs = 're') +
               s(league, bs = 're') +
               s(club_name, bs = 're') +
               s(season, bs = 're') +
               s(age, by = position2) +
               minutes_played +
               goals +
               assists,
             data = train, select = TRUE)
summary(mod11)

pp <- predict(mod11,type='response',se.fit = TRUE)
pp.fit <- 10^pp$fit
pp.lower <- 10^(pp$fit-1.96*pp$se.fit)
pp.upper <- 10^(pp$fit+1.96*pp$se.fit)
train$predicted_mod11 <- pp.fit
train$predicted_mod11.l <- pp.lower
train$predicted_mod11.u <- pp.upper

first_players<-train[c(1:297),]

ggplot(first_players, aes(x = age, y = predicted_mod11, color = player_name)) +
  geom_line() +
  geom_ribbon(aes(ymin = predicted_mod11.l, ymax = predicted_mod11.u), alpha = 0.1, linetype = 'dashed',size=0) +
  geom_point(aes(x = age, y = market_value, color = player_name)) +
  theme_bw() +
  facet_wrap(~ player_name) + 
  theme(legend.position = "none") +
  labs(y = "market_value")

models_info[12,] <- c("mod11", AIC(mod11), BIC(mod11),
                      mae(train$market_value, train$predicted_mod11))

# ranking as fixed effect

mod12 <- bam(log10(market_value) ~
               s(player_id, bs ='re') +
               s(age, player_id, bs = 're') +
               s(league, bs = 're') +
               s(club_name, bs = 're') +
               s(season, bs = 're') +
               s(age, by = position2) +
               minutes_played +
               goals +
               assists +
               ranking,
             data = train, select = TRUE)
summary(mod12)

pp <- predict(mod12,type='response',se.fit = TRUE)
pp.fit <- 10^pp$fit
pp.lower <- 10^(pp$fit-1.96*pp$se.fit)
pp.upper <- 10^(pp$fit+1.96*pp$se.fit)
train$predicted_mod12 <- pp.fit
train$predicted_mod12.l <- pp.lower
train$predicted_mod12.u <- pp.upper

first_players<-train[c(1:297),]

ggplot(first_players, aes(x = age, y = predicted_mod12, color = player_name)) +
  geom_line() +
  geom_ribbon(aes(ymin = predicted_mod12.l, ymax = predicted_mod12.u), alpha = 0.1, linetype = 'dashed',size=0) +
  geom_point(aes(x = age, y = market_value, color = player_name)) +
  theme_bw() +
  facet_wrap(~ player_name) + 
  theme(legend.position = "none") +
  labs(y = "market_value")

models_info[13,] <- c("mod12", AIC(mod12), BIC(mod12),
                      mae(train$market_value, train$predicted_mod12))


# position2 as fixed effect

mod13 <- bam(log10(market_value) ~
               s(player_id, bs ='re') +
               s(age, player_id, bs = 're') +
               s(league, bs = 're') +
               s(club_name, bs = 're') +
               s(season, bs = 're') +
               s(age, by = position2) +
               minutes_played +
               goals +
               assists +
               ranking +
               position2,
             data = train, select = TRUE)
summary(mod13)

pp <- predict(mod13,type='response',se.fit = TRUE)
pp1 <- predict(mod13,type='response', interval = "prediction",se.fit = TRUE)
pp2 <- predict(mod13,type='response', interval = "confidence", se.fit = TRUE)
pp.fit <- 10^pp$fit
pp.lower <- 10^(pp$fit-1.96*pp$se.fit)
pp.upper <- 10^(pp$fit+1.96*pp$se.fit)
train$predicted_mod13 <- pp.fit
train$predicted_mod13.l <- pp.lower
train$predicted_mod13.u <- pp.upper
train$errors <- pp$fit - log10(train$market_value)
train$errors2 <- train$predicted_mod13 - train$market_value

first_players<-train[c(1:297),]

ggplot(first_players, aes(x = age, y = predicted_mod13, color = player_name)) +
  geom_line() +
  geom_ribbon(aes(ymin = predicted_mod13.l, ymax = predicted_mod13.u), alpha = 0.1, linetype = 'dashed',size=0) +
  geom_point(aes(x = age, y = market_value, color = player_name)) +
  theme_bw() +
  facet_wrap(~ player_name) + 
  theme(legend.position = "none") +
  labs(y = "market_value (€)")

models_info[14,] <- c("mod13", AIC(mod13), BIC(mod13),
                      mae(train$market_value, train$predicted_mod13))



# Family: gaussian
# Link function: identity
# 
# Formula:
#   log10(market_value) ~ s(player_id, bs = "re") + s(age, player_id, bs = "re") +
#                         s(league, bs = "re") + s(club_name, bs = "re") +
#                         s(season, bs = "re") + s(age, by = position2) +
#                         minutes_played + goals + assists + ranking + position2
# 
# Parametric coefficients:
#                         Estimate Std. Error t value Pr(>|t|)
#   (Intercept)          6.409e+00  5.495e-02 116.628  < 2e-16 ***
#   minutes_played       1.113e-04  3.208e-06  34.703  < 2e-16 ***
#   goals                8.512e-03  7.298e-04  11.664  < 2e-16 ***
#   assists              4.615e-03  9.545e-04   4.836 1.36e-06 ***
#   ranking             -9.805e-03  6.968e-04 -14.072  < 2e-16 ***
#   position2Midfielder  7.582e-02  2.026e-02   3.742 0.000184 ***
#   position2Forward     1.352e-01  2.316e-02   5.838 5.55e-09 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#                                  edf Ref.df       F  p-value
#   s(player_id)               344.133    646   45.02  < 2e-16 ***
#   s(age,player_id)           283.490    649   48.47  < 2e-16 ***
#   s(league)                    3.677      4 9429.13  < 2e-16 ***
#   s(club_name)               114.517    163  103.34 6.61e-06 ***
#   s(season)                   14.896     15 1747.38  < 2e-16 ***
#   s(age):position2Defender     7.303      9 8824.43  < 2e-16 ***
#   s(age):position2Midfielder   6.677      9 4692.20  < 2e-16 ***
#   s(age):position2Forward      7.033      9 4386.07  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# R-sq.(adj) =  0.883   Deviance explained = 89.6%
# fREML = -862.33  Scale est. = 0.032697  n = 6987


# fixed effect plots

b <- getViz(mod13)
plot(b,allTerms = TRUE)
plot_model(mod13, type = 'pred',terms = c('age'),axis.title = 'log10(market_value)',title = 'Predicted values')
plot_model(mod13, type = 'pred',terms = c('goals'),axis.title = 'log10(market_value)',title = 'Predicted values')
plot_model(mod13, type = 'pred',terms = c('position2'),axis.title = 'log10(market_value)',title = 'Predicted values')
#...
plot_model(mod13, type = 'pred',terms = c('age','position2'),axis.title = 'log10(market_value)',title = 'Predicted values')


# random effects plots

clubs <- as.data.frame(gammit::extract_ranef(mod13,re = 'club_name')[c('group','value','lower_2.5','upper_97.5')])
ggplot(clubs, aes(sample = value) ) +
  stat_qq(color = "darkblue") +
  stat_qq_line(color = "darkblue") +
  xlab("Normal theoretical quantiles") + ylab("log10(market_value) data quantiles")
ggplotly(ggplot(clubs, aes(x=reorder(group, -value), y=value, fill=group)) +
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 45)) +
  xlab("Clubs"))
clubs_top <- arrange(clubs, -value)[c(1:10),]
ggplot(clubs_top, aes(x=reorder(group, -value), y=value, fill=group)) +
  geom_bar(stat="identity") + 
  coord_flip() +
  xlab("Clubs") +
  theme(legend.position = "none")
  

leagues <- as.data.frame(gammit::extract_ranef(mod13,re = 'league')[c('group','value','lower_2.5','upper_97.5')])
ggplot(leagues, aes(sample = value) ) +
  stat_qq(color = "darkblue") +
  stat_qq_line(color = "darkblue") +
  xlab("Normal theoretical quantiles") + ylab("log10(market_value) data quantiles")
ggplotly(ggplot(leagues, aes(x=reorder(group, -value), y=value, fill=group)) +
           geom_bar(stat="identity") + 
           theme(axis.text.x = element_text(angle = 45)) +
           xlab("League"))


players_re <- as.data.frame(gammit::extract_ranef(mod13,re = 'player_id')[c('group','value','lower_2.5','upper_97.5')])
players_re$player_id <- players_re$group
players_re <- merge(x = players_re, y = train[,c("player_id", "player_name")], by = "player_id")
players_re <- unique(players_re)
ggplot(players_re, aes(sample = value) ) +
  stat_qq(color = "darkblue") +
  stat_qq_line(color = "darkblue") +
  xlab("Normal theoretical quantiles") + ylab("log10(market_value) data quantiles")
ggplotly(ggplot(players_re, aes(x=reorder(player_name, -value), y=value, fill=player_name)) +
           geom_bar(stat="identity") + 
           theme(axis.text.x = element_text(angle = 45)) +
           xlab("Players"))


seasons <- as.data.frame(gammit::extract_ranef(mod13,re = 'season')[c('group','value','lower_2.5','upper_97.5')])
ggplot(seasons, aes(sample = value) ) +
  stat_qq(color = "darkblue") +
  stat_qq_line(color = "darkblue")
ggplotly(ggplot(seasons, aes(x=reorder(group, -value), y=value, fill=group)) +
           geom_bar(stat="identity") + 
           theme(axis.text.x = element_text(angle = 45)) +
           xlab("League"))


# residual errors analysis

ggplot(train, aes(x = errors)) +
  geom_histogram(aes(y = ..density..), color = "darkblue", fill = "lightblue") +
  geom_density(colour="darkblue", fill="lightblue", alpha=0.2, adjust=3)
ggplot(train, aes(sample = errors) ) +
  stat_qq(color = "darkblue") +
  stat_qq_line(color = "darkblue")
ggplot(train, aes(log10(market_value), errors)) +
  geom_point()+
  geom_hline(yintercept=0.5, linetype="dashed", color = "red")+
  geom_hline(yintercept=-0.5, linetype="dashed", color = "red")+
  geom_hline(yintercept=0, color = "grey")+
  stat_smooth()
mae(train$market_value, train$predicted_mod13)


# plot any player as:

player <- train %>% filter(player_name == "fernando-torres")

ggplot(player, aes(x = age, y = predicted_mod13, color = player_name)) +
  geom_line() +
  geom_ribbon(aes(ymin = predicted_mod13.l, ymax = predicted_mod13.u), alpha = 0.1, linetype = 'dashed',size=0) +
  geom_point(aes(x = age, y = market_value, color = player_name)) +
  theme_bw() +
  facet_wrap(~ player_name) + 
  theme(legend.position = "none") +
  labs(y = "market_value (€)")


# all players:

jug <- levels(train$player_name)

plot_list = list()
for(i in 1:length(jug)){
  aux <- train[train$player_name == jug[i],]
  g <- ggplot(aux, aes(x = age, y = predicted_mod13)) +
    geom_line() +
    geom_ribbon(aes(ymin = predicted_mod13.l, ymax = predicted_mod13.u), alpha = 0.1, linetype = 'dashed',size=0) +
    geom_point(aes(x = age, y = market_value, color = club_name, shape = league),size = 2.5) +
    theme_bw() +
    facet_wrap(~ player_name) +
    theme(legend.position = "right") + labs(y = "market_value")
  plot_list[[i]] <- g
}


pdf("analysis/results/pred-plots-indiv-train.pdf",8,6)
for (i in 1:length(jug)) {
  print(plot_list[[i]])
}
dev.off()


# PREDICTIONS OF 2022

pp <- predict(mod13, newdata=test, type='response',se.fit = TRUE)
pp.fit <- 10^pp$fit
pp.lower <- 10^(pp$fit-1.96*pp$se.fit)
pp.upper <- 10^(pp$fit+1.96*pp$se.fit)
test$predicted_mod13 <- pp.fit
test$predicted_mod13.l <- pp.lower
test$predicted_mod13.u <- pp.upper
test$errors <- pp$fit - log10(test$market_value)
test$errors2 <- test$predicted_mod13 - test$market_value
mae(test$market_value, 10^pp$fit)

ggplot(test, aes(x = errors)) +
  geom_histogram(aes(y = ..density..), color = "darkblue", fill = "lightblue") +
  geom_density(colour="darkblue", fill="lightblue", alpha=0.2, adjust=3)
ggplot(test, aes(sample = errors) ) +
  stat_qq(color = "darkblue") +
  stat_qq_line(color = "darkblue")
ggplot(test, aes(log10(market_value), errors)) +
  geom_point()+
  geom_hline(yintercept=0.5, linetype="dashed", color = "red")+
  geom_hline(yintercept=-0.5, linetype="dashed", color = "red")+
  geom_hline(yintercept=0, color = "grey")+
  stat_smooth()
mae(test$market_value, test$predicted_mod13)


#Train and Test All
# Train & Test

train <- players0 %>% filter(year < 2021)

pp <- predict(mod13,type='response',se.fit = TRUE)
pp.fit <- 10^pp$fit
pp.lower <- 10^(pp$fit-1.96*pp$se.fit)
pp.upper <- 10^(pp$fit+1.96*pp$se.fit)
train$predicted_mod13 <- pp.fit
train$predicted_mod13.l <- pp.lower
train$predicted_mod13.u <- pp.upper
train$errors <- pp$fit - log10(train$market_value)
train$errors2 <- train$predicted_mod13 - train$market_value

train_and_test <- rbind(train,test)
saveRDS(train, "analysis/data/train.RDs", compress = TRUE)
saveRDS(test, "analysis/data/test.RDs", compress = TRUE)
saveRDS(train_and_test, "analysis/data/train_and_test.RDs", compress = TRUE)


# all players:

jug <- levels(train_and_test$player_name)

plot_list = list()
for(i in 1:length(jug)){
  aux <- train_and_test[train_and_test$player_name == jug[i],]
  g <- ggplot(aux, aes(x = age, y = predicted_mod13)) +
    geom_line() +
    geom_ribbon(aes(ymin = predicted_mod13.l, ymax = predicted_mod13.u), alpha = 0.1, linetype = 'dashed',size=0) +
    geom_point(aes(x = age, y = market_value, color = club_name, shape = league),size = 2.5) +
    theme_bw() +
    facet_wrap(~ player_name) +
    theme(legend.position = "right") + labs(y = "market_value")
  plot_list[[i]] <- g
}


pdf("analysis/results/pred-plots-indiv-train-and-test.pdf",8,6)
for (i in 1:length(jug)) {
  print(plot_list[[i]])
}
dev.off()
