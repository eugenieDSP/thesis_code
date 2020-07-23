###Dataset 1###
##Importing##
normalizePath(path, winslash = "/", mustWork = NA)

path <- "C:/Users/Evgeniya Shtyrkova/Documents/MEGA/PhD Thesis/Data/dataset1_v2.dta"
dataset1 <- read.dta13(path, nonint.factors = T, generate.factors = T, convert.factors = T, convert.underscore = T)

glimpse(dataset1)
dataset1 %>%
  group_by(country, party, year.trend) %>%
  summarise(mean(polarity))



ds1 <- dataset1 %>%
  dplyr::select(count, year, country, party, doctype, year.trend, issue.topic, issue.dum, Iss.subtopic, party.size, niche, incumb,
                left.right, party.fam, perso, polarity, fragm, lrecon, galtan, lrpos, stdfragm, stdperso, stdlrpos, stdsize,
                stdlrecon, stdgaltan, level2, sact.ind, sact.party)
glimpse(ds1)


ds1 <- ds1 %>% mutate(issue.dum = as.factor(issue.dum), sact.ind = as.factor(sact.ind),
                                    sact.party = as.factor(sact.party), issue.topic = as.factor(issue.topic))
glimpse(ds1)
levels(ds1$party.fam)

mean(ds1$polarity)
sd(ds1$polarity)
ds1 <- ds1 %>%
  mutate(stdpol = scale(polarity, center = T, scale = T))
glimpse(ds1$stdpol)

##Count plots###

p1 <- ggplot(subset(ds1, country == "Germany"), aes(count)) + 
  geom_histogram(binwidth = 1) + 
  facet_grid(party ~ year.trend, margins = T, scales = "free") +
  theme_light()


p2 <- ggplot(subset(dataset1_selected, country == "Switzerland"), aes(count)) + 
  geom_histogram(binwidth = 1) + 
  facet_grid(party ~ year.trend, margins = T, scales = "free")

##Descriptive statistics##
# Mean and SD personalization
mean(ds1$perso)
sd(ds1$perso)
# mean and SD fragmentation
mean(ds1$fragm)
sd(ds1$fragm)

#Personalization-count
dataset1_selected %>%
  filter(country == "Switzerland" & count != 0) %>%
  ggplot(aes(x = perso, y = count, color = party1)) + 
  geom_jitter() +
  scale_color_manual(values = cbp1)

dataset1_selected %>%
  filter(country == "Germany"& count != 0) %>%
  ggplot(aes(x = perso, y = count)) +
  geom_jitter() +
  scale_color_manual(values = cbp1)


##Change in personalization
perso.df <- ds1 %>%
  filter(count != 0) %>%
  group_by(country, party, year.trend) %>%
  summarise(mean.perso = mean(perso)) %>%
  mutate(year.tren = as.numeric(year.trend))

glimpse(perso.df)
plot.perso <- ggplot(perso.df,aes(x = year.trend, y = mean.perso, color = party)) +
  geom_bar() +
  scale_x_continuous(name = "Election year", minor_breaks = NULL, breaks = c(1,2), labels = c("1st", "2nd")) +
  scale_y_continuous(name = "Text personalization") +
  labs(title = "Change in text personalization across parties") +
  facet_wrap(country~.) +
  theme(plot.margin = unit(c(2,2,2,2), "cm")) +
  scale_color_manual(values = cbp1, name = "Party")
  
 plot.perso + theme_light()
  

#Party size-count
ds1_z %>%
  filter(country == "Switzerland" & count != 0) %>%
  ggplot(aes(x = party.size, y = count, color = party)) +
  geom_jitter() +
  scale_color_manual(values = cbp1)

ds1_z %>%
  filter(country == "Germany") %>%
  ggplot(aes(x = party.size, y = count)) +
  geom_jitter() +
  scale_color_manual(values = cbp1)

#Party size - incumbent
dataset1_selected %>%
  filter(country == "Switzerland" & count != 0) %>%
  ggplot(aes(x = incumb, y = party.size)) +
  geom_jitter() +
  scale_color_manual(values = cbp1)

ds1 %>%
  filter(country == "Germany"& count != 0) %>%
  ggplot(aes(x = incumb, y = party.size)) +
  geom_jitter() +
  scale_color_manual(values = cbp1)


###Fitting negbin model###

#Formula 2 - NegBin model
formula2 <- count ~ niche + incumb + country + year.trend + party.fam + stdfragm + stdperso + stdsize + stdlrpos
model2 <- glm.nb(formula2, data = ds1)
tidied_model <- tidy(model2)
tidied_model
str(tidied_model)
summary(model2)

anova(model2)
res <- residuals.glm(model2)

##probabilites
tidied_model2 <- tidied_model %>%
  mutate(prob = sapply(estimate, logit2prob))


#Visualising the model
screenreg(model2)
wordreg(model2, file = "ds1_model_output.doc", 
        stars = c(0.001, 0.01, 0.05, 0.1))
plot.reg <- plotreg(model2, custom.title = "Negative binomial regression, image count",
                custom.model.names = "Model",
                omit.coef = "(Intercept)",
                custom.coef.names = c("Intercept", "Niche", "Incumbent", "Country: Germany",
                              "Electoral trend: 2nd elections", "Social Democrats", "Green", "Liberal",
                              "Christian Democrats","Radical right wing",
                              "System fragmentation", "Text personalization", "Party size", "Left-right position"))
plot.reg <- plot.reg + labs(subtitle = "Untransformed log-odds")

# Formula 3 - interactions
formula3 <- count ~ niche + incumb + country + year.trend + party.fam + stdfragm + stdperso + stdsize + stdlrpos + stdsize*year.trend
model3 <- glm.nb(formula3, data = ds1)
tidied_model3 <- tidy(model3)
str(tidied_model3)
summary(model3)
wordreg(model3, file = "ds1_model_output_2.doc", 
        stars = c(0.001, 0.01, 0.05, 0.1))

tidied_model3 <- tidied_model3 %>%
  mutate(prob = sapply(estimate, logit2prob))

anova(model2, model3)

model2_exp <- round(cbind(beta = coef(model2),
            expbeta = exp(coef(model2)),
            pct = 100 * (exp(coef(model2)) - 1)), 3)

model3_coef <- round(cbind(beta = coef(model3),
            expbeta = exp(coef(model3)),
            pct = 100 * (exp(coef(model3)) - 1)), 3)

# Visualizing
plotreg(list(model2, model3))
plotreg(model3)
plot.reg2$data$lab <- NA
plot.reg2$data$lab <- replace_na(plot.reg2$data$lab, "Model 2")
plot.reg2 <- plotreg(model3, custom.title = "Negative binomial regression with interaction terms, image count",
        model.names = c(NULL),
        omit.coef = "(Intercept)",
        custom.coef.names = c("Intercept", "Niche", "Incumbent", "Country: Germany",
                                                "Electoral trend: 2nd elections", "Social Democrats", "Green", "Liberal",
                                                "Christian Democrats","Radical right wing",
                                                "System fragmentation", "Text personalization", "Party size", "Left-right position",
                                                "Election*size"))
plot.reg2 <- plot.reg2 + labs(subtitle = "Untransformed log-odds")


str(plot.reg2)
screenreg(list(model2, model3))
##Marginplots

incumb_p1 <- ggpredict(model3, terms = "incumb")
plot_incumb1 <- plot(incumb_p1, connect.lines = T, use.theme = T)
class(plot_incumb1)
plot_incumb1 <- plot_incumb1 +
  labs(x = "Status",
       y = "Count",
       subtitle = "Incumbency status") +
  scale_y_continuous(limits = c(0,0.6)) +
  theme_bw()


niche_p1<- ggpredict(model3, terms = "niche")
niche_plot1 <- plot(niche_p1, connect.lines = T) +
  scale_y_continuous(limits = c(0, 0.6)) +
  labs(x = "Status",
       y = "Count",
       title = "Predicted values of count",
       subtitle = "Niche / mainstream status") +
  theme_bw()


pf_p1 <- ggeffect(model3, terms = "party.fam")
pf_plot <- 
  ggplot(pf_p1, aes(x = fct_reorder(x, predicted, .fun = "desc"), y = predicted)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
  scale_y_continuous(limits = c(0, 0.5)) +
  labs(x = NULL,
       y = "Count",
       title = "Predicted values of count",
       subtitle = "Party family") +
  theme_bw()
 

size_p1 <- ggpredict(model3, terms = "stdsize")
size_plot1 <-
  ggplot(size_p1, aes(x=x, y = predicted)) +
  geom_ribbon(aes(ymin = predicted - std.error, ymax = predicted + std.error), data = size_p1, fill = "grey50", alpha = 0.5) +
  geom_line() +
  coord_cartesian(xlim = c(-2, 2), ylim = c(0, 0.5)) +
    labs(x = "Party size (standardised values)",
       y = "Count",
       title = "Predicted values of count",
       subtitle = "Party size") +
    theme_bw()

lrpos_p1 <- ggpredict(model3, terms = "stdlrpos [-2,2]")
str(lrpos_p1)
lrpos_plot1 <- ggplot(lrpos_p1, aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), data = lrpos_p1, fill = "grey50", alpha = 0.5) +
  geom_line() +
  labs(x = "Left-right position (standardised)",
       y = "Count",
       title = "Predicted values of count",
       subtitle = "Party ideological position") +
  theme_bw() +
  coord_cartesian(xlim = c(-2, 2), ylim = c(0, 0.8),
                  clip = "on") 
  

year_p1 <- ggpredict(model3, terms = "year.trend")
year_plot1 <- plot(year_p1, connect.lines = T) +
  scale_y_continuous(limits = c(0, 0.5)) +
  labs(x = "Elections",
       y = "Count",
       title = "Predicted values of count",
       subtitle = "Electoral trend") +
  theme_bw()

country_p1 <- ggpredict(model3, terms = "country")
country_plot1 <- plot(country_p1, connect.lines = T) +
  coord_cartesian(ylim = c(0, 0.6)) +
  labs(x = NULL,
       y = "Count",
       title = "Predicted values of count",
       subtitle = "Country") +
  theme_bw()


int_p1 <- ggpredict(model3, terms = c("stdsize [-2:2]", "year.trend"))
plot_int <- plot(int_p1, connect.lines =T)
plot_int <- plot_int + theme_bw() +
  coord_cartesian(ylim = c(0, 0.8)) +
  labs(x = "Party size",
       y = "Count",
       title = "Predicted values of count",
       subtitle = "Electoral trend and party size") +
  scale_color_npg(name = "Electoral year")

# Some other ideas on models

for4 <- count ~ niche + incumb + country + year.trend + stdfragm + stdperso + stdsize + stdlrpos + stdsize*stdlrpos
model4 <- glm.nb(for4, data = ds1)
tidied_model4 <- tidy(model4)
(tidied_model4)
summary(model4)
plotreg(model4)



## Sentence level
#dummify issues
ds1_d <- ds1 %>%
  dplyr::select(count, country, party, doctype, issue.topic, year.trend)

ds1_d <- dummy_cols(ds1_d, select_columns = "issue.topic")
ds1_d <- as_tibble(ds1_d)

# renaming
names(ds1_d)[7] <- "no.issue"
names(ds1_d)[8] <- "macroeconomics"
names(ds1_d)[9] <- "rights.and.liberties"
names(ds1_d)[10] <- "health"
names(ds1_d)[11] <- "agriculture"
names(ds1_d)[12] <- "labour"
names(ds1_d)[13] <- "education"
names(ds1_d)[14] <- "environment"
names(ds1_d)[15] <- "energy"
names(ds1_d)[16] <- "migration"
names(ds1_d)[17] <- "transportation"
names(ds1_d)[18] <- "law.and.crime"
names(ds1_d)[19] <- "social.welfare"
names(ds1_d)[20] <- "housing"
names(ds1_d)[21] <- "banking.and.commerce"
names(ds1_d)[22] <- "defense"
names(ds1_d)[23] <- "science"
names(ds1_d)[24] <- "foreign.trade"
names(ds1_d)[25] <- "int.affairs"
names(ds1_d)[26] <- "government"
names(ds1_d)[27] <- "public lands"
names(ds1_d)[28] <- "culture"

str(ds1_d)
ds1_d <- ds1_d %>%
  mutate_if(is.integer, as.factor)

formula1 <- count ~ doctype + no.issue + macroeconomics + rights.and.liberties +
  labour + environment + migration + social.welfare + banking.and.commerce
  
model5 <- glm.nb(formula1, data = ds1_d)
tidied_model5 <- tidy(model5)
summary(model5)
plotreg(model5)

model5_exp <- round(cbind(beta = coef(model5),
                          expbeta = exp(coef(model5)),
                          pct = 100 * (exp(coef(model5)) - 1)), 3)

tidied_model5 <- tidied_model5 %>%
  mutate(prob = sapply(estimate, logit2prob))
no.issue.p <- ggpredict(model5, terms = "no.issue")
plot(no.issue.p)

size.df <- ds1 %>% dplyr::select(party, party.size, stdsize, year.trend) %>%
  group_by(party, year.trend) %>%
  summarise(mean_size = mean(party.size, na.rm = T), mean_sd = mean(stdsize))
ggplot(size.df, aes(x = mean_size, y = mean_sd, color = party)) +
  geom_text_repel(aes(label = party)) +
  facet_grid(.~year.trend)
  
