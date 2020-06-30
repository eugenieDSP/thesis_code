###Dataset 1###
##Importing##
normalizePath(path, winslash = "/", mustWork = NA)

path <- "C:/Users/Evgeniya Shtyrkova/Documents/MEGA/PhD Thesis/Data/dataset1_v2.dta"
dataset1 <- read.dta13(path, nonint.factors = T, generate.factors = T, convert.factors = T, convert.underscore = T)

glimpse(dataset1)

ds1 <- dataset1 %>%
  dplyr::select(count, year, country, party, doctype, year.trend, issue.topic, issue.dum, Iss.subtopic, party.size, niche, incumb,
                left.right, party.fam, perso, polarity, fragm, lrecon, galtan, lrpos, stdfragm, stdperso, stdlrpos, stdsize,
                stdlrecon, stdgaltan, level2, sact.ind, sact.party)
glimpse(ds1)


ds1 <- ds1 %>% mutate(issue.dum = as.factor(issue.dum), sact.ind = as.factor(sact.ind),
                                    sact.party = as.factor(sact.party), issue.topic = as.factor(issue.topic))
glimpse(ds1)
levels(ds1$party.fam)

##Count plots###

p1 <- ggplot(subset(ds1, country == "Germany"), aes(count)) + 
  geom_histogram(binwidth = 1) + 
  facet_grid(party1 ~ year.trend, margins = T, scales = "free") +
  theme_light()


p2 <- ggplot(subset(dataset1_selected, country == "Switzerland"), aes(count)) + 
  geom_histogram(binwidth = 1) + 
  facet_grid(party1 ~ year.trend, margins = T, scales = "free")

##Descriptive statistics##
# Number of image occurrences in core sentences, by country and document type

#Image count by country

#Image count by election year and country

#Image count by party and election year (relatively for each party)

##Independent variables

##Bivariate statistics: look for non-linear relationships
CrossTable(ds1_z$image.freq, ds1_z$country, digits = 2, prop.chisq=F, format = "SPSS")


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
perso.df <- dataset1_selected %>%
  filter(count != 0) %>%
  group_by(country, party1, year.trend) %>%
  summarise(mean.perso = mean(perso)) %>%
  mutate(year.tren = as.numeric(year.trend))

glimpse(perso.df)
plot.perso <- ggplot(perso.df,aes(x = year.trend, y = mean.perso, color = party1)) +
  geom_point() + geom_line(size = 1) +
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

dataset1_selected %>%
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
plotreg(model2, custom.title = "Negative binomial regression, image count",
                custom.model.names = "Model",
                custom.note = "Untransformed log-odds", omit.coef = "(Intercept)",
                custom.coef.names = c("Intercept", "Niche", "Incumbent", "Country: Germany",
                              "2nd elections", "Social Democrats", "Green", "Liberal",
                              "Christian Democrats","Radical right wing",
                              "System fragmentation", "Text personalization", "Party size", "Left-right position"))


##Marginplots

incumb_p1 <- ggpredict(model2, terms = "incumb")
plot_incumb1 <- plot(incumb_p1, connect.lines = T, use.theme = T)
class(plot_incumb1)
plot_incumb1 <- plot_incumb1 +
  labs(x = "Incumbency status",
       y = "Count",
       subtitle = "Incumbency status") +
  scale_y_continuous(limits = c(0,0.6))


niche_p1<- ggpredict(model2, terms = "niche")
niche_plot1 <- plot(niche_p1, connect.lines = T) +
  scale_y_continuous(limits = c(0, 0.5)) +
  labs(x = "Status",
       y = "Count",
       title = "Predicted values of count",
       subtitle = "Niche / mainstream status") +
  theme_pubclean()

str(pf_p1)
pf_p1 <- ggeffect(model2, terms = "party.fam")
pf_plot <- 
  ggplot(pf_p1, aes(x = fct_reorder(x, predicted, .fun = "desc"), y = predicted)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
  scale_y_continuous(limits = c(0, 0.5)) +
  labs(x = "Party family",
       y = "Count",
       title = "Predicted values of count",
       subtitle = "Party family") +
  theme_pubclean()
 
size_p1 <- ggpredict(model2, terms = "stdsize")
size_plot1 <-
  plot(size_p1, connect.lines = T) +
    scale_x_continuous(limits = c(-2, 2)) +
    scale_y_continuous(limits = c(0, 0.5)) +
    labs(x = "Party size",
       y = "Count",
       title = "Predicted values of count",
       subtitle = "Party size") +
    theme_pubclean()

lrpos_p1 <- ggpredict(model2, terms = "stdlrpos")
lrpos_plot1 <- plot(lrpos_p1, connect.line = T) +
  scale_y_continuous(limits = c(0, 0.5)) +
  scale_x_continuous(limits = c(-1.5, 2)) +
  labs(x = "Left-right position",
       y = "Count",
       title = "Predicted values of count",
       subtitle = "Party ideological position") +
  theme_pubclean()
  

year_p1 <- ggpredict(model2, terms = "year.trend")
year_plot1 <- plot(year_p1, connect.lines = T) +
  scale_y_continuous(limits = c(0, 0.5)) +
  labs(x = "Elections",
       y = "Count",
       title = "Predicted values of count",
       subtitle = "Electoral trend") +
  theme_pubclean()


stargazer(model2, type = "html")
