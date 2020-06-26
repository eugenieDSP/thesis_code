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

incumb_p <- ggpredict(model2, terms = "incumb")
plot_incumb <- plot(incumb_p, connect.lines = T, use.theme = T)
class(plot_incumb)
plot_incumb +
  labs(x = "Incumbency status") +
  scale_y_continuous(limits = c(0.2,0.6))


niche_p <- ggpredict(model2, terms = "niche")
plot(niche_p, connect.lines = T, )

pf_p <- ggpredict(model2, terms = "party.fam")
plot(pf_p, connect.lines = T)

size_p <- ggpredict(model2, terms = "stdsize")
plot(size_p, connect.lines = T) +
    scale_x_continuous(limits = c(-2, 2))
    scale_y_continuous(limits = c(0.2, 0.5)) +
    theme(text = element_text(family = "Bookman"))

lrpos_p <- ggpredict(model2, terms = "stdlrpos")
plot(lrpos_p, connect.line = T)

year_p <- ggpredict(model2, terms = "year.trend")
plot(year_p, connect.lines = T)

stargazer(model2, type = "html")
