
normalizePath(path, winslash = "/", mustWork = NA)

path <- "C:/Users/Evgeniya Shtyrkova/Documents/MEGA/PhD Thesis/Data/dataset2_v4.dta"

dataset2 <- read.dta13(path, nonint.factors = T, generate.factors = T, convert.factors = T, convert.underscore = T)


glimpse(dataset2)

dataset2 <- as_tibble(dataset2)
save(dataset2, file = "dataset2.R")


##save as factor
dataset2 <- dataset2 %>%
    mutate(year = as.factor(year), party.fam = as.factor(party.fam), country = as.factor(country), niche = as.factor(niche),
           dims = as.factor(dims), Iss.topic = as.factor(Iss.topic))

cols <- c("count", "image.freq", "Iss.subtopic", "issue.dum", "econ.crisis", "ecol.issue", "mig.crisis", "incumb", "dims")
cols2 <- c("image.dum", "per.char", "integrity", "competence", "pragmatism", "strat.vis", "prof.qual", "lr.image", "society.cat", "econ.cat", "ecol.cat", "pol.val", "cul.val", "uni.val", "core.val", "core.pol.val", "gr.sc", "gr.se", "gr.pl", "gr.op", "groups", "dims", "fre.lib")

dataset2 %<>% mutate_at(cols, factor)
dataset2 %<>% mutate_at(cols2, factor)
str(dataset2)

dataset2 <- dataset2 %>%
  mutate(lrpos = as.numeric(lrpos), lrpos.10 = as.numeric(lrpos.10))

save(dataset2, file = "dataset2.R")


### plots ###
table <- dataset2 %>%
  group_by(country, dims, party.fam) %>%
  summarise(n = n())

ggplot(table, aes(y=n, x=party.fam, fill = dims)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_rickandmorty() +
  facet_wrap(.~country, scales = "free") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1))


##Adding weights###


##Regression analysis##
model_ds2 <- multinom(dims ~ niche + incumb + stdlrpos + stdsize +stdlrpos*stdsize + stdperso + stdfragm +year.trend + country,
                      data = dataset2,
                      margins = T)
tidied_multinom <- tidy(model_ds2)
tidied_multinom
summary(model_ds2)

model_ds3 <- multinom(dims ~ niche + incumb + stdlrpos + stdsize + stdperso + stdfragm +year.trend + country,
                      data = dataset2,
                      margins = T)
tidied_multinom <- tidy(model_ds3)
tidied_multinom
summary(model_ds3)
plotreg(model_ds3)

model.pf <- multinom(dims ~ party.fam,
                     data = dataset2,
                     margins = T)

screenreg(model_ds2)
plotreg(model_ds2)
plotreg(model.pf)

niche_p2 <- ggpredict(model_ds2, terms = "niche")
plot(niche_p2, connect.lines = T, )

incumb_p2 <- ggpredict(model_ds2, terms = "incumb")
plot_incumb <- plot(incumb_p2, connect.lines = T, use.theme = T)

lrpos_p2 <- ggpredict(model_ds2, terms = "stdlrpos [all]")
plot(lrpos_p2, connect.line = T)

year_p2 <- ggpredict(model_ds2, terms = "year.trend")
plot(year_p2, connect.lines = T)

cou_p2 <- ggpredict(model_ds2, terms = "country")
plot(cou_p2, connect.lines = T) + labs(title = "Predicted probabilities of country effect")

size_p2 <- ggpredict(model_ds2, terms = "stdsize [all]")
plot(size_p2, connect.line = T)

fragm_p2 <- ggpredict(model_ds2, terms = "stdfragm [all]")
plot(fragm_p2, connect.line = T)

pos <- ggpredict(model_ds2, terms = "stdlrpos [all]")
plot(pos)

perso_p <- ggpredict(model_ds2, terms = "stdperso [all]")
plot(perso_p)
