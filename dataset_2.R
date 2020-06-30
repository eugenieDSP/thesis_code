#Import the file
normalizePath(path, winslash = "/", mustWork = NA)
path <- "C:/Users/Evgeniya Shtyrkova/Documents/MEGA/PhD Thesis/Data/dataset2_v4.dta"
dataset2 <- read.dta13(path, nonint.factors = T, generate.factors = T, convert.factors = T, convert.underscore = T)
glimpse(dataset2)
dataset2 <- as_tibble(dataset2)
save(dataset2, file = "dataset2.RData") #that's where I had the error! Save it as csv.


##save variables as factor
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
glimpse(dataset2)

#Incumbency level
levels(dataset2$incumb)
dataset2$incumb <- factor(dataset2$incumb,
                          levels = c(0, 1),
                          labels = c("Not incumbent", "Incumbent"))

### plots ###
table <- dataset2 %>%
  group_by(country, dims, party.fam) %>%
  summarise(n = n())

ggplot(table, aes(y=n, x=party.fam, fill = dims)) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_rickandmorty() +
  facet_wrap(.~country, scales = "free") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1))

dfds2 <- as.data.frame(dataset2)

##Regression analysis##
model_ds2 <- multinom(dims ~ niche + incumb + stdlrpos + stdsize + stdsize*stdlrpos + stdperso + stdfragm + year.trend + country,
                      data = dataset2)
tidied_multinom <- tidy(model_ds2)
tidied_multinom
aug_mn <- augment(model_ds2)
summary(model_ds2) # better model
str(model_ds2)

screenreg(model_ds2)
plotreg(model_ds2)
class(plot_reg2)
str(plot_reg2)# ggplot class so it can be altered as a ggplot

extract(model_ds2)

## Cleaner plots ##

plotreg_model <- plotreg(model_ds2, custom.title = "Multinomial logistical regression, image dimensions",
                         custom.coef.names = c("Professional qualities: Intercept",
                                              "2: Niche status", "2: Incumbency",
                                              "2: Left-right position", "2:Party size", "2: Text personalization",
                                              "2: System fragmentation","2: Electoral trend",
                                              "2: Country (Germany)", "2: Position*size",
                                              "Core political values: Intercept", "3: Niche status", "3: Incumbency",
                                               "3: Left-right position", "3:Party size", "3: Text personalization",
                                               "3: System fragmentation", "3: Electoral trend",
                                              "3: Country (Germany)", "3: Position*size",
                                               "Group representation: Intercept", "4: Niche status", "4: Incumbency",
                                               "4: Left-right position", "4: Party size", "4: Text personalization",
                                               "4: System fragmentation",
                                               "4: Electoral trend", "4: Country (Germany)", "4: Position*size"),
        use.se = T)


str(model_plotreg)
niche_p2 <- ggpredict(model_ds2, terms = "niche", ci.lvl = 0.95)
plot_niche <- plot(niche_p2) +
  geom_line(alpha = 0.5, size = 1) +
  labs(x = NULL, y = NULL,
    title = "Predicted probabilites of image dimensions",
    subtitle = "Niche/ mainstream status") +
  theme_pubclean()

plot(ggeffect(model_ds2, terms = "incumb"))
plot(ggemmeans(model_ds2, terms = "incumb"))

incumb_p2 <- ggpredict(model_ds2, terms = "incumb")
plot_incumb <- plot(incumb_p2) +
                  geom_line(alpha = 0.5, size = 1.2) +
                  labs(x = NULL, y = NULL,
                  title = "Predicted probabilites of image dimensions",
                  subtitle = "Incumbent status") +
                  theme_pubclean()

lrpos_p2 <- ggpredict(model_ds2, terms = "stdlrpos [all]")
plot_lrpos <- plot(lrpos_p2) +
  geom_line(alpha = 0.5, size = 1.2) +
  labs(x = NULL, y = NULL,
       title = "Predicted probabilites of image dimensions",
       subtitle = "Ideological position") +
  theme_pubclean()

year_p2 <- ggpredict(model_ds2, terms = "year.trend")
yeat_plot <- plot(year_p2) +
  geom_line(alpha = 0.5, size = 1.2) +
  labs(x = NULL, y = NULL,
       title = "Predicted probabilites of image dimensions",
       subtitle = "Electoral trend") +
  theme_pubclean()


cou_p2 <- ggpredict(model_ds2, terms = "country")
count_plot <- plot(cou_p2, connect.lines = T) +
  geom_line(alpha = 0.5, size = 1.2) +
  labs(x = NULL, y = NULL,
       title = "Predicted probabilities of image dimensions",
       subtitle = "Country effect") +
      theme_pubclean()

size_p2 <- ggpredict(model_ds2, terms = "stdsize [all]")
size_plot <- plot(size_p2) +
  geom_line(alpha = 0.5, size = 1.2) +
  labs(x = NULL, y = NULL,
       title = "Predicted probabilites of image dimensions",
       subtitle = "Party size") +
  theme_pubclean()


fragm_p2 <- ggpredict(model_ds2, terms = "stdfragm [all]")
fragm_plot <- plot(fragm_p2) +
  geom_line(alpha = 0.5, size = 1.2) +
  labs(x = NULL, y = NULL,
       title = "Predicted probabilites of image dimensions",
       subtitle = "System fragmentation") +
  theme_pubclean()

perso_p <- ggpredict(model_ds2, terms = "stdperso [all]")
perso_plot <- plot(perso_p) +
  geom_line(alpha = 0.5, size = 1.2) +
  labs(x = NULL, y = NULL,
       title = "Predicted probabilites of image dimensions",
       subtitle = "Party text personalization") +
  theme_pubclean()

sizepos_p <- ggpredict(model_ds2, terms = "stdsize * stdlrpos [all]")
inter_plot <- plot(sizepos_p) +
  geom_line(alpha = 0.5, size = 1.2) +
              labs(x = NULL, y = NULL,
                   title = "Predicted probabilites of image dimensions",
                   subtitle = "Party size / ideological position interaction") +
              theme_pubclean()
