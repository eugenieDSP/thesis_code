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
dataset2 %>%
  group_by(party) %>%
  summarise(n = n())

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
model_ds2 <- multinom(dims ~ niche + incumb + stdsize + stdperso + stdfragm +
                        year.trend + country + stdlrpos*stdsize + year.trend*stdsize,
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
plotreg_a <- plotreg(model_ds2)
str(plotreg_a)
print(plotreg_a$data$co.names)
plotreg_ds2 <- plotreg(model_ds2, custom.title = "Multinomial logistical regression, image dimensions",
                        custom.note = "",
                        custom.coef.map = list("Professional qualities: nicheNiche" = "2.Niche status",
                                                "Professional qualities: incumbIncumbent" = "2.Incumbency",
                                                "Professional qualities: stdsize" = "2.Party size",
                                                "Professional qualities: stdperso" = "2.Text personalization",
                                                "Professional qualities: stdfragm" = "2.System fragmentation",
                                                "Professional qualities: year.trend2nd elections" = "2.Second elections",
                                                "Professional qualities: countryGermany" = "2.Country (Germany)",
                                                "Professional qualities: stdlrpos" = "2.Left-right position",
                                                "Professional qualities: stdsize:stdlrpos" = "2.Position*size",
                                                "Professional qualities: stdsize:year.trend2nd elections" = "2.Year*size",
                                                "Core political values: nicheNiche" = "3.Niche status",
                                                "Core political values: incumbIncumbent" = "3.Incumbency",
                                                "Core political values: stdsize" = "3.Party size",
                                                "Core political values: stdperso" = "3.Text personalization",
                                                "Core political values: stdfragm" = "3.System fragmentation",
                                                "Core political values: year.trend2nd elections" = "3.Second elections",
                                                "Core political values: countryGermany" = "3.Country (Germany)",
                                                "Core political values: stdlrpos" = "3.Left-right position",
                                                "Core political values: stdsize:stdlrpos" = "3.Position*size",
                                                "Core political values: stdsize:year.trend2nd elections" = "3.Year*size",
                                                "Group representation: nicheNiche" = "4.Niche status",
                                                "Group representation: incumbIncumbent" = "4.Incumbency",
                                                "Group representation: stdsize" = "4.Party size",
                                                "Group representation: stdperso" = "4.Text personalization",
                                                "Group representation: stdfragm" =  "4.System fragmentation",
                                                "Group representation: year.trend2nd elections" = "4.Second elections",
                                                "Group representation: countryGermany" = "4.Country (Germany)",
                                                "Group representation: stdlrpos" = "4.Left-right position",
                                                "Group representation: stdsize:stdlrpos" = "4.Position*size",
                                                "Group representation: stdsize:year.trend2nd elections" = "4.Year*size"))

plotreg_ds2 <- plotreg_ds2 +
  labs(subtitle = "2: Professional qualities, 3: Core political values, 4: Group representation")

wordreg(model_ds2, stars = c(0.001, 0.01, 0.05, 0.1), file = "reg_ds2_results.doc")

niche_p2 <- ggpredict(model_ds2, terms = "niche", ci.lvl = 0.95)
str(niche_p2)
plot_niche2 <- plot(niche_p2) +
  geom_line(alpha = 0.5, size = 1.2) +
  labs(x = NULL, y = NULL,
    title = "Predicted probabilites of image dimensions",
    subtitle = "Niche/ mainstream status") +
  theme_bw() +
  geom_text(aes(label = paste0(as.character(round(predicted, 3)*100), "%")), vjust = -0.5) +
  coord_cartesian(ylim = c(0, 0.8))

incumb_p2 <- ggpredict(model_ds2, terms = "incumb")
plot_incumb2 <- plot(incumb_p2) +
                  geom_line(alpha = 0.5, size = 1.2) +
                  coord_cartesian(ylim=c(0, 0.8)) +
                  labs(x = NULL, y = NULL,
                    title = "Predicted probabilites of image dimensions",
                    subtitle = "Incumbent status") +
                  theme_bw() +
                  geom_text(aes(label = paste0(as.character(round(predicted, 3)*100), "%")), vjust = -0.5)

year_p2 <- ggpredict(model_ds2, terms = "year.trend")
year_plot2 <- plot(year_p2) +
  geom_line(alpha = 0.5, size = 1.2) +
  labs(x = NULL, y = NULL,
       title = "Predicted probabilites of image dimensions",
       subtitle = "Electoral trend") +
  theme_bw() +
  coord_cartesian(ylim=c(0, 0.8)) +
  geom_text(aes(label = paste0(as.character(round(predicted, 3)*100), "%")), vjust = -0.5)


cou_p2 <- ggpredict(model_ds2, terms = "country")
cnt_plot <- plot(cou_p2, connect.lines = T) +
 geom_line(alpha = 0.5, size = 1.2) +
  labs(x = NULL, y = NULL,
     title = "Predicted probabilities of image dimensions",
   subtitle = "Country effect") +
 theme_bw()

size_p2 <- ggpredict(model_ds2, terms = "stdsize [all]")
size_plot <- plot(size_p2) +
  geom_line(alpha = 0.5, size = 1.2) +
  labs(x = NULL, y = NULL,
       title = "Predicted probabilites of image dimensions",
       subtitle = "Party size") +
  theme_bw() +
  coord_cartesian(ylim=c(0, 0.8))


fragm_p2 <- ggpredict(model_ds2, terms = "stdfragm [all]")
fragm_plot <- plot(fragm_p2) +
 #geom_line(alpha = 0.5, size = 1.2) +
  labs(x = NULL, y = NULL,
       title = "Predicted probabilites of image dimensions",
       subtitle = "System fragmentation") +
  theme_bw()

perso_p <- ggpredict(model_ds2, terms = "stdperso [all]")
perso_plot <- plot(perso_p) +
 geom_line(alpha = 0.5, size = 1.2) +
  labs(x = "Party text personalization (standardised)", y = NULL,
       title = "Predicted probabilites of image dimensions",
       subtitle = "Party text personalization") +
  theme_bw() +
  coord_cartesian(ylim = c(0,0.8))

pos_p <- ggpredict(model_ds2, terms = "stdlrpos [all]")
pos_plot <- plot(pos_p) +
  geom_line(alpha = 0.5, size = 1.2) +
  labs(x = "Position (standardised)", y = NULL,
       title = "Predicted probabilites of image dimensions",
       subtitle = "Left-right ideoloigcal position") +
  theme_bw() +
  coord_cartesian(ylim = c(0,0.8))

sizepos_p <- ggpredict(model_ds2, terms = c("stdsize[-2:2]",  "stdlrpos[-2:2]"))
inter_plot <- plot(sizepos_p) +
  geom_line(alpha = 0.5, size = 1) +
              labs(x = "Party size", y = NULL,
                   title = "Predicted probabilites of image dimensions",
                   subtitle = "Party size / ideological position interaction") +
  theme_bw() +
  scale_color_npg(name = "Left-right position")

plot_model(model_ds2, type ="pred", terms =c("stdsize[-2,-1,0,1,2]", "stdlrpos[-2,-1,0,1,2]"))

sizeyear_p <- ggpredict(model_ds2, terms = c("stdsize[-2:2]", "year.trend"))
sizeyear_plot <- plot(sizeyear_p) +
  geom_line(alpha = 0.5, size = 1.2) +
  labs(x = "Party size", y = NULL,
       title = "Predicted probabilites of image dimensions",
       subtitle = "Party size / electoral year interaction") +
  theme_bw() +
  scale_color_npg(name = "Elections") +
  coord_cartesian(ylim = c(0, 0.8))

year.p2 <- ggpredict(model_ds2, terms = "year.trend")
year_plot2 <- plot(year.p2) +
  geom_line(alpha = 0.5, size = 1.2) +
  labs(x = NULL, y = NULL,
       title = "Predicted probabilites of image dimensions",
       subtitle = "Electoral year") +
  theme_bw() +
  coord_cartesian(ylim = c(0, 0.8)) +
  geom_text(aes(label = paste0(as.character(round(predicted, 3)*100), "%")), vjust = -0.5)


year.country.p2 <- ggpredict(model_ds2, terms = c("year.trend", "country"))
plot(year.country.p2, connect.lines = T) +
  coord_cartesian(ylim = c(0, 0.8)) +
  geom_text_repel(aes(label = paste0(as.character(round(predicted, 3)*100), "%")), vjust = -0.5) +
  theme_bw() +
  labs(x = NULL,
       y = NULL,
       title = "Predicted probabilities of image dimensions",
       subtitle = "Electoral trend and country")
  
incumb.size <- ggpredict(model_ds2, terms = c("stdsize[all]", "incumb"))
plot(incumb.size) + 
  geom_line(alpha = 0.5, size = 1.2) +
  coord_cartesian(ylim = c(0, 0.8)) +
  theme_bw() +
  scale_color_npg(name = "Incumbency") +
  labs(x = NULL,
       y = NULL,
       title = "Predicted probabilities of image dimensions",
       subtitle = "Party size and incumbency status")

