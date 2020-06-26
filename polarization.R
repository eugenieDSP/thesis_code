#Polarization

dt.pol <- dataset1 %>%
    dplyr::select(country, party, lrpos.10, party.size)

dt.pol <- dt.pol %>%
  group_by(country, party) %>%
  summarize(lrpos.10.mean = mean(lrpos.10), party.size.mean = mean(party.size))

dt.pol <- dt.pol %>%
  filter(party != "CSU", party != "CDU")

dt.pol <- dt.pol %>%
  mutate(lrpos.5 = ((lrpos.10.mean-mean(lrpos.10.mean))/5)^2)
#Germany
dt.pol %>%
  filter(country == "Germany") %>%
  summarise(pol.index = sqrt(sum(party.size.mean*lrpos.5)))
#Switzerland
dt.pol %>%
  filter(country == "Switzerland") %>%
  summarise(pol.index = sqrt(sum(party.size.mean*lrpos.5)))


###Importing different left-right positions
path1 <- "C:/Users/Evgeniya Shtyrkova/Documents/MEGA/PhD Thesis/Data/Datasets/2010_CHES_dataset_means.dta"
path2 <- "C:/Users/Evgeniya Shtyrkova/Documents/MEGA/PhD Thesis/Data/Datasets/2014_CHES_dataset_means-2.dta"

ches10 <- read.dta13(path1, nonint.factors = T, generate.factors = T, convert.factors = T, convert.underscore = T)
ches14 <- read.dta13(path2, nonint.factors = T, generate.factors = T, convert.factors = T, convert.underscore = T)
glimpse(ches10)
glimpse(ches14)

ches10$cname <- as.factor(ches10$cname)
ches14$cname <- as.factor(ches14$cname)

ches14s <- ches14 %>%
  dplyr::select(country, cname, party.id, party.name, lrgen, lrecon, galtan, std.lrecon, std.galtan, position)
  

glimpse(ches14s)
glimpse(ches10)

ches10dt <- as.data.table(ches10)
ches14dt <- as.data.table(ches14s)
ches14dt <- ches14dt[cname %in% c("swi", "ger")]

require(data.table)
setDT(ches10dt)
setDT(ches14dt)

chesm <- merge(ches10dt, ches14dt, by = c("party.name", "position", "cname", "lrgen", "lrecon", "galtan", "std.galtan",
                                          "std.lrecon"), all = TRUE)
glimpse(chesm)
chesm <- chesm[, !9:11]
chesm <- chesm[, !10:11]

chesm <- chesm %>% mutate(year = ifelse(cname == "ger" & is.na(year), 2013, year))
chesm <- chesm %>% mutate(year = ifelse(cname == "swi" & is.na(year), 2015, year))
glimpse(chesm)

chesm$party.name <- as.factor(chesm$party.name)
glimpse(chesm)
write.csv(chesm, file = "ches_merged.csv")

ggplot(chesm, aes(x = lrecon, y = galtan, color = cname)) +
  geom_point()
