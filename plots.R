 ###Graphs for descriptives and bivariates

##plot colors
plot.colors <- c("#a39193", "#c39a9d", "#eea990", "#f6e0b5",
                 "#91a3a1", "#c3ac9a", "#90d5ee", "#90eea9",
                 "#b5cbf6", "#b5f6e0", "#b1a7b2", "#DDA2D4")
##Grouped data frame
grouped.ds2 <- dataset2 %>%
  group_by(country, party, dims)

###Niche
niche.ds2 <- grouped.ds2 %>%
  dplyr::select(country, dims, niche) %>%
  group_by(country, dims, niche) %>%
  summarise(N = n()) %>%
  arrange(desc(niche, N))

#Percentages df
perc.niche <- niche.ds2 %>%
  group_by(country, niche) %>%
  summarise(nsum = sum(N))

#Merging
niche.m <- inner_join(niche.ds2, perc.niche, by = c("country", "niche")) %>%
  mutate(prop = N/nsum*100) %>%
  mutate(prop = as.character(round(prop, 2)))


ggplot(niche.m, aes(niche, fill = reorder(dims, N), label = prop)) +
  geom_bar(aes(y = N), stat = "identity", position = "fill") +
  geom_text(aes(y = N, label = paste0(prop, "%")), size = 3.5, position = position_fill(vjust = 0.5)) +
  facet_wrap(country~.,) +
  theme_light() +
  theme(legend.position = "top") +
  scale_fill_manual(values = plot.colors, name = NULL) +
  labs(title = "Images per niche/mainstream status, by country",
       x = "Status")


###Incumbent status
incumb.ds2 <- grouped.ds2 %>%
  mutate(incumb = as.factor(incumb)) %>%
  group_by(country, dims, incumb) %>%
  summarise(N = n()) %>%
  arrange(desc(incumb, N))

perc.incumb <- incumb.ds2 %>%
  group_by(country, incumb) %>%
  summarise(nsum = sum(N))

#Merging
incumb.m <- inner_join(incumb.ds2, perc.incumb, by = c("country", "incumb")) %>%
  mutate(prop = N/nsum*100) %>%
  mutate(prop = as.character(round(prop, 2)))

ggplot(incumb.m, aes(incumb, fill = reorder(dims, N))) +
  geom_bar(aes(y = N), stat = "identity", position = "fill") +
  geom_text(aes(y = N, label = paste0(prop, "%")), size = 3.5, position = position_fill(vjust = 0.5)) +
  facet_wrap(country~.,) +
  theme_light() +
  theme(legend.position = "top") +
  scale_fill_manual(values = plot.colors, name = NULL) +
  labs(title = "Images per incumbent status, by country", x = "Status")


###Party family - images
pf.ds2 <- grouped.ds2 %>%
  dplyr::select(country, dims, party.fam) %>%
  group_by(country, dims, party.fam) %>%
  summarise(N = n()) %>%
  arrange(desc(party.fam, N))

#Percentage df
perc.pf <- pf.ds2 %>%
  group_by(country, party.fam) %>%
  summarise(nsum = sum(N))

#Merging
pf.m <- inner_join(pf.ds2, perc.pf, by = c("country", "party.fam")) %>%
  mutate(prop = N/nsum*100) %>%
  mutate(prop = as.character(round(prop, 2)))

ggplot(pf.m, aes(party.fam, fill = reorder(dims, N), label = prop)) +
  geom_bar(aes(y = N), stat = "identity", position = "fill") +
  geom_text(aes(y = N, label = paste0(prop, "%")), size = 3.5, position = position_fill(vjust = 0.2)) +
  facet_wrap(.~country, scales = "free") +
  labs(title = "Images per party family, by country", x = NULL, y = NULL) +
  theme_light(base_size = 10) +
  theme(axis.text.x=element_text(angle=30,hjust=1),
        plot.margin=unit(c(1,1,1,1),"cm"),
        legend.position = "top") +
  scale_fill_manual(values = plot.colors, name = NULL) +
  labs(caption = "N: 14463")

##Document type

doc.ds2 <- dataset2 %>%
  dplyr::select(country, year.trend, dims, doctype) %>%
  group_by(country, year.trend, dims, doctype) %>%
  summarise(N = n())

attributes(doc.ds2$doctype)

#Fix factor levels
doc.ds2$doctype <- unfactor(doc.ds2$doctype)
str(doc.ds2$doctype)
doc.ds2$doctype <- as.numeric(doc.ds2$doctype)
doc.ds2$doctype <- factor(doc.ds2$doctype, levels = c(1, 2), labels = c("Manifesto", "Press releases"))
str(doc.ds2$doctype)

# Percentage dataframe
doc.perc <- doc.ds2 %>%
  group_by(country, year.trend, doctype) %>%
  summarise(nsum = sum(N))

doc.merged <- inner_join(doc.ds2, doc.perc, by = c("country", "doctype", "year.trend")) %>%
  mutate(prop = N/nsum*100) %>%
  mutate(prop = as.character(round(prop, 2)))

# Plot
ggplot(doc.merged, aes(doctype, fill = reorder(dims, N), label = prop)) +
  geom_bar(aes(y = N), stat = "identity", position = "fill") +
  geom_text(aes(y = N, label = paste0(prop, "%")), size = 3.5, position = position_fill(vjust = 0.2)) +
  facet_wrap(country~year.trend, scales = "free") +
  scale_fill_manual(values = plot.colors, name = NULL) +
  labs(title = "Images per document type, by country and elections", x = NULL, y = NULL) +
  theme_bw(base_size = 10) +
  theme(plot.margin=unit(c(1,1,1,1),"cm"),
        legend.position = "top") +
  labs(caption = "N: 14463")

##Two-dimensional positions
ggplot(dataset2, aes(lrecon, galtan, color = country)) +
  geom_text(aes(label = party)) +
  scale_x_continuous(limits = c(0, 10)) +
  scale_y_continuous(limits = c(0,10)) +
  geom_segment(aes(x = 0, y = 5, xend = 10, yend = 5), color = "gray60") +
  geom_segment(aes(x = 5, y = 0, xend = 5, yend = 10), color = "gray60") +
  theme_bw()

## Image category and dimension plots - the ones that disappeared T_T

#Image dimensions by parties
#create a table with absolute frequencies
abs.dims <- dataset2 %>%
  dplyr::select(country, party, dims) %>%
  group_by(country, party, dims) %>%
  summarize(N = n())

sum.dims <- abs.dims %>%
  group_by(country, party) %>%
  summarise(nsum = sum(N))

#Overall plot, absolute frequencies
abs.plot <- ggplot(abs.dims, aes(x = reorder(party, -N), y = N, fill = dims)) +
  geom_bar(stat = "identity") +
  facet_wrap(.~country, scales = "free")

abs.plot <- abs.plot + geom_text(aes(label = N), size = 3, position = position_stack(vjust = 0.3))
 

abs.plot <- abs.plot + geom_text(aes(x = reorder(party, -nsum),
                                     y = nsum,
                                     label = nsum,
                                     fill = NULL),
                     data = sum.dims,
                     position = "stack", 
                     vjust = -0.9, size = 4)

abs.plot <- abs.plot + theme_bw() +
  scale_fill_manual(values = plot.colors, name = NULL) +
  labs(title = "Number of images per dimension, by party and country; absolute frequencies",
       x = "Party",
       y = "N of images",
       caption = "N: 14463") +
  theme(legend.position = "top")

abs.plot <- abs.plot + 

#Overall plot, relative (%) frequencies
#Prepare the table
merged.dims <- inner_join(abs.dims, sum.dims, by = c("party", "country")) %>%
  mutate(prop = N/nsum*100) %>%
  arrange(country, party, dims, prop)
 
merged.dims$prop<- round(merged.dims$prop, digits = 2)
merged.dims <- merged.dims %>% mutate(prop = as.character(prop))

rel_plot <- ggplot(merged.dims, aes(x = party, y = N, fill = dims, label = prop)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(.~country, scales = "free") +
  geom_text(aes(label = paste0(prop, "%")), size = 3.5, position = position_fill(vjust = 0.5)) +
  scale_fill_manual(values = plot.colors, name = "Dimension") +
  labs(title = "Number of images per dimension, by party and country; relative frequencies",
       x = "Party",
       y = "% of images",
       caption = "N: 14463") +
  theme_bw() +
  theme(legend.position = "top")

### Plots by image dimensions, relative (percentages) frequencies
## Personal characteristics

## Professional qualities

## Core political values

## Group representation
