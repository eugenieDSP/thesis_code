 ###Graphs for descriptives and bivariates

grouped.ds2 <- ds_2 %>%
  group_by(country, dims)

##plot colors
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#ff4500", "#0072B2", "#b22222", "#CC79A7",
          "#293352", "#8ff173", "#8f6787", "#F0E442",
          "#ff7373")
colors <- c("#800080", "#ffa500", "#40e0d0", "#ffd700",
            "#ff4040", "#000080", "#0000ff", "#ffc0cb",
            "#407294", "#ffff66", "#666666", "#bada55",
            "#008000", "#cbcba9")

##Niche
niche.ds2 <- grouped.ds2 %>%
  group_by(country, dims, niche) %>%
  summarise(N = n()) %>%
  arrange(desc(niche, N))

ggplot(niche.ds2, aes(niche, fill = reorder(dims, N))) +
  geom_bar(aes(y = N), stat = "identity", position = "fill") +
  facet_wrap(country~.,) +
  theme_light() +
  scale_fill_manual(values = colors, name = "Image dimension") +
  labs(title = "Images per niche/mainstream status, by country", x = "Status")


#Incumbent status
incumb.ds2 <- grouped.ds2 %>%
  mutate(incumb = as.factor(incumb)) %>%
  group_by(country, dims, incumb) %>%
  summarise(N = n()) %>%
  arrange(desc(incumb, N))

incumb.ds2$incumb <- factor(incumb.ds2$incumb, levels = c("0", "1"), labels=c("Not incumbent", "Incumbent"))

ggplot(incumb.ds2, aes(incumb, fill = reorder(dims, N))) +
  geom_bar(aes(y = N), stat = "identity", position = "fill") +
  facet_wrap(country~.,) +
  theme_light() +
  scale_fill_manual(values = cbp1, name = "Image dimension") +
  labs(title = "Images per incumbent status, by country", x = "Status")

#Party family - images
pf.ds2 <- df.dataset2 %>%
  group_by(country, year.trend, dims, party.fam) %>%
  summarise(N = n()) %>%
  arrange(desc(party.fam, N))

ggplot(pf.ds2, aes(party.fam, fill = reorder(dims, N))) +
  geom_bar(aes(y = N), stat = "identity", position = "fill") +
  facet_wrap(year.trend~country, scales = "free") +
  labs(title = "Images per party family, by country", x = "Party family", y = NULL) +
  theme_light(base_size = 10) +
  theme(axis.text.x=element_text(angle=30,hjust=1)) +
  theme(plot.margin=unit(c(1,1,1,1),"cm")) +
  labs(caption = "N: 14463")

##Document type

doc.ds2 <- grouped.ds2 %>%
  group_by(country, year.trend, dims, DOC.type) %>%
  summarise(N = n()) %>%
  arrange(desc(DOC.type, N))
attributes(doc.ds2$DOC.type)


doc.ds2$DOC.type <- unfactor(doc.ds2$DOC.type)
str(doc.ds2$DOC.type)
doc.ds2$DOC.type <- as.numeric(doc.ds2$DOC.type)
doc.ds2$DOC.type <- factor(doc.ds2$DOC.type, levels = c("Manifesto", "Press releases"), labels = c("Manifesto", "Press releases"))

ggplot(doc.ds2, aes(DOC.type, fill = reorder(dims, N))) +
  geom_bar(aes(y = N), stat = "identity", position = "fill") +
  facet_wrap(country~., scales = "free") +
  scale_fill_manual(values = cbp1, name = "Image dimension") +
  labs(title = "Images per document type, by country", x = "Document type", y = NULL) +
  theme_light(base_size = 10) +
  theme(plot.margin=unit(c(1,1,1,1),"cm")) +
  labs(caption = "N: 14463")

##Two-dimensional positions
ggplot(dataset2, aes(lrecon, galtan, color = country)) +
  geom_text(aes(label = party)) +
  scale_x_continuous(limits = c(0, 10)) +
  scale_y_continuous(limits = c(0,10)) +
  geom_segment(aes(x = 0, y = 5, xend = 10, yend = 5), color = "gray60") +
  geom_segment(aes(x = 5, y = 0, xend = 5, yend = 10), color = "gray60")

## Image category and dimension plots - the ones that disappeared T_T

#Image dimensions by parties
#create a table with absolute frequencies
abs.dims <- df.dataset2 %>%
  dplyr::select(country, party, dims) %>%
  group_by(country, party, dims) %>%
  summarize(N = n())

sum.dims <- abs.dims %>%
  group_by(country, party) %>%
  summarise(sum = sum(N))

ggplot(abs.dims, aes(x = reorder(party, -N), y = N, fill = dims, label = N)) +
  geom_bar(stat = "identity") +
  facet_wrap(.~country, scales = "free") +
  geom_text(size = 2, position = position_stack(vjust = 0.5))+
  geom_text(aes(party, sum, label = sum, fill = NULL), data = sum.dims, vjust = -0.9, size = 3)