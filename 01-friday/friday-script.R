library('ggplot2')

data(gapminder, package = 'gapminder')

ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) +
  geom_point(shape = 1, aes(color = continent)) +
  stat_smooth(method = "lm", size = 1, color = "black") +
  scale_x_log10() +
  xlab("Per Capita GDP") +
  ylab("Life Expectancy (yrs)") +
  facet_wrap(~continent, scales = "free") +
  theme_bw() +
  guides(color=FALSE)

# gmkfmgflmglmgfr
myplot <- ggplot(data = iris,
                 aes(x = Sepal.Length, y = Sepal.Width))
summary(myplot)

ggplot(data = iris,
       aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point()

myplot + geom_point(size = 0.8, colour = 'red')

ggplot(data = iris,
       aes(x = Sepal.Length, y = Sepal.Width,
           group = Species)) +
  geom_point(aes(shape = Species, colour = Species)) +
  geom_smooth(method = 'lm', colour = "black")

d2 <- diamonds[sample(1:nrow(diamonds), 1000), ]

ggplot(data = d2,
       aes(x = carat, y = price, colour = color)) +
  geom_point()

data(birthwt, package = "MASS")
head(birthwt)

ggplot(birthwt,
       aes(x = factor(smoke), y = bwt)) +
  geom_boxplot(notch = TRUE)

birthwt <- transform(birthwt,
                     smoke = factor(smoke,
                                    labels = c("no", "yes")))

ggplot(birthwt,
       aes(x = smoke, y = bwt)) +
  geom_boxplot(notch = TRUE)


ggplot(iris, aes(x = Sepal.Length,
                 y = Sepal.Width,
                 colour = Species)) +
  geom_point() +
  facet_wrap(~ Species, ncol = 2,
             scales = 'fixed')


aes(color = variable) # mapping
color = "black" # setting
# Or add it as a scale
scale_colour_manual(values = c("red", "green", "blue"))


library("RColorBrewer")
display.brewer.all()

library("RColorBrewer")
library('tidyr')
df <- gather(iris, key = flower_attribute,
             value = measurement, -Species)
df[1:2,]

ggplot(df, aes(Species, measurement, fill = flower_attribute)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = "Dark2")

ggplot(iris,
       aes(x = Sepal.Length, y = Sepal.Width,
           colour = Species)) +
  geom_point() +
  scale_color_brewer(palette = "Dark2")

ggplot(iris,
       aes(x = Sepal.Length, y = Sepal.Width,
           colour = Species)) +
  geom_point() +
  scale_color_manual(values = c('red','green', 'blue'))

ggplot(birthwt, aes(factor(smoke), bwt)) +
  geom_boxplot(width = .2) +
  scale_y_continuous(labels = paste(1:4, "Kg"),
                     breaks = seq(1000, 4000, by = 1000)) +
  labs(y = NULL)

library('viridis')
ggplot(iris,
       aes(x = Sepal.Length, y = Sepal.Width,
           colour = Species)) +
  geom_point() +
  scale_color_viridis(discrete = TRUE) # need discrete = TRUE for categorical

## After Lunch

library('readr')
climate <- read_csv('http://bit.ly/2JLBIFv')
climate <- climate[, -1]
climate[1:2, ]

h <- ggplot(faithful, aes(x = waiting))

h + geom_histogram()

h + geom_histogram(binwidth = 5, fill = 'steelblue', colour = 'black')

head(climate)
climate[1:6,]

c2 <- climate[sample(nrow(climate)), ]

ggplot(c2, aes(x = Year, y = Anomaly10y)) +
  geom_line()

ggplot(c2, aes(x = Year, y = Anomaly10y)) +
  geom_path()

ggplot(climate, aes(x = Year, y = Anomaly10y)) +
  geom_ribbon(aes(ymin = Anomaly10y - Unc10y,
                  ymax = Anomaly10y + Unc10y),
              fill = "blue", alpha = 0.3) +
  geom_line()

ggplot(climate, aes(x = Year, y = Anomaly10y)) +
  geom_line(aes(y = Anomaly10y - Unc10y),
              colour = "red", linetype = 'dashed') +
  geom_line(aes(y = Anomaly10y + Unc10y),
            colour = "red", linetype = 'dashed') +
  geom_line()

ggplot(iris, aes(x = Species, y = Sepal.Length)) +
  geom_bar(stat = "identity")

ggplot(df, aes(x = Species, y = measurement,
               fill = flower_attribute)) +
  geom_bar(stat = 'identity', position = "dodge", colour = "black")


ggplot(d2, aes(x = clarity, fill = cut)) +
  geom_bar(position = "dodge", colour = "black")

ggplot(faithful, aes(x = waiting)) +
  geom_density(fill = "forestgreen")

ggplot(faithful, aes(x = waiting)) +
  geom_line(colour = "forestgreen", stat = "density")

## Themeing

theme_set(theme_bw())

plt <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, colour = Species)) +
  geom_point() +
  facet_wrap(~ Species) +
  theme(legend.key = element_rect(fill = NA),
        legend.position = "bottom",
        strip.background = element_rect(fill = "red"),
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        text = element_text(colour = "red"))
plt

ggsave("gavs-funky-plot.png", plt, height = 6, width = 8, dpi = 300)

plt2 <- ggplot(faithful, aes(x = waiting)) +
  geom_line(colour = "forestgreen", stat = "density")
plt2

library("cowplot")
theme_set(theme_bw())

plot_grid(plt, plt2, align = "vh", axis = "lrtb",
          labels = c("a)", "b)"), rel_heights = c(0.66, 0.34),
          ncol = 1)

plt + labs(title = "Gav's funky plot", subtitle = "Not!",
           caption = "Source: E. Anderson")

library("analogue")
?Stratiplot

data(V12.122)
Depths <- as.numeric(rownames(V12.122))

plt <- Stratiplot(Depths ~ O.univ + G.ruber + G.tenel + G.pacR,
                  data = V12.122,  type = c("h","l","g","smooth"))
plt

coredat <- transform(V12.122, Depths = Depths)
df2 <- gather(coredat[, c('O.univ', 'G.ruber', 'G.tenel', 'G.pacR',
                          'Depths')], key = "Species",
              value = "Abundance",
              - Depths)

ggplot(df2, aes(y = Depths, x = Abundance, group = Species)) +
  geom_point() +
  facet_grid(. ~ Species, scales = "free_x",
             space = "free_x") +
  scale_y_reverse()

df3 <- chooseTaxa(V12.122, max.abun = 0.1, n.occ = 10)
coredat <- transform(df3, Depths = Depths)
df4 <- gather(coredat, key = "Species", value = "Abundance",
              - Depths)

ggplot(df4, aes(y = Depths, x = Abundance, group = Species)) +
  geom_path() +
  geom_segment(aes(xend = 0, yend = Depths)) +
  facet_grid(. ~ Species, scales = "free_x", space = "free_x") +
  scale_y_reverse() +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50)/100,
                     labels = c("0", as.character(c(10, 20, 30, 40, 50)/100))) +
  expand_limits(x = 0) +
  labs(x = NULL, y = "Depth (cm)") +
  theme(panel.spacing.x = unit(0.2, "cm"),
        strip.text.x = element_text(angle = 60, hjust = 0.1, vjust = 0),
        strip.background = element_blank()) +
  labs(title = "V12.122", caption = "Proportional Abundance")
