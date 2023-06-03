##Time to maximum germination (Tmax)


#Re-naming the dataset

Tmax <- T50.data

#Checking the distrubution and transforming to log.

hist(Tmax$Days_since_start)
hist(log10(Tmax$Days_since_start)) #denne er best
Tmax$Tmaxlog <- log10(Tmax$Days_since_start)


#Re-leveling for the right comparison

Tmax$Field_treatment <- relevel(factor(Tmax$Field_treatment), 
                                ref = "Control")

Tmax$Phase <- relevel(factor(Tmax$Phase), 
                      ref = "Young")

Tmax$Lab_treatment <- relevel(factor(Tmax$Lab_treatment), 
                              ref = "WP1")


#Creating the figure for Time to maximum germination

plotTTmax <- Tmax %>% 
  ggplot(aes(x = Lab_treatment, 
             y = Days_since_start, 
             fill=Field_treatment)) +
  geom_boxplot()+
  facet_grid(Phase~Geography,
             labeller = labeller(Phase = new_labelsP,
                                 Geography = new_labelsG)) +
  scale_fill_manual(labels= FT, 
                    breaks = c("Control", "50cover", "90cover"),
                    values = c("#4575b4", "#74add1", "#abd9e9")) +
  labs(fill="", x="", y="Tmax")  +
  theme(legend.position="bottom", 
        axis.title.y = element_text(size = 10, vjust = 2.5),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 10),
        strip.text = element_text(size = 12),
        panel.background = element_rect(fill = "grey96"),
        panel.grid = element_line(size = 0.5, 
                                  linetype = "solid", colour = "white"))

gmax <- ggplot_gtable(ggplot_build(plotTTmax))
strip_both <- which(grepl('strip-', gmax$layout$name))
fills <- c("#e7d4e8", "#af8dc3",  "#d9f0d3", "#7fbf7b", "#1b7837")
k <- 1
for (i in strip_both) {
  j <- which(grepl('rect', gmax$grobs[[i]]$grobs[[1]]$childrenOrder))
  gmax$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(gmax)

#Saving the figure

ggsave("Tmaxplot.jpeg", 
       plot=gmax, 
       width = 7, 
       height = 8, 
       device = 'tiff', 
       dpi = 300)


#The linear mixed effect model for Time to maximum germination

Tmaxmodel <- lmer(Days_since_start ~ 
                           Geography*Phase*Field_treatment*Lab_treatment
                         + (1|Site/Plot/ID), 
                         data = Tmax)

#Testing the model using Anova

anoTmax <- Anova(Tmaxmodel, type = "II")
anoTmax
