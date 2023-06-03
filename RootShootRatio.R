##Root:shoot

#See SRL.R for the download
#the data can be downloaded from osf: https://osf.io/nu7mv/

#Remove NAs for root:shoot

CalGermST <- CalGermST %>% 
  drop_na(RootShootRatio)


#checking the distribution and log transforming it for normal distrubution
RSR <- CalGermST$RootShootRatio
hist(RSR)

CalGermST$logRSR <- log10(RSR)
hist(CalGermST$logRSR)


#Re-leveling the data for the right comparison

CalGermST$Field_treatment <- relevel(factor(CalGermST$Field_treatment), 
                                     ref = "Control")

CalGermST$Phase <- relevel(factor(CalGermST$Phase), 
                           ref = "Young")

CalGermST$Lab_treatment <- relevel(factor(CalGermST$Lab_treatment), 
                                   ref = "WP1")


#Making the figure
RoottoShootPlot <- CalGermST %>% 
  ggplot(aes(x=Lab_treatment, y=logRSR, fill=Field_treatment)) +
  geom_boxplot()+
  facet_grid(Phase~Geography,
             labeller = labeller(Phase = new_labelsP,
                                 Geography = new_labelsG)) +
  scale_fill_manual(labels= FT, 
                    breaks = c("Control", "50cover", "90cover"),
                    values = c("#4575b4", "#74add1", "#abd9e9")) +
  labs(fill="", x="", y="Root:Shoot (log)") +
  theme(legend.position="bottom") +
  theme(legend.position="bottom", 
        axis.title.y = element_text(size = 12, vjust = 2.5),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        panel.background = element_rect(fill = "grey96"),
        panel.grid = element_line(size = 0.5, 
                                  linetype = "solid", 
                                  colour = "white"))
RoottoShootPlot

g6 <- ggplot_gtable(ggplot_build(RoottoShootPlot))
strip_both <- which(grepl('strip-', g6$layout$name))
fills <- c("#e7d4e8", "#af8dc3", "#d9f0d3", "#7fbf7b", "#1b7837")
k <- 1
for (i in strip_both) {
  j <- which(grepl('rect', g6$grobs[[i]]$grobs[[1]]$childrenOrder))
  g6$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g6)


#Saving the figure

ggsave("SRLogRSR.jpeg", 
       plot=g6, 
       width = 7, 
       height = 8, 
       device = 'tiff', 
       dpi = 300)


#The linear mixed-effect model for Root:shoot

RSRmodel <- lmer(logRSR ~ Geography*Phase*Field_treatment*Lab_treatment+
                   (1|Site/Plot/ID), data = CalGermST)


#testing the model using Anova()

anoRSR <- Anova(RSRmodel)
summary(RSRmodel)
anoRSR


#Post-hoc test using emmeans()


#Testing the populations (North vs South)

emmSWM <- emmeans(RSRmodel, ~ Geography)
ComparisonsRSRP <- pairs(emmSWM)
summary(ComparisonsRSRP)


#Testing the interaction between successional stage and water potential
emmRSR <- emmeans(RSRmodel, ~ Phase*Lab_treatment)
comparisonRSRPL <- pairs(emmSW, by = c("Phase", "Lab_treatment"))
summary(comparisonRSRPL)


emmRSR <- emmeans(RSRmodel, ~ Phase:Lab_treatment)
comparison <- pairs(emmRSR, by = c("Phase"))
summary(comparison)
contrasts <- pairs(emmRSR, simple = "each")
summary(contrasts)
