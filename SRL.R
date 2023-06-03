#Import dataset

CallunaGermination <- read_excel(
  "~/Master/Dataset/CallunaGermination.xlsx")


CalGermST <- excel_sheets(
  "~/Master/Dataset/CallunaGermination.xlsx") %>% 
  purrr::map_df(~ read_excel(
    "~/Master/Dataset/CallunaGermination.xlsx", 
                             sheet = "SeedlingTraits"))


CalGermST <- CalGermST %>% 
  group_by(ID, Plot, Lab_treatment)


#Remove replicates and NAs

CalGermST <- unique(CalGermST)
CalGermST <- CalGermST %>% 
  drop_na(SRL)


#Re-leveling for the right comparison

CalGermST$Field_treatment <- relevel(factor(CalGermST$Field_treatment), 
                                     ref = "Control")

CalGermST$Phase <- relevel(factor(CalGermST$Phase), 
                           ref = "Young")

CalGermST$Lab_treatment <- relevel(factor(CalGermST$Lab_treatment), 
                                   ref = "WP1")

#log transforme SRL for normal distribution
srl <- CalGermST$SRL
hist(srl)

hist(log10(srl))
CalGermST$logSRL <- log10(srl)


#The linear mixed-effects model for SRL

SRLmodel <- lmer(logSRL ~ 
                   Geography * Phase * Field_treatment * Lab_treatment + 
                   (1|Site/Plot/ID), 
                 data = CalGermST)

anoSRL <- Anova(SRLmodel)

summary(SRLmodel)


#Post-Hoc using emmeans to investigate differences 
#between water potentials (Lab_treatment)

emmSW <- emmeans(SRLmodel, ~ Geography:Lab_treatment)
comparison <- pairs(emmSW, by = c("Geography"))
summary(comparison)


emm <- emmeans(SRLmodel, ~ Geography*Lab_treatment)
contrasts <- contrast(emm, "pairwise")

#Summarize the contrasts

summary(contrasts, infer = TRUE)


#The linear mixed effect model for SRL

SRLemms <- emmeans(SRLmodel, 
                   ~Geography *Phase*Field_treatment* Lab_treatment)
SRLemms
pairs(SRLemms, 
      simple = "each")



#Creating new labels

new_labelsG <- c("N" = "North", 
                 "S" = "South")

new_labelsP <- c("Young" = "Pioneer", 
                 "Intermediate" = "Building", 
                 "Old" = "Mature")


#Making the figure

SRLPlot <- CalGermST %>% 
  ggplot(aes(x = Lab_treatment, 
             y = logSRL, 
             fill=Field_treatment)) +
  geom_boxplot()+
  facet_grid(Phase~Geography,
             labeller = labeller(Phase = new_labelsP,
                                 Geography = new_labelsG)) +
  scale_fill_manual(labels= FT, 
                    breaks = c("Control", "50cover", "90cover"),
                    values = c("#4575b4", "#74add1", "#abd9e9"))+
  labs(fill="", x="", y="SRL (log)") +
  theme(legend.position="bottom", 
        axis.title.y = element_text(size = 12, 
                                    vjust = 2.5),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        panel.background = element_rect(fill = "grey96"),
        panel.grid = element_line(size = 0.5, 
                                  linetype = "solid", 
                                  colour = "white"))
SRLPlot

g7 <- ggplot_gtable(ggplot_build(SRLPlot))
strip_both <- which(grepl('strip-', g7$layout$name))
fills <- c("#e7d4e8", "#af8dc3", "#d9f0d3", "#7fbf7b", "#1b7837")
k <- 1
for (i in strip_both) {
  j <- which(grepl('rect', g7$grobs[[i]]$grobs[[1]]$childrenOrder))
  g7$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g7)


#Saving the figure

ggsave("SRLplotX.jpeg",
       plot=g7, 
       width = 7, 
       height = 8, 
       device = 'tiff', 
       dpi = 300)

