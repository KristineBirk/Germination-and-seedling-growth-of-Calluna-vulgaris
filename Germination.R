#Accessing the data

germ <- read.csv("~/Master/Dataset/CallunaGerminationTrue.csv", 
                 header = TRUE, sep = ";", 
         stringsAsFactors = FALSE)

#Fixing the naming of the ID coloumn

germ2 <- germ %>% 
  rename(ID = ï..ID) %>% 
  mutate(PetriID = paste0(Lab_treatment, "_", ID, "_", Plot))


#Adding numerical value to the water potentials (WP1 - WP5)

germ2 <- germ2 %>% 
  mutate(numWP = case_when(Lab_treatment == "WP1" ~ -0.25,
                           Lab_treatment == "WP2" ~ -0.5,
                           Lab_treatment == "WP3" ~ -0.7,
                           Lab_treatment == "WP4" ~ -1.2,
                           Lab_treatment == "WP5" ~ -1.7))

GermPercentage1 <- germ2 %>%
  group_by(ID, Plot, Site, PetriID, Geography, 
           Field_treatment, Phase, Lab_treatment, numWP, Days_since_start) %>%
  summarise(Nr_seeds = n(),
    Nr_germinated = sum(Germinated), 
    GermFail = Nr_seeds-Nr_germinated)
GermPercentage1


#Re-level the variables for the right comparisons 

GermPercentage1$Field_treatment <- relevel(factor(GermPercentage1$Field_treatment), 
                                   ref = "Control")

GermPercentage1$Phase <- relevel(factor(GermPercentage1$Phase), 
                                 ref = "Young")

GermPercentage1$Lab_treatment <- relevel(factor(GermPercentage1$Lab_treatment), 
                                         ref = "WP1")

#The generalized linear model with binomial distribution

GermBio2 <- glmer(cbind(Nr_germinated, GermFail) ~ Geography * Phase
                 * Field_treatment * Lab_treatment
                 + (1|Site/Plot/ID), 
                 data = GermPercentage1, family = binomial,
                 control=glmerControl(optCtrl=list(maxfun=1e7)))

#Anova

anova_glmer <- car::Anova(GermBio2, type="II")
anova_glmer

summary(GermBio2)


#Post-hoc to compare the water potentials using emmeans.
emmGM <- emmeans(GermBio2, ~ Lab_treatment)
pairwiseG <- pairs(emmGM)
summary(pairwiseG)


#Post-hoc to test the differences between the populations
emmGMGeo <- emmeans(GermBio2, ~ Geography)
comparisonGMGeo <- pairs(emmGMGeo)
summary(comparisonGMGeo)


#To test the successional stages from each other
emmGMP <- emmeans(GermBio2, ~ Phase)
comparisonGMP <- pairs(emmGMP)
summary(comparisonGMP)


#Geography x Phase
emmGSS <- emmeans(GermBio2, ~ Geography:Phase)
comparisonGSS <- pairs(emmGSS, by = "Geography")
summary(comparisonGSS)


#Geography x lab_treatment
emmGSS <- emmeans(GermBio2, ~ Geography:Lab_treatment)
comparisonGSS <- pairs(emmGSS, by = "Lab_treatment")
summary(comparisonGSS)


#Testing the four-way interaction for the full model. 
emmG <- emmeans(GermBio2, ~ Geography:Phase:Field_treatment:Lab_treatment)
comparisonGSS <- pairs(emmG, by = "Phase","Field_treatment","Lab_treatment")


#compare north and south within mature
comparisonGeoxSS <- pairs(emmG, by = c("Geography","Field_treatment","Lab_treatment"))
summary(comparisonGeoxSS)



#Compare all levels of Lab_treatment within each combination of Geography and Phase
comparisonG <- pairs(emmG, by = c("Geography", "Phase", "Field_treatment"))
summary(comparisonG)



comparisonG <- pairs(emmG, by = c("Field_treatment", "Lab_treatment"))
summary(comparisonG)


##Creating the figures

#Make a dataframe with the germination percentage 

Germplot.df <- GermPercentage1 %>% 
  group_by(ID, Plot, Site, PetriID, 
           Geography, Field_treatment, 
           Phase, Lab_treatment, numWP, 
           Nr_seeds, Nr_germinated) %>%
  summarise(GermPercent = Nr_germinated/Nr_seeds*100)


#Removing the NAs

Germplot.df <- Germplot.df %>% 
  drop_na(numWP)


#Re-leveling for the figure to have the right order

Germplot.df$Field_treatment <- relevel(factor
                                       (Germplot.df$Field_treatment), 
                                       ref = "Control")

Germplot.df$Phase <- relevel(factor(Germplot.df$Phase), 
                             ref = "Young")


#Giving new names for the figure

new_labelsG <- c("N" = "North", 
                 "S" = "South")
new_labelsP <- c("Young" = "Pioneer", 
                 "Intermediate" = "Building", 
                 "Old" = "Mature")

new_labelsFT <- c("Control" = "Control", 
                 "50cover" = "60% cover", 
                 "90cover" = "90% cover")


#Creating the figures

germplotX <- Germplot.df %>% 
  ggplot(aes(x = Lab_treatment, y = GermPercent, fill=Field_treatment)) +
  geom_boxplot()+
  facet_grid(Phase~Geography,
             labeller = labeller(Phase = new_labelsP,
                                 Geography = new_labelsG)) +
  scale_fill_manual(labels= FT, 
                    breaks = c("Control", "50cover", "90cover"),
                    values = c("#4575b4", "#74add1", "#abd9e9")) +
  labs(fill="", x="", y="Germination %") +
  theme(legend.position="bottom",
        plot.margin = unit(c(1, 1, 4, 1), "lines"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),
      axis.text = element_text(size = 10),
        legend.text = element_text(size = 10),
        strip.text = element_text(size = 12),
        panel.background = element_rect(fill = "grey96"),
        panel.grid = element_line(size = 0.5, 
                                  linetype = "solid", 
                                  colour = "white"))
germplotX

g4 <- ggplot_gtable(ggplot_build(germplotX))
strip_both <- which(grepl('strip-', g4$layout$name))
fills <- c("#e7d4e8", "#af8dc3", "#d9f0d3", "#7fbf7b", "#1b7837")
k <- 1
for (i in strip_both) {
  j <- which(grepl('rect', g4$grobs[[i]]$grobs[[1]]$childrenOrder))
  g4$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g4) 

ggsave("GermplotX.jpeg", 
       plot=g4, 
       width = 7, 
       height = 8, 
       device = 'tiff', 
       dpi = 300)


####Her nede må eg sjekke at det er ok!!!! ELLER OM DET SKAL VEKK

#Her legger eg til det fullstendige datasettet og 
#legger til at dei som ikkje
#har verdi for spiringsprosent fÃ¥r verdien 0.

Germana <- subset(germ2, select = -c(Seed_nr, Year, Month, Day,
                                     Days_since_start, Petri_dish, 
                                     Germinated))
Germana <- distinct(Germana)
Germana <- left_join(Germana, GermPercentage1)
Germana <- mutate_at(Germana, c("Rel.freq"), ~replace(., is.na(.), 0))

Germana <- Germana %>%
  group_by(PetriID)

GermAna <- subset(Germana, 
                  select = -c(Seed_nr, Year, Month, Day,
                                       Days_since_start, Petri_dish, 
                                       Germinated, 
                              Nr_germinated,
                              Nr_seeds))
GermAna <- distinct(GermAna)
GermAna <- GermAna %>% 
  drop_na(ID)

#Her fiksar eg slik at riktige delar hamnar som intercept.
GermAna$Field_treatment <- relevel(factor(GermAna$Field_treatment), 
                                   ref = "Control")
GermAna$Phase <- relevel(factor(GermAna$Phase), ref = "Young")








#Her er plottet for spiringsprosenten.
germplot <- GermAna %>% 
  ggplot(aes(x = Lab_treatment, y = Rel.freq, fill=Phase)) +
  geom_boxplot(position = position_dodge(preserve = "single")) +
  facet_wrap(~Geography, labeller = as_labeller(G)) +
  scale_fill_manual(labels= SS, breaks = c("Young", 
                                           "Intermediate", 
                                           "Old"), 
                    values = c("#d9f0d3", "#7fbf7b", "#1b7837"))+
  labs(fill="", x="", y="Germination %") +
  #annotate(geom = "text", x = 1.5 + 4 * (0:4), 
  #y = 100, label = unique(GermPercentage$Lab_treatment), size = 10)+
  #coord_cartesian(ylim = c(0, 100), expand = FALSE, clip = "off") +
  theme(legend.position="bottom",
        #plot.margin = unit(c(1, 1, 4, 1), "lines"),
        #axis.title.x = element_blank(),
        #axis.text.x = element_blank(),
        axis.title.y = element_text(size = 10),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 10),
        strip.text = element_text(size = 12),
        panel.background = element_rect(fill = "grey96"),
        panel.grid = element_line(size = 0.5, 
                                  linetype = "solid", 
                                  colour = "white"))
germplot

#Change the color of the facet_wrap

g4 <- ggplot_gtable(ggplot_build(germplot))
strip_both <- which(grepl('strip-', g4$layout$name))
fills <- c("#e7d4e8", "#af8dc3")
k <- 1
for (i in strip_both) {
  j <- which(grepl('rect', g4$grobs[[i]]$grobs[[1]]$childrenOrder))
  g4$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g4) 
ggsave("GermPplot.jpeg", plot=g4, width = 7, 
       height = 4, device = 'tiff', dpi = 300)



anova(GermPmodel1)
summary(GermPmodel1)
tab_model(GermPmodel1,  dv.labels = c("Germination %"))





prøve <- distinct(germ2)
prøve <- prøve[-c(6849),]
prøve <- subset(prøve, select = -c(Seed_nr, Year, Month, Day,
                                   Days_since_start, Petri_dish, X))

prøve$Field_treatment <- relevel(factor(prøve$Field_treatment), 
                                 ref = "Control")
prøve$Phase <- relevel(factor(prøve$Phase), ref = "Young")

###Test 5-way
