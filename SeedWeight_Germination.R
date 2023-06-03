#Effect of seed mass on germinaion percentage

#the data can be downloaded from osf: https://osf.io/nu7mv/

#merging the datasets for the germination percentage and the seed weight
SeedGerm <- merge(Seed.df, GermPercentage1, by=c("ID","Plot"))
SeedGerm <- SeedGerm %>% 
  group_by(PetriID, Plot, ID)


#Removing replicated coloumns and re-naming coloumns used
SeedGerm <- subset(SeedGerm, select = -c(Nr_of_seeds, Site.y,
                             Geography.y, Field_treatment.y, 
                             Phase.y))

SeedGerm <- rename(SeedGerm,
                   Field_treatment = Field_treatment.x,
                   Geography = Geography.x,
                   Phase = Phase.x, 
                   Site = Site.x)


#re-level for right comparison

SeedGerm$Field_treatment <- relevel(factor(SeedGerm$Field_treatment), 
                                    ref = "control")

SeedGerm$Phase <- relevel(factor(SeedGerm$Phase), 
                          ref = "Young")

SeedGerm$Lab_treatment <- relevel(factor(SeedGerm$Lab_treatment), 
                                  ref = "WP1")


#Removing replicates
SeedGerm <- unique(SeedGerm)

plotSW3 <- SeedGerm %>% 
  mutate(Field_treatment = fct_relevel(Field_treatment, 
                                       "control", 
                                       "50cover", 
                                       "90cover")) %>%
  ggplot() +
  facet_wrap(~Geography, labeller = as_labeller(G))+
  geom_point(aes(x=SW, y=Rel.freq, color=Field_treatment)) +
  geom_smooth(method="lm", 
              aes(y=SeedGerm$preddi, 
                  x=SeedGerm$SW, 
                  color=Field_treatment),
              size=1, alpha=0.2) +
  scale_color_manual(labels= FT, limits=c("control", 
                                          "50cover", 
                                          "90cover"), 
                     values = c("#4575b4", "#74add1", "#abd9e9")) +
  labs(color="", x="Seed weight (mg)", 
       y="Germination %") +
  theme(legend.position="bottom",
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10, vjust = 2.5),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 10),
        strip.text = element_text(size = 12),
        panel.background = element_rect(fill = "grey96"),
        panel.grid = element_line(size = 0.5, 
                                  linetype = "solid", 
                                  colour = "white"))
plotSW3

gSW <- ggplot_gtable(ggplot_build(plotSW3))
strip_both <- which(grepl('strip-', gSW$layout$name))
fills <- c("#e7d4e8", "#af8dc3")
k <- 1
for (i in strip_both) {
  j <- which(grepl('rect', gSW$grobs[[i]]$grobs[[1]]$childrenOrder))
  gSW$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(gSW)

ggsave("seedGermplot.jpeg", plot=gSW, width = 7, 
       height = 4, device = 'tiff', dpi = 300)


#Full 5-way interaction model, no used in the manuscript

GermSeedBio <- glmer(cbind(Nr_germinated, GermFail) ~ 
                       Geography * Phase * 
                       Field_treatment * Lab_treatment * SW  + 
                       (1|Site/Plot/ID), 
                 data = SeedGerm, family = binomial,
                 control=glmerControl(optCtrl=list(maxfun=1e7)))
Anova(GermSeedBio)


#Creating prediction line for the figure

SeedGerm$preddi <- predict(GermSeedBio)


#Testing the residuals of effect of seed mass on the germination percentage

#Fit the initial model and extract the residuals

initial_model <- glmer(cbind(Nr_germinated, GermFail) ~ 
                         SW + (1 | Site/Plot/ID), 
                       data = SeedGerm, 
                       family = binomial)


#Adding the residuals to the dataset

residualsGS_df <- data.frame(SeedGerm, residuals = residuals(initial_model))


#Fit the model with the four-way interaction using residualsGS_df

four_way_model <- lmer(residuals ~ Geography * Phase * Field_treatment * Lab_treatment + 
                          (1 | Site/Plot/ID), data = residualsGS_df)


#Testing the four-way model using Anova

Anova(four_way_model, type = "II")


##The effect of seed mass on the root:shoot

#Combining the dataset created from SRL.R (CalGermST) 
#and the SeedWeight.R (Seed.df)

total <- merge(CalGermST, Seed.df, by=c("ID","Plot"))


#Removing replicates and NAs

total <- unique(total)
total <- drop_na(total, RootShootRatio)


#Re-leveling for the right comparison

total$Field_treatment <- relevel(factor(total$Field_treatment.x), 
                                 ref = "Control")

total$Phase <- relevel(factor(total$Phase.x), 
                       ref = "Young")

total$Lab_treatment <- relevel(factor(total$Lab_treatment), 
                               ref = "WP1")


#Re-naming some of the coloumns

total <- rename(total, 
                Geography = Geography.x,
                Site = Site.x)



#The full 5-way interaction model

SWSRmodel <- lmer(logRSR 
                  ~Geography*Phase*Field_treatment*Lab_treatment*SW + 
                    (1|Site/Plot/ID), data = total)

anoSWSR <- Anova(SWSRmodel)


#Creating prediction line for the figure

total$prediction <- predict(SWSRmodel)



#Testing the residuals of effect of seed mass on root:shoot

#Fit the initial model and extract the residuals
initial_modelSWSR <- lmer(logRSR ~ 
                         SW + (1 | Site/Plot/ID), 
                       data = total)

Anova(initial_modelSWSR)


#Adding the residuals to the dataset

residualsSWSR_df <- data.frame(total, residuals = residuals(initial_modelSWSR))


#Fit the model with the four-way interaction using residuals_df

four_way_modelSWSR <- lmer(residuals ~ Geography * Phase * Field_treatment * Lab_treatment 
                       +(1 | Site/Plot/ID), data = residualsSWSR_df)

Anova(four_way_modelSWSR, type = "II")


#Making the figure

plotrsrsw <- total %>% 
  mutate(Field_treatment = fct_relevel(Field_treatment, 
                                       "Control", 
                                       "50cover", 
                                       "90cover")) %>%
  ggplot() +
  facet_grid(Field_treatment~Geography,
             labeller = labeller(Field_treatment = new_labelsFT,
                                 Geography = new_labelsG)) +
  geom_point(aes(x=SW, y=log10(RootShootRatio), 
                 color=Lab_treatment)) +
  geom_smooth(method="lm", 
              aes(y=prediction, x=SW, color=Lab_treatment),
              size=1, alpha=0.2) +
  scale_color_manual(labels= FT, limits=c("WP1", 
                                          "WP2", 
                                          "WP3"), 
                     values = c("#969696", 
                                "#636363", 
                                "#252525")) +
  labs(color="", x="Seed weight (mg)", 
       y="Root:Shoot (log)") +
  theme(legend.position="bottom",
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10, vjust = 2.5),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 10),
        strip.text = element_text(size = 12),
        panel.background = element_rect(fill = "grey96"),
        panel.grid = element_line(size = 0.5, 
                                  linetype = "solid", 
                                  colour = "white"))
plotrsrsw

grsrSW <- ggplot_gtable(ggplot_build(plotrsrsw))
strip_both <- which(grepl('strip-', grsrSW$layout$name))
fills <- c("#e7d4e8", "#af8dc3", "#4575b4", "#74add1", "#abd9e9")
k <- 1
for (i in strip_both) {
  j <- which(grepl('rect', grsrSW$grobs[[i]]$grobs[[1]]$childrenOrder))
  grsrSW$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(grsrSW)

ggsave("GRSRplot.jpeg", plot=grsrSW, width = 7, 
       height = 8, 
       device = 'tiff', dpi = 300)

