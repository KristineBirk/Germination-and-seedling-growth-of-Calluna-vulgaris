#Downloading the data 

CalGermSeed1 <- excel_sheets(
  "~/Master/Dataset/CallunaGermination.xlsx") %>% 
  purrr::map_df(~ read_excel("~/Master/Dataset/CallunaGermination.xlsx", 
                             sheet = "SeedWeight"))


#Removing unneccessary coloumns

Seed.df = subset(CalGermSeed1, 
                 select = -c(Year, Month, Day, Seed_weight, Comments))

#Making the seed weight (SW) mg and not g.

Seed.df <- Seed.df %>% 
  mutate(SW = SW*1000)


#re-leveling for the right comparison

Seed.df$Field_treatment <- relevel(factor(Seed.df$Field_treatment), 
                                   ref = "control")
Seed.df$Phase <- relevel(factor(Seed.df$Phase), ref = "Young")


#Checking the distrubution
vekt <- Seed.df$SW
hist(vekt) #Normal distribution


#Re-naming the variables

FT <- c('control'="Control", 
        '50cover'="60% cover", 
        '90cover'="90% cover")
SS <- c('Young' = "Pioneer", 
        'Intermediate' = "Building",
        'Old' = "Mature")
P <- c("Pioneer", "Building", "Mature")
G <- c('N' = "North",
       'S' = "South")

#Removing NAs from the dataset

Seed.df <- drop_na(Seed.df)


#Creating the figure

SWplot <- Seed.df %>%
  mutate(Field_treatment = fct_relevel(Field_treatment, 
                                       "control", 
                                       "50cover", 
                                       "90cover")) %>%
  ggplot(aes(Phase, SW, fill = Field_treatment)) +
  geom_boxplot()+
 facet_wrap(.~Geography, 
            labeller = as_labeller(G)) +
  scale_x_discrete(labels= SS, 
                   limits=c("Young", "Intermediate", "Old")) +
  scale_fill_manual(labels= FT, 
                    limits=c("control", "50cover", "90cover"), 
                    values = c("#4575b4", "#74add1", "#abd9e9")) +
  labs(fill="", x="", 
       y="Seed weight (mg)") +
  ylim(0, 0.06)+
  theme(legend.position="bottom", 
        axis.title.y = element_text(size = 10, vjust = 2.5),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 10),
        strip.text = element_text(size = 12),
        panel.background = element_rect(fill = "grey96"),
        panel.grid = element_line(size = 0.5, 
                                  linetype = "solid", colour = "white"))


g3 <- ggplot_gtable(ggplot_build(SWplot))
strip_both <- which(grepl('strip-', g3$layout$name))
fills <- c("#e7d4e8", "#af8dc3")
k <- 1
for (i in strip_both) {
  j <- which(grepl('rect', g3$grobs[[i]]$grobs[[1]]$childrenOrder))
  g3$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g3)


#Saving the plot as a .jpeg

ggsave("seedplot.jpeg", 
       plot=g3, 
       width = 7, 
       height = 4, 
       device = 'tiff', 
       dpi = 300)


#The linear mixed-effects model

SWmodel1 <- lme4::lmer(SW ~Geography*Phase*Field_treatment + 
                  (1|Site/Plot/ID), data = Seed.df)


#Using Anova to test the model
SWA <- Anova(SWmodel1)
SWA
summary(SWmodel1)

#Testing the difference between the northern and southern seeds, 
#post-hoc using emmeans

emmSWM <- emmeans(SWmodel1, ~ Geography)
pairwise <- pairs(emmSWM)
summary(pairwise)


#Testing the difference between the successional stages seeds, 
#post-hoc using emmeans
emmSWM <- emmeans(SWmodel1, ~ Phase)
pairwise <- pairs(emmSWM)
summary(pairwise)


#Testing the three-way interaction for the full model. 
emmSW <- emmeans(SWmodel1, ~ Geography:Phase:Field_treatment)


#Compare all levels of Field_treatment within each combination of Geography and Phase
comparison <- pairs(emmSW, by = c("Geography", "Phase"))
summary(comparison)


#Weight

#Calculate average weight and standard deviation for Northern seeds
north_stats <- aggregate(SW ~ Geography, data = Seed.df, subset = Geography == "N",
                         FUN = function(x) c(avg_weight = mean(x), sd_weight = sd(x)))

#Calculate average weight and standard deviation for Southern seeds
south_stats <- aggregate(SW ~ Geography, data = Seed.df, subset = Geography == "S",
                         FUN = function(x) c(avg_weight = mean(x), sd_weight = sd(x)))

#Print the results
north_stats
south_stats
