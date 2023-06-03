##Time to 50% germination

#here we use the dataset created in CreateT50.R and 
#merging it with the germination dataset

T50 = germ2[germ2$Germinated > 0,]
DT50 <- subset(T50, select = -c(X, Seed_nr, 
                                   Year, Month, Day, Petri_dish, X))

T50.dataset <- read.csv("~/Master/Dataset/T50.dataset.csv", header = TRUE, sep = ";", 
                 stringsAsFactors = FALSE)

T50.dataset <- T50.dataset %>% 
  rename(PetriID = ï..PetriID)

T50.data <- merge(DT50, T50.dataset, by=c("PetriID"))


#Make the time to 50% germination numeric (T50 coloumn)

T50.data$T50 <- as.numeric(as.character(T50.data$T50))


T50.data <- T50.data %>% 
  group_by(PetriID) %>% 
  top_n(1, Days_since_start)

hist((T50.data$T50))
hist(log(T50.data$T50))
T50.data$T50log <- log10(T50.data$T50)


#Re-leveling the variables for the right comparison

T50.data$Field_treatment <- relevel(factor(T50.data$Field_treatment), 
                                    ref = "Control")

T50.data$Phase <- relevel(factor(T50.data$Phase), 
                          ref = "Young")

T50.data$Lab_treatment <- relevel(factor(T50.data$Lab_treatment), 
                                  ref = "WP1")


#Making the figure

T50.data$Phase <- factor(T50.data$Phase,
                               levels = c('Young','Intermediate', 'Old'),ordered = TRUE)

plotT50g <- T50.data %>% 
  mutate(Phase = fct_relevel(Phase, "Young", "Intermediate", "Old")) %>% 
  ggplot(aes(Lab_treatment, T50, fill=Field_treatment)) +
  geom_boxplot()+
  facet_grid(Phase~Geography,
             labeller = labeller(Phase = new_labelsP,
                                 Geography = new_labelsG)) +
  scale_fill_manual(labels= FT, 
                    breaks = c("Control", "50cover", "90cover"),
                    values = c("#4575b4", "#74add1", "#abd9e9")) +
  labs(fill="", x="", y="T50%") +
  theme(legend.position="bottom") +
  theme(legend.position="bottom", axis.title.y = element_text(size = 10, vjust = 2.5),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 10),
        strip.text = element_text(size = 12),
        panel.background = element_rect(fill = "grey96"),
        panel.grid = element_line(size = 0.5, linetype = "solid", colour = "white"))
plotT50g

g5 <- ggplot_gtable(ggplot_build(plotT50g))
strip_both <- which(grepl('strip-', g5$layout$name))
fills <- c("#e7d4e8", "#af8dc3", "#d9f0d3", "#7fbf7b", "#1b7837")
k <- 1
for (i in strip_both) {
  j <- which(grepl('rect', g5$grobs[[i]]$grobs[[1]]$childrenOrder))
  g5$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(g5)


#Saving the figure

ggsave("T50plot.jpeg", 
       plot=g5, 
       width = 7, 
       height = 8, 
       device = 'tiff', 
       dpi = 300)

#model
T50model <- lmer(T50 ~ Geography*Phase*Field_treatment*Lab_treatment
                 + (1|Site/Plot/ID), 
                 data = T50.data)


anoT50 <- Anova(T50model, type = "II")
anoT50
summary(T50model)

#Testing between north and south 
emmT50P <- emmeans(T50model, ~ Geography:Lab_treatment)
comparisonGSS <- pairs(emmT50P, by = "Lab_treatment")
summary(comparisonGSS)


#Testing between phases
emmT50SS <- emmeans(T50model, ~ Phase:Lab_treatment)
comparisonT50SS <- pairs(emmT50SS, by = "Phase")
summary(comparisonT50SS)

#Testing between maternal drought
emmT50MDT <- emmeans(T50model, ~ Field_treatment:Lab_treatment)
comparisonT50MDT <- pairs(emmT50MDT, by = "Field_treatment")
summary(comparisonT50MDT)

emmT50SSMDT <- emmeans(T50model, ~ Phase:Field_treatment:Lab_treatment)
comparisonT50SSMDT <- pairs(emmT50SSMDT, by = "each")
summary(comparisonT50SSMDT)


emGermT50 <- emmeans(T50model, 
                      ~Geography*Phase*Field_treatment*Lab_treatment)
pairs(emGermT50, simple = "each")




#Finding the average time to 50% germination for each water potential

WP1m <- subset(T50.data, Lab_treatment!="WP2")
WP1m <- subset(WP1m, Lab_treatment!="WP3")  
WP1m <- subset(WP1m, Lab_treatment!="WP4") 
WP1m <- subset(WP1m, Lab_treatment!="WP5") 

mean(WP1m$T50) #19.68624

WP2m <- subset(T50.data, Lab_treatment!="WP1")
WP2m <- subset(WP2m, Lab_treatment!="WP3")  
WP2m <- subset(WP2m, Lab_treatment!="WP4") 
WP2m <- subset(WP2m, Lab_treatment!="WP5") 

mean(WP2m$T50)  #27.16552

WP3m <- subset(T50.data, Lab_treatment!="WP1")
WP3m <- subset(WP3m, Lab_treatment!="WP2")  
WP3m <- subset(WP3m, Lab_treatment!="WP4") 
WP3m <- subset(WP3m, Lab_treatment!="WP5") 
mean(WP3m$T50)  #45.32246

WP4m <- subset(T50.data, Lab_treatment!="WP1")
WP4m <- subset(WP4m, Lab_treatment!="WP3")  
WP4m <- subset(WP4m, Lab_treatment!="WP2") 
WP4m <- subset(WP4m, Lab_treatment!="WP5") 
mean(WP4m$T50) #53.8125

WP5m <- subset(T50.data, Lab_treatment!="WP1")
WP5m <- subset(WP5m, Lab_treatment!="WP2")  
WP5m <- subset(WP5m, Lab_treatment!="WP4") 
WP5m <- subset(WP5m, Lab_treatment!="WP3") 
mean(WP5m$T50) #19.8333


#Finding time to 50% germination for the northern and southern site

GNm <- subset(T50.data, Geography!="S")  

mean(GNm$T50)  #26.86035

GSm <- subset(T50.data, Geography!="N")  

mean(GSm$T50) #23.07698


