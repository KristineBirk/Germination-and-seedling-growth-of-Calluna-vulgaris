##SLA


#Downloading the data for the SLA

sla.df <- excel_sheets("~/Master/Dataset/CallunaGermination.xlsx") %>% 
  purrr::map_df(~ read_excel("~/Master/Dataset/CallunaGermination.xlsx", 
                             sheet = "SLA"))


#Combine SLA and other traits 

SLA.d <- merge(CalGermST, sla.df, by=c("ID","Plot","Seed"))

SLA.d <- SLA.d %>% 
  group_by(Plot, ID, Seed) 


#Remove replicates

SLA.d <- unique(SLA.d)


#Calculate the SLA

SLA.d <- SLA.d %>%
  mutate(sla = Area/leaf_DW)


#Checking the distrubution and log transform 

slasjekk <- SLA.d$sla
hist(log(slasjekk))
SLA.d$SLAlog <- log10(slasjekk)


#Re-naming the the coloumns

SLA.d <- rename(SLA.d, Lab_treatment = Lab_treatment.x,
                Field_treatment = Field_treatment.x,
                Geography = Geography.x,
                Phase = Phase.x, 
                Site = Site.x)


#Creating the figure

plotSLA <- SLA.d %>% 
  ggplot(aes(Lab_treatment, SLAlog, fill=Field_treatment)) +
  geom_boxplot()+
  theme_bw()+
  facet_grid(Phase~Geography,
             labeller = labeller(Phase = new_labelsP,
                                 Geography = new_labelsG)) +
  scale_fill_manual(labels= FT, 
                    breaks = c("Control", "50cover", "90cover"),
                    values = c("#4575b4", "#74add1", "#abd9e9")) +
  labs(fill="", x="", y="SLA (log)") +
  theme(legend.position="bottom") +
  theme(legend.position="bottom", 
        axis.title.y = element_text(size = 10, vjust = 2.5),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 10),
        strip.text = element_text(size = 12),
        panel.background = element_rect(fill = "grey96"),
        panel.grid = element_line(size = 0.5, 
                                  linetype = "solid", 
                                  colour = "white"))
plotSLA

SLA.p <- ggplot_gtable(ggplot_build(plotSLA))
strip_both <- which(grepl('strip-', SLA.p$layout$name))
fills <- c("#e7d4e8", "#af8dc3", "#d9f0d3", "#7fbf7b", "#1b7837")
k <- 1
for (i in strip_both) {
  j <- which(grepl('rect', SLA.p$grobs[[i]]$grobs[[1]]$childrenOrder))
  SLA.p$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k+1
}
grid.draw(SLA.p)

#Save the figure

ggsave("SLA.jpeg", 
       plot=SLA.p, 
       width = 7, 
       height = 8, 
       device = 'tiff', 
       dpi = 300)


#The linear mixed effect model for SLA

SLAmodel <- lmer(SLAlog ~ Geography*Phase*Field_treatment*Lab_treatment +
                   (1|Site/Plot/ID),
                 data = SLA.d)


#testing the model using Anova

anoSLA <- Anova(SLAmodel)
summary(SLAmodel)