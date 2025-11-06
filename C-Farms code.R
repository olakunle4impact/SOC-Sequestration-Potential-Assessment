### Working directory #####
setwd(dir = "C:/Users/oogunyemi/Documents/Tuscia University/C-Farms/Datasets")

area_stat <- read.csv("Area_statistics.csv")
names(area_stat) <- c("Strata","Ha")
library(dplyr)

## Barplot for the Total Count on Strata Basis
library(ggplot2)
ar <- ggplot(area_stat,
       aes(x = Ha, y = Strata, fill = Strata)) +
  geom_col() +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05)))+
  labs(x = "Area Covered (Ha)",
       y = "Strata") +
  theme_bw()+
  theme(panel.grid.major.y = element_blank(), legend.position = "off",
        axis.text.x = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 6, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ggsave("Area_Covered_land.png", plot = ar,
       width = 12, height = 6, dpi = 700)


##################### BOXPLOT #########################################
dat <- read.csv("Data for Section 2.csv")
names(dat) <- c("Strata","SOC")
str(dat)
dat$Strata <- as.factor(dat$Strata)
attach(dat)

dat$Crop_Types <- ifelse(
  Strata == "1_1_1"|Strata == "2_1_1"|Strata == "2_2_1"|
    Strata == "2_3_1"|Strata == "2_4_1"|Strata == "3_1_1"| 
    Strata == "3_2_1"| Strata == "3_3_1"| 
    Strata == "3_4_1", "Permanent Crops", ifelse(Strata == "2_1_2"|
    Strata == "2_2_2"|Strata == "2_4_2"|Strata == "3_1_2"|
    Strata == "3_2_2"|Strata == "3_3_2"|Strata == "3_4_2","Poplar", 
    ifelse(Strata == "1_1_3"|Strata == "1_2_3"|Strata == "1_3_3"|
    Strata == "2_1_3"|Strata == "2_2_3"|Strata == "2_3_3"|
    Strata == "2_4_3"|Strata == "3_1_3"|Strata == "3_2_3"|
    Strata == "3_3_3"|Strata == "3_4_3", "Grasslands", 
    ifelse(Strata == "2_1_4"|Strata == "2_2_4"|Strata == "2_4_4"|
    Strata == "3_1_4"|Strata == "3_2_4"|Strata == "3_3_4"|
    Strata == "3_4_4", "Rice","Annual Croplands"))))


library(ggpubr)
gen_bp <- ggboxplot(dat, x = "Strata", y = "SOC", fill = "Crop_Types", 
          palette = c("brown", "pink", "yellow","blue","green"), 
          font.label = list(size = 30, color = "black"), 
          ggtheme = theme_bw()) + theme(legend.position = "right", 
          legend.text = element_text(size = 30), 
          legend.key.size = unit(2, "cm"),legend.key.width = unit(1.5,"cm"),
          panel.spacing=unit(1, "lines"), text = element_text(size=35),
          axis.text.x = element_text(angle=90, hjust=.5, vjust = .3))+
          labs(x = "Strata",y = "SOC (Mg ha-1)")
          # caption = "Project: Life GIS_FARMS")
gen_bp

gen_bp <- ggboxplot(dat, x = "Strata", y = "SOC", fill = "Crop_Types", 
          palette = c("brown", "pink", "yellow","blue","green"), 
          font.label = list(size = 28, color = "black"), 
          ggtheme = theme_bw()) + theme(legend.position = "right", 
          legend.text = element_text(size = 23), 
          legend.key.size = unit(0.8, "cm"),legend.key.width = unit(1,"cm"),
          panel.spacing=unit(1, "lines"), text = element_text(size=28),
          axis.text.x = element_text(size = 12, angle=90, hjust=.5, vjust = .3)) +
          labs(x = "Strata",y = "SOC (Mg ha-1)")
# caption = "Project: Life GIS_FARMS")
gen_bp

ggsave("All_strata_boxplot.png", plot = gen_bp,
       width = 12, height = 6, dpi = 700)

## Facted by crop types
library(ggpubr)
library(ggplot2)

gen_bp <- ggboxplot(dat, x = "Strata", y = "SOC", fill = "Crop_Types", 
              palette = c("brown", "pink", "yellow", "blue", "green"), 
              font.label = list(size = 12, color = "black"), 
              ggtheme = theme_bw()) +
              facet_wrap(~ Crop_Types, ncol = 3) +  
              theme(legend.position = "none",  
              legend.text = element_text(size = 18), 
              panel.spacing = unit(1, "lines"), 
              text = element_text(size = 12),
              axis.text.x = element_text(size = 5, angle = 90, 
                                         hjust = 0.5, vjust = 0.3, face = "bold")) +
              labs(x = "", y = "SOC (Mg/ha)")

gen_bp
ggsave("All_strata_boxplot_facet.png", plot = gen_bp,
       width = 12, height = 6, dpi = 700)
                               
gen_bp <- gen_bp + ylab("SOC (Mg ha⁻¹)")

gen_bp <- ggboxplot(dat, x = "Strata", y = "SOC", fill = "Crop_Types", 
                    palette = c("brown", "pink", "yellow", "blue", "green"), 
                    font.label = list(size = 28, color = "black"), 
                    ggtheme = theme_bw()) +
  facet_wrap(~ Crop_Types, ncol = 3) +  
  theme(legend.position = "none",  
        legend.text = element_text(size = 23), 
        panel.spacing = unit(1, "lines"), 
        text = element_text(size = 28),
        axis.text.x = element_text(size = 10, angle = 90, 
                                   hjust = 0.5, vjust = 0.3, face = "bold"),
        plot.margin = margin(10, 10, 10, 30)) +
  coord_cartesian(clip = "off") +
  labs(x = "", y = "SOC (Mg ha⁻¹)")

gen_bp



### Hypothesis test
# Perform ANOVA with Land use
dat$CTypes <- ifelse(dat$Crop_Types=="Permanent Crops","PC",
                ifelse(dat$Crop_Types=="Poplar","PO",
                ifelse(dat$Crop_Types=="Grasslands","GL",
                ifelse(dat$Crop_Types=="Rice","RI","AC"))))

anova_result <- aov(SOC ~ CTypes, data = dat)
summary(anova_result)
str(dat)
# Perform Tukey HSD post-hoc test for Land use
posthoc_result <- TukeyHSD(anova_result)
print(posthoc_result)
plot(posthoc_result, las = 1)
## last for the orientation of the x labels

dat %>% group_by(CTypes) %>% summary()

library(ggplot2)
library(dplyr)
library(multcompView)

## compact letter display
cld <- multcompLetters4(anova_result, posthoc_result)
print(cld)

str(dat)
dat$CTypes <- as.factor(dat$CTypes)

## table with factors and 3rd quantile
tk <- group_by(dat, CTypes) %>% 
  summarise(mean = mean(SOC), quant = quantile(SOC,
                                probs = 0.75)) %>%
  arrange(desc(mean))

## Extracting the compact letter display and adding to the Tk table
cnd <- as.data.frame.list(cld$CTypes)
tk$cnd <- cnd$Letters
print(tk)

## This code can be used for a scientific purposes
ggplot(dat, aes(CTypes, SOC)) +
  geom_boxplot(fill ="lightblue", color ="darkblue") +
  labs(x = "Crop Types", y = "SOC (Mg/ha)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_text(data = tk, aes(label = cnd, x= CTypes, y = quant),
            vjust = -1, hjust = -1, size = 3)

ggsave("boxplot.png", width = 5.5, height = 3, dpi = 1000)


## This code can be used for a scientific purposes
ggplot(dat, aes(CTypes, SOC)) +
  geom_boxplot() +
  labs(x = "Crop Types", y = "SOC (Mg/ha)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_text(data = tk, aes(label = cnd, x= CTypes, y = quant),
            vjust = -1, hjust = -1, size = 3)


### Let us add some colours to it
ggplot(dat, aes(CTypes, SOC)) +
  geom_boxplot(fill ="lightblue", color ="darkblue") +
  labs(x = "Crop Types", y = "SOC (Mg/ha)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_text(data = tk, aes(label = cnd, x= CTypes, y = quant),
            vjust = -1, hjust = -1, size = 3)

ggsave("boxplot.png", width = 5.5, height = 3, dpi = 1000)

### Let's colour according to the median value
a <- ggplot(dat, aes(CTypes, SOC)) +
  geom_boxplot(aes(fill = factor(..middle..))) +
  labs(x = "Crop Types", y = "SOC (Mg/ha)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_text(data = tk, aes(label = cnd, x= CTypes, y = quant),
            vjust = -1, hjust = -1, size = 3) +
  scale_fill_brewer(palette = "Blues", name = "Median SOC")
## the name argument in the scale_fill_brewer is used in changing the legend name.
ggsave("boxplot.png", width = 5.5, height = 3, dpi = 1000)

library(biwavelet)
library(gridGraphics)
library(cowplot)

plot(posthoc_result, las = 1)
b <- recordPlot()

plot_grid(b,a, nrow = 1, ncol = 2, align = "h")
ggsave("boxplot1.png", width = 10, height = 5, dpi = 1000)


## To remove the legend, we will use the show.legend argument
ggplot(dat, aes(CTypes, SOC)) +
  geom_boxplot(aes(fill = factor(..middle..)), show.legend =F) +
  labs(x = "Crop Types", y = "SOC (Mg/ha)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_text(data = tk, aes(label = cnd, x= CTypes, y = quant),
            vjust = -1, hjust = -1, size = 3) +
  scale_fill_brewer(palette = "Blues", name = "Median SOC")


##### Sequestration Potential #####
seq <- read.csv("Dominant_Strata_Data.csv")
head(seq)

gen_bp2 <- ggboxplot(seq, x = "Dom_strata", y = "Acc_pot", 
                     fill =  font.label = list(size = 9, color = "black"), ggtheme = theme_bw()) + theme(legend.position = "right", text = element_text(size=7),axis.text.x = element_text(angle=90, hjust=.5, vjust = .3))+labs(x = "Dominant Strata",y = "SOC Sequestration Potential (Mg/ha)")
print(gen_bp2)

### Legend by Texture
seq$texture_code <- sub(".*_", "", seq$Dom_strata)


gen_bp2 <- ggboxplot(seq, 
                     x = "Dom_strata", 
                     y = "Acc_pot", 
                     fill = "texture_code",     
                     palette = "lancet",    
                     font.label = list(size = 9, color = "black"), 
                     ggtheme = theme_bw()) + 
  theme(
    legend.position = "right", legend.text = element_text(size = 23), 
    legend.key.size = unit(0.8, "cm"),legend.key.width = unit(1,"cm"),
    panel.spacing=unit(1, "lines"), text = element_text(size=28),
    axis.text.x = element_text(size = 12, angle=90, hjust=.5, vjust = .3)
  ) +
  labs(
    x = "Dominant Strata",
    y = "SOC Sequestration Potential (Mg/ha)",
    fill = "Soil Texture" 
  )

print(gen_bp2)


seq$texture_code <- dplyr::recode(seq$texture_code,
                                       "1" = "Clay Loam",
                                       "2" = "Loam",
                                       "3" = "Sandy Loam",
                                       "4" = "Silt Loam")


gen_bp2 <- ggboxplot(seq, 
                     x = "Dom_strata", 
                     y = "Acc_pot", 
                     fill = "texture_code",     
                     palette = "lancet",    
                     font.label = list(size = 9, color = "black"), 
                     ggtheme = theme_bw()) + 
  theme(
    legend.position = "right", legend.text = element_text(size = 15), 
    legend.key.size = unit(0.8, "cm"),legend.key.width = unit(1,"cm"),
    panel.spacing=unit(1, "lines"), text = element_text(size=18),
    axis.text.x = element_text(size = 10, angle=90, hjust=.5, vjust = .3, face = "bold")
  ) +
  labs(
    x = "Dominant Strata",
    y = "SOC Sequestration Potential (Mg/ha)",
    fill = "Soil Texture" 
  )

print(gen_bp2)

ggsave("Only12_strata_boxplot.png", plot = gen_bp2,
       width = 12, height = 6, dpi = 700)

### Post hoc
attach(seq)
str(seq)
max(seq$Acc_pot)
phoc_result <- aov(Acc_pot~Land_Use, data = seq)
summary(phoc_result)

# Perform Tukey HSD post-hoc test for Land use
posthoc_result <- TukeyHSD(phoc_result)
print(posthoc_result)
plot(posthoc_result, las = 1)
