setwd("C:/Users/tanus/Google Drive/MSSc Peace and Conflict Research/Master Thesis/Thesis Data")
load("data.Rda")


# MAPS

library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(viridis)
library(ggplot2)
library(dplyr)

world <- ne_countries(scale = 50, returnclass = "sf")


world <- world %>%
    full_join(data, by=c("iso_a3" = "iso3c"))

worldaid <- world %>%
    group_by(iso_a3) %>%
    mutate(aid_ag_pcap_avg = mean(aid_ag_pcap)) %>%
    ungroup()

world$aid_ag_pcapC <- base::cut(worldaid$aid_ag_pcap_avg,
                            breaks = c(0, 0.05, 0.10, 0.20, 0.50, 1, 
                                       2, 5, 10, 80), 
                            labels = 1:9, right = F, ordered_result = T)


library(RColorBrewer)

# create map - aid
aidmap <- ggplot(data = worldaid) +
    geom_sf(aes(fill = aid_ag_pcapC), stroke=0.01) +
    coord_sf(ylim = c(-55, 85), xlim=c(-165, 165)) +
    scale_fill_brewer(palette = "YlGnBu",
                      guide="legend",
                      na.value="white",
                      name="Aid Per Capita ($USD)",
                      labels=c("0.01-0.05", "0.06-0.10", "0.11-0.20", "0.21-0.50", "0.51-1.00", "1.01-2.00", "2.01-5.00", "5.01-10.00", ">10.00")
                      )+
    labs(title="Agricultural Aid Received Per Capita, 2002-2016 Average",
         caption = "Values constant at 2018 prices") +
    theme(
        panel.grid=element_blank(),
        panel.border = element_rect(colour = "dark grey", fill=NA, size=0.2),
        axis.title=element_blank(),
        axis.ticks=element_line(),
        axis.text=element_text(),
        panel.background = element_rect(fill = 'aliceblue'),
        plot.caption = element_text(hjust = 0.5, colour = "dark grey"),
        legend.direction = "vertical",
        legend.position = "right",
        legend.box = "horizontal",
        legend.key=element_blank(),
        legend.title = element_text(),
        legend.spacing = unit(0.4, "cm"),
        legend.box.spacing = unit(1, "cm"),
        legend.margin = margin(c(0, 10, 10, 0)),
        legend.text = element_text(margin = margin(r = 20, unit = "pt"))
        )

# display map
aidmap

ggsave("aidmap.png")


# create map - undernourishment

world$undernourish_3yr <- as.numeric(world$undernourish_3yr)

worldfs <- world %>%
    group_by(iso_a3) %>%
    mutate(undernourish_3yr_avg = mean(undernourish_3yr)) %>%
    ungroup()


fsmap <- ggplot(data = worldfs) +
    geom_sf(aes(fill = undernourish_3yr_avg), stroke=0.01) +
    coord_sf(ylim = c(-55, 85), xlim=c(-165, 165)) +
    scale_fill_viridis(option = "inferno",
                       direction=-1,
                      na.value="white",
                      name="Percent (%)"
                      )+
    labs(title="Prevalence of Undernourishment (%), 2002-2016 Average",
         caption = "Value expressed as percentage of total population") +
    theme(
        panel.grid=element_blank(),
        panel.border = element_rect(colour = "dark grey", fill=NA, size=0.2),
        axis.title=element_blank(),
        axis.ticks=element_line(),
        axis.text=element_text(),
        panel.background = element_rect(fill = 'aliceblue'),
        plot.caption = element_text(hjust = 0.5, colour = "dark grey"),
        legend.direction = "vertical",
        legend.position = "right",
        legend.box = "horizontal",
        legend.key=element_blank(),
        legend.title = element_text(),
        legend.spacing = unit(0.4, "cm"),
        legend.box.spacing = unit(1, "cm"),
        legend.margin = margin(c(0, 10, 10, 0)),
        legend.text = element_text(margin = margin(r = 20, unit = "pt"))
    )


fsmap



ggsave("fsmap.png")



# plots

data$peaceyears_cat <- as.character(data$peaceyears_cat)



library(stringr)
data$peaceyears_cat <- str_replace(data$peaceyears_cat, "1", "conflict")
data$peaceyears_cat <- str_replace(data$peaceyears_cat, "2", "post-conflict")
data$peaceyears_cat <- str_replace(data$peaceyears_cat, "3", "stable")



aidfsplot <- ggplot(data,
                      aes(x = aid_ag_pcap_c2018_log, 
                          y = lundernourish_3yr,
                          fill=peaceyears_cat
                          )
                    ) +
    scale_fill_brewer(palette="Set3", 
                       name="Peace Status",
                       guide="legend") +
    geom_point(shape=21, stroke=0.01, color="black", alpha = 0.9, size=2.5) +
    geom_smooth(aes(fill=NA), method=lm, col="black", show.legend = FALSE) +
    labs(title="Agricultural Aid and the Prevalence of Undernourishment, 2002-2017",
         legend.title="Peace Status",
         caption = "Undernourishment expressed with 1-year lead time.
         Agricultural aid expressed as USD per capita, log-transformed.",
         x="Agricultural Aid Received Per Capita ($)",
         y="Prevalence of Undernourishment (%)") +
    guides(fill=guide_legend(override.aes = list(size=5))) +
    theme(
        plot.caption = element_text(hjust = 0.5, colour = "dark grey"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.background = element_rect(fill = "white", color = NA), 
        panel.grid=element_line(color="grey"),
        legend.direction = "vertical",
        legend.position = "right",
        legend.box = "horizontal",
        legend.key=element_blank(),
        legend.title = element_text(),
        legend.spacing = unit(0.4, "cm"),
        legend.box.spacing = unit(1, "cm"),
        legend.margin = margin(c(0, 10, 10, 0)),
        legend.text = element_text(margin = margin(r = 20, unit = "pt"))
    )


aidfsplot

ggsave("aidfsplot.png")

# same plot for countries in conflict

conflict <- world %>%
    filter(peaceyears_cat=="conflict")


aidfsconflictplot <- ggplot(conflict,
                    aes(x = aid_ag_pcap_c2018_log, 
                        y = lundernourish_3yr,
                        fill=economy
                    )
                    ) +
    scale_fill_brewer(palette="Paired", 
                      name="GDP per capita",
                      guide="legend",
                      labels=c("Emerging region: BRIC", "Emerging region: MIKT", "Emerging region: G20", "Developing region", "Least developed region")) +
    geom_point(shape=21, col="black", stroke=0.01, alpha = 0.9, size=3) +
    geom_smooth(aes(fill=NA), method=lm, col="black", show.legend = FALSE) +
    labs(title="Agricultural Aid and Undernourishment in Conflict Countries, 2002-2017",
         caption = "Undernourishment expressed as a percentage of the population, with 1-year lead time.
         Agricultural aid expressed as USD per capita, log-transformed.",
         x="Agricultural Aid Received Per Capita ($)",
         y="Prevalence of Undernourishment (%)") +
    guides(fill=guide_legend(override.aes = list(size=5))) +
    theme(
        plot.caption = element_text(hjust = 0.5, colour = "dark grey"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.background = element_rect(fill = "white", color = NA), 
        panel.grid=element_line(color="grey"),
        legend.direction = "vertical",
        legend.position = "right",
        legend.box = "horizontal",
        legend.key=element_blank(),
        legend.title = element_text(),
        legend.spacing = unit(0.4, "cm"),
        legend.box.spacing = unit(1, "cm"),
        legend.margin = margin(c(0, 10, 10, 0)),
        legend.text = element_text(margin = margin(r = 20, unit = "pt"))
    )



aidfsconflictplot

ggsave("aidfs_conflict.png")



# includes regression lines in multiple categories

aidfscatplot <- ggplot(data,
                    aes(x = aid_ag_pcap_c2018_log, 
                        y = lundernourish_3yr,
                        fill=peaceyears_cat,
                    )
) +
    geom_point(shape=21, stroke=0.3, color="dark grey", alpha = 0.8, size=3) +
    scale_fill_brewer(palette="Paired", 
                      name="Peace Status",
                      guide="legend") +
    geom_smooth(method=lm, col="black", show.legend = FALSE) +
    labs(title="Agricultural Aid and the Prevalence of Undernourishment, 2002-2017",
         caption = "Undernourishment expressed with 1-year lead time.
         Agricultural aid expressed as USD per capita, log-transformed.",
         x="Agricultural Aid Received Per Capita ($)",
         y="Prevalence of Undernourishment (%)")



aidfscatplot
