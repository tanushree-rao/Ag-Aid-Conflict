setwd("C:/Users/tanus/Google Drive/MSSc Peace and Conflict Research/Master Thesis/Data")
load("data.Rda")


# MAPS

library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(viridis)
library(ggplot2)

world <- ne_countries(scale = 50, returnclass = "sf")

library(dplyr)

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
