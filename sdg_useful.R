library(ggplot2)

sdg_colours <- c("#E61E2E","#D79D2F", "#209F49", "#BA2430", "#EF402C", "#00AEDA",
                 "#FFB715", "#901938", "#F36D24", "#E01585", "#F99E24", "#D08D2D",
                 "#48783F", "#017DBE", "#3EB04A", "#02558B", "#183768")
names(sdg_colours) <- 1:17
sdg_fill_scale <- scale_fill_manual(name = "goal",values = sdg_colours)
sdg_col_scale <- scale_color_manual(name = "goal",values = sdg_colours)


pillar_colours <- c("#d95f02", "#7570b3", "#1b9e77")
names(pillar_colours) <- c("Economy", "Society", "Biosphere")
pillar_fill_scale <- scale_fill_manual(name = "pillar",values = pillar_colours)
pillar_col_scale <- scale_color_manual(name = "pillar",values = pillar_colours)


tier_colours <- c("#fef0d9", "#fdd49e", "#fdbb84", "#fc8d59", "#e34a33", "#b30000")
names(tier_colours) <- c("1", "1/2", "1/3", "2", "2/3", "3")
tier_fill_scale <- scale_fill_manual(name = "tier",values = tier_colours)
tier_col_scale <- scale_color_manual(name = "tier",values = tier_colours)



theme_fiona <- function () { 
  theme_minimal(base_size=11) %+replace% 
    theme(
      panel.background  = element_blank(),
      #panel.border = element_rect(colour = "black", fill = NA, size = 0.2),
      axis.text=element_text(size=12),
      axis.title=element_text(size=14,face="bold"),
      strip.text.x = element_text(size = 12),
      plot.background = element_rect(fill="transparent", colour=NA), 
      legend.background = element_rect(fill="transparent", colour=NA),
      legend.key = element_rect(fill="transparent", colour=NA)
    )
}



