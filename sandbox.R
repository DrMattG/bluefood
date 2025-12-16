library(tidyverse)
library(taylor) # colour palettes
library(ggtext)
#devtools::install_github("thomasp85/patchwork")
library(patchwork)
library(grid) # legend extraction
library(gridExtra) # legend extraction

# load data
data <- read.csv('data/data.csv')

# set overall colour palette limits
my_pal <- color_palette(c("#2c454a", "#577c84", "#a8bdbe", "#e2b8a2", "#ff9d78", "#e55634"))

#### species ####
# separate study species
species <- separate_rows(data, Study.species, sep = ", ")
# count species
species_count <- count(species, Study.species)
# set factor ordering
species_count$Study.species <- species_count$Study.species %>%
  as.factor() %>%
  factor(levels=c("Atlantic cod (Gadus morhua)",
                  "Gilthead sea bream (Sparus aurata)",
                  "Sea bass (Dicentrus labrax)",
                  "Atlantic salmon (Salmo salar)",
                  "Chinook salmon (Oncorhynchus tshawytscha)",
                  "Chum salmon (Oncorhynchus keta)",
                  "Coho salmon (Oncorhynchus kisutch)",
                  "Pink salmon (Oncorhynchus gorbuscha)",
                  "Salmon (unspecified)"))
# set palette
n_col <- nrow(species_count)
palette <- color_palette(my_pal, n = n_col)
species_count <- mutate(species_count, palette = as.vector(palette))
# stacked bar plot
species_plot <- ggplot(species_count,
  aes(fill = Study.species, y = n, x = 3)
) +
  geom_bar(stat = "identity") +
  theme_classic() +
  theme(
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x  = element_blank(),
    axis.title.y  = element_blank(),
    legend.text = element_markdown()
  ) +
  scale_fill_manual(
    values = species_count$palette) +
  ylab("Study count") 
species_plot_0 <- species_plot + 
  coord_flip() +
  guides(fill = "none") +
  scale_y_reverse() +
  scale_x_discrete(as.character(0:1))

species_plot1 <- species_plot +
  scale_fill_manual(
    values = species_count$palette[1:4],
    labels = function(x) {
      x %>%
        gsub("\\(", "\\(\\*", .) %>%
        gsub("\\)", "\\*)", .) %>%
        gsub("\\*unspecified\\*", "unspecified", .)
    },
    limits=c("Atlantic cod (Gadus morhua)",
             "Gilthead sea bream (Sparus aurata)",
             "Sea bass (Dicentrus labrax)",
             "Atlantic salmon (Salmo salar)")
  ) +
  guides(fill = guide_legend(title = "Study species"))

species_plot2 <- species_plot +
  scale_fill_manual(
    values = species_count$palette[5:9],
    labels = function(x) {
      x %>%
        gsub("\\(", "\\(\\*", .) %>%
        gsub("\\)", "\\*)", .) %>%
        gsub("\\*unspecified\\*", "unspecified", .)
    },
    limits=c("Chinook salmon (Oncorhynchus tshawytscha)",
             "Chum salmon (Oncorhynchus keta)",
             "Coho salmon (Oncorhynchus kisutch)",
             "Pink salmon (Oncorhynchus gorbuscha)",
             "Salmon (unspecified)")
  ) + 
  guides(fill = guide_legend(title = ""))

## Function to extract legend
g_legend <- function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  legend
} 

species_legend1 <- g_legend(species_plot1) # extract legend
species_legend2 <- g_legend(species_plot2) # extract legend
species_plots <- grid.arrange(species_plot_0, species_legend1, species_legend2, ncol=3)

#### close ####

#### production stage ####
# separate study species
production <- separate_rows(data, Production.stage, sep = ", ")
# count species
production_count <- count(production, Production.stage)
# set factor ordering
production_count$Production.stage <- production_count$Production.stage %>%
  as.factor() %>%
  factor(levels=c("Feed",
                  "Roe",
                  "Fry",
                  "Smolt",
                  "Adult",
                  "Processing",
                  "Not stated"))
# set palette
n_col <- nrow(production_count)
palette <- color_palette(my_pal, n = n_col)
production_count <- mutate(production_count, palette = as.vector(palette))
# stacked bar plot
production_plot <- ggplot(production_count,
                       aes(fill = Production.stage, y = n, x = 3)
) +
  geom_bar(stat = "identity") +
  theme_classic() +
  theme(
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x  = element_blank(),
    axis.title.y  = element_blank(),
    legend.text = element_markdown()
  ) +
  scale_fill_manual(
    values = production_count$palette) +
  ylab("Study count") 
production_plot_0 <- production_plot + 
  coord_flip() +
  guides(fill = "none") +
  scale_y_reverse() +
  scale_x_discrete(as.character(0:1))

production_plot1 <- production_plot +
  scale_fill_manual(
    values = production_count$palette[1:3],
    limits=c("Feed",
             "Roe",
             "Fry")
  ) +
  guides(fill = guide_legend(title = "Production stage"))

production_plot2 <- production_plot +
  scale_fill_manual(
    values = production_count$palette[4:7],
    limits=c("Smolt",
             "Adult",
             "Processing",
             "Not stated")
  ) + 
  guides(fill = guide_legend(title = ""))

## Function to extract legend
g_legend <- function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  legend
} 

production_legend1 <- g_legend(production_plot1) # extract legend
production_legend2 <- g_legend(production_plot2) # extract legend
production_plots <- grid.arrange(production_plot_0, production_legend1, production_legend2, ncol=3)

#### close ####

#### farming system ####
# separate study species
farming <- separate_rows(data, Farming.system.coding, sep = ", ")
# count species
farming_count <- count(farming, Farming.system.coding)
# set factor ordering
farming_count$Farming.system.coding <- farming_count$Farming.system.coding %>%
  as.factor() %>%
  factor(levels=c("Freshwater hatcheries",
                  "Offshore pens",
                  "Semi-closed pens",
                  "Processing plants",
                  "Integrated Multi-Trophic Aquaculture (IMTA)",
                  "Multi-Purpose Platforms (MPPs)",
                  "Recirculating Aquaculture System (RAS)"))
# set palette
n_col <- nrow(farming_count)
palette <- color_palette(my_pal, n = n_col)
farming_count <- mutate(farming_count, palette = as.vector(palette))
# stacked bar plot
farming_plot <- ggplot(farming_count,
                          aes(fill = Farming.system.coding, y = n, x = 3)
) +
  geom_bar(stat = "identity") +
  theme_classic() +
  theme(
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x  = element_blank(),
    axis.title.y  = element_blank(),
    legend.text = element_markdown()
  ) +
  scale_fill_manual(
    values = farming_count$palette) +
  ylab("Study count") 
farming_plot_0 <- farming_plot + 
  coord_flip() +
  guides(fill = "none") +
  scale_y_reverse() +
  scale_x_discrete(as.character(0:1))

farming_plot1 <- farming_plot +
  scale_fill_manual(
    values = farming_count$palette[1:3],
    limits=c("Freshwater hatcheries",
             "Offshore pens",
             "Semi-closed pens")
  ) +
  guides(fill = guide_legend(title = "Farming system"))

farming_plot2 <- farming_plot +
  scale_fill_manual(
    values = farming_count$palette[4:7],
    limits=c("Processing plants",
             "Integrated Multi-Trophic Aquaculture (IMTA)",
             "Multi-Purpose Platforms (MPPs)",
             "Recirculating Aquaculture System (RAS)")
  ) + 
  guides(fill = guide_legend(title = ""))

## Function to extract legend
g_legend <- function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  legend
} 

farming_legend1 <- g_legend(farming_plot1) # extract legend
farming_legend2 <- g_legend(farming_plot2) # extract legend
farming_plots <- grid.arrange(farming_plot_0, farming_legend1, farming_legend2, ncol=3)

#### close ####




# create composite plot
max_width <- unit.pmax(species_legend1$widths, species_legend2$widths,
                       production_legend1$widths, production_legend2$widths,
                       farming_legend1$widths, farming_legend2$widths)
species_legend1$widths <- max_width
species_legend2$widths <- max_width
production_legend1$widths <- max_width
production_legend2$widths <- max_width
farming_legend1$widths <- max_width
farming_legend2$widths <- max_width

# save plot
pdf(file="outputs/figures/composite_plot.pdf",
    width=11,
    height=5)
grid.arrange(species_plot_0, species_legend1, species_legend2,
             production_plot_0, production_legend1, production_legend2,
             farming_plot_0, farming_legend1, farming_legend2,
             ncol=3, nrow=3,
             widths=c(1.5,1,1))
dev.off()

