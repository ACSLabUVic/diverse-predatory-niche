##########################################
##        Darimont et al. 2023          ##
##  Humanity's diverse predatory niche  ##
##             Mapping                  ##
##########################################

# authors: "Mathieu Bourbonnais", "Rob Cooke"

# required packages
library(tidyverse)
library(sf)
library(data.table)
library(tmap)
library(tools)
library(MASS)
library(lwgeom)
library(viridis)

# animal use data (usedata.csv) with counts and proportions derived from IUCN
# and BirdLife International data
# gIDN: 1deg grid cell identifier (join with grid_1deg.shp)
# used: count of species classified as 'used' in a grid
# notused: count of species classified as 'not used' in a grid
# food: count of species classified as 'used for food' in a grid
# pets: count of species classified as 'used for pets' in a grid
# allspecies: count of all species (used and not used) in a grid
# food_percent: species used as food / species used in a grid
# pets_percent: species used as pets / species used in a grid

# data
sp_n = readr::read_csv('data/raw/usedata.csv')

# MODEL
# negative binomial generalized linear model, species used ~ log(available species)
mod_used = MASS::glm.nb(used ~ log(allspecies), data = sp_n, link = 'log')

# Figure 5A - model fit
mod_pred = data.frame(allspecies = sp_n$allspecies, used = predict(mod_used, type = 'link'))

fitplot = ggplot2::ggplot(data = sp_n, ggplot2::aes(x = log(allspecies), y = log(used + 0.01)), size = 0.5) +
  ggplot2::geom_point(size = 0.2, alpha = 0.5) +
  ggplot2::geom_line(data = mod_pred, ggplot2::aes(x = log(allspecies), y = used), col = 'blue', linewidth = 1) +
  ggplot2::labs(y = 'log(Used)', x = 'log(Available)') +
  ggplot2::theme_classic()

# Figure 5B - model residuals
mod_resid = data.frame(allspecies = sp_n$allspecies, residuals = residuals(mod_used))

residplot = ggplot2::ggplot(data = mod_resid, ggplot2::aes(x = allspecies, y = residuals)) + 
  ggplot2::geom_point(size = 0.5) +
  ggplot2::labs(x = "Available", y = "Residuals") +
  ggplot2::theme_classic()

# combine plots
fitresidplot <- cowplot::plot_grid(fitplot, residplot, labels = "AUTO")

# save plot
cowplot::save_plot("figs/fig5_model.png", fitresidplot, base_height = 4, base_width = 6)

# MAPPING
# add residuals to sp_n for mapping
sp_n$residuals = residuals(mod_used)

## 1deg grid data (grid_1deg.shp)

# unzip file
unzip(zipfile = 'data/raw/grid_1deg.zip', exdir = 'data/raw/')

# load file
g1deg = sf::read_sf('data/raw/grid_1deg/grid_1deg.shp')

# delete unzipped file
unlink('data/raw/grid_1deg', recursive = TRUE)

## countries (countries.shp), source = naturalearthdata.com

# unzip file
unzip(zipfile = 'data/raw/countries.zip', exdir = 'data/raw/')

# load file
countries = sf::read_sf('data/raw/countries/countries.shp')

# delete unzipped file
unlink('data/raw/countries', recursive = TRUE)

# join count data with g1deg
dat4plot = dplyr::left_join(g1deg, sp_n, by = 'gIDN')

# project data for mapping
dat4plot = lwgeom::st_transform_proj(dat4plot, crs = "+proj=wintri")
dat4plot = sf::st_make_valid(dat4plot)
countries = lwgeom::st_transform_proj(countries, crs = "+proj=wintri")
countries = sf::st_make_valid(countries)

# Figure 3A - Species Used (Count)
p1 = tmap::tm_shape(dat4plot) +
  tmap::tm_borders(alpha = 0) +
  tmap::tm_fill('used', palette =  "magma", style = "cont", title = '',
                legend.reverse = TRUE, showNA = FALSE, 
                breaks = c(0, 400, 800, 1200, 1600),
                labels = c("0", "400", "800", "1200", "1600")) +
  tmap::tm_shape(countries) +
  tmap::tm_borders('grey', lwd = 1) +
  tmap::tm_credits("A", position = c("left", "top"), size = 1.5) +
  tmap::tm_layout(panel.labels = "Species Used (Count)",
                  panel.label.bg.color = NA,
                  panel.label.size = 1.5,
                  legend.outside = TRUE,
                  legend.text.size = 1,
                  frame = FALSE, 
                  frame.lwd = NA)

# Figure 3B - Species Used (Residuals)
p2 = tmap::tm_shape(dat4plot) +
  tmap::tm_borders(alpha = 0) +
  tmap::tm_fill('residuals', palette =  "magma", midpoint = NA, style = "cont", title = '',
                legend.reverse = TRUE, showNA = FALSE, 
                labels = c("-3 (5%)", "-2 (14%)", "-1 (37%)", "0 (100%)", "1 (272%)")) +
  tmap::tm_shape(countries) +
  tmap::tm_borders("grey", lwd = 1) +
  tmap::tm_credits("B", position = c("left", "top"), size = 1.5) +
  tmap::tm_layout(panel.labels = "Species Used (Residuals)",
                  panel.label.bg.color = NA,
                  panel.label.size = 1.5,
                  legend.outside = TRUE,
                  legend.text.size = 1,
                  frame = FALSE, 
                  frame.lwd = NA)

# Figure 3C - Food (% Species Used)
p3 = tmap::tm_shape(dat4plot) +
  tmap::tm_borders(alpha = 0) +
  tmap::tm_fill('food_percent', palette =  "magma", style = "cont", title = '',
                legend.reverse = TRUE, showNA = FALSE, 
                breaks = c(0, 25, 50, 75, 100), 
                labels = c("0", "25", "50", "75", "100")) +
  tmap::tm_shape(countries) +
  tmap::tm_borders("grey", lwd = 1) +
  tmap::tm_credits("C", position = c("left", "top"), size = 1.5) +
  tmap::tm_layout(panel.labels = "Food (% Species Used)",
                  panel.label.bg.color = NA,
                  panel.label.size = 1.5,
                  legend.outside = TRUE,
                  legend.text.size = 1,
                  frame = FALSE, 
                  frame.lwd = NA)

# Figure 3D - Pets (% Species Used)
p4 = tmap::tm_shape(dat4plot) +
  tmap::tm_borders(alpha = 0) +
  tmap::tm_fill('pets_percent', palette =  "magma", style = "cont", title = '',
                legend.reverse = TRUE, showNA = FALSE,
                breaks = c(0, 25, 50, 75, 100),
                labels = c("0", "25", "50", "75", "100")) +
  tmap::tm_shape(countries) +
  tmap::tm_borders("grey", lwd = 1) +
  tmap::tm_credits("D", position = c("left", "top"), size = 1.5) +
  tmap::tm_layout(panel.labels = "Pets (% Species Used)",
                  panel.label.bg.color = NA,
                  panel.label.size = 1.5,
                  legend.outside = TRUE,
                  legend.text.size = 1,
                  frame = FALSE, 
                  frame.lwd = NA)

# combine plots
p_comb <- tmap::tmap_arrange(p1, p2, p3, p4, ncol = 1)

# save plot
tmap::tmap_save(p_comb, "figs/fig3_maps.png", width = 5.5, height = 8)

