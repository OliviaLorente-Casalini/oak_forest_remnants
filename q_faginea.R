
# QUERCUS FAGINEA

## Packages
library(tidyverse)
library(stats)
library(MASS)
library(DHARMa)
library(nlme)
library(pscl)
library(akima)
library(aods3)


## Data
setwd()
dat <- read.csv("./data.csv", header = T, sep = ",")
str(dat)

dat$species <- as.factor(dat$species)
summary(dat)

datqf <- subset(dat, dat$species == "q_faginea")
row.names(datqf) <- NULL 


## Spatial autocorrelation
geo <- cbind(datqf$utm_x, datqf$utm_y)  # matrix
samples.dist <- as.matrix(dist(geo)) 
samples.dist.inv <- 1 / samples.dist
diag(samples.dist.inv) <- 0

Moran.I(datqf$reg_fag, samples.dist.inv, alternative = "greater", na.rm = TRUE)  # I Moran test


## Standardization
datqf$abs <- (datqf$ba - mean(datqf$ba, na.rm = T)) / sd(datqf$ba, na.rm = T)
datqf$dists <- (datqf$dist - mean(datqf$dist, na.rm = T)) / sd(datqf$dist, na.rm = T)
datqf$difs <- (datqf$alt_dif - mean(datqf$alt_dif, na.rm = T)) / sd(datqf$alt_dif, na.rm = T)
datqf$spts <- (datqf$sppt - mean(datqf$sppt, na.rm = T)) / sd(datqf$sppt, na.rm = T)
datqf$twis <- (datqf$twi - mean(datqf$twi, na.rm = T)) / sd(datqf$twi, na.rm = T)


## Selected model
p1 <- zeroinfl(reg_fag ~ dists + difs + dists:difs, data = datqf2, dist = "poisson")
summary(p1)


## Null model
p0 <- zeroinfl(reg_fag ~ 1, data = datqf, dist = "poisson")

AICc(p0, p1)


## Plot
datqf2 <- datqf |> dplyr::select(reg_fag, alt_dif, dist) |> drop_na(reg_fag)
pred <- predict(p1, datqf, type = "response")

dat_plot <- data.frame(predicciones = predictions, datqf2$dif, datqf2$dist) |> drop_na() |> rename(dif = datqf2.dif, dist = datqf2.dist)
dat_plot$reg_fag <- dat_plot$pred * sd(dat_plot$pred) + mean(dat_plot$pred)
dat_plot$reg_fag <- exp(dat_plot$reg_fag)
dat_plot <- dat_plot |> mutate(across(c(dist, dif, reg_fag), round, digits = 2))
dat_plot <- expand.grid(x = seq(min(dat_plot$dist), max(dat_plot$dist), by = 0.5), y = seq(min(dat_plot$dif), max(dat_plot$dif),  by = 0.5),
                           z = rnorm(dat_plot$reg_fag))

## Regular coordinates plot
n_points_x <- 700
n_points_y <- 700 
x_grid <- seq(min(dat_plot$dist), max(dat_plot$dist), length.out = n_points_x)
y_grid <- seq(min(dat_plot$dif), max(dat_plot$dif), length.out = n_points_y)
xy_grid <- expand.grid(x = x_grid, y = y_grid)


interp_data <- akima::interp(dat_plot$dist, dat_plot$dif, dat_plot$reg_fag,  # Bicubic interpolation Z values
                             xo = x_grid, yo = y_grid)

grid_data <- data.frame(
  x = xy_grid$x,
  y = xy_grid$y,
  z = as.vector(interp_data$z))
grid_data <- grid_data |> drop_na()


plot_interaction <- ggplot(grid_data, aes(x, y)) +
  geom_raster(aes(fill = z)) +
  geom_contour(aes(z = z), bins = 10, color = "white", linewidth = 0.4) +
  labs(x = "Distance (m)", y = "Altitude difference (m)", fill = expression(atop("",
                                                                                 atop(textstyle("Oak juvenile density"),      
                                                                                      atop(textstyle("(indv./100 m"^2*")")))))) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'white'),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12)) +
  scale_fill_gradientn(colours = c("#2F4858", "#1A6674", "#008580", "#3EA37A", "#82BC67", "#D1CE57", "#FCCA65", "#FFC781", "#FFC6A4"))

plot_interaction


## Tiff
setwd()
ggsave(filename = "Fig.4.tiff", plot = plot_def, device = "tiff", dpi = "retina", units = "mm", width = 170, height = 93)

