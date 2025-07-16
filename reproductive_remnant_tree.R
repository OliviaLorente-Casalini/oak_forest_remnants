
# TREE SIZE Q.ILEX

# Packages
library(tidyverse)
library(nlme)
library(MASS)
library(gstat)
library(sf)
library(spdep)
library(ncf)
library(gstat)
library(brms)
library(glmmTMB)


## Data
setwd()
dat <- read.csv("./data.csv", header = T, sep = ",")
str(dat)

dat$species <- as.factor(dat$species)
summary(dat)

datqi <- subset(dat, dat$species == "q_ilex")
row.names(datqi) <- NULL

datos <- datqi |> 
  dplyr::select(id_plot, species, reg_ilex, spp3, spp12, utm_x, utm_y) |> 
  pivot_longer(cols = starts_with("spp"), names_to = "tipo_madre", values_to = "spp") |> 
  mutate(tipo_madre = as.factor(tipo_madre))

datos <- datos |> 
  mutate(position = numFactor(utm_x, utm_y))

plot(datos$spp)
datos2 <- datos |> filter(id_plot != 133)


## Model
b1 <- glmmTMB(reg_ilex ~ spp + tipo_madre + cs(position + 0 | id_plot), family = poisson(), data = datos2)


## Plot
datos3 <- datos2 |> dplyr::select(reg_ilex, spp, tipo_madre, position, id_plot)

predictions <- predict(b1, datos3, type = "response")

datos_plot <- data.frame(predicciones = predictions, datos3$spp, datos3$tipo_madre) |> 
              rename(spp = datos3.spp, tipo_madre = datos3.tipo_madre)

datos_plot$reg_ilex <- exp(datos_plot$predicciones) 

k <- ggplot(datos_plot, aes(spp, predicciones)) +
  geom_point(aes(color = tipo_madre)) +
  geom_smooth(aes(color = tipo_madre, fill = tipo_madre), method = "loess", span = 1, alpha = 0.3) +
  labs(x = "Seed Pressure Potential (SPP)", y = expression(paste("Oak juvenile density (indv./100 m"^"2",")"))) +
  scale_color_manual(values = c("#FCCA65", "#008580"),
                     labels = c("Small/medium trees", "Old-growth trees"), name = NULL) + 
  scale_fill_manual(values = c("#FCCA65", "#008580"),
                    labels = c("Small/medium trees", "Old-growth trees"), name = NULL) +
  theme_classic() +
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.text = element_text(size = 10), 
        legend.position = c(0.8, 0.15),
        legend.background = element_rect(fill = "white", color = "black"))
k


## Tiff
setwd()
ggsave(filename = "Fig.5.tiff", plot = k, device = "tiff", dpi = "retina", units = "mm", width = 150, height = 93)




