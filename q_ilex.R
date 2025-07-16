
# QUERCUS ILEX

## Packages
library(tidyverse)
library(nlme)
library(MASS)
library(stats)
library(pslc)
library(DHARMa)
library(patchwork)


## Data
setwd()
dat <- read.csv("./data.csv", header = T, sep = ",")
str(dat)

dat$species <- as.factor(dat$species)
summary(dat)

datqi <- subset(dat, dat$species == "q_ilex")
row.names(datqi) <- NULL


## Spatial autocorrelation
geo <- cbind(datqi$utm_x, datqi$utm_y)  # matrix
samples.dist <- as.matrix(dist(geo)) 
samples.dist.inv <- 1 / samples.dist
diag(samples.dist.inv) <- 0

Moran.I(datqi$reg_ilex, samples.dist.inv, alternative = "greater", na.rm = TRUE) # I Moran test


## Standardization
datqi$abs <- (datqi$ba - mean(datqi$ba, na.rm = T)) / sd(datqi$ba, na.rm = T)
datqi$dists <- (datqi$dist - mean(datqi$dist, na.rm = T)) / sd(datqi$dist, na.rm = T)
datqi$difs <- (datqi$alt_dif - mean(datqi$alt_dif, na.rm = T)) / sd(datqi$alt_dif, na.rm = T)
datqi$spts <- (datqi$sppt - mean(datqi$sppt, na.rm = T)) / sd(datqi$sppt, na.rm = T)
datqi$twis <- (datqi$twi - mean(datqi$twi, na.rm = T)) / sd(datqi$twi, na.rm = T)


## Selected model
z1 <- gls(log(reg_ilex + 1) ~ abs + dists,
          correlation = corSpher(form = ~utm_x + utm_y, nugget = TRUE), method = "ML", data = datqi, na.action = na.omit)
summary(z1)


# Null model
z0 <- gls(log(reg_ilex + 1) ~ 1,
          correlation = corSpher(form = ~utm_x + utm_y, nugget = TRUE), method = "ML", data = datqi)

AIC(z0, z1)


## Plot
# Distance
scale_x_dist <- c(round(-3 * 30.30149 + 374.3457 + 2),  # distance
                  round(-2 * 30.30149 + 374.3457 + 1),
                  round(-1 * 30.30149 + 374.3457 + 1),
                  round(0 * 30.30149 + 374.3457 + 1),
                  round(1 * 30.30149 + 374.3457),
                  round(2 * 30.30149 + 374.3457))

g <- plot_model(z1, type = "pred", terms = c("dists"), show.data = T, jitter = T, dot.size = 1.5) +
  labs(title = NULL,
       x = "Distance (m)",
       y = expression(paste("Oak juvenile density (indv./100 m"^"2",")"))) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'white'),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10)) +
  scale_x_continuous(labels = scale_x_dist) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = '#008B92', alpha = 0.2) + 
  geom_line(color = '#2F4858', size = 0.8)
g

# Basal area
scale_x_ab <- c(round(-1 * 34.52283 + 28.73695 + 6), 
                round(0 * 34.52283 + 28.73695 + 1),    
                round(1 * 34.52283 + 28.73695 - 3),    
                round(2 * 34.52283 + 28.73695 + 2),
                round(3 * 34.52283 + 28.73695 - 2),
                round(4 * 34.52283 + 28.73695 + 3))


k <- plot_model(z1, type = "pred", terms = c("abs"), show.data = T, jitter = T, dot.size = 1.5) +
  labs(title = NULL,
       x = expression(paste("Basal area (m"^"2",")")),
       y = NULL) +
  theme_classic() +
  theme(panel.background = element_rect(fill = 'white'),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10)) +
  scale_x_continuous(labels = scale_x_ab) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = '#008B92', alpha = 0.2) + 
  geom_line(color = '#2F4858', size = 0.8)
k

#
plot_def <- g + k
plot_def


## Tiff
setwd()
ggsave(filename = "Fig.3.tiff", plot = plot_def, device = "tiff", dpi = "retina", units = "mm", width = 170, height = 93)


