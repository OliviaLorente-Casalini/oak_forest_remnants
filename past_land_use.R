
## PAST LAND USE

## Packages
library(tidyverse)
library(stats)
library(MASS)
library(DHARMa)
library(nlme)
library(pscl)
library(AER)
library(MuMIn)
library(aods3)
library(ggbreak)
library(patchwork)


## Data
setwd()
dat <- read.csv("./data.csv", header = T, sep = ",")
str(dat)

dat$species <- as.factor(dat$species)
dat$land_use <- as.factor(dat$land_use)
summary(dat)

dat <- dat |> mutate(reg = ifelse(species == "q_faginea", reg_fag, reg_ilex))


## Bootstrap
dat_tbl <- as_tibble(dat) |> group_by(species, land_use) |> 
  dplyr::select(id_plot, reg, species, land_use, sppt)


datasets_list <- list() # datasets                                              
for(i in 1:1000){                                                     
  datasets_list[[i]] <- sample_n(dat_tbl, 64, replace = TRUE)          
}                                                                      


results_models <- list() # models                         
for (i in 1:length(datasets_list)) {
  results_models[[i]] <- glm.nb(reg ~ species + land_use + sppt, data = datasets_list[[i]])
}


coefs_list <- matrix(0, nrow = 1000, ncol = 4) # matrix model coefs
for (i in 1:length(resuls_models)) {
  coefs <- coef(resuls_models[[i]])
  coefs_list[i, ] <- coefs
}

colnames(coefs_list) <- names(coef(results_modlos[[1]]))

t(apply(coefs_list, 2, function(x) quantile(x, c(0.025, 0.975))))  # confidence intervals    
t(apply(coefs_list, 2, function(x) mean(x)))                       # mean                      


predicts_log_list <- list()  # predictions
for (i in 1:length(results_models)) {
  predicts_log_list[[i]] <- predict(results_models[[i]], datasets_list[[i]], se.fit = TRUE)    
}


predicts_list <- list()
for (i in 1:length(predicts_log_list)) {
  predicts_list[[i]] <- exp(predicts_log_list[[i]]$fit)                                        
}


error_standar_list <- list()  # error
for (i in 1:length(predicts_log_list)) {
  error_standar_list[[i]] <- predicts_log_list[[i]]$se.fit * predicts_list[[i]]              
}



datasets_plot_list <- list()                 
for (i in 1:length(datasets_list)) {
  datasets_plot_list[[i]] <- data.frame(predictions = predicts_list[[i]], datasets_list[[i]]$species, datasets_list[[i]]$land_use, errores = error_standar_list[[i]])
}

datasets_plot_list <- map(datasets_plot_list, ~ dplyr::rename(., species = datasets_list..i...species,
                                                              land_use = datasets_list..i...land_use))
datasets_plot_list2 <- map(datasets_plot_list, ~group_by(., species, land_use) |> 
                             dplyr::summarise(predictions = mean(predictions),
                                              errores = mean(errores)))               

## Plot
df_combined <- do.call(rbind, datasets_plot_list2)

dat_plot <- df_combined |> 
  dplyr::group_by(species, land_use) |> 
  dplyr::summarise(predictions = mean(predictions),
                   errores = mean(errores))


ts <- ggplot(dat_plot, aes(sspecies, predictions, color = land_use)) +
  geom_point(position = position_dodge(width = 0.55)) +
  geom_errorbar(aes(ymin = predictions - errores, ymax = predictions + errores), width = 0.1, position = position_dodge(width = 0.55), linewidth = 0.8) +
  labs(x = "Oak species", y = expression(paste("Oak juvenile density (indv./100 m"^"2",")"))) +
  scale_x_discrete(labels = c(~italic("Q. faginea"), ~italic("Q. ilex"))) +
  scale_color_manual(values = c("#E39C57", "#3EA37A"),
                     breaks = c("A", "M"),
                     labels = c("Cropland", "Shrubland"), name = NULL) + 
  ylim(0.1, 14) +
  theme_classic() +
  theme(axis.text.x.top = element_blank(),
        axis.title = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        panel.grid.major.y = element_line(color = "grey80", linetype = "dashed")) +
  scale_y_break(c(1, 5), scales = c(0.9, 0.4), expand = TRUE, space = 0.02)
ts

## Tiff
setwd()
ggsave(filename = "Fig.2.tiff", plot = ts, device = "tiff", dpi = "retina", units = "mm", width = 150, height = 93)


