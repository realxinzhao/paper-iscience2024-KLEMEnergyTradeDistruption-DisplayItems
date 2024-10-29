# Load libs ----
library(tidyr)
library(stringr)
library(ggplot2)
library(ggsci)
library(scales)
library(dplyr)
library(gcamdata)
library(purrr)
library(patchwork)
library(RColorBrewer)

source("R/LoadPackagesFuncs.R")
source("R/GCAM_module_funcs.R")

DIR_OUTPUT <- "output"
Project <- "iscience"

theme1 <- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
                legend.text.align = 0,
                strip.background = element_rect(fill="grey99"),
                strip.text = element_text(size = 12),
                axis.text.x.bottom = element_text(size = 12),
                axis.text.y = element_text(size = 12),
                panel.spacing.y = unit(0.5, "lines"),
                panel.spacing.x = unit(0.5, "lines"))

library(ggthemes)


readr::read_csv("data/inst/Fig2.csv") %>%
  rename(region0 = region) %>%
  left_join_error_no_match(
    Regmapping %>% select(region0 = region, region = REG1), by = "region0") ->
  df_fig2

df_fig2 %>%
  gather_years() %>%
  group_by_at(vars(-value, -year)) %>%
  mutate(value = value/first(value)) %>%
  mutate(variable = factor(variable, levels = c("Oil", "Gas", "Coal")),
         region = factor(region, levels = c("Russia", "USA", "China", "Middle East")),
         scenario = factor(scenario, levels = c("SSP2-ref","SSP2|NoRusX","RCP2.6","RCP2.6|NoRusX" ))) %>%
  ggplot() + facet_grid(region~variable, scales = "fixed") +
  geom_line(aes(x = year, y = value, color = scenario), size = 1) +
  geom_point(aes(x = year, y = value, color = scenario)) +
  geom_hline(yintercept = 1) +
  labs(y = "Index (2025 = 1)", x = "Year", color = "Scenario", fill = "Scenario") +
  scale_y_continuous(limits = c(0.2, 1.3), breaks = seq(0.2, 1.2, 0.2), expand = c(0,0)) +
  #scale_color_brewer(palette = "Set1") +
  scale_color_manual(values = c("#A5A5A5","#4472C4","#FFC000", "#ED7D31"))+
  #scale_color_excel_new() +
  theme_bw() + theme0 + theme_leg + theme1 + theme(legend.position = "right") -> pp;pp


pp %>% Write_png(.name = "Fig2", .DIR_MODULE = "", h = 9, w = 11, r = 300)



excel_new_pal("Office Theme")(10)



