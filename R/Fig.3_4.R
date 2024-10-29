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


readr::read_csv("data/inst/Fig3.csv") %>%
  rename(region0 = region) %>%
  left_join(
    Regmapping %>% select(region0 = region, region = REG1), by = "region0") %>%
  mutate(region = if_else(is.na(region), region0, region)) %>%
  gather(variable, value, `del K(materials) Share`:`del GDP`) %>%
  mutate(region = factor(region, levels = c(unique(Regmapping$REG1) %>% sort(), "GLOBAL"))) %>%
  Agg_reg(variable, region)->
  df_fig3



df_fig3 %>%
  filter(variable != "del GDP") %>%
  filter(scenario == "SSP2|NoRusX less SSP2") %>%
  mutate(variable = factor(variable, levels = c("del K(materials) Share","del Labor Share",
    "del E (energy service) Share", "del Net Energy Export" ))) %>%
  ggplot() + facet_grid(year~scenario,  scales = "free_y") +
  geom_bar(aes(x = region, y = value, fill = variable), size = 0.3, color = "black",
           stat = "identity") +
  geom_hline(yintercept = 0) +
  labs(y = "Billion 2015 USD", x = "Region", fill = "Components") +
  #scale_fill_brewer(palette = "Set2") +
  scale_fill_manual(values = c("#4472C4",  "#70AD47", "#ED7D31", "#A5A5A5"))+
  #scale_fill_excel_new() +
  theme_bw() + theme0 + theme1 +
  theme(axis.title.x = element_blank(), legend.position = "bottom")-> p1;p1

p1 %>% Write_png(.name = "Fig3", .DIR_MODULE = "", h = 13, w = 11, r = 300)


df_fig3 %>%
  filter(variable != "del GDP") %>%
  filter(scenario == "RCP2.6|NoRusX less RCP2.6") %>%
  mutate(variable = factor(variable, levels = c("del K(materials) Share","del Labor Share",
                                                "del E (energy service) Share", "del Net Energy Export" ))) %>%
  ggplot() + facet_grid(year~scenario,  scales = "free_y") +
  geom_bar(aes(x = region, y = value, fill = variable), size = 0.3, color = "black",
           stat = "identity") +
  geom_hline(yintercept = 0) +
  labs(y = "Billion 2015 USD", x = "Region", fill = "Components") +
  scale_fill_manual(values = c("#4472C4",  "#70AD47", "#ED7D31", "#A5A5A5"))+
  theme_bw() + theme0 + theme1 +
  theme(axis.title.x = element_blank(), legend.position = "bottom")-> p1;p1

p1 %>% Write_png(.name = "Fig4", .DIR_MODULE = "", h = 13, w = 11, r = 300)



readr::read_csv("data/inst/Fig5.csv") ->
  df_fig5


df_fig5 %>% gather(region, value, Russia:Importers) %>%
  mutate(variable = factor(variable, levels = c("del K(materials) Share","del Labor Share",
                                                "del E (energy service) Share", "del Net Energy Export" ))) %>%
  mutate(region = factor(region, levels = c("Russia", "Exporters", "Importers"),
                         labels = c("Russia", "Non-Russia\nExporters", "Non-Russia\nImporters"))) %>%
  mutate(scenario = factor(scenario, levels = c("SSP2|NoRusX less SSP2", "RCP2.6|NoRusX less RCP2.6"))) %>%
  ggplot() + #facet_wrap(year~scenario,  scales = "fixed", nrow =1) +
  facet_grid(scenario~year,  scales = "fixed") +
  geom_bar(aes(x = region, y = value, fill = variable), size = 0.2, color = "black",
           stat = "identity") +
  geom_hline(yintercept = 0) +
  labs(y = "Billion 2015 USD", x = "Region", fill = "Components") +
  scale_fill_manual(values = c("#4472C4",  "#70AD47", "#ED7D31", "#A5A5A5"))+
  scale_y_continuous(limits = c(-850, 850), breaks = seq(-800, 800, 400), expand = c(0,0)) +
  theme_bw() + theme0 + theme1 +
  theme(axis.title.x = element_blank(), legend.position = "right",
        axis.text.x = element_text(angle = 40, hjust =1, vjust = 1.2) )-> p1;p1

p1 %>% Write_png(.name = "Fig5", .DIR_MODULE = "", h = 8, w = 11, r = 300)

p1 %>% Write_png(.name = "Fig5", .DIR_MODULE = "", h = 8, w = 16, r = 300)



df_fig5 %>% gather(region, value, Russia:Importers) %>%
  mutate(variable = factor(variable, levels = c("del K(materials) Share","del Labor Share",
                                                "del E (energy service) Share", "del Net Energy Export" ))) %>%
  mutate(region = factor(region, levels = c("Russia", "Exporters", "Importers"),
                         labels = c("Russia", "Non-Russia\nExporters", "Non-Russia\nImporters"))) %>%
  mutate(scenario = factor(scenario, levels = c("SSP2|NoRusX less SSP2", "RCP2.6|NoRusX less RCP2.6"))) %>%
  filter(year == 2050, scenario == "SSP2|NoRusX less SSP2") %>%
  mutate(ss = "Changes in GDP") %>%
  ggplot() + facet_wrap(~ss) +
  geom_bar(aes(x = region, y = value, fill = variable), size = 0.2, color = "black",
           stat = "identity") +
  geom_hline(yintercept = 0) +
  labs(y = "Billion 2015 USD", x = "Region", fill = "Components") +
  scale_fill_manual(values = c("#4472C4",  "#70AD47", "#ED7D31", "#A5A5A5"))+
  scale_y_continuous(limits = c(-650, 650), breaks = seq(-600, 600, 200), expand = c(0,0), position = "right") +
  theme_bw() + #theme0 + theme1 +
  theme(text = element_text(size = 12),
        axis.text = element_text(size = 11),
        axis.title.x = element_blank(), legend.position = "none",
        axis.text.x = element_blank(), panel.grid = element_blank(), strip.text = element_text(size = 11)
          )-> p1;p1

p1 %>% Write_png(.name = "Fig_GA1", .DIR_MODULE = "", h = 3.33, w = 2, r = 300)

# GA2 ----


readr::read_csv("data/inst/Fig_GA2.csv") ->
  df_figGA2

df_figGA2 %>%
  gather(energy, value, Oil, Gas, Coal) %>%
  group_by(player, year, energy) %>%
  summarize(value = sum(value)) %>% #distinct(player)
  mutate(player = factor(player, levels = c("Russia", "Exporter", "Importer"),
                         labels = c("Russia", "Non-Russia\nExporters", "Non-Russia\nImporters"))) %>%
  mutate(ss = "Changes in trade") %>%
  ggplot() + facet_wrap(~ss) +
  geom_bar(aes(x = player, y = value, fill = energy), size = 0.2, color = "black",
           stat = "identity") +
  geom_hline(yintercept = 0) +
  labs(y = "EJ/year", x = "Region", fill = "Components") +
  scale_fill_brewer(palette = "Set3") +
  #scale_fill_manual(values = c("#4472C4",  "#70AD47", "#ED7D31", "#A5A5A5"))+
  scale_y_continuous(limits = c(-65, 65), breaks = seq(-60, 60, 20), expand = c(0,0), position = "left") +
  theme_bw() + #theme0 + theme1 +
  theme(text = element_text(size = 12),
        axis.text = element_text(size = 11),
        axis.title.x = element_blank(), legend.position = "none",
        axis.text.x = element_blank(), panel.grid = element_blank(), strip.text = element_text(size = 11)
  )-> p1;p1

p1 %>% Write_png(.name = "Fig_GA2", .DIR_MODULE = "", h = 3.33, w = 2, r = 300)

