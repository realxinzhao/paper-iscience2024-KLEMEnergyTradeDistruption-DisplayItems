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


readr::read_csv("data/inst/Fig1.csv") %>%
  rename(region0 = region) %>%
  left_join_error_no_match(
    Regmapping %>% select(region0 = region, region = REG1), by = "region0") ->
  df_fig1




df_fig1 %>%
  gather_years() %>% filter(year == 2050) %>%
  Agg_reg(region, RUSX, variable) %>%
  filter(!RUSX == "NoRusX") %>%
  mutate(scenario = factor(scenario, levels = c("SSP2-ref", "RCP2.6"))) %>%
  mutate(ss = if_else(region == "Russia", "Russia", "Others")) %>%
  ggplot() + facet_wrap(~scenario, nrow = 2, scales = "fixed") +
  geom_bar(aes(x = region, y = value, fill = ss), size = 0.3, color = "black",
           stat = "identity") +
  geom_hline(yintercept = 0) +
  labs(y = "EJ/year", x = "Region", title = "(A) Net fossil fuel trade in SSP2-ref and RCP2.6, 2050") +
  scale_fill_manual(values = c("grey", "red")) +
  theme_bw() + theme0 + theme1 + theme(legend.position = "none")+
  theme(axis.title.x = element_blank())-> p1;p1

df_fig1 %>%
  gather_years() %>% filter(year == 2050) %>%
  Agg_reg(region, RUSX, variable) %>%
  mutate(scenario = gsub("-ref|\\|NoRusX", "", scenario)) %>%
  group_by_at(vars(scenario, variable, region)) %>%
  mutate(value = value - value[RUSX == "YesRusX"]) %>%
  filter(RUSX == "NoRusX") %>%
  mutate(scenario = factor(scenario, levels = c("SSP2", "RCP2.6"),
                           labels = c("SSP2|NoRussX minus SSP2-ref", "RCP2.6|NoRussX minus RCP2.6"))) %>%
  mutate(ss = if_else(region == "Russia", "Russia", "Others")) %>%
  ggplot() + facet_wrap(~scenario, nrow = 2, scales = "fixed") +
  geom_bar(aes(x = region, y = value, fill = ss), size = 0.3, color = "black",
           stat = "identity") +
  geom_hline(yintercept = 0) +
  labs(y = "EJ/year", x = "Region", title = "(B) Changes in net fossil fuel trade due to NoRussX, 2050") +
  scale_fill_manual(values = c("grey", "red")) +
  theme_bw() + theme0 + theme1 + theme(legend.position = "none") -> p2

p1/p2 -> pp


pp %>% Write_png(.name = "Fig1", .DIR_MODULE = "", h = 12, w = 10, r = 300)


