library(tidyverse)
library(gdxrrw)
library(ggplot2)

theme_1 <- theme_bw() +
  theme(
    text = element_text(size = 20), axis.text.x = element_text(size = 12),
    legend.position = "bottom",
    legend.title = element_blank(),
    strip.background = element_blank(),
    strip.clip = "off"
  )


# Use local copies when present; otherwise use the model-data directory.
gdx_dir <- "../data"
if (!file.exists(file.path(gdx_dir, "data.gdx")) ||
    !file.exists(file.path(gdx_dir, "baseyear.gdx"))) {
  gdx_dir <- "../../../../data/AIMALPHA_data/model_data"
}

gams_sys_dir <- Sys.getenv("GAMS_SYSDIR")
if (nzchar(gams_sys_dir)) {
  igdx(gams_sys_dir)
}

# data --------------------------------------------------------------------
gdpPc <- rgdx.param(file.path(gdx_dir, "baseyear.gdx"), "gdpPc")
feedReq <- rgdx.param(file.path(gdx_dir, "data.gdx"), "feedReq")
feedConcShr <- rgdx.param(file.path(gdx_dir, "data.gdx"), "feedConcShr")
elaP <- rgdx.param(file.path(gdx_dir, "data.gdx"), "elaP") %>%
  filter(as.character(c) == as.character(c2)) %>%
  dplyr::select(!c2)
sysShr <- rgdx.param(file.path(gdx_dir, "data.gdx"), "sysShr") %>%
  rename(c = a_liv)
elaIncFdm <- rgdx.param(file.path(gdx_dir, "baseyear.gdx"), "elaIncFdm")

# 6-1 ---------------------------------------------------------------------
gdpPc_2005 <- gdpPc %>%
  filter(ssp == "SSP2") %>%
  filter(yr == "2005") %>%
  dplyr::select(!c(ssp, yr))
data6_1 <- full_join(elaP, gdpPc_2005, by = "cty")
data6_1 <- data6_1 %>%
  filter(c %in% c("wht", "str", "cmt", "mlk", "vol", "alc", "sgr")) %>%
  mutate(c = dplyr::recode(c,
    "wht" = "wht rce mze crl",
    "str" = "str pls nut ocr vgt frt",
    "cmt" = "cmt rmt omt pmt",
    "mlk" = "mlk dai",
    "vol" = "vol",
    "alc" = "stm spc alc",
    "sgr" = "sgr swt egg"
  ))
g6_1 <- data6_1 %>% ggplot() +
  geom_point(aes(x = log(gdpPc), y = elaP)) +
  labs(x = "log(gdpPc)", y = "elaP") +
  facet_wrap(vars(c), scales = "free_y") +
  theme_1
ggsave("../../../figs/fig6_1.png", plot = g6_1, width = 14, height = 8)
c_list <- unique(as.character(data6_1$c))
regresult <- as.data.frame(c_list)
regresult <- regresult %>% mutate(alpha = 0, beta = 0, r2 = 0)
for (i in c_list) {
  data6_1_c <- data6_1 %>%
    filter(c == i) %>%
    na.omit()
  regression <- lm(data = data6_1_c, elaP ~ log(gdpPc))
  regresult_temp <- summary(regression)
  regresult[c_list == i, "alpha"] <- regresult_temp$coefficients[1]
  regresult[c_list == i, "beta"] <- regresult_temp$coefficients[2]
  regresult[c_list == i, "r2"] <- regresult_temp$r.squared
  regresult[c_list == i, "lower"] <- confint(regression, level = 0.95)[2, 1]
  regresult[c_list == i, "upper"] <- confint(regression, level = 0.95)[2, 2]
  regresult[c_list == i, "p"] <- regresult_temp$coefficients[1, 4]
}
write.csv(regresult, file = "../../../data/table6_1.csv", quote = FALSE, row.names = FALSE)
# 6-2 ---------------------------------------------------------------------
data6_2 <- sysShr %>%
  filter(sys != "tot") %>%
  mutate(cz = dplyr::recode(cz,
    "A" = "Arid",
    "T" = "Temperate",
    "H" = "Humid",
    "Y" = "HyperArid"
  )) %>%
  pivot_wider(names_from = "sys", values_from = "sysShr")
data6_2[is.na(data6_2)] <- 0
data6_2 <- data6_2 %>%
  mutate(TOT = MX + LG) %>%
  mutate(MX = MX / TOT, LG = LG / TOT) %>%
  dplyr::select(!TOT) %>%
  mutate(MX = log(MX / (1 - MX)), LG = log(LG / (1 - LG))) %>%
  dplyr::select(!MX) %>%
  filter(!LG %in% c(Inf, -Inf))
data6_2 <- left_join(data6_2, gdpPc_2005, by = "cty")
g6_2 <- data6_2 %>% ggplot() +
  geom_point(aes(x = log(gdpPc), y = LG, color = cz)) +
  labs(x = "log(gdpPc)", y = "logit(sysShr)") +
  facet_wrap(vars(c, cz), scales = "free_y") +
  theme_1
ggsave("../../../figs/fig6_2.png", plot = g6_2, width = 16, height = 10)
c_list <- unique(data6_2$c)
regresult <- as.data.frame(c_list)
regresult <- regresult %>% mutate(ro = 0, delta_A = 0, delta_T = 0, delta_H = 0, delta_Y = 0, r2 = 0, upper = 0, lower = 0, p = 0)
for (i in c_list) {
  data6_2_c <- data6_2 %>% filter(c == i)
  regression <- lm(data = data6_2_c, LG ~ -1 + log(gdpPc) + cz)
  regresult_temp <- summary(regression)
  regresult[c_list == i, "ro"] <- regresult_temp$coefficients[1]
  regresult[c_list == i, "delta_A"] <- regresult_temp$coefficients[2]
  regresult[c_list == i, "delta_T"] <- regresult_temp$coefficients[3]
  regresult[c_list == i, "delta_H"] <- regresult_temp$coefficients[4]
  regresult[c_list == i, "delta_Y"] <- regresult_temp$coefficients[5]
  regresult[c_list == i, "r2"] <- regresult_temp$adj.r.squared
  regresult[c_list == i, "lower"] <- confint(regression, level = 0.95)[1, 1]
  regresult[c_list == i, "upper"] <- confint(regression, level = 0.95)[1, 2]
  regresult[c_list == i, "p"] <- regresult_temp$coefficients[1, 4]
}
write.csv(regresult, file = "../../../data/table6_2.csv", quote = FALSE, row.names = FALSE)
# 6-3 ---------------------------------------------------------------------
data6_3 <- left_join(feedReq, gdpPc_2005, by = "cty") %>%
  mutate(
    cz = dplyr::recode(cz,
      "A" = "Arid",
      "T" = "Temperate",
      "H" = "Humid",
      "Y" = "HyperArid",
      "tot" = "Total"
    ),
    sys = dplyr::recode(sys, "tot" = "Total")
  ) %>%
  mutate(c = factor(c, levels = c("cmt", "rmt", "mlk", "pmt", "omt")))
g6_3 <- data6_3 %>% ggplot() +
  geom_point(aes(x = log(gdpPc), y = log(feedReq), color = cz)) +
  facet_wrap(vars(cz, sys, c), scales = "free_y", ncol = 6) +
  labs(x = "log(gdpPc)", y = "log(feedReq)") +
  theme_1
ggsave("../../../figs/fig6_3.png", g6_3, width = 18, height = 14, create.dir = T)
c_list <- c("cmt", "rmt", "mlk")
regresult <- as.data.frame(c_list)
regresult <- regresult %>% mutate(beta = 0, alpha_A = 0, alpha_T = 0, alpha_H = 0, alpha_Y = 0, r2 = 0)
for (j in c("LG", "MX")) {
  data6_3_sys <- data6_3 %>% filter(sys == j)
  for (i in c_list) {
    data6_3_c <- data6_3_sys %>% filter(c == i)
    regression <- lm(data = data6_3_c, log(feedReq) ~ -1 + log(gdpPc) + cz)
    regresult_temp <- summary(regression)
    regresult[c_list == i, "beta"] <- regresult_temp$coefficients[1]
    regresult[c_list == i, "alpha_A"] <- regresult_temp$coefficients[2]
    regresult[c_list == i, "alpha_T"] <- regresult_temp$coefficients[3]
    regresult[c_list == i, "alpha_H"] <- regresult_temp$coefficients[4]
    regresult[c_list == i, "alpha_Y"] <- regresult_temp$coefficients[5]
    regresult[c_list == i, "r2"] <- regresult_temp$adj.r.squared
    regresult[c_list == i, "lower"] <- confint(regression, level = 0.95)[1, 1]
    regresult[c_list == i, "upper"] <- confint(regression, level = 0.95)[1, 2]
    regresult[c_list == i, "p"] <- regresult_temp$coefficients[1, 4]
    regresult <- regresult %>% mutate(sys = j)
  }
  if (j == "LG") {
    regresult_agg <- regresult
  } else {
    regresult_agg <- rbind(regresult_agg, regresult)
  }
}
c_list <- c("pmt", "omt")
regresult <- as.data.frame(c_list)
regresult <- regresult %>% mutate(beta = 0, alpha_tot = 0, r2 = 0, sys = "Total")
data6_3_sys <- data6_3 %>% filter(sys == "Total")
for (i in c_list) {
  data6_3_c <- data6_3_sys %>% filter(c == i)
  regression <- lm(data = data6_3_c, log(feedReq) ~ log(gdpPc))
  regresult_temp <- summary(regression)
  regresult[c_list == i, "beta"] <- regresult_temp$coefficients[2]
  regresult[c_list == i, "alpha_tot"] <- regresult_temp$coefficients[1]
  regresult[c_list == i, "r2"] <- regresult_temp$r.squared
  regresult[c_list == i, "lower"] <- confint(regression, level = 0.95)[2, 1]
  regresult[c_list == i, "upper"] <- confint(regression, level = 0.95)[2, 2]
  regresult[c_list == i, "p"] <- regresult_temp$coefficients[1, 4]
}
regresult <- full_join(regresult_agg, regresult)
write.csv(regresult, file = "../../../data/table6_3.csv", quote = FALSE, row.names = FALSE)
# 6-4 ---------------------------------------------------------------------
data6_4 <- left_join(feedConcShr, gdpPc_2005, by = "cty") %>%
  mutate(cz = dplyr::recode(cz,
    "A" = "Arid",
    "T" = "Temperate",
    "H" = "Humid",
    "Y" = "HyperArid"
  )) %>%
  filter(!c %in% c("omt", "pmt")) %>%
  mutate(logit_feed_conc_shr = log(feedConcShr / (1 - feedConcShr)))
g6_4 <- data6_4 %>% ggplot() +
  geom_point(aes(x = log(gdpPc), y = logit_feed_conc_shr, color = cz)) +
  facet_wrap(vars(cz, sys, c), scales = "free_y", ncol = 6) +
  labs(x = "log(gdpPc)", y = "logit(feedConcShr)") +
  theme_1
ggsave("../../../figs/fig6_4.png", g6_4, width = 18, height = 12, create.dir = T)
c_list <- c("cmt", "rmt", "mlk")
regresult <- as.data.frame(c_list)
regresult <- regresult %>% mutate(beta = 0, alpha_A = 0, alpha_T = 0, alpha_H = 0, alpha_Y = 0, r2 = 0)
for (j in c("LG", "MX")) {
  data6_4_sys <- data6_4 %>% filter(sys == j)
  for (i in c_list) {
    data6_4_c <- data6_4_sys %>% filter(c == i)
    regression <- lm(data = data6_4_c, logit_feed_conc_shr ~ -1 + log(gdpPc) + cz)
    regresult_temp <- summary(regression)
    regresult[c_list == i, "beta"] <- regresult_temp$coefficients[1]
    regresult[c_list == i, "alpha_A"] <- regresult_temp$coefficients[2]
    regresult[c_list == i, "alpha_T"] <- regresult_temp$coefficients[3]
    regresult[c_list == i, "alpha_H"] <- regresult_temp$coefficients[4]
    regresult[c_list == i, "alpha_Y"] <- regresult_temp$coefficients[5]
    regresult[c_list == i, "r2"] <- regresult_temp$adj.r.squared
    regresult[c_list == i, "lower"] <- confint(regression, level = 0.95)[1, 1]
    regresult[c_list == i, "upper"] <- confint(regression, level = 0.95)[1, 2]
    regresult[c_list == i, "p"] <- regresult_temp$coefficients[1, 4]
    regresult <- regresult %>% mutate(sys = j)
  }
  if (j == "LG") {
    regresult_agg <- regresult
  } else {
    regresult_agg <- rbind(regresult_agg, regresult)
  }
}
write.csv(regresult_agg, file = "../../../data/table6_4.csv", quote = FALSE, row.names = FALSE)

# Appendix C ------------------------------------------
representative_countries <- c(
  "USA", "DEU", "JPN", "AUS",
  "RUS", "IDN", "IND", "BRA",
  "EGY", "NGA", "ZAF", "NPL",
  "CHN", "MEX", "TUR", "VNM"
)
data_ela_inc <- elaIncFdm %>%
  select(c, cty, ssp, yr, elaIncFdm) %>%
  filter(c %in% c("wht", "rce", "crl", "sgr", "swt",
                  "pls", "vol", "vgt", "frt",
                  "cmt", "pmt", "omt", "mlk"),
         cty %in% representative_countries) %>%
  mutate(
    c = dplyr::recode(
      c,
      "wht" = "wht",
      "rce" = "rce",
      "crl" = "crl_mze_str",
      "sgr" = "sgr",
      "swt" = "swt_stm_alc",
      "pls" = "pls_nut_ocr_spc",
      "frt" = "frt",
      "vgt" = "vgt",
      "pmt" = "pmt",
      "cmt" = "cmt_rmt_omt",
      "vol" = "vol",
      "mlk" = "mlk_dai_egg"
    ),
    elaIncFdm = round(elaIncFdm, 3)
  ) %>%
  filter(yr %in% c(2015, 2050, 2100)) %>%
  pivot_wider(
    names_from  = c,
    values_from = elaIncFdm
  )
write.csv(data_ela_inc, file = "../../../data/tableC_1.csv", quote = FALSE, row.names = FALSE)
