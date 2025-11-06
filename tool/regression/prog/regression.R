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

# data --------------------------------------------------------------------
GDPPC <- rgdx.param("../data/baseyear.gdx", "GDPpc")
feed_efficiency <- rgdx.param("../data/data.gdx", "feed_efficiency")
feed_share <- rgdx.param("../data/data.gdx", "feed_share")
elhp <- rgdx.param("../data/data.gdx", "elhp")
SYSSHARE <- rgdx.param("../data/data.gdx", "Liv")

# 6-1 ---------------------------------------------------------------------
GDPPC_2005 <- GDPPC %>%
  filter(sc == "SSP2") %>%
  filter(yr == "2005") %>%
  dplyr::select(!c(sc, yr))
data6_1 <- full_join(elhp, GDPPC_2005)
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
  geom_point(aes(x = log(GDPpc), y = elhp)) +
  labs(x = "log(GDPPC)", y = "elhp") +
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
  regression <- lm(data = data6_1_c, elhp ~ log(GDPpc))
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
data6_2 <- SYSSHARE %>%
  filter(sys != "tot") %>%
  mutate(cz = dplyr::recode(cz,
    "A" = "Arid",
    "T" = "Temperate",
    "H" = "Humid",
    "Y" = "HyperArid"
  )) %>%
  pivot_wider(names_from = "sys", values_from = "Liv")
data6_2[is.na(data6_2)] <- 0
data6_2 <- data6_2 %>%
  mutate(TOT = MX + LG) %>%
  mutate(MX = MX / TOT, LG = LG / TOT) %>%
  dplyr::select(!TOT) %>%
  mutate(MX = log(MX / (1 - MX)), LG = log(LG / (1 - LG))) %>%
  dplyr::select(!MX) %>%
  filter(!LG %in% c(Inf, -Inf))
data6_2 <- left_join(data6_2, GDPPC_2005)
g6_2 <- data6_2 %>% ggplot() +
  geom_point(aes(x = log(GDPpc), y = LG, color = cz)) +
  labs(x = "log(GDPPC)", y = "logit(SYSSHARE)") +
  facet_wrap(vars(c, cz), scales = "free_y") +
  theme_1
ggsave("../../../figs/fig6_2.png", plot = g6_2, width = 16, height = 10)
c_list <- unique(data6_2$c)
regresult <- as.data.frame(c_list)
regresult <- regresult %>% mutate(ro = 0, delta_A = 0, delta_T = 0, delta_H = 0, delta_Y = 0, r2 = 0, upper = 0, lower = 0, p = 0)
for (i in c_list) {
  data6_2_c <- data6_2 %>% filter(c == i)
  regression <- lm(data = data6_2_c, LG ~ -1 + log(GDPpc) + cz)
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
data6_3 <- left_join(feed_efficiency, GDPPC_2005) %>%
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
  geom_point(aes(x = log(GDPpc), y = log(feed_efficiency), color = cz)) +
  facet_wrap(vars(cz, sys, c), scales = "free_y", ncol = 6) +
  labs(x = "log(GDPPC)", y = "log(FEEDEFFICIENCY)") +
  theme_1
ggsave("../../../figs/fig6_3.png", g6_3, width = 18, height = 14, create.dir = T)
c_list <- c("cmt", "rmt", "mlk")
regresult <- as.data.frame(c_list)
regresult <- regresult %>% mutate(beta = 0, alpha_A = 0, alpha_T = 0, alpha_H = 0, alpha_Y = 0, r2 = 0)
for (j in c("LG", "MX")) {
  data6_3_sys <- data6_3 %>% filter(sys == j)
  for (i in c_list) {
    data6_3_c <- data6_3_sys %>% filter(c == i)
    regression <- lm(data = data6_3_c, log(feed_efficiency) ~ -1 + log(GDPpc) + cz)
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
  regression <- lm(data = data6_3_c, log(feed_efficiency) ~ log(GDPpc))
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
data6_4 <- left_join(feed_share, GDPPC_2005) %>%
  mutate(cz = dplyr::recode(cz,
    "A" = "Arid",
    "T" = "Temperate",
    "H" = "Humid",
    "Y" = "HyperArid"
  )) %>%
  filter(!c %in% c("omt", "pmt")) %>%
  mutate(logit_feed_share = log(feed_share / (1 - feed_share)))

g6_4 <- data6_4 %>% ggplot() +
  geom_point(aes(x = log(GDPpc), y = logit_feed_share, color = cz)) +
  facet_wrap(vars(cz, sys, c), scales = "free_y", ncol = 6) +
  labs(x = "log(GDPPC)", y = "logit(FEEDSHARE)") +
  theme_1
ggsave("../../../figs/fig6_4.png", g6_4, width = 18, height = 12, create.dir = T)
c_list <- c("cmt", "rmt", "mlk")
regresult <- as.data.frame(c_list)
regresult <- regresult %>% mutate(beta = 0, alpha_A = 0, alpha_T = 0, alpha_H = 0, alpha_Y = 0, r2 = 0)
for (j in c("LG", "MX")) {
  data6_4_sys <- data6_4 %>% filter(sys == j)
  for (i in c_list) {
    data6_4_c <- data6_4_sys %>% filter(c == i)
    regression <- lm(data = data6_4_c, logit_feed_share ~ -1 + log(GDPpc) + cz)
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