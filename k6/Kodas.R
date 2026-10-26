library(dplyr)
library(readr)
library(nlme)
library(ggplot2)
library(latex2exp)


## Duomenų paruošimas
ess_raw <- read.csv("Duomenys/ESS10.csv", stringsAsFactors = FALSE)
gdp_raw <- read.csv("Duomenys/GDP.csv", stringsAsFactors = FALSE)


gdp_clean <- gdp_raw %>%
  select(cntry = geo, gdp_pc = OBS_VALUE) %>%
  mutate(
    gdp_pc = as.numeric(gdp_pc),
    gdp_pc_k = gdp_pc / 1000, # BVP tūkstančiais EUR
    cntry = ifelse(cntry == "EL", "GR", cntry)
  ) %>%
  filter(!is.na(cntry) & !is.na(gdp_pc))


ess_selected <- ess_raw %>%
  select(cntry, stfgov, hinctnta, polintr, agea) %>%
  mutate(
    stfgov       = as.numeric(stfgov),
    hinctnta     = as.numeric(hinctnta),
    polintr      = as.numeric(polintr),
    agea         = as.numeric(agea),
    # 1 = visai nesidomi, 4 = labai domisi politika
    pol_interest_num = 5 - polintr
  ) %>%
  filter(
    !is.na(cntry) &
      !is.na(stfgov)           & stfgov <= 10 &
      !is.na(hinctnta)         & hinctnta <= 10 &
      !is.na(pol_interest_num) & pol_interest_num >= 1 & pol_interest_num <= 4 &
      !is.na(agea)             & agea <= 110
  ) %>%
  mutate(
    # Binarinis faktorius: 1-2 (visai nesidomi / šiek tiek) = nesidomi,
    # 3-4 (gana / labai domisi) = domisi; atskaitos lygis = "Nesidomi politika"
    pol_interest = factor(
      ifelse(pol_interest_num >= 3, "Domisi politika", "Nesidomi politika"),
      levels = c("Nesidomi politika", "Domisi politika")
    )
  )

df_final <- ess_selected %>%
  inner_join(gdp_clean, by = "cntry")

df_final

cat("Eilučių (respondentų) skaičius:", nrow(df_final), "\n")
cat("Šalių (II lygio aibių) skaičius:", length(unique(df_final$cntry)), "\n\n")

print(head(df_final))


## Aprašomoji statistika --------------
vars_summary <- df_final %>%
  select(stfgov, pol_interest, hinctnta, agea, gdp_pc_k) %>%
  summary()
print(vars_summary)

## Grafikas:
country_names <- c(
  "BG" = "Bulgarija", "HR" = "Kroatija", "MK" = "Š. Makedonija", "SK" = "Slovakija",
  "SI" = "Slovėnija", "ME" = "Juodkalnija", "GR" = "Graikija", "LT" = "Lietuva",
  "IT" = "Italija", "CZ" = "Čekija", "EE" = "Estija", "FR" = "Prancūzija",
  "BE" = "Belgija", "PT" = "Portugalija", "HU" = "Vengrija", "NL" = "Nyderlandai",
  "IE" = "Airija", "IS" = "Islandija", "NO" = "Norvegija", "FI" = "Suomija",
  "CH" = "Šveicarija"
)
overall_mean <- mean(df_final$stfgov)
plot_data <- df_final %>%
  group_by(cntry) %>%
  summarise(mean_stfgov = mean(stfgov), .groups = "drop") %>%
  mutate(
    country_label = country_names[cntry],
    status = ifelse(mean_stfgov >= overall_mean, "Aukštesnis nei vidurkis", "Žemesnis nei vidurkis")
  )
 
# p_clean <- ggplot(plot_data, aes(x = reorder(country_label, mean_stfgov), y = mean_stfgov, fill = status)) +
#   geom_col(width = 0.72) +
#   geom_hline(yintercept = overall_mean, linetype = "dashed", color = "black", linewidth = 0.7) +
#   geom_text(aes(label = sprintf("%.2f", mean_stfgov)),
#             hjust = -0.15, size = 3.6, fontface = "bold", color = "gray15") +
#   coord_flip() +
#   scale_y_continuous(limits = c(0, 8.2), breaks = seq(0, 8, by = 2), expand = c(0, 0)) +
#   scale_fill_manual(values = c("Aukštesnis nei vidurkis" = "#2b7bba",
#                                "Žemesnis nei vidurkis" = "#d95f02")) +
#   labs(
#     title = "Pasitenkinimas nacionaline vyriausybe (0–10 balų)",
#     subtitle = paste0("Punktyrinė linija žymi bendrą imties vidurkį (", round(overall_mean, 2), ")"),
#     x = NULL,
#     y = "Vidutinis balas",
#     fill = NULL
#   ) +
#   theme_minimal(base_size = 12) +
#   theme(
#     legend.position = "top",
#     legend.text = element_text(size = 10, face = "bold"),
#     axis.text.y = element_text(size = 10, face = "bold", color = "black"),
#     axis.text.x = element_text(size = 10),
#     panel.grid.major.y = element_blank(),
#     panel.grid.minor = element_blank(),
#     plot.title = element_text(face = "bold", size = 13),
#     plot.subtitle = element_text(size = 10, color = "gray30")
#   )
# p_clean
# ggsave("stfgov_presentation.png", plot = p_clean, width = 6.2, height = 6.0, dpi = 300)

## Pirmo lygmens koreliaciju matrica (naudojama skaitinė pol_interest versija)
cor_vars <- df_final %>% select(pol_interest_num, hinctnta, agea)
cor_mat <- cor(cor_vars, use = "complete.obs")
print(round(cor_mat, 3))


## Besąlyginis modelis
m0 <- lme(
  fixed  = stfgov ~ 1,
  random = ~ 1 | cntry,
  data   = df_final,
  method = "ML"
)

summary(m0)

## Dispersijos ir ICC
var_comp_m0 <- VarCorr(m0)
tau00_sq    <- as.numeric(var_comp_m0["(Intercept)", "Variance"])
sigma_sq_m0 <- as.numeric(var_comp_m0["Residual", "Variance"])
icc_val <- tau00_sq / (tau00_sq + sigma_sq_m0)

cat("\n================ DISPERSIJOS IR ICC ================\n")
cat(sprintf("Šalių lygmens dispersija (tau00^2) : %.4f\n", tau00_sq))
cat(sprintf("Individų lygmens dispersija (sigma^2): %.4f\n", sigma_sq_m0))
cat(sprintf("Besąlyginio modelio ICC            : %.4f (arba %.2f%%)\n", icc_val, icc_val * 100))
cat(sprintf("Modelio log-tikėtinumas (logLik)   : %.2f\n", logLik(m0)))
cat(sprintf("Modelio Akaikės indeksas (AIC)     : %.2f\n", AIC(m0)))


## Sudėtinis modelis
## pol_interest (binarinis faktorius) tik fiksuotoje dalyje
m1 <- lme(
  fixed   = stfgov ~ pol_interest + hinctnta + agea + gdp_pc_k,
  random  = ~ (1 + hinctnta + agea + pol_interest) | cntry,
  data    = df_final,
  method  = "ML"
)
AIC(m1)

## Modelis su sąveika
m2 <- lme(
  fixed   = stfgov ~ pol_interest * gdp_pc_k + hinctnta + agea,
  random  = ~ 1 + hinctnta + agea + pol_interest | cntry,
  data    = df_final,
  method  = "ML"
)

AIC(m2)
summary(m2)

VarCorr(m2)


## Modelių palyginimas
cat("\n=== Sudėtinis vs Besąlyginis ===\n")
print(anova(m0, m1))

cat("\n=== Sąveikos reikšmingumas (m1 vs m2) ===\n")
print(anova(m1, m2))

## Dispersijos ir kokybės rodiklių palyginimas (m0 -> m2)
var_comp_m2 <- VarCorr(m2)
tau00_sq_m2 <- as.numeric(var_comp_m2["(Intercept)", "Variance"])
sigma_sq_m2 <- as.numeric(var_comp_m2["Residual", "Variance"])

delta_aic <- AIC(m2) - AIC(m0)
r1_sq     <- (sigma_sq_m0 - sigma_sq_m2) / sigma_sq_m0
r2_sq     <- (tau00_sq - tau00_sq_m2) / tau00_sq

cat(sprintf("\nBesąlyginio modelio AIC: %.2f\n", AIC(m0)))
cat(sprintf("Galutinio modelio AIC:   %.2f\n", AIC(m2)))
cat(sprintf("AIC sumažėjimas:         %.2f\n", abs(delta_aic)))
cat(sprintf("I lygmens liekamoji dispersija sigma^2: m0 = %.4f -> m2 = %.4f\n", sigma_sq_m0, sigma_sq_m2))
cat(sprintf("I lygmens dispersijos sąlyginis sumažėjimas : %.2f%%\n", r1_sq * 100))
cat(sprintf("II lygmens dispersijos sąlyginis sumažėjimas: %.2f%%\n", r2_sq * 100))

# Fiksuotieji efektai (faktoriui sukuriama 1 dummy: pol_interestDomisi politika,
# ir 1 sąveika: pol_interestDomisi politika:gdp_pc_k)
cat("\nFiksuotieji efektai (m2):\n")
print(round(summary(m2)$tTable, 4))

# Q-Q grafikai
par(mar = c(5.5, 5.5, 4, 2) + 0.1)

# I lygmens liekanų Q-Q grafikas
qqnorm(residuals(m2, type = "normalized"),
       main = "I lygmens liekanų kvantilių grafikas",
       xlab = "Teoriniai kvantiliai",
       ylab = "Imties kvantiliai",
       pch = 16, col = rgb(0.17, 0.48, 0.73, 0.2), cex = 0.8,
       cex.main = 1.6,
       cex.lab  = 1.5,
       cex.axis = 1.1)
qqline(residuals(m2, type = "normalized"), col = "firebrick", lwd = 2)


# II lygmens atsitiktinių efektų (u_0) Q-Q grafikai
re <- ranef(m2)   # random effects by country

# A list is needed because TeX() returns expression objects
titles <- lapply(seq_along(re) - 1, function(k)
  TeX(sprintf("$u_{%d}$ kvantilių grafikas", k)))

op <- par(mfrow = c(2, 2),
          mar = c(5, 5, 4, 1.5) + 0.1)   # bottom, left, top, right

re <- ranef(m2)
for (i in seq_along(re)) {
  qqnorm(re[[i]],
         main = titles[i],
         xlab = "Teoriniai kvantiliai",
         ylab = "Imties kvantiliai",
         pch = 16, col = "#2b7bba", cex = 1.4,
         cex.main = 1.5, cex.lab = 1.2, cex.axis = 1.0)
  qqline(re[[i]], col = "firebrick", lwd = 2)
}

par(op)

par(mfrow = c(1, 1))
