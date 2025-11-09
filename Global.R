rm(list = ls())

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(stringr)
})

# ---------- DADOS ----------
plas <- tibble::tribble(
  ~Term, ~Value, ~lower.CI, ~upper.CI,
  "Intercept", -3.93, -6.53, -1.43,
  "Body mass", 0.40, 0.20, 0.59,
  "Species abundance", 0.01, -0.03, 0.06,
  "Forest cover", -0.10, -0.17, -0.04,
  "PCHB1", 0.01, -0.09, 0.11,
  "Wetland cover", 0.04, -0.01, 0.09,
  "Percent foraging in the canopy", -0.01, -0.04, 0.03,
  "Climate_2", -0.01, -0.01, -0.00,
  "Elevation", -0.26, -0.32, -0.21,
  "BSA", -0.04, -0.14, 0.05,
  "Species diversity", -0.02, -0.05, 0.02,
  "Haemoproteus co-occurrence", -2.06, -3.02, -1.26,
  "Parahaemoproteus co-occurrence", -1.80, -1.93, -1.66,
  "Latitude", -0.00, -0.00, 0.00
) %>% mutate(Response = "Plasmodium")

haemo <- tibble::tribble(
  ~Term, ~Value, ~lower.CI, ~upper.CI,
  "Intercept", -5.12, -12.04, 2.03,
  "Migration distance", -0.26, -1.05, 0.47,
  "Body mass", -0.18, -0.89, 0.47,
  "Forest cover", -0.08, -0.36, 0.21,
  "Species abundance", 0.05, -0.23, 0.14,
  "PCHB1", 0.30, -0.15, 0.77,
  "Percent foraging in the canopy", -0.34, -0.81, 0.11,
  "Wetland cover", -0.11, -0.33, 0.10,
  "Climate_2", -0.01, -0.03, 0.01,
  "Climate_4", -0.01, -0.10, 0.08,
  "Elevation", 0.29, 0.09, 0.48,
  "Species diversity", 0.00, -0.14, 0.15,
  "BSA", 0.13, -0.27, 0.55,
  "Plasmodium co-occurrence", -1.75, -2.76, -0.93,
  "Latitude", -0.02, -0.03, -0.01
) %>% mutate(Response = "Haemoproteus")

para <- tibble::tribble(
  ~Term, ~Value, ~lower.CI, ~upper.CI,
  "Intercept", -1.23, -4.70, 1.94,
  "Body mass", 0.18, -0.09, 0.42,
  "Forest cover", -0.21, -0.29, -0.14,
  "Species abundance", -0.08, -0.13, -0.04,
  "PCHB1", 0.13, 0.01, 0.25,
  "Percent foraging in the canopy", 0.21, 0.11, 0.30,
  "Wetland cover", 0.04, -0.00, 0.08,
  "Species diversity", 0.00, -0.04, 0.04,
  "BSA", -0.07, -0.19, 0.04,
  "Plasmodium occurrence", -1.79, -1.93, -1.65,
  "Latitude", 0.03, 0.03, 0.03
) %>%
  mutate(Response = "Parahaemoproteus",
         Term = if_else(Term == "Plasmodium occurrence", "Plasmodium co-occurrence", Term))

# ---------- MERGE + LIMPEZA ----------
merged_results <- bind_rows(plas, haemo, para) %>%
  filter(Term != "Intercept") %>%
  mutate(
    lower.CI = pmin(lower.CI, upper.CI),
    upper.CI = pmax(lower.CI, upper.CI),
    across(c(Value, lower.CI, upper.CI), ~ ifelse(abs(.) < .Machine$double.eps, 0, .)),
    sig = ifelse((lower.CI > 0 & upper.CI > 0) | (lower.CI < 0 & upper.CI < 0),
                 "Significant", "Not significant"),
    is_cooc = ifelse(str_detect(Term, "co-occurrence"), "Co-occurrence", "Other")
  )

# ---------- RELABEL CLIMATE VARS ----------
merged_results <- merged_results %>%
  mutate(Term = dplyr::recode(Term,
                              "Climate_2" = "Annual precipitation (Bio12)",
                              "Climate_4" = "Precipitation seasonality (Bio15)"))

term_levels <- c(
  "Migration distance",
  "Body mass","Species abundance","Forest cover","PCHB1",
  "Percent foraging in the canopy","Wetland cover",
  "Annual precipitation (Bio12)","Precipitation seasonality (Bio15)",
  "Elevation","Species diversity","BSA","Latitude",
  "Haemoproteus co-occurrence","Parahaemoproteus co-occurrence","Plasmodium co-occurrence"
)
merged_results$Term <- factor(merged_results$Term,
                              levels = term_levels[term_levels %in% merged_results$Term])

# ---------- ESCALA PROPORCIONAL ----------
M <- max(abs(c(merged_results$lower.CI,
               merged_results$upper.CI,
               merged_results$Value)), na.rm = TRUE)
stopifnot(is.finite(M) && M > 0)
sf <- 1 / M

merged_results <- merged_results %>%
  mutate(
    Value_s = Value * sf,
    lower_s = lower.CI * sf,
    upper_s = upper.CI * sf
  )

# ---------- EXPRESSÕES PARA EIXO Y ----------
term_labels_expr <- lapply(levels(merged_results$Term), function(x) {
  if (x %in% c("Haemoproteus co-occurrence", "Parahaemoproteus co-occurrence", "Plasmodium co-occurrence")) {
    bquote(italic(.(x)))
  } else {
    x
  }
})
names(term_labels_expr) <- levels(merged_results$Term)

# ---------- EXPRESSÕES PARA LEGENDA ----------
labels_expr <- c(
  "Plasmodium" = expression(italic("Plasmodium")),
  "Haemoproteus" = expression(italic("Haemoproteus")),
  "Parahaemoproteus" = expression(italic("Parahaemoproteus"))
)

# ---------- PLOT ----------
pd <- position_dodge(width = 0.6)

p <- ggplot(merged_results,
            aes(y = Term, x = Value_s, color = Response, alpha = sig, shape = is_cooc)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(size = 3, position = pd) +
  geom_errorbarh(aes(xmin = lower_s, xmax = upper_s), height = 0.22, position = pd) +
  scale_alpha_manual(values = c("Significant" = 1, "Not significant" = 0.35)) +
  scale_shape_manual(values = c("Other" = 16, "Co-occurrence" = 17)) +
  scale_color_manual(
    values = c("Plasmodium" = "#7570B3", "Haemoproteus" = "#D95F02", "Parahaemoproteus" = "#1B9E77"),
    labels = labels_expr
  ) +
  scale_y_discrete(labels = term_labels_expr) +
  scale_x_continuous(breaks = seq(-1, 1, 0.5), limits = c(-1, 1)) +
  labs(x = "Estimate (posterior mean ± 95% CrI), proportional scale centered at 0 [-1, 1]",
       y = NULL, shape = NULL, alpha = NULL, color = NULL) +
  guides(color = guide_legend(override.aes = list(shape = 16))) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 11),
    axis.text.y = element_text(size = 11)
  )

print(p)

# ---------- SALVAR

# ---------- SALVAR ----------
ggsave("/forest_haemosporidians.png", p, width = 12, height = 6, dpi = 300, bg = "white")
