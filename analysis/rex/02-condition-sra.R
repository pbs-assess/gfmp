# Plan:
# - drop 150 ceq scenario
# - move the 50 ceq noncpue scenario to the robustness set
# - add a scenario with 250% ceq

FRENCH <- TRUE
if (FRENCH) options(OutDec =  ",")

library("dplyr")
library("DLMtool")
library("MSEtool")
library("here")
library("purrr")
library("ggplot2")
library("future")
cores <- floor(future::availableCores()/2)
plan(multisession, workers = cores)

rex_om <- readRDS(here("generated-data", "rex-om.rds"))
all_years <- seq(1996, 2019)
nyear <- length(all_years)

fig_dir <- here("report", "figure")
if (!dir.exists(fig_dir)) dir.create(fig_dir)

drex <- readRDS(here("generated-data", "rex-filter-data.rds"))
d_catch <- readRDS(here::here("generated-data", "rex-catch2.rds"))
drex$catch <- dplyr::filter(d_catch, year >= 1996, year <= 2019)

nsim <- rex_om@nsim

# Just in case:
rex_om@Msd <- c(0, 0)
rex_om@Linfsd <- c(0, 0)
rex_om@Ksd <- c(0, 0)
rex_om@Cobs <- c(0, 0)
rex_om@Cbiascv <- c(0, 0)
rex_om@D <- c(0.3, 0.8) # gets replaced

stopifnot(identical(rex_om@nyears, length(all_years)))

make_cal <- function(dat, survey, yrs, length_bin = 2) {
  cal <- dat %>%
    dplyr::filter(survey_abbrev == survey, sex == 2) %>%
    gfdlm::tidy_cal(yrs = yrs, interval = length_bin)

  length_bins <- gfdlm:::get_cal_bins(cal, length_bin_interval = length_bin)
  list(cal = cal[1, , ], length_bins = length_bins)
}

cal_wcvi <- make_cal(drex$survey_samples, "SYN WCVI", yrs = all_years)
# MSEtool::plot_composition(all_years,
#   obs = cal_wcvi$cal,
#   CAL_bins = cal_wcvi$length_bins
# )

catch <- drex$catch %>%
  gfplot::tidy_catch() %>%
  group_by(year) %>%
  summarize(value = sum(value)) %>%
  right_join(tibble(year = all_years), by = "year") %>%
  pull(value)
# plot(all_years, catch, type = "o")

# d_cpue <- readRDS("/Volumes/Extreme-SSD/gfs/report/data-cache/cpue-index-dat.rds")
# d_cpue <- gfdata::get_cpue_index()
# saveRDS(d_cpue, file = here::here("generated-data", "trawl-cpue-data.rds"))
# ind <- gfsynopsis::fit_cpue_indices(dat = d_cpue,
#   species = "rex sole", areas = "3[CD]",
#   cache = here::here("generated-data"),
#   year_range = c(1996, 2019),
#   save_model = TRUE)
# saveRDS(ind, file =  here::here("generated-data", "rex-cpue-index.rds"))
cpue <- readRDS(here::here("generated-data", "rex-cpue-index.rds"))
# ggplot(cpue, aes(year, est)) + geom_line() + facet_wrap(~model) +
  # geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.5)

indexes <- drex$survey_index %>%
  dplyr::filter(survey_abbrev %in% c("SYN WCVI")) %>%
  select(year, biomass, re) %>%
  right_join(tibble(year = all_years),by  = "year") %>%
  left_join(rename(select(cpue, year, est, se_link), trawl_cpue = est, trawl_sd = se_link), by = "year") %>%
  rename(syn_wcvi = biomass, syn_wcvi_sd = re)

# Set up alternative OMs for reference set and robustness set -----------------

saveRDS(indexes, file = here("generated-data/rex-indexes.rds"))

# matching maturity, approximately:
.commercial_vul <- c(32, 22, 1)
.surv_vul <- c(32, 22, 1)

# # maturity ogive:
# mean(vb_post$linf) * (1-exp(-mean(vb_post$k) * (2 - mean(vb_post$t0))))
# mean(vb_post$linf) * (1-exp(-mean(vb_post$k) * (4.5 - mean(vb_post$t0))))
# mean(vb_post$linf) * (1-exp(-mean(vb_post$k) * (8 - mean(vb_post$t0))))

fit_sra_rex_cpue <- function(om,
  c_eq = 2,
  commercial_vul = .commercial_vul,
  surv_vul = .surv_vul,
  ...) {
  MSEtool::SRA_scope(om,
    Chist = catch,
    C_eq = c_eq * catch[1],
    Index = cbind(indexes$syn_wcvi, indexes$trawl_cpue),
    I_sd = cbind(indexes$syn_wcvi_sd, indexes$trawl_sd * 1.5),
    I_type = c("est", 1),
    s_selectivity = c("logistic", "logistic"),
    cores = 1,
    drop_nonconv = TRUE,
    vul_par = matrix(commercial_vul, 3, 1),
    s_vul_par = matrix(c(surv_vul, commercial_vul), 3, 2),
    map_vul_par = matrix(NA, 3, 1),
    map_s_vul_par = matrix(NA, 3, 2),
    f_name = "Commercial trawl",
    drop_highF = TRUE,
    s_name = c("SYN WCVI", "Commercial CPUE"),
    mean_fit = TRUE,
    ...
  )
}

rex_sra_ceq200 %<-% fit_sra_rex_cpue(rex_om, c_eq = 2)
rex_sra_ceq250 %<-% fit_sra_rex_cpue(rex_om, c_eq = 2.5)
# plot(rex_sra_ceq200)
# plot(rex_sra_ceq250)

# Alternative Reference Set OMs: M --------------------------------------------
# Only look at higher M and time-varying M. M in BC unlikely to be lower than GOA.

rex_om@M
rex_om_high_m <- rex_om
rex_om_high_m@M <- c(0.30, 0.30)
rex_sra_high_m %<-% fit_sra_rex_cpue(rex_om_high_m)
# plot(rex_sra_high_m)

# Alternative Reference Set OMs: Steepness (h) --------------------------------

rex_om_high_h <- rex_om
quantile(rex_om@cpars$h)
rex_om_high_h@cpars$h <- rep(0.95, length(rex_om@cpars$h))
rex_sra_high_h %<-% fit_sra_rex_cpue(rex_om_high_h)

# No CPUE ---------------------------------------------------------------------

fit_sra_rex_no_cpue <- function(om,
  c_eq = 2,
  commercial_vul = .commercial_vul,
  surv_vul = .surv_vul,
  ...) {
  MSEtool::SRA_scope(om,
    Chist = catch,
    C_eq = c_eq * catch[1],
    Index = cbind(indexes$syn_wcvi),
    I_sd = cbind(indexes$syn_wcvi_sd),
    I_type = c("est"),
    s_selectivity = c("logistic"),
    cores = 1,
    drop_nonconv = TRUE,
    vul_par = matrix(commercial_vul, 3, 1),
    s_vul_par = matrix(c(surv_vul), 3, 1),
    map_vul_par = matrix(NA, 3, 1),
    map_s_vul_par = matrix(NA, 3, 1),
    f_name = "Commercial trawl",
    drop_highF = TRUE,
    s_name = c("SYN WCVI"),
    mean_fit = TRUE,
    ...
  )
}

rex_sra_no_cpue %<-% fit_sra_rex_no_cpue(rex_om, c_eq = 3)
# plot(rex_sra_no_cpue)

# No CPUE and lightly fished before 1996 --------------------------------------

rex_sra_light %<-% fit_sra_rex_no_cpue(rex_om, c_eq = .5, commercial_vul = c(28, 17, 1), surv_vul = c(28, 17, 1))
# plot(rex_sra_light)

# Shift commercial selectivity curve left -------------------------------------

rex_sra_sel1 %<-%
  fit_sra_rex_cpue(rex_om, commercial_vul = c(28, 17, 1), surv_vul = c(28, 17, 1))
# plot(rex_sra_sel1)

# Robustness Set OMs: M increasing over time ----------------------------------

rex_om_inc_m <- rex_om
rex_om_inc_m@nyears
rex_om_inc_m@proyears
M_age <- array(0.2,
  c(rex_om_inc_m@nsim, rex_om_inc_m@maxage, rex_om_inc_m@nyears + rex_om_inc_m@proyears))
m <- seq(0.2, 0.4, length.out = rex_om_inc_m@proyears)
for (i in seq_along(m)) M_age[, , rex_om_inc_m@nyears + i] <- m[i]
# plot(M_age[1,1,]) # one sim; one age
rex_om_inc_m@cpars$M_ageArray <- M_age
rex_sra_inc_m %<-% fit_sra_rex_cpue(rex_om_inc_m)

# Save SRAs -------------------------------------------------------------------

saveRDS(rex_sra_inc_m, file = here("generated-data", "rex-sra-inc-m.rds"))
saveRDS(rex_sra_sel1, file = here("generated-data", "rex-sra-sel1.rds"))
saveRDS(rex_sra_ceq200, file = here("generated-data", "rex-sra-ceq200.rds"))
saveRDS(rex_sra_ceq250, file = here("generated-data", "rex-sra-ceq250.rds"))
saveRDS(rex_sra_high_m, file = here("generated-data", "rex-sra-high-m.rds"))
saveRDS(rex_sra_high_h, file = here("generated-data", "rex-sra-high-h.rds"))
saveRDS(rex_sra_no_cpue, file = here("generated-data", "rex-sra-no-cpue.rds"))
saveRDS(rex_sra_light, file = here("generated-data", "rex-sra-no-cpue-light.rds"))

# Set up the scenario names ---------------------------------------------------

sc <- tibble::tribble(
  ~scenario, ~scenario_human, ~scenario_type,
  "ceq200", "Ceq. 200%", "Reference",
  "ceq250", "Ceq. 250%", "Reference",
  "high-m", "Higher M", "Reference",
  "high-h", "Higher\nsteepness", "Reference",
  "sel1", "Lower\nselectivity", "Reference",
  "no-cpue", "No CPUE\nCeq. 250%", "Reference",
  "no-cpue-light", "No CPUE\nCeq. 50%", "Robustness",
  "inc-m", "M\nincreasing", "Robustness"
)

if (FRENCH) {
  sc$scenario_human <- c(
    "Ceq. 200%",
    "Ceq. 250%",
    "M plus\nélevée",
    "Taux de variation\nplus élevé",
    "Plus faible\nsélectivité",
    "Pas de CPUE\nCeq. 250%",
    "Pas de CPUE\nCeq. 50%",
    "M\ncroissante")
}

sc <- mutate(sc, order = seq_len(n()))
saveRDS(sc, file = here("generated-data/rex-scenarios.rds"))
sc$scenario_human <- paste0(sc$order, " - ", sc$scenario_human)

sra_rex <- purrr::map(sc$scenario, ~ {
  readRDS(here("generated-data", paste0("rex-sra-", .x, ".rds")))
})
names(sra_rex) <- sc$scenario

# Some plots ------------------------------------------------------------------

# FIXME: get this into gfdlm:
get_depletion <- function(x, scenario) {
  depletion <- x@SSB / sapply(x@Misc, getElement, "E0_SR")
  last_year <- dim(depletion)[2]
  all_years <- seq(x@OM@CurrentYr - x@OM@nyears + 1, x@OM@CurrentYr)

  d1 <- t(apply(depletion[, -last_year], 2,
    FUN = quantile,
    probs = c(0.025, 0.5, 0.975)
  )) %>%
    as.data.frame() %>%
    cbind(all_years) %>%
    mutate(scenario = scenario) %>%
    rename(lwr = 1, med = 2, upr = 3, year = all_years)
  d2 <- t(apply(depletion[, -last_year], 2,
    FUN = quantile,
    probs = c(0.25, 0.75))) %>%
    as.data.frame() %>%
    cbind(all_years) %>%
    rename(lwr50 = 1, upr50 = 2, year = all_years)

  left_join(d1, d2, by = "year")
}

get_F <- function(x, scenario) {

  .F1 <- map(x@Misc, "F_at_age")
  .F <- map_dfc(.F1, ~tibble(.F = apply(.x, 1, max)))
  .F <- t(as.matrix(.F))

  last_year <- dim(.F)[2]
  all_years <- seq(x@OM@CurrentYr - x@OM@nyears + 1, x@OM@CurrentYr)
  all_years <- all_years #[-length(all_years)]

  d1 <- t(apply(.F[, ], 2,
    FUN = quantile,
    probs = c(0.025, 0.5, 0.975)
  )) %>%
    as.data.frame() %>%
    cbind(all_years) %>%
    mutate(scenario = scenario) %>%
    rename(lwr = 1, med = 2, upr = 3, year = all_years)
  d2 <- t(apply(.F[, ], 2,
    FUN = quantile,
    probs = c(0.25, 0.75))) %>%
    as.data.frame() %>%
    cbind(all_years) %>%
    rename(lwr50 = 1, upr50 = 2, year = all_years)

  left_join(d1, d2, by = "year")
}

get_Perr_y <- function(x, scenario) {
  max_age <- x@OM@maxage
  nyears <- x@OM@nyears
  perr_y <- x@OM@cpars$Perr_y[,max_age:(max_age+nyears-1), drop=FALSE]
  all_years <- seq(x@OM@CurrentYr - x@OM@nyears + 1, x@OM@CurrentYr)
  reshape2::melt(perr_y) %>%
    rename(iteration = Var1) %>%
    mutate(year = rep(all_years, each = max(iteration))) %>%
    mutate(scenario = scenario)
}

if (!FRENCH) {
  fig_dir <- "report/figure"
} else {
  fig_dir <- "report-french/figure"
}

g <- purrr::map2_df(sra_rex, sc$scenario_human, get_depletion) %>%
  mutate(scenario = factor(scenario, levels = sc$scenario_human)) %>%
  ggplot(aes(year, med, ymin = lwr, ymax = upr)) +
  geom_ribbon(fill = "grey90") +
  geom_ribbon(fill = "grey70", mapping = aes(ymin = lwr50, ymax = upr50)) +
  geom_line() +
  facet_wrap(vars(scenario)) +
  gfplot::theme_pbs() +
  labs(x = en2fr("Year", FRENCH), y = en2fr("Depletion", FRENCH)) +
  coord_cartesian(ylim = c(0, 1), expand = FALSE)
ggsave(here::here(paste0(fig_dir, "/rex-compare-SRA-depletion-panel.png")),
  width = 8, height = 6.75
)
g <- purrr::map2_df(sra_rex, sc$scenario_human, get_F) %>%
  mutate(scenario = factor(scenario, levels = sc$scenario_human)) %>%
  ggplot(aes(year, med, ymin = lwr, ymax = upr)) +
  geom_ribbon(fill = "grey90") +
  geom_ribbon(fill = "grey70", mapping = aes(ymin = lwr50, ymax = upr50)) +
  geom_line() +
  facet_wrap(vars(scenario)) +
  gfplot::theme_pbs() +
  labs(x = en2fr("Year", FRENCH), y = "F") +
  coord_cartesian(ylim = c(0, 1.8), expand = FALSE)
ggsave(here::here(fig_dir, "rex-compare-F-panel.png"),
  width = 8, height = 6.75
)
g <- purrr::map2_df(sra_rex, sc$scenario_human, get_Perr_y) %>%
  mutate(scenario = factor(scenario, levels = sc$scenario_human)) %>%
  dplyr::filter(iteration %in% 1:100) %>%
  ggplot(aes(year, y = log(value), group = iteration)) +
  geom_line(alpha = 0.1) +
  facet_wrap(vars(scenario)) +
  gfplot::theme_pbs() +
  labs(x = en2fr("Year", FRENCH),
    y = if (!FRENCH) "Recruitment deviation in log space" else "Écart de recrutement") +
  coord_cartesian(ylim = c(-1.5, 1.7), expand = FALSE) +
  geom_hline(yintercept = 0, lty = 2, alpha = 0.6)
# g
ggsave(here::here(fig_dir, "rex-compare-recdev-panel.png"),
  width = 8, height = 6.75
)

g <- sra_rex %>% set_names(sc$scenario_human) %>%
  gfdlm::plot_index_fits(survey_names = c("SYN WCVI", "Commercial CPUE"), french = FRENCH) +
  coord_cartesian(ylim = c(0, 2.5), expand = FALSE) +
  scale_y_continuous(breaks = seq(0, 2, .5))
ggsave(here::here(fig_dir, "rex-index-fits.png"), width = 5.5, height = 9.5)

# FIXME: get this into gfdlm:
get_sra_selectivity <- function(sc_name) {
  sra <- sra_rex[[sc_name]]
  x <- do.call(cbind, purrr::map(sra@Misc, ~ .$vul_len))
  out <- reshape2::melt(x) %>%
    rename(Length = Var1, iter = Var2)
  out$scenario <- sc_name
  out
}
sel <- map_dfr(sc$scenario[1], get_sra_selectivity) # pick one
sel %>%
  ggplot(aes(Length, value, group = paste(iter))) +
  geom_line(alpha = 0.15) +
  gfplot::theme_pbs() +
  ylab(if (!FRENCH) "Selectivity" else "Sélectivité") + xlab(en2fr("Length", FRENCH)) +
  coord_cartesian(expand = FALSE, ylim = c(-0.01, 1.01))
ggsave(here::here(fig_dir, "rex-selectivity.png"), width = 5, height = 3)

# sra_rex[[1]]@Misc[[1]]$s_vul[1,,1]
# sra_rex[[1]]@Misc[[1]]$s_vul[1,,2]

# for report ---------------------------------

sp <- "rex"
scenarios <- sc$scenario %>% purrr::set_names(sc$scenario_human)
oms <- map(scenarios, ~ {
  readRDS(here("generated-data", paste0(sp, "-sra-", .x, ".rds")))@OM
})
rex_converged <- map_dfr(oms, ~tibble(nsim = .x@nsim), .id = "scenario")
saveRDS(rex_converged, file = here("generated-data/rex-converged.rds"))

sc2 <- readRDS(here("generated-data", "rex-scenarios.rds"))
sc2$scenario_human <- paste0(sc2$order, " - ", sc2$scenario_human)

if (FRENCH) {
  sc2$scenario_human <- c(
    "1 - Ceq. 200%",
    "2 - Ceq. 250%",
    "3 - M plus\nélevée",
    "4 - Taux de variation\nplus élevé",
    "5 - Plus faible\nsélectivité",
    "6 - Pas de CPUE\nCeq. 250%",
    "7 - Pas de CPUE\nCeq. 50%",
    "8 - M\ncroissante")
}

x <- oms %>% set_names(sc2$scenario_human) %>%
  map_dfr(~tibble(
    D = .x@cpars$D,
    h = .x@cpars$h,
    R0 = .x@cpars$R0,
    sigma_R = .x@cpars$Perr,
    AC = .x@cpars$AC,
    L50 = .x@cpars$L50,
    L50_95 = .x@cpars$L50_95,
    t0 = .x@cpars$t0,
    k = .x@cpars$K,
    Linf = .x@cpars$Linf,
    M = .x@cpars$M_ageArray[,1,1],
  ), .id = "Scenario") %>%
  reshape2::melt(id.vars = "Scenario") %>%
  dplyr::filter(!(variable == "R0" & value > 1e7))

# x %>% dplyr::filter(variable %in% c("R0", "AC", "D")) %>%
#   ggplot(aes(value)) +
#   geom_histogram(bins = 40, colour = "grey60") +
#   facet_grid(Scenario~variable, scales = "free_x")+
#   theme_pbs() +
#   coord_cartesian(ylim = c(0, 200), expand = FALSE) +
#   xlab("Parameter value") +
#   theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank())

x %>% dplyr::filter(variable %in% c("R0", "AC", "D")) %>%
  ggplot(aes(value)) +
  geom_histogram(bins = 30, colour = "grey40", fill = "white", lwd = 0.4) +
  # geom_freqpoly(aes(colour = Scenario), bins = 30) +
  facet_grid(Scenario~variable, scales = "free_x")+
  gfdlm::theme_pbs() +
  coord_cartesian(ylim = c(0, 200), expand = FALSE) +
  xlab(en2fr("Parameter value", FRENCH)) + ylab(en2fr("Count", FRENCH)) +
  # theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank()) +
  scale_colour_brewer(palette = "Dark2")

ggsave(here::here(fig_dir, "rex-sra-estimated.png"),
  width = 6.5, height = 8.5)

x %>% dplyr::filter(variable %in% c("sigma_R", "h", "L50", "L50_95", "t0", "k", "Linf", "M")) %>%
  ggplot(aes(value)) +
  # geom_histogram(bins = 40, colour = "grey60") +
  geom_freqpoly(aes(colour = Scenario), bins = 30) +
  facet_wrap(~variable, scales = "free_x")+
  gfdlm::theme_pbs() +
  coord_cartesian(ylim = c(0, 200), expand = FALSE) +
  xlab(en2fr("Parameter value", FRENCH)) + ylab(en2fr("Count", FRENCH)) +
  # theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.title.y = element_blank()) +
  scale_colour_brewer(palette = "Dark2")

ggsave(here::here(fig_dir, "rex-sra-filtered.png"),
  width = 6.5, height = 5.5)

# Substantially speeds up LaTeX rendering on a Mac
# by pre-optimizing the PNG compression:
optimize_png <- TRUE
if (optimize_png && !identical(.Platform$OS.type, "windows")) {
  files_per_core <- 4
  if (!FRENCH) {
    setwd("report/figure")
  } else {
    setwd("report-french/figure")
}
  system(paste0(
    "find -X . -name 'rex-*.png' -print0 | xargs -0 -n ",
    files_per_core, " -P ", cores, " optipng -strip all"
  ))
  setwd(here())
}
