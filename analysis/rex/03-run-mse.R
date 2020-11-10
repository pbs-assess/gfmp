library("DLMtool")
library("MSEtool")
library("dplyr")
library("purrr")
library("ggplot2")
library("gfdlm")
library("here")
library("assertthat")
# load_all("../gfdlm")

cores <- floor(parallel::detectCores() / 2L)
# future::plan(future::multiprocess, workers = cores)
future::plan(future::sequential)

# Settings --------------------------------------------------------------------

sp <- "rex" # Species: used in filenames
sc <- readRDS(here("generated-data", "rex-scenarios.rds"))
sc$scenario_human <- paste0(sc$order, " - ", sc$scenario_human)
sc # look good?
nsim <- 250L
interval <- 2L
mp <- suppressMessages(readr::read_csv(here("analysis", "rex", "mp.txt"), comment = "#"))
as.data.frame(mp) # look good?
reference_mp <- c("FMSYref75", "NFref", "FMSYref")
ref_mp_cols <- c("grey45", "grey10", "grey75") %>% set_names(reference_mp)

catch_breaks <- seq(0, 600000, 100000)
catch_labels <- catch_breaks / 1000

# Set up PMs ------------------------------------------------------------------

d_catch <- readRDS(here("generated-data", "rex-catch2.rds"))
d_catch <- dplyr::filter(d_catch, year >= 1996, year <= 2019)
catch <- suppressWarnings(gfplot::tidy_catch(d_catch))
catch <- catch %>%
  group_by(year) %>%
  summarize(value = sum(value)) %>%
  pull(value)

yrs <- length(catch)
ref_aadc <- gfdlm:::get_aadc(catch[(yrs - 10 + 1):yrs])
ref_catch <- mean(catch[(yrs - 5 + 1):yrs])

`LT LRP` <- gfdlm::pm_factory("SBMSY", 0.4, c(36, 50))
`LT USR` <- gfdlm::pm_factory("SBMSY", 0.8, c(36, 50))
FMSY <- DLMtool::PNOF
STC <- gfdlm::pm_factory("LTY", 1, c(1, 10))
LTC <- gfdlm::pm_factory("LTY", 1, c(36, 50))
AADC <- gfdlm::pm_factory("AADC", ref_aadc, c(1, 50))
PM <- c("LT LRP", "LT USR", "FMSY", "STC", "LTC", "AADC")

# Set up and checks -----------------------------------------------------------

stopifnot(all(reference_mp %in% mp$mp))
fig_dir <- here("report", "figure")
if (!dir.exists(fig_dir)) dir.create(fig_dir)
.ggsave <- function(filename, width, height, ...) {
  ggsave(file.path(fig_dir, paste0(sp, "-", filename, ".png")),
    width = width, height = height, ...
  )
}

get_filtered_scenario <- function(type, column) {
  dplyr::filter(sc, scenario_type == type) %>%
    pull(!!column) %>%
    set_names()
}
scenarios <- sc$scenario %>% set_names()
scenarios_ref <- get_filtered_scenario("Reference", "scenario")
scenarios_rob <- get_filtered_scenario("Robustness", "scenario")

# Read OMs --------------------------------------------------------------------

om <- map(scenarios, ~ {
  om <- readRDS(here("generated-data", paste0(sp, "-sra-", .x, ".rds")))@OM
  if (om@nsim < nsim) {
    stop("nsim set larger than in conditioned OM.", call. = FALSE)
  }
  om@nsim <- nsim
  om@interval <- interval
  om <- gfdlm::sample_AddIerr(om)
  om
})

# Fit MPs to all OMs ----------------------------------------------------------

oddify <- function(x) seq(2, x, by = 2)
.Iratio2 <- use_AddInd(reduce_survey(Iratio2, index = oddify))
.GB_slope6_0.66 <- use_AddInd(reduce_survey(GB_slope6_0.66, index = oddify))
.GB_slope6_1 <- use_AddInd(reduce_survey(GB_slope6_1, index = oddify))
.GB_slope8_0.66 <- use_AddInd(reduce_survey(GB_slope8_0.66, index = oddify))
.GB_slope8_1 <- use_AddInd(reduce_survey(GB_slope8_1, index = oddify))

.Islope0.2_80 <- use_AddInd(reduce_survey(Islope0.2_80, index = oddify))
.Islope0.2_100 <- use_AddInd(reduce_survey(Islope0.2_100, index = oddify))
.Islope0.4_80 <- use_AddInd(reduce_survey(Islope0.4_80, index = oddify))
.Islope0.4_100 <- use_AddInd(reduce_survey(Islope0.4_100, index = oddify))

.IDX <- use_AddInd(reduce_survey(IDX, index = oddify))
.IDX_smooth <- use_AddInd(reduce_survey(IDX_smooth, index = oddify))

.IT10_hist <- use_AddInd(reduce_survey(IT10_hist, index = oddify))
.IT5_hist <- use_AddInd(reduce_survey(IT5_hist, index = oddify))

.Itarget_base <- use_AddInd(reduce_survey(Itarget_base, index = oddify))
.Itarget_w0.8 <- use_AddInd(reduce_survey(Itarget_w0.8, index = oddify))
.Itarget_x0.2 <- use_AddInd(reduce_survey(Itarget_x0.2, index = oddify))
.Itarget_x0.8 <- use_AddInd(reduce_survey(Itarget_x0.8, index = oddify))
.Itarget_d1.2 <- use_AddInd(reduce_survey(Itarget_d1.2, index = oddify))
.Itarget_d0.8 <- use_AddInd(reduce_survey(Itarget_d0.8, index = oddify))

.ITM_hist <- use_AddInd(reduce_survey(ITM_hist, index = oddify))

.SP4010_0.6 <- SP4010_gf %>%
  add_SP_prior(r_prior = c(0.6, 0.1), initial_tac = ref_catch) %>%
  reduce_survey(index = oddify) %>%
  use_AddInd()
.SP8040_0.6 <- SP8040_gf %>%
  add_SP_prior(r_prior = c(0.6, 0.1), initial_tac = ref_catch) %>%
  reduce_survey(index = oddify) %>%
  use_AddInd()
.SP6040_0.6 <- SP6040_gf %>%
  add_SP_prior(r_prior = c(0.6, 0.1), initial_tac = ref_catch) %>%
  reduce_survey(index = oddify) %>%
  use_AddInd()
.SP6040_0.4 <- SP6040_gf %>%
  add_SP_prior(r_prior = c(0.4, 0.1), initial_tac = ref_catch) %>%
  reduce_survey(index = oddify) %>%
  use_AddInd()
.SP6040_0.5 <- SP6040_gf %>%
  add_SP_prior(r_prior = c(0.5, 0.1), initial_tac = ref_catch) %>%
  reduce_survey(index = oddify) %>%
  use_AddInd()
.SP6040_0.6_fox <- SP6040_gf %>%
  add_SP_prior(r_prior = c(0.6, 0.1), initial_tac = ref_catch, start = list(n = 1)) %>%
  reduce_survey(index = oddify) %>%
  use_AddInd()

missing <- mp$mp[!map_lgl(mp$mp, exists)]
assert_that(length(missing) == 0,
  msg = glue::glue(
    "{paste(missing, collapse = ', ')} do not exist in the current environment."
  )
)

fit_scenario <- function(scenario, mp_vec, id = "", sp = "rex") {
  if (id != "") id <- paste0(id, "-")
  file_name <- here::here("generated-data", paste0(sp, "-mse-", id, scenario, ".rds"))
  if (!file.exists(file_name)) {
    cat("Running closed-loop-simulation for", scenario, "OM\n")
    o <- om[[scenario]]
    mse <- DLMtool::runMSE(OM = o, MPs = mp_vec, parallel = FALSE)
    saveRDS(mse, file = file_name)
  } else {
    cat("Loading closed-loop-simulation for", scenario, "OM\n")
    mse <- readRDS(file_name)
  }
  mse
}

fo <- furrr::furrr_options(
  packages = c("gfdlm", "DLMtool", "MSEtool", "here"),
  globals = c("ref_catch", "om", mp$mp)
)
mse_cc <- furrr::future_map(scenarios, fit_scenario,
  mp_vec = mp$mp[grepl("^Constant", mp$type)], id = "cc", .options = fo)
mse_ind <- furrr::future_map(scenarios, fit_scenario,
  mp_vec = mp$mp[grepl("^Index", mp$type)], id = "ind", .options = fo)
mse_sp <- furrr::future_map(scenarios, fit_scenario,
  mp_vec = mp$mp[grepl("^Surplus", mp$type)], id = "sp", .options = fo)
mse_ref <- furrr::future_map(scenarios, fit_scenario,
  mp_vec = mp$mp[grepl("^Reference", mp$type)], id = "ref", .options = fo)

source(here("analysis/rex/merge_MSE.R"))
mse <- pmap(list(mse_cc, mse_ind, mse_sp, mse_ref), merge_MSE)
for (i in seq_along(mse)) mse[[i]]@OM$RefY <- ref_catch

# Satisficing -----------------------------------------------------------------

pm_df_list <- map(mse[scenarios_ref], ~ gfdlm::get_probs(.x, PM))
pm_df_list_rob <- map(mse[scenarios_rob], ~ gfdlm::get_probs(.x, PM))
pm_df <- bind_rows(pm_df_list, .id = "scenario")
pm_avg <- group_by(pm_df, MP) %>% summarise_if(is.numeric, mean)
pm_min <- group_by(pm_df, MP) %>% summarise_if(is.numeric, min)
saveRDS(pm_df_list, file = here("generated-data", "rex-pm-all.rds"))
saveRDS(pm_df_list_rob, file = here("generated-data", "rex-pm-all-rob.rds"))

satisficed_criteria <- c("LT LRP" = 0.9, "STC" = 0.8)
# plot_tigure(pm_avg, satisficed = satisficed_criteria)
# plot_tigure(pm_min, satisficed = satisficed_criteria)

mp_sat <- dplyr::filter(pm_min, `LT LRP` > satisficed_criteria[1], `STC` > satisficed_criteria[2]) %>%
  pull(MP)
mp_sat <- mp_sat[!mp_sat %in% reference_mp]
mp_sat

# Near identical performance:
mp_sat <- mp_sat[!mp_sat %in% c(".SP4010_0.6", ".SP6040_0.6_fox", ".SP6040_0.6")]
mp_sat <- mp_sat[!mp_sat %in% c("CC_hist20")] # similar to "CC1.2"
mp_sat

stopifnot(length(mp_sat) >= 1)
stopifnot(length(mp_sat) <= 8) # for RColorBrewer::brewer.pal()
mp_sat_with_ref <- union(mp_sat, reference_mp)
mp_not_sat <- mp$mp[!mp$mp %in% mp_sat_with_ref]
stopifnot(length(mp_not_sat) > 1)

custom_pal <- c(RColorBrewer::brewer.pal(8, "Dark2")[seq_along(mp_sat)], ref_mp_cols) %>%
  set_names(mp_sat_with_ref)

mp_eg_not_sat <- c(
  "CC0.6",
  ".Iratio2",
  ".GB_slope8_0.66",
  ".Islope0.2_80",
  ".IDX",
  ".IDX_smooth",
  ".IT5_hist",
  ".IT10_hist",
  ".ITM_hist",
  ".SP6040_0.4"
)

plots <- gfdlm::plot_factory(
  mse_list = mse,
  pm = PM,
  scenario_df = sc,
  mp_sat = mp_sat,
  mp_not_sat = mp_not_sat,
  mp_not_sat2 = mp_eg_not_sat,
  mp_ref = reference_mp,
  custom_pal = custom_pal,
  eg_scenario = sc$scenario[1],
  tradeoff = names(satisficed_criteria),
  catch_breaks = catch_breaks,
  catch_labels = catch_labels,
  satisficed_criteria = satisficed_criteria,
  skip_projections = FALSE,
  survey_type = "AddInd",
  omit_index_fn = oddify
)

.ggsave("dot-refset-avg", width = 7.5, height = 4.5, plot = plots$dot_refset_avg)
.ggsave("dot-robset", width = 8, height = 6, plot = plots$dot_robset + facet_wrap(~scenario, ncol = 1))
.ggsave("convergence", width = 10.5, height = 8.25,
  plot = plots$convergence + scale_x_continuous(breaks = seq(100, 300, 100)) +
    theme(
      legend.box.margin = ggplot2::margin(0.2, 0.2, 12, .2),
      legend.position = "bottom"
    ))

g <- plots$tradeoff_refset + facet_wrap(~scenario, ncol = 6)
.ggsave("tradeoff-refset", width = 9.5, height = 5, plot = g)
.ggsave("tradeoff-refset-avg", width = 4.5, height = 4,
  plot = plots$tradeoff_avg + coord_equal(xlim = c(0.8, 1), ylim = c(0.8, 1), expand = FALSE))
.ggsave("tradeoff-robset", width = 7, height = 3, plot = plots$tradeoff_robset)

.ggsave("tigure-refset", width = 6.5, height = 8, plot = plots$tigure_refset)
.ggsave("tigure-robset", width = 6.5, height = 3, plot = plots$tigure_robset)
.ggsave("tigure-refset-min", width = 4.7, height = 7.5, plot = plots$tigure_refset_min)
.ggsave("tigure-refset-avg", width = 4.7, height = 7.5, plot = plots$tigure_refset_avg)

.ggsave("radar-refset", width = 11.5, height = 9, plot = plots$radar_refset)
.ggsave("radar-robset", width = 9, height = 5, plot = plots$radar_robset)
.ggsave("radar-refset-avg", width = 6, height = 6, plot = plots$radar_refset_avg)
.ggsave("radar-refset-min", width = 6, height = 6, plot = plots$radar_refset_min)

g <- plots$projections_index +
  scale_x_continuous(breaks = seq(1975, 2100, 25)) +
  coord_cartesian(ylim = c(0, 16e6)) +
  scale_y_continuous(labels = function(x) x / 1e6)
.ggsave("projections-index", width = 9, height = 8.5, plot = g)

walk(names(plots$projections), ~ {
  .ggsave(paste0("projections-", .x),
    width = 8, height = 10,
    plot = plots$projections[[.x]]
  )
})
.ggsave("projections-not-sat2",
  width = 7, height = 10,
  plot = plots$projections_not_sat2
)
.ggsave("projections-not-sat",
  width = 6.5, height = 20,
  plot = plots$projections_not_sat
)
.ggsave("projections-scenarios",
  width = 8, height = 10,
  plot = plots$projections_scenarios
)

.ggsave("worms-proj", width = 8.5, height = 9.5, plot = plots$worms_proj_ref)
.ggsave("worms-hist-proj", width = 8.5, height = 9.5, plot = plots$worms_hist_proj_ref)
.ggsave("kobe", width = 8, height = 10.5, plot = plots$kobe_ref)

# Substantially speeds up LaTeX rendering on a Mac
# by pre-optimizing the PNG compression:
optimize_png <- FALSE
if (optimize_png && !identical(.Platform$OS.type, "windows")) {
  files_per_core <- 4
  setwd("report/figure")
  system(paste0(
    "find -X . -name 'rex-*.png' -print0 | xargs -0 -n ",
    files_per_core, " -P ", cores, " optipng -strip all"
  ))
  setwd(here())
}
