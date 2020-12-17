library("dplyr")
library("purrr")
library("ggplot2")
library("gfdlm")
library("here")
library("rosettafish")

FRENCH <- TRUE
options(french = TRUE)
if (FRENCH) options(OutDec = ",")

if (!FRENCH) {
  fig_dir <- here("report", "figure")
} else {
  fig_dir <- here("report-french", "figure")
}

dir.create("report", showWarnings = FALSE)
dir.create("report-french", showWarnings = FALSE)
if (!dir.exists(fig_dir)) dir.create(fig_dir)
.ggsave <- function(filename, width, height, ...) {
  ggsave(file.path(fig_dir, paste0("framework-", filename, ".png")),
    width = width, height = height, ...
  )
}

sc <- readRDS(here("generated-data/rex-scenarios.rds"))
d_catch <- readRDS(here("generated-data", "rex-catch2.rds"))
d_catch <- dplyr::filter(d_catch, year >= 1996, year <= 2019)
catch <- suppressWarnings(gfplot::tidy_catch(d_catch))
catch <- catch %>%
  group_by(year) %>%
  summarize(value = sum(value)) %>%
  pull(value)

yrs <- length(catch)
ref_aadc <- gfdlm:::get_aadc(catch[(yrs - 10 + 1):yrs])
ref_catch <- mean(catch[(yrs - 5 + 1):yrs]) - 5000

get_scenario <- function(scenario, id = "", sp = "rex") {
  if (id != "") id <- paste0(id, "-")
  file_name <- here::here("generated-data", paste0(sp, "-mse-", id, scenario, ".rds"))
  readRDS(file_name)
}
scenarios <- c(sc$scenario[1], sc$scenario[7])
# mse_cc <- map(scenarios, get_scenario, id = "cc")
mse_ind <- map(scenarios, get_scenario, id = "ind")
mse_sp <- map(scenarios, get_scenario, id = "sp")
mse_ref <- map(scenarios, get_scenario, id = "ref")
source(here("analysis/rex/merge_MSE.R"))
mse_all <- pmap(list(mse_ind, mse_sp, mse_ref), merge_MSE)
for (i in seq_along(mse_all)) mse_all[[i]]@OM$RefY <- ref_catch

pm_df_list_all <- readRDS(here("generated-data", "rex-pm-all.rds"))
pm_df_list_all_rob <- readRDS(here("generated-data", "rex-pm-all-rob.rds"))

mse2 <- readRDS(here("generated-data", "rex-mse-ind-ceq200.rds"))

STC <- gfdlm::pm_factory("LTY", 1, c(1, 10))
LTC <- gfdlm::pm_factory("LTY", 1, c(36, 50))
AADC <- gfdlm::pm_factory("AADC", ref_aadc, c(1, 50))

if (!FRENCH) {
  `LT LRP` <- gfdlm::pm_factory("SBMSY", 0.4, c(36, 50))
  `LT USR` <- gfdlm::pm_factory("SBMSY", 0.8, c(36, 50))
  FMSY <- DLMtool::PNOF
} else {
  `LT PRL` <- gfdlm::pm_factory("SBMSY", 0.4, c(36, 50))
  `LT RSS` <- gfdlm::pm_factory("SBMSY", 0.8, c(36, 50))
  FRMD <- DLMtool::PNOF
}

if (!FRENCH) {
  PM <- c("LT LRP", "LT USR", "FMSY", "STC", "LTC", "AADC")
} else {
  PM <- c("LT PRL", "LT RSS", "FRMD", "STC", "LTC", "AADC")
}

mse2@OM$RefY <- ref_catch

# mse <- DLMtool::Sub(mse, MPs = c("CC0.8", "CC0.9", "CC1.0", "CC1.1", "CC1.2"))
mse <- DLMtool::Sub(mse2, MPs = c(".Itarget_d0.8", ".Itarget_w0.8", ".Itarget_base", ".Itarget_x0.2"))
mse@OM$RefY <- ref_catch

pm_df <- gfdlm::get_probs(mse, PM)

pm_df_list <- list("Scenario 1" = pm_df)
if (FRENCH) names(pm_df_list)[1] <- paste(en2fr("Scenario", FRENCH), "1")

catch_breaks <- seq(0, 600000, 100000)
catch_labels <- catch_breaks / 1000

# Convergence -----------------------------------------------------------------

x3 <- mse
x3@MPs <- paste0(en2fr("MP", FRENCH), "-", seq_along(x3@MPs))
gfdlm::plot_convergence(x3, PM[2]) +
  scale_colour_brewer(palette = "Dark2")
.ggsave("convergence", 5, 3.5)

# Main projections ------------------------------------------------------------

x <- DLMtool::Sub(mse, MPs = c(".Itarget_d0.8"))
x@MPs <- x3@MPs[[1]]
x %>% gfdlm::plot_main_projections(catch_breaks = catch_breaks, catch_labels = catch_labels)
.ggsave("main-projections", 8, 2.5)

# Index projections -----------------------------------------------------------

ind_breaks <- seq(0, 10000000, length.out = 5)
ind_breaks
ind_labels <- ind_breaks / 100000
ind_labels
oddify <- function(x) seq(2, x, by = 2)
temp <- DLMtool::Sub(mse, MPs = c(".Itarget_d0.8")) %>%
  list("Scenario 1" = .)
if (FRENCH) names(temp)[1] <- paste(en2fr("Scenario", FRENCH), "1")
temp %>%
  map(~{.x@MPs <- paste0(en2fr("MP", FRENCH), " 1"); .x}) %>%
  gfdlm::plot_index(type = "AddInd", omit_index_fn = oddify,
    n_samples = 3, seed = 12345) + coord_cartesian(expand = FALSE, ylim = c(0, 10000000)) +
  scale_y_continuous(breaks = ind_breaks, labels = ind_labels)
.ggsave("index-projections", 6, 3)

# Probability table -----------------------------------------------------------

mse2 %>%
  gfdlm::get_probs(PM) %>%
  mutate(MP = c(paste0(en2fr("MP", FRENCH), "-", seq_len(length(MP)-1)), paste0(en2fr("MP", FRENCH), "-ref"))) %>%
  gfdlm::plot_tigure(satisficed = c("LT LRP" = 0.9, "LTC" = 0.8))
.ggsave("tigure", 3.5, 5.5)

# Dot plot --------------------------------------------------------------------

.names <- c("MP-1", "MP-2", "MP-3", "MP-4", "MP-ref")
if (FRENCH) .names <- gsub("MP", "PG", .names)

temp <- map(pm_df_list_all, dplyr::filter, MP %in% c("CC1.2", "CC1.0", "CC0.8", ".Itarget_x0.2", "FMSYref75")) %>%
  map(~ {.x$MP <- .names;.x}) %>%
  map(~ {names(.x)[2:4] <- PM[1:3];.x})
temp %>% gfdlm::plot_dots(type = "single") +
  scale_colour_manual(values = c( RColorBrewer::brewer.pal(4, "Dark2"), "grey50"))
.ggsave("dot", 6, 3)

# Trade-off plot --------------------------------------------------------------

temp <- pm_df_list_all_rob$`no-cpue-light` %>%
  dplyr::filter(grepl("^.Itarg", MP)) %>%
  list("Scenario 1" = .)
if (FRENCH) names(temp)[1] <- paste(en2fr("Scenario", FRENCH), "1")

if (!FRENCH) {
  temp %>%
    map(~{.x$MP <- paste0("MP-", seq_along(.x$MP));.x}) %>%
    gfdlm::plot_tradeoff("LT LRP", "STC") +
    scale_colour_brewer(palette = "Dark2") +
    coord_equal(xlim = c(0.6, 1), ylim = c(0.6, 1), expand = FALSE)+
    guides(shape = "none")
} else {
  colnames(temp[[1]])[2] <- "LT PRL"
  temp %>%
    map(~{.x$MP <- paste0("PG-", seq_along(.x$MP));.x}) %>%
    gfdlm::plot_tradeoff("LT PRL", "STC", french = TRUE) +
    scale_colour_brewer(palette = "Dark2") +
    coord_equal(xlim = c(0.6, 1), ylim = c(0.6, 1), expand = FALSE)+
    guides(shape = "none")
}
.ggsave("trade-off", 3.5, 4.5)

# Radar plot ------------------------------------------------------------------

.names <- c("MP-1", "MP-2", "MP-3", "MP-4")
if (FRENCH) .names <- gsub("MP", "PG", .names)
pm_df %>%
  mutate(MP = .names) %>%
  gfdlm::plot_radar(palette = "Dark2", french = FRENCH)
.ggsave("radar", 5, 5)

# Kobe ------------------------------------------------------------------------

x <- DLMtool::Sub(mse, MPs = c(".Itarget_base"))
x@MPs <- paste0("MP-", seq_along(x@MPs))
if (FRENCH) x@MPs <- gsub("MP", "PG", x@MPs)
temp <- list("Scenario 1" = x)
if (FRENCH) names(temp)[1] <- paste(en2fr("Scenario", FRENCH), "1")
gfdlm::plot_kobe_grid(temp, french = T) +
  coord_equal(xlim = c(0, 3.2), ylim = c(0, 2.1), expand = FALSE)
.ggsave("kobe", 4.5, 2.7)

# Worm plot -------------------------------------------------------------------

# worm
gfdlm::plot_worms_grid(temp, include_historical = TRUE) +
  coord_equal(xlim = c(0, 2.5), ylim = c(0, 1.5), expand = FALSE)
.ggsave("worm", 4.5, 2.5)

# Scenario projection sensitivities -------------------------------------------

temp <- purrr::map(seq_along(mse_all), ~ DLMtool::Sub(mse_all[[.x]], MPs = c(".Itarget_d0.8"))) %>%
  set_names(c("Scenario 1", "Scenario 2"))
if (FRENCH) names(temp) <- c("Scénario 1", "Scénario 2")
temp %>% purrr::map(~ {.x@MPs <- if (!FRENCH) "MP-1" else "PG-1";.x}) %>%
  gfdlm::plot_scenario_projections(catch_breaks = catch_breaks, catch_labels = catch_labels)
.ggsave("sens-projections", 8, 2.5)

# Substantially speeds up LaTeX rendering on a Mac
# by pre-optimizing the PNG compression:
optimize_png <- TRUE
if (optimize_png && !identical(.Platform$OS.type, "windows")) {
  files_per_core <- 1
  cores <- floor(parallel::detectCores() / 2L)
  setwd(fig_dir)
  system(paste0(
    "find -X . -name 'framework-*.png' -print0 | xargs -0 -n ",
    files_per_core, " -P ", cores, " optipng -strip all"
  ))
  setwd(here())
}
