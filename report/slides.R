library(ggplot2)
library(dplyr)

d <- tibble(index = seq(0, 2, length.out = 10), tac = rep(100, 10),
  tac1 = rep(120, 10), tac2 = rep(80, 10))

d %>%
  ggplot(aes(index, tac)) +
  geom_line(colour = "grey50", aes(y = tac), lwd = 1.0) +
  geom_line(colour = "grey5", aes(y = tac1), lwd = 1.0) +
  geom_line(colour = "grey90", aes(y = tac2), lwd = 1.0) +
  gfdlm::theme_pbs() +
  labs(
    x = "Survey index",
    y = "TAC (based on 100 t average catch over last 5 years)"
  ) +
  coord_cartesian(ylim = c(0, 140), expand = FALSE) +
  theme(axis.text.x = element_blank())
ggsave(here::here("figure/slide-cc.png"), dpi = 400, width = 5, height = 4)


d %>%
  ggplot(aes(index, tac)) +
  geom_line(colour = "grey50", aes(y = tac), lwd = 1.0) +
  geom_line(colour = "grey5", aes(y = tac1), lwd = 1.0) +
  geom_line(colour = "grey90", aes(y = tac2), lwd = 1.0) +
  gfdlm::theme_pbs() +
  labs(
    x = "Number of Nicolas Cage movies",
    y = "TAC (based on 100 t average catch over last 5 years)"
  ) +
  coord_cartesian(ylim = c(0, 140), expand = FALSE) +
  theme(axis.text.x = element_blank())
ggsave(here::here("figure/slide-cc-nc.png"), dpi = 400, width = 5, height = 4)


# Itarget
f_itarget <- function(TACtarg, Irecent, Itarg, Iave, w, lambda = 0.2) {
  I0 <- lambda * Iave
  if (Irecent >= I0) {
    TAC <- TACtarg * ((w + (1 - w) * ((Irecent - I0) / (Itarg - I0))))
  } else {
    TAC <- w * TACtarg * (Irecent / I0)^2
  }
  if (TAC < 0) TAC <- 0
  TAC
}
pars <- expand.grid(
  TACtarg = c(80, 100, 120),
  Irecent = seq(0, 1.8, length.out = 300),
  Itarg = c(0.8, 1, 1.2),
  Iave = 1,
  w = seq(0.2, 0.8, 0.1),
  lambda = c(0.2)
)
pars$tac <- purrr::pmap_dbl(pars, f_itarget)
pars %>%
  dplyr::filter(TACtarg == 100, Itarg == 1, w == 0.5) %>%
  mutate(TACtarg = paste("x = ", sprintf("%3.0f", TACtarg))) %>%
  mutate(Itarg = paste("delta = ", Itarg)) %>%
  ggplot(aes(Irecent, tac, colour = as.factor(w), group = paste(lambda, w))) +
  geom_line() +
  gfdlm::theme_pbs() +
  facet_grid(TACtarg ~ Itarg) +
  labs(
    colour = "w",
    x = expression(I[recent] / I[target]),
    y = "TAC (based on 100 t average\nin last 5 historical years)"
  ) +
  geom_hline(yintercept = 100, lty = 2, alpha = 0.4) +
  geom_vline(xintercept = 1, lty = 2, alpha = 0.2) +
  scale_colour_viridis_d(begin = 0.5) +
  coord_cartesian(expand = FALSE)
ggsave(here::here("figure/slide-itarget1.png"), dpi = 500, width = 5, height = 4)

pars %>%
  dplyr::filter(TACtarg == 100, w == 0.5) %>%
  mutate(TACtarg = paste("x = ", sprintf("%3.0f", TACtarg))) %>%
  mutate(Itarg = paste("delta = ", Itarg)) %>%
  ggplot(aes(Irecent, tac, colour = as.factor(w), group = paste(lambda, w))) +
  geom_line() +
  gfdlm::theme_pbs() +
  facet_grid(TACtarg ~ Itarg) +
  labs(
    colour = "w",
    x = expression(I[recent] / I[target]),
    y = "TAC (based on 100 t average\nin last 5 historical years)"
  ) +
  geom_hline(yintercept = 100, lty = 2, alpha = 0.4) +
  geom_vline(xintercept = 1, lty = 2, alpha = 0.2) +
  scale_colour_viridis_d(begin = 0.5) +
  coord_cartesian(expand = FALSE)
ggsave(here::here("figure/slide-itarget3.png"), dpi = 500, width = 9, height = 4)


pars %>%
  dplyr::filter(Itarg == 1, w == 0.5) %>%
  mutate(TACtarg = paste("x = ", sprintf("%3.0f", TACtarg))) %>%
  mutate(Itarg = paste("delta = ", Itarg)) %>%
  ggplot(aes(Irecent, tac, colour = as.factor(w), group = paste(lambda, w))) +
  geom_line() +
  gfdlm::theme_pbs() +
  facet_grid(TACtarg ~ Itarg) +
  labs(
    colour = "w",
    x = expression(I[recent] / I[target]),
    y = "TAC (based on 100 t average\nin last 5 historical years)"
  ) +
  geom_hline(yintercept = 100, lty = 2, alpha = 0.4) +
  geom_vline(xintercept = 1, lty = 2, alpha = 0.2) +
  scale_colour_viridis_d(begin = 0.5) +
  coord_cartesian(expand = FALSE)
ggsave(here::here("figure/slide-itarget4.png"), dpi = 500, width = 4, height = 8)


pars %>%
  dplyr::filter(TACtarg == 100, Itarg == 1) %>%
  mutate(TACtarg = paste("x = ", sprintf("%3.0f", TACtarg))) %>%
  mutate(Itarg = paste("delta = ", Itarg)) %>%
  ggplot(aes(Irecent, tac, colour = as.factor(w), group = paste(lambda, w))) +
  geom_line() +
  gfdlm::theme_pbs() +
  facet_grid(TACtarg ~ Itarg) +
  labs(
    colour = "w",
    x = expression(I[recent] / I[target]),
    y = "TAC (based on 100 t average\nin last 5 historical years)"
  ) +
  geom_hline(yintercept = 100, lty = 2, alpha = 0.4) +
  geom_vline(xintercept = 1, lty = 2, alpha = 0.2) +
  scale_colour_viridis_d() +
  coord_cartesian(expand = FALSE)
ggsave(here::here("figure/slide-itarget2.png"), dpi = 500, width = 5, height = 4)
