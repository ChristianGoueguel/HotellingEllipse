library(HotellingEllipse)
library(tidyverse)
library(hexSticker)
library(ggplot2)

data("LIBS_spec")

set.seed(123)
pca_mod <- LIBS_spec %>%
  select(where(is.numeric)) %>%
  FactoMineR::PCA(scale.unit = FALSE, graph = FALSE)

pca_scores <- pca_mod %>%
  pluck("ind", "coord") %>%
  as_tibble()

res <- HotellingEllipse(data = pca_scores, k = 2, pcx = 1, pcy = 2)

a1 <- pluck(res, "Ellipse", "a1")
b1 <- pluck(res, "Ellipse", "b1")
a2 <- pluck(res, "Ellipse", "a2")
b2 <- pluck(res, "Ellipse", "b2")

p <- pca_scores %>%
  ggplot(aes(x = Dim.1, y = Dim.2)) +
  ggforce::geom_ellipse(aes(x0 = 0, y0 = 0, a = a1, b = b1, angle = 0), size = .1, linetype = "dotted", color = "white") +
  ggforce::geom_ellipse(aes(x0 = 0, y0 = 0, a = a2, b = b2, angle = 0), size = .1, linetype = "dashed", color = "white") +
  geom_point(color = "white", shape = 21, size = 1) +
  geom_hline(yintercept = 0, linetype = "solid", color = "white", size = .05) +
  geom_vline(xintercept = 0, linetype = "solid", color = "white", size = .05)

p <- p + theme_void() + theme_transparent()
ggsave("~/Documents/Packages/HotellingEllipse/ggplot2.png")

s <- sticker(
  p,
  package = "HotellingEllipse",
  p_size = 13,
  p_color = "#F49F1C",
  s_x = 1,
  s_y = .75,
  s_width = 1.3,
  s_height = 1,
  h_fill="#00154F",
  h_color="#F49F1C",
  filename = "~/Documents/Packages/HotellingEllipse/ggplot2.png"
)
