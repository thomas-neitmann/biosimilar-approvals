library(dplyr)
library(ggplot2)

load("data/fda_biosimilars_by_year.RData")
load("data/ema_biosimilars_by_year.RData")

colors = c("EMA" = "lightgray", "FDA" = "#B22234")
breaks = 2006:2019

fill_fda <- tibble(
  approval_year = 2006:2014,
  n = 0,
  agency = "FDA"
)

fill_ema <- tibble(
  approval_year = c(2011, 2012, 2015),
  n = 0,
  agency = "EMA"
)

approvals <- bind_rows(
  fda_biosimilars_by_year, ema_biosimilars_by_year, fill_ema, fill_fda
) %>% mutate(n = ifelse(n == 0, .08, n))

plot <- ggplot(approvals) +
  aes(approval_year, n, fill = agency) +
  geom_col(position = "dodge", width = .9) +
  ggthemes::theme_few() +
  scale_fill_manual(values = colors) +
  scale_x_continuous(breaks = breaks, name = element_blank()) +
  scale_y_continuous(expand = expand_scale(), name = "Number of Approved Biosimilar Drugs") +
  labs(
    title = "The <span style='color:#B22234'>FDA</span> Has Overtaken the <span style='color:#d3d3d3'>EMA</span> in Terms of Biosimilar Approvals",
    caption = "Created by Thomas Neitmann"
  ) +
  theme(
    plot.title = ggtext::element_markdown(hjust = 0, face = "bold", size = 18),
    plot.caption = element_text(size = 8, color = "#5c5c5c"),
    legend.title = element_blank(),
    legend.position = "none",
    axis.text.x = element_text(),
    panel.border = element_blank(),
    axis.line = element_line(color = "gray"),
    axis.ticks = element_line(color = "gray"),
    text = element_text(family = "Trebuchet MS")
  ) +
  geom_text(
    data = dplyr::filter(approvals, agency == "FDA", approval_year >= 2015),
    aes(x = approval_year + .225, y = n - .4, label = n),
    color = "white", fontface = "bold"
  ) +
  annotate(
    geom = "curve", x = 2008, y = 14, xend = 2005.75, yend = 2.2,
    arrow = arrow(length = unit(2, "mm")), curvature = .2,
    color = "gray"
  ) +
  annotate(
    geom = "label", x = 2008.055, y = 14,
    label = "The first biosimilar,\nOmnitrope by Sandoz,\nis approved in the\nEuropean Union",
    hjust = "left", family = "Trebuchet MS"
  ) +
  annotate(
    geom = "curve", x = 2011, y = 11, xend = 2009.25, yend = 0.2,
    arrow = arrow(length = unit(2, "mm")), curvature = .2,
    color = "gray"
  ) +
  annotate(
    geom = "label", x = 2011.05, y = 11,
    label = "US Congress passes the\nBiologics Price Competition\nand Innovation (BPCI) Act\nwhich establishes an approval\npathway for biosimilars",
    hjust = "left", family = "Trebuchet MS"
  ) +
  annotate(
    geom = "curve", x = 2014, y = 6, xend = 2015.25, yend = 1.2,
    arrow = arrow(length = unit(2, "mm")), curvature = -.2,
    color = "gray"
  ) +
  annotate(
    geom = "label", x = 2013.95, y = 6,
    label = "Six years after the BPCI Act\nwas passed, the FDA approves\nSandoz' Zarxio as the first\nbiosimilar in the United States",
    hjust = "right", family = "Trebuchet MS"
  )
