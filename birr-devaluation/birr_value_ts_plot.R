library(data.table)
library(ggplot2)
library(marquee)

colors = c(blue = "#0072B2", green = "#009E73", red = "#D55E00")

# import exchange rates data ----
## exchange rates from NBE ----
# interbank exchange rates
interbank = fread("birr-devaluation/interbank_exchange_rates.csv", col.names = tolower)

interbank[, date := as.Date(market_day, format = "%m/%d/%Y")
          ][, market_day := NULL
           ][, lowest_rate := as.numeric(lowest_rate)
             ][, highest_rate := as.numeric(highest_rate)
               ][, weighted_average_rate := as.numeric(weighted_average_rate)]
# in case of missing, compute it from the average of the lowest and highest rates
interbank[, weighted_average_rate :=
            fcoalesce(weighted_average_rate, (lowest_rate + highest_rate) / 2)
          ][, c("lowest_rate", "highest_rate", "banks_participated") := NULL
            ][, type := "interbank"
              ][, source := "NBE"]
setnames(interbank, "weighted_average_rate", "rate")

# transaction exchange rates
transact = fread("birr-devaluation/exchange_rates_nbe.csv")
transact = transact[currency_name == "US DOLLAR",
                    c("date", "buying", "selling"), with = FALSE
                    ][, rate := rowMeans(.SD, na.rm = TRUE), .SDcols = c("buying", "selling")
                      ][, c("buying", "selling") := NULL
                        ][, type := "transaction"
                          ][, source := "NBE"]

# nbe datasets: supplement the interbank data with missing dates from the transaction dataset
nbe = rbind(interbank, transact[date %notin% interbank$date,])
setorder(nbe, date)


## exchange rates from CBE ----
cbe = fread("birr-devaluation/exchange_rates_cbe.csv", col.names = tolower)
cbe = cbe[currencyname == "US DOLLAR", c("date", "transactionalbuying", "transactionalselling"), with = FALSE]

cbe[, rate := rowMeans(.SD, na.rm = TRUE), .SDcols = c("transactionalbuying", "transactionalselling")
    ][, c("transactionalbuying", "transactionalselling") := NULL
      ][, type := "transaction"
        ][, source := "CBE"]

# create a full dataset by adding data for missing dates from CBE
data = rbind(nbe, cbe[date %notin% nbe$date, ])
setorder(data, date)

data[, grate := 100 * (rate / shift(rate) - 1)] # growth rates

# filter out weekends
data = data[format(date, "%A") %notin% c("Saturday", "Sunday"), ]

# add regime information
regimes = data.frame(
  name = c("Meles Zenawi", "Hailemariam Desalegn", "Abiy Ahmed"),
  date = as.Date(c("1991-08-23", "2012-08-20", "2018-04-02"))
)
first_name = \(x) sub("([[:alpha:]]+) ([[:alpha:]]+)", "\\1", x)

data[, regime := fcase(
  date >= regimes$date[1] & date < regimes$date[2], regimes$name[1],
  date >= regimes$date[2] & date < regimes$date[3], regimes$name[2],
  date >= regimes$date[3], regimes$name[3]
)]

# other events
events = data.frame(
  date = as.Date(c("2020-11-03", "2024-07-29")),
  name = c("Northern conflict", "Shift to mkt-based fx regime")
)
events = data[, .(date, rate)][events, on="date", nomatch=NULL]


# plotting ----
BASE_SIZE = 13
BASE_FAMILY = "Helvetica"
CAPTION_SIZE_REL = .6675
AXIS_TEXT_SIZE_REL = .8
appreciated_by_pct = 1
depreciated_by_pct = 1
rng = range(data$rate, na.rm = TRUE)
timespan = range(data$date, na.rm = TRUE)
# breaks = seq(rng[1], rng[2], length.out = 5)
breaks = c(11, 22, 32, 54, 110)

notes = sprintf("**Notes:** The data spans from %1$s, and primarily consists of daily {.%2$s interbank} exchange rates from NBE. For missing data points, the dataset is supplemented with {.%2$s transactional} rates from NBE and CBE.", paste(timespan, collapse = " to "), colors["red"])

# main plot
data |>
  transform(regime = reorder(as.factor(first_name(regime)), date)) |>
  ggplot(aes(date, 1 / rate)) +
  geom_line(aes(linetype = regime), color = colors["blue"]) +
  geom_point(
    aes(date, 1 / rate, fill = sprintf("Appreciated (by > %.f%%)", appreciated_by_pct)),
    data[sign(grate) == -1L & abs(grate) > appreciated_by_pct, ],
    shape = 24, color = "white", size = 1.75
  ) +
  geom_point(
    aes(date, 1 / rate, fill = sprintf("Depreciated (by > %.f%%)", depreciated_by_pct)),
    data[sign(grate) == 1L & abs(grate) > depreciated_by_pct, ],
    shape = 25, color = "white", size = 1.75
  ) +
  # Regime transition markers
  geom_vline(
    aes(xintercept = date),
    data = regimes, linetype = "dotted", color = "gray45"
  ) +
  # event markers
  geom_vline(
    aes(xintercept = date), data = events, linetype = "dashed", color = "gray45"
  ) +
  geom_text(
    aes(x = date, y = 1 / rate, label = paste0(name, " (", date, ")")), data = events,
    size = CAPTION_SIZE_REL * BASE_SIZE / .pt,
    angle = -90, hjust = 1.2, vjust = -0.5
  ) +
  scale_x_date(
    date_breaks = "2 year", date_labels = "%Y",
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(
    # n.breaks = 10, labels = \(x) sprintf("%.0f", 1 / x),
    breaks = 1 / round(breaks), labels = round(breaks),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_linetype_manual(values = c("solid", "dashed", "dotdash")) +
  scale_color_manual(values = unname(colors)) +
  scale_fill_manual(values = c("#28a745", "#dc3545")) +
  labs(
   x = NULL,
   y = "Birr / USD",
   color = NULL, fill = NULL, linetype = NULL,
   title ="**How has the Ethiopian Birr's value changed?**",
   subtitle = sprintf("The y-axis scale is {.%1$s inverted} to show the {.%1$s decline} in the Birr's value.",colors["blue"]),
  caption = "**Graphic:** By {.#1D9BF0 @eyayaw}, inspired by *NYT's Ruble's value plot*."
 ) +
  coord_cartesian(clip = "off") +
   annotate(
    "marquee",
    label = notes,
    x = min(data$date), y = -Inf,
    width = 0.6,
    hjust = 0.1,
    vjust = 1.75,
    size = (CAPTION_SIZE_REL * BASE_SIZE) / .pt,
    style = classic_style(body_font = BASE_FAMILY)
  ) +
  theme_bw(base_size = BASE_SIZE, base_family = BASE_FAMILY) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(1, 1),
    legend.justification = c(1.05, 1.05),
    legend.margin = margin(0, 0, 0, 0, "mm"),
    legend.background = element_rect(fill = "gray99", linewidth = 1, colour = "white"),
    legend.spacing = unit(0.1, "mm"),
    legend.text = element_text(size = rel(CAPTION_SIZE_REL)),
    axis.text = element_text(size = rel(AXIS_TEXT_SIZE_REL)),
    axis.title = element_text(size = rel(AXIS_TEXT_SIZE_REL)),
    # axis.text.x = element_text(angle = 45, vjust = 0.5),
    axis.ticks = element_line(colour = "grey70", linewidth = 0.2),
    panel.grid.major = element_line(color = "gray75", linewidth = 0.2, linetype = "dotted"),
    panel.grid.minor = element_blank(),
    plot.margin = margin(2, 2, 8, 2, "mm"),
    plot.title.position = "plot",
    plot.title = element_marquee(margin = margin(b = 1, unit = "mm")),
    plot.subtitle = element_marquee(size = rel(0.9)),
    plot.caption = element_marquee(
      family = BASE_FAMILY, size = rel(CAPTION_SIZE_REL), margin = margin(3,0,0,0,"mm")
      )
  ) -> p

ggsave("birr-devaluation/etb-usd.png", width = 9, height = 6, dpi = 300, device = ragg::agg_png)


# top 10 changes ----
top_changes = data |>
  transform(t0 = shift(date), rate0 = shift(rate)) |>
  _[order(-abs(grate)), ] |>
  head(10) |>
  _[, .(admin = first_name(regime), t0, t1 = date, rate0, rate1 = rate, grate = -1 * grate)]

top_changes |>
  _[, .(
    Date = sprintf("%s --> %s", t0, t1),
    Rate = sprintf("%3.2f --> %3.2f", rate0, rate1),
    `Change (%)` = sprintf("%6.2f", grate)
  )] |>
  knitr::kable(
    format = "markdown",
    caption = "Top 10 largest changes in Birr's value against the USD."
  )

# tweet draft ----
sprintf(
  r"(📉 Here is a viz of the #Ethiopian Birr's declining value, inspired by NYT's Ruble value plot. The Birr has weakened significantly, falling from around 11 Birr in 2008 to over 110 Birr per USD in late 2024. The currency's depreciation has accelerated recently following the shift to a market-based foreign exchange regime, dropping by %.2f%% on %s.
#rstats #dataviz
)",
  abs(top_changes$grate[1]),
  format(top_changes[["t1"]][1], "%B %d, %Y")
) |>
  cat()
