###--------------------------------------------------
### US and England/Wales births
###--------------------------------------------------

library(tidyverse)
library(gtable)
library(viridis)

## ggplot theme
theme_set(theme_minimal())


## Colors
bly_palette <- c("#E69F00", "#0072B2", "#000000", "#56B4E9",
        "#009E73", "#F0E442", "#D55E00", "#CC79A7")

## Make a "figures" subdirectory if one doesn't exist
ifelse(!dir.exists(file.path("figures")),
       dir.create(file.path("figures")),
       FALSE)

## Nice alignment solution for the STL plots, from Baptiste Auguié
## http://stackoverflow.com/questions/13294952/left-align-two-graph-edges-ggplot/22984913
## (Probably don't really need this approach anymore, but I know it works.)
rbind_gtable_max <- function(...) {

    gtl <- list(...)
    stopifnot(all(sapply(gtl, gtable::is.gtable)))
    bind2 <- function(x, y) {
        stopifnot(ncol(x) == ncol(y))
        if (nrow(x) == 0)
            return(y)
        if (nrow(y) == 0)
            return(x)
        y$layout$t <- y$layout$t + nrow(x)
        y$layout$b <- y$layout$b + nrow(x)
        x$layout <- rbind(x$layout, y$layout)
        x$heights <- gtable:::insert.unit(x$heights, y$heights)
        x$rownames <- c(x$rownames, y$rownames)
        x$widths <- grid::unit.pmax(x$widths, y$widths)
        x$grobs <- append(x$grobs, y$grobs)
        x
    }

    Reduce(bind2, gtl)
}

draw_stl <- function(data_stl = boom,
                     title_txt = "Births per month per million people, 1938-1991",
                     start_date = "1938-01-01",
                     end_date = "1991-12-01",
                     by_unit = "year",
                     bar.width = 0.5,
                     p_col = "gray30") {
    theme_set(theme_minimal())

    break_vec <- seq(from=as.Date(start_date), to=as.Date(end_date), by = by_unit)
    break_vec <- break_vec[seq(1, length(break_vec), 5)]

    p <- ggplot(data_stl, aes(x = date, y = births_pct_day))
    p1 <- p + geom_line(color = p_col) + ylab("Data") + xlab("") +
        scale_x_date(breaks = break_vec) +
        theme(axis.text.x = element_blank(),
        axis.title.y = element_text(size = rel(0.8)), plot.title = element_text(size = rel(1))) +
        ggtitle(title_txt)

    p <- ggplot(data_stl, aes(x = date, y = trend))
    p2 <- p + geom_line(color = p_col) + ylab("Trend") + xlab("") +
        scale_x_date(breaks = break_vec) +
        theme(axis.text.x = element_blank(),
        axis.title.y = element_text(size = rel(0.8)))

    p <- ggplot(data_stl, aes(x = date, y = seasonal))
    p3 <- p + geom_line(color = p_col) + ylab("Seasonal") + xlab("") +
        scale_x_date(breaks = break_vec) +
        theme(axis.text.x = element_blank(),
        axis.title.y = element_text(size = rel(0.8)))

    p <- ggplot(data_stl, aes(x = date, ymax = remainder, ymin = 0))
    p4 <- p + geom_linerange(size = bar.width, color = p_col) +
        scale_x_date(breaks = break_vec, date_labels = "%Y") +
        ylab("Remainder") + xlab(by_unit) +
        theme(axis.title.y = element_text(size = rel(0.8)))

    g1 <- ggplotGrob(p1)
    g2 <- ggplotGrob(p2)
    g3 <- ggplotGrob(p3)
    g4 <- ggplotGrob(p4)
    out <- rbind_gtable_max(g1, g2, g3, g4)
    out
}


###--------------------------------------------------
### Data objects
###--------------------------------------------------

boom <- read_csv("data/boom_births.csv")

start_date <- "1938-01-01"
end_date <- "1991-12-01"
by_unit = "year"

###--------------------------------------------------
### Time series
###--------------------------------------------------

break_vec <- seq(from=as.Date(start_date), to=as.Date(end_date), by = "month")
break_vec <- break_vec[seq(25, length(break_vec), 60)]
title_txt <- "Monthly Birth Rates, 1938-1991"
subtitle_txt <- "Average births per million people per day."

p <- ggplot(subset(boom, date >= as.Date(start_date) & date <= as.Date(end_date)),
            aes(x = date, y = births_pct_day, color = country))
p1 <- p + geom_line() + ylab("Data") + xlab("") +
    scale_x_date(breaks = break_vec) +
    scale_color_manual(values = bly_palette) +
    coord_cartesian(expand = FALSE) +
    theme(axis.text.x = element_blank(),
          axis.title.y = element_text(size = rel(0.8)),
          legend.justification = "left",
          plot.title = element_text(size = rel(1), face = "bold"),
          legend.position = "top") +
    labs(color = "Country")

p <- ggplot(subset(boom, date >= as.Date(start_date) & date <= as.Date(end_date)),
            aes(x = date, y = trend, color = country))
p2 <- p + geom_line() + ylab("Trend") + xlab("") +
    scale_color_manual(values = bly_palette) +
    scale_x_date(breaks = break_vec) +
    coord_cartesian(expand = FALSE) +
    guides(color = FALSE) +
    theme(axis.text.x = element_blank(),
          axis.title.y = element_text(size = rel(0.8)))

p <- ggplot(subset(boom, date >= as.Date(start_date) & date <= as.Date(end_date) & country == "England and Wales"),
            aes(x = date, y = seasonal, color = country))
p3 <- p + geom_line() + ylab("Seasonal") + xlab("") +
    coord_cartesian(expand = FALSE) +
    scale_x_date(breaks = break_vec) +
    scale_y_continuous(limits = c(-3, 4)) +
    scale_color_manual(values = bly_palette[1]) +
    guides(color = FALSE) +
    theme(axis.text.x = element_blank(),
          axis.title.y = element_text(size = rel(0.8)))


p <- ggplot(subset(boom, date >= as.Date(start_date) & date <= as.Date(end_date) & country == "United States"),
            aes(x = date, y = seasonal, color = country))
p3a <- p + geom_line() + ylab("Seasonal") + xlab("") +
    coord_cartesian(expand = FALSE) +
    scale_x_date(breaks = break_vec) +
    scale_y_continuous(limits = c(-3, 4)) +
    scale_color_manual(values = bly_palette[2]) +
    guides(color = FALSE) +
    theme(axis.text.x = element_blank(),
          axis.title.y = element_text(size = rel(0.8)))


p <- ggplot(subset(boom, date >= as.Date(start_date) & date <= as.Date(end_date)),
            aes(x = date, ymax = remainder, ymin = 0, color = country))
p4 <- p + geom_linerange(size = 0.25) +
    guides(color = FALSE) +
    coord_cartesian(expand = FALSE) +
    scale_color_manual(values = bly_palette) +
    scale_x_date(breaks = break_vec, date_labels = "%Y") +
    ylab("Remainder") + xlab("Year") +
    theme(axis.title.y = element_text(size = rel(0.8)),
          axis.text.y = element_text(size = rel(0.7)))

g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)
g3 <- ggplotGrob(p3)
g3a <- ggplotGrob(p3a)
g4 <- ggplotGrob(p4)

p_out <- rbind_gtable_max(g1, g2, g3, g3a, g4)


pdf(file = "figures/us_eng_wa_births.pdf", height = 8, width = 10)
grid.draw(p_out)
dev.off()


pdf(file = "figures/us_eng_wa_births_p1.pdf", height = 2.25, width = 10)
grid.draw(g1)
dev.off()

pdf(file = "figures/us_eng_wa_births_p2.pdf", height = 2.25, width = 10)
grid.draw(g2)
dev.off()

pdf(file = "figures/us_eng_wa_births_p3.pdf", height = 1, width = 10)
grid.draw(g3)
dev.off()

pdf(file = "figures/us_eng_wa_births_p3a.pdf", height = 1, width = 10)
grid.draw(g3a)
dev.off()

pdf(file = "figures/us_eng_wa_births_p4.pdf", height = 2.25, width = 10)
grid.draw(g4)
dev.off()




###----------------------------------------
### Tiles
###--------------------------------------------------

p <- ggplot(subset(boom, date >= as.Date(start_date) & date <= as.Date(end_date)),
            aes(y = factor(month,
                           levels = c(12:1),
                           labels = rev(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
                           ordered = TRUE),
                x = factor(year)))
p_tile <- p + geom_tile(aes(fill = births_pct_day), color = "white") + labs(x = "", y = "") +
    scale_x_discrete(breaks = seq(1940, 1990, 5)) +
    scale_fill_viridis(option = "inferno") +
    facet_wrap(~ country, ncol = 1) +
    labs(x = "Year", fill = "", title = "Monthly Birth Rates, 1938-1991",
         subtitle = "Average births per million people per day.",
         caption = "Kieran Healy (kieranhealy.org). Data: UK ONS, US Census Bureau.") +
    theme(legend.position = "top",
          legend.justification = "left",
          strip.text = element_text(size = 12, face = "bold"),
          plot.title = element_text(face = "bold", size = rel(2)),
          plot.caption = element_text(size = 6))

pdf("figures/births_monthly_tile.pdf", width = 12, height = 8)
print(p_tile)
dev.off()
