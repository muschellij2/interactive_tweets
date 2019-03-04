library(ggplot2)
library(plotly)
library(htmlwidgets)

make_card = function(handle = "@StrictlyStat",
                     title = "",
                     description = "",
                     img = NULL,
                     html = NULL,
                     height = 480,
                     width = 480
) {
  glue::glue(
    '
    <meta content="text/html; charset=UTF-8" http-equiv="Content-Type" />
    <meta name="twitter:card" content="player" />
    <meta name="twitter:site" content="{handle}" />
    <meta name="twitter:title" content="{title}" />
    <meta name="twitter:description" content="{description}" />
    <meta name="twitter:image" content="{img}" />
    <meta name="twitter:player" content="{html}" />
    <meta name="twitter:player:width" content="{width}" />
    <meta name="twitter:player:height" content="{height}" />
    ', 
    handle = handle,
    title = title,
    description = description,
    img = img,
    html = html,
    width = width,
    height = height,
  )
}


make_interactive_card = function(slug, plot, site, ...) {
  html_file = paste0(slug, ".html")
  png_file = paste0(slug, ".png")
  
  html_name = file.path(site, html_file)
  img_name = file.path(site, png_file)
  
  
  
  ggsave(plot = static_plot, filename = png_file)
  interactive_plot <- plotly::ggplotly(static_plot)
  
  interactive_plot$sizingPolicy$padding <- "0"
  res = htmlwidgets::saveWidget(
    interactive_plot, file = html_file, libdir = "lib",
    title = title,
    selfcontained = FALSE
  )
  
  
  card = make_card(...,
                   img = img_name,
                   html = html_name)
  html = readLines(html_file)
  start_head = grep("^<head>", html)
  html = c(html[1:start_head], card, html[(start_head + 1):length(html)])
  writeLines(html, con = html_file)
  
  
}


site = "https://johnmuschelli.com/interactive_tweets"
slug = "test"
title = "Iris Flowers Interactive"

static_plot <- ggplot(iris, 
                      aes(x = Petal.Length, y = Petal.Width, color = Species)) +
  geom_point()
description = "Shows an interactive tweet."
make_interactive_card(slug = slug, plot = static_plot,
                      site = site, title = title,
                      description = description,
                      handle = "@StrictlyStat")

