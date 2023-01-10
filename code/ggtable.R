# create two simple tiles
number_of_rows <- 3

key_tiles <- tibble(
  x = rep.int(c(0, 7, 14), number_of_rows),
  y = rev(c(rep.int(0, number_of_rows),
            rep.int(4.5, number_of_rows),
            rep.int(9, number_of_rows))),
  h = rep.int(4.25, 3*number_of_rows) ,
  w = rep.int(6.75, 3*number_of_rows),
  val = rep.int(c("$1.80", "$200", 1), number_of_rows),
  pct_change = rep.int(c("(3.5%)"), number_of_rows*3),
  text = c("Petrol price",
           "Wholesale price of electricity",
           "Price of white bread",
           "Price of milk",
           "Median listed house price",
           "Median rental price",
           "Consumer interest rate",
           "Official inflation rate",
           "Official wages price index"),
  change = rep.int(c("(Weekly change)"), number_of_rows*3),
  # icon = c('emojifont::emoji('arrow_up_small'),
  #          emojifont::emoji('arrow_down_small'),
  #          emojifont::emoji("bread")),
  icon = rep.int(c(fontawesome("fa-minus"), fontawesome("fa-arrow-up"), fontawesome("fa-arrow-down")), number_of_rows),
  color = factor(c(rep.int(1, 6), rep.int(2, 3)))
)

adj_factor <- 3
x_adj <-  3.3
# plot two key highlight tiles
ggplot(data = key_tiles,
       aes(x, y, height = h, width = w, label = text)) +
  geom_tile(aes(fill = color)) +
  scale_fill_manual(values = c("#dd7400", "#d1d3d4")) +
  geom_text(color = "white", fontface = "bold", size = 16*adj_factor,
            aes(label = val, x = x - x_adj, y = y + 1.5), hjust = 0) +
  geom_text(color = "white", fontface = "bold", size = 10*adj_factor,
            aes(label = pct_change, x = x - x_adj, y = y + 0.5), hjust = 0) +
  geom_text(color = "white", fontface = "bold", size = 6*adj_factor,
            aes(label = text, x = x - x_adj, y = y - 1.3), hjust = 0) +
  geom_text(color = "white", fontface = "bold", size = 6*adj_factor,
            aes(label = change, x = x - x_adj, y = y - 1.8), hjust = 0) +
  coord_fixed() +
  geom_text(size = 30*adj_factor,
            color = "white",
            aes(label = icon,
                family = 'fontawesome-webfont',
                x = x + 1.5, y = y + 0.3,),
            alpha = 0.25) +
  theme(plot.margin = unit(c(-0.30,0,0,0), "null"),
        legend.position = "none") + # remove margin around plot
  theme_void() +
  guides(fill = "none")
