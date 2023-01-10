#' Set some standard font choices
#'
#' @param .ft
#' @param font
#' @param size
#'
#' @return
#' @export
#'
#' @examples
ft_set_fonts <- function(.ft,
                         font = "Calibri",
                         size = 11) {
  .ft %>%
    # Font
    font(fontname = font,
         part = "all") %>%
    fontsize(size = size,
             part = "all")
}

#' Style the header row of the flextable
#'
#' This is a function to style the header row of a flextable. It is
#' stolen from pm-brief. The default is green with white text.
#'
#'
#' @param .ft flextable object
#' @param text_colour the text colour
#' @param bg_colour background colour
#' @param bold make it bold
#' @param italic make it italic
#'
#' @return
#' @export
#'
#' @examples
ft_header_row <- function(.ft,
                          text_colour = "white",
                          bg_colour = rgb(10, 100, 70, maxColorValue = 255),
                          bold = TRUE,
                          italic = FALSE) {
  res <- .ft %>%
    color(color = text_colour,
          part = "header") %>%
    bg(bg = bg_colour,
       part = "header")

  if (bold) {
    res <- res %>%
      bold(part = "header")
  }

  if (italic) {
    res <- res %>%
      italic(part = "header")
  }

  return(res)
}


format_summarised_grocery_table <- function(table_df) {

  scrape_date_grocery <- table_df %>%
    slice_max(scrape_day) %>%
    pull(scrape_day) %>%
    first() %>%
    format('%A %d %B %Y')

  table_df %>%
    transmute(name = glue::glue("{product} ({size})"),
              price = scales::label_dollar()(price)) %>%
    flextable() %>%
    delete_part(part = "header") %>%
    add_header_lines(values = glue::glue("Groceries: Latest price at Coles online on {scrape_date_grocery}")) %>%
    autofit() %>%
    flextable::bg(
      bg = bg_col,
      j = 1
    ) %>%
    flextable::bg(
      bg = bg_col,
      j = 1,
      part = "header"
    ) %>%
    flextable::color(
      j = 1,
      color = "white"
    ) %>%
    flextable::color(
      j = 1,
      color = "white",
      part = "header"
    )
}


format_fuel_prices <- function(output_df) {
  recent_wk_date_top <- output_df %>%
    distinct(week) %>%
    slice_max(week) %>%
    pull(week)

  # flex this bad boi
  output_df %>%
    select(-week) %>%
    relocate(type, .before = location) %>%
    relocate(National, .before = ACT) %>%
    mutate(location = case_when(
      location == "Metro" & type == "per_litre" ~ "Metropolitan \n (%) change",
      location == "Regional" & type == "per_litre" ~ "Regional \n (%) change",
      location == "Metro" & type == "tank" ~ "Metropolitan",
      TRUE ~ location
    )) %>%
    mutate(type = case_when(
      type == "per_litre" ~ "$ per litre",
      type == "tank" ~ "50 litre tank",
      TRUE ~ type
    )) %>%
    flextable() %>%
    add_header_lines(glue::glue("Petrol: Average reported price of petrol between {(recent_wk_date_top %m-% days(7)) %>% format('%d %B %Y')} - {recent_wk_date_top %>% format('%d %B %Y')}")) %>%
    flextable::theme_vanilla() %>%
    merge_at(i = 1:2, j = 1) %>% # merge types
    merge_at(i = 3:4, j = 1) %>%
    merge_at(i = 1:2, j = 3) %>% # merge national
    merge_at(i = 3:4, j = 3) %>%
    merge_at(i = 1:2, j = 4) %>% # merge ACT
    merge_at(i = 3:4, j = 4) %>%
    flextable::bg(
      bg = national_colour,
      j = 3
    ) %>%
    flextable::bg(
      bg = national_colour,
      j = 3,
      part = "header"
    ) %>%
    flextable::bg(
      bg = bg_col,
      j = 1:2
    ) %>%
    flextable::bg(
      bg = bg_col,
      j = 1:2,
      part = "header"
    ) %>%
    flextable::color(
      j = 1:2,
      color = "white"
    ) %>%
    flextable::color(
      j = 1:2,
      color = "white",
      part = "header"
    )

}


format_electricity_table <- function(table_df) {

  elec_date_max <- table_df %>%
    distinct(date_uploaded) %>%
    slice_max(date_uploaded) %>%
    pull(date_uploaded)

  table_df %>%
    select(-date_uploaded) %>%
    flextable() %>%
    autofit() %>%
    add_header_lines(glue::glue("Electricity bills: Median yearly electricity \\
                                plan on Energy Made Easy website, \\
                                {elec_date_max %>% format('%d %B %Y')} (based on a 2-3 person \\
                                household with air conditioning, electric heating and electric water heating)")) %>%
    theme_vanilla() %>%
    flextable::bg(
      bg = bg_col,
      j = 1
    ) %>%
    flextable::bg(
      bg = bg_col,
      j = 1:2,
      part = "header"
    ) %>%
    flextable::bg(
      bg = national_colour,
      j = 2
    ) %>%
    flextable::bg(
      bg = national_colour,
      j = 2,
      part = "header"
    ) %>%
    flextable::color(
      j = 1,
      color = "white"
    ) %>%
    flextable::color(
      j = 1,
      color = "white",
      part = "header"
    )
}


format_rental_table <- function(table_df) {

  max_date <- table_df %>%
    distinct(date_max) %>%
    slice_max(date_max) %>%
    pull(date_max)

  table_df %>%
    select(-date_max) %>%
    mutate(across(National:WA, .fns = scales::label_dollar(accuracy = 1))) %>%
    rename(` ` = state) %>%
    flextable() %>%
    autofit() %>%
    add_header_lines(glue::glue("Rent: Median weekly rental asking price on Domain between {(max_date %m-% lubridate::days(7)) %>% format('%d %B')} - {max_date %>% format('%d %B %Y')}")) %>%
    theme_vanilla() %>%
    flextable::bg(
      bg = bg_col,
      j = 1
    ) %>%
    flextable::bg(
      bg = bg_col,
      j = 1:2,
      part = "header"
    ) %>%
    flextable::bg(
      bg = national_colour,
      j = 2
    ) %>%
    flextable::bg(
      bg = national_colour,
      j = 2,
      part = "header"
    ) %>%
    flextable::color(
      j = 1,
      color = "white"
    ) %>%
    flextable::color(
      j = 1,
      color = "white",
      part = "header"
    )
}


format_mortgages_table <- function(table_df) {
  table_df %>%
    relocate(National, .before = 2) %>%
    relocate(type, .before = 1) %>%
    relocate(ACT, .before = NSW) %>%
    mutate(type = case_when(
      type == "morts" ~ "Monthly repayments",
      TRUE ~ "House prices"
    )) %>%
    rename(` ` = type) %>%
    mutate(across(National:last_col(), .fns = scales::label_dollar(accuracy = 1))) %>%
    flextable() %>%
    autofit() %>%
    add_header_lines(glue::glue("New mortgages: Average monthly mortgage repayments (principal and interest) for houses sold in {today() %>% format('%B %Y')}")) %>%
    theme_vanilla() %>%
    merge_at(i = 1:2, j = 1) %>% # merge types
    merge_at(i = 3:4, j = 1) %>%
    merge_at(i = 1:2, j = 3) %>% # merge national
    merge_at(i = 3:4, j = 3) %>%
    merge_at(i = 1:2, j = 4) %>% # merge ACT
    merge_at(i = 3:4, j = 4) %>%
    flextable::bg(
      bg = bg_col,
      j = 1:2
    ) %>%
    flextable::bg(
      bg = bg_col,
      j = 1:2,
      part = "header"
    ) %>%
    flextable::bg(
      bg = national_colour,
      j = 3
    ) %>%
    flextable::bg(
      bg = national_colour,
      j = 3,
      part = "header"
    ) %>%
    flextable::color(
      j = 1:2,
      color = "white"
    ) %>%
    flextable::color(
      j = 1:2,
      color = "white",
      part = "header"
    ) %>%
    flextable::compose(i = 2, j = 2, flextable::as_paragraph(""), part = "header")
}
