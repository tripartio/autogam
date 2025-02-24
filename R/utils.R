condense_levels <- function(df, other = 'other', threshold = 0.01) {
  df <- df |>
    imap(\(it.col, it.col_name) {
      if (is.character(it.col) || (is.factor(it.col) && !is.ordered(it.col))) {
        it.col <- as.factor(it.col)
        low_freq_levels <- (prop.table(table(it.col)) <= threshold) |>
          which() |>
          names()
        it.col <- if_else(
          it.col %in% low_freq_levels,
          other,
          as.character(it.col)
        ) |>
          factor()
      }

      it.col
    }) |>
    # Restore dataframe structure of the columns
    bind_cols()

  return(
    # Return a list to allow future options (especially the details of transformed columns)
    list(
      data = df
    )
  )
}

