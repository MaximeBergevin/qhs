pcs_eng <- dplyr::tibble(
  id = c("id001", "id002", "id003", "id004", "id005"),
  day = c("2022-07-12","2022-07-14","2022-07-25","2022-08-01","2022-08-06"),
  helplessness.1 = c('Not at all', 'To a slight degree', 'To a moderate degree', 'To a great degree', 'All the time'),
  helplessness.2 = c('Not at all', 'To a slight degree', 'To a moderate degree', 'To a great degree', 'All the time'),
  helplessness.3 = c('Not at all', 'To a slight degree', 'To a moderate degree', 'To a great degree', 'All the time'),
  helplessness.4 = c('Not at all', 'To a slight degree', 'To a moderate degree', 'To a great degree', 'All the time'),
  helplessness.5 = c('Not at all', 'To a slight degree', 'To a moderate degree', 'To a great degree', 'All the time'),
  magnification.6 = c('Not at all', 'To a slight degree', 'To a moderate degree', 'To a great degree', 'All the time'),
  magnification.7 = c('Not at all', 'To a slight degree', 'To a moderate degree', 'To a great degree', 'All the time'),
  rumination.8 = c('Not at all', 'To a slight degree', 'To a moderate degree', 'To a great degree', 'All the time'),
  rumination.9 = c('Not at all', 'To a slight degree', 'To a moderate degree', 'To a great degree', 'All the time'),
  rumination.10 = c('Not at all', 'To a slight degree', 'To a moderate degree', 'To a great degree', 'All the time'),
  rumination.11 = c('Not at all', 'To a slight degree', 'To a moderate degree', 'To a great degree', 'All the time'),
  helplessness.12 = c('Not at all', 'To a slight degree', 'To a moderate degree', 'To a great degree', 'All the time'),
  magnification.13 = c('Not at all', 'To a slight degree', 'To a moderate degree', 'To a great degree', 'All the time'),
  scored.helplessness = c(seq(from = 0, to = 24, by = 6)),
  scored.magnification = c(seq(from = 0, to = 12, by = 3)),
  scored.rumination = c(seq(from = 0, to = 16, by = 4)),
  pcs = c(seq(from = 0, to = 52, by = 13))
  )


pcs_fr <- dplyr::tibble(
  id = c("id001", "id002", "id003", "id004", "id005"),
  day = c("2022-07-12","2022-07-14","2022-07-25","2022-08-01","2022-08-06"),
  helplessness.1 = c('Pas du tout', 'Quelque peu', 'De façon modérée', 'Beaucoup', 'Tout le temps'),
  helplessness.2 = c('Pas du tout', 'Quelque peu', 'De façon modérée', 'Beaucoup', 'Tout le temps'),
  helplessness.3 = c('Pas du tout', 'Quelque peu', 'De façon modérée', 'Beaucoup', 'Tout le temps'),
  helplessness.4 = c('Pas du tout', 'Quelque peu', 'De façon modérée', 'Beaucoup', 'Tout le temps'),
  helplessness.5 = c('Pas du tout', 'Quelque peu', 'De façon modérée', 'Beaucoup', 'Tout le temps'),
  magnification.6 = c('Pas du tout', 'Quelque peu', 'De façon modérée', 'Beaucoup', 'Tout le temps'),
  magnification.7 = c('Pas du tout', 'Quelque peu', 'De façon modérée', 'Beaucoup', 'Tout le temps'),
  rumination.8 = c('Pas du tout', 'Quelque peu', 'De façon modérée', 'Beaucoup', 'Tout le temps'),
  rumination.9 = c('Pas du tout', 'Quelque peu', 'De façon modérée', 'Beaucoup', 'Tout le temps'),
  rumination.10 = c('Pas du tout', 'Quelque peu', 'De façon modérée', 'Beaucoup', 'Tout le temps'),
  rumination.11 = c('Pas du tout', 'Quelque peu', 'De façon modérée', 'Beaucoup', 'Tout le temps'),
  helplessness.12 = c('Pas du tout', 'Quelque peu', 'De façon modérée', 'Beaucoup', 'Tout le temps'),
  magnification.13 = c('Pas du tout', 'Quelque peu', 'De façon modérée', 'Beaucoup', 'Tout le temps'),
  scored.helplessness = c(seq(from = 0, to = 24, by = 6)),
  scored.magnification = c(seq(from = 0, to = 12, by = 3)),
  scored.rumination = c(seq(from = 0, to = 16, by = 4)),
  pcs = c(seq(from = 0, to = 52, by = 13))
)


## Creating data frames column by colum with the clipboard
## Copy  the colum you want, then run the two lines below
## Paste the output in a vector in base::data.frame()

#x <- readClipboard()
#cat('"',x, sep = '","','"') # For strings
#cat(x, sep = ",")           # For numbers
