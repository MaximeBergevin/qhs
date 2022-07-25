tsk_eng <- dplyr::tibble(
  id = c("id001", "id002", "id003", "id004"),
  Day = c("2022-07-12","2022-07-14","2022-07-25","2022-08-01"),
  item.1 = c('Strongly disagree', 'Somewhat disagree', 'Somewhat agree', 'Strongly agree'),
  item.2 = c('Strongly disagree', 'Somewhat disagree', 'Somewhat agree', 'Strongly agree'),
  item.3 = c('Strongly disagree', 'Somewhat disagree', 'Somewhat agree', 'Strongly agree'),
  item.4 = c('Strongly disagree', 'Somewhat disagree', 'Somewhat agree', 'Strongly agree'),
  item.5 = c('Strongly disagree', 'Somewhat disagree', 'Somewhat agree', 'Strongly agree'),
  item.6 = c('Strongly disagree', 'Somewhat disagree', 'Somewhat agree', 'Strongly agree'),
  item.7 = c('Strongly disagree', 'Somewhat disagree', 'Somewhat agree', 'Strongly agree'),
  item.8 = c('Strongly disagree', 'Somewhat disagree', 'Somewhat agree', 'Strongly agree'),
  item.9 = c('Strongly disagree', 'Somewhat disagree', 'Somewhat agree', 'Strongly agree'),
  item.10 = c('Strongly disagree', 'Somewhat disagree', 'Somewhat agree', 'Strongly agree'),
  item.11 = c('Strongly disagree', 'Somewhat disagree', 'Somewhat agree', 'Strongly agree'),
  scored.1 = seq(from = 1, to = 4, by = 1),
  scored.2 = seq(from = 1, to = 4, by = 1),
  scored.3 = seq(from = 1, to = 4, by = 1),
  scored.4 = seq(from = 1, to = 4, by = 1),
  scored.5 = seq(from = 1, to = 4, by = 1),
  scored.6 = seq(from = 1, to = 4, by = 1),
  scored.7 = seq(from = 1, to = 4, by = 1),
  scored.8 = seq(from = 1, to = 4, by = 1),
  scored.9 = seq(from = 1, to = 4, by = 1),
  scored.10 = seq(from = 1, to = 4, by = 1),
  scored.11 = seq(from = 1, to = 4, by = 1),
  tsk = seq(from = 11, to = 44, by = 11)
  )

tsk_fr <- dplyr::tibble(
  id = c("id001", "id002", "id003", "id004"),
  Day = c("2022-07-12","2022-07-14","2022-07-25","2022-08-01"),
  item.1 = c('Fortement en désaccord', 'Quelque peu en désaccord', 'Quelque peu en accord', 'Fortement en accord'),
  item.2 = c('Fortement en désaccord', 'Quelque peu en désaccord', 'Quelque peu en accord', 'Fortement en accord'),
  item.3 = c('Fortement en désaccord', 'Quelque peu en désaccord', 'Quelque peu en accord', 'Fortement en accord'),
  item.4 = c('Fortement en désaccord', 'Quelque peu en désaccord', 'Quelque peu en accord', 'Fortement en accord'),
  item.5 = c('Fortement en désaccord', 'Quelque peu en désaccord', 'Quelque peu en accord', 'Fortement en accord'),
  item.6 = c('Fortement en désaccord', 'Quelque peu en désaccord', 'Quelque peu en accord', 'Fortement en accord'),
  item.7 = c('Fortement en désaccord', 'Quelque peu en désaccord', 'Quelque peu en accord', 'Fortement en accord'),
  item.8 = c('Fortement en désaccord', 'Quelque peu en désaccord', 'Quelque peu en accord', 'Fortement en accord'),
  item.9 = c('Fortement en désaccord', 'Quelque peu en désaccord', 'Quelque peu en accord', 'Fortement en accord'),
  item.10 = c('Fortement en désaccord', 'Quelque peu en désaccord', 'Quelque peu en accord', 'Fortement en accord'),
  item.11 = c('Fortement en désaccord', 'Quelque peu en désaccord', 'Quelque peu en accord', 'Fortement en accord'),
  scored.1 = seq(from = 1, to = 4, by = 1),
  scored.2 = seq(from = 1, to = 4, by = 1),
  scored.3 = seq(from = 1, to = 4, by = 1),
  scored.4 = seq(from = 1, to = 4, by = 1),
  scored.5 = seq(from = 1, to = 4, by = 1),
  scored.6 = seq(from = 1, to = 4, by = 1),
  scored.7 = seq(from = 1, to = 4, by = 1),
  scored.8 = seq(from = 1, to = 4, by = 1),
  scored.9 = seq(from = 1, to = 4, by = 1),
  scored.10 = seq(from = 1, to = 4, by = 1),
  scored.11 = seq(from = 1, to = 4, by = 1),
  tsk = seq(from = 11, to = 44, by = 11)
)


## Creating data frames column by colum with the clipboard
## Copy  the colum you want, then run the two lines below
## Paste the output in a vector in base::data.frame()

#x <- readClipboard()
#cat('"',x, sep = '","','"') # For strings
#cat(x, sep = ",")           # For numbers
