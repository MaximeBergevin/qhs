## Script containing the functions to score the following questionnaire:
## Tampa Scale of Kinesiophobia (TSK)
##
## Author: Maxime Bergevin, MSc
## GitHub: https://github.com/MaximeBergevin
## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# score.tsk assumes the data is set in a long format
# There should only be one line per observation
# Each observation has, at least, 14 columns (1 id + 13 items)
# If this is not the case, please, pivot your dataframe
# This is easily doable with dplyr::pivot_longer

#' Tampa Scale of of Kinesiophobia (TSK)
#'
#' Returns a tibble containing the decoded scores of the Tampa Scale of Kinesiophobia.
#' It is possible to keep the original item repsonses and the item scoring if desired.
#'
#' @import dplyr
#' @export
#'
#' @param data A dataframe containing the survey responses.
#' @param obs A string specifying the column containing participants' id.
#' @param cols An interger vector specifying the range of the items to score.
#'             Must be of length 10. Note that `unique()` is applied on the vector.
#'             A warning is given if too many NA values are detected in the output.
#'             Additionnaly, errors given in ``rename_impl()` also hint towards cols set incorrectly.
#' @param language A string specifying the language. Can be either "english" or "french".
#' @param version An interger specifying the version of the TSK to be decoded. For the moment, only the TSK-11 is supported.
#' @param date A string specifying the column containing the date.
#' @param keepResponses A logical: should original item responses be kept?
#' @param keepScoring  A logical: should item scoring be kept?
#'
#' @return
#' A dataframe or tibble containing the scored Tampa Scale of Kinesiophobia
#' @references
#'        French DJ, France CR, Vigneau F, French JA, Evans RT. Fear of movement/(re) injury in chronic pain: a psychometric assessment of the original English version of the Tampa scale for kinesiophobia (TSK). Pain. 2007 Jan 1;127(1-2):42-51.
#'
#'


score.tsk <- function(
    data,                   # Dataframe containing responses
    obs,                    # String: Column identifying participants
    cols,                   # Vector: Range of columns containing responses
    language = 'english',   # String: 'english'( default) or 'french'
    version = c(11),        # Interger: Version of the TSK scale
    date = NULL,            # Optional: column with the date of responses
    keepResponses = FALSE,  # Logical: Should original responses be kept?
    keepScoring = FALSE     # Logical: Should item scoring be kept?
) {


  # CHECK: cols should be of length 13
  if(unique(length(cols)) != 11){
    base::stop(
      base::paste(
        'cols should be of length 11. It is currently of length',
        unique(length(cols))
      )
    )
  }


  # CHECK: language should be 'EN', 'FR'
  if(!(base::tolower(language) %in% c('english', 'french'))){
    base::stop(
      base::paste(
        "language is currently set to", language, ".",
        "Please, choose between 'english' or 'french'."
      )
    )
  }

  # CHECK: version should be 11 or 17
  if(!(version %in% c(11, 17))){
    base::stop(
      base::paste(
      'version is currently set to', version, '.',
      'Please, choose between 11 or 17.')
    )
  }
  if(version == 17){
    base::stop(
      base::paste(
        'TSK-17 is currently not supported.'
      )
    )
  }

  # SCORING IN ENGLISH, version 11
  if(base::tolower(language) == 'english' & version == 11){
    .scored.tsk <- data %>%
      dplyr::rename(id = obs) %>%
      dplyr::rename(date = date) %>%
      dplyr::mutate(id = base::tolower(id)) %>%
      # Rename cols enabling the use of case_when
      # Currently, I do not know how to use col index in case_when
      dplyr::rename(
        item.1 = cols[1],
        item.2 = cols[2],
        item.3 = cols[3],
        item.4 = cols[4],
        item.5 = cols[5],
        item.6 = cols[6],
        item.7 = cols[7],
        item.8 = cols[8],
        item.9 = cols[9],
        item.10 = cols[10],
        item.11 = cols[11]
      ) %>%
      # Score individual questions
      dplyr::mutate(
        scored.1 = case_when(
          grepl('strongly disagree', base::tolower(item.1)) ~ 1,
          grepl('somewhat disagree', base::tolower(item.1)) ~ 2,
          grepl('somewhat agree', base::tolower(item.1)) ~ 3,
          grepl('strongly agree', base::tolower(item.1)) ~ 4
          ),
        scored.2 = case_when(
          grepl('strongly disagree', base::tolower(item.2)) ~ 1,
          grepl('somewhat disagree', base::tolower(item.2)) ~ 2,
          grepl('somewhat agree', base::tolower(item.2)) ~ 3,
          grepl('strongly agree', base::tolower(item.2)) ~ 4
        ),
        scored.3 = case_when(
          grepl('strongly disagree', base::tolower(item.3)) ~ 1,
          grepl('somewhat disagree', base::tolower(item.3)) ~ 2,
          grepl('somewhat agree', base::tolower(item.3)) ~ 3,
          grepl('strongly agree', base::tolower(item.3)) ~ 4
        ),
        scored.4 = case_when(
          grepl('strongly disagree', base::tolower(item.4)) ~ 1,
          grepl('somewhat disagree', base::tolower(item.4)) ~ 2,
          grepl('somewhat agree', base::tolower(item.4)) ~ 3,
          grepl('strongly agree', base::tolower(item.4)) ~ 4
        ),
        scored.5 = case_when(
          grepl('strongly disagree', base::tolower(item.5)) ~ 1,
          grepl('somewhat disagree', base::tolower(item.5)) ~ 2,
          grepl('somewhat agree', base::tolower(item.5)) ~ 3,
          grepl('strongly agree', base::tolower(item.5)) ~ 4
        ),
        scored.6 = case_when(
          grepl('strongly disagree', base::tolower(item.6)) ~ 1,
          grepl('somewhat disagree', base::tolower(item.6)) ~ 2,
          grepl('somewhat agree', base::tolower(item.6)) ~ 3,
          grepl('strongly agree', base::tolower(item.6)) ~ 4
        ),
        scored.7 = case_when(
          grepl('strongly disagree', base::tolower(item.7)) ~ 1,
          grepl('somewhat disagree', base::tolower(item.7)) ~ 2,
          grepl('somewhat agree', base::tolower(item.7)) ~ 3,
          grepl('strongly agree', base::tolower(item.7)) ~ 4
        ),
        scored.8 = case_when(
          grepl('strongly disagree', base::tolower(item.8)) ~ 1,
          grepl('somewhat disagree', base::tolower(item.8)) ~ 2,
          grepl('somewhat agree', base::tolower(item.8)) ~ 3,
          grepl('strongly agree', base::tolower(item.8)) ~ 4
        ),
        scored.9 = case_when(
          grepl('strongly disagree', base::tolower(item.9)) ~ 1,
          grepl('somewhat disagree', base::tolower(item.9)) ~ 2,
          grepl('somewhat agree', base::tolower(item.9)) ~ 3,
          grepl('strongly agree', base::tolower(item.9)) ~ 4
        ),
        scored.10 = case_when(
          grepl('strongly disagree', base::tolower(item.10)) ~ 1,
          grepl('somewhat disagree', base::tolower(item.10)) ~ 2,
          grepl('somewhat agree', base::tolower(item.10)) ~ 3,
          grepl('strongly agree', base::tolower(item.10)) ~ 4
        ),
        scored.11 = case_when(
          grepl('strongly disagree', base::tolower(item.11)) ~ 1,
          grepl('somewhat disagree', base::tolower(item.11)) ~ 2,
          grepl('somewhat agree', base::tolower(item.11)) ~ 3,
          grepl('strongly agree', base::tolower(item.11)) ~ 4
          )
        ) %>%
      # Score the subscale scores
      dplyr::mutate(
        tsk = scored.1 + scored.2 + scored.3 + scored.4 + scored.5 + scored.6 +
              scored.7 + scored.8 + scored.9 + scored.10 + scored.11
      )
  }


  # SCORING IN FRENCH
  if(base::tolower(language) == 'french'){
    .scored.tsk <- data %>%
      dplyr::rename(id = obs) %>%
      dplyr::rename(date = date) %>%
      dplyr::mutate(id = base::tolower(id)) %>%
      # Rename cols enabling the use of case_when
      # Currently, I do not know how to use col index in case_when
      dplyr::rename(
        item.1 = cols[1],
        item.2 = cols[2],
        item.3 = cols[3],
        item.4 = cols[4],
        item.5 = cols[5],
        item.6 = cols[6],
        item.7 = cols[7],
        item.8 = cols[8],
        item.9 = cols[9],
        item.10 = cols[10],
        item.11 = cols[11]
      ) %>%
      # Score individual questions
      dplyr::mutate(
        scored.1 = case_when(
          grepl('fortement .* accord', base::tolower(item.1)) ~ 4,
          grepl('quelque peu .* accord', base::tolower(item.1)) ~ 3,
          grepl('quelque peu', base::tolower(item.1)) ~ 2,
          grepl('fortement', base::tolower(item.1)) ~ 1
        ),
        scored.2 = case_when(
          grepl('fortement .* accord', base::tolower(item.2)) ~ 4,
          grepl('quelque peu .* accord', base::tolower(item.2)) ~ 3,
          grepl('quelque peu', base::tolower(item.2)) ~ 2,
          grepl('fortement', base::tolower(item.2)) ~ 1
        ),
        scored.3 = case_when(
          grepl('fortement .* accord', base::tolower(item.3)) ~ 4,
          grepl('quelque peu .* accord', base::tolower(item.3)) ~ 3,
          grepl('quelque peu', base::tolower(item.3)) ~ 2,
          grepl('fortement', base::tolower(item.3)) ~ 1
        ),
        scored.4 = case_when(
          grepl('fortement .* accord', base::tolower(item.4)) ~ 4,
          grepl('quelque peu .* accord', base::tolower(item.4)) ~ 3,
          grepl('quelque peu', base::tolower(item.4)) ~ 2,
          grepl('fortement', base::tolower(item.4)) ~ 1
        ),
        scored.5 = case_when(
          grepl('fortement .* accord', base::tolower(item.5)) ~ 4,
          grepl('quelque peu .* accord', base::tolower(item.5)) ~ 3,
          grepl('quelque peu', base::tolower(item.5)) ~ 2,
          grepl('fortement', base::tolower(item.5)) ~ 1
        ),
        scored.6 = case_when(
          grepl('fortement .* accord', base::tolower(item.6)) ~ 4,
          grepl('quelque peu .* accord', base::tolower(item.6)) ~ 3,
          grepl('quelque peu', base::tolower(item.6)) ~ 2,
          grepl('fortement', base::tolower(item.6)) ~ 1
        ),
        scored.7 = case_when(
          grepl('fortement .* accord', base::tolower(item.7)) ~ 4,
          grepl('quelque peu .* accord', base::tolower(item.7)) ~ 3,
          grepl('quelque peu', base::tolower(item.7)) ~ 2,
          grepl('fortement', base::tolower(item.7)) ~ 1
        ),
        scored.8 = case_when(
          grepl('fortement .* accord', base::tolower(item.8)) ~ 4,
          grepl('quelque peu .* accord', base::tolower(item.8)) ~ 3,
          grepl('quelque peu', base::tolower(item.8)) ~ 2,
          grepl('fortement', base::tolower(item.8)) ~ 1
        ),
        scored.9 = case_when(
          grepl('fortement .* accord', base::tolower(item.9)) ~ 4,
          grepl('quelque peu .* accord', base::tolower(item.9)) ~ 3,
          grepl('quelque peu', base::tolower(item.9)) ~ 2,
          grepl('fortement', base::tolower(item.9)) ~ 1
        ),
        scored.10 = case_when(
          grepl('fortement .* accord', base::tolower(item.10)) ~ 4,
          grepl('quelque peu .* accord', base::tolower(item.10)) ~ 3,
          grepl('quelque peu', base::tolower(item.10)) ~ 2,
          grepl('fortement', base::tolower(item.10)) ~ 1
        ),
        scored.11 = case_when(
          grepl('fortement .* accord', base::tolower(item.11)) ~ 4,
          grepl('quelque peu .* accord', base::tolower(item.11)) ~ 3,
          grepl('quelque peu', base::tolower(item.11)) ~ 2,
          grepl('fortement', base::tolower(item.11)) ~ 1
        )
      ) %>%
      # Score the subscale scores
      dplyr::mutate(
        tsk = scored.1 + scored.2 + scored.3 + scored.4 + scored.5 + scored.6 +
          scored.7 + scored.8 + scored.9 + scored.10 + scored.11
      )
  }

  # GIVES A WARNING IF AT LEAST ONE COLUMN HAS MORE THAN 50% NA VALUES
  # This indicates that the cols argument is probably set incorrectly
  # Possibly to supress with
  na_count <- .scored.tsk %>%
    dplyr::select(dplyr::contains("scored")) %>%
    dplyr::summarise_all(
      ~ sum(is.na(.))/length(.scored.tsk$id)) %>%
    tidyr::pivot_longer(cols = 1:11,
                        names_to = 'item',
                        values_to = 'na_prop') %>%
    filter(na_prop >= 0.5)
  if(length(na_count$na_prop) >= 1){
    base::warning("The following items have high proportions (>= 50%) of NA values.",
                  " Argument `cols` may not be set correctly. \n",
                  base::paste0(na_count$item, sep = ', ')
    )
  }


  # Only keep columns relevant to the oswestry.
  .scored.tsk <- .scored.tsk %>%
    dplyr::select(
      id,
      date,
      item.1,
      item.2,
      item.3,
      item.4,
      item.5,
      item.6,
      item.7,
      item.8,
      item.9,
      item.10,
      item.11,
      scored.1,
      scored.2,
      scored.3,
      scored.4,
      scored.5,
      scored.6,
      scored.7,
      scored.8,
      scored.9,
      scored.10,
      scored.11,
      tsk
    )


  # REMOVES INDIVIDUAL ITEM RESPONSES IF keepResponses == FALSE
  # Keep original responses if keepResponses == TRUE
  if(keepResponses == FALSE){
    .scored.tsk <- .scored.tsk %>%
      dplyr::select(-contains(c('item')))
    }


  # REMOVES INDIVIDUAL ITEM SCORING IF keepScoring == FALSE
  # Keep item scoring if keepScoring == TRUE
  if(keepScoring == FALSE){
    .scored.tsk <- .scored.tsk %>%
      dplyr::select(- contains('scored'))
  } else {
    cat('Subscale scores are available.')
  }


  return(.scored.tsk)
}

