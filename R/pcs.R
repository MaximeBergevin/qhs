## Script containing the functions to score the following questionnaire:
## Oswestry Disability Index (ODI)
##
## Author: Maxime Bergevin, MSc
## GitHub: https://github.com/MaximeBergevin
## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# score.pcs assumes the data is set in a long format
# There should only be one line per observation
# Each observation has, at least, 14 columns (1 id + 13 items)
# If this is not the case, please, pivot your dataframe
# This is easily doable with dplyr::pivot_longer

#' Pain Catastrophizing Scale
#'
#' Returns a tibble containing the decoded scores of the Pain Catastrophizing Scale.
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
#' @param date A string specifying the column containing the date.
#' @param keepResponses A logical: should original item responses be kept?
#' @param keepScoring  A logical: should item scoring be kept?
#'
#' @return
#' A dataframe or tibble containing the scored Pain Catastrophizing Scale
#' @references
#'        Wheeler, C., Williams, A., & Morley, S. J. (2019). Meta-analysis of the psychometric properties of the Pain Catastrophizing Scale and associations with participant characteristics. Pain, 160(9), 1946–1953. https://doi.org/10.1097/j.pain.0000000000001494
#'
#'        French, D.J., Noël, M., Vigneau, F., French, J.A., Cyr, C., & Evans, R.T. (2005). L'Échelle de dramatisation face à la douleur PCS-CF Adaptation canadienne en langue française de l'échelle « Pain Catastrophizing Scale ». Canadian Journal of Behavioural Science, 37, 181-192.
#'
#'


score.pcs <- function(
    data,                   # Dataframe containing responses
    obs,                    # String: Column identifying participants
    cols,                   # Vector: Range of columns containing responses
    language = 'english',   # String: 'english'( default) or 'french'
    date = NULL,            # Optional: column with the date of responses
    keepResponses = FALSE,  # Logical: Should original responses be kept?
    keepScoring = FALSE     # Logical: Should item scoring be kept?
) {


  # CHECK: cols should be of length 13
  if(unique(length(cols)) != 13){
    base::stop(
      base::paste(
        'cols should be of length 13. It is currently of length',
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


  # SCORING IN ENGLISH
  if(base::tolower(language) == 'english'){
    .scored.pcs <- data %>%
      dplyr::rename(id = obs) %>%
      dplyr::rename(date = date) %>%
      dplyr::mutate(id = base::tolower(id)) %>%
      # Rename cols enabling the use of case_when
      # Currently, I do not know how to use col index in case_when
      dplyr::rename(
        helplessness.1 = cols[1],
        helplessness.2 = cols[2],
        helplessness.3 = cols[3],
        helplessness.4 = cols[4],
        helplessness.5 = cols[5],
        magnification.6 = cols[6],
        magnification.7 = cols[7],
        rumination.8 = cols[8],
        rumination.9 = cols[9],
        rumination.10 = cols[10],
        rumination.11 = cols[11],
        helplessness.12 = cols[12],
        magnification.13 = cols[13]
      ) %>%
      # Score individual questions
      dplyr::mutate(
        item1 = case_when(
          grepl('not at all', base::tolower(.data$helplessness.1)) ~ 0,
          grepl('slight', base::tolower(.data$helplessness.1)) ~ 1,
          grepl('moderate', base::tolower(.data$helplessness.1)) ~ 2,
          grepl('great', base::tolower(.data$helplessness.1)) ~ 3,
          grepl('all the time', base::tolower(.data$helplessness.1)) ~ 4
          ),
        item2 = case_when(
          grepl('not at all', base::tolower(.data$helplessness.2)) ~ 0,
          grepl('slight', base::tolower(.data$helplessness.2)) ~ 1,
          grepl('moderate', base::tolower(.data$helplessness.2)) ~ 2,
          grepl('great', base::tolower(.data$helplessness.2)) ~ 3,
          grepl('all the time', base::tolower(.data$helplessness.2)) ~ 4
          ),
        item3 = case_when(
          grepl('not at all', base::tolower(.data$helplessness.3)) ~ 0,
          grepl('slight', base::tolower(.data$helplessness.3)) ~ 1,
          grepl('moderate', base::tolower(.data$helplessness.3)) ~ 2,
          grepl('great', base::tolower(.data$helplessness.3)) ~ 3,
          grepl('all the time', base::tolower(.data$helplessness.3)) ~ 4
          ),
        item4 = case_when(
          grepl('not at all', base::tolower(.data$helplessness.4)) ~ 0,
          grepl('slight', base::tolower(.data$helplessness.4)) ~ 1,
          grepl('moderate', base::tolower(.data$helplessness.4)) ~ 2,
          grepl('great', base::tolower(.data$helplessness.4)) ~ 3,
          grepl('all the time', base::tolower(.data$helplessness.4)) ~ 4
          ),
        item5 = case_when(
          grepl('not at all', base::tolower(.data$helplessness.5)) ~ 0,
          grepl('slight', base::tolower(.data$helplessness.5)) ~ 1,
          grepl('moderate', base::tolower(.data$helplessness.5)) ~ 2,
          grepl('great', base::tolower(.data$helplessness.5)) ~ 3,
          grepl('all the time', base::tolower(.data$helplessness.5)) ~ 4
          ),
        item6 = case_when(
          grepl('not at all', base::tolower(.data$magnification.6)) ~ 0,
          grepl('slight', base::tolower(.data$magnification.6)) ~ 1,
          grepl('moderate', base::tolower(.data$magnification.6)) ~ 2,
          grepl('great', base::tolower(.data$magnification.6)) ~ 3,
          grepl('all the time', base::tolower(.data$magnification.6)) ~ 4
          ),
        item7 = case_when(
          grepl('not at all', base::tolower(.data$magnification.7)) ~ 0,
          grepl('slight', base::tolower(.data$magnification.7)) ~ 1,
          grepl('moderate', base::tolower(.data$magnification.7)) ~ 2,
          grepl('great', base::tolower(.data$magnification.7)) ~ 3,
          grepl('all the time', base::tolower(.data$magnification.7)) ~ 4
          ),
        item8 = case_when(
          grepl('not at all', base::tolower(.data$rumination.8)) ~ 0,
          grepl('slight', base::tolower(.data$rumination.8)) ~ 1,
          grepl('moderate', base::tolower(.data$rumination.8)) ~ 2,
          grepl('great', base::tolower(.data$rumination.8)) ~ 3,
          grepl('all the time', base::tolower(.data$rumination.8)) ~ 4
          ),
        item9 = case_when(
          grepl('not at all', base::tolower(.data$rumination.9)) ~ 0,
          grepl('slight', base::tolower(.data$rumination.9)) ~ 1,
          grepl('moderate', base::tolower(.data$rumination.9)) ~ 2,
          grepl('great', base::tolower(.data$rumination.9)) ~ 3,
          grepl('all the time', base::tolower(.data$rumination.9)) ~ 4
          ),
        item10 = case_when(
          grepl('not at all', base::tolower(.data$rumination.10)) ~ 0,
          grepl('slight', base::tolower(.data$rumination.10)) ~ 1,
          grepl('moderate', base::tolower(.data$rumination.10)) ~ 2,
          grepl('great', base::tolower(.data$rumination.10)) ~ 3,
          grepl('all the time', base::tolower(.data$rumination.10)) ~ 4
          ),
        item11 = case_when(
          grepl('not at all', base::tolower(.data$rumination.11)) ~ 0,
          grepl('slight', base::tolower(.data$rumination.11)) ~ 1,
          grepl('moderate', base::tolower(.data$rumination.11)) ~ 2,
          grepl('great', base::tolower(.data$rumination.11)) ~ 3,
          grepl('all the time', base::tolower(.data$rumination.11)) ~ 4
          ),
        item12 = case_when(
          grepl('not at all', base::tolower(.data$helplessness.12)) ~ 0,
          grepl('slight', base::tolower(.data$helplessness.12)) ~ 1,
          grepl('moderate', base::tolower(.data$helplessness.12)) ~ 2,
          grepl('great', base::tolower(.data$helplessness.12)) ~ 3,
          grepl('all the time', base::tolower(.data$helplessness.12)) ~ 4
          ),
        item13 = case_when(
          grepl('not at all', base::tolower(.data$magnification.13)) ~ 0,
          grepl('slight', base::tolower(.data$magnification.13)) ~ 1,
          grepl('moderate', base::tolower(.data$magnification.13)) ~ 2,
          grepl('great', base::tolower(.data$magnification.13)) ~ 3,
          grepl('all the time', base::tolower(.data$magnification.13)) ~ 4
          )
        ) %>%
      # Score the subscale scores
      dplyr::mutate(
        scored.helplessness = .data$item1 + .data$item2 + .data$item3 + .data$item4 + .data$item5 + .data$item12,
        scored.magnification = .data$item6 + .data$item7 + .data$item13,
        scored.rumination = .data$item8 + .data$item9 + .data$item10 + .data$item11,
        pcs = .data$scored.helplessness + .data$scored.magnification + .data$scored.rumination
      )
  }


  # SCORING IN FRENCH
  if(base::tolower(language) == 'french'){
    .scored.pcs <- data %>%
      dplyr::rename(id = obs) %>%
      dplyr::rename(date = date) %>%
      dplyr::mutate(id = base::tolower(id)) %>%
      # Rename cols enabling the use of case_when
      # Currently, I do not know how to use col index in case_when
      dplyr::rename(
        helplessness.1 = cols[1],
        helplessness.2 = cols[2],
        helplessness.3 = cols[3],
        helplessness.4 = cols[4],
        helplessness.5 = cols[5],
        magnification.6 = cols[6],
        magnification.7 = cols[7],
        rumination.8 = cols[8],
        rumination.9 = cols[9],
        rumination.10 = cols[10],
        rumination.11 = cols[11],
        helplessness.12 = cols[12],
        magnification.13 = cols[13]
      ) %>%
      # Score individual questions
      dplyr::mutate(
        item1 = case_when(
          grepl('du tout', base::tolower(.data$helplessness.1)) ~ 0,
          grepl('peu', base::tolower(.data$helplessness.1)) ~ 1,
          grepl('mod.*r.*e', base::tolower(.data$helplessness.1)) ~ 2,
          grepl('beaucoup', base::tolower(.data$helplessness.1)) ~ 3,
          grepl('tout le temps', base::tolower(.data$helplessness.1)) ~ 4
        ),
        item2 = case_when(
          grepl('du tout', base::tolower(.data$helplessness.2)) ~ 0,
          grepl('peu', base::tolower(.data$helplessness.2)) ~ 1,
          grepl('mod.*r.*e', base::tolower(.data$helplessness.2)) ~ 2,
          grepl('beaucoup', base::tolower(.data$helplessness.2)) ~ 3,
          grepl('tout le temps', base::tolower(.data$helplessness.2)) ~ 4
        ),
        item3 = case_when(
          grepl('du tout', base::tolower(.data$helplessness.3)) ~ 0,
          grepl('peu', base::tolower(.data$helplessness.3)) ~ 1,
          grepl('mod.*r.*e', base::tolower(.data$helplessness.3)) ~ 2,
          grepl('beaucoup', base::tolower(.data$helplessness.3)) ~ 3,
          grepl('tout le temps', base::tolower(.data$helplessness.3)) ~ 4
        ),
        item4 = case_when(
          grepl('du tout', base::tolower(.data$helplessness.4)) ~ 0,
          grepl('peu', base::tolower(.data$helplessness.4)) ~ 1,
          grepl('mod.*r.*e', base::tolower(.data$helplessness.4)) ~ 2,
          grepl('beaucoup', base::tolower(.data$helplessness.4)) ~ 3,
          grepl('tout le temps', base::tolower(.data$helplessness.4)) ~ 4
        ),
        item5 = case_when(
          grepl('du tout', base::tolower(.data$helplessness.5)) ~ 0,
          grepl('peu', base::tolower(.data$helplessness.5)) ~ 1,
          grepl('mod.*r.*e', base::tolower(.data$helplessness.5)) ~ 2,
          grepl('beaucoup', base::tolower(.data$helplessness.5)) ~ 3,
          grepl('tout le temps', base::tolower(.data$helplessness.5)) ~ 4
        ),
        item6 = case_when(
          grepl('du tout', base::tolower(.data$magnification.6)) ~ 0,
          grepl('peu', base::tolower(.data$magnification.6)) ~ 1,
          grepl('mod.*r.*e', base::tolower(.data$magnification.6)) ~ 2,
          grepl('beaucoup', base::tolower(.data$magnification.6)) ~ 3,
          grepl('tout le temps', base::tolower(.data$magnification.6)) ~ 4
        ),
        item7 = case_when(
          grepl('du tout', base::tolower(.data$magnification.7)) ~ 0,
          grepl('peu', base::tolower(.data$magnification.7)) ~ 1,
          grepl('mod.*r.*e', base::tolower(.data$magnification.7)) ~ 2,
          grepl('beaucoup', base::tolower(.data$magnification.7)) ~ 3,
          grepl('tout le temps', base::tolower(.data$magnification.7)) ~ 4
        ),
        item8 = case_when(
          grepl('du tout', base::tolower(.data$rumination.8)) ~ 0,
          grepl('peu', base::tolower(.data$rumination.8)) ~ 1,
          grepl('mod.*r.*e', base::tolower(.data$rumination.8)) ~ 2,
          grepl('beaucoup', base::tolower(.data$rumination.8)) ~ 3,
          grepl('tout le temps', base::tolower(.data$rumination.8)) ~ 4
        ),
        item9 = case_when(
          grepl('du tout', base::tolower(.data$rumination.9)) ~ 0,
          grepl('peu', base::tolower(.data$rumination.9)) ~ 1,
          grepl('mod.*r.*e', base::tolower(.data$rumination.9)) ~ 2,
          grepl('beaucoup', base::tolower(.data$rumination.9)) ~ 3,
          grepl('tout le temps', base::tolower(.data$rumination.9)) ~ 4
        ),
        item10 = case_when(
          grepl('du tout', base::tolower(.data$rumination.10)) ~ 0,
          grepl('peu', base::tolower(.data$rumination.10)) ~ 1,
          grepl('mod.*r.*e', base::tolower(.data$rumination.10)) ~ 2,
          grepl('beaucoup', base::tolower(.data$rumination.10)) ~ 3,
          grepl('tout le temps', base::tolower(.data$rumination.10)) ~ 4
        ),
        item11 = case_when(
          grepl('du tout', base::tolower(.data$rumination.11)) ~ 0,
          grepl('peu', base::tolower(.data$rumination.11)) ~ 1,
          grepl('mod.*r.*e', base::tolower(.data$rumination.11)) ~ 2,
          grepl('beaucoup', base::tolower(.data$rumination.11)) ~ 3,
          grepl('tout le temps', base::tolower(.data$rumination.11)) ~ 4
        ),
        item12 = case_when(
          grepl('du tout', base::tolower(.data$helplessness.12)) ~ 0,
          grepl('peu', base::tolower(.data$helplessness.12)) ~ 1,
          grepl('mod.*r.*e', base::tolower(.data$helplessness.12)) ~ 2,
          grepl('beaucoup', base::tolower(.data$helplessness.12)) ~ 3,
          grepl('tout le temps', base::tolower(.data$helplessness.12)) ~ 4
        ),
        item13 = case_when(
          grepl('du tout', base::tolower(.data$magnification.13)) ~ 0,
          grepl('peu', base::tolower(.data$magnification.13)) ~ 1,
          grepl('mod.*r.*e', base::tolower(.data$magnification.13)) ~ 2,
          grepl('beaucoup', base::tolower(.data$magnification.13)) ~ 3,
          grepl('tout le temps', base::tolower(.data$magnification.13)) ~ 4
        )
      ) %>%
      # Score the subscale scores
      dplyr::mutate(
        scored.helplessness = .data$item1 + .data$item2 + .data$item3 + .data$item4 + .data$item5 + .data$item12,
        scored.magnification = .data$item6 + .data$item7 + .data$item13,
        scored.rumination = .data$item8 + .data$item9 + .data$item10 + .data$item11,
        pcs = .data$scored.helplessness + .data$scored.magnification + .data$scored.rumination
      )
  }

  # GIVES A WARNING IF AT LEAST ONE COLUMN HAS MORE THAN 50% NA VALUES
  # This indicates that the cols argument is probably set incorrectly
  # Possibly to supress with
  na_count <- .scored.pcs %>%
    dplyr::select(dplyr::contains("item")) %>%
    dplyr::summarise_all(
      ~ sum(is.na(.))/n()
      )%>%
    tidyr::pivot_longer(cols = 1:13,
                        names_to = 'item',
                        values_to = 'na_prop') %>%
    filter(.data$na_prop >= 0.5)
  if(length(na_count$na_prop) >= 1){
    base::warning("The following items have high proportions (>= 50%) of NA values.",
                  " Argument `cols` may not be set correctly. \n",
                  base::paste0(na_count$item, sep = ', ')
    )
  }


  # Only keep columns relevant to the oswestry.
  .scored.pcs <- .scored.pcs %>%
    dplyr::select(
      id,
      date,
      .data$helplessness.1,
      .data$helplessness.2,
      .data$helplessness.3,
      .data$helplessness.4,
      .data$helplessness.5,
      .data$magnification.6,
      .data$magnification.7,
      .data$rumination.8,
      .data$rumination.9,
      .data$rumination.10,
      .data$rumination.11,
      .data$helplessness.12,
      .data$magnification.13,
      .data$scored.helplessness,
      .data$scored.magnification,
      .data$scored.rumination,
      .data$pcs
    )


  # REMOVES INDIVIDUAL ITEM RESPONSES IF keepResponses == FALSE
  # Keep original responses if keepResponses == TRUE
  if(keepResponses == FALSE){
    .scored.pcs <- .scored.pcs %>%
      dplyr::select(-contains(c('helplessness.',
                                'magnification.',
                                'rumination.'
                                )
                              )
                    )
  }


  # REMOVES INDIVIDUAL ITEM SCORING IF keepScoring == FALSE
  # Keep item scoring if keepScoring == TRUE
  if(keepScoring == FALSE){
    .scored.pcs <- .scored.pcs %>%
      dplyr::select(- contains('scored')
      )
  } else {
    cat('Subscale scores are available.')
  }


  return(.scored.pcs)
}
