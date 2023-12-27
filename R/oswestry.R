## Script containing the functions to score the following questionnaire:
## Oswestry Disability Index (ODI)
##
## Author: Maxime Bergevin, MSc
## GitHub: https://github.com/MaximeBergevin
## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# score.oswestry assumes the data is set in a long format
# There should only be one line per observation
# Each observation has, at least, 11 columns (1 id + 10 items)
# If this is not the case, please, pivot your dataframe
# This is easily doable with dplyr::pivot_longer

#' Oswestry Disability Index
#'
#' Returns a tibble containing the decoded scores of the Oswestry Disability Index.
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
#' A dataframe or tibble containing the scored Oswestry Disability Index.
#' @references
#'        Denis, I., & Fortin, L. (2012). Development of a French-Canadian version of the Oswestry Disability Index: cross-cultural adaptation and validation. Spine, 37(7), E439–E444. https://doi.org/10.1097/BRS.0b013e318233eaf9
#'
#'        Fairbank J. C. (2014). Why are there different versions of the Oswestry Disability Index?. Journal of neurosurgery. Spine, 20(1), 83–86. https://doi.org/10.3171/2013.9.SPINE13344
#'

score.oswestry <- function(
    data,                   # Dataframe containing responses
    obs,                    # String: Column identifying participants
    cols,                   # Vector: Range of columns containing responses
    language = 'english',   # String: 'english'( default) or 'french'
    date = NULL,            # Optional: column with the date of responses
    keepResponses = FALSE,  # Logical: Should original responses be kept?
    keepScoring = FALSE     # Logical: Should item scoring be kept?
) {


  # CHECK: cols should be of length 21
  # Returns an error if
  if(unique(length(cols)) != 10){
    base::stop(
      base::paste(
        'cols should be of length 10. It is currently of length',
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
    .scored.oswestry <- data %>%
      dplyr::rename(id = obs) %>%
      dplyr::rename(date = date) %>%
      dplyr::mutate(id = base::tolower(id)) %>%
      # Rename cols enabling the use of case_when
      # Currently, I do not know how to use col index in case_when
      dplyr::rename(
        painIntensity = cols[1],
        personalCare = cols[2],
        lifting = cols[3],
        walking = cols[4],
        sitting = cols[5],
        standing = cols[6],
        sleeping = cols[7],
        sexLife = cols[8],
        socialLife = cols[9],
        travelling = cols[10]
      ) %>%
      # Score individual questions
      dplyr::mutate(
        scored.pain = dplyr::case_when(
          base::grepl('no pain', .data$painIntensity) ~ 0,
          base::grepl('pain is very mild', .data$painIntensity) ~ 1,
          base::grepl('I manage', .data$painIntensity) ~ 1,
          base::grepl('pain is moderate', .data$painIntensity) ~ 2,
          base::grepl('pain is fairly severe', .data$painIntensity) ~ 3,
          base::grepl('pain is very severe', .data$painIntensity) ~ 4,
          base::grepl('pain is the worst', .data$painIntensity) ~ 5
        ),
        scored.personalCare = dplyr::case_when(
          base::grepl('without causing', .data$personalCare) ~ 0,
          base::grepl('causes extra pain', .data$personalCare) ~ 1,
          base::grepl('slow and careful', .data$personalCare) ~ 2,
          base::grepl('need some help', .data$personalCare) ~ 3,
          base::grepl('need help every day', .data$personalCare) ~ 4,
          base::grepl('stay in bed', .data$personalCare) ~ 5,
        ),
        scored.lifting = dplyr::case_when(
          base::grepl('without extra pain', .data$lifting) ~ 0,
          base::grepl('gives extra pain', .data$lifting) ~ 1,
          base::grepl('if they are conveniently placed', .data$lifting) ~ 2,
          base::grepl('if they are conveniently positioned', .data$lifting) ~ 3,
          base::grepl('light weights', .data$lifting) ~ 4,
          base:: grepl('cannot lift', .data$lifting) ~ 5
        ),
        scored.walking = dplyr::case_when(
          base::grepl('does not prevent me', .data$walking) ~ 0,
          base::grepl('more than 1 mile', .data$walking) ~ 1,
          base::grepl('more than one mile', .data$walking) ~ 1,
          base::grepl('more than 2 k.*m', .data$walking) ~ 1,
          base::grepl('more than two k.*m', .data$walking) ~ 1,
          base::grepl('more than 1/4 mile', .data$walking) ~ 2,
          base::grepl('more than one quarter of a mile', .data$walking) ~ 2,
          base::grepl('more than 1 k.*m', .data$walking) ~ 2,
          base::grepl('more than one k.*m', .data$walking) ~ 2,
          base::grepl('more than 100 yards', .data$walking) ~ 3,
          base::grepl('more than one hundred yards', .data$walking) ~ 3,
          base::grepl('more than 500 met', .data$walking) ~ 3,
          base::grepl('more than five hundred met', .data$walking) ~ 3,
          base::grepl('using a stick or crutches', .data$walking) ~ 4,
          base::grepl('bed most of the time', .data$walking) ~ 5
        ),
        scored.sitting = dplyr::case_when(
          base::grepl('in any chair', .data$sitting) ~ 0,
          base::grepl('favo.*rite chair', .data$sitting) ~ 1,
          base::grepl('more than one hour', .data$sitting) ~ 2,
          base::grepl('more than 1 hour', .data$sitting) ~ 2,
          base::grepl('more than 1/2 hour', .data$sitting) ~ 3,
          base::grepl('more than half an hour', .data$sitting) ~ 3,
          base::grepl('more than 30 min', .data$sitting) ~ 3,
          base::grepl('more than thirty min', .data$sitting) ~ 3,
          base::grepl('more than 10 min', .data$sitting) ~ 4,
          base::grepl('more than ten min', .data$sitting) ~ 4,
          base::grepl('sitting at all', .data$sitting) ~ 5
        ),
        scored.standing = dplyr::case_when(
          base::grepl('without extra pain', .data$standing) ~ 0,
          base::grepl('gives me extra pain', .data$standing) ~ 1,
          base::grepl('more than 1 hour', .data$standing) ~ 2,
          base::grepl('more than one hour', .data$standing) ~ 2,
          base::grepl('more than 60 min', .data$standing) ~ 2,
          base::grepl('more than sixty min', .data$standing) ~  2,
          base::grepl('more than 1/2 hour', .data$standing) ~ 3,
          base::grepl('more than half an hour', .data$standing) ~ 3,
          base::grepl('more than 30 min', .data$standing) ~ 3,
          base::grepl('more than thirty min', .data$standing) ~ 3,
          base::grepl('more than 10 min', .data$standing) ~ 4,
          base::grepl('more than ten min', .data$standing) ~ 4,
          base::grepl('standing at all', .data$standing) ~ 5
        ),
        scored.sleeping = dplyr::case_when(
          base::grepl('never disturbed', .data$sleeping) ~ 0,
          base:: grepl('occasionally disturbed', .data$sleeping) ~ 1,
          base:: grepl('less than 6 hour', .data$sleeping) ~ 2,
          base:: grepl('less than six hour', .data$sleeping) ~ 2,
          base:: grepl('less than 4 hour', .data$sleeping) ~ 3,
          base:: grepl('less than four hour', .data$sleeping) ~ 3,
          base:: grepl('less than 2 hour', .data$sleeping) ~ 4,
          base:: grepl('less than two hour', .data$sleeping) ~ 4,
          base:: grepl('sleeping at all', .data$sleeping) ~ 5
        ),
        scored.sexLife = dplyr::case_when(
          base::grepl('causes no extra pain', .data$sexLife) ~ 0,
          base::grepl('some extra pain', .data$sexLife) ~ 1,
          base::grepl('normal .* painful', .data$sexLife) ~ 2,
          base::grepl('severely restricted', .data$sexLife) ~ 3,
          base::grepl('nearly absent', .data$sexLife) ~ 4,
          base::grepl('prevents any sex life', .data$sexLife) ~ 5
        ),
        scored.socialLife = dplyr::case_when(
          base::grepl('no extra pain', .data$socialLife) ~ 0,
          base::grepl('is normal but', .data$socialLife) ~ 1,
          base::grepl('no .* effect .* apart from', .data$socialLife) ~ 2,
          base::grepl('do not go out as often', .data$socialLife) ~ 3,
          base::grepl('to home', .data$socialLife) ~ 4,
          base::grepl('no social life', .data$socialLife) ~ 5
        ),
        scored.travelling = dplyr::case_when(
          base::grepl('without pain', .data$travelling) ~ 0,
          base::grepl('extra pain', .data$travelling) ~ 1,
          base::grepl('over 2 hour', .data$travelling) ~ 2,
          base::grepl('over two hour', .data$travelling) ~ 2,
          base::grepl('less than 1 hour', .data$travelling) ~ 3,
          base::grepl('less than one hour', .data$travelling) ~ 3,
          base::grepl('short .* journey', .data$travelling) ~ 4,
          base::grepl('except to receive treatment', .data$travelling) ~ 5
        )
      )
    # Sum of every observations' answers
    .scored.oswestry <- .scored.oswestry %>%
      dplyr::mutate(
        numerator =
          base::ifelse(is.na(.data$scored.pain),0,.data$scored.pain) +
          base::ifelse(is.na(.data$scored.personalCare),0,.data$scored.personalCare) +
          base::ifelse(is.na(.data$scored.lifting),0,.data$scored.lifting) +
          base::ifelse(is.na(.data$scored.walking),0,.data$scored.walking) +
          base::ifelse(is.na(.data$scored.sitting),0,.data$scored.sitting) +
          base::ifelse(is.na(.data$scored.standing),0,.data$scored.standing) +
          base::ifelse(is.na(.data$scored.sleeping),0,.data$scored.sleeping) +
          base::ifelse(is.na(.data$scored.sexLife),0,.data$scored.sexLife) +
          base::ifelse(is.na(.data$scored.socialLife),0,.data$scored.socialLife) +
          base::ifelse(is.na(.data$scored.travelling),0,.data$scored.travelling)
      ) %>%
      # Add the number of valid answers (NA values don't count in scoring)
      dplyr::mutate(
        denominator = 50 - 5 * base::rowSums(
          base::is.na(
            .scored.oswestry[, grep('scored.pain', colnames(.scored.oswestry)):
                              grep('scored.travelling', colnames(.scored.oswestry))]
          )
        )
      ) %>%
      # Computes the final Oswestry score
      dplyr::mutate(oswestry = .data$numerator / .data$denominator * 100)
  }


  # SCORING IN FRENCH
  if(base::tolower(language) == 'french'){
    .scored.oswestry <- data %>%
      dplyr::rename(id = obs) %>%
      dplyr::rename(date = date) %>%
      dplyr::mutate(id = base::tolower(id)) %>%
      # Rename cols enabling the use of case_when
      # Currently, I do not know how to use col index in case_when
      dplyr::rename(
        painIntensity = cols[1],
        personalCare = cols[2],
        lifting = cols[3],
        walking = cols[4],
        sitting = cols[5],
        standing = cols[6],
        sleeping = cols[7],
        sexLife = cols[8],
        socialLife = cols[9],
        travelling = cols[10]
      ) %>%
      # Score individual questions
      dplyr::mutate(
        scored.pain = dplyr::case_when(
          base::grepl('pas de douleur', .data$painIntensity) ~ 0,
          base::grepl('tr.*s l.*g.*re', .data$painIntensity) ~ 1,
          base::grepl('mod.*r.*e', .data$painIntensity) ~ 2,
          base::grepl('assez forte', .data$painIntensity) ~ 3,
          base::grepl('tr.*s forte', .data$painIntensity) ~ 4,
          base::grepl('douleur est la pire', .data$painIntensity) ~ 5,
          base::grepl('au-del.* de toute description', .data$painIntensity) ~ 5
        ),
        scored.personalCare = dplyr::case_when(
          base::grepl('sans augmenter', .data$personalCare) ~ 0,
          base::grepl('augment.* douleur', .data$personalCare) ~ 1,
          base::grepl('douloureux', .data$personalCare) ~ 2,
          base::grepl('un peu d.*aide', .data$personalCare) ~ 3,
          base::grepl('besoin d.*aide', .data$personalCare) ~ 4,
          base::grepl('je reste au lit', .data$personalCare) ~ 5,
        ),
        scored.lifting = dplyr::case_when(
          base::grepl('sans augmenter', .data$lifting) ~ 0,
          base::grepl('augment.* douleur', .data$lifting) ~ 1,
          base::grepl('partir du sol', .data$lifting) ~ 2,
          base::grepl('objets l.*gers ou moyens', .data$lifting) ~ 3,
          base::grepl('tr.*s l.*gers', .data$lifting) ~ 4,
          base:: grepl('rien soulever ni transporter', .data$lifting) ~ 5
        ),
        scored.walking = dplyr::case_when(
          base::grepl('peu importe', .data$walking) ~ 0,
          base::grepl('une mille', .data$walking) ~ 1,
          base::grepl('1 mille', .data$walking) ~ 1,
          base::grepl('1.5 k.*m', .data$walking) ~ 1,
          base::grepl('1,5 k.*m', .data$walking) ~ 1,
          base::grepl('une kilom.*tre et demi', .data$walking) ~ 1,
          base::grepl('0.5 mille', .data$walking) ~ 2,
          base::grepl('0,5 mille', .data$walking) ~ 2,
          base::grepl('1/2 mille', .data$walking) ~ 2,
          base::grepl('une demi-mille', .data$walking) ~ 2,
          base::grepl('0.75 k.*m', .data$walking) ~ 2,
          base::grepl('0,75 k.*m', .data$walking) ~ 2,
          base::grepl('3/4 .* k.*m', .data$walking) ~ 2,
          base::grepl('100 verges', .data$walking) ~ 3,
          base::grepl('cent verges', .data$walking) ~ 3,
          base::grepl('100 m.*tre', .data$walking) ~ 3,
          base::grepl('cent m.*tre', .data$walking) ~ 3,
          base::grepl('une canne', .data$walking) ~ 4,
          base::grepl('suis au lit', .data$walking) ~ 5
        ),
        scored.sitting = dplyr::case_when(
          base::grepl('n.*importe quel fauteuil', .data$sitting) ~ 0,
          base::grepl('fauteuil pr.*f.*r.*', .data$sitting) ~ 1,
          base::grepl('1 heure', .data$sitting) ~ 2,
          base::grepl('une heure', .data$sitting) ~ 2,
          base::grepl('1/2 heure', .data$sitting) ~ 3,
          base::grepl('0.5 heure', .data$sitting) ~ 3,
          base::grepl('0,5 heure', .data$sitting) ~ 3,
          base::grepl('une demi-heure', .data$sitting) ~ 3,
          base::grepl('10 min', .data$sitting) ~ 4,
          base::grepl('dix min', .data$sitting) ~ 4,
          base::grepl('compl.*tement', .data$sitting) ~ 5
        ),
        scored.standing = dplyr::case_when(
          base::grepl('sans augmenter la douleur', .data$standing) ~ 0,
          base::grepl('cela augmente la douleur', .data$standing) ~ 1,
          base::grepl('1 heure', .data$standing) ~ 2,
          base::grepl('une heure', .data$standing) ~ 2,
          base::grepl('60 min', .data$standing) ~ 2,
          base::grepl('soixante min', .data$standing) ~  2,
          base::grepl('0.5 heure', .data$standing) ~ 3,
          base::grepl('0,5 heure', .data$standing) ~ 3,
          base::grepl('une demi-heure', .data$standing) ~ 3,
          base::grepl('30 min', .data$standing) ~ 3,
          base::grepl('trente min', .data$standing) ~ 3,
          base::grepl('10 min', .data$standing) ~ 4,
          base::grepl('dix min', .data$standing) ~ 4,
          base::grepl('compl.*tement', .data$standing) ~ 5
        ),
        scored.sleeping = dplyr::case_when(
          base::grepl('jamais', .data$sleeping) ~ 0,
          base:: grepl('parfois', .data$sleeping) ~ 1,
          base:: grepl('6 heure', .data$sleeping) ~ 2,
          base:: grepl('six heure', .data$sleeping) ~ 2,
          base:: grepl('4 heure', .data$sleeping) ~ 3,
          base:: grepl('quatre heure', .data$sleeping) ~ 3,
          base:: grepl('2 heure', .data$sleeping) ~ 4,
          base:: grepl('deux heure', .data$sleeping) ~ 4,
          base:: grepl('compl.*tement', .data$sleeping) ~ 5
        ),
        scored.sexLife = dplyr::case_when(
          base::grepl('pas d.*augmentation de la douleur', .data$sexLife) ~ 0,
          base::grepl('certaine augmentation de la douleur', .data$sexLife) ~ 1,
          base::grepl('presque normale', .data$sexLife) ~ 2,
          base::grepl('tr.*s limit.*e', .data$sexLife) ~ 3,
          base::grepl('absente', .data$sexLife) ~ 4,
          base::grepl('emp.*che toute vie sexuelle', .data$sexLife) ~ 5
        ),
        scored.socialLife = dplyr::case_when(
          base::grepl('pas d.*augmentation', .data$socialLife) ~ 0,
          base::grepl('augment.*', .data$socialLife) ~ 1,
          base::grepl('limiter .* vigoureuses', .data$socialLife) ~ 2,
          base::grepl('ne sors plus autant qu.*avant', .data$socialLife) ~ 3,
          base::grepl('mon domicile', .data$socialLife) ~ 4,
          base::grepl('pas de vie sociale', .data$socialLife) ~ 5
        ),
        scored.travelling = dplyr::case_when(
          base::grepl('sans douleur', .data$travelling) ~ 0,
          base::grepl('augment.*', .data$travelling) ~ 1,
          base::grepl('2 heure', .data$travelling) ~ 2,
          base::grepl('deux heures', .data$travelling) ~ 2,
          base::grepl('d.*placements de moins', .data$travelling) ~ 3,
          base::grepl('courts d.*placements', .data$travelling) ~ 4,
          base::grepl('recevoir des traitements', .data$travelling) ~ 5
        )
      )
    # Sum of every observations' answers
    .scored.oswestry <- .scored.oswestry %>%
      dplyr::mutate(
        numerator =
          base::ifelse(is.na(.data$scored.pain),0,.data$scored.pain) +
          base::ifelse(is.na(.data$scored.personalCare),0,.data$scored.personalCare) +
          base::ifelse(is.na(.data$scored.lifting),0,.data$scored.lifting) +
          base::ifelse(is.na(.data$scored.walking),0,.data$scored.walking) +
          base::ifelse(is.na(.data$scored.sitting),0,.data$scored.sitting) +
          base::ifelse(is.na(.data$scored.standing),0,.data$scored.standing) +
          base::ifelse(is.na(.data$scored.sleeping),0,.data$scored.sleeping) +
          base::ifelse(is.na(.data$scored.sexLife),0,.data$scored.sexLife) +
          base::ifelse(is.na(.data$scored.socialLife),0,.data$scored.socialLife) +
          base::ifelse(is.na(.data$scored.travelling),0,.data$scored.travelling)
      ) %>%
      # Add the number of valid answers (NA values don't count in scoring)
      dplyr::mutate(
        denominator = 50 - 5 * base::rowSums(
          base::is.na(
            .scored.oswestry[, grep('scored.pain', colnames(.scored.oswestry)):
                               grep('scored.travelling', colnames(.scored.oswestry))]
          )
        )
      ) %>%
      # Computes the final Oswestry score
      dplyr::mutate(oswestry = .data$numerator / .data$denominator * 100)
  }

  # Only keep columns relevant to the oswestry.
  .scored.oswestry <- .scored.oswestry %>%
    dplyr::select(
      id,
      date,
      .data$painIntensity,
      .data$personalCare,
      .data$lifting,
      .data$walking,
      .data$sitting,
      .data$standing,
      .data$sleeping,
      .data$sexLife,
      .data$socialLife,
      .data$travelling,
      .data$scored.pain,
      .data$scored.personalCare,
      .data$scored.lifting,
      .data$scored.walking,
      .data$scored.sitting,
      .data$scored.standing,
      .data$scored.sleeping,
      .data$scored.sexLife,
      .data$scored.socialLife,
      .data$scored.travelling,
      .data$numerator,
      .data$denominator,
      .data$oswestry
    )


  # GIVES A WARNING IF AT LEAST ONE COLUMN HAS MORE THAN 50% NA VALUES
  # This indicates that the cols argument is probably set incorrectly
  # Possibly to supress with
  na_count <- .scored.oswestry %>%
    dplyr::select(dplyr::contains("scored.")) %>%
    dplyr::summarise_all(
      ~ sum(is.na(.)) / n()
      ) %>%
    tidyr::pivot_longer(cols = 2:10,
                        names_to = 'item',
                        values_to = 'na_prop') %>%
    filter(.data$na_prop >= 0.5)
  if(length(na_count$na_prop) >= 1){
    base::warning("The following items have high proportions (>= 50%) of NA values.",
                  " Argument `cols` may not be set correctly. \n",
                  base::paste0(na_count$item, sep = ', ')
    )
  }


  # REMOVES INDIVIDUAL ITEM RESPONSES IF keepResponses == FALSE
  # Keep original responses if keepResponses == TRUE
  if(keepResponses == FALSE){
    .scored.oswestry <- .scored.oswestry %>%
      dplyr::select(- .data$painIntensity,
                    - .data$personalCare,
                    - .data$lifting,
                    - .data$walking,
                    - .data$sitting,
                    - .data$standing,
                    - .data$sleeping,
                    - .data$sexLife,
                    - .data$socialLife,
                    - .data$travelling
      )
  }


  # REMOVES INDIVIDUAL ITEM SCORING IF keepScoring == FALSE
  # Keep item scoring if keepScoring == TRUE
  if(keepScoring == FALSE){
    .scored.oswestry <- .scored.oswestry %>%
      dplyr::select(- .data$scored.pain,
                    - .data$scored.personalCare,
                    - .data$scored.lifting,
                    - .data$scored.walking,
                    - .data$scored.sitting,
                    - .data$scored.standing,
                    - .data$scored.sleeping,
                    - .data$scored.sexLife,
                    - .data$scored.socialLife,
                    - .data$scored.travelling,
                    - .data$numerator,
                    - .data$denominator
      )
  } else {
    cat('numerator is the sum of item scoring of each observation. \n',
    'denominator is the maximal scoring possible, adjusted for NA values.')
  }


  return(.scored.oswestry)
}
