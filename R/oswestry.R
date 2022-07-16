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
          base::grepl('no pain', painIntensity) ~ 0,
          base::grepl('pain is very mild', painIntensity) ~ 1,
          base::grepl('I manage', painIntensity) ~ 1,
          base::grepl('pain is moderate', painIntensity) ~ 2,
          base::grepl('pain is fairly severe', painIntensity) ~ 3,
          base::grepl('pain is very severe', painIntensity) ~ 4,
          base::grepl('pain is the worst', painIntensity) ~ 5
        ),
        scored.personalCare = dplyr::case_when(
          base::grepl('without causing', personalCare) ~ 0,
          base::grepl('causes extra pain', personalCare) ~ 1,
          base::grepl('slow and careful', personalCare) ~ 2,
          base::grepl('need some help', personalCare) ~ 3,
          base::grepl('need help every day', personalCare) ~ 4,
          base::grepl('stay in bed', personalCare) ~ 5,
        ),
        scored.lifting = dplyr::case_when(
          base::grepl('without extra pain', lifting) ~ 0,
          base::grepl('gives extra pain', lifting) ~ 1,
          base::grepl('if they are conveniently placed', lifting) ~ 2,
          base::grepl('if they are conveniently positioned', lifting) ~ 3,
          base::grepl('light weights', lifting) ~ 4,
          base:: grepl('cannot lift', lifting) ~ 5
        ),
        scored.walking = dplyr::case_when(
          base::grepl('does not prevent me', walking) ~ 0,
          base::grepl('more than 1 mile', walking) ~ 1,
          base::grepl('more than one mile', walking) ~ 1,
          base::grepl('more than 2 k.*m', walking) ~ 1,
          base::grepl('more than two k.*m', walking) ~ 1,
          base::grepl('more than 1/4 mile', walking) ~ 2,
          base::grepl('more than one quarter of a mile', walking) ~ 2,
          base::grepl('more than 1 k.*m', walking) ~ 2,
          base::grepl('more than one k.*m', walking) ~ 2,
          base::grepl('more than 100 yards', walking) ~ 3,
          base::grepl('more than one hundred yards', walking) ~ 3,
          base::grepl('more than 500 met', walking) ~ 3,
          base::grepl('more than five hundred met', walking) ~ 3,
          base::grepl('using a stick or crutches', walking) ~ 4,
          base::grepl('bed most of the time', walking) ~ 5
        ),
        scored.sitting = dplyr::case_when(
          base::grepl('in any chair', sitting) ~ 0,
          base::grepl('favo.*rite chair', sitting) ~ 1,
          base::grepl('more than one hour', sitting) ~ 2,
          base::grepl('more than 1 hour', sitting) ~ 2,
          base::grepl('more than 1/2 hour', sitting) ~ 3,
          base::grepl('more than half an hour', sitting) ~ 3,
          base::grepl('more than 30 min', sitting) ~ 3,
          base::grepl('more than thirty min', sitting) ~ 3,
          base::grepl('more than 10 min', sitting) ~ 4,
          base::grepl('more than ten min', sitting) ~ 4,
          base::grepl('sitting at all', sitting) ~ 5
        ),
        scored.standing = dplyr::case_when(
          base::grepl('without extra pain', standing) ~ 0,
          base::grepl('gives me extra pain', standing) ~ 1,
          base::grepl('more than 1 hour', standing) ~ 2,
          base::grepl('more than one hour', standing) ~ 2,
          base::grepl('more than 60 min', standing) ~ 2,
          base::grepl('more than sixty min', standing) ~  2,
          base::grepl('more than 1/2 hour', standing) ~ 3,
          base::grepl('more than half an hour', standing) ~ 3,
          base::grepl('more than 30 min', standing) ~ 3,
          base::grepl('more than thirty min', standing) ~ 3,
          base::grepl('more than 10 min', standing) ~ 4,
          base::grepl('more than ten min', standing) ~ 4,
          base::grepl('standing at all', standing) ~ 5
        ),
        scored.sleeping = dplyr::case_when(
          base::grepl('never disturbed', sleeping) ~ 0,
          base:: grepl('occasionally disturbed', sleeping) ~ 1,
          base:: grepl('less than 6 hour', sleeping) ~ 2,
          base:: grepl('less than six hour', sleeping) ~ 2,
          base:: grepl('less than 4 hour', sleeping) ~ 3,
          base:: grepl('less than four hour', sleeping) ~ 3,
          base:: grepl('less than 2 hour', sleeping) ~ 4,
          base:: grepl('less than two hour', sleeping) ~ 4,
          base:: grepl('sleeping at all', sleeping) ~ 5
        ),
        scored.sexLife = dplyr::case_when(
          base::grepl('causes no extra pain', sexLife) ~ 0,
          base::grepl('some extra pain', sexLife) ~ 1,
          base::grepl('normal .* painful', sexLife) ~ 2,
          base::grepl('severely restricted', sexLife) ~ 3,
          base::grepl('nearly absent', sexLife) ~ 4,
          base::grepl('prevents any sex life', sexLife) ~ 5
        ),
        scored.socialLife = dplyr::case_when(
          base::grepl('no extra pain', socialLife) ~ 0,
          base::grepl('is normal but', socialLife) ~ 1,
          base::grepl('no .* effect .* apart from', socialLife) ~ 2,
          base::grepl('do not go out as often', socialLife) ~ 3,
          base::grepl('to home', socialLife) ~ 4,
          base::grepl('no social life', socialLife) ~ 5
        ),
        scored.travelling = dplyr::case_when(
          base::grepl('without pain', travelling) ~ 0,
          base::grepl('extra pain', travelling) ~ 1,
          base::grepl('over 2 hour', travelling) ~ 2,
          base::grepl('over two hour', travelling) ~ 2,
          base::grepl('less than 1 hour', travelling) ~ 3,
          base::grepl('less than one hour', travelling) ~ 3,
          base::grepl('short .* journey', travelling) ~ 4,
          base::grepl('except to receive treatment', travelling) ~ 5
        )
      )
    # Sum of every observations' answers
    .scored.oswestry <- .scored.oswestry %>%
      dplyr::mutate(
        numerator =
          base::ifelse(is.na(scored.pain),0,scored.pain) +
          base::ifelse(is.na(scored.personalCare),0,scored.personalCare) +
          base::ifelse(is.na(scored.lifting),0,scored.lifting) +
          base::ifelse(is.na(scored.walking),0,scored.walking) +
          base::ifelse(is.na(scored.sitting),0,scored.sitting) +
          base::ifelse(is.na(scored.standing),0,scored.standing) +
          base::ifelse(is.na(scored.sleeping),0,scored.sleeping) +
          base::ifelse(is.na(scored.sexLife),0,scored.sexLife) +
          base::ifelse(is.na(scored.socialLife),0,scored.socialLife) +
          base::ifelse(is.na(scored.travelling),0,scored.travelling)
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
      dplyr::mutate(oswestry = numerator / denominator * 100)
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
          base::grepl('pas de douleur', painIntensity) ~ 0,
          base::grepl('tr.*s l.*g.*re', painIntensity) ~ 1,
          base::grepl('mod.*r.*e', painIntensity) ~ 2,
          base::grepl('assez forte', painIntensity) ~ 3,
          base::grepl('tr.*s forte', painIntensity) ~ 4,
          base::grepl('douleur est la pire', painIntensity) ~ 5,
          base::grepl('au-del.* de toute description', painIntensity) ~ 5
        ),
        scored.personalCare = dplyr::case_when(
          base::grepl('sans augmenter', personalCare) ~ 0,
          base::grepl('augment.* douleur', personalCare) ~ 1,
          base::grepl('douloureux', personalCare) ~ 2,
          base::grepl('un peu d.*aide', personalCare) ~ 3,
          base::grepl('besoin d.*aide', personalCare) ~ 4,
          base::grepl('je reste au lit', personalCare) ~ 5,
        ),
        scored.lifting = dplyr::case_when(
          base::grepl('sans augmenter', lifting) ~ 0,
          base::grepl('augment.* douleur', lifting) ~ 1,
          base::grepl('partir du sol', lifting) ~ 2,
          base::grepl('objets l.*gers ou moyens', lifting) ~ 3,
          base::grepl('tr.*s l.*gers', lifting) ~ 4,
          base:: grepl('rien soulever ni transporter', lifting) ~ 5
        ),
        scored.walking = dplyr::case_when(
          base::grepl('peu importe', walking) ~ 0,
          base::grepl('une mille', walking) ~ 1,
          base::grepl('1 mille', walking) ~ 1,
          base::grepl('1.5 k.*m', walking) ~ 1,
          base::grepl('1,5 k.*m', walking) ~ 1,
          base::grepl('une kilom.*tre et demi', walking) ~ 1,
          base::grepl('0.5 mille', walking) ~ 2,
          base::grepl('0,5 mille', walking) ~ 2,
          base::grepl('1/2 mille', walking) ~ 2,
          base::grepl('une demi-mille', walking) ~ 2,
          base::grepl('0.75 k.*m', walking) ~ 2,
          base::grepl('0,75 k.*m', walking) ~ 2,
          base::grepl('3/4 .* k.*m', walking) ~ 2,
          base::grepl('100 verges', walking) ~ 3,
          base::grepl('cent verges', walking) ~ 3,
          base::grepl('100 m.*tre', walking) ~ 3,
          base::grepl('cent m.*tre', walking) ~ 3,
          base::grepl('une canne', walking) ~ 4,
          base::grepl('suis au lit', walking) ~ 5
        ),
        scored.sitting = dplyr::case_when(
          base::grepl('n.*importe quel fauteuil', sitting) ~ 0,
          base::grepl('fauteuil pr.*f.*r.*', sitting) ~ 1,
          base::grepl('1 heure', sitting) ~ 2,
          base::grepl('une heure', sitting) ~ 2,
          base::grepl('1/2 heure', sitting) ~ 3,
          base::grepl('0.5 heure', sitting) ~ 3,
          base::grepl('0,5 heure', sitting) ~ 3,
          base::grepl('une demi-heure', sitting) ~ 3,
          base::grepl('10 min', sitting) ~ 4,
          base::grepl('dix min', sitting) ~ 4,
          base::grepl('compl.*tement', sitting) ~ 5
        ),
        scored.standing = dplyr::case_when(
          base::grepl('sans augmenter la douleur', standing) ~ 0,
          base::grepl('cela augmente la douleur', standing) ~ 1,
          base::grepl('1 heure', standing) ~ 2,
          base::grepl('une heure', standing) ~ 2,
          base::grepl('60 min', standing) ~ 2,
          base::grepl('soixante min', standing) ~  2,
          base::grepl('0.5 heure', standing) ~ 3,
          base::grepl('0,5 heure', standing) ~ 3,
          base::grepl('une demi-heure', standing) ~ 3,
          base::grepl('30 min', standing) ~ 3,
          base::grepl('trente min', standing) ~ 3,
          base::grepl('10 min', standing) ~ 4,
          base::grepl('dix min', standing) ~ 4,
          base::grepl('compl.*tement', standing) ~ 5
        ),
        scored.sleeping = dplyr::case_when(
          base::grepl('jamais', sleeping) ~ 0,
          base:: grepl('parfois', sleeping) ~ 1,
          base:: grepl('6 heure', sleeping) ~ 2,
          base:: grepl('six heure', sleeping) ~ 2,
          base:: grepl('4 heure', sleeping) ~ 3,
          base:: grepl('quatre heure', sleeping) ~ 3,
          base:: grepl('2 heure', sleeping) ~ 4,
          base:: grepl('deux heure', sleeping) ~ 4,
          base:: grepl('compl.*tement', sleeping) ~ 5
        ),
        scored.sexLife = dplyr::case_when(
          base::grepl('pas d.*augmentation de la douleur', sexLife) ~ 0,
          base::grepl('certaine augmentation de la douleur', sexLife) ~ 1,
          base::grepl('presque normale', sexLife) ~ 2,
          base::grepl('tr.*s limit.*e', sexLife) ~ 3,
          base::grepl('absente', sexLife) ~ 4,
          base::grepl('emp.*che toute vie sexuelle', sexLife) ~ 5
        ),
        scored.socialLife = dplyr::case_when(
          base::grepl('pas d.*augmentation', socialLife) ~ 0,
          base::grepl('augment.*', socialLife) ~ 1,
          base::grepl('limiter .* vigoureuses', socialLife) ~ 2,
          base::grepl('ne sors plus autant qu.*avant', socialLife) ~ 3,
          base::grepl('mon domicile', socialLife) ~ 4,
          base::grepl('pas de vie sociale', socialLife) ~ 5
        ),
        scored.travelling = dplyr::case_when(
          base::grepl('sans douleur', travelling) ~ 0,
          base::grepl('augment.*', travelling) ~ 1,
          base::grepl('2 heure', travelling) ~ 2,
          base::grepl('deux heures', travelling) ~ 2,
          base::grepl('d.*placements de moins', travelling) ~ 3,
          base::grepl('courts d.*placements', travelling) ~ 4,
          base::grepl('recevoir des traitements', travelling) ~ 5
        )
      )
    # Sum of every observations' answers
    .scored.oswestry <- .scored.oswestry %>%
      dplyr::mutate(
        numerator =
          base::ifelse(is.na(scored.pain),0,scored.pain) +
          base::ifelse(is.na(scored.personalCare),0,scored.personalCare) +
          base::ifelse(is.na(scored.lifting),0,scored.lifting) +
          base::ifelse(is.na(scored.walking),0,scored.walking) +
          base::ifelse(is.na(scored.sitting),0,scored.sitting) +
          base::ifelse(is.na(scored.standing),0,scored.standing) +
          base::ifelse(is.na(scored.sleeping),0,scored.sleeping) +
          base::ifelse(is.na(scored.sexLife),0,scored.sexLife) +
          base::ifelse(is.na(scored.socialLife),0,scored.socialLife) +
          base::ifelse(is.na(scored.travelling),0,scored.travelling)
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
      dplyr::mutate(oswestry = numerator / denominator * 100)
  }

  # Only keep columns relevant to the oswestry.
  .scored.oswestry <- .scored.oswestry %>%
    dplyr::select(
      id,
      date,
      painIntensity,
      personalCare,
      lifting,
      walking,
      sitting,
      standing,
      sleeping,
      sexLife,
      socialLife,
      travelling,
      scored.pain,
      scored.personalCare,
      scored.lifting,
      scored.walking,
      scored.sitting,
      scored.standing,
      scored.sleeping,
      scored.sexLife,
      scored.socialLife,
      scored.travelling,
      numerator,
      denominator,
      oswestry
    )


  # GIVES A WARNING IF AT LEAST ONE COLUMN HAS MORE THAN 50% NA VALUES
  # This indicates that the cols argument is probably set incorrectly
  # Possibly to supress with
  na_count <- .scored.oswestry %>%
    dplyr::select(dplyr::contains("scored.")) %>%
    dplyr::summarise_all(
      ~ sum(is.na(.))/length(.scored.oswestry$id)) %>%
    tidyr::pivot_longer(cols = 2:10,
                        names_to = 'item',
                        values_to = 'na_prop') %>%
    filter(na_prop >= 0.5)
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
      dplyr::select(- painIntensity,
                    - personalCare,
                    - lifting,
                    - walking,
                    - sitting,
                    - standing,
                    - sleeping,
                    - sexLife,
                    - socialLife,
                    - travelling
      )
  }


  # REMOVES INDIVIDUAL ITEM SCORING IF keepScoring == FALSE
  # Keep item scoring if keepScoring == TRUE
  if(keepScoring == FALSE){
    .scored.oswestry <- .scored.oswestry %>%
      dplyr::select(- scored.pain,
                    - scored.personalCare,
                    - scored.lifting,
                    - scored.walking,
                    - scored.sitting,
                    - scored.standing,
                    - scored.sleeping,
                    - scored.sexLife,
                    - scored.socialLife,
                    - scored.travelling,
                    - numerator,
                    - denominator
      )
  } else {
    cat('numerator is the sum of item scoring of each observation. \n',
    'denominator is the maximal scoring possible, adjusted for NA values.')
  }


  return(.scored.oswestry)
}
