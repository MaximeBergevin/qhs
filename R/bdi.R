## Script containing the functions to score the following questionnaire:
## Beck Depression Inventory (BDI-II)
##
## Author: Maxime Bergevin, MSc
## GitHub: https://github.com/MaximeBergevin
## =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# score.bdi assumes the data is set in a long format
# There should only be one line per observation
# Each observation has, at least, 22 columns (1 id + 21 items)
# If this is not the case, please, pivot your dataframe
# This is easily doable with dplyr::pivot_longer

#' Beck Depression Inventory
#'
#' Returns a tibble containing the decoded scores of the Beck Depression Inventory.
#' It is possible to keep the original item repsonses and the item scoring if desired.
#'
#' @import dplyr
#' @export
#'
#' @param data A dataframe containing the survey responses.
#' @param obs A string specifying the column containing participants' id.
#' @param cols An interger vector specifying the range of the items to score.
#'             Must be of length 21. Note that `unique()` is applied on the vector.
#'             A warning is given if too many NA values are detected in the output.
#'             Additionnaly, errors given in ``rename_impl()` also hint towards cols set incorrectly.
#' @param date A string specifying the column containing the date.
#' @param keepResponses A logical: should original item responses be kept?
#' @param keepScoring  A logical: should item scoring be kept?
#'
#' @return
#' A dataframe or tibble containing the scored Beck Depression Inventory.
#'

score.bdi <- function(
    data,                   # Dataframe containing responses
    obs,                    # String: Column identifying participants
    cols,                   # Vector: Range of columns containing responses
    date = NULL,            # Optional: column with the date of responses
    keepResponses = FALSE,  # Logical: Should original responses be kept?
    keepScoring = FALSE     # Logical: Should item scoring be kept?
) {


  # CHECK: cols should be of length 21
  # Returns an error if
  if(unique(length(cols)) != 21){
    base::stop(
      base::paste(
        'cols should be of length 21. It is currently of length',
        unique(length(cols))
      )
    )
  }


  # SCORING IN ENGLISH
    .scored.bdi <- data %>%
      dplyr::rename(id = obs) %>%
      dplyr::rename(date = date) %>%
      dplyr::mutate(id = base::tolower(id)) %>%
      # Rename cols enabling the use of case_when
      # Currently, I do not know how to use col index in case_when
      dplyr::rename(
        sadness = cols[1],
        pessimism = cols[2],
        pastFailure = cols[3],
        lossPleasure = cols[4],
        guiltyFeeling = cols[5],
        punishment = cols[6],
        selfDislike = cols[7],
        selfCriticalness = cols[8],
        suicidalThoughts = cols[9],
        crying = cols[10],
        agitation = cols[11],
        lossInterest = cols[12],
        indecisiveness = cols[13],
        worthlessness = cols[14],
        lossEnergy = cols[15],
        changeSleep = cols[16],
        irritability = cols[17],
        changeAppetite = cols[18],
        concentrationDifficulty = cols[19],
        tiredness = cols[20],
        interestSex = cols[21]
      ) %>%
      # Score individual questions
      dplyr::mutate(
        scored.sadness = dplyr::case_when(
          base::grepl('do not feel', sadness) ~ 0,
          base::grepl('much of the time', sadness) ~ 1,
          base::grepl('all the time', sadness) ~ 2,
          base::grepl('so sad', sadness) ~ 3,
        ),
        scored.pessimism = dplyr::case_when(
          base::grepl('not discouraged', pessimism) ~ 0,
          base::grepl('more discouraged', pessimism) ~ 1,
          base::grepl('do not expect', pessimism) ~ 2,
          base::grepl('future is hopeless', pessimism) ~ 3,
        ),
        scored.pastFailure = dplyr::case_when(
          base::grepl('do not feel', pastFailure) ~ 0,
          base::grepl('failed more', pastFailure) ~ 1,
          base::grepl('lot of failures', pastFailure) ~ 2,
          base::grepl('total failure', pastFailure) ~ 3,
        ),
        scored.lossPleasure = dplyr::case_when(
          base::grepl('as much pleasure', lossPleasure) ~ 0,
          base::grepl('as much as I used to', lossPleasure) ~ 1,
          base::grepl('very little pleasure', lossPleasure) ~ 2,
          base::grepl('any pleasure', lossPleasure) ~ 3,
        ),
        scored.guiltyFeeling = dplyr::case_when(
          base::grepl('particularly', guiltyFeeling) ~ 0,
          base::grepl('over many things', guiltyFeeling) ~ 1,
          base::grepl('most of the time', guiltyFeeling) ~ 2,
          base::grepl('all of the time', guiltyFeeling) ~ 3,
        ),
        scored.punishment = dplyr::case_when(
          base::grepl("don't feel", punishment) ~ 0,
          base::grepl('do not feel', punishment) ~ 0,
          base::grepl('may be', punishment) ~ 1,
          base::grepl('expect to be', punishment) ~ 2,
          base::grepl('am being', punishment) ~ 3,
        ),
        scored.selfDislike = dplyr::case_when(
          base::grepl('feel the same', selfDislike) ~ 0,
          base:: grepl('lost confidence', selfDislike) ~ 1,
          base:: grepl('disappointed', selfDislike) ~ 2,
          base:: grepl('dislike myself', selfDislike) ~ 3,
        ),
        scored.selfCriticalness = dplyr::case_when(
          base::grepl('or blame', selfCriticalness) ~ 0,
          base::grepl('more critical', selfCriticalness) ~ 1,
          base::grepl('for all of my faults', selfCriticalness) ~ 2,
          base::grepl('for everything', selfCriticalness) ~ 3,
        ),
        scored.suicidalThoughts = dplyr::case_when(
          base::grepl('have any', suicidalThoughts) ~ 0,
          base::grepl('have thoughts', suicidalThoughts) ~ 1,
          base::grepl('would like to', suicidalThoughts) ~ 2,
          base::grepl('would kill', suicidalThoughts) ~ 3,
        ),
        scored.crying = dplyr::case_when(
          base::grepl('anymore', crying) ~ 0,
          base::grepl('more than', crying) ~ 1,
          base::grepl('over every', crying) ~ 2,
          base::grepl('feel like crying', crying) ~ 3
      ),
      scored.agitation = dplyr::case_when(
        base::grepl('no more', agitation) ~ 0,
        base::grepl('feel more', agitation) ~ 1,
        base::grepl('hard to stay still', agitation) ~ 2,
        base::grepl('have to keep moving', agitation) ~ 3
      ),
      scored.lossInterest = dplyr::case_when(
        base::grepl('not lost', lossInterest) ~ 0,
        base::grepl('less interested', lossInterest) ~ 1,
        base::grepl('lost most', lossInterest) ~ 2,
        base::grepl('hard to get interested', lossInterest) ~ 3
      ),
      scored.indecisiveness = dplyr::case_when(
        base::grepl('as well as ever', indecisiveness) ~ 0,
        base::grepl('more difficult', indecisiveness) ~ 1,
        base::grepl('greater difficulty', indecisiveness) ~ 2,
        base::grepl('trouble making', indecisiveness) ~ 3
      ),
      scored.worthlessness = dplyr::case_when(
        base::grepl('do.*n.*t feel', worthlessness) ~ 0,
        base::grepl('do.*n.*t consider', worthlessness) ~ 1,
        base::grepl('feel more', worthlessness) ~ 2,
        base::grepl('feel utterly', worthlessness) ~ 3
      ),
      scored.lossEnergy = dplyr::case_when(
        base::grepl('have as much', lossEnergy) ~ 0,
        base::grepl('have less', lossEnergy) ~ 1,
        base::grepl('to do very much', lossEnergy) ~ 2,
        base::grepl('to do anything', lossEnergy) ~ 3
      ),
      scored.changeSleep = dplyr::case_when(
        base::grepl('have not', changeSleep) ~ 0,
        base::grepl('somewhat', changeSleep) ~ 1,
        base::grepl('a lot', changeSleep) ~ 2,
        base::grepl('most of the day', changeSleep) ~ 3,
        base::grepl('wake up', changeSleep) ~ 3
      ),
      scored.irritability = dplyr::case_when(
        base::grepl('no more', irritability) ~ 0,
        base::grepl('am more', irritability) ~ 1,
        base::grepl('much more', irritability) ~ 2,
        base::grepl('all the time', irritability) ~ 3
      ),
      scored.changeAppetite = dplyr::case_when(
        base::grepl('have not', changeAppetite) ~ 0,
        base::grepl('somewhat', changeAppetite) ~ 1,
        base::grepl('is much', changeAppetite) ~ 2,
        base::grepl('no appetite', changeAppetite) ~ 3,
        base::grepl('all the time', changeAppetite) ~ 3
      ),
      scored.concentrationDifficulty = dplyr::case_when(
        base::grepl('as well as ever', concentrationDifficulty) ~ 0,
        base::grepl('can.*t concentrate as well', concentrationDifficulty) ~ 1,
        base::grepl('hard to keep', concentrationDifficulty) ~ 2,
        base::grepl('on anything', concentrationDifficulty) ~ 3,
      ),
      scored.tiredness = dplyr::case_when(
        base::grepl('no more', tiredness) ~ 0,
        base::grepl('more tired', tiredness) ~ 1,
        base::grepl('a lot of', tiredness) ~ 2,
        base::grepl('most of', tiredness) ~ 3,
      ),
      scored.interestSex = dplyr::case_when(
        base::grepl('not noticed', interestSex) ~ 0,
        base::grepl('.*m less', interestSex) ~ 1,
        base::grepl('much less', interestSex) ~ 2,
        base::grepl('lost', interestSex) ~ 3,
        )
      )
    # Sum of every observations' answers (final score)
    .scored.bdi <- .scored.bdi %>%
      dplyr::mutate(
        scored.bdi =
          base::ifelse(is.na(scored.sadness),0,scored.sadness) +
          base::ifelse(is.na(scored.pessimism),0,scored.pessimism) +
          base::ifelse(is.na(scored.pastFailure),0,scored.pastFailure) +
          base::ifelse(is.na(scored.lossPleasure),0,scored.lossPleasure) +
          base::ifelse(is.na(scored.guiltyFeeling),0,scored.guiltyFeeling) +
          base::ifelse(is.na(scored.punishment),0,scored.punishment) +
          base::ifelse(is.na(scored.selfDislike),0,scored.selfDislike) +
          base::ifelse(is.na(scored.selfCriticalness),0,scored.selfCriticalness) +
          base::ifelse(is.na(scored.suicidalThoughts),0,scored.suicidalThoughts) +
          base::ifelse(is.na(scored.crying),0,scored.crying) +
          base::ifelse(is.na(scored.agitation),0,scored.agitation) +
          base::ifelse(is.na(scored.lossInterest),0,scored.lossInterest) +
          base::ifelse(is.na(scored.indecisiveness),0,scored.indecisiveness) +
          base::ifelse(is.na(scored.worthlessness),0,scored.worthlessness) +
          base::ifelse(is.na(scored.lossEnergy),0,scored.lossEnergy) +
          base::ifelse(is.na(scored.changeSleep),0,scored.changeSleep) +
          base::ifelse(is.na(scored.irritability),0,scored.irritability) +
          base::ifelse(is.na(scored.changeAppetite),0,scored.changeAppetite) +
          base::ifelse(is.na(scored.concentrationDifficulty),0,scored.concentrationDifficulty) +
          base::ifelse(is.na(scored.tiredness),0,scored.tiredness) +
          base::ifelse(is.na(scored.interestSex),0,scored.interestSex)
      )


  # Only keep columns relevant to the Beck Depression Inventory.
  .scored.bdi <- .scored.bdi %>%
    dplyr::select(
      id,
      date,
      sadness, scored.sadness,
      pessimism, scored.pessimism,
      pastFailure, scored.pastFailure,
      lossPleasure, scored.lossPleasure,
      guiltyFeeling, scored.guiltyFeeling,
      punishment, scored.punishment,
      selfDislike, scored.selfDislike,
      selfCriticalness, scored.selfCriticalness,
      suicidalThoughts, scored.suicidalThoughts,
      crying, scored.crying,
      agitation, scored.agitation,
      lossInterest, scored.lossInterest,
      indecisiveness, scored.indecisiveness,
      worthlessness, scored.worthlessness,
      lossEnergy, scored.lossEnergy,
      changeSleep, scored.changeSleep,
      irritability, scored.irritability,
      changeAppetite, scored.changeAppetite,
      concentrationDifficulty, scored.concentrationDifficulty,
      tiredness, scored.tiredness,
      interestSex, scored.interestSex,
      scored.bdi
    )


  # GIVES A WARNING IF AT LEAST ONE COLUMN HAS MORE THAN 50% NA VALUES
  # This indicates that the cols argument is probably set incorrectly
  # Possibly to supress with
  na_count <- .scored.bdi %>%
    dplyr::select(dplyr::contains("scored.")) %>%
    dplyr::summarise_all(
      ~ sum(is.na(.))/length(.scored.bdi$id)) %>%
    tidyr::pivot_longer(cols = 2:21,
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
    .scored.bdi <- .scored.bdi %>%
      dplyr::select(- sadness,
                    - pessimism,
                    - pastFailure,
                    - lossPleasure,
                    - guiltyFeeling,
                    - punishment,
                    - selfDislike,
                    - selfCriticalness,
                    - suicidalThoughts,
                    - crying,
                    - agitation,
                    - lossInterest,
                    - indecisiveness,
                    - worthlessness,
                    - lossEnergy,
                    - changeSleep,
                    - irritability,
                    - changeAppetite,
                    - concentrationDifficulty,
                    - tiredness,
                    - interestSex
      )
  }


  # REMOVES INDIVIDUAL ITEM SCORING IF keepScoring == FALSE
  # Keep item scoring if keepScoring == TRUE
  if(keepScoring == FALSE){
    .scored.bdi <- .scored.bdi %>%
      dplyr::select(- scored.sadness,
                    - scored.pessimism,
                    - scored.pastFailure,
                    - scored.lossPleasure,
                    - scored.guiltyFeeling,
                    - scored.punishment,
                    - scored.selfDislike,
                    - scored.selfCriticalness,
                    - scored.suicidalThoughts,
                    - scored.crying,
                    - scored.agitation,
                    - scored.lossInterest,
                    - scored.indecisiveness,
                    - scored.worthlessness,
                    - scored.lossEnergy,
                    - scored.changeSleep,
                    - scored.irritability,
                    - scored.changeAppetite,
                    - scored.concentrationDifficulty,
                    - scored.tiredness,
                    - scored.interestSex
      )
  }

  return(.scored.bdi)
}
