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
          base::grepl('do not feel', .data$sadness) ~ 0,
          base::grepl('much of the time', .data$sadness) ~ 1,
          base::grepl('all the time', .data$sadness) ~ 2,
          base::grepl('so sad', .data$sadness) ~ 3,
        ),
        scored.pessimism = dplyr::case_when(
          base::grepl('not discouraged', .data$pessimism) ~ 0,
          base::grepl('more discouraged', .data$pessimism) ~ 1,
          base::grepl('do not expect', .data$pessimism) ~ 2,
          base::grepl('future is hopeless', .data$pessimism) ~ 3,
        ),
        scored.pastFailure = dplyr::case_when(
          base::grepl('do not feel', .data$pastFailure) ~ 0,
          base::grepl('failed more', .data$pastFailure) ~ 1,
          base::grepl('lot of failures', .data$pastFailure) ~ 2,
          base::grepl('total failure', .data$pastFailure) ~ 3,
        ),
        scored.lossPleasure = dplyr::case_when(
          base::grepl('as much pleasure', .data$lossPleasure) ~ 0,
          base::grepl('as much as I used to', .data$lossPleasure) ~ 1,
          base::grepl('very little pleasure', .data$lossPleasure) ~ 2,
          base::grepl('any pleasure', .data$lossPleasure) ~ 3,
        ),
        scored.guiltyFeeling = dplyr::case_when(
          base::grepl('particularly', .data$guiltyFeeling) ~ 0,
          base::grepl('over many things', .data$guiltyFeeling) ~ 1,
          base::grepl('most of the time', .data$guiltyFeeling) ~ 2,
          base::grepl('all of the time', .data$guiltyFeeling) ~ 3,
        ),
        scored.punishment = dplyr::case_when(
          base::grepl("don't feel", .data$punishment) ~ 0,
          base::grepl('do not feel', .data$punishment) ~ 0,
          base::grepl('may be', .data$punishment) ~ 1,
          base::grepl('expect to be', .data$punishment) ~ 2,
          base::grepl('am being', .data$punishment) ~ 3,
        ),
        scored.selfDislike = dplyr::case_when(
          base::grepl('feel the same', .data$selfDislike) ~ 0,
          base:: grepl('lost confidence', .data$selfDislike) ~ 1,
          base:: grepl('disappointed', .data$selfDislike) ~ 2,
          base:: grepl('dislike myself', .data$selfDislike) ~ 3,
        ),
        scored.selfCriticalness = dplyr::case_when(
          base::grepl('or blame', .data$selfCriticalness) ~ 0,
          base::grepl('more critical', .data$selfCriticalness) ~ 1,
          base::grepl('for all of my faults', .data$selfCriticalness) ~ 2,
          base::grepl('for everything', .data$selfCriticalness) ~ 3,
        ),
        scored.suicidalThoughts = dplyr::case_when(
          base::grepl('have any', .data$suicidalThoughts) ~ 0,
          base::grepl('have thoughts', .data$suicidalThoughts) ~ 1,
          base::grepl('would like to', .data$suicidalThoughts) ~ 2,
          base::grepl('would kill', .data$suicidalThoughts) ~ 3,
        ),
        scored.crying = dplyr::case_when(
          base::grepl('anymore', .data$crying) ~ 0,
          base::grepl('more than', .data$crying) ~ 1,
          base::grepl('over every', .data$crying) ~ 2,
          base::grepl('feel like crying', .data$crying) ~ 3
      ),
      scored.agitation = dplyr::case_when(
        base::grepl('no more', .data$agitation) ~ 0,
        base::grepl('feel more', .data$agitation) ~ 1,
        base::grepl('hard to stay still', .data$agitation) ~ 2,
        base::grepl('have to keep moving', .data$agitation) ~ 3
      ),
      scored.lossInterest = dplyr::case_when(
        base::grepl('not lost', .data$lossInterest) ~ 0,
        base::grepl('less interested', .data$lossInterest) ~ 1,
        base::grepl('lost most', .data$lossInterest) ~ 2,
        base::grepl('hard to get interested', .data$lossInterest) ~ 3
      ),
      scored.indecisiveness = dplyr::case_when(
        base::grepl('as well as ever', .data$indecisiveness) ~ 0,
        base::grepl('more difficult', .data$indecisiveness) ~ 1,
        base::grepl('greater difficulty', .data$indecisiveness) ~ 2,
        base::grepl('trouble making', .data$indecisiveness) ~ 3
      ),
      scored.worthlessness = dplyr::case_when(
        base::grepl('do.*n.*t feel', .data$worthlessness) ~ 0,
        base::grepl('do.*n.*t consider', .data$worthlessness) ~ 1,
        base::grepl('feel more', .data$worthlessness) ~ 2,
        base::grepl('feel utterly', .data$worthlessness) ~ 3
      ),
      scored.lossEnergy = dplyr::case_when(
        base::grepl('have as much', .data$lossEnergy) ~ 0,
        base::grepl('have less', .data$lossEnergy) ~ 1,
        base::grepl('to do very much', .data$lossEnergy) ~ 2,
        base::grepl('to do anything', .data$lossEnergy) ~ 3
      ),
      scored.changeSleep = dplyr::case_when(
        base::grepl('have not', .data$changeSleep) ~ 0,
        base::grepl('somewhat', .data$changeSleep) ~ 1,
        base::grepl('a lot', .data$changeSleep) ~ 2,
        base::grepl('most of the day', .data$changeSleep) ~ 3,
        base::grepl('wake up', .data$changeSleep) ~ 3
      ),
      scored.irritability = dplyr::case_when(
        base::grepl('no more', .data$irritability) ~ 0,
        base::grepl('am more', .data$irritability) ~ 1,
        base::grepl('much more', .data$irritability) ~ 2,
        base::grepl('all the time', .data$irritability) ~ 3
      ),
      scored.changeAppetite = dplyr::case_when(
        base::grepl('have not', .data$changeAppetite) ~ 0,
        base::grepl('somewhat', .data$changeAppetite) ~ 1,
        base::grepl('is much', .data$changeAppetite) ~ 2,
        base::grepl('no appetite', .data$changeAppetite) ~ 3,
        base::grepl('all the time', .data$changeAppetite) ~ 3
      ),
      scored.concentrationDifficulty = dplyr::case_when(
        base::grepl('as well as ever', .data$concentrationDifficulty) ~ 0,
        base::grepl('can.*t concentrate as well', .data$concentrationDifficulty) ~ 1,
        base::grepl('hard to keep', .data$concentrationDifficulty) ~ 2,
        base::grepl('on anything', .data$concentrationDifficulty) ~ 3,
      ),
      scored.tiredness = dplyr::case_when(
        base::grepl('no more', .data$tiredness) ~ 0,
        base::grepl('more tired', .data$tiredness) ~ 1,
        base::grepl('a lot of', .data$tiredness) ~ 2,
        base::grepl('most of', .data$tiredness) ~ 3,
      ),
      scored.interestSex = dplyr::case_when(
        base::grepl('not noticed', .data$interestSex) ~ 0,
        base::grepl('.*m less', .data$interestSex) ~ 1,
        base::grepl('much less', .data$interestSex) ~ 2,
        base::grepl('lost', .data$interestSex) ~ 3,
        )
      )
    # Sum of every observations' answers (final score)
    .scored.bdi <- .scored.bdi %>%
      dplyr::mutate(
        scored.bdi =
          base::ifelse(is.na(.data$scored.sadness),0,.data$scored.sadness) +
          base::ifelse(is.na(.data$scored.pessimism),0,.data$scored.pessimism) +
          base::ifelse(is.na(.data$scored.pastFailure),0,.data$scored.pastFailure) +
          base::ifelse(is.na(.data$scored.lossPleasure),0,.data$scored.lossPleasure) +
          base::ifelse(is.na(.data$scored.guiltyFeeling),0,.data$scored.guiltyFeeling) +
          base::ifelse(is.na(.data$scored.punishment),0,.data$scored.punishment) +
          base::ifelse(is.na(.data$scored.selfDislike),0,.data$scored.selfDislike) +
          base::ifelse(is.na(.data$scored.selfCriticalness),0,.data$scored.selfCriticalness) +
          base::ifelse(is.na(.data$scored.suicidalThoughts),0,.data$scored.suicidalThoughts) +
          base::ifelse(is.na(.data$scored.crying),0,.data$scored.crying) +
          base::ifelse(is.na(.data$scored.agitation),0,.data$scored.agitation) +
          base::ifelse(is.na(.data$scored.lossInterest),0,.data$scored.lossInterest) +
          base::ifelse(is.na(.data$scored.indecisiveness),0,.data$scored.indecisiveness) +
          base::ifelse(is.na(.data$scored.worthlessness),0,.data$scored.worthlessness) +
          base::ifelse(is.na(.data$scored.lossEnergy),0,.data$scored.lossEnergy) +
          base::ifelse(is.na(.data$scored.changeSleep),0,.data$scored.changeSleep) +
          base::ifelse(is.na(.data$scored.irritability),0,.data$scored.irritability) +
          base::ifelse(is.na(.data$scored.changeAppetite),0,.data$scored.changeAppetite) +
          base::ifelse(is.na(.data$scored.concentrationDifficulty),0,.data$scored.concentrationDifficulty) +
          base::ifelse(is.na(.data$scored.tiredness),0,.data$scored.tiredness) +
          base::ifelse(is.na(.data$scored.interestSex),0,.data$scored.interestSex)
      )


  # Only keep columns relevant to the Beck Depression Inventory.
  .scored.bdi <- .scored.bdi %>%
    dplyr::select(
      id,
      date,
      .data$sadness, .data$scored.sadness,
      .data$pessimism, .data$scored.pessimism,
      .data$pastFailure, .data$scored.pastFailure,
      .data$lossPleasure, .data$scored.lossPleasure,
      .data$guiltyFeeling, .data$scored.guiltyFeeling,
      .data$punishment, .data$scored.punishment,
      .data$selfDislike, .data$scored.selfDislike,
      .data$selfCriticalness, .data$scored.selfCriticalness,
      .data$suicidalThoughts, .data$scored.suicidalThoughts,
      .data$crying, .data$scored.crying,
      .data$agitation, .data$scored.agitation,
      .data$lossInterest, .data$scored.lossInterest,
      .data$indecisiveness, .data$scored.indecisiveness,
      .data$worthlessness, .data$scored.worthlessness,
      .data$lossEnergy, .data$scored.lossEnergy,
      .data$changeSleep, .data$scored.changeSleep,
      .data$irritability, .data$scored.irritability,
      .data$changeAppetite, .data$scored.changeAppetite,
      .data$concentrationDifficulty, .data$scored.concentrationDifficulty,
      .data$tiredness, .data$scored.tiredness,
      .data$interestSex, .data$scored.interestSex,
      .data$scored.bdi
    )


  # GIVES A WARNING IF AT LEAST ONE COLUMN HAS MORE THAN 50% NA VALUES
  # This indicates that the cols argument is probably set incorrectly
  # Possibly to supress with
  na_count <- .scored.bdi %>%
    dplyr::select(dplyr::contains("scored.")) %>%
    dplyr::summarise_all(
      ~ sum(is.na(.))/n()
      )%>%
    tidyr::pivot_longer(cols = 2:21,
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
    .scored.bdi <- .scored.bdi %>%
      dplyr::select(- .data$sadness,
                    - .data$pessimism,
                    - .data$pastFailure,
                    - .data$lossPleasure,
                    - .data$guiltyFeeling,
                    - .data$punishment,
                    - .data$selfDislike,
                    - .data$selfCriticalness,
                    - .data$suicidalThoughts,
                    - .data$crying,
                    - .data$agitation,
                    - .data$lossInterest,
                    - .data$indecisiveness,
                    - .data$worthlessness,
                    - .data$lossEnergy,
                    - .data$changeSleep,
                    - .data$irritability,
                    - .data$changeAppetite,
                    - .data$concentrationDifficulty,
                    - .data$tiredness,
                    - .data$interestSex
      )
  }


  # REMOVES INDIVIDUAL ITEM SCORING IF keepScoring == FALSE
  # Keep item scoring if keepScoring == TRUE
  if(keepScoring == FALSE){
    .scored.bdi <- .scored.bdi %>%
      dplyr::select(- .data$scored.sadness,
                    - .data$scored.pessimism,
                    - .data$scored.pastFailure,
                    - .data$scored.lossPleasure,
                    - .data$scored.guiltyFeeling,
                    - .data$scored.punishment,
                    - .data$scored.selfDislike,
                    - .data$scored.selfCriticalness,
                    - .data$scored.suicidalThoughts,
                    - .data$scored.crying,
                    - .data$scored.agitation,
                    - .data$scored.lossInterest,
                    - .data$scored.indecisiveness,
                    - .data$scored.worthlessness,
                    - .data$scored.lossEnergy,
                    - .data$scored.changeSleep,
                    - .data$scored.irritability,
                    - .data$scored.changeAppetite,
                    - .data$scored.concentrationDifficulty,
                    - .data$scored.tiredness,
                    - .data$scored.interestSex
      )
  }

  return(.scored.bdi)
}
