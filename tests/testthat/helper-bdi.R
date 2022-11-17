bdi_eng <- dplyr::tibble(
  id = c("id001", "id002", "id003", "id004", "id005", "id006", "id007"),
  day = c("2022-07-12","2022-07-14","2022-07-16","2022-07-18","2022-07-23", "2022-07-26", "2022-07-27"),
  sadness = c("I do not feel sad.","I feel sad much of the time.","I feel sad much of the time.","I am sad all the time.","I am sad all the time.","I am so sad or unhappy that I can't stand it. ","I am so sad or unhappy that I can't stand it. "),
  pessimism = c("I am not discouraged about my future.","I feel more discouraged about my future than I used to be.","I feel more discouraged about my future than I used to be.","I do not expect things to work out for me.","I do not expect things to work out for me.","I feel my future is hopeless and will only get worse.","I feel my future is hopeless and will only get worse."),
  pastFailure = c("I do not feel like a failure.","I have failed more than I should have.","I have failed more than I should have.","As I look back, I see a lot of failures.","As I look back, I see a lot of failures.","I feel I am a total failure as a person.","I feel I am a total failure as a person."),
  lossPleasure = c("I get as much pleasure as I ever did from the things I enjoy.","I don't enjoy things as much as I used to.","I don't enjoy things as much as I used to.","I get very little pleasure from the things I used to enjoy.","I get very little pleasure from the things I used to enjoy.","I can't get any pleasure from the things I used to enjoy.","I can't get any pleasure from the things I used to enjoy."),
  guiltyFeeling = c("I don't feel particularly guilty.","I feel guilty over many things I have done or should have done.","I feel guilty over many things I have done or should have done.","I feel quite guilty most of the time.","I feel quite guilty most of the time.","I feel guilty all of the time.","I feel guilty all of the time."),
  punishment = c("I don't feel I am being punished.","I feel I may be punished.","I feel I may be punished.","I expect to be punished.","I expect to be punished.","I feel I am being punished.","I feel I am being punished."),
  selfDislike = c("I feel the same about myself as ever.","I have lost confidence in myself.","I have lost confidence in myself.","I am disappointed in myself.","I am disappointed in myself.","I dislike myself.","I dislike myself."),
  selfCriticalness = c("I don't criticize or blame myself more than usual.","I am more critical of myself than I used to be. ","I am more critical of myself than I used to be. ","I criticize myself for all of my faults. ","I criticize myself for all of my faults. ","I blame myself for everything bad that happens.","I blame myself for everything bad that happens."),
  suicidalThoughts = c("I don't have any thoughts of killing myself.","I have thoughts of killing myself, but I would not carry them out.","I have thoughts of killing myself, but I would not carry them out.","I would like to kill myself.","I would like to kill myself.","I would kill myself if I had the chance.","I would kill myself if I had the chance."),
  crying = c("I don't cry anymore than I used to.","I cry more than I used to.","I cry more than I used to.","I cry over every little thing.","I cry over every little thing.","I feel like crying, but I can't.","I feel like crying, but I can't."),
  agitation = c("I am no more restless or wound up than usual.","I feel more restless or wound up than usual.","I feel more restless or wound up than usual.","I am so restless or agitated that it's hard to stay still.","I am so restless or agitated that it's hard to stay still.","I am so restless or agitated that I have to keep moving or doing something.","I am so restless or agitated that I have to keep moving or doing something."),
  lossInterest = c("I have not lost interest in other people or activities.","I am less interested in other people or things than before.","I am less interested in other people or things than before.","I have lost most of my interest in other people or things.","I have lost most of my interest in other people or things.","It's hard to get interested in anything.","It's hard to get interested in anything."),
  indecisiveness = c("I make decisions about as well as ever.","I find it more difficult to make decisions than usual.","I find it more difficult to make decisions than usual.","I have much greater difficulty in making decisions than I used to.","I have much greater difficulty in making decisions than I used to.","I have trouble making any decisions.","I have trouble making any decisions."),
  worthlessness = c("I do not feel I am worthless.","I don't consider myself as worthwhile and useful as I used to.","I don't consider myself as worthwhile and useful as I used to.","I feel more worthless as compared to other people.","I feel more worthless as compared to other people.","I feel utterly worthless.","I feel utterly worthless."),
  lossEnergy = c("I have as much energy as ever.","I have less energy than I used to have.","I have less energy than I used to have.","I don't have enough energy to do very much.","I don't have enough energy to do very much.","I don't have enough energy to do anything.","I don't have enough energy to do anything."),
  changeSleep = c("I have not experienced any change in my sleeping pattern.","I sleep somewhat more than usual.","I sleep somewhat less than usual.","I sleep a lot more than usual.","I sleep a lot less than usual.","I sleep most of the day.","I wake up 1-2 hours early and can't get back to sleep."),
  irritability = c("I am no more irritable than usual.","I am more irritable than usual.","I am more irritable than usual.","I am much more irritable than usual.","I am much more irritable than usual.","I am irritable all the time.","I am irritable all the time."),
  changeAppetite = c("I have not experienced any change in my appetite.","My appetite is somewhat less than usual.","My appetite is somewhat greater than usual.","My appetite is much less than before.","My appetite is much greater than usual.","I have no appetite at all.","I crave food all the time."),
  concentrationDifficulty = c("I can concentrate as well as ever. ","I can't concentrate as well as usual.","I can't concentrate as well as usual.","It's hard to keep my mind on anything for very long.","It's hard to keep my mind on anything for very long.","I find I can't concentrate on anything.","I find I can't concentrate on anything."),
  tiredness = c("I am no more tired or fatigued than usual.","I get more tired or fatigued more easily than usual.","I get more tired or fatigued more easily than usual.","I am too tired or fatigued to do a lot of the things I used to do.","I am too tired or fatigued to do a lot of the things I used to do.","I am too tired or fatigued to do most of the things I used to do. ","I am too tired or fatigued to do most of the things I used to do."),
  interestSex = c("I have not noticed any recent change in my interest in sex.","I am less interested in sex than I used to be. ","I am less interested in sex than I used to be. ","I am much less interested in sex now.","I am much less interested in sex now.","I have lost interest in sex completely.","I have lost interest in sex completely."),
  scored.sadness = c(0,1,1,2,2,3,3),
  scored.pessimism = c(0,1,1,2,2,3,3),
  scored.pastFailure = c(0,1,1,2,2,3,3),
  scored.lossPleasure = c(0,1,1,2,2,3,3),
  scored.guiltyFeeling = c(0,1,1,2,2,3,3),
  scored.punishment = c(0,1,1,2,2,3,3),
  scored.selfDislike = c(0,1,1,2,2,3,3),
  scored.selfCriticalness = c(0,1,1,2,2,3,3),
  scored.suicidalThoughts = c(0,1,1,2,2,3,3),
  scored.crying = c(0,1,1,2,2,3,3),
  scored.agitation = c(0,1,1,2,2,3,3),
  scored.lossInterest = c(0,1,1,2,2,3,3),
  scored.indecisiveness = c(0,1,1,2,2,3,3),
  scored.worthlessness = c(0,1,1,2,2,3,3),
  scored.lossEnergy = c(0,1,1,2,2,3,3),
  scored.changeSleep = c(0,1,1,2,2,3,3),
  scored.irritability = c(0,1,1,2,2,3,3),
  scored.changeAppetite = c(0,1,1,2,2,3,3),
  scored.concentrationDifficulty = c(0,1,1,2,2,3,3),
  scored.tiredness = c(0,1,1,2,2,3,3),
  scored.interestSex = c(0,1,1,2,2,3,3),
  scored.bdi = c(0,21,21,42,42,63,63)
  )


## Creating data frames column by colum with the clipboard
## Copy  the colum you want, then run the two lines below
## Paste the output in a vector in base::data.frame()

x <- readClipboard()
#cat('"',x, sep = '","','"') # For strings
#cat(x, sep = ",")           # For numbers
