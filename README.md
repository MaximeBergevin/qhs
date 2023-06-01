# Questionnaires in Health Sciences (QHS)
`qhs` is meant to be an easy to use, dplyr-friendly package to score and decode common questionnaires used in health sciences. As I work in a multidisciplinary laboratory focusing on kinesiology, neuroscience and (neuro)psychology, most of the early functions will be relevant to that field. However, I will be taking requests to add new questionnaires periodically. Please note that this process is time-consumming and I do this on my own time. As such, maintenance and development of this package is most likely going to be slow.

## Installation.
I will start the process to publish it to CRAN once there are enough functions in the package. At the moment, `qhs` is solely hosted on GitHub. You can install it via the following code.
```
devtools::install_github('MaximeBergevin/qhs')
```
## Usage.
Simplicity is a core value of this package. It is a tool that allows to **conveniently** score and decode questionnaires. As such, not a lot of options will be available. In addition to scoring your questionnaires, you will have the option to keep/remove original item responses as well as the item scoring. The most minimalistic output possible returns a dataframe with two columns: participants' IDs and final scores.
Please note a few quirks of this package:
- The column containing participants' IDs will be renamed to `id`.
- The `date` column will be absent if not specified, unless the original dataframe contains the column `date`. At the moment, there is no option to remove this column as it is a useful information to identify the participants' response order in repeated-measure designs. If it is not desired, it is possible to pipe (`%>%`) the output and apply `dplyr::select(-date)`.
- **Most importantly, while the package prevents you from specifying too many columns, it does not prevent you from scoring items in the *wrong* order. However, the code checks if at least one column has only NA values and, if this is the case, will return a warning that item ordering may not be adequate.**

## Available questionnaires

- Chronic pain -- Oswestry Disability Index (ODI)
- Chronic pain -- Pain Catastrophizing Scale (PCS)
- Chronic pain -- Tampa Scale of Kinesiophobia (TSK)
- Psychology -- Beck Depression Inventory II (BDI-II)

## Planned
- Psychology -- Big Five Inventory (BFI)
- Psychology -- State-Trait Anxiety Inventory (STAI)
- Sleep medicine -- Epworth Sleepiness Scale (ESS)
- Sleep medicine --Insomnia Severity Index (ISS)
- Sleep medicine -- Pittsburgh Sleep Quality Index (PSQ)
- MORE TO COME!