library(tidyverse)
library(tinytable)

conflicted::conflicts_prefer(dplyr::select)
conflicted::conflicts_prefer(dplyr::filter)
conflicted::conflicts_prefer(lmerTest::lmer)

options(list(dplyr.summarise.inform = FALSE))
theme_set( theme_classic() )
