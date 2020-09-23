#--------------------------------------------------------------------------------
# @Title: Analysis.R
# @Author: Julian Ibarguen
# @Date created: 23.09.2020
# @Re: predict_idp_displacement
# -----------------------------------------------------------------------------
# Notes:
# The whole project is uploaded to GitHub: 
#   https://github.com/ibarons/predict_idp_displacement
#
# The whole project runs in ~10 min.. It includes the training of Random Forest 
#   algorithms over 4 models.
# 
# All packages are installed automatically, however some packages might need compilation,
#   thus might require to actively accept it.
#-------------------------------------------------------------------------------


########################### SET-UP PACKAGES ####################################

start.all <- Sys.time()

if (!require(utils)) install.packages("utils", repos = "http://cran.us.r-project.org")
if (!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")

if(packageVersion("dplyr") < "1.0.0") {install.packages("dplyr")}

## Install and load packages
check.packages <- function(pkg){
  ## Function description: credit to Danielle Smith
  ## https://gist.github.com/smithdanielle/9913897 . This function checks if a 
  ## specific list of packages are installed and if not, installs them. It then
  ## loads them.  
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))  
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, library, character.only = TRUE)
}

packages.project <- c(
  "stringi","readxl", "lubridate", "scales", "dplyr", "tidyr", "forcats", "purrr",
  "knitr", "kableExtra", "rmarkdown", "ggplot2", "grid", "gridExtra",
  "caret", "matrixStats", "stats", "Rborist", "pls", "glmnet", "Matrix"
)

check.packages(packages.project)


################################ LOAD & CLEAN DATA #############################

# Download data

distances_index <- read.csv(url(
  "https://raw.githubusercontent.com/ibarons/predict_idp_displacement/master/db/itinerary.distances.csv"
))
db_to.from <- read.csv(url(
  "https://raw.githubusercontent.com/ibarons/predict_idp_displacement/master/db/pop.displacement.flow.csv"
))
pop_concent <- read.csv(url(
  "https://raw.githubusercontent.com/ibarons/predict_idp_displacement/master/db/pop.concentration.csv"
))
pop.in.need <- read.csv(url(
  "https://raw.githubusercontent.com/ibarons/predict_idp_displacement/master/db/pop.in.need.csv"
))

# ############################ FORMAT & CLEAN DATA ###############################

###### COMPILATION & CLEAN
# 1. Join data bases
# 2. Clean missing values and not targeted variables 

db_final <- dplyr::left_join(
  db_to.from, pop.in.need, 
  by = c("arrive.to_mun.pcode" = "MunicipalPcode", "pop.group", "datestamp")
) %>%
  dplyr::rename_with(~ gsub("^(.+)", "\\1_at.arrival", .), dplyr::matches("^need\\.|^pop$")) %>%
  dplyr::left_join(
    pop.in.need, 
    by = c("depart.from_mun.pcode" = "MunicipalPcode", "pop.group", "datestamp")
  ) %>%
  dplyr::rename_with(
    ~ gsub("^(.+)", "\\1_at.departure", .), 
    dplyr::matches("^need\\.(health|water|food|nfi|shelter|education|livlihood|security|basicservices)$|^pop$")
  ) %>%
  dplyr::left_join(
    pop_concent, by = c("arrive.to_mun.pcode" = "MunicipalPcode", "datestamp")
  ) %>%
  dplyr::rename_with(
    ~ gsub("^(.+)", "\\1_at.arrival", .), dplyr::matches("^total\\.pop$|^.+\\.ratio$")
  ) %>%
  dplyr::left_join(
    pop_concent, by = c("depart.from_mun.pcode" = "MunicipalPcode", "datestamp")
  ) %>%
  dplyr::rename_with(
    ~ gsub("^(.+)", "\\1_at.departure", .), dplyr::matches("^total\\.pop$|^.+\\.ratio$")
  ) %>%
  dplyr::left_join(
    distances_index, by = c(
      "arrive.to_mun.pcode" = "MunicipalPcode.A", "depart.from_mun.pcode" = "MunicipalPcode.B"
    )
  ) %>%
  dplyr::mutate(
    month.seq = dplyr::if_else(
      lubridate::year(datestamp) == min(lubridate::year(datestamp)), 
      lubridate::month(datestamp) - 1,  lubridate::month(datestamp) + 11
    ),
    itinerary = dplyr::if_else(
      displ.num != 0 , paste0(depart.from_mun.pcode, "_", arrive.to_mun.pcode),
      NA_character_
    ),
    dplyr::across(dplyr::matches("^need\\."),  ~ dplyr::if_else(is.na(.), 0, .)), # Clena NaN (no idps) to 0 (no need)
    dplyr::across( # Clena NA (no population) to 0 (no idp.ratio)
      idp.ratio_at.departure, 
      ~ dplyr::if_else(is.na(.) & total.pop_at.departure == 0, 0, .) 
    )
  ) %>%
  # impute missing values for push factors (median) by closest admin level available
  dplyr::group_by(month.seq, depart.from_mun.pcode) %>%
  dplyr::mutate(dplyr::across(
    dplyr::matches("^push\\."), ~ dplyr::if_else(is.na(.), median(., na.rm = TRUE), .)
  )) %>%
  dplyr::group_by(month.seq, depart.from_pro.pcode) %>%
  dplyr::mutate(dplyr::across(
    dplyr::matches("^push\\."), ~ dplyr::if_else(is.na(.), median(., na.rm = TRUE), .)
  )) %>%
  dplyr::group_by(month.seq) %>%
  dplyr::mutate(dplyr::across(
    dplyr::matches("^push\\."), ~ dplyr::if_else(is.na(.), median(., na.rm = TRUE), .)
  )) %>%
  # impute missing values for pull factors (median) by closest level available
  dplyr::group_by(month.seq, arrive.to_mun.pcode) %>%
  dplyr::mutate(dplyr::across(
    dplyr::matches("^pull\\."), ~ dplyr::if_else(is.na(.), median(., na.rm = TRUE), .)
  )) %>%
  dplyr::group_by(month.seq, arrive.to_pro.pcode) %>%
  dplyr::mutate(dplyr::across( 
    dplyr::matches("^pull\\."), ~ dplyr::if_else(is.na(.), median(., na.rm = TRUE), .)
  )) %>%
  dplyr::group_by(month.seq) %>%
  dplyr::mutate(dplyr::across(
    dplyr::matches("^pull\\."), ~ dplyr::if_else(is.na(.), median(., na.rm = TRUE), .)
  )) %>%
  dplyr::ungroup() %>%
  dplyr::filter(
    pop.group == "idp", # Filter out no target group (no idp)
    displ.num != 0, # Clean no-displacement
    !(displ.num != 0 & is.na(depart.from_pro.pcode)), # Clean missing departure location
    !(displ.num != 0 & depart.from_mun.pcode == "abroad") # "Clean "abroad" departure
  ) %>% 
  dplyr::select( # Remove not applicable variables
    -pull.ProtectAssets, - push.SocialCulturIssues, -push.ForcedReturn, 
    -dplyr::matches("^return.ratio"), -pop.group
  ) %>%
  dplyr::relocate(
    month.seq, itinerary, displ.num, distance, pop_at.arrival, total.pop_at.arrival,
    idp.ratio_at.arrival, pop_at.departure, total.pop_at.departure, idp.ratio_at.departure,
    .after = datestamp
  ) %>%
  dplyr::arrange(month.seq, arrive.to_mun.pcode) %>%
  dplyr::ungroup()

# Check no missing values
miss <- lapply(db_final, function(x) sum(is.na(x)))
miss[which(miss != 0)]

# Basic description
itineraries <- dplyr::count(db_final, itinerary)
n.itineraries <- sum(itineraries$n)
n.repeat.itiner <- mean(itineraries$n > 1)

n.displ.num <- sum(db_final$displ.num)
n.months <- length(unique(db_final$month.seq))


############################## DATA PARTITION: HOLD-OUT ########################

# Hold out last two months
db_hold <- dplyr::filter(db_final, month.seq > 17)
db_train <- dplyr::filter(db_final, month.seq <= 17)


######################### EXPLORATORY ANALYSIS & PREDICTORS ####################

################ DISPLACEMENT PREDICTORS

##### Number of IDP: the outcome

# sumamry stats
summ.displ.num <- summary(db_train$displ.num)

# No transformation
p.displ.num <- ggplot2::ggplot(db_train, ggplot2::aes(displ.num)) +
  ggplot2::geom_histogram(ggplot2::aes(y = ..density..), bins = 10, fill = "gray") +
  ggplot2::geom_density(alpha = 0.2, fill = "red3") +
  ggplot2::scale_x_continuous(labels = scales::comma) +
  ggplot2::labs(x = "number of IDPs") +
  ggplot2::theme_bw()

#Sumamries for log
summ.lg.displ.num <- summary(log10(db_train$displ.num))
sd.lg.displ.num <- sd(log10(db_train$displ.num))

# Log transform
p.lg.displ.num <- ggplot2::ggplot(db_train, ggplot2::aes(log10(displ.num))) +
  ggplot2::geom_histogram(ggplot2::aes(y = ..density..), bins = 10, fill = "gray") +
  ggplot2::geom_density(alpha = 0.2, fill = "red3") +
  ggplot2::geom_vline(
    xintercept =  mean(log10(db_train$displ.num)), linetype = "dashed", color = "red"
  ) +
  ggplot2::geom_vline(
    xintercept =  median(log10(db_train$displ.num)), linetype = "dashed", color = "blue"
  ) +
  ggplot2::labs(
    x = "number of IDPs (log)", 
    caption = "Red dashed line represent the overall average; blue dashed line shows the median"
  ) +
  ggplot2::theme_bw()


##### Time effect on displacement

# Time effect on displacement mean fo raw displacement

displ_time <- db_train %>%
  dplyr::mutate(hist.avg = mean(displ.num, na.rm = TRUE)) %>%
  dplyr::group_by(month.seq, hist.avg) %>%
  dplyr::summarise(avg.displ = mean(displ.num, na.rm = TRUE), .groups = "drop")

ggplot2::ggplot(displ_time, ggplot2::aes(month.seq, avg.displ, group = 1)) +
  ggplot2::geom_line(stat = "identity") +
  ggplot2::geom_hline(
    yintercept =  mean(db_train$displ.num), linetype = "dashed", color = "red"
  ) +
  ggplot2::scale_x_continuous(breaks = 0:17) +
  ggplot2::theme_bw()

# Time effect on displacement boxplot on log displacement

tb.displ_time.lg <- db_train %>%
  dplyr::mutate(hist.avg = mean(displ.num, na.rm = TRUE)) %>%
  dplyr::group_by(month.seq, hist.avg) %>%
  dplyr::summarise(avg.displ = mean(log10(displ.num), na.rm = TRUE), .groups = "drop")

sd.time.lg <- sd(tb.displ_time.lg$avg.displ)

p.time.displ <- ggplot2::ggplot(
  db_train, ggplot2::aes( month.seq, log10(displ.num), group = month.seq)
) + 
  ggplot2::geom_boxplot() + 
  ggplot2::geom_jitter(alpha = 0.05) +
  ggplot2::stat_summary(
    fun = mean, geom = "line", ggplot2::aes(group = 1), linetype = "dashed", color = "blue"
  ) +
  ggplot2::geom_hline(
    yintercept =  mean(log10(db_train$displ.num)), linetype = "dashed", color = "red"
  ) +
  ggplot2::scale_x_continuous(breaks = 0:17) +
  ggplot2::labs(
    x = "Months sequence", y = "number of IDPs (log)",
    caption = "Red dashed line represent the historical average; Blue dashed line shows the mean displacement per month"
  ) +
ggplot2::theme_bw()

# Lagged displacement (autoregressive effect)
tb.disp_lag <- db_train %>%
  dplyr::select(month.seq, displ.num, itinerary) %>%
  dplyr::group_by(itinerary) %>%
  dplyr::mutate(
    lg.displ.mum = log10(displ.num),
    lag_lg.displ.num = dplyr::lag(log10(displ.num), n = 1, order_by = month.seq)
  ) %>%
  dplyr::ungroup()

p.lag.displ <- ggplot2::ggplot(tb.disp_lag, ggplot2::aes(lag_lg.displ.num, log10(displ.num))) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = "gam")  +
  ggplot2::labs(
    x =  "lagged number of IDPs (log)",  y = "number of IDPs (log)"
  ) +
  ggplot2::theme_bw()


###### Itineraries: departure

# Explore location of departure
tb.depart.loc <- db_train %>% 
  dplyr::group_by(depart.from_pro.pcode) %>% 
  dplyr::summarise(avg.depart = mean(log10(displ.num))) 

summ.lg.displ.depart <- summary(tb.depart.loc$avg.depart)
sd.lg.displ.depart <- sd(tb.depart.loc$avg.depart)

# Province of departure
p.depart.loc <- ggplot2::ggplot(db_train, ggplot2::aes(
  depart.from_pro.pcode, log10(displ.num)
)) +
  ggplot2::geom_boxplot() +
  ggplot2::geom_jitter(alpha = 0.1) +
  ggplot2::geom_hline(
    yintercept =  mean(log10(db_train$displ.num)), linetype = "dashed", color = "red"
  ) +
  ggplot2::labs(
    y = "number of IDPs (log)", x = "Province of departure"
  ) + 
  ggplot2::theme_bw()

# Municipality of departure
ggplot2::ggplot(db_train, ggplot2::aes(depart.from_mun.pcode, log10(displ.num))) +
  ggplot2::geom_boxplot() +
  ggplot2::geom_hline(
    yintercept =  mean(log10(db_train$displ.num)), linetype = "dashed", color = "red"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, size = 8)) +
  ggplot2::facet_wrap(depart.from_pro.pcode ~ ., scales = "free") 


###### Itineraries: arrival

# Explore location of arrival
tb.arrival.loc <- db_train %>% 
  dplyr::group_by(arrive.to_pro.pcode) %>% 
  dplyr::summarise(avg.arrive = mean(log10(displ.num))) 

summ.lg.displ.arrive <- summary(tb.arrival.loc$avg.arrive)
sd.lg.displ.arrive <- sd(tb.arrival.loc$avg.arrive)

# Province of arrival
p.arrive.loc <- ggplot2::ggplot(db_train, ggplot2::aes(arrive.to_pro.pcode, log10(displ.num))) +
  ggplot2::geom_boxplot() +
  ggplot2::geom_jitter(alpha = 0.1) +
  ggplot2::geom_hline(
    yintercept =  mean(log10(db_train$displ.num)), linetype = "dashed", color = "red"
  ) +
  ggplot2::labs(
    y = "number of IDPs (log)", x = "Province of arrival",
    caption = "Red dashed line represent the historical average"
  ) + 
  ggplot2::theme_bw()

# Municipality of arrival
ggplot2::ggplot(db_train, ggplot2::aes(arrive.to_mun.pcode, log10(displ.num))) +
  ggplot2::geom_boxplot() +
  ggplot2::geom_hline(
    yintercept =  mean(log10(db_train$displ.num)), linetype = "dashed", color = "red"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, size = 8)) +
  ggplot2::facet_wrap(arrive.to_pro.pcode ~ ., scales = "free")


#### Historical average displacement of the itinerary

# Explore effect of number of times displacement from an itinerary in the relationship 
# between Historical average displacement of the itinerary and displacement numbers
plot <- c()
for(i in 1:4) {
  
  dat <- db_train %>%
    dplyr::group_by(itinerary) %>%
    dplyr::mutate(hist.itiner.avg = mean(log10(displ.num))) %>%
    dplyr::add_count(itinerary) %>%
    dplyr::filter(n >= i)
  
  plot[[i]] <- ggplot2::ggplot(dat, ggplot2::aes(hist.itiner.avg, log10(displ.num))) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth()
  
  print(plot[[i]])
  
}
# Final plot on historical average displacement of the itinerary
p.hist.avg.disp <- db_train %>%
  dplyr::group_by(itinerary) %>%
  dplyr::mutate(hist.itiner.avg = mean(log10(displ.num))) %>%
  dplyr::add_count(itinerary) %>%
  dplyr::filter(n >= i) %>%
  {
    ggplot2::ggplot(., ggplot2::aes(hist.itiner.avg, log10(displ.num))) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth() +
      ggplot2::labs(
        y = "number of IDPs (log)", x = "Historical average of displacement by itinerary",
        caption = "Only itineraries with more than one displacement in the period of study are plotted"
      ) + 
      ggplot2::theme_bw()
  }


##### Number of times departed from a location

# Explore n.time departed
tb.ndepart <- dplyr::add_count(db_train, month.seq, depart.from_mun.pcode, name = "n.depart") %>%
  dplyr::group_by(n.depart) %>%
  dplyr::summarise(avg.ndepart = mean(log10(displ.num)))

summ.ndepart <- summary(tb.ndepart$avg.ndepart)
sd.ndepart <- sd(tb.ndepart$avg.ndepart)

p.n.departed <- dplyr::add_count(db_train, month.seq, depart.from_mun.pcode, name = "n.depart") %>%
  dplyr::ungroup() %>%
  {
    ggplot2::ggplot(., ggplot2::aes(as.factor(n.depart), log10(displ.num))) +
      ggplot2::geom_boxplot() +
      ggplot2::geom_hline(
        yintercept =  mean(log10(db_train$displ.num)), linetype = "dashed", color = "red"
      ) +
      ggplot2::labs(
        y = "number of IDPs (log)", x = "Number of times departed to a location",
        caption = "Red dashed line represent the historical average"
      ) + 
      ggplot2::theme_bw()
  }

##### Number of times arrived to a location

# Explore n.time arrived
tb.narrive <- dplyr::add_count(db_train, month.seq, arrive.to_mun.pcode, name = "n.arrive") %>%
  dplyr::group_by(n.arrive) %>%
  dplyr::summarise(avg.narrive = mean(log10(displ.num)))

summ.narrive <- summary(tb.narrive$avg.narrive)
sd.narrive <- sd(tb.narrive$avg.narrive)

p.n.arrived <- dplyr::add_count(db_train, datestamp, arrive.to_mun.pcode, name = "n.arrive") %>%
  {
    ggplot2::ggplot(., ggplot2::aes(as.factor(n.arrive), log10(displ.num))) +
      ggplot2::geom_boxplot() +
      ggplot2::geom_hline(
        yintercept =  mean(log10(db_train$displ.num)), linetype = "dashed", color = "red"
      ) +
      ggplot2::labs(
        y = "number of IDPs (log)", x = "Number of times arrived to a location",
        caption = "Red dashed line represent the historical average"
      ) +
      ggplot2::theme_bw()
  }


##### Travelled distance

#### Explore distance
summ.distance <- summary(db_train$distance)
sd.distance <- sd(db_train$distance)
cor(db_train$distance, log10(db_train$displ.num))


p.distance <- ggplot2::ggplot(db_train, ggplot2::aes(distance/1000)) +
  ggplot2::geom_histogram(ggplot2::aes(y = ..density..), bins = 10, fill = "gray") +
  ggplot2::geom_density(alpha = 0.2, fill = "red3") +
  ggplot2::geom_vline(
    xintercept =  mean(db_train$distance/1000), linetype = "dashed", color = "red"
  ) +
  ggplot2::geom_vline(
    xintercept =  median(db_train$distance/1000), linetype = "dashed", color = "blue"
  ) +
  ggplot2::labs(
    y = "number of IDPs (log)", x = "Distance (KM)",
    caption = "Red dashed line represent the average; Blue dashed line shows the median distance"
  ) +
  ggplot2::theme_bw()

# Log trasnformation 
ggplot2::ggplot(db_train, ggplot2::aes(log10(distance))) +
  ggplot2::geom_histogram(ggplot2::aes(y = ..density..), bins = 10, fill = "gray") +
  ggplot2::geom_density(alpha = 0.2, fill = "red3") +
  ggplot2::geom_vline(
    xintercept =  mean(log10(db_train$distance[db_train$distance != 0])),
    linetype = "dashed", color = "red"
  ) +
  ggplot2::geom_vline(
    xintercept =  median(log10(db_train$distance[db_train$distance != 0])),
    linetype = "dashed", color = "blue"
  ) +
  ggplot2::theme_bw()

# Distance + displacement
p.lg.displ.distance <- db_train %>%
  dplyr::select(displ.num, distance, month.seq) %>%
  {
    ggplot2::ggplot(., ggplot2::aes(distance/1000, log10(displ.num))) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth(method = "gam")  +
      ggplot2::geom_hline(
        yintercept =  mean(log10(db_train$displ.num)), linetype = "dashed", color = "red"
      ) +
      ggplot2::labs(
        y = "number of IDPs (log)", x = "Distance (KM)",
        caption = "Red dashed line represent the historical average"
      ) +
      ggplot2::theme_bw()
  }


################ DEMOGRAPHIC PREDICTORS

###### Total population

#### Explore total population at departure

summ.pop.depart <- summary(db_train$total.pop_at.departure)
sd.pop.depart <- sd(db_train$total.pop_at.departure)
cor.pop.depart <- cor(db_train$total.pop_at.departure, log10(db_train$displ.num))

# Total population at departure distribution
ggplot2::ggplot(db_train, ggplot2::aes(total.pop_at.departure)) +
  ggplot2::geom_histogram(ggplot2::aes(y = ..density..), bins = 10, fill = "gray") +
  ggplot2::geom_density(alpha = 0.2, fill = "red3") +
  ggplot2::theme_bw()


# Total pop at departure VS log displacement
p.t.pop.depart <- db_train %>%
  dplyr::select(displ.num, total.pop_at.departure) %>%
  {
    ggplot2::ggplot(., ggplot2::aes(total.pop_at.departure, log10(displ.num))) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth(method = "gam") +
      ggplot2::geom_hline(
        yintercept =  mean(log10(db_train$displ.num)), linetype = "dashed", color = "red"
      ) +
      ggplot2::scale_x_continuous(labels = scales::comma) +
      ggplot2::labs(
        y = "number of IDPs (log)", x = "Total population at departure",
        caption = "Red dashed line represent the historical average"
      ) +
      ggplot2::theme_bw()
  }

summ.pop.arrive <- summary(db_train$total.pop_at.arrival)
sd.pop.arrive <- sd(db_train$total.pop_at.arrival)
cor.pop.arrive <- cor(db_train$total.pop_at.arrival, log10(db_train$displ.num))

# Total pop at arrival VS log displacement
p.t.pop.arrive <- db_train %>%
  dplyr::select(displ.num, total.pop_at.arrival) %>%
  {
    ggplot2::ggplot(., ggplot2::aes(total.pop_at.arrival, log10(displ.num))) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth(method = "gam") +
      ggplot2::geom_hline(
        yintercept =  mean(log10(db_train$displ.num)), linetype = "dashed", color = "red"
      ) +
      ggplot2::scale_x_continuous(labels = scales::comma) +
      ggplot2::labs(
        y = "number of IDPs (log)", x = "Total population at arrival",
        caption = "Red dashed line represent the historical average"
      ) +
      ggplot2::theme_bw()
  }


###### Rates of displacement/arrival

# Explore rates of displacement

tb.rate.disp <- db_train %>%
  dplyr::select(month.seq, displ.num, total.pop_at.departure, depart.from_mun.pcode) %>%
  dplyr::group_by(depart.from_mun.pcode, month.seq) %>%
  dplyr::summarise(
    total.pop_at.departure = total.pop_at.departure[1],
    s.displ.num = sum(displ.num)
  ) %>%
  dplyr::mutate(
    lag_total.pop_at.depart = dplyr::lag(
      total.pop_at.departure, n = 1, order_by = month.seq, 
      default = total.pop_at.departure[1]), # t0 imputed by itself to avoid breaking asc|desc trends if imputing by center measure (e.g. median)
    rate.displ = s.displ.num / lag_total.pop_at.depart,
    rate.displ = dplyr::if_else(is.infinite(rate.displ) | rate.displ > 1, 1, rate.displ), # Clean: displ > pop (n = 40/1275)
    lg.rate.displ = log10(rate.displ),
  ) %>%
  dplyr::select(-total.pop_at.departure) %>%
  dplyr::left_join(db_train, ., by = c("month.seq", "depart.from_mun.pcode")) %>%
  dplyr::ungroup() 


summ.rate.disp <- summary(tb.rate.disp$rate.displ)
sd.rate.disp <- sd(tb.rate.disp$rate.displ)
cor.rate.disp <- cor(tb.rate.disp$lg.rate.displ, log10(tb.rate.disp$displ.num))


p.rate.disp <- ggplot2::ggplot(
  tb.rate.disp, ggplot2::aes(log(rate.displ), log10(displ.num))
) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = "gam")  +
  ggplot2::geom_hline(
    yintercept =  mean(log10(db_train$displ.num)), linetype = "dashed", color = "red"
  ) +
  ggplot2::scale_x_continuous(labels = scales::comma) +
  ggplot2::labs(
    y = "number of IDPs (log)", x = "Rate of displacements (log)",
    caption = "Red dashed line represent the historical average"
  ) +
    ggplot2::theme_bw()


# Explore rates of arrival

tb.rate.arrive <- db_train %>%
  dplyr::select(month.seq, displ.num, total.pop_at.arrival, arrive.to_mun.pcode) %>%
  dplyr::group_by(arrive.to_mun.pcode, month.seq) %>%
  dplyr::mutate(
    rate.arrive = displ.num / total.pop_at.arrival,
    rate.arrive = dplyr::if_else(is.infinite(rate.arrive) | rate.arrive > 1, 1, rate.arrive), # Clean: displ > pop (n = 40/1275)
    lg.rate.arrive = log10(rate.arrive),
  ) %>%
  dplyr::select(-total.pop_at.arrival, -displ.num) %>%
  dplyr::left_join(db_train, ., by = c("month.seq", "arrive.to_mun.pcode")) %>%
  dplyr::ungroup()


summ.rate.arrive <- summary(tb.rate.arrive$rate.arrive)
sd.rate.arrive <- sd(tb.rate.arrive$rate.arrive)
cor.rate.arrive <- cor(tb.rate.arrive$lg.rate.arrive, log10(tb.rate.arrive$displ.num))

p.rate.arrive <- ggplot2::ggplot(
  tb.rate.arrive, ggplot2::aes(log10(rate.arrive), log10(displ.num))
) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = "gam")  +
  ggplot2::geom_hline(
    yintercept =  mean(log10(db_train$displ.num)), linetype = "dashed", color = "red"
  ) +
  ggplot2::scale_x_continuous(labels = scales::comma) +
  ggplot2::labs(
    y = "number of IDPs (log)", x = "Rate of arrivals (log)",
    caption = "Red dashed line represent the historical average"
  ) +
  ggplot2::theme_bw()



###### IDP rates at departure and arrival

# Explore IDP rates at departure

summ.idp.rate.depart <- summary(db_train$idp.ratio_at.departure)
sd.idp.rate.depart <- sd(db_train$idp.ratio_at.departure)
cor.idp.rate.depart <- cor(db_train$idp.ratio_at.departure, log10(db_train$displ.num))


p.idp.rate.depart <- db_train %>%
  dplyr::select(displ.num, idp.ratio_at.departure) %>%
  {
    ggplot2::ggplot(., ggplot2::aes(idp.ratio_at.departure, log10(displ.num))) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth(method = "gam") +
      ggplot2::geom_hline(
        yintercept =  mean(log10(db_train$displ.num)), linetype = "dashed", color = "red"
      ) +
      ggplot2::labs(
        y = "number of IDPs (log)", x = "IDP rates at departure",
        caption = "Red dashed line represent the historical average"
      ) +
      ggplot2::theme_bw() 
  }


# Explore IDP rates at arrival

summ.idp.rate.arrive <- summary(db_train$idp.ratio_at.arrival)
sd.idp.rate.arrive <- sd(db_train$idp.ratio_at.arrival)
cor.idp.rate.arrive <- cor(db_train$idp.ratio_at.arrival, log10(db_train$displ.num))

p.idp.rate.arrive <- db_train %>%
  dplyr::select(displ.num, idp.ratio_at.arrival) %>%
  {
    ggplot2::ggplot(., ggplot2::aes(idp.ratio_at.arrival, log10(displ.num))) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth(method = "gam") +
      ggplot2::geom_hline(
        yintercept =  mean(log10(db_train$displ.num)), linetype = "dashed", color = "red"
      ) +
      ggplot2::labs(
        y = "number of IDPs (log)", x = "IDP rates at arrival",
        caption = "Red dashed line represent the historical average"
      ) +
      ggplot2::theme_bw()
  }


################## SECTOR NEEDS

# Explore needs at departure
summ.need.depart <- summary(db_train[, grep("^need.*departure$", colnames(db_train))])
sd.need.depart <- lapply(db_train[, grep("^need.*departure$", colnames(db_train))], sd)
cor.need.depart <- lapply(
  db_train[, grep("^need.*departure$", colnames(db_train))], function(x)
    cor(x, log10(db_train$displ.num))
)
# Tidy cor table for report
cor.need.depart <- dplyr::bind_rows(cor.need.depart) %>%
  tidyr::pivot_longer(
    dplyr::everything(), names_to = c("sector.need", "place"), names_sep = "_"
  ) %>%
  tidyr::pivot_wider(names_from = place, values_from = value)
  
db_train %>%
  dplyr::select(month.seq, displ.num, itinerary, dplyr::matches("need.+_at.departure$")) %>%
  dplyr::select(displ.num, dplyr::matches("need.+_at.departure$")) %>%
  tidyr::pivot_longer(-displ.num, names_to = "need", values_to = "pop.in.need") %>%
  {
    ggplot2::ggplot(., ggplot2::aes(pop.in.need, log10(displ.num))) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth(method = "gam") +
      ggplot2::geom_hline(
        yintercept =  mean(log10(db_train$displ.num)), linetype = "dashed", color = "red"
      ) +
      ggplot2::theme_bw() +
      ggplot2::facet_wrap(. ~ need)
  }


# Explore needs at arrival
summ.need.arrive <- summary(db_train[, grep("^need.*arrival$", colnames(db_train))])
sd.need.arrive <- lapply(db_train[, grep("^need.*arrival$", colnames(db_train))], sd)
cor.need.arrive <- lapply(
  db_train[, grep("^need.*arrival$", colnames(db_train))], function(x)
    cor(x, log10(db_train$displ.num))
)

# Tidy cor table for report
cor.need.arrive <- dplyr::bind_rows(cor.need.arrive) %>%
  tidyr::pivot_longer(
    dplyr::everything(), names_to = c("sector.need", "place"), names_sep = "_"
  ) %>%
  tidyr::pivot_wider(names_from = place, values_from = value)

db_train %>%
  dplyr::select(displ.num, dplyr::matches("need.+_at.arrival$")) %>%
  tidyr::pivot_longer(-displ.num, names_to = "need", values_to = "pop.in.need") %>%
  {
    ggplot2::ggplot(., ggplot2::aes(pop.in.need, log10(displ.num))) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth(method = "gam") +
      ggplot2::geom_hline(
        yintercept =  mean(log10(db_train$displ.num)), linetype = "dashed", color = "red"
      ) +
      ggplot2::theme_bw() +
      ggplot2::facet_wrap(. ~ need)
  }

# Compile tidy cors tables
tb.cor.needs <- dplyr::full_join(cor.need.depart, cor.need.arrive, by = "sector.need")

# Explore correlation structure of needs at arrival/departure
p.cor.needs <- dplyr::select(db_train, dplyr::matches("^need")) %>%
  cor() %>% as.data.frame() %>%
  dplyr::mutate(needs = rownames(.)) %>%
  tidyr::pivot_longer(-dplyr::last_col(), names_to = "needs2", values_to = "rho") %>%
  dplyr::mutate(
    dplyr::across(c(needs, needs2), ~ gsub("^(.+)(_)(.+)$", "\\3\\2\\1", .))
  ) %>%
  {
    ggplot2::ggplot(., ggplot2::aes(needs, needs2, fill = rho)) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_distiller(palette = "RdBu", direction = 1, limits = c(-1, 1)) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  }



################## REASONS OF DISPLACEMENT

# Explore push factors (departure)

summ.push <- summary(db_train[, grep("^push", colnames(db_train))])
cor.push <- lapply(
  db_train[, grep("^push", colnames(db_train))], function(x)
    cor(x, log10(db_train$displ.num), method = "spearman")
)
# Tidy cor table for report
cor.push <- dplyr::bind_rows(cor.push) %>%
  tidyr::pivot_longer(
    dplyr::everything(), names_to = c("place", "factors"), names_sep = "\\."
  ) %>%
  tidyr::pivot_wider(names_from = place, values_from = value)

db_train %>% 
  dplyr::select(displ.num, dplyr::matches("push")) %>%
  tidyr::pivot_longer(-c(1), names_to = "factor", values_to = "score") %>%
  dplyr::mutate(score = dplyr::case_when(
    score == 1.5 ~ 2, score == 0.5 ~ 0, TRUE ~ score
  )) %>%
  dplyr::filter(!is.na(score)) %>%
  {
    ggplot2::ggplot(., ggplot2::aes(as.factor(score), log10(displ.num))) +
      ggplot2::geom_boxplot() +
      ggplot2::geom_hline(
        yintercept = mean(log10(.$displ.num)), linetype = "dashed", color = "red"
      ) +
      ggplot2::facet_wrap(. ~ factor, nrow = 2)  +
      ggplot2::theme_bw()
  }

# Explore pull factors (arrival)

summ.pull <- summary(db_train[, grep("^pull", colnames(db_train))])
cor.pull <- lapply(
  db_train[, grep("^pull", colnames(db_train))], function(x)
    cor(x, log10(db_train$displ.num), method = "spearman")
)
# Tidy cor table for report
cor.pull <- dplyr::bind_rows(cor.pull) %>%
  tidyr::pivot_longer(
    dplyr::everything(), names_to = c("place", "factors"), names_sep = "\\."
  ) %>%
  tidyr::pivot_wider(names_from = place, values_from = value)

db_train %>% 
  dplyr::select(month.seq, displ.num, itinerary, dplyr::matches("pull")) %>%
  dplyr::select(displ.num, dplyr::matches("pull")) %>%
  tidyr::pivot_longer(-c(1), names_to = "factor", values_to = "score") %>%
  dplyr::mutate(score = dplyr::case_when(
    score == 1.5 ~ 2, score == 0.5 ~ 0, TRUE ~ score
  )) %>%
  dplyr::filter(!is.na(score)) %>%
  {
    ggplot2::ggplot(., ggplot2::aes(as.factor(score), log10(displ.num))) +
      ggplot2::geom_boxplot() +
      ggplot2::geom_hline(
        yintercept = mean(log10(.$displ.num)), linetype = "dashed", color = "red"
      ) +
      ggplot2::facet_wrap(. ~ factor, nrow = 2)  +
      ggplot2::theme_bw()
  }

# Explore correlation structure push/pull factors

dplyr::select(db_train, dplyr::matches("^(push|pull)")) %>%
  cor() %>% as.data.frame() %>%
  dplyr::mutate(factors = rownames(.)) %>%
  tidyr::pivot_longer(-dplyr::last_col(), names_to = "factors2", values_to = "rho") %>%
  dplyr::mutate(
    dplyr::across(c(factors, factors2), ~ gsub("^(.+)(_)(.+)$", "\\3\\2\\1", .))
  ) %>%
  {
    ggplot2::ggplot(., ggplot2::aes(factors, factors2, fill = rho)) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_distiller(palette = "RdBu", direction = 1, limits = c(-1, 1)) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  }



######################### DATA PARTITION: TRAIN FOLDS ##########################

# Create time-series CV x5 (Rolling origin)
set.seed(20200328)
time_slice <- caret::createTimeSlices(
  1:length(unique(db_train$datestamp)),
  initialWindow = 10, horizon = 2, fixedWindow = FALSE
)

# Case index of selected partition
train_i <- lapply(time_slice[[1]], function(i) which((db_train$month.seq + 1) %in% i))
test_i <- lapply(time_slice[[2]], function(i) which((db_train$month.seq + 1) %in% i))

#### Train control

# Split partition into hold=out and train
train_i <- list(train = train_i, test = test_i)

t.control <- caret::trainControl(
  number = 15, index = train_i$train, indexOut = train_i$test
)

#### Train dfs

train_set <- lapply(1:length(time_slice[[1]]), function(i) 
  db_train[(db_train$month.seq + 1) %in% time_slice[[1]][[i]], ]
)
test_set <- lapply(1:length(time_slice[[2]]), function(i) 
  db_train[(db_train$month.seq + 1) %in% time_slice[[2]][[i]], ]
)

train_set <- list(train = train_set, test = test_set)


################################## PRE-PROCESSING ##############################

#######  NEW PREDICTOR AND OUTCOME TRANSFORMATION

# Apply to db_final
db_final <- db_final %>%
  dplyr::group_by(itinerary) %>%
  dplyr::mutate(
    lg.displ.num = dplyr::if_else(
      displ.num == 0, as.numeric(displ.num), log10(displ.num) # Log outcome
    ),
    hist.itiner.avg = mean(lg.displ.num) # historic avg. by itinerart
  ) %>%
  dplyr::ungroup() %>%
  dplyr::add_count(depart.from_mun.pcode, name = "nt.depart") %>% # #times departed from loc
  dplyr::add_count(arrive.to_mun.pcode, name = "nt.arrive")# #times arrived to loc

# split train from hold-out
db_hold <- dplyr::filter(db_final, month.seq > 17)
db_train <- dplyr::filter(db_final, month.seq <= 17)

# Apply to training set
train_set <- purrr::pmap(train_set,  ~ {
  
    train <-  ..1 %>%
      dplyr::group_by(itinerary) %>%
      dplyr::mutate(
        lg.displ.num = dplyr::if_else(
          displ.num == 0, as.numeric(displ.num), log10(displ.num) 
        ),
        hist.itiner.avg = mean(lg.displ.num) 
      ) %>%
      dplyr::ungroup() %>%
      dplyr::add_count(depart.from_mun.pcode, name = "nt.depart") %>% # #times departed from loc
      dplyr::add_count(arrive.to_mun.pcode, name = "nt.arrive") # #times arrived to loc
    
    test <-  ..2 %>%
      dplyr::group_by(itinerary) %>%
      dplyr::mutate(
        lg.displ.num = dplyr::if_else(
          displ.num == 0, as.numeric(displ.num), log10(displ.num) 
        ),
        hist.itiner.avg = mean(lg.displ.num) 
      ) %>%
      dplyr::ungroup() %>%
      dplyr::add_count(depart.from_mun.pcode, name = "nt.depart") %>% # #times departed from loc
      dplyr::add_count(arrive.to_mun.pcode, name = "nt.arrive") # #times arrived to loc
    
    list(train = train, test = test)
  })
# Set train set back to original structure
train_set <- list(
  train = lapply(train_set, "[[", "train"),
  test = lapply(train_set, "[[", "test")
)


###### RATE OF DISPLACEMENT FROM DEPARTURE

# in db_final
db_final <- db_final %>%
  dplyr::select(month.seq, displ.num, total.pop_at.departure, depart.from_mun.pcode) %>%
  dplyr::group_by(depart.from_mun.pcode, month.seq) %>%
  dplyr::summarise(
    total.pop_at.departure = total.pop_at.departure[1],
    s.displ.num = sum(displ.num),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    lag_total.pop_at.depart = dplyr::lag(
      total.pop_at.departure, n = 1, order_by = month.seq, 
      default = total.pop_at.departure[1]), # t0 imputed by itself to avoid breaking asc|desc trends if imputing by center measure (e.g. median)
    rate.displ = s.displ.num / lag_total.pop_at.depart,
    rate.displ = dplyr::if_else(is.infinite(rate.displ) | rate.displ > 1, 1, rate.displ), # Clean: displ > pop (n = 40/1275)
    lg.rate.displ = log10(rate.displ),
  ) %>%
  dplyr::select(-total.pop_at.departure) %>%
  dplyr::left_join(db_final, ., by = c("month.seq", "depart.from_mun.pcode")) %>%
  dplyr::ungroup()

# split train from hold-out
db_hold <- dplyr::filter(db_final, month.seq > 17)
db_train <- dplyr::filter(db_final, month.seq <= 17)

# in training set
train_set <- purrr::pmap(train_set,  ~ {
  
  train <- ..1 %>%
    dplyr::select(month.seq, displ.num, total.pop_at.departure, depart.from_mun.pcode) %>%
    dplyr::group_by(depart.from_mun.pcode, month.seq) %>%
    dplyr::summarise(
      total.pop_at.departure = total.pop_at.departure[1],
      s.displ.num = sum(displ.num),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      lag_total.pop_at.depart = dplyr::lag(
        total.pop_at.departure, n = 1, order_by = month.seq, 
        default = total.pop_at.departure[1]), # t0 imputed by itself to avoid breaking asc|desc trends if imputing by center measure (e.g. median)
      rate.displ = s.displ.num / lag_total.pop_at.depart,
      rate.displ = dplyr::if_else(is.infinite(rate.displ) | rate.displ > 1, 1, rate.displ), # Clean: displ > pop (n = 40/1275)
      lg.rate.displ = log10(rate.displ),
    ) %>%
    dplyr::select(-total.pop_at.departure) %>%
    dplyr::left_join(..1, ., by = c("month.seq", "depart.from_mun.pcode")) %>%
    dplyr::ungroup()
  
  test <- ..2 %>%
    dplyr::select(month.seq, displ.num, total.pop_at.departure, depart.from_mun.pcode) %>%
    dplyr::group_by(depart.from_mun.pcode, month.seq) %>%
    dplyr::summarise(
      total.pop_at.departure = total.pop_at.departure[1],
      s.displ.num = sum(displ.num),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      lag_total.pop_at.depart = dplyr::lag(
        total.pop_at.departure, n = 1, order_by = month.seq, 
        default = total.pop_at.departure[1]), # t0 imputed by itself to avoid breaking asc|desc trends if imputing by center measure (e.g. median)
      rate.displ = s.displ.num / lag_total.pop_at.depart,
      rate.displ = dplyr::if_else(is.infinite(rate.displ) | rate.displ > 1, 1, rate.displ), # Clean: displ > pop (n = 40/1275)
      lg.rate.displ = log10(rate.displ),
    ) %>%
    dplyr::select(-total.pop_at.departure) %>%
    dplyr::left_join(..2, ., by = c("month.seq", "depart.from_mun.pcode")) %>%
    dplyr::ungroup()
  
  list(train = train, test = test)
})
# Set train set back to original structure
train_set <- list(
  train = lapply(train_set, "[[", "train"),
  test = lapply(train_set, "[[", "test")
)


##### RATE OF ARRIVAL TO DESTINATION

# in db_final
db_final <- db_final %>%
  dplyr::group_by(arrive.to_mun.pcode, month.seq) %>%
  dplyr::mutate(
    rate.arrive = displ.num / total.pop_at.arrival,
    rate.arrive = dplyr::if_else(is.infinite(rate.arrive) | rate.arrive > 1, 1, rate.arrive), # Clean: displ > pop (n = 40/1275)
    lg.rate.arrive = log10(rate.arrive),
  ) %>%
  dplyr::ungroup()

# split train from hold-out
db_hold <- dplyr::filter(db_final, month.seq > 17)
db_train <- dplyr::filter(db_final, month.seq <= 17)

# in training set
train_set <- purrr::pmap(train_set,  ~ {
  
  train <- ..1 %>%
    dplyr::group_by(arrive.to_mun.pcode, month.seq) %>%
    dplyr::mutate(
      rate.arrive = displ.num / total.pop_at.arrival,
      rate.arrive = dplyr::if_else(is.infinite(rate.arrive) | rate.arrive > 1, 1, rate.arrive), # Clean: displ > pop (n = 40/1275)
      lg.rate.arrive = log10(rate.arrive),
    ) %>%
    dplyr::ungroup()
  
  test <- ..2 %>%
    dplyr::group_by(arrive.to_mun.pcode, month.seq) %>%
    dplyr::mutate(
      rate.arrive = displ.num / total.pop_at.arrival,
      rate.arrive = dplyr::if_else(is.infinite(rate.arrive) | rate.arrive > 1, 1, rate.arrive), # Clean: displ > pop (n = 40/1275)
      lg.rate.arrive = log10(rate.arrive),
    ) %>%
    dplyr::ungroup()
  
  list(train = train, test = test)
})
# Set train set back to original structure
train_set <- list(
  train = lapply(train_set, "[[", "train"),
  test = lapply(train_set, "[[", "test")
)



######## FACTORIZE NEEDS

# Fucntion to explore and create factors
tune_fact <- function(data, regex, ncomp = 7, var = NULL) {
  # args: data = list containing 1st element: training df; 2nd test dfs
  # regex = string as regex expresion for selecting variables to factorize
  # ncomp = number of components to select
  # var = cut-off for cummulative variance. use either comp or var
  
  name <- gsub("[[:punct:]]", "", regex) # get name for uid PCA vars
  
  out <- purrr::pmap(data, ~ {
    # Train data: transform to matrix
    need_mtx <- ..1 %>%
      dplyr::select(dplyr::matches(regex)) %>%
      as.matrix()
    rownames(need_mtx) <- paste0(..1$itinerary, " ", ..1$month.seq)
    # PCA & select components
    pca <- prcomp(need_mtx)
    if (!is.null(var)) {
      ncomp <- min(which(as.numeric(as.data.frame(summary(pca)[6])[3,]) > var))
    }
    components <- pca$x[, 1:ncomp]
    # join components with df
    train <- as.data.frame(components) %>%
      dplyr::mutate(id = rownames(.)) %>%
      tidyr::separate(id, c("itinerary", "month.seq"), "\\s") %>%
      dplyr::mutate(month.seq = as.numeric(month.seq)) %>%
      dplyr::left_join(..1, .) %>%
      dplyr::rename_with(~ paste0(., "_", name), dplyr::matches("^PC\\d+$"))
    
    # Print PCA plots
    print(ggplot2::qplot(pca$x[,1], pca$x[,2]) + ggplot2::geom_smooth())
    print(ggplot2::qplot(pca$x[,1], ..1$lg.displ.num) + ggplot2::geom_smooth())
    print(ggplot2::qplot(pca$x[,2], ..1$lg.displ.num) + ggplot2::geom_smooth())
    
    # If test data then repeat
    if (length(data) > 1) {

      need_mtx <- ..2 %>%
        dplyr::select(dplyr::matches(regex)) %>%
        as.matrix()
      rownames(need_mtx) <- paste0(..2$itinerary, " ", ..2$month.seq)
  
      pca <- prcomp(need_mtx)
      if (!is.null(var)) {
        ncomp <- min(which(as.numeric(as.data.frame(summary(pca)[6])[3,]) > var))
      }
      components <- pca$x[, 1:ncomp]
  
      test <- as.data.frame(components) %>%
        dplyr::mutate(id = rownames(.)) %>%
        tidyr::separate(id, c("itinerary", "month.seq"), "\\s") %>%
        dplyr::mutate(month.seq = as.numeric(month.seq)) %>%
        dplyr::left_join(..2, .)  %>%
        dplyr::rename_with(~ paste0(., "_", name), dplyr::matches("^PC\\d+$"))
      
      list(train = train, test = test)
    
    } else {train}
  })
  
  if (length(data) > 1) { # Set train set (train+set) back to original structure
    return(list( #
      train = lapply(out, "[[", "train"),
      test = lapply(out, "[[", "test")
    ))
  } else {return(out[[1]])}
}

## Tune n.components at Arrival & Departure in train set
train_set <- tune_fact(train_set, "^need.*arrival$", ncomp = 4) # tested with var = 0.9, mode of 4 comp in 5 folds   
train_set <- tune_fact(train_set, "^need.*depart.+$", ncomp = 4) # tested with var = 0.9, mode of 4 comp in 5 folds   

## Create components in db.final
db_final <- tune_fact(list(list(db_final)), "^need.*arrival$", ncomp = 4) 
db_final <- tune_fact(list(list(db_final)), "^need.*depart.+$", ncomp = 4)

# split train from hold-out
db_hold <- dplyr::filter(db_final, month.seq > 17)
db_train <- dplyr::filter(db_final, month.seq <= 17)

# Plot factor interpretation (first insight)
db_final %>%
  ggplot2::ggplot(ggplot2::aes(PC1_needarrival, PC2_needarrival, color = arrive.to_pro.pcode)) +
  ggplot2::geom_point() +
  ggplot2::geom_density_2d(size = 0.01, colour = "black", linetype = "dashed", bins = 10) +
  ggplot2::theme_bw()
 
db_final %>%
  ggplot2::ggplot(ggplot2::aes(PC1_needdepart, PC2_needdepart, color = depart.from_pro.pcode)) +
  ggplot2::geom_point() +
  ggplot2::geom_density_2d(size = 0.01, colour = "black", linetype = "dashed", bins = 10) +
  ggplot2::theme_bw()

#### Component interpretation

# For arrival
need_mtx <- db_final %>%
  dplyr::filter(month.seq <= 17) %>%
  dplyr::select(dplyr::matches("^need.*arrival$")) %>%
  as.matrix()
# plot loadings
p.pca.arrival <- as.data.frame(prcomp(need_mtx)$rotation) %>%
  dplyr::mutate(sector.needs = rownames(.)) %>%
  tidyr::pivot_longer(-sector.needs, names_to = "factors", values_to = "loading") %>%
  {
    ggplot2::ggplot(., ggplot2::aes(factors, sector.needs, fill = loading)) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_distiller(palette = "RdBu", direction = 1, limits = c(-1, 1)) +
      ggplot2::theme_bw()
  }

# For departure
need_mtx <- db_final %>%
  dplyr::filter(month.seq <= 17) %>%
  dplyr::select(dplyr::matches("^need.*departure$")) %>%
  as.matrix()
# plot loadings
p.pca.depart <- as.data.frame(prcomp(need_mtx)$rotation) %>%
  dplyr::mutate(sector.needs = rownames(.)) %>%
  tidyr::pivot_longer(-sector.needs, names_to = "factors", values_to = "loading") %>%
  {
    ggplot2::ggplot(., ggplot2::aes(factors, sector.needs, fill = loading)) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_distiller(palette = "RdBu", direction = 1, limits = c(-1, 1)) +
      ggplot2::theme_bw()
  }




################################## MODELLING ###################################

####### MODEL 0: HISTORIC MEAN (baseline)

model_0 <- caret::train(
  lg.displ.num ~ hist.itiner.avg,
  data = db_train, method = "lm", trControl = t.control, 
  tuneGrid = expand.grid(intercept = 1:5)
)

model_0_best.t <- model_0$bestTune[[1]]
model_0_eval <- model_0$results[which.min(model_0$results$RMSE), ] %>%
  dplyr::mutate(model = "Historic Average") %>%
  dplyr::select(model, RMSE, Rsquared, MAE)
  

######## MODEL 1: DISPLACEMENT PREDICTORS

# Select predictors

db_model_1 <- db_train %>%
  dplyr::select(
    lg.displ.num, hist.itiner.avg, month.seq, distance, 
    depart.from_pro.pcode, nt.depart, arrive.to_pro.pcode, nt.arrive
  )

# Create output storages
model_eval_1 <- c() 
var_import_1 <- c() 

#### LASSO
set.seed(20200328)
start <- Sys.time()
print(start)
fit_glmnet_1 <- caret::train(
  lg.displ.num ~ ., data = db_model_1, method = "glmnet", trControl = t.control,
  tuneGrid = expand.grid(alpha = 1, lambda = 0)
)
end <- Sys.time()
print(end-start)
# Evaluation
model_eval_1[["glmnet"]] <- data.frame(
  model = "LASSO",
  fit_glmnet_1$results[which.min(fit_glmnet_1$results$RMSE),]
)
# Predictor importance
var_import_1[["glmnet"]] <- data.frame(
  predictor = rownames(caret::varImp(fit_glmnet_1)[[1]]),
  model = "LASSO",
  caret::varImp(fit_glmnet_1)[[1]],
  row.names = NULL
)

#### PARTIAL LEAST SQUARES
set.seed(20200328)
start <- Sys.time()
print(start)
fit_pls_1 <- caret::train(
  lg.displ.num ~ ., data = db_model_1, method = "pls", trControl = t.control,
  tuneGrid = expand.grid(ncomp = 6:24)
)
end <- Sys.time()
print(end-start)
# Evaluation
plot(fit_pls_1)
model_eval_1[["pls"]] <- data.frame(
  model = "PLS",
  fit_pls_1$results[which.min(fit_pls_1$results$RMSE),]
)
# Predictor importance
var_import_1[["pls"]] <- data.frame(
  predictor = rownames(caret::varImp(fit_pls_1)[[1]]),
  model = "PLS",
  caret::varImp(fit_pls_1)[[1]],
  row.names = NULL
)

##### RANDOM FOREST
set.seed(20200328)
start <- Sys.time()
print(start)
fit_Rborist_1 <- caret::train(
  lg.displ.num ~ ., data = db_model_1, method = "Rborist", nTree = 250,
  quantiles = TRUE, trControl = t.control,
  tuneGrid = expand.grid(predFixed = 15, minNode = 27) 
)
end <- Sys.time()
print(end-start)
# Evaluation
#plot(fit_Rborist_1)
model_eval_1[["Rborist"]] <- data.frame(
  model = "RF",
  fit_Rborist_1$results[which.min(fit_Rborist_1$results$RMSE),]
)
 
# Predictor importance
var_import_1[["Rborist"]] <- data.frame(
  predictor = rownames(caret::varImp(fit_Rborist_1)[[1]]),
  model = "RF",
  caret::varImp(fit_Rborist_1)[[1]],
  row.names = NULL
)

#### COMPILED RESULTS

# Model evaluation (train)
model_eval_1 <- dplyr::bind_rows(model_eval_1) %>%
  dplyr::select(model, RMSE, Rsquared, MAE) %>%
  dplyr::mutate(dplyr::across(-1, ~ round(., 4)))

# Predictors' importance
var_importance_1 <- dplyr::bind_rows(var_import_1) %>%
  dplyr::group_by(model) %>%
  dplyr::mutate(rank = rank(Overall)) %>%
  tidyr::pivot_wider(names_from = model, values_from = c(Overall, rank)) %>%
  dplyr::mutate(avg_rank = rowMeans(.[,grep("^rank_", colnames(.))], na.rm = TRUE)) %>%
  dplyr::select(-dplyr::matches("Overall")) %>%
  dplyr::arrange(dplyr::desc(avg_rank))

model_eval_1 
var_importance_1

## To report
best_1 <- model_eval_1[which.min(model_eval_1$RMSE), c("model", "RMSE")]

p.var_importance_1 <- var_importance_1 %>%
  tidyr::pivot_longer(-1, names_to = "method", values_to = "rank") %>%
  dplyr::filter(!grepl("^arrive|^depart", predictor), method != "avg_rank") %>%
  {
    ggplot2::ggplot(., ggplot2::aes(
      forcats::fct_reorder(predictor, rank, .desc = FALSE), rank, color = method
    )) +
      ggplot2::geom_point(size = 3.5) +
      ggplot2::geom_segment(ggplot2::aes(
        x = predictor, xend = predictor, y = rank, yend = 0), color  = "black", size = 0.3) +
      ggplot2::scale_y_continuous(breaks = seq(0, max(.$rank), 10)) +
      ggplot2::labs(
        caption = "Method's importance coefficient were converted to ranks for comparability. 
        The greater the rank, the more important the predictor. Points with same ranking overlap.
        Municipalities of departure and arrival are exlcuded form the figure."
      ) + 
      ggplot2::theme_bw() +
      ggplot2::theme(
        legend.position = "top", panel.grid.major.y = ggplot2::element_blank()
      ) +
      ggplot2::coord_flip()
  }


######## MODEL 2: DISPLACEMENT, POPULATION & BASIC NEEDS

# Select predictors

db_model_2 <- db_train %>%
  dplyr::select(
    lg.displ.num, hist.itiner.avg, month.seq, distance, 
    depart.from_pro.pcode, nt.depart, arrive.to_pro.pcode, nt.arrive,
    lg.rate.displ, total.pop_at.departure, idp.ratio_at.departure,
    lg.rate.arrive, total.pop_at.arrival, idp.ratio_at.arrival
  )

# Create output storages
model_eval_2 <- c() 
var_import_2 <- c() 

#### LASSO
set.seed(20200328)
start <- Sys.time()
print(start)
fit_glmnet_2 <- caret::train(
  lg.displ.num ~ ., data = db_model_2, method = "glmnet", trControl = t.control,
  tuneGrid = expand.grid(alpha = 1, lambda = 0)
)
end <- Sys.time()
print(end-start)
# Evaluation
model_eval_2[["glmnet"]] <- data.frame(
  model = "LASSO",
  fit_glmnet_2$results[which.min(fit_glmnet_2$results$RMSE),]
)
# Predictor importance
var_import_2[["glmnet"]] <- data.frame(
  predictor = rownames(caret::varImp(fit_glmnet_2)[[1]]),
  model = "LASSO",
  caret::varImp(fit_glmnet_2)[[1]],
  row.names = NULL
)

#### PARTIAL LEAST SQUARES
set.seed(20200328)
start <- Sys.time()
print(start)
fit_pls_2 <- caret::train(
  lg.displ.num ~ ., data = db_model_2, method = "pls", trControl = t.control,
  tuneGrid = expand.grid(ncomp = 13:20)
)
end <- Sys.time()
print(end-start)
# Evaluation
plot(fit_pls_2)
model_eval_2[["pls"]] <- data.frame(
  model = "PLS",
  fit_pls_2$results[which.min(fit_pls_2$results$RMSE),]
)
# Predictor importance
var_import_2[["pls"]] <- data.frame(
  predictor = rownames(caret::varImp(fit_pls_2)[[1]]),
  model = "PLS",
  caret::varImp(fit_pls_2)[[1]],
  row.names = NULL
)

##### RANDOM FOREST
set.seed(20200328)
start <- Sys.time()
print(start)
fit_Rborist_2 <- caret::train(
  lg.displ.num ~ ., data = db_model_2, method = "Rborist", nTree = 250,
  quantiles = TRUE, trControl = t.control,
  tuneGrid = expand.grid(predFixed = 29, minNode = 1) 
)
end <- Sys.time()
print(end-start)
# Evaluation
#plot(fit_Rborist_2)
model_eval_2[["Rborist"]] <- data.frame(
  model = "RF",
  fit_Rborist_2$results[which.min(fit_Rborist_2$results$RMSE),]
)

# Predictor importance
var_import_2[["Rborist"]] <- data.frame(
  predictor = rownames(caret::varImp(fit_Rborist_2)[[1]]),
  model = "RF",
  caret::varImp(fit_Rborist_2)[[1]],
  row.names = NULL
)

#### COMPILED RESULTS

# Model evaluation (train)
model_eval_2 <- dplyr::bind_rows(model_eval_2) %>%
  dplyr::select(model, RMSE, Rsquared, MAE) %>%
  dplyr::mutate(dplyr::across(-1, ~ round(., 4)))

# Predictors' importance
var_importance_2 <- dplyr::bind_rows(var_import_2) %>%
  dplyr::group_by(model) %>%
  dplyr::mutate(rank = rank(Overall)) %>%
  tidyr::pivot_wider(names_from = model, values_from = c(Overall, rank)) %>%
  dplyr::mutate(avg_rank = rowMeans(.[,grep("^rank_", colnames(.))], na.rm = TRUE)) %>%
  dplyr::select(-dplyr::matches("Overall")) %>%
  dplyr::arrange(dplyr::desc(avg_rank))

model_eval_2 
var_importance_2

# To report
best_2 <- model_eval_2[which.min(model_eval_2$RMSE), c("model", "RMSE")]

p.var_importance_2 <- var_importance_2 %>%
  tidyr::pivot_longer(-1, names_to = "method", values_to = "rank") %>%
  dplyr::filter(!grepl("^arrive|^depart", predictor), method != "avg_rank") %>%
  {
    ggplot2::ggplot(., ggplot2::aes(
      forcats::fct_reorder(predictor, rank, .desc = FALSE), rank, color = method)
    ) +
      ggplot2::geom_point(size = 3.5) +
      ggplot2::geom_segment(ggplot2::aes(
        x = predictor, xend = predictor, y = rank, yend = 0), color  = "black", size = 0.3) +
      ggplot2::scale_y_continuous(breaks = seq(0, max(.$rank), 10)) +
      ggplot2::labs(
        x = "predictors", 
        caption = "Method's importance coefficient were converted to ranks for comparability. 
        The greater the rank, the more important the predictor. Points with same ranking overlap. 
        Municipalities of departure and arrival are exlcuded form the figure."
      ) +      
      ggplot2::theme_bw() +
      ggplot2::theme(
        legend.position = "top", panel.grid.major.y = ggplot2::element_blank()
      ) +
      ggplot2::coord_flip()
  }



######## MODEL 3: DISPLACEMENT, POPULATION & BASIC NEEDS

# Select predictors

db_model_3 <- db_train %>%
  dplyr::select(
    lg.displ.num, hist.itiner.avg, month.seq, distance, 
    depart.from_pro.pcode, nt.depart, arrive.to_pro.pcode, nt.arrive,
    lg.rate.displ, total.pop_at.departure, idp.ratio_at.departure,
    lg.rate.arrive, total.pop_at.arrival, idp.ratio_at.arrival,
    dplyr::matches("^PC\\d")
  )

# Create output storages
model_eval_3 <- c() 
var_import_3 <- c() 

#### LASSO
set.seed(20200328)
start <- Sys.time()
print(start)
fit_glmnet_3 <- caret::train(
  lg.displ.num ~ ., data = db_model_3, method = "glmnet", trControl = t.control,
  tuneGrid = expand.grid(alpha = 1, lambda = 0)
)
end <- Sys.time()
print(end-start)
# Evaluation
model_eval_3[["glmnet"]] <- data.frame(
  model = "LASSO",
  fit_glmnet_3$results[which.min(fit_glmnet_3$results$RMSE),]
)
# Predictor importance
var_import_3[["glmnet"]] <- data.frame(
  predictor = rownames(caret::varImp(fit_glmnet_3)[[1]]),
  model = "LASSO",
  caret::varImp(fit_glmnet_3)[[1]],
  row.names = NULL
)

#### PARTIAL LEAST SQUARES
set.seed(20200328)
start <- Sys.time()
print(start)
fit_pls_3 <- caret::train(
  lg.displ.num ~ ., data = db_model_3, method = "pls", trControl = t.control,
  tuneGrid = expand.grid(ncomp = 36:42)
)
end <- Sys.time()
print(end-start)
# Evaluation
plot(fit_pls_3)
model_eval_3[["pls"]] <- data.frame(
  model = "PLS",
  fit_pls_3$results[which.min(fit_pls_3$results$RMSE),]
)
# Predictor importance
var_import_3[["pls"]] <- data.frame(
  predictor = rownames(caret::varImp(fit_pls_3)[[1]]),
  model = "PLS",
  caret::varImp(fit_pls_3)[[1]],
  row.names = NULL
)

##### RANDOM FOREST
set.seed(20200328)
start <- Sys.time()
print(start)
fit_Rborist_3 <- caret::train(
  lg.displ.num ~ ., data = db_model_3, method = "Rborist", nTree = 250,
  quantiles = TRUE, trControl = t.control,
  tuneGrid = expand.grid(predFixed = 37, minNode = 2) 
)
end <- Sys.time()
print(end-start)
# Evaluation
#plot(fit_Rborist_3)
model_eval_3[["Rborist"]] <- data.frame(
  model = "RF",
  fit_Rborist_3$results[which.min(fit_Rborist_3$results$RMSE),]
)

# Predictor importance
var_import_3[["Rborist"]] <- data.frame(
  predictor = rownames(caret::varImp(fit_Rborist_3)[[1]]),
  model = "RF",
  caret::varImp(fit_Rborist_3)[[1]],
  row.names = NULL
)

#### COMPILED RESULTS

# Model evaluation (train)
model_eval_3 <- dplyr::bind_rows(model_eval_3) %>%
  dplyr::select(model, RMSE, Rsquared, MAE) %>%
  dplyr::mutate(dplyr::across(-1, ~ round(., 4)))

# Predictors' importance
var_importance_3 <- dplyr::bind_rows(var_import_3) %>%
  dplyr::group_by(model) %>%
  dplyr::mutate(rank = rank(Overall)) %>%
  tidyr::pivot_wider(names_from = model, values_from = c(Overall, rank)) %>%
  dplyr::mutate(avg_rank = rowMeans(.[,grep("^rank_", colnames(.))], na.rm = TRUE)) %>%
  dplyr::select(-dplyr::matches("Overall")) %>%
  dplyr::arrange(dplyr::desc(avg_rank))

model_eval_3 
var_importance_3

# To report
best_3 <- model_eval_3[which.min(model_eval_3$RMSE), c("model", "RMSE")]

p.var_importance_3 <- var_importance_3 %>%
  tidyr::pivot_longer(-1, names_to = "method", values_to = "rank") %>%
  dplyr::filter(!grepl("^arrive|^depart", predictor), method != "avg_rank") %>%
  {
    ggplot2::ggplot(., ggplot2::aes(
      forcats::fct_reorder(predictor, rank, .desc = FALSE), rank, color = method
    )) +
      ggplot2::geom_point(size = 3) +
      ggplot2::geom_segment(ggplot2::aes(
        x = predictor, xend = predictor, y = rank, yend = 0), color  = "black", size = 0.3) +
      ggplot2::scale_y_continuous(breaks = seq(0, max(.$rank), 10)) +
      ggplot2::labs(
        x = "predictors", y = "importance coefficient for RF",
        caption = "The greater the value, the more important the predictor."
      ) +   
      ggplot2::labs(
        x = "predictors",
        caption = "Method's importance coefficient were converted to ranks for comparability. 
        The greater the rank, the more important the predictor. Points with same ranking overlap.
        Municipalities of departure and arrival are exlcuded form the figure."
      ) + 
      ggplot2::theme_bw() +
      ggplot2::theme(
        legend.position = "top", panel.grid.major.y = ggplot2::element_blank()
      ) +
      ggplot2::coord_flip()
  }


######## MODEL 4: DISPLACEMENT, POPULATION, BASIC NEEDS & REASONS OF DISPALCEMENT

# Select predictors

db_model_4 <- db_train %>%
  dplyr::select(
    lg.displ.num, hist.itiner.avg, month.seq, distance, 
    depart.from_pro.pcode, nt.depart, arrive.to_pro.pcode, nt.arrive,
    lg.rate.displ, total.pop_at.departure, idp.ratio_at.departure,
    lg.rate.arrive, total.pop_at.arrival, idp.ratio_at.arrival,
    dplyr::matches("^PC\\d"), dplyr::matches("^(pull|push)")
  )

# Create output storages
model_eval_4 <- c() 
var_import_4 <- c() 

#### LASSO
set.seed(20200328)
start <- Sys.time()
print(start)
fit_glmnet_4 <- caret::train(
  lg.displ.num ~ ., data = db_model_4, method = "glmnet", trControl = t.control,
  tuneGrid = expand.grid(alpha = 1, lambda = 0)
)
end <- Sys.time()
print(end-start)
# Evaluation
model_eval_4[["glmnet"]] <- data.frame(
  model = "LASSO",
  fit_glmnet_4$results[which.min(fit_glmnet_4$results$RMSE),]
)
# Predictor importance
var_import_4[["glmnet"]] <- data.frame(
  predictor = rownames(caret::varImp(fit_glmnet_4)[[1]]),
  model = "LASSO",
  caret::varImp(fit_glmnet_4)[[1]],
  row.names = NULL
)

#### PARTIAL LEAST SQUARES
set.seed(20200328)
start <- Sys.time()
print(start)
fit_pls_4 <- caret::train(
  lg.displ.num ~ ., data = db_model_4, method = "pls", trControl = t.control,
  tuneGrid = expand.grid(ncomp = 25:40)
)
end <- Sys.time()
print(end-start)
# Evaluation
plot(fit_pls_4)
model_eval_4[["pls"]] <- data.frame(
  model = "PLS",
  fit_pls_4$results[which.min(fit_pls_4$results$RMSE),]
)
# Predictor importance
var_import_4[["pls"]] <- data.frame(
  predictor = rownames(caret::varImp(fit_pls_4)[[1]]),
  model = "PLS",
  caret::varImp(fit_pls_4)[[1]],
  row.names = NULL
)

##### RANDOM FOREST
set.seed(20200328)
start <- Sys.time()
print(start)
fit_Rborist_4 <- caret::train(
  lg.displ.num ~ ., data = db_model_4, method = "Rborist", nTree = 250,
  quantiles = TRUE, trControl = t.control,
  tuneGrid = expand.grid(predFixed = 50, minNode = 2) 
)
end <- Sys.time()
print(end-start)
# Evaluation
#plot(fit_Rborist_4)
model_eval_4[["Rborist"]] <- data.frame(
  model = "RF",
  fit_Rborist_4$results[which.min(fit_Rborist_4$results$RMSE),]
)

# Predictor importance
var_import_4[["Rborist"]] <- data.frame(
  predictor = rownames(caret::varImp(fit_Rborist_4)[[1]]),
  model = "RF",
  caret::varImp(fit_Rborist_4)[[1]],
  row.names = NULL
)

#### COMPILED RESULTS

# Model evaluation (train)
model_eval_4 <- dplyr::bind_rows(model_eval_4) %>%
  dplyr::select(model, RMSE, Rsquared, MAE) %>%
  dplyr::mutate(dplyr::across(-1, ~ round(., 4)))

# Predictors' importance
var_importance_4 <- dplyr::bind_rows(var_import_4) %>%
  dplyr::group_by(model) %>%
  dplyr::mutate(rank = rank(Overall)) %>%
  tidyr::pivot_wider(names_from = model, values_from = c(Overall, rank)) %>%
  dplyr::mutate(avg_rank = rowMeans(.[,grep("^rank_", colnames(.))], na.rm = TRUE)) %>%
  dplyr::select(-dplyr::matches("Overall")) %>%
  dplyr::arrange(dplyr::desc(avg_rank))

model_eval_4 
var_importance_4

# To report
best_4 <- model_eval_4[which.min(model_eval_4$RMSE), c("model", "RMSE")]

p.var_importance_4 <- var_importance_4 %>%
  tidyr::pivot_longer(-1, names_to = "method", values_to = "rank") %>%
  dplyr::filter(!grepl("^arrive|^depart", predictor), method == "rank_RF") %>%
  {
    ggplot2::ggplot(., ggplot2::aes(
      forcats::fct_reorder(predictor, rank, .desc = FALSE), rank
    )) +
      ggplot2::geom_point(size = 2) +
      ggplot2::geom_segment(ggplot2::aes(
        x = predictor, xend = predictor, y = rank, yend = 0), size = 0.5) +
      ggplot2::scale_y_continuous(breaks = seq(0, max(.$rank), 10)) +
      ggplot2::labs(
        x = "predictors", 
        caption = "Only the ranking for the best performing method is presented. 
        Municipalities of departure and arrival are exlcuded form the figure."
      ) +      
      ggplot2::theme_bw() +
      ggplot2::theme(
        legend.position = "top",
        panel.grid.major.y = ggplot2::element_blank()
      ) +
       ggplot2::coord_flip()
  }


######## SELECTED MODEL: DISPLACEMENT, POPULATION & BASIC NEEDS (FINAL)

# Hyndman proposed test for usage of tradition CV on time series
ljung.test <- Box.test(as.numeric(residuals(fit_Rborist_2)), type = "Ljung-Box")

# Regression plot of prediction vs observed
y_test <- predict(fit_Rborist_2, db_hold)

p.test_final <- ggplot2::qplot(db_hold$lg.displ.num, y_test) + ggplot2::geom_smooth() +
  ggplot2::labs(x = "observed displacement (hold-out)", y = "predicted displacement (model 4)") +
  ggplot2::theme_bw()

# Table of final model evaluation paramaters
model_eval_final <- data.frame(
  model = "Model 4 - RF",
  RMSE = caret::RMSE(db_hold$lg.displ.num, y_test),
  Rsquared = caret::R2(db_hold$lg.displ.num, y_test),
  MAE = caret::MAE(db_hold$lg.displ.num, y_test)
)

# Variance importance of the final model
p.var_importance_final <- var_import_2[["Rborist"]] %>%
  dplyr::filter(!grepl("^arrive|^depart", predictor)) %>%
  {
    ggplot2::ggplot(., ggplot2::aes(forcats::fct_reorder(predictor, Overall, .desc = FALSE), Overall)) +
      ggplot2::geom_point(size = 2) +
      ggplot2::geom_segment(ggplot2::aes(
        x = predictor, xend = predictor, y = Overall, yend = 0), size = 0.5) +
      ggplot2::scale_y_continuous(breaks = seq(0, max(.$Overall), 10)) +
      ggplot2::labs(
        x = "predictors", y = "importance coefficient for RF",
        caption = "The greater the value, the more important the predictor.
        Municipalities of departure and arrival are exlcuded form the figure."
      ) +      
      ggplot2::theme_bw() +
      ggplot2::theme(panel.grid.major.y = ggplot2::element_blank()) +
       ggplot2::coord_flip()
  }

# report to annex comple list of importance
p.var_importance_final_full <- var_import_2[["Rborist"]] %>%
  {
    ggplot2::ggplot(., ggplot2::aes(forcats::fct_reorder(predictor, Overall, .desc = FALSE), Overall)) +
      ggplot2::geom_point(size = 2) +
      ggplot2::geom_segment(ggplot2::aes(
        x = predictor, xend = predictor, y = Overall, yend = 0), size = 0.5) +
      ggplot2::scale_y_continuous(breaks = seq(0, max(.$Overall), 10)) +
      ggplot2::labs(
        x = "predictors", y = "importance coefficient for RF",
        caption = "The greater the value, the more important the predictor."
      ) +      
      ggplot2::theme_bw() +
      ggplot2::theme(panel.grid.major.y = ggplot2::element_blank()) +
      ggplot2::coord_flip()
  }


# Plot of time series by province of arrival

y_final <- predict(fit_Rborist_2, db_final)

p.trend_final <- data.frame(
  month.seq = db_final$month.seq,
  province = db_final$arrive.to_pro.pcode,
  observed = db_final$lg.displ.num,
  predicted = y_final
) %>%
  tidyr::pivot_longer(-c(1:2), names_to = "value", values_to = "idps") %>%
  dplyr::group_by(month.seq, province, value) %>%
  dplyr::summarise(avg.lg.displ = mean(idps)) %>%
  {
    ggplot2::ggplot(., ggplot2::aes(month.seq, avg.lg.displ, color = value)) +
      ggplot2::geom_line(size = 0.1) +
      ggplot2::geom_vline(xintercept = 17, linetype = "dashed", size = 0.3) +
      ggplot2::labs(
        x = "month sequence", y = "IDP arrivals", 
        caption = "Dashed vertical line mark the hold-out months"
      ) +
      ggplot2::facet_wrap(. ~ province) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        legend.position = "top",
        strip.text.x = ggplot2::element_text(size = 5)
      )
  }

p.trend_final_antilog <- data.frame(
  month.seq = db_final$month.seq,
  province = db_final$depart.from_pro.pcode,
  observed = db_final$lg.displ.num,
  predicted = y_final
) %>%
  tidyr::pivot_longer(-c(1:2), names_to = "value", values_to = "idps") %>%
  dplyr::group_by(month.seq, province, value) %>%
  dplyr::summarise(avg.displ = mean(10^idps)) %>%
  {
    ggplot2::ggplot(., ggplot2::aes(month.seq, avg.displ, color = value)) +
      ggplot2::geom_line(size = 0.3) +
      ggplot2::geom_vline(xintercept = 17, linetype = "dashed", size = 0.3) +
      ggplot2::labs(
        x = "month sequence", y = "IDP arrivals", 
        caption = "Dashed vertical line mark the hold-out months"
      ) +
      ggplot2::facet_wrap(. ~ province) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        legend.position = "top",
        strip.text.x = ggplot2::element_text(size = 5)
      )
  }


############################## RENDER REPORT ###################################

rmarkdown::render("PH125.9x_JIO/Report.Rmd")


end.all <- Sys.time()
print(end.all - start.all)
