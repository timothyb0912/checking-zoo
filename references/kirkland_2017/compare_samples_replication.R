# Replication Archive for: 
# Kirkland, Patricia A. and Alexander Coppock
# Candidate Choice without Party Labels: New Insights from Conjoint Survey Experiments
# Forthcoming at Political Behavior
# Comparing YouGov and MTurk Results

rm(list = ls())

# uncomment to set working directory
# setwd("")

# Source helper functions
source("mayors_source.R")

# uncomment to install packages
# install.packages("tidyverse")

library(tidyverse)

# Load data
load("mturk_replication.rdata")
load("yougov_replication.rdata")

# Figure B.1 Effects Scatterplot -----------------------------------------------------

fit_1 <-
  lm(win ~ Gender + Age + Race + Job + Political,
     data = filter(mturk_clean, Party == "non-partisan"))
fit_2 <-
  lm(
    win ~ Gender + Age + Race + Job + Political + Party,
    data = filter(mturk_clean, Party != "non-partisan")
  )
fit_3 <-
  lm(win ~ Gender + Age + Race + Job + Political,
     data = filter(yougov_clean, Party == "non-partisan"))
fit_4 <-
  lm(
    win ~ Gender + Age + Race + Job + Political + Party,
    data = filter(yougov_clean, Party != "non-partisan")
  )

fit_1_cl <- cl(
  dat = filter(mturk_clean, Party == "non-partisan"),
  cluster = filter(mturk_clean, Party == "non-partisan")$resp_mturkid,
  fm = fit_1
)
fit_2_cl <- cl(
  dat = filter(mturk_clean, Party != "non-partisan"),
  cluster = filter(mturk_clean, Party != "non-partisan")$resp_mturkid,
  fm = fit_2
)
fit_3_cl <- cl(
  dat = filter(yougov_clean, Party == "non-partisan"),
  cluster = filter(yougov_clean, Party == "non-partisan")$caseid,
  fm = fit_3
)
fit_4_cl <- cl(
  dat = filter(yougov_clean, Party != "non-partisan"),
  cluster = filter(yougov_clean, Party != "non-partisan")$caseid,
  fm = fit_4
)

fit_1_df <-
  data.frame(fit_1_cl[, ]) %>%
  rownames_to_column() %>%
  filter(rowname != "(Intercept)") %>%
  transmute(
    attribute = rowname,
    mt_estimate = Estimate,
    mt_uis = Estimate + 1.96 * Std..Error,
    mt_lis = Estimate - 1.96 * Std..Error,
    election_type = "nonpartisan"
  )

fit_2_df <-
  data.frame(fit_2_cl[, ]) %>%
  rownames_to_column() %>%
  filter(rowname !=
           "(Intercept)") %>%
  transmute(
    attribute = rowname,
    mt_estimate = Estimate,
    mt_uis = Estimate + 1.96 * Std..Error,
    mt_lis = Estimate - 1.96 * Std..Error,
    election_type = "partisan"
  )

fit_3_df <-
  data.frame(fit_3_cl[, ]) %>%
  rownames_to_column() %>%
  filter(rowname !=
           "(Intercept)") %>%
  transmute(
    attribute = rowname,
    yg_estimate = Estimate,
    yg_uis = Estimate + 1.96 * Std..Error,
    yg_lis = Estimate - 1.96 * Std..Error,
    election_type = "nonpartisan"
  )

fit_4_df <-
  data.frame(fit_4_cl[, ]) %>%
  rownames_to_column() %>%
  filter(rowname !=
           "(Intercept)") %>%
  transmute(
    attribute = rowname,
    yg_estimate = Estimate,
    yg_uis = Estimate + 1.96 * Std..Error,
    yg_lis = Estimate - 1.96 * Std..Error,
    election_type = "partisan"
  )

all_df <-
  left_join(bind_rows(fit_1_df, fit_2_df), bind_rows(fit_3_df, fit_4_df))

cor_pearson <-
  with(all_df, cor(mt_estimate, yg_estimate, method = "pearson"))
cor_spearman <-
  with(all_df, cor(mt_estimate, yg_estimate, method = "spearman"))

g <-
  ggplot(all_df,
         aes(x = mt_estimate, y = yg_estimate, group = election_type)) +
  geom_rect(
    xmin = .09,
    xmax = .36,
    ymin = .05,
    ymax = .32,
    fill = "lightgrey",
    alpha = .02,
    linetype = "blank"
  ) +
  annotate(
    "text",
    x = 0.22,
    y = .35,
    label = "Political Experience Coefficients",
    size = 4
  ) +
  geom_point(aes(shape = election_type), size = 3) +
  scale_shape_manual(values = c(1, 19),
                     guide = guide_legend(title = "Election Type")) +
  geom_segment(aes(
    x = mt_estimate,
    y = yg_lis,
    xend = mt_estimate,
    yend = yg_uis
  ),
  alpha = .5) +
  geom_segment(aes(
    y = yg_estimate,
    x = mt_lis,
    yend = yg_estimate,
    xend = mt_uis
  ),
  alpha = .5) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             alpha = 0.5) +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             alpha = 0.5) +
  xlim(-.40, .40) + ylim(-.40, .40) +
  theme_bw() +
  #geom_abline(intercept=0, slope =1) +
  stat_smooth(
    method = 'lm',
    se = FALSE,
    fullrange = TRUE,
    aes(linetype = election_type),
    color = "black",
    alpha = .5
  ) +
  scale_linetype_manual(values = c("dotted", "solid"),
                        guide = guide_legend(title = "Election Type")) +
  theme(legend.position = "bottom",
        legend.key.width = unit(6, "lines")) +
  xlab("Mechanical Turk Estimate") +
  ylab("YouGov Estimate")

