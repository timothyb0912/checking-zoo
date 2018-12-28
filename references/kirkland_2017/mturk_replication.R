# Replication Archive for: 
# Kirkland, Patricia A. and Alexander Coppock
# Candidate Choice without Party Labels: New Insights from Conjoint Survey Experiments
# Forthcoming at Political Behavior
# MTurk Replication Code

rm(list = ls())

# uncomment to set working directory
# setwd("")

# Source helper functions
source("mayors_source.R")

# uncomment to install packages
# install.packages("tidyverse")
# it appears that we need version > 1.2.4.9000 of coefplot; obtain from github
# install.packages("devtools")
# devtools::install_github("jaredlander/coefplot")

library(tidyverse)
library(coefplot)

# Load data and some variable definitions
load("mturk_replication.rdata")
load("base_categories.rdata")

# Figure 2 Candidate pref ------------------------------------------------------------------------------

fit_1 <- lm(win ~ Gender + Age + Race + Job + Political,
            data = filter(mturk_clean, Party == "non-partisan"))

fit_2 <- lm(
  win ~ Gender + Age + Race + Job + Political + Party,
  data = filter(mturk_clean, Party != "non-partisan")
)

fit_3 <-
  lm(
    win ~ (Gender + Age + Race + Job + Political) * (Party == "non-partisan") +
      (Party == "Republican") + (Party == "Democrat"),
    data = mturk_clean
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
fit_3_cl <- cl(dat = mturk_clean,
               cluster = mturk_clean$resp_mturkid,
               fm = fit_3)


fit_1_df <-
  data.frame(fit_1_cl[,], group = "Nonpartisan") %>% rownames_to_column() %>%
  filter(rowname != "(Intercept)")
fit_2_df <-
  data.frame(fit_2_cl[,], group = "Partisan") %>% rownames_to_column() %>%
  filter(rowname != "(Intercept)")
fit_3_df <-
  data.frame(fit_3_cl[,], group = "Difference") %>% rownames_to_column() %>%
  filter(rowname != "(Intercept)")
fit_3_df <- fit_3_df[19:33,]
fit_3_df$rowname <- fit_1_df$rowname

all_df <-
  rbind(fit_1_df, fit_2_df, fit_3_df) %>%
  mutate(
    lis = Estimate - 1.96 * Std..Error,
    uis = Estimate + 1.96 * Std..Error,
    attribute = sub(
      pattern = "Gender|Age|Race|Job|Political|Party",
      replacement = "",
      x = rowname
    )
  ) %>%
  bind_rows(type_base_categores_df) %>%
  make_attributes() %>%
  make_coef_group()


table_d3 <-
  all_df %>%
  filter(!is.na(Estimate)) %>%
  mutate(entry = gen_entry_vec(est = Estimate, se = Std..Error, p = Pr...t..))  %>%
  select(coef_group, attribute, group, entry) %>%
  spread(key = group, value = entry) %>%
  select(-coef_group)


g2 <-
  ggplot(all_df, aes(x = Estimate, y = attribute)) +
  geom_point() +
  geom_segment(aes(yend = attribute, x = lis, xend = uis)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_grid(coef_group ~ group, scales = "free_y") +
  coord_cartesian(xlim = c(-.4, .4)) +
  scale_x_continuous(breaks = round(seq(-.3, .3, .1), 1)) +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    strip.background = element_blank()
  )



# Figure 4 Heterogeneous effects - candidate pref ------------------------------------------------------

fit_1 <-
  lm(
    win ~ Gender + Age + Race + Job + Political,
    data = filter(mturk_clean, Party == "non-partisan", democrat == 1)
  )
fit_2 <-
  lm(
    win ~ Gender + Age + Race + Job + Political + Party,
    filter(mturk_clean, Party != "non-partisan", democrat == 1)
  )
fit_3 <-
  lm(
    win ~ (Gender + Age + Race + Job + Political) * (Party == "non-partisan") +
      (Party == "Republican") + (Party == "Democrat") ,
    filter(mturk_clean, democrat == 1)
  )

fit_4 <-
  lm(
    win ~ Gender + Age + Race + Job + Political,
    data = filter(mturk_clean, Party == "non-partisan", republican == 1)
  )
fit_5 <-
  lm(
    win ~ Gender + Age + Race + Job + Political + Party,
    data = filter(mturk_clean, Party != "non-partisan", republican == 1)
  )
fit_6 <-
  lm(
    win ~ (Gender + Age + Race + Job + Political) * (Party == "non-partisan") +
      (Party == "Republican") + (Party == "Democrat") ,
    data = filter(mturk_clean, republican == 1)
  )


fit_1_cl <-
  cl(
    dat = filter(mturk_clean, Party == "non-partisan", democrat == 1),
    cluster = filter(mturk_clean, Party == "non-partisan", democrat ==
                       1)$resp_mturkid,
    fm = fit_1
  )
fit_2_cl <-
  cl(
    dat = filter(mturk_clean, Party != "non-partisan", democrat == 1),
    cluster = filter(mturk_clean, Party != "non-partisan", democrat ==
                       1)$resp_mturkid,
    fm = fit_2
  )
fit_3_cl <- cl(
  dat = filter(mturk_clean, democrat == 1),
  cluster = filter(mturk_clean, democrat == 1)$resp_mturkid,
  fm = fit_3
)

fit_4_cl <-
  cl(
    dat = filter(mturk_clean, Party == "non-partisan", republican == 1),
    cluster = filter(mturk_clean, Party == "non-partisan", republican ==
                       1)$resp_mturkid,
    fm = fit_4
  )
fit_5_cl <-
  cl(
    dat = filter(mturk_clean, Party != "non-partisan", republican == 1),
    cluster = filter(mturk_clean, Party != "non-partisan", republican ==
                       1)$resp_mturkid,
    fm = fit_5
  )
fit_6_cl <- cl(
  dat = filter(mturk_clean, republican == 1),
  cluster = filter(mturk_clean, republican == 1)$resp_mturkid,
  fm = fit_6
)


fit_1_df <-
  data.frame(fit_1_cl[,], group = "Nonpartisan") %>% rownames_to_column() %>%
  filter(rowname != "(Intercept)")
fit_2_df <-
  data.frame(fit_2_cl[,], group = "Partisan") %>% rownames_to_column() %>%
  filter(rowname != "(Intercept)")
fit_3_df <-
  data.frame(fit_3_cl[,], group = "Difference") %>% rownames_to_column() %>%
  filter(rowname != "(Intercept)")
fit_3_df <- fit_3_df[19:33,]
fit_3_df$rowname <- fit_1_df$rowname
fit_1_df$party <- "Democrats"
fit_2_df$party <- "Democrats"
fit_3_df$party <- "Democrats"

fit_4_df <-
  data.frame(fit_4_cl[,], group = "Nonpartisan") %>% rownames_to_column() %>%
  filter(rowname != "(Intercept)")
fit_5_df <-
  data.frame(fit_5_cl[,], group = "Partisan") %>% rownames_to_column() %>%
  filter(rowname != "(Intercept)")
fit_6_df <-
  data.frame(fit_6_cl[,], group = "Difference") %>% rownames_to_column() %>%
  filter(rowname != "(Intercept)")
fit_6_df <- fit_6_df[19:33,]
fit_6_df$rowname <- fit_1_df$rowname
fit_4_df$party <- "Republicans"
fit_5_df$party <- "Republicans"
fit_6_df$party <- "Republicans"




all_df <-
  bind_rows(fit_1_df, fit_2_df, fit_3_df, fit_4_df, fit_5_df, fit_6_df) %>%
  mutate(
    lis = Estimate - 1.96 * Std..Error,
    uis = Estimate + 1.96 * Std..Error,
    attribute = sub(
      pattern = "Gender|Age|Race|Job|Political|Party",
      replacement = "",
      x = rowname
    )
  ) %>%
  bind_rows(type_base_categores_df) %>%
  make_attributes() %>%
  make_coef_group() %>%
  mutate(group = factor(group, levels = c("Nonpartisan", "Partisan", "Difference")))

table_d5_dem <-
  all_df %>%
  filter(!is.na(Estimate), party == "Democrats") %>%
  mutate(entry = gen_entry_vec(est = Estimate, se = Std..Error, p = Pr...t..))  %>%
  select(coef_group, attribute, group, entry, party) %>%
  spread(key = group, value = entry) %>%
  select(-coef_group, -party)

table_d5_rep <-
  all_df %>%
  filter(!is.na(Estimate), party == "Republicans") %>%
  mutate(entry = gen_entry_vec(est = Estimate, se = Std..Error, p = Pr...t..))  %>%
  select(coef_group, attribute, group, entry) %>%
  spread(key = group, value = entry) %>%
  select(-coef_group)

g4 <-
  ggplot(all_df,
         aes(
           x = Estimate,
           y = attribute,
           group = party,
           color = party,
           shape = party
         )) +
  scale_color_manual(values = c("blue", "red")) +
  geom_point(position = position_dodgev(height = 0.5), size = 2) +
  geom_errorbarh(aes(xmin = lis, xmax = uis),
                 position = position_dodgev(height = 0.5),
                 height = 0) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_grid(coef_group ~ group, scales = "free_y") +
  coord_cartesian(xlim = c(-.4, .4)) +
  scale_x_continuous(breaks = round(seq(-.3, .3, .1), 1)) +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    strip.background = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank()
  )


# Figure 6 Competence heterogeneous effects ----------------------------------------------------------

fit_1 <-
  lm(
    comp ~ Gender + Age + Race + Job + Political,
    data = filter(mturk_clean, Party == "non-partisan", democrat == 1)
  )
fit_2 <-
  lm(
    comp ~ Gender + Age + Race + Job + Political + Party,
    filter(mturk_clean, Party != "non-partisan", democrat == 1)
  )
fit_3 <-
  lm(
    comp ~ (Gender + Age + Race + Job + Political) * (Party == "non-partisan") +
      (Party == "Republican") + (Party == "Democrat") ,
    filter(mturk_clean, democrat == 1)
  )

fit_4 <-
  lm(
    comp ~ Gender + Age + Race + Job + Political,
    data = filter(mturk_clean, Party == "non-partisan", republican == 1)
  )
fit_5 <-
  lm(
    comp ~ Gender + Age + Race + Job + Political + Party,
    data = filter(mturk_clean, Party != "non-partisan", republican == 1)
  )
fit_6 <-
  lm(
    comp ~ (Gender + Age + Race + Job + Political) * (Party == "non-partisan") +
      (Party == "Republican") + (Party == "Democrat") ,
    data = filter(mturk_clean, republican == 1)
  )


fit_1_cl <-
  cl(
    dat = filter(mturk_clean, Party == "non-partisan", democrat == 1),
    cluster = filter(mturk_clean, Party == "non-partisan", democrat ==
                       1)$resp_mturkid,
    fm = fit_1
  )
fit_2_cl <-
  cl(
    dat = filter(mturk_clean, Party != "non-partisan", democrat == 1),
    cluster = filter(mturk_clean, Party != "non-partisan", democrat ==
                       1)$resp_mturkid,
    fm = fit_2
  )
fit_3_cl <- cl(
  dat = filter(mturk_clean, democrat == 1),
  cluster = filter(mturk_clean, democrat == 1)$resp_mturkid,
  fm = fit_3
)

fit_4_cl <-
  cl(
    dat = filter(mturk_clean, Party == "non-partisan", republican == 1),
    cluster = filter(mturk_clean, Party == "non-partisan", republican ==
                       1)$resp_mturkid,
    fm = fit_4
  )
fit_5_cl <-
  cl(
    dat = filter(mturk_clean, Party != "non-partisan", republican == 1),
    cluster = filter(mturk_clean, Party != "non-partisan", republican ==
                       1)$resp_mturkid,
    fm = fit_5
  )
fit_6_cl <- cl(
  dat = filter(mturk_clean, republican == 1),
  cluster = filter(mturk_clean, republican == 1)$resp_mturkid,
  fm = fit_6
)


fit_1_df <-
  data.frame(fit_1_cl[,], group = "Nonpartisan") %>% rownames_to_column() %>%
  filter(rowname != "(Intercept)")
fit_2_df <-
  data.frame(fit_2_cl[,], group = "Partisan") %>% rownames_to_column() %>%
  filter(rowname != "(Intercept)")
fit_3_df <-
  data.frame(fit_3_cl[,], group = "Difference") %>% rownames_to_column() %>%
  filter(rowname != "(Intercept)")
fit_3_df <- fit_3_df[19:33,]
fit_3_df$rowname <- fit_1_df$rowname
fit_1_df$party <- "Democrats"
fit_2_df$party <- "Democrats"
fit_3_df$party <- "Democrats"

fit_4_df <-
  data.frame(fit_4_cl[,], group = "Nonpartisan") %>% rownames_to_column() %>%
  filter(rowname != "(Intercept)")
fit_5_df <-
  data.frame(fit_5_cl[,], group = "Partisan") %>% rownames_to_column() %>%
  filter(rowname != "(Intercept)")
fit_6_df <-
  data.frame(fit_6_cl[,], group = "Difference") %>% rownames_to_column() %>%
  filter(rowname != "(Intercept)")
fit_6_df <- fit_6_df[19:33,]
fit_6_df$rowname <- fit_1_df$rowname
fit_4_df$party <- "Republicans"
fit_5_df$party <- "Republicans"
fit_6_df$party <- "Republicans"



all_df <-
  bind_rows(fit_1_df, fit_2_df, fit_3_df, fit_4_df, fit_5_df, fit_6_df) %>%
  mutate(
    lis = Estimate - 1.96 * Std..Error,
    uis = Estimate + 1.96 * Std..Error,
    attribute = sub(
      pattern = "Gender|Age|Race|Job|Political|Party",
      replacement = "",
      x = rowname
    )
  ) %>%
  bind_rows(type_base_categores_df) %>%
  make_attributes() %>%
  make_coef_group() %>%
  mutate(group = factor(group, levels = c("Nonpartisan", "Partisan", "Difference")))

table_d7_dem <-
  all_df %>%
  filter(!is.na(Estimate), party == "Democrats") %>%
  mutate(entry = gen_entry_vec(est = Estimate, se = Std..Error, p = Pr...t..))  %>%
  select(coef_group, attribute, group, entry, party) %>%
  spread(key = group, value = entry) %>%
  select(-coef_group, -party)

table_d7_rep <-
  all_df %>%
  filter(!is.na(Estimate), party == "Republicans") %>%
  mutate(entry = gen_entry_vec(est = Estimate, se = Std..Error, p = Pr...t..))  %>%
  select(coef_group, attribute, group, entry) %>%
  spread(key = group, value = entry) %>%
  select(-coef_group)

g6 <-
  ggplot(all_df,
         aes(
           x = Estimate,
           y = attribute,
           group = party,
           color = party,
           shape = party
         )) +
  scale_color_manual(values = c("blue", "red")) +
  geom_point(position = position_dodgev(height = 0.5), size = 2) +
  geom_errorbarh(aes(xmin = lis, xmax = uis),
                 position = position_dodgev(height = 0.5),
                 height = 0) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_grid(coef_group ~ group, scales = "free_y") +
  coord_cartesian(xlim = c(-15, 15)) +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    strip.background = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank()
  )




# Figure C.2 Competence ----------------------------------------------------------------

fit_1 <-
  lm(comp ~ Gender + Age + Race + Job + Political,
     data = filter(mturk_clean, Party == "non-partisan"))
fit_2 <-
  lm(
    comp ~ Gender + Age + Race + Job + Political + Party,
    data = filter(mturk_clean, Party != "non-partisan")
  )
fit_3 <-
  lm(
    comp ~ (Gender + Age + Race + Job + Political) * (Party == "non-partisan") +
      (Party == "Republican") + (Party == "Democrat") ,
    data = mturk_clean
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
fit_3_cl <- cl(dat = mturk_clean,
               cluster = mturk_clean$resp_mturkid,
               fm = fit_3)

fit_1_df <-
  data.frame(fit_1_cl[,], group = "Nonpartisan") %>% rownames_to_column() %>%
  filter(rowname != "(Intercept)")
fit_2_df <-
  data.frame(fit_2_cl[,], group = "Partisan") %>% rownames_to_column() %>%
  filter(rowname != "(Intercept)")
fit_3_df <-
  data.frame(fit_3_cl[,], group = "Difference") %>% rownames_to_column() %>%
  filter(rowname != "(Intercept)")
fit_3_df <- fit_3_df[19:33,]
fit_3_df$rowname <- fit_1_df$rowname

all_df <- rbind(fit_1_df, fit_2_df, fit_3_df) %>%
  mutate(
    lis = Estimate - 1.96 * Std..Error,
    uis = Estimate + 1.96 * Std..Error,
    attribute = sub(
      pattern = "Gender|Age|Race|Job|Political|Party",
      replacement = "",
      x = rowname
    )
  ) %>%
  bind_rows(type_base_categores_df) %>%
  make_attributes() %>%
  make_coef_group()


gc2 <-
  ggplot(all_df, aes(x = Estimate, y = attribute)) +
  geom_point() +
  geom_segment(aes(yend = attribute, x = lis, xend = uis)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_grid(coef_group ~ group, scales = "free_y") +
  coord_cartesian(xlim = c(-15, 15)) +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    strip.background = element_blank()
  )




# Figure C.4 Heterogeneous effects - valence issues -------------------------------------------------------------------------------


fit_1 <-
  lm(
    valence_index ~ Gender + Age + Race + Job + Political,
    data = filter(mturk_clean, Party == "non-partisan", democrat == 1)
  )
fit_2 <-
  lm(
    valence_index ~ Gender + Age + Race + Job + Political + Party,
    filter(mturk_clean, Party != "non-partisan", democrat == 1)
  )
fit_3 <-
  lm(
    valence_index ~ (Gender + Age + Race + Job + Political) * (Party == "non-partisan") +
      (Party == "Republican") + (Party == "Democrat") ,
    filter(mturk_clean, democrat == 1)
  )

fit_4 <-
  lm(
    valence_index ~ Gender + Age + Race + Job + Political,
    data = filter(mturk_clean, Party == "non-partisan", republican == 1)
  )
fit_5 <-
  lm(
    valence_index ~ Gender + Age + Race + Job + Political + Party,
    data = filter(mturk_clean, Party != "non-partisan", republican == 1)
  )
fit_6 <-
  lm(
    valence_index ~ (Gender + Age + Race + Job + Political) * (Party == "non-partisan") +
      (Party == "Republican") + (Party == "Democrat") ,
    data = filter(mturk_clean, republican == 1)
  )


fit_1_cl <-
  cl(
    dat = filter(
      mturk_clean,
      Party == "non-partisan",
      democrat == 1,!is.na(valence_index)
    ),
    cluster = filter(
      mturk_clean,
      Party == "non-partisan",
      democrat == 1,!is.na(valence_index)
    )$resp_mturkid,
    fm = fit_1
  )
fit_2_cl <-
  cl(
    dat = filter(
      mturk_clean,
      Party != "non-partisan",
      democrat == 1,!is.na(valence_index)
    ),
    cluster = filter(
      mturk_clean,
      Party != "non-partisan",
      democrat == 1,!is.na(valence_index)
    )$resp_mturkid,
    fm = fit_2
  )
fit_3_cl <-
  cl(
    dat = filter(mturk_clean, democrat == 1, !is.na(valence_index)),
    cluster = filter(mturk_clean, democrat == 1, !is.na(valence_index))$resp_mturkid,
    fm = fit_3
  )

fit_4_cl <-
  cl(
    dat = filter(
      mturk_clean,
      Party == "non-partisan",
      republican == 1,!is.na(valence_index)
    ),
    cluster = filter(
      mturk_clean,
      Party == "non-partisan",
      republican == 1,!is.na(valence_index)
    )$resp_mturkid,
    fm = fit_4
  )
fit_5_cl <-
  cl(
    dat = filter(
      mturk_clean,
      Party != "non-partisan",
      republican == 1,!is.na(valence_index)
    ),
    cluster = filter(
      mturk_clean,
      Party != "non-partisan",
      republican == 1,!is.na(valence_index)
    )$resp_mturkid,
    fm = fit_5
  )
fit_6_cl <-
  cl(
    dat = filter(mturk_clean, republican == 1, !is.na(valence_index)),
    cluster = filter(mturk_clean, republican == 1, !is.na(valence_index))$resp_mturkid,
    fm = fit_6
  )


fit_1_df <-
  data.frame(fit_1_cl[,], group = "Nonpartisan") %>% rownames_to_column() %>%
  filter(rowname != "(Intercept)")
fit_2_df <-
  data.frame(fit_2_cl[,], group = "Partisan") %>% rownames_to_column() %>%
  filter(rowname != "(Intercept)")
fit_3_df <-
  data.frame(fit_3_cl[,], group = "Difference") %>% rownames_to_column() %>%
  filter(rowname != "(Intercept)")
fit_3_df <- fit_3_df[19:33,]
fit_3_df$rowname <- fit_1_df$rowname
fit_1_df$party <- "Democrats"
fit_2_df$party <- "Democrats"
fit_3_df$party <- "Democrats"

fit_4_df <-
  data.frame(fit_4_cl[,], group = "Nonpartisan") %>% rownames_to_column() %>%
  filter(rowname != "(Intercept)")
fit_5_df <-
  data.frame(fit_5_cl[,], group = "Partisan") %>% rownames_to_column() %>%
  filter(rowname != "(Intercept)")
fit_6_df <-
  data.frame(fit_6_cl[,], group = "Difference") %>% rownames_to_column() %>%
  filter(rowname != "(Intercept)")
fit_6_df <- fit_6_df[19:33,]
fit_6_df$rowname <- fit_1_df$rowname
fit_4_df$party <- "Republicans"
fit_5_df$party <- "Republicans"
fit_6_df$party <- "Republicans"




all_df <-
  bind_rows(fit_1_df, fit_2_df, fit_3_df, fit_4_df, fit_5_df, fit_6_df) %>%
  mutate(
    lis = Estimate - 1.96 * Std..Error,
    uis = Estimate + 1.96 * Std..Error,
    attribute = sub(
      pattern = "Gender|Age|Race|Job|Political|Party",
      replacement = "",
      x = rowname
    )
  ) %>%
  bind_rows(type_base_categores_df) %>%
  make_attributes() %>%
  make_coef_group() %>%
  mutate(group = factor(group, levels = c("Nonpartisan", "Partisan", "Difference")))


table_d9_dem <-
  all_df %>%
  filter(!is.na(Estimate), party == "Democrats") %>%
  mutate(entry = gen_entry_vec(est = Estimate, se = Std..Error, p = Pr...t..))  %>%
  select(coef_group, attribute, group, entry, party) %>%
  spread(key = group, value = entry) %>%
  select(-coef_group, -party)

table_d9_rep <-
  all_df %>%
  filter(!is.na(Estimate), party == "Republicans") %>%
  mutate(entry = gen_entry_vec(est = Estimate, se = Std..Error, p = Pr...t..))  %>%
  select(coef_group, attribute, group, entry) %>%
  spread(key = group, value = entry) %>%
  select(-coef_group)

gc4 <-
  ggplot(all_df,
         aes(
           x = Estimate,
           y = attribute,
           group = party,
           color = party,
           shape = party
         )) +
  scale_color_manual(values = c("blue", "red")) +
  geom_point(position = position_dodgev(height = 0.5), size = 2) +
  geom_errorbarh(aes(xmin = lis, xmax = uis),
                 position = position_dodgev(height = 0.5),
                 height = 0) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_grid(coef_group ~ group, scales = "free_y") +
  coord_cartesian(xlim = c(-1, 1)) +
  scale_x_continuous(breaks = round(seq(-.75, .75, .5), 2)) +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    strip.background = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank()
  )




# Figure C.6 Heterogeneous effects - policy index -------------------------------------------------------------------------------


fit_1 <-
  lm(
    policy_index ~ Gender + Age + Race + Job + Political,
    data = filter(mturk_clean, Party == "non-partisan", democrat == 1)
  )
fit_2 <-
  lm(
    policy_index ~ Gender + Age + Race + Job + Political + Party,
    filter(mturk_clean, Party != "non-partisan", democrat == 1)
  )
fit_3 <-
  lm(
    policy_index ~ (Gender + Age + Race + Job + Political) * (Party == "non-partisan") +
      (Party == "Republican") + (Party == "Democrat") ,
    filter(mturk_clean, democrat == 1)
  )

fit_4 <-
  lm(
    policy_index ~ Gender + Age + Race + Job + Political,
    data = filter(mturk_clean, Party == "non-partisan", republican == 1)
  )
fit_5 <-
  lm(
    policy_index ~ Gender + Age + Race + Job + Political + Party,
    data = filter(mturk_clean, Party != "non-partisan", republican == 1)
  )
fit_6 <-
  lm(
    policy_index ~ (Gender + Age + Race + Job + Political) * (Party == "non-partisan") +
      (Party == "Republican") + (Party == "Democrat") ,
    data = filter(mturk_clean, republican == 1)
  )


fit_1_cl <-
  cl(
    dat = filter(
      mturk_clean,
      Party == "non-partisan",
      democrat == 1,!is.na(policy_index)
    ),
    cluster = filter(
      mturk_clean,
      Party == "non-partisan",
      democrat == 1,!is.na(policy_index)
    )$resp_mturkid,
    fm = fit_1
  )
fit_2_cl <-
  cl(
    dat = filter(
      mturk_clean,
      Party != "non-partisan",
      democrat == 1,!is.na(policy_index)
    ),
    cluster = filter(
      mturk_clean,
      Party != "non-partisan",
      democrat == 1,!is.na(policy_index)
    )$resp_mturkid,
    fm = fit_2
  )
fit_3_cl <-
  cl(
    dat = filter(mturk_clean, democrat == 1, !is.na(policy_index)),
    cluster = filter(mturk_clean, democrat == 1, !is.na(policy_index))$resp_mturkid,
    fm = fit_3
  )

fit_4_cl <-
  cl(
    dat = filter(
      mturk_clean,
      Party == "non-partisan",
      republican == 1,!is.na(policy_index)
    ),
    cluster = filter(
      mturk_clean,
      Party == "non-partisan",
      republican == 1,!is.na(policy_index)
    )$resp_mturkid,
    fm = fit_4
  )
fit_5_cl <-
  cl(
    dat = filter(
      mturk_clean,
      Party != "non-partisan",
      republican == 1,!is.na(policy_index)
    ),
    cluster = filter(
      mturk_clean,
      Party != "non-partisan",
      republican == 1,!is.na(policy_index)
    )$resp_mturkid,
    fm = fit_5
  )
fit_6_cl <-
  cl(
    dat = filter(mturk_clean, republican == 1, !is.na(policy_index)),
    cluster = filter(mturk_clean, republican == 1, !is.na(policy_index))$resp_mturkid,
    fm = fit_6
  )


fit_1_df <-
  data.frame(fit_1_cl[,], group = "Nonpartisan") %>% rownames_to_column() %>%
  filter(rowname != "(Intercept)")
fit_2_df <-
  data.frame(fit_2_cl[,], group = "Partisan") %>% rownames_to_column() %>%
  filter(rowname != "(Intercept)")
fit_3_df <-
  data.frame(fit_3_cl[,], group = "Difference") %>% rownames_to_column() %>%
  filter(rowname != "(Intercept)")
fit_3_df <- fit_3_df[19:33,]
fit_3_df$rowname <- fit_1_df$rowname
fit_1_df$party <- "Democrats"
fit_2_df$party <- "Democrats"
fit_3_df$party <- "Democrats"

fit_4_df <-
  data.frame(fit_4_cl[,], group = "Nonpartisan") %>% rownames_to_column() %>%
  filter(rowname != "(Intercept)")
fit_5_df <-
  data.frame(fit_5_cl[,], group = "Partisan") %>% rownames_to_column() %>%
  filter(rowname != "(Intercept)")
fit_6_df <-
  data.frame(fit_6_cl[,], group = "Difference") %>% rownames_to_column() %>%
  filter(rowname != "(Intercept)")
fit_6_df <- fit_6_df[19:33,]
fit_6_df$rowname <- fit_1_df$rowname
fit_4_df$party <- "Republicans"
fit_5_df$party <- "Republicans"
fit_6_df$party <- "Republicans"




all_df <-
  bind_rows(fit_1_df, fit_2_df, fit_3_df, fit_4_df, fit_5_df, fit_6_df) %>%
  mutate(
    lis = Estimate - 1.96 * Std..Error,
    uis = Estimate + 1.96 * Std..Error,
    attribute = sub(
      pattern = "Gender|Age|Race|Job|Political|Party",
      replacement = "",
      x = rowname
    )
  ) %>%
  bind_rows(type_base_categores_df) %>%
  make_attributes() %>%
  make_coef_group() %>%
  mutate(group = factor(group, levels = c("Nonpartisan", "Partisan", "Difference")))


table_d11_dem <-
  all_df %>%
  filter(!is.na(Estimate), party == "Democrats") %>%
  mutate(entry = gen_entry_vec(est = Estimate, se = Std..Error, p = Pr...t..))  %>%
  select(coef_group, attribute, group, entry, party) %>%
  spread(key = group, value = entry) %>%
  select(-coef_group, -party)

table_d11_rep <-
  all_df %>%
  filter(!is.na(Estimate), party == "Republicans") %>%
  mutate(entry = gen_entry_vec(est = Estimate, se = Std..Error, p = Pr...t..))  %>%
  select(coef_group, attribute, group, entry) %>%
  spread(key = group, value = entry) %>%
  select(-coef_group)

gc6 <-
  ggplot(all_df,
         aes(
           x = Estimate,
           y = attribute,
           group = party,
           color = party,
           shape = party
         )) +
  scale_color_manual(values = c("blue", "red")) +
  geom_point(position = position_dodgev(height = 0.5), size = 2) +
  geom_errorbarh(aes(xmin = lis, xmax = uis),
                 position = position_dodgev(height = 0.5),
                 height = 0) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_grid(coef_group ~ group, scales = "free_y") +
  coord_cartesian(xlim = c(-1, 1)) +
  scale_x_continuous(breaks = round(seq(-.75, .75, .5), 2)) +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    strip.background = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank()
  )


# Figure C.8 Satisficing-------------------------------------------------------------------------------

different_df <-
  mturk_clean %>%
  filter(Party != "non-partisan", same_party == 0)

same_df <-
  mturk_clean %>%
  filter(Party != "non-partisan", same_party == 1)

partisan_df <-
  mturk_clean %>%
  filter(Party != "non-partisan")

fit_1 <-
  lm(win ~ Gender + Age + Race + Job + Political + Party, data = same_df)
fit_2 <-
  lm(win ~ Gender + Age + Race + Job + Political + Party, data = different_df)
fit_3 <-
  lm(win ~ (Gender + Age + Race + Job + Political + Party) * (same_party ==
                                                                1),
     data = partisan_df)

fit_1_cl <- cl(dat = same_df,
               cluster = same_df$resp_mturkid,
               fm = fit_1)
fit_2_cl <- cl(dat = different_df,
               cluster = different_df$resp_mturkid,
               fm = fit_2)
fit_3_cl <- cl(dat = partisan_df,
               cluster = partisan_df$resp_mturkid,
               fm = fit_3)

fit_1_df <-
  data.frame(fit_1_cl[,], group = "Same Party") %>% rownames_to_column() %>%
  filter(rowname != "(Intercept)")
fit_2_df <-
  data.frame(fit_2_cl[,], group = "Different Parties") %>% rownames_to_column() %>%
  filter(rowname != "(Intercept)")
fit_3_df <-
  data.frame(fit_3_cl[,], group = "Difference") %>% rownames_to_column() %>%
  filter(rowname != "(Intercept)")
fit_3_df <- fit_3_df[19:35,]
fit_3_df$rowname <- fit_1_df$rowname

all_df <- rbind(fit_1_df, fit_2_df, fit_3_df) %>%
  mutate(
    lis = Estimate - 1.96 * Std..Error,
    uis = Estimate + 1.96 * Std..Error,
    attribute = sub(
      pattern = "Gender|Age|Race|Job|Political|Party",
      replacement = "",
      x = rowname
    )
  ) %>%
  bind_rows(satisfice_base_categores_df) %>%
  make_attributes() %>%
  make_coef_group()

gc8 <-
  ggplot(all_df, aes(x = Estimate, y = attribute)) +
  geom_point() +
  geom_segment(aes(yend = attribute, x = lis, xend = uis)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_grid(coef_group ~ group, scales = "free_y") +
  coord_cartesian(xlim = c(-.4, .4)) +
  scale_x_continuous(breaks = round(seq(-.3, .3, .1), 1)) +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    strip.background = element_blank()
  )
