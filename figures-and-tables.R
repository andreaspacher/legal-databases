library(tidyverse)
library(googlesheets4)

DF <- readxl::read_excel("official-legal-databases.xlsx", skip = 1)

# ========
#
# summary stats
#
# ========
DF %>%
  group_by(total_score) %>%
  summarise(sum = n(),
            pct = sum / nrow(DF),
            pct = scales::percent(pct, accuracy = 1)) %>%
  arrange(desc(total_score))

# ========
#
# Table 1
#
# ========
DF %>%
  select(
    #free_availability,
    searchability_title,
    searchability_fulltext,
    searchability_title,
    plausible_comprehensiveness) %>%
  cor() %>%
  round(2) 

DF %>%
  select(
    #free_availability,
    searchability_title,
    searchability_fulltext,
    searchability_title,
    plausible_comprehensiveness) %>%
  reshape2::melt() %>%
  group_by(variable) %>%
  summarise(sum = sum(value),
            pct = round(sum / nrow(DF), 2),
            pct = scales::percent(pct, accuracy = 1))

# ========
#
# Table 2
#
# ========

# done manually based on this approach:
DF_table2 <- DF %>%
  filter(total_score == 5) %>% # change the score!
  select(country)
aggregate(country ~., DF_table2, toString)

#
# Chi-Square-Test
# (correlation between continent & OLD-score)
#
DF_CORR <- DF %>% select(total_score, region)
DF_CORR$total_score <- as.factor(DF_CORR$total_score)
DF_CORR$region <- as.factor(DF_CORR$region)
chisq.test(DF_CORR$region, DF_CORR$total_score)
chi2 <- chisq.test(DF_CORR$total_score, DF_CORR$region)
corrplot::corrplot(chi2$residuals, is.cor = FALSE)

# ========
#
# Fig. 1: By contintent
#
# ========

DF3 <- DF %>%
  group_by(region) %>%
  mutate(cont_avg = round(mean(total_score), 2),
         region = paste0(region, " (mean score: ", cont_avg, ")")) %>%
  count(region, total_score) %>%
  mutate(pct = round(prop.table(n), 3)) %>%
  mutate(pct2 = scales::percent(pct, accuracy = 1)) %>%
  mutate(labelling = paste0(n, "\n(", pct2, ")"))

DF3$continent_f = factor(DF3$region,
                         levels = c("Europe (mean score: 4.91)", "Asia (mean score: 3.91)", "Africa (mean score: 1.96)",
                                    "Americas (mean score: 3.57)", "Oceania (mean score: 2.93)", "Contested States (mean score: 3.64)"))

ggplot(DF3, aes(x=total_score, y=as.numeric(n),
                label = labelling)) +
  geom_bar(stat='identity') +
  facet_wrap(~ continent_f, ncol = 2) +
  scale_x_continuous("Score", labels = as.character(DF3$total_score), breaks = DF3$total_score) +
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.2,    # nudge above top of bar
            size = 3) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +
  ylim(0, 50) +
  ylab("Countries")

ggsave("graphs\\continent.png",
       width = 7,
       height = 6.5,
       units = "in",
       dpi = 300)


ggplot(DF3, aes(x=total_score, y=pct,
                label = labelling)) +
  geom_bar(stat='identity') +
  facet_wrap(~ continent_f, ncol = 2) +
  scale_x_continuous("Score", labels = as.character(DF3$total_score), breaks = DF3$total_score) +
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.1,    # nudge above top of bar
            size = 3) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) + 
  scale_y_continuous(limits = c(0, 1.09),
                     labels = scales::percent_format(accuracy = 1)) +
  ylab("Countries")

ggsave("graphs\\continent_pct.png",
       width = 7,
       height = 6.5,
       units = "in",
       dpi = 300)

# ========
#
# Fig. 2: OECD Members
#
# ========

DF2 <- DF %>%
  count(oecd, total_score) %>%
  group_by(oecd) %>%
  mutate(pct = round(prop.table(n), 2)) %>%
  mutate(pct2 = scales::percent(pct, accuracy = 1)) %>%
  mutate(labelling = paste0(n, "\n(", pct2, ")"))

oecd_names <- c(
  `0` = "Others",
  `1` = "OECD Members"
)

DF2$oecd_f = factor(DF2$oecd, levels=c('1', '0'))

ggplot(DF2, aes(x = total_score, y = pct,
                label = labelling)) +
  geom_bar(stat='identity') +
  facet_wrap(~oecd_f, labeller = as_labeller(oecd_names)) +
  scale_x_continuous("Score", labels = as.character(DF2$total_score), breaks = DF2$total_score) +
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.1,    # nudge above top of bar
            size = 3) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank()) +
  ylab("Countries") +
  scale_y_continuous(limits = c(0, 1.05),
                     labels = scales::percent_format(accuracy = 1))

ggsave("graphs\\oecd.png",
       width = 7,
       height = 3.2,
       units = "in",
       dpi = 300)

# chi square:
chisq.test(DF$total_score, DF$oecd)


# ========
#
# Fig. 3: Boxplot Internet Usage
#
# ========
DF <- DF %>%
  select(country, iso3, total_score) %>%
  mutate(iso3 = ifelse(country == "Turkish Republic of Northern Cyprus", NA, iso3))
# load WorldBank data
worldbank <- readxl::read_excel("Data\\worldbank_internet_usage.xls", skip = 2) %>%
  janitor::clean_names()
# note: source is 
# https://data.worldbank.org/indicator/IT.NET.USER.ZS
# 5 Oct 2021
worldbank <- worldbank %>%
  group_by(country_name) %>%
  mutate(lastvalue = ifelse(!is.na(x2020), x2020,
                            ifelse(!is.na(x2019), x2019,
                                   ifelse(!is.na(x2018), x2018,
                                          ifelse(!is.na(x2017), x2017,
                                                 ifelse(!is.na(x2016), x2016,
                                                        ifelse(!is.na(x2015), x2015,
                                                               ifelse(!is.na(x2014), x2014,
                                                                      ifelse(!is.na(x2013), x2013, NA)))))))),
         lastyear = ifelse(!is.na(x2020), 2020,
                           ifelse(!is.na(x2019), 2019,
                                  ifelse(!is.na(x2018), 2018,
                                         ifelse(!is.na(x2017), 2017,
                                                ifelse(!is.na(x2016), 2016,
                                                       ifelse(!is.na(x2015), 2015,
                                                              ifelse(!is.na(x2014), 2014,
                                                                     ifelse(!is.na(x2013), 2013, NA)))))))),
  ) %>%
  select(country_name, country_code, indicator_name, lastvalue, lastyear)
DF4 <- right_join(worldbank, DF, by = c("country_code" = "iso3")) %>%
  filter(!is.na(country_name) & !is.na(lastvalue))
# ANOVA
DF4$total <- as.factor(DF4$total_score)
anova <- aov(lastvalue ~ total_score, data = DF4)
summary(anova)

DF4 %>%
  ungroup() %>%
  select(total_score, lastvalue) %>%
  ggplot(aes(x = total_score, y = lastvalue)) +
  #stat_boxplot(geom ='errorbar') + 
  geom_boxplot(width = 0.35) +
  ylab("Internet Usage of a Country's Population") +
  xlab("Score of Legal Database") +
  theme_minimal() +
  theme(
    # remove vertical grid lines
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(size = 0.1),
    axis.title = element_text(size = 7),
    axis.text = element_text(size = 7)
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     n.breaks = 10,
                     limits = c(1, 99))

ggsave("graphs\\boxplot_internet_usage.png",
       plot = last_plot(),
       width = 4,
       height = 2.5,
       units = "in",
       dpi = 300)