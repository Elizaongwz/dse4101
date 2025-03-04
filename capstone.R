library(dplyr)
library(grf)
library(Matching)
library(policytree)
library(DiagrammeR)

final <- read_csv("final.csv")
data = read_csv("echo_chamber.csv")

data = data %>%
  filter(sept20==1)

table4 = data.frame(infopros=data$infopros, infoproh=data$infoproh)

clean = final %>%
  filter(sept20 == 1) %>%
  mutate(polint2 = recode(polint2, "Very interested" = 4, "Somewhat interested" = 3, "Slightly interested" = 2, "Not at all interested" = 1))

clean = cbind(clean, table4)