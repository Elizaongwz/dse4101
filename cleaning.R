library(dplyr)

final <- read.csv("final.csv")
data = read.csv("echo_chamber.csv")

data = data %>%
  filter(sept20==1)

table4 = data.frame(infopros=data$infopros, infoproh=data$infoproh)

clean = final %>%
  filter(sept20 == 1) %>%
  mutate(polint2 = recode(polint2, "Very interested" = 4, "Somewhat interested" = 3, "Slightly interested" = 2, "Not at all interested" = 1))

clean = cbind(clean, table4)
df = select(clean,-state, -race, -trump, -liberal,-birthy,-sept20, -effserve:-effcarerev, -choice16,-fakedem:-realmix,-rfdemavg, -realdem, -rfgopavg, -rfdem, -rfgop, -mixfakeavg, -mixrealavg, -knowvp:-knowhouse, -intsocial, -totalfakeshareavg, -totalrealshareavg, -totaltruth, -totalshare, -gopfakeavg:-demrealavg)
df <- df %>% filter(!is.na(edu)) %>% filter(!is.na(male))
## using time spent on individual social media apps and external and internal efficacy instead of averaging all
df1 = select(df, -effavg, -socialavg)

## averaging efficacy but keeping individual time spent on social media apps separate
df_avgeff = select(df, -effext, -effint)

## averaging time spent on social media across all apps but keeping internal and external efficacy separate
df_avgsocial = select(df, -facebook:-reddit, -effavg)

## averaging all
df_avg = select(df,-facebook:-reddit, -effext,-effint)

save.image("clean.RData")


