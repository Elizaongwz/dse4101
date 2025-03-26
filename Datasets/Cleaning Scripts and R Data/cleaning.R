library(dplyr)

final <- read_csv("Datasets/final.csv")
data = read.csv("Datasets/echo_chamber.csv")

data = data %>%
  filter(sept20==1)

table4 = data.frame(infopros=data$infopros, infoproh=data$infoproh)

clean <- final %>%
  filter(sept20 == 1) %>%
  mutate(polint2 = case_when(
    polint2 == "Very interested" ~ 4,
    polint2 == "Somewhat interested" ~ 3,
    polint2 == "Slightly interested" ~ 2,
    polint2 == "Not at all interested" ~ 1,))

clean = cbind(clean, table4)
df = dplyr::select(clean,-state, -race, -trump, -liberal,-birthy,-sept20, -effserve:-effcarerev, -choice16,-fakedem:-realmix,-rfdemavg, -realdem, -rfgopavg, -rfdem, -rfgop, -mixfakeavg, -mixrealavg, -knowvp:-knowhouse, -intsocial, -totalfakeshareavg, -totalrealshareavg, -totaltruth, -totalshare, -gopfakeavg:-demrealavg)
df <- df %>% filter(!is.na(edu)) %>% filter(!is.na(male))
## using time spent on individual social media apps and external and internal efficacy instead of averaging all

# for echo treatment
df1 = dplyr::select(df, -effavg, -socialavg) %>%
  filter(prime==0)
df1_avg = dplyr::select(df,-facebook:-reddit, -effext,-effint) %>%
  filter(prime==0)
# for prime treatment
df2 = dplyr::select(df, -effavg, -socialavg) %>%
  filter(echo==0)
df2_avg = dplyr::select(df, -facebook:-reddit, -effext,-effint) %>%
  filter(echo==0)

# for real average (unable to use since everyone was in an echo chamber)
# for echo treatment
df1_real = df %>%
  filter(!is.na(totalrealavg)) %>%
  filter(prime==0)
# for prime treatment
df2_real = df %>%
  filter(!is.na(totalrealavg)) %>%
  filter(echo==0)

save.image("Datasets/Cleaning Scripts and R Data/clean.RData")


