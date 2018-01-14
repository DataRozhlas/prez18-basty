library(tidyverse)

# načíst okrsková data 2013

download.file("https://www.volby.cz/opendata/prez2013/PREZ2013data20130126_csv.zip", "PREZ2013data20130126_csv.zip")
unzip("PREZ2013data20130126_csv.zip")
okrsky2013 <- read_csv2("pet1.csv")

obce2013 <- okrsky2013 %>%
  filter(KOLO==1) %>%
  group_by(OBEC, OKRES) %>%
  summarise(volici=sum(VOL_SEZNAM), vydane_obalky=sum(VYD_OBALKY), odevzdane_obalky=sum(ODEVZ_OBAL), platne_hlasy=sum(PL_HL_CELK), roithova=sum(HLASY_01), fischer=sum(HLASY_02), bobosikova=sum(HLASY_03), fischerova=sum(HLASY_04), sobotka=sum(HLASY_05), zeman=sum(HLASY_06), franz=sum(HLASY_07), dienstbier=sum(HLASY_08), schwarzenberg=sum(HLASY_09)) %>%
  mutate(ucast=vydane_obalky/volici)

#načíst okrsková data 2017

download.file("https://volby.cz/opendata/prez2018/PREZ2018data20180113_csv_kolo1.zip", "PREZ2018data20180113_csv_kolo1.zip")
unzip("PREZ2018data20180113_csv_kolo1.zip")
okrsky2017 <- read_csv2("pet1.csv")

cisob17 <- read_csv2("PREZ2018ciselniky20171215_csv/pecoco.csv")
cisnuts17 <- read_csv2("PREZ2018ciselniky20171215_csv/cnumnuts.csv")
names(cisnuts17)[1] <- "OKRES" 

okrsky2017 <- okrsky2017 %>%
  left_join(cisob17) %>%
  left_join(cisnuts17)

# kolik okrsků kdo vyrál

vitezove17 <- character()
for (i in 1:nrow(okrsky2017)) {
  vitez <- names(which.max(okrsky2017[i,14:22]))
  vitezove17 <- append(vitezove17, vitez)
  print(vitez)
}
rm(i,vitez)
table(vitezove17)

# kde to bylo
vitezove17 <- data.frame(vitezove17, okrsky2017, stringsAsFactors = F)
vitezove17 %>%
  select(vitezove17, NAZEVOBCE, NAZEVNUTS) %>%
  filter(vitezove17=="HLASY_01")

# volební místnosti, kam nepřišel vůbec nikdo
ucast_v_okrscich <- okrsky2017 %>%
  mutate(ucast=VYD_OBALKY/VOL_SEZNAM) %>%
  arrange(ucast)



obce2017 <- okrsky2017 %>%
  group_by(OBEC) %>%
  summarise(volici=sum(VOL_SEZNAM), vydane_obalky=sum(VYD_OBALKY), odevzdane_obalky=sum(ODEVZ_OBAL), platne_hlasy=sum(PL_HL_CELK), topolanek=sum(HLASY_01), horacek=sum(HLASY_02), fischer=sum(HLASY_03), hynek=sum(HLASY_04), hannig=sum(HLASY_05), kulhanek=sum(HLASY_06), zeman=sum(HLASY_07), hilser=sum(HLASY_08), drahos=sum(HLASY_09)) %>%
  left_join(cisob17) %>%
  left_join(cisnuts17) %>%
  mutate(ucast=vydane_obalky/volici, zeman)

### nejvyšší a nejnižší účast
nejnizsi_ucast <- obce2017 %>%
  select(obec=NAZEVOBCE, okres=NAZEVNUTS, ucast_pct=ucast, zapsani_volici=volici) %>%
  mutate(ucast_pct=ucast_pct*100) %>%
  arrange(ucast_pct)

nejvyssi_ucast <- obce2017 %>%
  select(obec=NAZEVOBCE, okres=NAZEVNUTS, ucast_pct=ucast, zapsani_volici=volici) %>%
  mutate(ucast_pct=ucast_pct*100) %>%
  arrange(desc(ucast_pct))

# kolik okrsků získali?
vitezove17 <- okrsky2017 %>%
  left_join(cisob17) %>%
  left_join(cisnuts17) %>%
  mutate(vitez=max(HLASY_01, HLASY_02, HLASY_03, HLASY_04, HLASY_05, HLASY_06, HLASY_07, HLASY_08, HLASY_09))

# platné hlasy vs. obálky
chybne <- okrsky2017 %>%
  left_join(cisob17) %>%
  left_join(cisnuts17) %>%
  filter(PL_HL_CELK>ODEVZ_OBAL)
