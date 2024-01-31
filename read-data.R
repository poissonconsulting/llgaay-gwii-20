source("header.R")

islands <- read_xlsx("input/islands.xlsx")
event <- read_csv("input/event.csv")
encounter <- read_csv("input/encounter.csv")
huntingteam <- read_csv("input/huntingteam.csv")
costs <- read_csv("input/costs.csv")

detection <- read_xlsx("input/CameraDataJuly20-Aug27-2017Ramsay.xlsx", sheet = "Aug2017-Jan2018CamData")
camera <- read_xlsx("input/CameraDataJuly20-Aug27-2017Ramsay.xlsx", sheet = "Metadata",
                    skip = 6)

island <- tribble(
  ~Island, ~Detections,~ObsEff,
  "Ramsay Island", 10L, 0.3,
  "Murchison Island", 2L, 0.75,
  "House Island", 0L, 0.9)

#saves in directory as objects
sbf_set_sub("read", rm = TRUE)

sbf_save_datas()
