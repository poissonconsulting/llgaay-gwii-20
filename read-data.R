source("header.R")

# encounter <- read_csv("input/encounter.csv")

islands <- download_data("6990e632-bc99-4805-aa8c-36fd23c5f8f3")
event <- download_data("b91cbeb4-18a1-4b81-88a8-a46f40efa4f5")
#encounter <- download_data("b91cbeb4-18a1-4b81-88a8-a46f40efa4f5")
huntingteam <- download_data("2145143e-e3fa-4e4f-a8df-c8efefc7becc")

costs <- tibble::tribble(
  ~Type, ~HourlyRate,
  "BaitHunter",          180,
  "BaitHunterLead",      280,
  "BaitHunterAvg",       230,
  "BoatPlusOperator",     90,
  "HunterAvg",           135,
  "Dog",                  40,
  "DogHunter",            90,
  "HeliHunter",          405,
  "HeliPlusOperator",   1560
)

islands %<>%
  mutate(Area = as.numeric(Area))

huntingteam %<>%
  mutate(TrackTime = as.numeric(TrackTime),
         TrackLength = as.numeric(TrackLength),
         TrackFileError = as.logical(TrackFileError))

event %<>%
  mutate(across(c(Dogs, Hunters, Boats, Helicopters), ~as.integer(.x))) %>%
  mutate(across(c(BaitStationID, CommentEvent), ~na_if(.x, "NA")),
         across(c(OpportunisticHunting, GridSearch), ~as.logical(.x)),
         across(c(DateTimeOutingStart, DateTimeOutingEnd), ~dtt_date_time(.x, tz = "UTC")),
         CommentEvent = str_trim(CommentEvent))

island <- tribble(
  ~Island, ~Detections,~ObsEff,
  "Ramsay Island", 10L, 0.3,
  "Murchison Island", 2L, 0.75,
  "House Island", 0L, 0.9)

#saves in directory as objects
sbf_set_sub("read", rm = TRUE)

sbf_save_datas()

if(FALSE) {
  sbf_compare_data_archive()
}

