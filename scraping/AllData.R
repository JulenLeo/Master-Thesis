# Packages

pacman::p_load(rvest, stringr, dplyr, purrr, doParallel, foreach, xml2, tibble)

# load data

load("../data/clubs.RData")
load("../data/injuries.RData")
load("../data/players.RData")

# prepare the data to merge it

## club_df

clubs_df$foreigners <- as.numeric(clubs_df$foreigners)
clubs_df$season <- as.factor(clubs_df$season)
clubs_df$league <- as.factor(clubs_df$league)

## categorize injuries

injuries <- unique(injury_df$injury) %>% 
  sort()

injury_df <- injury_df %>% 
  mutate(injury_severity = case_when(
    days_missed < 7 ~ "Minor",
    days_missed >= 7 & days_missed < 28 ~ "Moderate",
    days_missed >= 28 & days_missed < 84 ~ "Severe",
    days_missed >= 84 ~"Very_severe"
  ))
injury_df$injury_severity <- as.factor(injury_df$injury_severity)

# Journal of Science and Medicine in Sport Paper ----

## ACL
injury_df$ACL <- injury_df$injury

table(injury_df$ACL[str_detect(injury_df$ACL, "Cruciate Ligament Rupture")])
idx <- str_detect(injury_df$ACL, "Cruciate Ligament Rupture") %>% which()
table(injury_df$ACL[str_detect(injury_df$ACL, "Cruciate Ligament Injury")])
idx <- c(idx, str_detect(injury_df$ACL, "Cruciate Ligament Injury") %>% which())
table(injury_df$ACL[str_detect(injury_df$ACL, "Cruciate Ligament Surgery")])
idx <- c(idx, str_detect(injury_df$ACL, "Cruciate Ligament Surgery") %>% which())

injury_df$ACL[idx] <- "ACL"
injury_df$ACL[-idx] <- "NO_ACL"
injury_df$ACL <- as.factor(injury_df$ACL)

## Injury location
injury_df$injury_loc <- injury_df$injury

# Head-neck
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Head Injury")])
idx <- str_detect(injury_df$injury_loc, "Head Injury") %>% which()
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Neck")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Neck") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Skull")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Skull") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Nose")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Nose") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Nasal")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Nasal") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Eye")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Eye") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Cheek")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Cheek") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Orbit")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Orbit") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Jaw")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Jaw") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Frontal")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Frontal") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Midface")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Midface") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Timpanum")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Timpanum") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Whiplash")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Whiplash") %>% which())

injury_df$injury_loc[idx] <- "Head-neck"

# Upper limb
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Wrist")])
idx <- str_detect(injury_df$injury_loc, "Wrist") %>% which()
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Wirst")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Wirst") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Finger")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Finger") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Hand")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Hand") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Thumb")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Thumb") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Arm")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Arm") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Forearm")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Forearm") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Elbow")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Elbow") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Ulnar")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Ulnar") %>% which())

injury_df$injury_loc[idx] <- "Upper limb"

# Trunk
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Shoulder")])
idx <- str_detect(injury_df$injury_loc, "Shoulder") %>% which()
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Abdominal Muscles Injury")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Abdominal Muscles Injury") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Abdominal Strain")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Abdominal Strain") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Lumbar")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Lumbar") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Chest")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Chest") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Vertebra")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Vertebra") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Hip")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Hip") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Acromioclavicular")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Acromioclavicular") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Collarbone")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Collarbone") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Back")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Back") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Rib")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Rib") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Pelvi")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Pelvi") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Cervical")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Cervical") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Coccyx")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Coccyx") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Spin")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Spin") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Herniated Disc")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Herniated Disc") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Umbilical Hernia")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Umbilical Hernia") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Lumbago")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Lumbago") %>% which())

injury_df$injury_loc[idx] <- "Trunk"

# Thigh-groin
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Groin")])
idx <- str_detect(injury_df$injury_loc, "Groin") %>% which()
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Thigh")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Thigh") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Biceps")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Biceps") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Hamstring")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Hamstring") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Adductor")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Adductor") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Abductor")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Abductor") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Femoral")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Femoral") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Dead Leg")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Dead Leg") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Inguinal Hernia")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Inguinal Hernia") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Pub")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Pub") %>% which())

injury_df$injury_loc[idx] <- "Thigh-groin"

# Knee
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Knee")])
idx <- str_detect(injury_df$injury_loc, "Knee") %>% which()
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Cruciate")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Cruciate") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Menisc")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Menisc") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Mensic")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Mensic") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Patella")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Patella") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Collateral")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Collateral") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Cartilage")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Cartilage") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Double Torn Ligament")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Double Torn Ligament") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Pattella")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Pattella") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Sideband")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Sideband") %>% which())

injury_df$injury_loc[idx] <- "Knee"


# Lower leg
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Tibia")])
idx <- str_detect(injury_df$injury_loc, "Tibia") %>% which()
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Fibula")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Fibula") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Leg Injury")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Leg Injury") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Fractured Leg")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Fractured Leg") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Achilles")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Achilles") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Shin")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Shin") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Calf")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Calf") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Perone")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Perone") %>% which())

injury_df$injury_loc[idx] <- "Lower leg"

# Ankle
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Ankle")])
idx <- str_detect(injury_df$injury_loc, "Ankle") %>% which()
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Syndesmotic")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Syndesmotic") %>% which())


injury_df$injury_loc[idx] <- "Ankle"

# Foot
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Foot")])
idx <- str_detect(injury_df$injury_loc, "Foot") %>% which()
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Toe")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Toe") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Heel")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Heel") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Metatarsal")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Metatarsal") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Arch")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Arch") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Navicular")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Navicular") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Plantar")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Plantar") %>% which())
table(injury_df$injury_loc[str_detect(injury_df$injury_loc, "Tarsal")])
idx <- c(idx, str_detect(injury_df$injury_loc, "Tarsal") %>% which())

injury_df$injury_loc[idx] <- "Foot"

# Unknown(or other)
locations<-c("Head-neck","Upper limb","Trunk","Thigh-groin","Knee","Lower leg",
             "Ankle","Foot")
logical <- !(injury_df$injury_loc %in% locations)
injury_df$injury_loc[logical] <- "Unknown"

table(injury_df$injury_loc)
injury_df$injury_loc <- as.factor(injury_df$injury_loc)

## Injury type
injury_df$injury_type <- injury_df$injury

# Muscle
table(injury_df$injury_type[str_detect(injury_df$injury_type, "Muscle")])
idx <- str_detect(injury_df$injury_type, "Muscle") %>% which()
table(injury_df$injury_type[str_detect(injury_df$injury_type, "Muscular")])
idx <- c(idx, str_detect(injury_df$injury_type, "Muscular") %>% which())
table(injury_df$injury_type[str_detect(injury_df$injury_type, "Aducctor")])
idx <- c(idx, str_detect(injury_df$injury_type, "Aducctor") %>% which())
table(injury_df$injury_type[str_detect(injury_df$injury_type, "Calf")])
idx <- c(idx, str_detect(injury_df$injury_type, "Calf") %>% which())
table(injury_df$injury_type[str_detect(injury_df$injury_type, "Contracture")])
idx <- c(idx, str_detect(injury_df$injury_type, "Condtracture") %>% which())
table(injury_df$injury_type[str_detect(injury_df$injury_type, "Groin")])
idx <- c(idx, str_detect(injury_df$injury_type, "Groin") %>% which())
table(injury_df$injury_type[str_detect(injury_df$injury_type, "Hamstring")])
idx <- c(idx, str_detect(injury_df$injury_type, "Hamstring") %>% which())
table(injury_df$injury_type[str_detect(injury_df$injury_type, "Biceps")])
idx <- c(idx, str_detect(injury_df$injury_type, "Biceps") %>% which())
table(injury_df$injury_type[str_detect(injury_df$injury_type, "Lumbago")])
idx <- c(idx, str_detect(injury_df$injury_type, "Lumbago") %>% which())
table(injury_df$injury_type[str_detect(injury_df$injury_type, "Pubalgia")])
idx <- c(idx, str_detect(injury_df$injury_type, "Pubalgia") %>% which())
table(injury_df$injury_type[str_detect(injury_df$injury_type, "Pubitis")])
idx <- c(idx, str_detect(injury_df$injury_type, "Pubitis") %>% which())
table(injury_df$injury_type[str_detect(injury_df$injury_type, "Thigh")])
idx <- c(idx, str_detect(injury_df$injury_type, "Thigh") %>% which())

injury_df$injury_type[idx] <- "Muscle"

# Ligament
table(injury_df$injury_type[str_detect(injury_df$injury_type, "Ligament")])
idx <- str_detect(injury_df$injury_type, "Ligament") %>% which()
table(injury_df$injury_type[str_detect(injury_df$injury_type, "Aigament")])
idx <- c(idx, str_detect(injury_df$injury_type, "Aigament") %>% which())
table(injury_df$injury_type[str_detect(injury_df$injury_type, "Sideband")])
idx <- c(idx, str_detect(injury_df$injury_type, "Sideband") %>% which())
table(injury_df$injury_type[str_detect(injury_df$injury_type, "Sprain")])
idx <- c(idx, str_detect(injury_df$injury_type, "Sprain") %>% which())
table(injury_df$injury_type[str_detect(injury_df$injury_type, "Strain")])
idx <- c(idx, str_detect(injury_df$injury_type, "Strain") %>% which())
table(injury_df$injury_type[str_detect(injury_df$injury_type, "Ankle")])
idx <- c(idx, str_detect(injury_df$injury_type, "Ankle") %>% which())

injury_df$injury_type[idx] <- "Ligament"

# Tendon
table(injury_df$injury_type[str_detect(injury_df$injury_type, "Tendon")])
idx <- str_detect(injury_df$injury_type, "Tendon") %>% which()
table(injury_df$injury_type[str_detect(injury_df$injury_type, "Achilles")])
idx <- c(idx, str_detect(injury_df$injury_type, "Achilles") %>% which())

injury_df$injury_type[idx] <- "Tendon"

# Bone
table(injury_df$injury_type[str_detect(injury_df$injury_type, "Rupture")])
idx <- str_detect(injury_df$injury_type, "Rupture") %>% which()
table(injury_df$injury_type[str_detect(injury_df$injury_type, "Fracture")])
idx <- c(idx, str_detect(injury_df$injury_type, "Fracture") %>% which())
table(injury_df$injury_type[str_detect(injury_df$injury_type, "Bone")])
idx <- c(idx, str_detect(injury_df$injury_type, "Bone") %>% which())
table(injury_df$injury_type[str_detect(injury_df$injury_type, "Broke")])
idx <- c(idx, str_detect(injury_df$injury_type, "Broke") %>% which())
table(injury_df$injury_type[str_detect(injury_df$injury_type, "Fissure")])
idx <- c(idx, str_detect(injury_df$injury_type, "Fissure") %>% which())
table(injury_df$injury_type[str_detect(injury_df$injury_type, "Spin")])
idx <- c(idx, str_detect(injury_df$injury_type, "Spin") %>% which())
table(injury_df$injury_type[str_detect(injury_df$injury_type, "Vertebra")])
idx <- c(idx, str_detect(injury_df$injury_type, "Vertebra") %>% which())
table(injury_df$injury_type[str_detect(injury_df$injury_type, "Patella")])
idx <- c(idx, str_detect(injury_df$injury_type, "Patella") %>% which())
table(injury_df$injury_type[str_detect(injury_df$injury_type, "Elbow")])
idx <- c(idx, str_detect(injury_df$injury_type, "Elbow") %>% which())
table(injury_df$injury_type[str_detect(injury_df$injury_type, "Facial")])
idx <- c(idx, str_detect(injury_df$injury_type, "Facial") %>% which())
table(injury_df$injury_type[str_detect(injury_df$injury_type, "Foot")])
idx <- c(idx, str_detect(injury_df$injury_type, "Foot") %>% which())
table(injury_df$injury_type[str_detect(injury_df$injury_type, "Crack")])
idx <- c(idx, str_detect(injury_df$injury_type, "Crack") %>% which())
table(injury_df$injury_type[str_detect(injury_df$injury_type, "Head")])
idx <- c(idx, str_detect(injury_df$injury_type, "Head") %>% which())
table(injury_df$injury_type[str_detect(injury_df$injury_type, "Hand")])
idx <- c(idx, str_detect(injury_df$injury_type, "Hand") %>% which())
table(injury_df$injury_type[str_detect(injury_df$injury_type, "Heel")])
idx <- c(idx, str_detect(injury_df$injury_type, "Heel") %>% which())
table(injury_df$injury_type[str_detect(injury_df$injury_type, "Hip")])
idx <- c(idx, str_detect(injury_df$injury_type, "Hip") %>% which())
table(injury_df$injury_type[str_detect(injury_df$injury_type, "Fibula")])
idx <- c(idx, str_detect(injury_df$injury_type, "Fibula") %>% which())
table(injury_df$injury_type[str_detect(injury_df$injury_type, "Nose")])
idx <- c(idx, str_detect(injury_df$injury_type, "Nose") %>% which())
table(injury_df$injury_type[str_detect(injury_df$injury_type, "Pelvi")])
idx <- c(idx, str_detect(injury_df$injury_type, "Pelvi") %>% which())
table(injury_df$injury_type[str_detect(injury_df$injury_type, "Shinbone")])
idx <- c(idx, str_detect(injury_df$injury_type, "Shinbone") %>% which())
table(injury_df$injury_type[str_detect(injury_df$injury_type, "Thumb")])
idx <- c(idx, str_detect(injury_df$injury_type, "Thumb") %>% which())
table(injury_df$injury_type[str_detect(injury_df$injury_type, "Toe")])
idx <- c(idx, str_detect(injury_df$injury_type, "Toe") %>% which())
table(injury_df$injury_type[str_detect(injury_df$injury_type, "Wirst")])
idx <- c(idx, str_detect(injury_df$injury_type, "Wirst") %>% which())

injury_df$injury_type[idx] <- "Bone"

# Cartilage
table(injury_df$injury_type[str_detect(injury_df$injury_type, "Cartilage")])
idx <- str_detect(injury_df$injury_type, "Cartilage") %>% which()
table(injury_df$injury_type[str_detect(injury_df$injury_type, "Hernia")])
idx <- c(idx, str_detect(injury_df$injury_type, "Hernia") %>% which())
table(injury_df$injury_type[str_detect(injury_df$injury_type, "Menisc")])
idx <- c(idx, str_detect(injury_df$injury_type, "Menisc") %>% which())

injury_df$injury_type[idx] <- "Cartilage"

# Superficial
table(injury_df$injury_type[str_detect(injury_df$injury_type, "Cut")])
idx <- str_detect(injury_df$injury_type, "Cut") %>% which()
table(injury_df$injury_type[str_detect(injury_df$injury_type, "Bruise")])
idx <- c(idx, str_detect(injury_df$injury_type, "Bruise") %>% which())
table(injury_df$injury_type[str_detect(injury_df$injury_type, "Edema")])
idx <- c(idx, str_detect(injury_df$injury_type, "Edema") %>% which())
table(injury_df$injury_type[str_detect(injury_df$injury_type, "Wound")])
idx <- c(idx, str_detect(injury_df$injury_type, "Wound") %>% which())

injury_df$injury_type[idx] <- "Superficial"

# Concussion
table(injury_df$injury_type[str_detect(injury_df$injury_type, "Concussion")])
idx <- str_detect(injury_df$injury_type, "Concussion") %>% which()
table(injury_df$injury_type[str_detect(injury_df$injury_type, "Contus")])
idx <- c(idx, str_detect(injury_df$injury_type, "Contus") %>% which())
table(injury_df$injury_type[str_detect(injury_df$injury_type, "Knock")])
idx <- c(idx, str_detect(injury_df$injury_type, "Knock") %>% which())
table(injury_df$injury_type[str_detect(injury_df$injury_type, "Dead Leg")])
idx <- c(idx, str_detect(injury_df$injury_type, "Dead Leg") %>% which())
table(injury_df$injury_type[str_detect(injury_df$injury_type, "Stroke")])
idx <- c(idx, str_detect(injury_df$injury_type, "Stroke") %>% which())

injury_df$injury_type[idx] <- "Concussion"

# Unknown(or other)
types<-c("Muscle","Ligament","Tendon","Bone","Cartilage","Superficial","Concussion")
logical <- !(injury_df$injury_type %in% types)
injury_df$injury_type[logical] <- "Unknown"

table(injury_df$injury_type)
injury_df$injury_type <- as.factor(injury_df$injury_type)

# Append injuries information to players_all_df ----

players_longitudinal_df <- players_all_df

clubs_info <- clubs_df[,c(3,10,11,12)]
players_longitudinal_df <- left_join(players_longitudinal_df, clubs_info, by = c("club_id", "season"))

players_longitudinal_df$injuries_total <- 0
players_longitudinal_df$inj_incidence <- 0

players_longitudinal_df$days_lost_total <- 0
players_longitudinal_df$inj_burden <- 0
players_longitudinal_df$games_lost_total <- 0

players_longitudinal_df$minor_inj_total <- 0
players_longitudinal_df$moderate_inj_total <- 0
players_longitudinal_df$severe_inj_total <- 0
players_longitudinal_df$very_severe_inj_total <- 0

players_longitudinal_df$acl_total <- 0

players_longitudinal_df$ankle_inj_total <- 0
players_longitudinal_df$foot_inj_total <- 0
players_longitudinal_df$headneck_inj_total <- 0
players_longitudinal_df$knee_inj_total <- 0
players_longitudinal_df$lowerleg_inj_total <- 0
players_longitudinal_df$thighgroin_inj_total <- 0
players_longitudinal_df$trunk_inj_total <- 0
players_longitudinal_df$upperlimb_inj_total <- 0
players_longitudinal_df$unknown_loc_inj_total <- 0

players_longitudinal_df$bone_inj_total <- 0
players_longitudinal_df$cartilage_inj_total <- 0
players_longitudinal_df$concussion_inj_total <- 0
players_longitudinal_df$ligament_inj_total <- 0
players_longitudinal_df$muscle_inj_total <- 0
players_longitudinal_df$superficial_inj_total <- 0
players_longitudinal_df$tendon_inj_total <- 0
players_longitudinal_df$unknown_type_inj_total <- 0

for(i in 1:nrow(players_longitudinal_df)){
  id <- players_longitudinal_df[i,2]
  seasoni <- players_longitudinal_df[i,6]
  df <- subset(injury_df, player_id==id & season==seasoni)
  if (length(df)){
    players_longitudinal_df$injuries_total[i]<-nrow(df)
    players_longitudinal_df$days_lost_total[i]<-sum(df$days_missed, na.rm = TRUE)
    players_longitudinal_df$games_lost_total[i]<-sum(df$games_missed, na.rm = TRUE)
    players_longitudinal_df$minor_inj_total[i]<-table(df$injury_severity)[1]
    players_longitudinal_df$moderate_inj_total[i]<-table(df$injury_severity)[2]
    players_longitudinal_df$severe_inj_total[i]<-table(df$injury_severity)[3]
    players_longitudinal_df$very_severe_inj_total[i]<-table(df$injury_severity)[4]
    players_longitudinal_df$acl_total[i] <- table(df$ACL)[1]
    players_longitudinal_df$ankle_inj_total[i] <- table(df$injury_loc)[1]
    players_longitudinal_df$foot_inj_total[i] <- table(df$injury_loc)[2]
    players_longitudinal_df$headneck_inj_total[i] <- table(df$injury_loc)[3]
    players_longitudinal_df$knee_inj_total[i] <- table(df$injury_loc)[4]
    players_longitudinal_df$lowerleg_inj_total[i] <- table(df$injury_loc)[5]
    players_longitudinal_df$thighgroin_inj_total[i] <- table(df$injury_loc)[6]
    players_longitudinal_df$trunk_inj_total[i] <- table(df$injury_loc)[7]
    players_longitudinal_df$unknown_loc_inj_total[i] <- table(df$injury_loc)[8]
    players_longitudinal_df$upperlimb_inj_total[i] <- table(df$injury_loc)[9]
    players_longitudinal_df$bone_inj_total[i] <- table(df$injury_type)[1]
    players_longitudinal_df$cartilage_inj_total[i] <- table(df$injury_type)[2]
    players_longitudinal_df$concussion_inj_total[i] <- table(df$injury_type)[3]
    players_longitudinal_df$ligament_inj_total[i] <- table(df$injury_type)[4]
    players_longitudinal_df$muscle_inj_total[i] <- table(df$injury_type)[5]
    players_longitudinal_df$superficial_inj_total[i] <- table(df$injury_type)[6]
    players_longitudinal_df$tendon_inj_total[i] <- table(df$injury_type)[7]
    players_longitudinal_df$unknown_type_inj_total[i] <- table(df$injury_type)[8]
  }
}

players_longitudinal_df$inj_incidence <- (1000*players_longitudinal_df$injuries_total)/(players_longitudinal_df$minutes_played/60)
players_longitudinal_df$inj_burden <- (1000*players_longitudinal_df$days_lost_total)/(players_longitudinal_df$minutes_played/60)
players_longitudinal_df$inj_incidence[is.nan(players_longitudinal_df$inj_incidence)]<-NA
players_longitudinal_df$inj_incidence[players_longitudinal_df$inj_incidence==Inf]<-NA
players_longitudinal_df$inj_burden[is.nan(players_longitudinal_df$inj_burden)]<-NA
players_longitudinal_df$inj_burden[players_longitudinal_df$inj_burden==Inf]<-NA
players_all<-players_longitudinal_df

# Factors order
players_all$position <- factor(players_all$position,
                               levels = c("Goalkeeper",
                                          "Defender - Centre-Back",
                                          "Defender - Left-Back",
                                          "Defender - Right-Back",
                                          "Midfielder - Defensive Midfielder",
                                          "Midfielder - Central Midfielder",
                                          "Midfielder - Left Midfielder",
                                          "Midfielder - Right Midfielder",
                                          "Midfielder - Attacking Midfielder",
                                          "Forward - Second Striker",
                                          "Forward - Left Winger",
                                          "Forward - Right Winger",
                                          "Forward - Centre-Forward"))
players_all$position2 <- factor(players_all$position2,
                               levels = c("Goalkeeper",
                                          "Defender",
                                          "Midfielder",
                                          "Forward"))

players_all$foot <- factor(players_all$foot,
                                levels = c("left",
                                           "both",
                                           "right"))

# SAVE ----
saveRDS(players_all, "../data/players_all.RDs", compress = TRUE)
