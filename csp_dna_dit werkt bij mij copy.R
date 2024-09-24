# This scripts runs fully on my computer
# install.packages("devtools")
# devtools::install_github("gertstulp/ggzcentraal")
# library(ggzcentraal)
# 
# library(cspdna)
# 
# library(devtools)

devtools::load_all(".")

# Need to have local version of answers.json
source("tests/testthat/help_functions.R")
JSONdata <- opencpu_like_parse_json("tests/testthat/json/answers.json")


raw <- JSON_to_DF(JSONdata$answers)
data <- prepare_data(raw)
# one measurement 
data_one_meas <- dplyr::filter(data, Datum == as_datetime("2021-04-12 11:00:00", tz = "Europe/Amsterdam")) 
# for grid per day
data_day <- dplyr::filter(data, pertwee == 1) 


viz_grid(data_day)


# testje <- "blabla"
# viz_report_response(testje)
# viz_report_alone(testje)
# viz_report_behaviour(testje)
# viz_report_grid(testje)
# viz_carousel(testje) 
# viz_poster(testje)
# testje2 <- list(3, 4, 5)
# viz_report_response(testje2)
# viz_report_alone(testje2)
# viz_report_behaviour(testje2)
# viz_report_grid(testje2)
# viz_carousel(testje2) 
# viz_poster(testje2)

viz_grid(data)

source("tests/testthat/help_functions.R")
testthat::test_dir("tests/")
testthat::test_file("tests/testthat/test_viz_report_response.R")
testthat::test_file("tests/testthat/test_viz_report_alone.R")
testthat::test_file("tests/testthat/test_viz_report_behaviour.R")
testthat::test_file("tests/testthat/test_viz_report_grid.R")
testthat::test_file("tests/testthat/test_viz_zoom.R")
testthat::test_file("tests/testthat/test_viz_ts.R")
testthat::test_file("tests/testthat/test_comment_graph.R")
testthat::test_file("tests/testthat/test_time_count.R")
testthat::test_file("tests/testthat/test_viz_nw.R")
testthat::test_file("tests/testthat/test_viz_grid.R")
testthat::test_file("tests/testthat/test_viz_poster.R")
testthat::test_file("tests/testthat/test_viz_carousel.R")
testthat::test_file("tests/testthat/test_prepare_data.R")



testthat::test_file("tests/testthat/test_viz_poster.R")

testthat::test_file("tests/testthat/test_viz_poster.R")

system.time(
  viz_carousel(data)
)

system.time(
  viz_poster(data)
)


system.time(
  testthat::test_file("tests/testthat/test_viz_carousel.R")
)

benchplot(
viz_nw(data_one_meas, output = "slider")
)

system.time(
  viz_ts(data, left_right = "left")
)

benchplot(
  viz_ts(data, left_right = "left") + viz_ts(data, left_right = "left") + viz_ts(data, left_right = "left")
)

a <- viz_ts(data, left_right = "left") 
a

ggplot(iris, aes(Species, Sepal.Length)) + geom_point()

SVGlist <- grid.export(name = NULL)
#str(SVGlist, 1)

print(SVGlist$svg)

testthat::test_file("tests/testthat/test_viz_carousel.R")

data_one_meas <- dplyr::filter(data, Datum == as_datetime("2023-07-21 20:00:00")) 
data_one_meas_post <- data_one_meas %>% 
  mutate(across(.cols = everything(), .fns = function(x) ifelse(is.numeric(x) & x > 0, runif(1, min = 60, max = 100), x)),
         Datum = as_datetime("2021-04-17 14:00:00"),
         csp_dna_fase = 3)


system.time(
temp <- viz_nw(data_one_meas, output = "slider") + viz_nw(data_one_meas, output = "slider") + viz_grid(data)
)
system.time(
viz_ts(data, left_right = "left")
)

system.time(
viz_grid(data)
)

test <- data %>%
  select(all_of(c("Datum", "csp_dna_55a", "csp_dna_56a",
                  "csp_dna_57a", "csp_dna_fase", "dayno", "pertwee"))) %>%
  tidyr::gather()

viz_nw(output = "slider")

/ viz_nw(data_zoom, output = "slider")


system.time(
viz_carousel(data) 
)

data_one_meas <- dplyr::filter(data, Datum == as_datetime("2023-07-21 20:00:00")) 

testthat::test_dir("tests/")


# First visualisation for report
viz_report_response(data)
viz_report_response(data, output_format = "ggplot")
testthat::test_file("tests/testthat/test_viz_report_response.R")
ggsave("report.pdf", viz_report_alone(data, output_format = "ggplot"),
       height = 2.5, width = 5)

# Creates second visualisation (svg) for report
viz_report_alone(data)
viz_report_alone(data, output_format = "ggplot")
testthat::test_file("tests/testthat/test_viz_report_alone.R")
ggsave("alone.pdf", viz_report_alone(data, output_format = "ggplot"), 
       height = 1.5, width = 5)

# Creates third visualisation (svg) for report
viz_report_behaviour(data)
viz_report_behaviour(data, output_format = "ggplot")
testthat::test_file("tests/testthat/test_viz_report_behaviour.R")
ggsave("behaviour.pdf", viz_report_behaviour(data, output_format = "ggplot"), 
       height = 7.5, width = 5)

# Creates fourth visualisation (svg) for report
viz_report_grid(data)
viz_report_grid(data, output_format = "ggplot")

# for legend for laura
data_one_meas_grid <- data_one_meas %>% 
  mutate(across(.cols = everything(), .fns = function(x) ifelse(x == 0, 1, x)),
         Datum = as_datetime("2021-04-17 14:00:00"))
viz_grid(data_one_meas_grid)
ggsave("grid_legend.pdf", width = 5, height = 6)
ggsave("grid_legend.jpg", width = 5, height = 6, dpi = 300)
ggsave("grid_legend.svg", width = 5, height = 6)

data_one_meas_post <- data_one_meas %>% 
  mutate(across(.cols = everything(), .fns = function(x) ifelse(is.numeric(x) & x > 0, runif(1, min = 60, max = 100), x)),
         Datum = as_datetime("2021-04-17 14:00:00"),
         csp_dna_fase = 3)
viz_nw(data_one_meas_post)
ggsave("circle_legend.pdf", width = 5, height = 5)
ggsave("circle_legend.jpg", width = 5, height = 5, dpi = 300)
ggsave("circle_legend.svg", width = 5, height = 5)


testthat::test_file("tests/testthat/test_viz_report_grid.R")
ggsave("grid.pdf", viz_report_grid(data, output_format = "ggplot"), 
       height = 7.5, width = 5)



prepare_data(data)

# Creates multiple visualisations (svg) that should be incorporated into slider
viz_carousel(data) 
testthat::test_file("tests/testthat/test_viz_carousel.R")

temp$svgs$slider$svg_slider_0

# Creates poster (svg) 

#viz_poster_long(data, height = 1189 / 2.54 / 10, width = 841 / 2.54 / 10)



# TOO NARROW viz_poster(data, height = (29.7 / 2.54) * 3, width = 42 / 2.54)
# BIJNA viz_poster(data, height = 2 * (29.7 / 2.54) * 3, width = 2 * (42 / 2.54))
# bijnaviz_poster(data, height = 2.2 * (29.7 / 2.54) * 3, width = 2.2 * (42 / 2.54))
# BIJNA MAAR LENGTE NIET GOED, BREEDTE WEL 
# viz_poster(data, height = 2 * (29.7 / 2.54) * 3, width = 2.1 * (42 / 2.54)) 
# viz_poster(data, height = 1.8 * (29.7 / 2.54) * 3, width = 2.1 * (42 / 2.54)) 

# viz_poster(data, height = 1.7 * (29.7 / 2.54) * 3, width = 2.1 * (42 / 2.54)) 

# 14 juli
viz_poster(data, height = 1.6 * (29.7 / 2.54) * 3, width = 2.12 * (42 / 2.54)) 
viz_poster(data)


#viz_poster(data, height = 1189 / 2.54 / 10, width = 841 / 2.54 / 10)
testthat::test_file("tests/testthat/test_viz_poster.R")


# test bc

data %>% slice(1) %>% viz_bc

negatief <- c("Bedroefd", "Boos", "Bang", "Energie", "Spanning", "Zelfbeeld",
              "Terugtrekken", "Destructief", "Suicidaliteit", "Onrustig", "Bijzondere_ervaringen",
              "Negatief_contact", "Lichamelijke_klachten", "Onplezierig")

# Vector with variable names of positive emotions
positief <- c("Blij", "Ondernemen", "Contact_behoefte", "Ervaren_controle",
              "Zorg_zelf", "Activiteiten", "Verplichtingen", "Plezierig")

# Vector with variable names
vars_meas <- c(negatief, positief)



ggplot(filter(long_test, Datum %in% c(as_datetime("2021-04-12 09:00:00"), as_datetime("2021-04-12 14:00:00"))), 
       aes(x = factor(Var, levels = vars_meas, ordered = TRUE), 
                         y = Score, fill = pos_neg)) +
  geom_bar(stat = "identity", na.rm = TRUE) +
  facet_wrap(as.formula(paste("~", "factor(Datum, ordered = TRUE)")), nrow = 1) +
  scale_x_discrete(labels = function(x) tolower(substr(x, 1, 4))) +
  scale_y_continuous(limits = c(0, 150), breaks = c(0, 50, 100), expand = c(0, 0)) +
  scale_fill_manual(values = c("#CC79A7", "#56B4E9")) +
  labs(x = NULL, y = NULL) +
  coord_flip() + 
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 90),
    # axis.ticks = element_blank(),
    # axis.title = element_blank(),
    # legend.key = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    strip.background = element_rect(fill = NA, colour = "grey50"),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )







### TESTING VIZ GRID #####


data_day <- dplyr::filter(data, pertwee == 3) 

viz_grid(data_day)

viz_grid(data_one_meas)

# New names of all variables in grid
grid_nms_mw <- c("Whatsapp", "Bellen", "Deurbel", "Smsen", "Afgezegd", "Werk_school_sport",
                 "Douchen", "Dag_nacht_ritme", "Eten", "Bewogen", "Recept_Medicatie",
                 "Medicatie_ingenomen", "Snijden", "Bonken_hoofd", "Krabben",
                 "Krassen", "Slaan_vuist", "Anderen_schade", "Spullen_kapot",
                 "Uitrekken_haren", "Alcohol_drugs", "Uitgeven_geld", "Gokken",
                 "Seksueel_risicovol", "Eetbui", 
                 "Anders_destructief", # NIEUW  
                 "Passieve_gedachtes", "Actieve_gedachtes",
                 "Afscheidsbrief", "Concreet_plan", "TV", "Muziek", "Yoga", "Wandelen",
                 "Creatief", "Lezen", "Sporten", "Huishouden", "Spelletje", 
                 "Slapen_act", # NIEUW
                 "Contact_zoeken", "Afgesproken", "Buiten", "Anders", "Nagelbijten",
                 "Ijsberen", "Mouw_trekken", "Schoonmaken", "Roken", "Praten_anders",
                 "Uitpraten_niet", "Piekeren", "Dwanghandelingen", "Contact_veel",
                 "Alleen_niet", "Beslissingen_zelf", 
                 "Anders_onrustig", # NIEUW
                 "Stemmen", "Schimmen", "Dissociaties",
                 "Achterdochtig", "Opdracht_krijgen", "Herbeleving", "Werken",
                 "Opleiding", "Kind_zorg", "Wassen", "Koken", "Schoonmaken_verplichting",
                 "Boodschappen", "Afspraken_nakomen", "Telefoon_onaardig", "Face_to_face_onaardig",
                 "Whatsapp_onaardig", "Social_media_onaardig", "Ademhalingsproblemen",
                 "Pijn_borst", "Hartkloppingen", "Misselijkheid", "Ontlasting",
                 "Buikpijn", "Hoofdpijn", "Duizeligheid", "Vermoeidheid", "Rugpijn",
                 "Spierpijn", "Tintelingen", "Zweten", "Veel_slapen", "Weinig_slapen",
                 "Nachtmerries")

# This creates a "long" rather than "wide" dataset, necessary for visualisation
grid_df <- data_day %>%
  # Select variables
  select(Datum, all_of(grid_nms_mw), csp_dna_fase, dayno, pertwee) %>%
  # From wide to long dataformat
  gather(all_of(grid_nms_mw), key = "Variabele", value = "Score") %>%
  # Create new variables
  mutate(var_cat = case_when(Variabele %in% c("Whatsapp","Bellen","Deurbel","Smsen","Afgezegd","Werk_school_sport") ~ "Terugtrekken",
                             Variabele %in% c("Douchen","Dag_nacht_ritme" ,"Eten","Bewogen", "Recept_Medicatie","Medicatie_ingenomen") ~ "Zorg_zelf",
                             Variabele %in% c("Snijden","Bonken_hoofd","Krabben","Krassen","Slaan_vuist","Anderen_schade","Spullen_kapot","Uitrekken_haren",
                                              "Alcohol_drugs","Uitgeven_geld","Gokken","Seksueel_risicovol","Eetbui", "Anders_destructief"  ) ~ "Destructief",
                             Variabele %in% c("Passieve_gedachtes","Actieve_gedachtes","Afscheidsbrief","Concreet_plan") ~ "Suicidaliteit",
                             Variabele %in% c("TV","Muziek","Yoga","Wandelen","Creatief","Lezen","Sporten","Huishouden","Spelletje",
                                              "Slapen_act","Contact_zoeken" ,"Afgesproken","Buiten", "Anders") ~ "Activiteiten",
                             Variabele %in% c("Nagelbijten","Ijsberen","Mouw_trekken","Schoonmaken","Roken","Praten_anders","Uitpraten_niet",
                                              "Piekeren","Dwanghandelingen","Contact_veel","Alleen_niet","Beslissingen_zelf", "Anders_onrustig") ~  "Onrustig" ,
                             Variabele %in% c("Stemmen", "Schimmen", "Dissociaties", "Achterdochtig", "Opdracht_krijgen",
                                              "Herbeleving" ) ~  "Bijzondere_ervaringen",
                             Variabele %in% c("Werken","Opleiding","Kind_zorg", "Wassen","Koken",
                                              "Schoonmaken_verplichting", "Boodschappen","Afspraken_nakomen" ) ~   "Verplichtingen",
                             Variabele %in% c("Telefoon_onaardig", "Face_to_face_onaardig", "Whatsapp_onaardig",
                                              "Social_media_onaardig") ~ "Negatief_contact",
                             Variabele %in% c("Ademhalingsproblemen","Pijn_borst","Hartkloppingen","Misselijkheid","Ontlasting","Buikpijn",
                                              "Hoofdpijn", "Duizeligheid","Vermoeidheid","Rugpijn","Spierpijn", "Tintelingen",
                                              "Zweten","Veel_slapen","Weinig_slapen", "Nachtmerries") ~ "Lichamelijke_klachten"),
         clr = case_when(var_cat %in% c("Terugtrekken", "Lichamelijke_klachten", "Bijzondere_ervaringen", "Onrustig","Suicidaliteit","Destructief","Negatief_contact") ~ "#CC79A7",
                         var_cat %in% c("Zorg_zelf", "Verplichtingen", "Activiteiten") ~ "#56B4E9"), # This adds colouring variable
         var_cat = fct_inorder(var_cat),
         Var2 = fct_inorder(Variabele),
         Score = as.numeric(Score)) %>%
  # Remove missing values for variable clr
  filter(!is.na(clr))

# Creating order in factor
grid_df <- grid_df %>%
  group_by(var_cat) %>%
  mutate(y = dense_rank(as.numeric(Var2)))

ggplot(grid_df, aes(x = var_cat, y = y)) +
  geom_blank() +
  geom_point(data = filter(grid_df, Score == 1),
             aes(x = var_cat, y = y, colour = clr), size = 8, inherit.aes = FALSE) +
  geom_text(data = filter(grid_df, Score == 1),
            aes(x = var_cat, y = y, label = tolower(substr(Variabele, 1, 3))), size = 3.5,
            colour = "white", angle = 45, inherit.aes = FALSE) +
  theme_classic() +
  scale_colour_identity() +
  coord_flip() +
  scale_x_discrete(limits = rev(c("Terugtrekken", "Lichamelijke_klachten", "Bijzondere_ervaringen", "Onrustig","Suicidaliteit","Destructief","Negatief_contact","Zorg_zelf", "Verplichtingen", "Activiteiten"))) +
  scale_y_continuous(breaks = c(1:50)) +
  labs(x = NULL, y = NULL) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 0.5))  +
  facet_wrap(~Datum, nrow = 1) 

#### TESTING VIZ_NW

viz_nw(data_one_meas)
viz_nw(data_day)

temp <- ggplot_build(plot)


# Vector with variable names of negative emotions
negatief <- c("Bedroefd", "Boos", "Bang", "Energie", "Spanning", "Zelfbeeld",
              "Terugtrekken", "Destructief", "Suicidaliteit", "Onrustig", "Bijzondere_ervaringen",
              "Negatief_contact", "Lichamelijke_klachten", "Onplezierig")

# Vector with variable names of positive emotions
positief <- c("Blij", "Ondernemen", "Contact_behoefte", "Ervaren_controle",
              "Zorg_zelf", "Activiteiten", "Verplichtingen", "Slapen", "Plezierig")

# Create long dataframe
long <- data_day %>%
  # Select variables
  select(all_of(c("Datum","csp_dna_fase", "dayno", "pertwee",
                  positief, negatief))) %>%
  # From wide to long
  gather(all_of(c(positief, negatief)), key = "Var", value = "Score") %>%
  # Create new variable
  mutate(pos_neg = case_when(Var %in% positief ~ "positief",
                             Var %in% negatief ~ "negatief"))


# Vector with variable names
vars_meas <- c(negatief, positief)

# Create dataframe with layout of nodes in a circle
no_nodes <- length(vars_meas)
if (no_nodes == 2) {
  node_df <- data.frame(Name = vars_meas,
                        x = c(0,0),
                        y = c(1, -1),
                        stringsAsFactors = FALSE)
} else {
  node_df <- data.frame(Name = vars_meas,
                        x = sin(2 * pi * ((0:(no_nodes - 1))/no_nodes)),
                        y = cos(2 * pi * ((0:(no_nodes - 1))/no_nodes)),
                        stringsAsFactors = FALSE)
}

# Combine coordinates nodes with data
long <- dplyr::left_join(long, node_df, by = c("Var" = "Name"))

# Create abbreviations of node names
long$abbr <- tolower(substr(long[["Var"]], 1, 4))

# Base plot
plot <- ggplot(long, aes(x = .data[["x"]], y = .data[["y"]])) +
  geom_point(size = 10, colour = "lightgrey", na.rm = TRUE) +
  scale_x_continuous(expand = c(0.20, 0)) +
  #scale_y_continuous(expand = c(0.20, 0)) +
  coord_fixed() +
  facet_wrap(~Datum, nrow = 1) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.key = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    strip.background = element_rect(fill = NA, colour = "grey50"),
    panel.border = element_blank(),
    panel.grid = element_blank()
  ) +
  # Add coloured circles depending on score
  geom_point(aes(size = .data[["Score"]], colour = .data[["pos_neg"]]), na.rm = TRUE) +
  scale_size_area(max_size = 10) +
  scale_colour_manual(values = c("#CC79A7", "#56B4E9")) +
  # Add variable label in circle
  geom_text(aes(label = abbr), colour = "white") +
  guides(colour = "none", size = "none") +
  # add bar for colour fase
  geom_rect(data = filter(long, !is.na(csp_dna_fase)),
            aes(xmin = -Inf, xmax = Inf,
                ymin = -1.21, ymax = -1.26, fill = factor(csp_dna_fase)), inherit.aes = FALSE) +
  scale_fill_manual(values = c("1" = "green3", "2" = "yellow2",
                               "3" = "darkorange", "4" = "firebrick2"), 
                    guide = "none") +
  ylim(c(-1.27, 1.27)) 


if(output == "slider") {
  # Create labels for slider
  # Create label dataframe
  df_label <- data_day %>%
    # Select variables
    select(all_of(c("Datum", "csp_dna_55a", "csp_dna_56a", "csp_dna_57a"))) %>%
    # Create better labels
    mutate(
      csp_dna_55a_v = map_chr(csp_dna_55a, comment_graph),
      csp_dna_56a_v = map_chr(csp_dna_56a, comment_graph),
      csp_dna_57a_v = map_chr(csp_dna_57a, comment_graph)
    )
    
  # Add event labels for slider
  return(
    plot +
      # top label
      geom_label(data = filter(df_label, !is.na(csp_dna_55a_v)),
                 aes(x = 0, y = 0.6, label = csp_dna_55a_v),
                 lineheight = 0.72, size = 3) + 
      geom_label(data = filter(df_label, !is.na(csp_dna_56a_v)),
                 aes(x = 0, y = 0.1, label = csp_dna_56a_v),
                 lineheight = 0.72, size = 3) +
      geom_label(data = filter(df_label, !is.na(csp_dna_57a_v)),
                 aes(x = 0, y = -0.40, label = csp_dna_57a_v),
                 lineheight = 0.72, size = 3) 
  )
} else {
  # Create labels for poster
  # Create label dataframe
  df_label <- data_day %>%
    # Select variables
    select(all_of(c("Datum", "csp_dna_55a", "csp_dna_77a", "csp_dna_78a"))) %>%
    # Create better labels
    mutate(
      csp_dna_55a_v = map_chr(csp_dna_55a, comment_graph),
      csp_dna_77a_v = map_chr(csp_dna_77a, comment_graph),
      csp_dna_78a_v = map_chr(csp_dna_78a, comment_graph)
    )
  
  # Add event labels for poster
  return(
    plot +
      # top label
      geom_label(data = filter(df_label, !is.na(csp_dna_55a_v)),
                 aes(x = 0, y = 0.6, label = csp_dna_55a_v),
                 lineheight = 0.72, size = 3) + 
      geom_label(data = filter(df_label, !is.na(csp_dna_77a_v)),
                 aes(x = 0, y = 0.1, label = csp_dna_77a_v),
                 lineheight = 0.72, size = 3) +
      geom_label(data = filter(df_label, !is.na(csp_dna_78a_v)),
                 aes(x = 0, y = -0.5, label = csp_dna_78a_v),
                 lineheight = 0.72, size = 3) 
  )
}




### ERROR WITH PERSON cspdna_461094.json ###

JSONdata <- opencpu_like_parse_json("tests/testthat/json/cspdna_461094.json")

JSONdata <- opencpu_like_parse_json("/Users/gert/Desktop/cspdna_461094.json")


raw <- JSON_to_DF(JSONdata$answers)
data <- prepare_data(raw)

source("tests/testthat/help_functions.R")
testthat::test_dir("tests/")
testthat::test_file("tests/testthat/test_viz_report_response.R")
testthat::test_file("tests/testthat/test_viz_report_alone.R")
testthat::test_file("tests/testthat/test_viz_report_behaviour.R")
testthat::test_file("tests/testthat/test_viz_report_grid.R")
testthat::test_file("tests/testthat/test_viz_zoom.R")
testthat::test_file("tests/testthat/test_viz_ts.R")
testthat::test_file("tests/testthat/test_comment_graph.R")
testthat::test_file("tests/testthat/test_time_count.R")
testthat::test_file("tests/testthat/test_viz_nw.R")
testthat::test_file("tests/testthat/test_viz_grid.R")
testthat::test_file("tests/testthat/test_viz_poster.R")
testthat::test_file("tests/testthat/test_viz_carousel.R")
testthat::test_file("tests/testthat/test_prepare_data.R")



testthat::test_file("tests/testthat/test_viz_poster.R")

testthat::test_file("tests/testthat/test_viz_poster.R")





# First visualisation for report
viz_report_response(data)
viz_report_response(data, output_format = "ggplot")
ggsave("report_461094.pdf", viz_report_response(data, output_format = "ggplot"),
       height = 1.5, width = 5)


# Creates second visualisation (svg) for report
viz_report_alone(data)
viz_report_alone(data, output_format = "ggplot")
ggsave("alone_461094.pdf", viz_report_alone(data, output_format = "ggplot"), 
       height = 2.5, width = 5)

# Creates third visualisation (svg) for report
viz_report_behaviour(data)
viz_report_behaviour(data, output_format = "ggplot")
ggsave("behaviour_461094.pdf", 
       height = 7.5, width = 5)
ggsave("behaviour_461094.pdf", viz_report_behaviour(data, output_format = "ggplot"), 
       height = 7.5, width = 5)

# Creates fourth visualisation (svg) for report
viz_report_grid(data)
viz_report_grid(data, output_format = "ggplot")

ggsave("grid_461094.pdf", viz_report_grid(data, output_format = "ggplot"), 
       height = 7.5, width = 5)



prepare_data(data)

# Creates multiple visualisations (svg) that should be incorporated into slider
viz_carousel(data) 
testthat::test_file("tests/testthat/test_viz_carousel.R")

temp$svgs$slider$svg_slider_0

# Creates poster (svg) 

#viz_poster_long(data, height = 1189 / 2.54 / 10, width = 841 / 2.54 / 10)

viz_poster(data)
