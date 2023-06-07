
library(eurostat)
library(tidyverse)
library(rvest)
library(dplyr)
library(magrittr)
library(readr)
library(dplyr)
library(ggplot2)
library(rpart)
library(stringr)
#Problema 1 a) 
# Problema 1: Se da o lista cu numere naturale pozitive. Determinati cel mai mare numar din lista.

cel_mai_mare_nr <- function(vector) {
  max <- 0
  if(length(vector)<1)
  {
    return("Lista este goala")
  }
  for (i in 1:length(vector)) {
    if (vector[i] > max) {
      max <- vector[i]
    }
  }
  return(max)
}

cel_mai_mare_nr(c())

# Problema 2: Generati toate numerele din seria lui fibonacci care sunt mai mici decat un numar dat n:
fibo <- function(n) {
  serie <- c(0, 1)
  i <- 2
  repeat {
    urmatorul_termen <- serie[i] + serie[i - 1]
    if (urmatorul_termen >= n) {
      break
    }
    i <- i + 1
    serie[i] <- urmatorul_termen
  }
  return(serie)
}

# Problema 3: Calcularea mediei ponderate a notelor unui student în funcție de ponderile asociate fiecărei note. 
# Se dau 2 vectori, unul contine notele in ordine, celalat contine ponderele din medie pentru fiecare nota. 
# Stabili daca studentul a trecut(are media peste 5)

medie_ponderata <-function(note,ponderi){


  media_ponderata <- 0
  for (i in 1:length(note)) {
    media_ponderata <- media_ponderata + note[i] * ponderi[i]
  }
  if (media_ponderata>=5)
  {
    print("Studentul a promovat cu media:")
    print(media_ponderata)
    return(1)
  }
  print("Studentul nu a promovat, a avut media:")
  print(media_ponderata)
  return(0)
}


#Problema 1 b)

# Funcție pentru calcularea sumei elementelor unui vector
suma <- function(vector) {
  sum_vector <- 0
  for (i in 1:length(vector))
  {
    sum_vector <- sum_vector + vector[i]
  }
  print(sum_vector)
}

# Funcție pentru calcularea mediei elementelor unui vector
medie <- function(vector){
  print(suma(vector) / length(vector))
}

# Funcție pentru calcularea abaterii standard a elementelor unui vector
abatere_standard <- function(vector){
  suma_var2 <- 0
  medie_x <- medie(vector)
  for (i in 1:length(vector)){
    suma_var2 <- suma_var2 + (vector[i] - medie_x) ^ 2
  }
  suma_var2 <- suma_var2 / (length(vector) - 1)
  print(sqrt(suma_var2))
}

#test student
test_student <- function(vector, c) {
  n <- length(vector)
  x_bar <- medie(vector)
  s <- abatere_standard(vector)
  t <- (x_bar - c) / (s / sqrt(n))
  print(t)
}


#functie pentru calculul covariatei
covariatie <- function(vector1, vector2) {
  n <- length(vector1)
  media1 <- medie(vector1)
  media2 <- medie(vector2)
  suma_produs <- suma((vector1 - media1) * (vector2 - media2))
  covariatie <- suma_produs / (n - 1)
  return(covariatie)
}


# Funcție pentru calcularea coeficientului de corelatie liniara intre 2 variabile
correlation_coefficient <- function(vector1, vector2) {
  covar <- covariatie(vector1, vector2) 
  sd1 <- abatere_standard(vector1)  # Calculează deviația standard a vectorului 1
  sd2 <- abatere_standard(vector2)  # Calculează deviația standard a vectorului 2
  
  correlation_coefficient <- covar / (sd1 * sd2)  # Calculează coeficientul de corelație liniară
  
  return(correlation_coefficient)
}


verificare_functii <- function(){
  r<-c(1:10)
  print("Pentru vectorul dat, avem :")
  
  cat("Suma calculata cu functia noastra:", suma(r), "\n")
  cat("Suma calculata cu functia predefinita:", sum(r),"\n")
  cat("Media calculata cu functia noastra:", medie(r), "\n")
  cat("Media calculata cu functia predefinita:", mean(r),"\n")
  cat("Deviatia standard calculata cu functia noastra:", abatere_standard(r), "\n")
  cat("Deviatia standard calculata cu functia predefinita:", sd(r),"\n")
  cat("Testul_student calculata cu functia noastra:", test_student(r,4), "\n")
  print("Testul calculat cu functia predefinita:")
  print(t.test(r,mu=4))
  vector1 <- c(1, 2, 3, 4, 5)
  vector2 <- c(2, 4, 6, 8, 10)
  cat("Covariatia calculata cu functia noastra:", covariatie(vector1,vector2), "\n")
  cat("Covariata calculata cu functia predefinita:", cov(vector1,vector2),"\n")
  cat("Corelatia calculata cu functia noastra:", correlation_coefficient(vector1,vector2), "\n")
  cat("Corelatia calculata cu functia predefinita:", cor(vector1,vector2),"\n")
}

#Problema 1 c)
#solutia 1 calcul:
sol1 = function(x)
{
  vanzari=matrix(x, nrow=12, ncol=4)
  medii <-colMeans(vanzari)
  return (cbind(c("Magazin 1", "Magazin 2", "Magazin 3", "Magazin 4"), medii))
}

#solutia 2 calcul:
sol2 =function(x)
{
  vanzari=matrix(x,nrow=12,ncol=4)
  medii <- apply(vanzari, 2, mean)
  return (medii)
}

#Problema 1 d)
#utilizare merge, cbind, innerjoin
pr1_d <- function(){
  drivers_teams<-data.frame(Name=c("Hamilton","Verstappen","Leclerc",
                                "Alonso","Norris"),Team=c("Mercedes","Red Bull","Ferrari",
                                                          "Aston Martin","McLaren"))
  drivers_country<-data.frame(Name=c("Hamilton","Verstappen","Leclerc",
                                     "Alonso","Norris","Stroll"), Country=c("England","Netherland","Monaco","Spain","England","Canada"))
  drivers_details<-merge(drivers_country,drivers_teams,by="Name")
  drivers_details
  drivers_age<-c(38,24,25,41,22)
  drivers_details_with_age<-cbind(drivers_details,"Age"=drivers_age)
  drivers_details_with_age
  driver_stats <- data.frame(Name = c("Hamilton", "Verstappen", "Leclerc", "Alonso", "Norris"),
                             Wins = c(100, 15, 5, 32, 3),
                             Races = c(250, 100, 75, 200, 50),
                             Championships = c(7, 0, 0, 2, 0),
                             Years_in_F1 = c(15, 6, 4, 12, 3))
  
  # Inner join
  result <- inner_join(drivers_details_with_age, driver_stats, by = "Name")
  

  result
}

#Problema 1 e)

eurostat_data<-function()
{
  dataset <- get_eurostat("nama_10_gdp",filters=list(na_item="B1GQ",unit="CLV_I15",freq="A"))
  gdp_countries <- subset(dataset, select = -c(na_item, unit,freq))
  gdp_countries$time <- substr(gdp_countries$time, 1, 4) #converting from yyyy-mm-dd to year only
  unique_geo <- unique(gdp_countries$geo)
  unique_geo
  gdp_countries <- subset(gdp_countries, !(geo %in% c("EU27_2020", "EU28", "EU15", "EA", "EA20", "EA19", "EA12")))
  tidy_data <- gdp_countries %>%
    pivot_longer(cols = -c(time, geo), names_to = "variable", values_to = "value") %>%
    filter(!is.na(value)) %>%
    group_by(time) %>%
    summarize(average_value = mean(value))
  
  tidy_data
  
}

#Problema 1 f)

HTML_data<-function(){
  url <- "https://en.wikipedia.org/wiki/Lists_of_earthquakes"
  html <- read_html(url)
  tables <- html %>% html_table()
  tables
  earthquakes_table<-tables[[4]]
  earthquakes_table
  tidy_data <- as_tibble(earthquakes_table)

  
  column_names <- colnames(tidy_data)
  column_names
  
  frequency_table <- tidy_data %>%
    filter(!is.na(as.numeric(gsub("[^0-9.]", "", `Depth (km)`)))) %>%
    mutate(Depth = as.numeric(gsub("[^0-9.]", "", `Depth (km)`))) %>%
    mutate(Depth_Category = case_when(
      Depth < 10 ~ "0-10km",
      Depth < 20 & Depth >= 10 ~ "10-20km",
      Depth < 30 & Depth >= 20 ~ "20-30km",
      Depth >= 30 & Depth <= 40 ~ "30-40 km",
      Depth > 40 ~ "40+ km",
    )) %>%
    group_by(Depth_Category) %>%
    summarize(Frequency = n()) %>%
    arrange(Depth_Category)
  
  print(frequency_table)
  
  
  frequency_table
  
  tidy_data$`Depth (km)` <- as.numeric(tidy_data$`Depth (km)`)
 
  tidy_data <- tidy_data %>%
    mutate(Magnitude = ifelse(is.na(as.numeric(Magnitude)), NA, as.numeric(Magnitude)))
  ggplot(tidy_data, aes(x = Magnitude)) +
    geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black") +
    labs(x = "Magnitude", y = "Frequency", title = "Histogram of Earthquake Magnitudes")

  regression_model <- lm(Magnitude ~ `Depth (km)`, data = tidy_data)
  

  summary(regression_model)
}

RMSE <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# Function to calculate Mean Absolute Percentage Error (MAPE)
MAPE <- function(actual, predicted) {
  mape <- mean(abs((actual - predicted) / pmax(actual, 1))) * 100
  ifelse(is.finite(mape), mape, NA)
}
#Problema 2
#citim fisierul
chess_players <- read.csv("C:/Users/Raul/Downloads/top_chess_players_aug_2020.csv/top_chess_players_aug_2020.csv")

# Verificați dacă există valori lipsă pe coloane
missing_values <- colSums(is.na(chess_players))
print(missing_values)

# Identificați locația valorilor lipsă
missing_rows <- which(is.na(chess_players$Variable))
print(missing_rows)

str(chess_players)

#transformam vectorii aferenti variabilelor categoriale in variabile de tip factor
chess_players$Federation <-as.factor(chess_players$Federation)
chess_players$Gender <-as.factor(chess_players$Gender)
chess_players$Title <-as.factor(chess_players$Title)
#dam nume mai sugestive coloanelor
names(chess_players)[5] <- "Year"
names(chess_players)[7] <- "Classic"
names(chess_players)[8] <- "Rapid"
names(chess_players)[9] <- "Blitz"
names(chess_players)[10]<-"Active"

#modificam valorile din coloana Active
chess_players$Active <- ifelse(chess_players$Active == "", "Yes", "No")


unique(chess_players$Active)
chess_players$Active <-as.factor(chess_players$Active)
str(chess_players)

#verificam outlieri

boxplot(chess_players$Classic, main = "Boxplot - Classic")
boxplot(chess_players$Rapid, main = "Boxplot - Rapid")
boxplot(chess_players$Blitz, main = "Boxplot - Blitz")


mean_classic <- mean(chess_players$Classic)
sd_classic <- sd(chess_players$Classic)
lower_limit_classic <- mean_classic - 3 * sd_classic
upper_limit_classic <- mean_classic + 3 * sd_classic

outliers_classic <- chess_players$Classic < lower_limit_classic | chess_players$Classic > upper_limit_classic
outliers_count_classic <- sum(outliers_classic)
print(paste("Number of outliers in Classic:", outliers_count_classic))

# calculam ratingul mediu pentru fiecare federatie
mean_Blitz_rating <- tapply(chess_players$Blitz, chess_players$Federation, function(x) mean(x, na.rm = TRUE))
print(mean_Blitz_rating)

# calculam ratingul mediu la rapid pentru titlu si gen
mean_rapid_rating <- aggregate(chess_players$Rapid, by = list(chess_players$Title, chess_players$Gender), FUN = function(x) mean(x, na.rm = TRUE))
print(mean_rapid_rating)

# Filtram jucatorii masculi din USA cu rating classic peste 2700
us_players <- chess_players %>%
  filter(Federation == "USA" & Classic >= 2700 & Gender == "M") %>%
  select(Name, Federation, Classic)
print(us_players)

# Adaugam o noua coloana care sa reprezinte varsta
chess_players %>%
  mutate(age = 2023 - Year) %>%
  select(Name, age)

# filtram jucatorii activi din cele 3 federatii principale (USA, CHINA RUSIA)
# Ii aranjam dupa ratingul rapid si selectam anumite coloane
chess_players %>%
  filter(Active == "Yes" & Gender == "F" & Federation %in% c("RUS", "CHN", "USA")) %>%
  arrange(desc(Rapid)) %>%
  select(Name, Rapid, Title, Federation)


#folosirea a 4 functii din pachetul string

extracted_names <- filter(chess_players, str_detect(Name, "^Z.*[A-Za-z]$"))

grandmasters <- str_subset(chess_players$Title, "GM")

replaced_names <- str_replace_all(chess_players$Name, "[^A-Za-z0-9]", " ")

extracted_ids <- str_extract(chess_players$Fide.id, "\\d+")

#distributia jucatorilor din 4 federatii selectate
chess_players %>%
  filter(Federation %in% c("USA", "RUS", "CHN", "ROU")) %>%
  na.omit() %>%
  ggplot(aes(x = Rapid, fill = Federation)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribution of Rapid Ratings by Federation",
       x = "Rating",
       y = "Density")

# media si deviatia standard pentru ratingul clasic
chess_players %>%
  summarise(mean_classic = mean(Classic, na.rm = TRUE),
            sd_classic = sd(Classic, na.rm = TRUE))

#corelatia dintre rapid si blitz
chess_players %>%
  filter(complete.cases(Rapid, Blitz)) %>%
  summarise(correlation = cor(Rapid, Blitz))



#ratingul mediu dupa titlu
chess_players %>%
  group_by(Title) %>%
  summarise(mean_rapid_rating = mean(Rapid, na.rm = TRUE))



#anova 
anova_result <- aov(Classic ~ Gender, data = chess_players)
summary(anova_result)


#chi_square
freq_table <- table(chess_players$Federation, chess_players$Gender)
chi_square_result <- chisq.test(freq_table)
chi_square_result



# Selectarea variabilelor de interes și crearea setului de date
data <- chess_players %>%
  select(Classic, Federation, Gender) %>%
  na.omit()

# Divizarea datelor în setul de antrenament și setul de test (80% și 20%)
set.seed(123) # Asigurăm reproducibilitatea
trainData <- data %>%
  sample_frac(0.8)
testData <- data %>%
  anti_join(trainData)

# Model 1: Regresie liniară multiplă
model_lm <- lm(Classic ~ Federation + Gender, data = trainData)

# Model 2: Decision Tree
model_tree <- rpart(Classic ~ Federation + Gender, data = trainData)

# Evaluam performanta modelelor
trainData$lm_predictions <- predict(model_lm, newdata = trainData)
trainData$tree_predictions <- predict(model_tree, newdata = trainData)

# Calculam rmse si mape pentru cele 2 modele
rmse_lm <- RMSE(trainData$Classic, trainData$lm_predictions)
rmse_tree <- RMSE(trainData$Classic, trainData$tree_predictions)

mape_lm <- MAPE(trainData$Classic, trainData$lm_predictions)
mape_tree <- MAPE(trainData$Classic, trainData$tree_predictions)

# Comparam performanta celor 2 modele
performance <- data.frame(Model = c("Multiple Linear Regression", "Decision Tree"),
                          RMSE = c(rmse_lm, rmse_tree),
                          MAPE = c(mape_lm, mape_tree))
print(performance)

# Salvam predictiile in setul de date
testData$lm_predictions <- predict(model_lm, newdata = testData)
testData$tree_predictions <- predict(model_tree, newdata = testData)

testData$lm_predictions
testData$tree_predictions

# Analizam eroarea predictiilor
lm_residuals <- testData$Classic - testData$lm_predictions
tree_residuals <- testData$Classic - testData$tree_predictions
lm_residuals
tree_residuals

# Aratam predictiile grafic
lm_residuals_df <- data.frame(Predicted = testData$lm_predictions, Residuals = lm_residuals)
lm_residuals_plot <- ggplot(lm_residuals_df, aes(x = Predicted, y = Residuals)) +
  geom_point() +
  labs(x = "Predicted", y = "Residuals", title = "Residual Plot - Multiple Linear Regression")
print(lm_residuals_plot)


tree_residuals_df <- data.frame(Predicted = testData$tree_predictions, Residuals = tree_residuals)
tree_residuals_plot <- ggplot(tree_residuals_df, aes(x = Predicted, y = Residuals)) +
  geom_point() +
  labs(x = "Predicted", y = "Residuals", title = "Residual Plot - Decision Tree")
print(tree_residuals_plot)





# filtram datele pentru a creea un model valid, eliminand valorile nule
filtered_data <- chess_players[complete.cases(chess_players$Classic), ]

# amestecam setul de date
shuffled_data <- filtered_data[sample(nrow(filtered_data)), ]


set.seed(123)

#aici vom stoca cele 200 de rmse
rmse_values <- vector("numeric", 200)

#impartim setul de date in 200 esantioane
data_parts <- split(shuffled_data, rep(1:200, each = ceiling(nrow(shuffled_data) / 200), length.out = nrow(shuffled_data)))

# parcurgem cele 200 de esantioane, impartind in 2 seturi 80% 20% training si test
for (i in 1:200) {
  train_indices <- sample(1:nrow(data_parts[[i]]), 0.8 * nrow(data_parts[[i]]))
  trainData <- data_parts[[i]][train_indices, ]
  testData <- data_parts[[i]][-train_indices, ]
  
  # modelul de regresie liniara pentru classic gender
  model_lm <- lm(Classic ~ Gender, data = trainData)
  
  # predictiile 
  testData$lm_predictions <- predict(model_lm, newdata = testData)
  
  #RMSE
  rmse <- sqrt(mean((testData$Classic - testData$lm_predictions)^2))
  rmse_values[i] <- rmse
}
#vizualizam rmse-urile
rmse_values

# Histograma pentru distributia de RMSE 
ggplot(data = data.frame(RMSE = rmse_values), aes(x = RMSE)) +
  geom_histogram(binwidth = 50, fill = "lightblue", color = "black") +
  labs(title = "Distribution of RMSE", x = "RMSE", y = "Frequency")

#Statistica descriptiva pentru RMSE
summary(rmse_values)

