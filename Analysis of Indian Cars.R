library(tidyverse)
library(ggplot2)
library(dplyr)

# Loading the dataset
data <- read.csv("C:/Users/akulg/Downloads/Indian Cars/cars_ds_final_2021.csv")

# Analysis 1: Distribution of car manufacturers
manufacturer_count <- data %>%
  group_by(Make) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Plot 1: Bar chart of car manufacturers
ggplot(manufacturer_count, aes(x = reorder(Make, -count), y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Distribution of Car Manufacturers", x = "Manufacturer", y = "Number of Cars") +
  theme_minimal()

# Analysis 2: Distribution of fuel types
fuel_count <- data %>%
  group_by(Fuel_Type) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Plot 2: Pie chart of fuel types
ggplot(fuel_count, aes(x = "", y = count, fill = Fuel_Type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Fuel Type Distribution", x = NULL, y = NULL) +
  theme_void()

# Analysis 3: Price range by manufacturer
price_range <- data %>%
  mutate(Price = as.numeric(gsub("[^0-9]", "", Ex.Showroom_Price))) %>%
  group_by(Make) %>%
  summarise(Min_Price = min(Price, na.rm = TRUE), Max_Price = max(Price, na.rm = TRUE))

# Plot 3: Box plot of prices by manufacturer
data %>%
  mutate(Price = as.numeric(gsub("[^0-9]", "", Ex.Showroom_Price))) %>%
  ggplot(aes(x = Make, y = Price)) +
  geom_boxplot(fill = "lightblue") +
  coord_flip() +
  labs(title = "Price Range by Manufacturer", x = "Manufacturer", y = "Price") +
  theme_minimal()

# Analysis 4: Engine displacement distribution
engine_displacement <- data %>%
  mutate(Displacement = as.numeric(gsub("[^0-9]", "", Displacement)))

# Plot 4: Histogram of engine displacement
ggplot(engine_displacement, aes(x = Displacement)) +
  geom_histogram(binwidth = 100, fill = "orange", color = "black") +
  labs(title = "Engine Displacement Distribution", x = "Displacement (cc)", y = "Frequency") +
  theme_minimal()

# Analysis 5: Mileage comparisons
mileage <- data %>%
  select(City_Mileage, Highway_Mileage) %>%
  mutate(City_Mileage = as.numeric(gsub("[^0-9.]", "", City_Mileage)),
         Highway_Mileage = as.numeric(gsub("[^0-9.]", "", Highway_Mileage)))

# Plot 5: Scatter plot of city vs highway mileage
ggplot(mileage, aes(x = City_Mileage, y = Highway_Mileage)) +
  geom_point(color = "darkgreen", alpha = 0.6) +
  labs(title = "City vs Highway Mileage", x = "City Mileage (kmpl)", y = "Highway Mileage (kmpl)") +
  theme_minimal()

# Analysis 6: Power and torque relationships
power_torque <- data %>%
  mutate(Power = as.numeric(gsub("[^0-9.]", "", Power)),
         Torque = as.numeric(gsub("[^0-9.]", "", Torque)))

# Plot 6: Scatter plot of power vs torque
ggplot(power_torque, aes(x = Power, y = Torque)) +
  geom_point(color = "purple", alpha = 0.6) +
  labs(title = "Power vs Torque Relationship", x = "Power (bhp)", y = "Torque (Nm)") +
  theme_minimal()

# Analysis 7: Number of cylinders distribution
cylinder_count <- data %>%
  group_by(Cylinders) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Plot 7: Bar chart of number of cylinders
ggplot(cylinder_count, aes(x = Cylinders, y = count)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Number of Cylinders Distribution", x = "Cylinders", y = "Count") +
  theme_minimal()

# Analysis 8: Drive type comparisons
drive_type <- data %>%
  group_by(Drivetrain) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Plot 8: Bar chart of drive types
ggplot(drive_type, aes(x = reorder(Drivetrain, -count), y = count)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  coord_flip() +
  labs(title = "Drive Type Distribution", x = "Drivetrain", y = "Count") +
  theme_minimal()

# Analysis 9: Body type distribution
body_type <- data %>%
  group_by(Body_Type) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Plot 9: Bar chart of body types
ggplot(body_type, aes(x = reorder(Body_Type, -count), y = count)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  coord_flip() +
  labs(title = "Body Type Distribution", x = "Body Type", y = "Count") +
  theme_minimal()

# Analysis 10: Electric car analysis (Battery capacity, range)
electric_cars <- data %>%
  filter(!is.na(Electric_Range)) %>%
  mutate(Electric_Range = as.numeric(Electric_Range))

# Plot 10: Histogram of electric car ranges
ggplot(electric_cars, aes(x = Electric_Range)) +
  geom_histogram(binwidth = 50, fill = "lightgreen", color = "black") +
  labs(title = "Electric Car Range Distribution", x = "Range (km)", y = "Frequency") +
  theme_minimal()

# Analysis 11: Safety features distribution (e.g., Airbags, ABS)
safety_features <- data %>%
  select(Airbags, ABS..Anti.lock_Braking_System.) %>%
  mutate(Airbags = as.numeric(gsub("[^0-9]", "", Airbags)))

# Plot 11: Bar chart of safety features
ggplot(safety_features, aes(x = Airbags)) +
  geom_bar(fill = "cyan") +
  labs(title = "Airbags Distribution", x = "Number of Airbags", y = "Count") +
  theme_minimal()

# Analysis 12: Car length distribution
car_length <- data %>%
  mutate(Length = as.numeric(gsub("[^0-9.]", "", Length)))

# Plot 12: Histogram of car lengths
ggplot(car_length, aes(x = Length)) +
  geom_histogram(binwidth = 100, fill = "pink", color = "black") +
  labs(title = "Car Length Distribution", x = "Length (mm)", y = "Frequency") +
  theme_minimal()

# Analysis 13: Seating capacity distribution
seating_capacity <- data %>%
  group_by(Seating_Capacity) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Plot 13: Bar chart of seating capacities
ggplot(seating_capacity, aes(x = Seating_Capacity, y = count)) +
  geom_bar(stat = "identity", fill = "brown") +
  labs(title = "Seating Capacity Distribution", x = "Seating Capacity", y = "Count") +
  theme_minimal()

# Analysis 14: Gearbox type distribution
gearbox <- data %>%
  group_by(Gearbox) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Plot 14: Bar chart of gearbox types
ggplot(gearbox, aes(x = reorder(Gearbox, -count), y = count)) +
  geom_bar(stat = "identity", fill = "gold") +
  labs(title = "Gearbox Type Distribution", x = "Gearbox Type", y = "Count") +
  theme_minimal()

