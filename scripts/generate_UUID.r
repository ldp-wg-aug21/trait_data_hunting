## Generate UUID for all species 

library(uuid)

# Read the CIEE LPI dataset to get species names
ciee_lpi <- read.csv("data-raw/CIEE_LPI_dataset.csv")

# Generate randomly universally unique identifier (UUID) for all species
set.seed(1234)
sp_binomial <- unique(ciee_lpi$Binomial)
sp_uuid <- UUIDgenerate(n = length(sp_binomial))

sp_uuid_binom <- tibble(Binomial = sp_binomial, UUID = sp_uuid)


# Add UUID to existing datasets

left_join_overwrite <- function(dataset) {
    dataset_uuid <- dataset %>%
                    left_join(sp_uuid_binom, by = "Binomial") %>%
                    select(-UUID.x, -X) %>%
                    rename(UUID = UUID.y) %>%
                    relocate(UUID, .before = Binomial)
}

add_uuid <- function(path) {
    dataset <- read.csv(path)
    dataset_uuid <- left_join_overwrite(dataset) 
    write.csv(dataset_uuid, path)
}

add_uuid("data-clean/traits-specific-birds.csv")
add_uuid("data-clean/traits-specific-mammals.csv")

canada_lpi <- read.csv("data-clean/canadian_lpi_data_with_habitat.csv")
canada_lpi_uuid <- canada_lpi %>%
                    left_join(sp_uuid_binom, by = "Binomial") %>%
                    relocate(UUID, .before = Binomial)
write.csv(canada_lpi_uuid, "data-clean/canadian_lpi_data_with_habitat_uuid.csv")

