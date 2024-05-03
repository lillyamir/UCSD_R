install.packages("keras")
library(keras)
install_keras()


encoder_input <- layer_input(shape = c(28 * 28))
encoded <- encoder_input %>%
  layer_dense(units = 256, activation = 'relu') %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dense(units = 2) # Latent space

encoder_model <- keras_model(inputs = encoder_input, outputs = encoded)

decoder_input <- layer_input(shape = c(2))
decoded <- decoder_input %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dense(units = 256, activation = 'relu') %>%
  layer_dense(units = 28 * 28, activation = 'sigmoid')

decoder_model <- keras_model(inputs = decoder_input, outputs = decoded)


discriminator_input <- layer_input(shape = c(2))
discriminated <- discriminator_input %>%
  layer_dense(units = 256, activation = 'relu') %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dense(units = 1, activation = 'sigmoid')

discriminator_model <- keras_model(inputs = discriminator_input, outputs = discriminated)
discriminator_model %>% compile(
  optimizer = optimizer_adam(),
  loss = 'binary_crossentropy',
  metrics = 'accuracy'
)


aae_input <- layer_input(shape = c(28 * 28))
encoded_output <- encoder_model(aae_input)
decoded_output <- decoder_model(encoded_output)

aae_model <- keras_model(inputs = aae_input, outputs = decoded_output)
aae_model %>% compile(
  optimizer = optimizer_adam(),
  loss = 'binary_crossentropy'
)

###Training the model###

simulate_data <- function(n_samples) {
  # Generating data from two different normal distributions
  set.seed(123)
  group1 <- matrix(rnorm(n_samples, mean = -2), ncol = 2)
  group2 <- matrix(rnorm(n_samples, mean = 2), ncol = 2)
  
  # Combine and shuffle
  data <- rbind(group1, group2)
  data <- data[sample(nrow(data)),]
  
  # Normalize data
  data <- scale(data)
  
  return(data)
}

# Create training and test datasets
train_data <- simulate_data(1000)
test_data <- simulate_data(500)



library(keras)
library(abind) # for abind

batch_size <- 50
latent_dim <- 2  # Adjust based on your actual model design

for(epoch in 1:50) {
  indices <- sample(1:nrow(train_data), size = batch_size)
  batch_data <- train_data[indices, ]
  
  # Ensure batch_data is correctly shaped; reshape only if using image-like data
  # batch_data <- array_reshape(batch_data, c(batch_size, 784))  # Uncomment this line if using 784-dimensional data
  
  # Train autoencoder
  loss_aae <- aae_model %>% train_on_batch(batch_data, batch_data)
  
  # Generate fake latent points
  latent_fake <- encoder_model %>% predict(batch_data)
  latent_real <- matrix(rnorm(latent_dim * batch_size), ncol = latent_dim)
  x_combined_batch <- abind(latent_fake, latent_real, along = 1)
  y_combined_batch <- c(rep(0, batch_size), rep(1, batch_size)) # 0: fake, 1: real
  
  # Train discriminator
  loss_d <- discriminator_model %>% train_on_batch(x_combined_batch, y_combined_batch)
}
