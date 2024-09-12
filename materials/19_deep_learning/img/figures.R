library(keras)


mnist <- dataset_mnist()

x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y

matrix.rotate <- function(img) { 
  t(apply(img, 2, rev))
}

png('materials/29_deep_learning/img/mnist_sample.png')
par(mfrow=c(3, 3))
for (idx in 1:9) {
  label <- y_train[idx]
  image(matrix.rotate(x_train[idx,,]), col = grey(level = seq(1, 0, by=-1/255)), axes=F, main=label)
}
dev.off()

kable(data.frame(x_train[1,,]))