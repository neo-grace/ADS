install.packages("deepnet")
library(deepnet)

show.digit <- function(arr784, col=gray(12:1/12), ...) {
  image(matrix(arr784, nrow=28)[,28:1], col=col, ...)
}


dnn <- dbn.dnn.train(train.x, train.y, hidden = c(300,300), numepochs = 2, cd=2)
err.dnn <- nn.test(dnn, test.x, test.y)
err.dnn
yy.dnn <- nn.predict(dnn, test.x)
head(yy.dnn)
show_digit(test.y)
show.digit(yy.dnn)


install.packages("LeNet")
