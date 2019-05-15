##Baca data
data <- read.delim("D:/proyek ci/letter-recognition.data", sep=",", header=F)
colnames(data) <- c("letter","x-box","y-box","width","height","onpix","x-bar","y-bar","x2bar","y2bar","xybar"
                    ,"x2ybr","xy2br","x-edge","xegvy","y-edge","yegvx")
#1.	lettr	capital letter	(26 values from A to Z) 
#2.	x-box	horizontal position of box	(integer) 
#3.	y-box	vertical position of box	(integer) 
#4.	width	width of box	(integer) 
#5.	high height of box	(integer) 
#6.	onpix	total # on pixels	(integer) 
#7.	x-bar	mean x of on pixels in box	(integer) 
#8.	y-bar	mean y of on pixels in box	(integer) 
#9.	x2bar	mean x variance	(integer) 
#10.	y2bar	mean y variance	(integer) 
#11.	xybar	mean x y correlation	(integer) 
#12.	x2ybr	mean of x * x * y	(integer) 
#13.	xy2br	mean of x * y * y	(integer) 
#14.	x-ege	mean edge count left to right	(integer) 
#15.	xegvy	correlation of x-ege with y	(integer) 
#16.	y-ege	mean edge count bottom to top	(integer) 
#17.	yegvx	correlation of y-ege with x	(integer)
head(data)
summary(data$letter)


####################################

library(class)
library(caret)

index = createDataPartition(data$letter, p = .7, list = F)

trainData = data[index, ]
testData = data[-index, ]

#Convert train and test data into a matrix type and flag column to factor type.

train = data.matrix(trainData[, 2:16])
test = data.matrix(testData[, 2:16])

train_label = factor(trainData[, "letter"])
test_label = testData$letter

#####TRAINING

# ensure results are repeatable
set.seed(7)
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(letter~., data, method="lvq", trControl=control, tuneLength=5)
# summarize the model
print(model)

# Building a codebook

#lvqinit() initialize an LVQ codebook/ vektor pewakil

codeBook = lvqinit(train, train_label, size = 686, k = 6)
#lvqinit(x, cl, size, prior, k = 5)
# Arguments
# x	= a matrix or data frame of training examples, n by p.
# cl =	the classifications for the training examples. A vector or factor of length n.
# size =	the size of the codebook. Defaults to min(round(0.4*ng*(ng-1 + p/2),0), n) where ng is the number of classes.
# prior =	Probabilities to represent classes in the codebook. Default proportions in the training set.
# k =	k used for k-NN test of correct classification. Default is 5.
# codeBook$x

#olvq1() represents the training set in a codebook.

buildCodeBook = olvq1(train, train_label, codeBook, alpha = 0.1) #iterasi = 40 * nrow(codeBook$x)

#lvtest() classifies a test data with the above codebook.

predict = lvqtest(buildCodeBook, test)

confusionMatrix(test_label, predict)


plot(model)
