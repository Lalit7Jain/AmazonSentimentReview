#python -m idlelib.idle
import pickle

## loading the data
w1=open("C:\\Users\\lalit\\Documents\\val", 'rb') #Open the file
targets =pickle.load(w1) #Assign the recreated object to bok	
print "Loaded the targets"

w2=open("C:\\Users\\lalit\\Documents\\data", 'rb') #Open the file
dataset = pickle.load(w2) #Assign the recreated object to bok
print "Loaded the dataset"


from sklearn.cross_validation import train_test_split
from sklearn.metrics import classification_report
from nolearn.dbn import DBN
import numpy as np
#import cv2

(trainX, testX, trainY, testY) = train_test_split(dataset, targets, test_size = 0.30)
X = np.array(trainX)
text_X = np.array(testX)

# train the Deep Belief Network with number of columns of all reviews (800) in our case (len(trainx[1]), 400 hidden units, 2 output units 
dbn = DBN(
	[X.shape[1], 400, 2],
	learn_rates = 0.01,
	learn_rate_decays = 0.2,
	epochs = 12,
	verbose = 1)
dbn.fit(X, trainY)

# compute the predictions for the test data and show a classification
# report
preds = dbn.predict(text_X)
print classification_report(testY, preds)