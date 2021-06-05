import matplotlib.pyplot as plt
import numpy as np
import time
from sklearn.neural_network import MLPClassifier
from sklearn.svm import SVC
from sklearn.model_selection import train_test_split
from sklearn.metrics import confusion_matrix,accuracy_score
from sklearn.datasets import fetch_openml
from sklearn.model_selection import GridSearchCV
from sklearn.metrics import classification_report
import pandas as ps


mnist                            = fetch_openml(name='mnist_784', data_home='data')
classes                          = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]
X_train, X_test, y_train, y_test = train_test_split(mnist.data, mnist.target, test_size=0.3)


def sim_ker(X, Y):
    return np.dot(X, Y.T)


model2 = SVC(kernel=sim_ker)

print("begin training custom")

model2.fit(X_train, y_train)

print("begin predictions")

y_pred2 = model2.predict(X_test)

print(classification_report(y_test, y_pred2, target_names=classes))


kernels = {'kernel':('linear', 'poly', 'rbf', 'sigmoid')}
model   = SVC(kernel='poly') # GridSearchCV(SVC(), kernels, verbose=10, cv=2, n_jobs = 8, return_train_score = True)

print("begin training poly")

model.fit(X_train, y_train)

y_pred = model.predict(X_test)

print(classification_report(y_test, y_pred, target_names=classes))



#digit0='3'
#digit1='7'
#mnist_bin_data=mnist.data[np.logical_or(mnist.target==digit0,mnist.target==digit1)]
#mnist_bin_target=mnist.target[np.logical_or(mnist.target==digit0,mnist.target==digit1)]
#
#plt.imshow(mnist_bin_data.iloc[[0]].values.reshape(28,28),cmap=plt.cm.gray_r)
#plt.show()

