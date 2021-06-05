import numpy as np
import matplotlib as mpl
import matplotlib.pyplot as plt
from sklearn.datasets import load_iris
from sklearn.neighbors import KNeighborsClassifier
from sklearn.linear_model import LinearRegression
from sklearn.model_selection import train_test_split
from sklearn import metrics

def maxpos(A):
    '''
    Takes an n x k array A, and returns 1-dim n array where the i'th
    entry is the index of column in A where the i'th row of A has its
    maximal value (application: turns a probabilitiy distribution over
    k classes for n instances into a single prediction)
    '''
    return np.argmax(A,axis=1)


irisdata = load_iris()

feat1 = 1
feat2 = 2

X_train,X_test,y_train,y_test=train_test_split(irisdata.data,irisdata.target,test_size=0.3)

onehotclasses = np.zeros((y_train.size,3))
for i in range(y_train.size): 
    onehotclasses[i][y_train[i]]=1.0


onehotclassestest = np.zeros((y_test.size,3))
for i in range(y_test.size): 
    onehotclassestest[i][y_test[i]]=1.0


index = 0
fig = plt.figure()

for i in range(0, 4):
    for j in range(i + 1, 4):
        
        index += 1

        feat1 = i
        feat2 = j

        print(f"training model {index} out of {6}")

        maxval1 = np.max(X_train[:,feat1])
        maxval2 = np.max(X_train[:,feat2])

        xx, yy = np.meshgrid(np.arange(0, maxval1+1, 0.02), np.arange(0, maxval2+1, 0.02))

        irislinreg = LinearRegression()
        irislinreg.fit(X_train[:,[feat1,feat2]],onehotclasses)
        Z = irislinreg.predict(np.c_[xx.ravel(), yy.ravel()])
        meshclasses = maxpos(Z)

        predtrain = maxpos(irislinreg.predict(X_train[:,[feat1,feat2]]))
        predtest  = maxpos(irislinreg.predict(X_test[:,[feat1,feat2]]))

        ax = fig.add_subplot(2, 3, index)
        ax.title.set_text(f"acc(train)={round(metrics.accuracy_score(y_train,predtrain), 4)}, acc(test)={round(metrics.accuracy_score(y_test,predtest), 4)}")

        ax.set_xlabel(irisdata['feature_names'][feat1])
        ax.set_ylabel(irisdata['feature_names'][feat2])

        meshclasses = meshclasses.reshape(xx.shape)
        ax.contourf(xx,yy,meshclasses,[-0.1,0.1,1,2],colors=('tomato','lightblue','lightgreen'))
        ax.scatter(X_test[:,feat1],X_test[:,feat2],c=y_test, cmap = mpl.colors.ListedColormap(['r', 'b', 'g']))

plt.show()

fig = plt.figure()

for i in range(0, 3):
    accsTrain = []
    accsTest  = []
    ks        = []

    for j in range(0, 15):
        feat1 = i
        feat2 = i + 1

        maxval1 = np.max(X_train[:,feat1])
        maxval2 = np.max(X_train[:,feat2])

        xx, yy = np.meshgrid(np.arange(0, maxval1+1, 0.02), np.arange(0, maxval2+1, 0.02))

        irisknn = KNeighborsClassifier(n_neighbors=j+1).fit(X_train[:,[feat1,feat2]],y_train)

        predtrain = irisknn.predict(X_train[:,[feat1,feat2]])
        predtest  = irisknn.predict(X_test[:,[feat1,feat2]])

        accTrain = metrics.accuracy_score(y_train,predtrain)
        accTest  = metrics.accuracy_score(y_test,predtest)

        accsTrain.append(accTrain)
        accsTest.append(accTest)
        ks.append(j+1)

        if j % 3 == 0:
            index = i * 6 + j // 3 + 1

            Z = irisknn.predict(np.c_[xx.ravel(), yy.ravel()])
            meshclasses = Z

            ax = fig.add_subplot(3, 6, index)
            ax.title.set_text(f"k={j+1}, acc(train)={round(accTrain, 4)}, acc(test)={round(accTest, 4)}")

            ax.set_xlabel(irisdata['feature_names'][feat1])
            ax.set_ylabel(irisdata['feature_names'][feat2])

            meshclasses = meshclasses.reshape(xx.shape)
            ax.contourf(xx,yy,meshclasses,[-0.1,0.1,1,2],colors=('tomato','lightblue','lightgreen'))
            ax.scatter(X_test[:,feat1],X_test[:,feat2],c=y_test, cmap = mpl.colors.ListedColormap(['r', 'b', 'g']))
    
    axp = fig.add_subplot(3, 6, (i + 1) * 6)
    axp.title.set_text(f"accuracy as function of k")

    axp.set_xlabel("k")
    axp.set_ylabel("acc")

    axp.plot(ks, accsTrain, c='r', label="train")
    axp.plot(ks, accsTest, c='b', label="test")
    axp.legend(loc="upper left")

plt.show()

#
#
