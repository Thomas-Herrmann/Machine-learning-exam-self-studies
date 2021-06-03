import numpy as np
import matplotlib as mpl
import pandas as pd
from numpy.random import multivariate_normal
import matplotlib.pyplot as plt
from sklearn.naive_bayes import GaussianNB
from sklearn.discriminant_analysis import LinearDiscriminantAnalysis
from sklearn.linear_model import LogisticRegression
from sklearn.svm import SVC
from sklearn import mixture
from sklearn.model_selection import train_test_split
from sklearn.datasets import make_moons,make_blobs
from sklearn.metrics import accuracy_score
from sklearn.datasets import load_boston
from sklearn.model_selection import GridSearchCV
from sklearn.preprocessing import StandardScaler


bostondata = load_boston() 


def sample_2d_gaussian(meanx,meany,variance_x,variance_y,covariance,numsamps):
    '''
    Generates a random sample of size 'numsamps' from a 2-dimensional Gaussian distribution.
    The Gaussian is defined by the mean vector (meanx,meany) and the 
    covariance matrix
    
    variance_x    covariance
    covariance    variance_y
    
    All parameters can be freely chosen, except covariance, which must satisfy the inequality
    
    covariance <= sqrt(variance_x * variance_y)
    '''
    meanvec = np.array([meanx,meany])
    covarmatrix = np.array([[variance_x,covariance],[covariance,variance_y]])

    return multivariate_normal(meanvec,covarmatrix,numsamps)


def maxpos(A):
    '''
    Takes an n x k array A, and returns 1-dim n array where the i'th
    entry is the index of column in A where the i'th row of A has its
    maximal value (application: turns a probabilitiy distribution over
    k classes for n instances into a single prediction)
    '''

    return np.argmax(A,axis=1)


midata = pd.read_csv("MI-labeled.txt", sep=',')
featuresmi = np.array(midata[['X1','X2']])
classlabels=midata['Class']
labelsmi = np.zeros(len(classlabels))
for i in range(len(classlabels)):
    if classlabels[i] == 'I':
        labelsmi[i]=1


np.random.seed(55)
datasize=250
mixturecoeff=np.array([0.4,0.2,0.4])
componentsizes=(datasize*mixturecoeff).astype(int)

#meanx,meany,variance_x,variance_y,covariance,numsamps
class0samp=sample_2d_gaussian(8,8,3.0,3.0,0.45,componentsizes[0])
class1samp=sample_2d_gaussian(5,3,0.1,0.1,-0.45,componentsizes[1])
class2samp=sample_2d_gaussian(3,2,0.1,0.1,0,componentsizes[2])
  
featuresg=np.concatenate((class0samp,class1samp,class2samp),axis=0)
labelsg=np.concatenate((np.zeros(componentsizes[0]),np.ones(componentsizes[1]),2*np.ones(componentsizes[2])))  

class0samp=sample_2d_gaussian(8,8,3.0,3.0,0.45,componentsizes[0])
class1samp=sample_2d_gaussian(5,3,0.1,0.1,-0.45,componentsizes[1])
class2samp=sample_2d_gaussian(3,2,0.1,0.1,0,componentsizes[2])
  
featuresgtest=np.concatenate((class0samp,class1samp,class2samp),axis=0)
labelsgtest=np.concatenate((np.zeros(componentsizes[0]),np.ones(componentsizes[1]),2*np.ones(componentsizes[2]))) 

mixturecoeff=np.array([0.5,0.5])
componentsizes=(datasize*mixturecoeff).astype(int)
class0samp=sample_2d_gaussian(8,8,3.0,3.0,0.45,componentsizes[0])
class1samp=sample_2d_gaussian(3,2,0.1,0.1,0,componentsizes[0])
  
featuresgsepa=np.concatenate((class0samp,class1samp),axis=0)
labelsgsepa=np.concatenate((np.zeros(componentsizes[0]),np.ones(componentsizes[1]))) 


datapairs = {
    "mi": [featuresmi, labelsmi], 
    "gd": [featuresg, labelsg],
    "gd lin sepa": [featuresgsepa, labelsgsepa]
    }

for dtype, (features, labels) in datapairs.items():

    maxvalx = np.max(features[:,0])
    maxvaly = np.max(features[:,1])
    minvalx = np.min(features[:,0])
    minvaly = np.min(features[:,1])
    border=2
    xinterval = (maxvalx-minvalx)/border
    yinterval = (maxvaly-minvaly)/border
    xx, yy = np.meshgrid(np.arange(minvalx-xinterval, maxvalx+xinterval, xinterval/100), np.arange(minvaly-yinterval, maxvaly+yinterval, yinterval/100))

    models = {
        "SVC": SVC(kernel='linear'),
        "GNB": GaussianNB(),
        "LDA": LinearDiscriminantAnalysis(),
        "LRN": LogisticRegression()
             }

    fig, axes = plt.subplots(2, 2)
    fig.suptitle(f"{dtype} - Model decision regions")

    plotIndex = 0

    for mtype, model in models.items():
        model.fit(features, labels)

        if mtype == "SVC":
            Z=model.decision_function(np.c_[xx.ravel(), yy.ravel()])
        else:
            Z=model.predict_proba(np.c_[xx.ravel(), yy.ravel()])

        if len(Z.shape)>1:
            meshclasses = maxpos(Z)
        else:
            meshclasses = np.ones(Z.size)
            meshclasses[Z<0]=0

        meshclasses = meshclasses.reshape(xx.shape)

        axes[plotIndex // 2, plotIndex % 2].contourf(xx,yy,meshclasses,[-0.1,0.1,1,2],colors=('tomato','lightblue','lightgreen'))
        axes[plotIndex // 2, plotIndex % 2].scatter(features[:,0],features[:,1],c=labels, cmap = mpl.colors.ListedColormap(['r', 'b','g']))
        
        pred_labels=model.predict(features)

        if dtype == "gd": 
            pred_test_labels=model.predict(featuresgtest)
            axes[plotIndex // 2, plotIndex % 2].set_title("{}: AccTrain={}, AccTest={}".format(mtype, round(accuracy_score(labels,pred_labels), 4), round(accuracy_score(labelsgtest,pred_test_labels), 4)))
        else:
            axes[plotIndex // 2, plotIndex % 2].set_title("{}: Acc={}".format(mtype, round(accuracy_score(labels,pred_labels), 4)))

        plotIndex += 1


    plt.show()

# Boston data:

np.random.seed(13)
labelsb = np.array([1 if y > np.median(bostondata['target']) else 0 for y in bostondata['target']])
featuresb = bostondata['data']
featuresb_train,featuresb_test,labelsb_train,labelsb_test = train_test_split(featuresb,labelsb) # train 0.67 ~ 0.33 test 

scaler = StandardScaler() # Normalize to reduce variance; We do not want one feature to dominate another
scaler.fit(featuresb_train)
features_train_norm=scaler.transform(featuresb_train)
features_test_norm=scaler.transform(featuresb_test)

models = {
    "SVC": [SVC(kernel='linear'), ["coef_"]],
    "GNB": [GaussianNB(), ["theta_", "sigma_"]],
    "LDA": [LinearDiscriminantAnalysis(), ["means_"]],
    "LRN": [LogisticRegression(), ["coef_"]]
    }

for mtype, (model, attr) in models.items():
    model.fit(features_train_norm,labelsb_train)
    pred_labels_train=model.predict(features_train_norm)
    pred_labels_test=model.predict(features_test_norm)

    print(f"{mtype}:")
    print("   Accuracy train: {}".format(accuracy_score(labelsb_train,pred_labels_train)))
    print("   Accuracy test: {}".format(accuracy_score(labelsb_test,pred_labels_test)))
    print("   Attributes:")

    for attribute in attr:
        print(f"      {attribute}: {getattr(model, attribute)}")

    print("")