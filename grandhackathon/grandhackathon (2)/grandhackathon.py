from sklearn import  svm
from sklearn.metrics import confusion_matrix,accuracy_score
from sklearn.model_selection import train_test_split
from sklearn.svm import SVC
import pandas as pd
from sklearn import  svm
from sklearn.metrics import confusion_matrix,accuracy_score
from sklearn.model_selection import train_test_split
from sklearn.svm import SVC
import pandas as pd
bank= pd.read_csv("C:\\Users\\ACER\\Desktop\\cust_Bank .csv")
bank_df=pd.DataFrame(bank)
bank_df
bank_df.drop(['y'],axis=1,inplace=True)
bank_df
bank_dumm=pd.get_dummies(bank_df,prefix=["job","marital","education","housing","loan"])
bank_dumm
bank_dumm.drop(['job_student','marital_single','education_illiterate','housing_no','loan_no'],axis=1,inplace=True)
targets = bank['y']
targets = targets.replace({'no':0,'yes':1})
X_data = bank_dumm
Y = targets
Y
X_train, X_test, y_train, y_test = train_test_split(X_data, Y, test_size=0.15, random_state=42)

#clf = svm.SVC(C=60, gamma=.09, kernel='poly')
#clf = svm.SVC(C=50, gamma=.09, kernel='linear')
clf = svm.SVC(C=50, gamma=.09, kernel='rbf')
clf.fit(X_train, y_train)
ans = clf.predict(X_test)
ans
confusion_matrix(y_test, ans)
accuracy_score(y_test, ans)


#########################neuralnet

from sklearn.neural_network import MLPRegressor
from sklearn.metrics import mean_squared_error
from sklearn.preprocessing import MinMaxScaler
from sklearn.model_selection import train_test_split
from sklearn.metrics import confusion_matrix,accuracy_score
import math

X_data = bank_dumm
Y = targets

scaler = MinMaxScaler()
scaler.fit(X_data)
bank_data = scaler.transform(X_data)
bank_data
#---------------- Regression begins -----------------


#split data to train and test
#from sklearn.cross_validation import train_test_split
X_train, X_test, y_train, y_test = train_test_split(bank_data, Y, test_size=0.25, random_state=133)

clf = MLPRegressor(hidden_layer_sizes=(7,5), learning_rate='constant', learning_rate_init=0.15, max_iter=2000000, tol= 0.003, activation='logistic',solver='sgd',verbose=True)
clf = MLPRegressor(hidden_layer_sizes=(7,5,3), learning_rate='constant', learning_rate_init=0.15, max_iter=2000000, tol= 0.003, activation='tanh',solver='sgd',verbose=True)
clf = MLPRegressor(hidden_layer_sizes=(8,4), learning_rate='constant', learning_rate_init=0.15, max_iter=2000000, tol= 0.003, activation='relu',solver='sgd',verbose=True)

clf.fit(X_train,y_train.ravel())
ans = clf.predict(X_test)
ans
math.sqrt(mean_squared_error(y_test,ans.reshape(-1,1)))

#Unscale the data
#unscale = scaler.inverse_transform(ans.reshape(-1,1))

#################ordinal########################################
import category_encoders as ce
bank= pd.read_csv("C:\\Users\\ACER\\Desktop\\cust_Bank .csv")
bank_df=pd.DataFrame(bank)
bank_df
bank_df.drop(['y'],axis=1,inplace=True)
bank_df
education_order=bank_df['education'].replace({'illiterate':1,'basic.4y':2,'basic.6y':3,"basic.9y":4,"high.school":5,"professional.course":6,"university.degree":7})
education_order=pd.DataFrame(education_order)
bank_df.drop(['education'],axis=1,inplace=True)
bank_df
bank_df = pd.concat([education_order,bank_df],axis=1)
bank_df
bank_dumm=pd.get_dummies(bank_df,prefix=["job","marital","housing","loan"])
bank_dumm
bank_dumm.drop(['job_student','marital_single','housing_no','loan_no'],axis=1,inplace=True)
targets = bank['y']
targets = targets.replace({'no':0,'yes':1})
X_data = bank_dumm
Y = targets
Y
X_train, X_test, y_train, y_test = train_test_split(X_data, Y, test_size=0.15, random_state=42)

#clf = svm.SVC(C=60, gamma=.09, kernel='poly')
#clf = svm.SVC(C=50, gamma=.05, kernel='linear')
clf = svm.SVC(C=50, gamma=.05, kernel='rbf')
clf.fit(X_train, y_train)
ans = clf.predict(X_test)
ans
confusion_matrix(y_test, ans)
accuracy_score(y_test, ans)

#---------------- Regression begins -----------------
X_data = bank_dumm
#Y = targets

scaler = MinMaxScaler()
scaler.fit(X_data)
bank_data = scaler.transform(X_data)
bank_data

#split data to train and test
#from sklearn.cross_validation import train_test_split
X_train, X_test, y_train, y_test = train_test_split(bank_data, Y, test_size=0.25, random_state=133)

clf = MLPRegressor(hidden_layer_sizes=(6,5), learning_rate='constant', learning_rate_init=0.15, max_iter=2000000, tol= 0.003, activation='logistic',solver='sgd',verbose=True)
clf = MLPRegressor(hidden_layer_sizes=(9,5,3), learning_rate='constant', learning_rate_init=0.15, max_iter=2000000, tol= 0.003, activation='tanh',solver='sgd',verbose=True)
clf = MLPRegressor(hidden_layer_sizes=(8,5,3), learning_rate='constant', learning_rate_init=0.15, max_iter=2000000, tol= 0.03, activation='relu',solver='sgd',verbose=True)

clf.fit(X_train,y_train.ravel())
ans = clf.predict(X_test)
ans
ans.reshape(-1,1)
y_test
math.sqrt(mean_squared_error(y_test,ans.reshape(-1,1)))

#Unscale the data
#unscale = scaler.inverse_transform(ans.reshape(-1,1))
