import pandas as pd
import matplotlib.pyplot as plt
import pickle

from sklearn import preprocessing
from sklearn.model_selection import train_test_split
from sklearn.ensemble import ExtraTreesClassifier
from imblearn.combine import SMOTEENN
from collections import Counter
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import classification_report
from imblearn.under_sampling import EditedNearestNeighbours

######1-Load Data
data = pd.read_csv("/Users/tanyufei/Desktop/Hack-a-thon/hackathon-2019-11-master/data/bank-full.csv", sep=';')
print(data.head())


######2-Tidy Data
def turn_to_dummy_variables(data_set, dummy_fields):
    for each in dummy_fields:
        dummies = pd.get_dummies(data_set.loc[:, each], prefix=each)
        data_set = pd.concat([data_set, dummies], axis=1)
    return data_set


'''
def turn_to_binary_variables(data_set, binary_fields):
    for i in binary_fields:
        for j in range(len(data_set[i])):
            if data_set[i][j] == "yes":
                data_set[i][j] = 1
            else:
                data_set[i][j] = 0
    return data_set
'''


def turn_to_binary_variables(data_set, binary_fields):
    for each in binary_fields:
        list = data_set[each].tolist()
        lb = preprocessing.LabelBinarizer()
        binaries = lb.fit_transform(list)
        binaries_pd = pd.DataFrame(binaries)
        new_col = [each+'_1']
        binaries_pd.columns = new_col
        data_set = pd.concat([data_set, binaries_pd], axis=1)
    return data_set


fields_d = ['job', 'marital', 'education', 'contact', 'month', 'poutcome']
data1 = turn_to_dummy_variables(data, fields_d)
fields_b = ['default', 'housing', 'loan', 'y']
data2 = turn_to_binary_variables(data1, fields_b)
fields_to_drop = ['job', 'marital', 'education', 'contact', 'month', 'poutcome', 'default', 'housing', 'loan', 'y']
new_data = data2.drop(fields_to_drop, axis=1)

print(new_data.head())
#new_data.to_csv("new_bank.csv", index = False, sep=',')


######3-Split training data & testing data
X = new_data.iloc[:, 0:48]
y = new_data.iloc[:, -1]
#print(X)
#print(y)

x_train, x_test, y_train, y_test = train_test_split(X, y, test_size=.1, random_state=100)


######4-Feature Selection
model = ExtraTreesClassifier()
model.fit(x_train,y_train)
print(model.feature_importances_)

#plot graph of feature importances for better visualization
feat_importances = pd.Series(model.feature_importances_, index=x_train.columns)
feat_importances.nlargest(48).plot(kind='barh')
plt.show()


######5-Resample
smote_enn = SMOTEENN(random_state=100)
x_resampled, y_resampled = smote_enn.fit_sample(x_train, y_train)
print(Counter(y_resampled))


######6-Random Forest
#dict1={"y": 9, "default": 1}
rf = RandomForestClassifier(n_estimators=200)
rf.fit(x_resampled, y_resampled)

y_predict = rf.predict(x_test)
print(classification_report(y_test, y_predict))

#save model
pickle.dump(rf,open(r"rf.pkl",'wb'))