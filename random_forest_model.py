# Random Forest Model
# 2022.06
# Jun

# make sure use the correct python environment that created by python_env_setup.R

# libraries
import pandas as pd
import numpy as np
from sklearn.ensemble import RandomForestRegressor
import matplotlib.pyplot as plt
from sklearn.metrics import explained_variance_score
from dtreeviz.trees import dtreeviz
from sklearn.tree import export_text
from treeinterpreter import treeinterpreter as ti
import random

# read in data.
train = pd.read_csv("./data/train_data.csv")
test = pd.read_csv("./data/test_data.csv")
df_predict = pd.read_csv("./data/predict_data.csv")

# Labels are the values we want to predict
## train data
labels = np.array(train['log_tickets_sold'])
# Remove y from the other features
features= train.drop('log_tickets_sold', axis = 1)
# Saving feature names for later use
feature_list = list(features.columns)# Convert to numpy array
features = np.array(features)

## test data
labels_test = np.array(test['log_tickets_sold'])
features_test= test.drop('log_tickets_sold', axis = 1)
feature_list_test = list(features_test.columns)
features_test = np.array(features_test)

## predict data
features_predict= df_predict.drop('total_tickets_sold', axis = 1)
feature_list_predict = list(features_predict.columns)
features_predict = np.array(features_predict)

# train model
## Instantiate model with 1000 decision trees
rf = RandomForestRegressor(n_estimators = 1000, random_state = 42)
## Train the model on training data
rf.fit(features, labels)

# make predictions on test data
## Use the forest's predict method on the test data
predictions = rf.predict(features_test)
## Calculate the absolute errors
errors = abs(predictions - labels_test)
## Print out the mean absolute error (mae)
print('Mean Absolute Error:', round(np.mean(errors), 2))
### Mean Absolute Error: 0.25

## Calculate mean absolute percentage error (MAPE)
mape = 100 * (errors / labels_test)
# Calculate and display accuracy
accuracy = 100 - np.mean(mape)
print('Accuracy:', round(accuracy, 2), '%.')
### Accuracy: 95.56 %.

## variance explained
explained_variance_score(labels_test, predictions)
### 0.7690777246851367

### features' importances
plt.figure(figsize=(28,23))
importances = rf.feature_importances_
importances
indices = np.argsort(importances)
plt.title('Feature Importances')
plt.barh(range(len(indices)), importances[indices], color='b', align='center')
plt.yticks(range(len(indices)), [feature_list[i] for i in indices])
plt.xlabel('Relative Importance')
plt.savefig("rf_importance.png")
plt.show()
# make predictions on predict data
predictions_predict = rf.predict(features_predict)
pred_test = pd.DataFrame({'pred_test': predictions}, columns=['pred_test'])
pred_goal = pd.DataFrame({'pred_goal': predictions_predict}, columns=['pred_goal'])

feature_import = pd.DataFrame({'features': feature_list, 'importance': importances}, columns=['features', 'importance'])

pred_test.to_csv("./data/pred_test.csv", index= False)
pred_goal.to_csv("./data/pred_goal.csv", index= False)
feature_import.to_csv("./data/feature_import.csv", index= False)

# viz = dtreeviz(rf.estimators_[0], features, labels,
#                feature_names=feature_list,
#                title="1st decision tree")
# 
# viz.save("decision_tree.svg")

print(export_text(rf.estimators_[0], spacing=3, decimals=3, feature_names=feature_list))

print("Test  R^2 Score : %.2f"%rf.score(features_test, labels_test))
print("Train R^2 Score : %.2f"%rf.score(features, labels))

preds, bias, contributions = ti.predict(rf, features_test)

random_sample = random.randint(1, len(features_test))
print("Selected Sample     : %d"%random_sample)
print("Actual Target Value : %.2f"%labels_test[random_sample])
print("Predicted Value     : %.2f"%preds[random_sample][0])

def create_contrbutions_df(contributions, random_sample, feature_names):
    contribs = contributions[random_sample].tolist()
    contribs.insert(0, bias[random_sample])
    contribs = np.array(contribs)
    contrib_df = pd.DataFrame(data=contribs, index=["Base"] + feature_names, columns=["Contributions"])
    prediction = contrib_df.Contributions.sum()
    contrib_df.loc["Prediction"] = prediction
    return contrib_df

contrib_df = create_contrbutions_df(contributions, random_sample, feature_list)
contrib_df
