from grid_search_inhigh import *
import argparse
import pandas as pd
import numpy as np
from sklearn.metrics import confusion_matrix, accuracy_score, recall_score
import joblib
import warnings
warnings.filterwarnings("ignore", category=DeprecationWarning)

# All patients & Patients with good-grade aSAH
parser = argparse.ArgumentParser(description='Arguments')
parser.add_argument('--dataset', type=str, default='sah', metavar='N',
                    help='the dataset used for prognosis analysis (default: sah)')

def load_dataset(dataset_name):
    train_data = pd.read_csv(f'data/{dataset_name}_train.csv', index_col=0)
    test_data = pd.read_csv(f'data/{dataset_name}_test.csv', index_col=0)
    return train_data, test_data

# randomly down-sampling the patients with favorable outcomes in the training set
def SAH_imbalance(train_x, train_y, down_ratio=1):
    train_data = pd.concat([train_x, train_y], axis=1)
    data_maj = train_data[train_data.label == 1.]
    data_min = train_data[train_data.label == 0.]
    data_maj_sample = resample(data_maj, replace=False, n_samples=data_min.shape[0] * down_ratio)
    data = pd.concat([data_maj_sample, data_min])
    train_x, train_y = data.drop('label', axis=1), data.label
    return train_x, train_y

def get_preds(threshold, probabilities):
    return [1 if prob > threshold else 0 for prob in probabilities]

def get_fpr_tpr(y, prob):
    roc_values = []
    preds = get_preds(0.5, prob)
    tn, fp, fn, tp = confusion_matrix(y, preds).ravel()
    tpr = tp / (tp + fn)
    fpr = fp / (fp + tn)
    ppv = tp / (tp + fp)
    roc_values.append([tpr, fpr, ppv])
    tpr_values, fpr_values, ppv_values = zip(*roc_values)
    tpr_values = np.array(tpr_values)
    fpr_values = np.array(fpr_values)
    ppv_values = np.array(ppv_values)

    return tpr_values, fpr_values, ppv_values

if __name__ == "__main__":
    args = parser.parse_args()
    train_data, test_data = load_dataset(args.dataset)  # default: sah
    # column_name = train_data.columns.tolist()

    sah_standard(train_data, test_data)
    train_x, train_y = train_data.drop(['label'], axis=1), train_data.label
    test_x, test_y = test_data.drop(['label'], axis=1), test_data.label
    train_x_noFI, train_y_noFI = train_data.drop(['label','nihtbx_fluidcomp_uncorrected'], axis=1), train_data.label
    test_x_noFI, test_y_noFI = test_data.drop(['label','nihtbx_fluidcomp_uncorrected'], axis=1), test_data.label

    base_rf_ = RandomForestClassifier(class_weight="balanced")
    estimators_rf, estimators_rf_noFI, scores_list_valid, scores_list_valid_noFI = [], [], [], []
    n_estimators = 11

    for est in range(n_estimators):
        X, y = SAH_imbalance(train_x, train_y)
        # bagging
        tuned_estimators = [{'n_estimators': [100, 200, 300]}]
        clf_rf = GridSearchCV(base_rf_, tuned_estimators, scoring="roc_auc")
        cv_estimator = sklearn.base.clone(clf_rf).fit(X, y)
        estimators_rf.append(cv_estimator)
    
    for est in range(n_estimators):
        X, y = SAH_imbalance(train_x_noFI, train_y_noFI)
        # bagging
        tuned_estimators = [{'n_estimators': [100, 200, 300]}]
        clf_rf = GridSearchCV(base_rf_, tuned_estimators, scoring="roc_auc")
        cv_estimator = sklearn.base.clone(clf_rf).fit(X, y)
        estimators_rf_noFI.append(cv_estimator)

    y_pred_rf = np.array([model.predict_proba(test_x)[:, 1] for model in estimators_rf]).mean(axis=0)
    rf_pred = [1 if ele > 0.5 else 0 for ele in y_pred_rf.tolist()]
    rf_auc, rf_acc = roc_auc_score(test_y, y_pred_rf), accuracy_score(test_y, rf_pred)
    tpr_rf, fpr_rf, ppv_rf = get_fpr_tpr(test_y, y_pred_rf)
    scores_list_valid.append([rf_auc, tpr_rf, 1-fpr_rf, ppv_rf])
    
    y_pred_rf_noFI = np.array([model.predict_proba(test_x_noFI)[:, 1] for model in estimators_rf_noFI]).mean(axis=0)
    rf_pred_noFI = [1 if ele > 0.5 else 0 for ele in y_pred_rf_noFI.tolist()]
    rf_auc_noFI, rf_acc_noFI = roc_auc_score(test_y_noFI, y_pred_rf_noFI), accuracy_score(test_y_noFI, rf_pred_noFI)
    tpr_rf_noFI, fpr_rf_noFI, ppv_rf_noFI = get_fpr_tpr(test_y_noFI, y_pred_rf_noFI)
    scores_list_valid_noFI.append([rf_auc_noFI, tpr_rf_noFI, 1-fpr_rf_noFI, ppv_rf_noFI])
    merged_valid_scores = pd.concat([pd.DataFrame(scores_list_valid), pd.DataFrame(scores_list_valid_noFI)], axis=1)
    merged_valid_scores.columns = ['rf_auc',  'rf_sen','rf_spe', 'rf_ppv',
                                    'rf_auc_noFI',  'rf_sen_noFI','rf_spe_noFI', 'rf_ppv_noFI']
    merged_valid_scores.to_csv('ensemble_valid_inhigh.csv',mode='a',header=False, index=False)
    print("rf_auc:", rf_auc, "rf spe:", 1-fpr_rf, "rf sen:", tpr_rf, "rf ppv:", ppv_rf)















