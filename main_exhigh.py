from grid_search_exhigh import *
import pandas as pd
import numpy as np
from mesa import Mesa
from arguments import parser
from utils import Rater, load_dataset
from sklearn.tree import DecisionTreeClassifier
from sklearn.metrics import recall_score
from sklearn.model_selection import StratifiedKFold

# def get_preds(threshold, probabilities):
#     return [1 if prob > threshold else 0 for prob in probabilities]
#
# def get_fpr_tpr(y, prob):
#     roc_values = []
#     for thresh in np.linspace(0, 1, 50):
#         preds = get_preds(thresh, prob)
#         tn, fp, fn, tp = confusion_matrix(y, preds).ravel()
#         tpr = tp / (tp + fn)
#         fpr = fp / (fp + tn)
#         roc_values.append([tpr, fpr])
#     tpr_values, fpr_values = zip(*roc_values)
#     tpr_values = np.array(tpr_values)
#     fpr_values = np.array(fpr_values)
#
#     return tpr_values, fpr_values

if __name__ == "__main__":
    # randomly split 10 times
    n_runs = 10

    args = parser.parse_args()
    rater = Rater(args.metric)  # default: auc
    

    # load data and standardization
    train_data, test_data = load_dataset(args.dataset)
    sah_standard(train_data, test_data)

    column_name = train_data.columns[1:].tolist()
    column_name_noFI = train_data.columns[2:].tolist()
    X = train_data.drop(['label','src_subject_id'], axis=1)
    X_noFI = train_data.drop(['label','src_subject_id','nihtbx_fluidcomp_uncorrected'], axis=1)
    y = train_data.label
    z = train_data.src_subject_id
    X, X_noFI, y, z = X.values, X_noFI.values, y.values, z.values

    # Four ML model
    base_estimator = DecisionTreeClassifier(max_depth=None)
    mesa = Mesa(
        args=args,
        base_estimator=base_estimator,
        n_estimators=args.max_estimators)
    down_ensemble = Ensemble(n_estimators=11)

    # 5-fold cross validation
    skf = StratifiedKFold(n_splits=5, shuffle=True)
    scores_list_train, scores_list_valid, y_pred_valid = [], [], []
    scores_list_train_noFI, scores_list_valid_noFI, y_pred_valid_noFI = [], [], []

    n = 1
    for _ in range(n_runs):
        for train_index, val_index in skf.split(X, y):
            print("cross validation (accumulate):", n)
            n += 1
            X_train, X_val = X[train_index], X[val_index]
            X_train_noFI, X_val_noFI = X_noFI[train_index], X_noFI[val_index]
            y_train, y_val = y[train_index], y[val_index]
            y_train = y_train[:, np.newaxis]
            y_val = y_val[:, np.newaxis]
            train_data = np.concatenate((X_train, y_train), axis=1)
            valid_data = np.concatenate((X_val, y_val), axis=1)
            train_data = pd.DataFrame(train_data, columns=column_name)
            valid_data = pd.DataFrame(valid_data, columns=column_name)
            train_data_noFI = np.concatenate((X_train_noFI, y_train), axis=1)
            valid_data_noFI = np.concatenate((X_val_noFI, y_val), axis=1)
            train_data_noFI = pd.DataFrame(train_data_noFI, columns=column_name_noFI)
            valid_data_noFI = pd.DataFrame(valid_data_noFI, columns=column_name_noFI)

            train_x, train_y, valid_x, valid_y, test_x, test_y = dataset_preprocessing(train_data, valid_data, test_data)
            train_x_noFI, train_y_noFI, valid_x_noFI, valid_y_noFI, test_x_noFI, test_y_noFI = dataset_preprocessing(train_data_noFI, valid_data_noFI, test_data)

            # fit model
            down_ensemble.fit_data(train_x, train_y)
            svm_auc_train, lr_auc_train, rf_auc_train = down_ensemble.score_all(train_x, train_y)
            tpr_svm, fpr_svm, ppv_svm, tpr_lr, fpr_lr, ppv_lr, tpr_rf, fpr_rf, ppv_rf = down_ensemble.fpr_tpr_total(train_x, train_y)
            scores_list_train.append([svm_auc_train, tpr_svm, fpr_svm, ppv_svm,
                                      lr_auc_train, tpr_lr, fpr_lr, ppv_lr,
                                      rf_auc_train, tpr_rf, fpr_rf, ppv_rf])
            
            svm_auc_valid, lr_auc_valid, rf_auc_valid = down_ensemble.score_all(valid_x, valid_y)
            tpr_svm, fpr_svm, ppv_svm, tpr_lr, fpr_lr, ppv_lr, tpr_rf, fpr_rf, ppv_rf = down_ensemble.fpr_tpr_total(valid_x, valid_y)
            scores_list_valid.append([svm_auc_valid, tpr_svm, fpr_svm, ppv_svm,
                                      lr_auc_valid, tpr_lr, fpr_lr, ppv_lr,
                                      rf_auc_valid, tpr_rf, fpr_rf, ppv_rf])
            y_pred_svm, y_pred_lr, y_pred_rf = down_ensemble.predict_proba(valid_x)
            y_pred_valid = pd.DataFrame({'src_subject_id': z[val_index],'valid_y': valid_y,'y_pred_svm': y_pred_svm,'y_pred_lr': y_pred_lr,'y_pred_rf': y_pred_rf})
            y_pred_valid.to_csv('ypred_valid{}.csv'.format(n-1), index=False)
            
            
            down_ensemble.fit_data_noFI(train_x_noFI, train_y_noFI)
            svm_auc_train_noFI, lr_auc_train_noFI, rf_auc_train_noFI = down_ensemble.score_all_noFI(train_x_noFI, train_y_noFI)
            tpr_svm_noFI, fpr_svm_noFI, ppv_svm_noFI, tpr_lr_noFI, fpr_lr_noFI, ppv_lr_noFI, tpr_rf_noFI, fpr_rf_noFI, ppv_rf_noFI = down_ensemble.fpr_tpr_total_noFI(train_x_noFI, train_y_noFI)
            scores_list_train_noFI.append([svm_auc_train_noFI, tpr_svm_noFI, fpr_svm_noFI, ppv_svm_noFI,
                                      lr_auc_train_noFI, tpr_lr_noFI, fpr_lr_noFI, ppv_lr_noFI,
                                      rf_auc_train_noFI, tpr_rf_noFI, fpr_rf_noFI, ppv_rf_noFI])

            svm_auc_valid_noFI, lr_auc_valid_noFI, rf_auc_valid_noFI = down_ensemble.score_all_noFI(valid_x_noFI, valid_y_noFI)
            tpr_svm_noFI, fpr_svm_noFI, ppv_svm_noFI, tpr_lr_noFI, fpr_lr_noFI, ppv_lr_noFI, tpr_rf_noFI, fpr_rf_noFI, ppv_rf_noFI = down_ensemble.fpr_tpr_total_noFI(valid_x_noFI, valid_y_noFI)
            scores_list_valid_noFI.append([svm_auc_valid_noFI, tpr_svm_noFI, fpr_svm_noFI, ppv_svm_noFI,
                                      lr_auc_valid_noFI, tpr_lr_noFI, fpr_lr_noFI, ppv_lr_noFI,
                                      rf_auc_valid_noFI, tpr_rf_noFI, fpr_rf_noFI, ppv_rf_noFI])
            y_pred_svm_noFI, y_pred_lr_noFI, y_pred_rf_noFI = down_ensemble.predict_proba_noFI(valid_x_noFI)
            y_pred_valid_noFI = pd.DataFrame({'src_subject_id': z[val_index],'valid_y_noFI': valid_y_noFI,'y_pred_svm_noFI': y_pred_svm_noFI,'y_pred_lr_noFI': y_pred_lr_noFI,'y_pred_rf_noFI': y_pred_rf_noFI})
            y_pred_valid_noFI.to_csv('ypred_valid_noFI{}.csv'.format(n-1), index=False)
            
    
    merged_train_scores = pd.concat([pd.DataFrame(scores_list_train), pd.DataFrame(scores_list_train_noFI)], axis=1)
    merged_valid_scores = pd.concat([pd.DataFrame(scores_list_valid), pd.DataFrame(scores_list_valid_noFI)], axis=1)

# 设置列名
    merged_train_scores.columns = ['svm_auc',  'svm_sen','svm_spe', 'svm_ppv',
                                   'lr_auc',  'lr_sen','lr_spe', 'lr_ppv',
                                    'rf_auc',  'rf_sen','rf_spe', 'rf_ppv',
                                    'svm_auc_noFI',  'svm_sen_noFI','svm_spe_noFI', 'svm_ppv_noFI',
                                    'lr_auc_noFI',  'lr_sen_noFI','lr_spe_noFI', 'lr_ppv_noFI',
                                    'rf_auc_noFI',  'rf_sen_noFI','rf_spe_noFI', 'rf_ppv_noFI']

    merged_valid_scores.columns = ['svm_auc',  'svm_sen','svm_spe', 'svm_ppv',
                                   'lr_auc',  'lr_sen','lr_spe', 'lr_ppv',
                                    'rf_auc',  'rf_sen','rf_spe', 'rf_ppv',
                                    'svm_auc_noFI',  'svm_sen_noFI','svm_spe_noFI', 'svm_ppv_noFI',
                                    'lr_auc_noFI',  'lr_sen_noFI','lr_spe_noFI', 'lr_ppv_noFI',
                                    'rf_auc_noFI',  'rf_sen_noFI','rf_spe_noFI', 'rf_ppv_noFI']

    merged_train_scores.to_csv('ml_train.csv', index=False)
    merged_valid_scores.to_csv('ml_valid.csv', index=False)

    print('##### training set ####')
    for column in merged_train_scores.columns:
        print(f'{column} | {merged_train_scores.mean()[column]} - {merged_train_scores.std()[column]}')

    print('\n##### validation set ####')
    for column in merged_valid_scores.columns:
        print(f'{column} | {merged_valid_scores.mean()[column]} - {merged_valid_scores.std()[column]}')












