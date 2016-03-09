The Andy Limber Klassifier is straightforward to use and requires only data (train and test) of the original form provided in the Kaggle competition. The data will be taken, modified, and then used to train a random forest to make predictions on the test data. The parameters of the random forest may be specified as well, but the defaults are the ones used for the current "best submission".

The function alFeatureGen will take any dataset and add the generated features, re-ordering the columns to keep popularity in the last position. This must be called on training and test sets separately, where no modification should be required as preparation. 

The function alKK will then perform the classification, feeding in the train and test sets as the only required parameters, outputting a CSV file of predictions in the required format.

There will be errors generated if the data is not of the exact right form, with all columns in the original order provided. Test data can be provided with a column of NAs for the popularity, or not. This will be added in later if not.

If the argument "Xtest" is set to TRUE, then the test set provided must have non-NA values for popularity and the code will finish with providing an "out of sample" accuracy as found from the prediction.

A function alXval is also provided to allow a k fold cross validation, packaged into a simple function. The function will just run the classification on k separate samples to validate and test any changes made. All the normal parameters can be played with.

The default return is a CSV file ready for submission as well as the random forest model. Importance can be specified so that the importance plot can be made later.
