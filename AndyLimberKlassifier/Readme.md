The Andy Limber Klassifier is straightforward to use and requires only data (train and test) of the original form provided in the Kaggle competition. The data will be taken, modified, and then used to train both an XG boost and random forest. These are then used in conjunction to make the final prediction. Parameters for both models may be varied, as well as the "threshold" variable that determines how the two mix together. All defaults are specified for the "current best submission".

The function alFeatureGen will take any dataset and add the generated features, re-ordering the columns to keep popularity in the last position. This must be called on training and test sets separately, where no modification should be required as preparation. The most recent update adds 3 columns to the data.

Then there are two functions, alXGB and alRF which train an XGboost or random forest (respectively) on the data. These are provided as separate functions for flexibility and testing. They work in almost identical ways and many parameters can be specified. No pre-processing is required for the alXGB, as it is all done internally.

These two functions are called and combined finally in the alKK function. All data must be of the correct format, which is the original order of the provided data (alFeatureGen will organise the columns properly). If not, errors will be generated. The combination of the two models is straightforward, where the XGB classification is taken unless the probability is below the "threshold" parameter, in which case the RF classification is chosen. Therefore the threshold should be between 0 and 1, where 0 would be a pure XGboost model.

All classification functions will return by default a data frame with the predicted labels and probabilities of each class. A csv will also be written in the correct format for submission, by default. It is also possible to return an out of sample classification accuracy, setting Xtest to true. This requires a non-NA popularity column.

A function alXvalidate is also provided to allow a k fold cross validation, packaged into a simple function. The function will just run the classification on k separate samples to validate and test any changes made. All the normal parameters can be played with. The method parameter also allows the choice of which function to test, out of alRF, alXGB, and alKK. Typically most stable over a 10 fold validation.

To properly install the package so that you can view the vignette, the argument build_vignettes = TRUE must be included when stalled using devtools (e.g. installing from github). Then it can be accessed using vignette("AndyLimberKlassifier"). The vignette contains details of what the best submission is. No parameters need to be changed for the functions.

If an error is encountered when trying to access documentation, such as through "?alKK", remove the package, clear the enviornment, and restart R. The problem should not persist.
