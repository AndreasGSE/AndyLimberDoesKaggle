% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/alKK.R
\name{alKK}
\alias{alKK}
\title{Andy Limber's Kaggle Klassifier}
\usage{
alKK(train, test, vars = NULL, thresh = 0.4, alXGB.control = NULL,
  alRF.control = NULL, seed = 123, Xtest = FALSE, CSV = TRUE)
}
\arguments{
\item{train}{A dataframe containing the training data for the
popularity classifications.}

\item{test}{A dataframe containing the data to be used to predict labels.
If a column of NAs is not supplied for the "popularity", one will be added.}

\item{vars}{Vector of column indices for the features to be used in the random forest.
Defaults to NULL and all features are used.}

\item{thresh}{The probability threshold for favouring a method. thresh = 0 would use
only alXGB. thresh = 1 would use only alRF.}

\item{alXGB.control}{Optional list containing parameters for alXGB. All must be named params
must be named and provided according to alXGB documentation. Train, test, seed, and
TRUE / FALSE parameters do not need to be provided.}

\item{alRF.control}{Optional list containing parameters for alRF. See alXGB.control. 
Vars argument does not need to be the same for both functions.}

\item{seed}{The seed to be used.}

\item{Xtest}{A logical that indicates whether you want to perform some
cross validation. Default is FALSE. If TRUE, will print an accuracy value.
Must supply values for popularity. Will over-ride other returned values.}

\item{CSV}{A logical that indicates whether a CSV of predictions should be
saved. Default is TRUE.}
}
\value{
Returns the predicted labels, probabilities and IDs, as well 
as a CSV of predictions if CSV is set to TRUE.
If Xtest is set to true, will return an accuracy.
}
\description{
Assign popularity values to news articles for submission to Kaggle. Can be used with
any set of features as unreferenced by name, but best submission includes feature generation
done by the alFeatuerGen function (must be called on train and test data separately).
The random forest and xgboost functions are both called internally.
}

