# Abstract

In this document we explore the possibility of defining a model to predict IDP displacement using predictors related to the displacement and the living condition at departure and arrival locations, leaving aside predictors related to the disaster or conflict events. For this we will use monthly collected data at location (community) level for an interval of 20 months. For each month the unit of analysis was the itinerary aggregated at municipal level. These itineraries were described with three thematic areas of variables: displacement, demographic characteristics, sector needs and reason of displacement; each of them corresponded to progressively more difficult to acquire and less readily available information. The data was split between training and hold-out. Furthermore, over the training set we applied a Rolling Origin Forecasting with six folds to create the Probe data

We defined a baseline model with the historical average displacement by itinerary (historical as to the period covered by the study). Aside we defined our final model in four steps incorporating predictors by thematic batch: the first one for displacement thematic predictors, the second one we added demographic thematic predictors, the third included sector needs and in the fourth we added reasons of displacement. We observed the gains in the Root Mean Squares Error (RMSE) against the baseline model and between the three progressive models as to observe the improvement of the model. Each model was processed with three trained algorithms: Least Absolute Shrinkage and Selection Operator (LASSO), Partial Least Squares (PLS) and Random Forest (RF).

At the end of the process the model and algorithm that provided the lowest RMSE was chosen and its predictive capacity tested against the hold-out data. Aside of the predictive model, the step approach for modeling allowed us to observe the performance of each batch of predictors as they are progressively added into to the model. Finally, but not least, we further obtained a valuable exploratory analysis on determinants of internal displacement.

## Data protection statement:
_The current project have been done with real data. However, the data in its original form is sensitive and
private, and its access requires signature of data protection waiver.
To ensure data protection, the original data have been altered: locations names and codes do not correspond
to its original; population and displacement figures have been transformed and all data have been aggregated
to a superior administrative level; among other alterations in the data and in the report. 
The original data and its processing script can be requested and will be provided prior signature of standard data protection waiver._
