# doctr
An R package to check data consistency

## Intro

The `doctr` package is an easy to use data consistency doctor for R. In short, it attempts to automate as much as possible the task of verifying if  everything is ok with a dataset.

This package allows the user to specify to what class a variable belongs ("money", "count", "quantity", "text", etc.) and runs automated tests to see if the variables do in fact fit their classes. The user can also specify if a variable is allowed to contain missing values, what is it's expected maximum value, among other settings.

`doctr` also produces automated exploratory reports for each variable, showing you how many missing values it contains, how many non-missing values it contains, and some percentiles. These reports are then displayed in an intuitive way along with any red flags encontered while parsing each variable.

## Functionalities

`doctr` has to main functions: `examine` and `diagnose`.

### `examine`

This function produces automated exploratory reports for the specified variables. Unlike `diagnose` you have to know close to nothing about the dataset to use this function, so I recommend using it to get to know your data.

It outputs the exploratory report and with `problems` you can see any inconsistencies found during analysis.

### `diagnose`

This function checks if a set of variables passes certain tests (passed through the `exams` parameter). `diagnose` returns a simple table telling you wether each variable passed its tests and, if a variable has failed these tests, you can use the `problems` function to see exactly what has made it fail them.

To use this function the user has to have some assumtions about the data, especially to what category each variable belongs, so I offen use it when I get multiple versions of the same dataset and have to see if each new version is in the same format as the previous ones.
