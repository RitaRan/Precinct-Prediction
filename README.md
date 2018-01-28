[![wercker status](https://app.wercker.com/status/906cdad4dcf8520707216d49f4441a4d/s/master "wercker status")](https://app.wercker.com/project/byKey/906cdad4dcf8520707216d49f4441a4d)
# Precinct Prediction
The goal is to reconstruct the precinct map of Manhattan using two source data:
- NYC parking violation data: including information about parking violation, the information we will use is the addresses and the corresponding precincts
- Manhattan parcels data: including addresses in Manhattan and corresponding longitude latitude

# Code written for this task
- merge.R: clean the two source data and create data tables for each of them; merge the two tables based on cleaned 'address'.
- hw6.Rmd: train the model and reconstruct the precincts; plot the results on a map
- precincts.json: visualize the predicted precints on a map
