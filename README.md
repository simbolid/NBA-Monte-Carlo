# Parallelizing Elo-Based Predictions for the 2024 NBA Playoffs

Project for Lehigh University CSE 375: Parallel Programming

We create an elo-based prediction model for the 2024 NBA Playoffs. 
We then run 10,000 simulations using this model.
Our goal is to improve the runtime of these simulations. 

### Running the Python Code 

```
pip install -r requirements.txt
python elo.py
```
### Running the R code
# for elo model implementation
```
Rscript .\elo_prediction.R   #for sequential
Rscript .\elo_parallel.R     #for parallel
```
# for YUSAG model
we need to first get the ranking model based on the input season, the input season is hardcoded in yale_model_ranking.R
```
Rscript .\yale_model_ranking.R
Rscript .\yale_model_parallel.R  #for parallel version
Rscript .\yale_model_simulation.R   #for sequential version
```
