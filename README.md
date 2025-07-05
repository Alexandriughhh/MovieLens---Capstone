# MovieLens Capstone Project 🎬

This project builds a movie recommendation system using the MovieLens 10M dataset and regularized linear models in R.

## Files Included

- `Capstone.R` – Full R script for data cleaning, modeling, and evaluation
- `MovieLens_Project.Rmd` – Annotated R Markdown notebook
- `MovieLens_Capstone.pdf` – Final report with visualizations and results
- `README.md` – Overview of the project

## Highlights

- Built a recommender system using user and movie effects
- Applied regularization and lambda tuning to reduce RMSE
- Compared models with and without user effects
- Visualized trends over time and genre differences

## RMSE Results

| Model                    | RMSE     |
|-------------------------|----------|
| Global Average          | 0.90     |
| Movie Effects           | 0.89     |
| Movie + User Effects    | 0.87     |
| Regularized Model       | **0.865** |

## Dataset

MovieLens 10M dataset  
📦 Download: [https://grouplens.org/datasets/movielens/10m/](https://grouplens.org/datasets/movielens/10m/)

## License

MIT License
