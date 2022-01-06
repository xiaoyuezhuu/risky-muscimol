### A repository for the code and data to generate results for [Frontal but not parietal cortex is required for decisions under risk](https://www.biorxiv.org/content/10.1101/2021.11.19.469107v1)
## How to install the package
1. `library(devtools)`
2. `install_git("https://github.com/xiaoyuezhuu/risky-muscimol.git")`
3. `load_all()`
4. The package `riskybioRxiv` will be installed and the functions ready to use

## How to use it
1. Simply run `figure_xxx` and it will output a ggplot figure
2. All data is stored in `csv/`
3. All stan and GLMM fits are stored in `fits/`
4. `.stan` files contain the stain models, there are here for reference only and won't be run by the pacakge
