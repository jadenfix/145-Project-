packages <- c("MSwM","car","dplyr","knitr","lmtest","msm","plotly","sandwich","tseries","urca","xts")
missing <- setdiff(packages, rownames(installed.packages()))
if (length(missing)) install.packages(missing, repos="https://cloud.r-project.org")
