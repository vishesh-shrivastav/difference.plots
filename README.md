# difference.plots

**About**  
`difference.plots` is a R package for plotting common difference in measurement plots like Mountain plot, Bland Altman plot and others.  

**Installation**  
`difference.plots` can be installed via Github:

```{r}
if (!require(devtools)) {  
    install.packages('devtools')  
}  
devtools::install_github('vishesh-shrivastav/difference.plots')
```

**Plot Examples**  

**Mountain Plot**  
```{r}
m <- runif(20, 0, 1)
r <- runif(20, 0, 1)
mountain_plot(m, r, "plotly")
```  

![mountain-plot-1](https://raw.githubusercontent.com/vishesh-shrivastav/difference.plots/master/docs/mountain_1.png)  

**Bland Altman Plot**
```{r}
bland_altman(m, r, "plotly")
```

![bland-altman-1](https://raw.githubusercontent.com/vishesh-shrivastav/difference.plots/master/docs/ba_01.png)
