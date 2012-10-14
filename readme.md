Diagrams of distributions in the style of Kruschke (2011)
=====================================================

The aim of the script `plot_dist.R` is to create diagrams of distribution to be used when illustrating Bayesian hierarchical models in the style of John K. Kruschke's [*Doing Bayesian Data Analysis*](http://www.indiana.edu/~kruschke/DoingBayesianDataAnalysis/). The image below shows an example taken from DBDA and Kruschke describes the advantages of this style of diagram compared to DoodleBUGS style diagrams in this [blog post](http://doingbayesiandataanalysis.blogspot.se/2012/05/graphical-model-diagrams-in-doing.html).

![DBDA diagram](https://raw.github.com/rasmusab/distribution_diagrams/master/dbda_diagram.jpg)

In order to create Kruschke style diagrams you need pretty pictures of the different distribution you have in your model, pictures that you later can stitch together in some drawing program (for example [Libre Office Draw](http://www.libreoffice.org/features/draw/) or [Inkscape](http://inkscape.org/)). The script `plot_dist.R` (written in [R](http://www.r-project.org/)) helps with this and to create a diagram of the normal distribution you would run the following code in your current R session.


```r
# Reads in the functions plot_dist, plot_dist_svg, plot_dist_png and a
# list of predefined distributions called dists.
source("plot_dist.R")
plot_dist(dists$normal)
```

![plot of chunk unnamed-chunk-1](https://raw.github.com/rasmusab/distribution_diagrams/master/figure/unnamed-chunk-1.png) 

If you want to you can fill in the parameters yourself in the drawing program later but you can also make `plot_dist` draw the parameters by supplying a character or expression vector.


```r
plot_dist(dists$normal, labels = c(mean = expression(mu[j]), right_sd = expression(tau)))
plot_dist(dists$normal, labels = c(mean = expression(M[0]), right_sd = expression(T[0])))
plot_dist(dists$normal, labels = c(mean = expression(M[1]), right_sd = expression(T[1])))
plot_dist(dists$gamma, labels = c(params = "S, R"))
```

![plot of chunk unnamed-chunk-2](https://raw.github.com/rasmusab/distribution_diagrams/master/figure/unnamed-chunk-21.png) ![plot of chunk unnamed-chunk-2](https://raw.github.com/rasmusab/distribution_diagrams/master/figure/unnamed-chunk-22.png) ![plot of chunk unnamed-chunk-2](https://raw.github.com/rasmusab/distribution_diagrams/master/figure/unnamed-chunk-23.png) ![plot of chunk unnamed-chunk-2](https://raw.github.com/rasmusab/distribution_diagrams/master/figure/unnamed-chunk-24.png) 


The image below shows the distributions that are currently implemented which covers the univariate distributions in [jags](http://mcmc-jags.sourceforge.net/) and some more. I am not overly familiar with all of these distributions and I was not sure what was the most "canonical" shape for some of them (the generalized gamma distribution for example). If you have any feedback, questions or suggestion (maybe on what distributions to add) please don't hesitate to contact me ([rasmus.baath@lucs.lu.se](rasmus.baath@lucs.lu.se))!

![The implemented distributions](https://raw.github.com/rasmusab/distribution_diagrams/master/all_dists_smaller.png)

Files
------------------------

The file [plot_dist.R](https://raw.github.com/rasmusab/distribution_diagrams/master/plot_dist.R) contains all you need to get going: the functions `plot_dist`, `plot_dist_svg`, `plot_dist_png` and the list of the predefined distributions.

If you don't want to bother with generating your own images you can download png and svg images for all the distributions [here](https://raw.github.com/rasmusab/distribution_diagrams/master/distribution_diagrams.zip). The actual script that generated the images is available [here](https://raw.github.com/rasmusab/distribution_diagrams/master/create_diagrams.R) which might be useful to look at as it contains many examples of how `plot_dist` works. 

Plot size and output format
---------------------------
The diagrams produced by `plot_dist` are made to look good when being around 2-3 inches wide and 1.5-2.5 inches high. If you want to make larger plots you have to play around with the scale parameter:


```r
plot_dist(dists$beta, labels = c(params = "a, b"), scale = 3)
```

![plot of chunk unnamed-chunk-3](https://raw.github.com/rasmusab/distribution_diagrams/master/figure/unnamed-chunk-3.png) 


However, if you just want to make `svg` or `png` files to import into a drawing program you can use the convenience functions `plot_dist_svg` and `plot_dist_png`.


```r
plot_dist_png("bernouli_dist.png", dists$bernouli, expression(theta))
plot_dist_png("bernouli_dist.svg", dists$bernouli, expression(theta))
```

As svg is a vector format you can then scale the diagrams to your liking in your drawing program.

Diagram customization
----------------

In order to know the label names of the different parameters you just look at the definition of the distribution in the file `plot_dist.R`. The following is the definition of the normal distribution.


```r
normal = list(
    # Name of the distribution to be displayed in the plot
    name = "normal",
    # Position of the name in the plot
    name_pos = c(0.5, 0.1),
    # Plot type, "line" for a line plots and "bar" for bar plots.
    plot_type = "line",
    # The values of the x-axis.
    x = seq(-3.3, 3.3, 0.01),
    # If top_space = 0 the distribution extends to the top of the graph, if 
    # 0 > top_space < 1 then that proportion of space is left at the top.
    top_space = 0,
    # The function defining the probability density function
    ddist = dnorm,
    # The arguments given to the probability density function (has to be named) 
    ddist_params = list(mean=0, sd=1),
    # Coordinates and names for the parameter labels
    labels = list(mean = c(0.5, 0.3), right_sd = c(0.80, 0.5), left_sd = c(0.20, 0.5))
  )

```

Here we see that the normal distribution has three named labels and to, for example, make a diagram with a $\sigma$ to the left we would write:


```r
plot_dist(dists$normal, labels = c(mean = "M", left_sd = expression(sigma)))
```

![plot of chunk unnamed-chunk-6](https://raw.github.com/rasmusab/distribution_diagrams/master/figure/unnamed-chunk-6.png) 


You can also change the color of the diagrams using the parameter `color`.

```r
plot_dist(dists$bernouli, labels = c(p = "p"), color = "purple")
```

![plot of chunk unnamed-chunk-7](https://raw.github.com/rasmusab/distribution_diagrams/master/figure/unnamed-chunk-7.png) 

