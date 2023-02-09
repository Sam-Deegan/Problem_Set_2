Problem_Set_2
================

## Theory

Show that, without perfect compliance, the Intention-to-treat effect
will be smaller than the

ITT and ATE are two methods used to estimate the impact of a treatment.

Without perfect compliance outcomes are not homogeneous. To account for
heterogeneous outcomes we can delineate our binary treatment indicator
($D_i$) by those who are and are not affected by the binary instrument
($Z_i$):

- $D_1i$ when $Z_i= 1$
- $D_0i$ when $Z_i= 0$

$i$ represents the treatment status ($D_i$) when the instrument ($Z_i$)
takes one of the two binary values.

We can represent the observed treatment status in the switching
equation:

$$D_i=D_{0i}+(D_{1i}-D_{0i})Z_i\ (Equation \ 1)$$ When $Z_i=1$, $D_i$
takes on the value $D_{1i}$, as the $D_{0i}$ cancel. When $Z_i=0$, $D_i$
takes on the value $D_{0i}$, the second terms disappears, leaving
$D_{0i}$.

We can rewrite this equation in regression form as:

$$\delta_{0}+\delta_{1i}Z_i+\eta_i$$

However $\delta_{1i}$ is not uniform for all individuals in the sample.
Those who have a high $\delta$ are encouraged or discouraged by $Z_i$.
But those with a low $\delta$ are less affected by $Z_i$.

There population can be divided into four groups. Those who are
unaffected by the instrument ($Z_i$):

- Always-Takers: People who always take the treatment.
  $D_{1i}=D_{0i}=1$.
- Never-Takers: People who never take the Treatment. $D_{1i}=D_{0i}=0$.

Additionally, there are those who are affected by the instrument. \*
Compliers: Those influenced by the instrument ($Z_i$) as expected.
$D_{1i}=1\ and\ D_{0i}=0$. \* Defiers: Those influenced by the
instrument ($Z_i$) unexpectedly. $D_{1i}=0\ and\ D_{0i}=1$.

## Simulation Excercise

You can include R code in the document as follows:

    ## Warning in 1:simulations: numerical expression has 1000 elements: only the first
    ## used

    ## Warning in 1:simulations: numerical expression has 1000 elements: only the first
    ## used

![](Problem_Set_2_files/figure-gfm/Generate%20Beta-1.png)<!-- -->

    ## Warning in 1:simulations: numerical expression has 1000 elements: only the first
    ## used

![](Problem_Set_2_files/figure-gfm/Generate%20Beta-2.png)<!-- -->

    ## Warning in 1:simulations: numerical expression has 1000 elements: only the first
    ## used

![](Problem_Set_2_files/figure-gfm/Generate%20Beta-3.png)<!-- -->![](Problem_Set_2_files/figure-gfm/Generate%20Beta-4.png)<!-- -->

## Empirical Application

You can also embed plots, for example:

![](Problem_Set_2_files/figure-gfm/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
