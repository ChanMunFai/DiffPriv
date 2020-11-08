# DiffPriv

[**Comparative Metrics Framework.pdf**](https://github.com/MUNFAI15/DiffPriv/blob/master/comparative%20metrics%20framework.pdf) provides a comprehensive and intuitive framework to evaluate the performance of any synthetic dataset vis-à-vis its original dataset and other synthetic datasets. It captures information on the tradeoff between **general utility, specific utility** and **disclosure risk**. 

#### Motivation

Whilst researching on the best data synthesis method, I struggled with finding suitable benchmarks to evaluate the quality of synthetic datasets. Taub et al.'s work on this is highly excellent, but was not easily reproducible. 

> I coded previously publicly unavailable functions on ROC and DCAP in the package **cmf**. 
>
> I made modifications to Taub's work when necessary, to ensure an intuitive, comprehensive and systematic evaluation of synthetic datasets. 

For code on the ROC and CAP functions as well as their usage examples, refer to the package [**cmf**](https://github.com/MUNFAI15/cmf).

Here is a high-level overview of the comparative metrics framework. 
![Screenshot](CMF overview.png)

[**DCAP functions**](https://github.com/MUNFAI15/DiffPriv/blob/master/DCAP%20functions.R) provides functions to calculate the Correct Attribution Probabilbility (CAP) score as given by Taub, J., Elliot, M., Pampaka, M., &amp; Smith, D. (2018). Differential Correct Attribution Probability for Synthetic Data: An Exploration. Privacy in Statistical Databases Lecture Notes in Computer Science, 122-137. doi:10.1007/978-3-319-99771-1_9

[**ROC compiled**](https://github.com/MUNFAI15/DiffPriv/blob/master/ROC_compiled.R) provides functions to provide the Ratio of Counts (ROC) score as given by Taub, J., Elliot, M., & Raab, G. (2019). Creating the Best Risk-Utility Profile : The Synthetic Data Challenge.

Author : Chan Mun Fai 
