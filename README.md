# Implementation of the Lind/Mehlum utest

To test for a U shaped or inverse U shaped relationship, it is necessary to provide an interval where the shape is located. A U shaped relationship is downward sloping at the lower bound and upward sloping at the upper bound, and vice versa for an inverted U shape. This package provides a joint test of the two slopes.

See Lind and Mehlum: [With or Without U? The Appropriate Test for a U‐Shaped Relationship](https://doi.org/10.1111/j.1468-0084.2009.00569.x) for details

## Installation

## Implementation for R

To install the R package from this repository (assuming `devtools` is installed)
```
devtools::install_github(`https://github.com/jotlind/utest`)
```

## Implementation for Stata
To install the Stata package from SSC
```
ssc install utest
```

## Running the package in R

Set up data
```
x <- runif(100,min=-1,max=1)
xsq <- x^2
y <- x^2+rnorm(100)
mod <- lm(y~x+xsq)
```

Run the U test
```
utest(mod,c("x","xsq"))
```

Get details on the slope of the relationship at the boundaries
```
uslopes(mod,c("x","xsq"))
```

## Authors

- [Jo Thori Lind](mailto:j.t.lind@econ.uio.no)
- [Halvor Mehlum](mailto:halvor.mehlum@econ.uio.no)


## License
[![License](http://img.shields.io/:license-mit-blue.svg?style=flat-square)](http://badges.mit-license.org)

- **[MIT license](http://opensource.org/licenses/mit-license.php)**
- Copyright 2020 © Jo Thori Lind and Halvor Mehlum.
