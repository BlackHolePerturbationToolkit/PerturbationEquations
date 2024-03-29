# PerturbationEquations

The PerturbationEquations package provides a set of tools for working with the spherical-harmonic decompositions of the first- and second-order Einstein equations and Teukolsky equations in Schwarzschild spacetime.

The package's main functions are SchwarzschildLinearOperator and SchwarzschildQuadraticOperator. SchwarzschildLinearOperator generates spherical-harmonic modes of the linearized Ricci or Einstein tensor in terms of modes of the metric perturbation. 

SchwarzschildQuadraticOperator generates spherical-harmonic modes of the quadratic source term in the second-order Einstein equation or second-order Teukolsky equation. This source term can be the quadratic Einstein tensor, the quadratic Ricci tensor, or an associated Teukolsky source term. These quadratic modes are expressed as products of modes of a metric perturbation.

Expressions can be generated in a number of common spherical harmonic bases. They can also be specialized to a number of common gauge choices.

## Dependencies and usage

To run this package you will need to install [xAct](http://www.xact.es/).

After installing xAct and the package you load it using

```
<<xAct`PerturbationEquations`
```

You can find examples in the documentation for the `SchwarzschildLinearOperator` and `SchwarzschildQuadraticOperator` functions.
