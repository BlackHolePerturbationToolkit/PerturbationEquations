---
layout: default
title: Black Hole Perturbation Toolkit
---

{% include head.html %}

<p>
 <h1 style="display:inline">PerturbationEquations</h1> <span style="float:right;"><a href="https://bhptoolkit.org/mathematica-install.html" class = "code_btn">Install this package!</a></span>
</p>

The PerturbationEquations Mathematica package provides a set of tools for working with the spherical-harmonic decompositions of the first- and second-order Einstein equations and Teukolsky equations in Schwarzschild spacetime.

The package's main functions are `SchwarzschildLinearOperator` and `SchwarzschildQuadraticOperator`. `SchwarzschildLinearOperator` generates spherical-harmonic modes of the lienarized Ricci or Einstein tensor in terms of modes of the metric perturbation.

`SchwarzschildQuadraticOperator` generates spherical-harmonic modes modes of the quadratic source term in the second-order Einstein equation or second-order Teukolsky equation. This source term can be the quadratic Einstein tensor, the quadratic Ricci tensor, or an associated Teukolsky source term. These quadratic modes are expressed as products of modes of a metric perturbation.

Expressions can be generated in a number of common spherical harmonic bases. They can also be specialized to a number of common gauge choices.

## Dependencies and usage

To run this package you will need to install [xAct](http://www.xact.es/).

After installing xAct and the package you load it using

```
<< xAct`PerturbationEquations`
```

You can find examples in the documentation for the `SchwarzschildLinearOperator` and `SchwarzschildQuadraticOperator` functions.

## Citing

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.8107166.svg)](https://doi.org/10.5281/zenodo.8107166)

This package is a supplement to the paper [Second-order perturbations of the Schwarzschild spacetime: practical, covariant and gauge-invariant formalisms](https://doi.org/10.48550/arXiv.2306.17847) by Andrew Spiers, Adam Pound and Barry Wardell.

In addition to citing the paper and acknowledging the Black Hole Perturbation Toolkit as suggested on the [front page](https://bhptoolkit.org) we also recommend citing the specific package version you use via the citation information on the package’s Zenodo page linked from the above DOI.
