
### Brief description

A refactoring of the IAU SOFA library to modern Fortran standards.

This is based on the 2019-07-22 Fortran 77 SOFA Release.

### Status

[![GitHub release](https://img.shields.io/github/release/jacobwilliams/astro-fortran.svg)](https://github.com/jacobwilliams/astro-fortran/releases/latest)
[![Build Status](https://github.com/jacobwilliams/astro-fortran/actions/workflows/CI.yml/badge.svg)](https://github.com/jacobwilliams/astro-fortran/actions)
[![codecov](https://codecov.io/gh/jacobwilliams/astro-fortran/branch/master/graph/badge.svg?token=43HK33CSMY)](https://codecov.io/gh/jacobwilliams/astro-fortran)
[![last-commit](https://img.shields.io/github/last-commit/jacobwilliams/astro-fortran)](https://github.com/jacobwilliams/astro-fortran/commits/master)

### Compiling

A [Fortran Package Manager](https://github.com/fortran-lang/fpm) manifest file is included, so that the library and tests cases can be compiled with FPM. For example:

```
fpm build --profile release
fpm test --profile release
```

To use `astro-fortran` within your FPM project, add the following to your `fpm.toml` file:
```toml
[dependencies]
astro-fortran = { git="https://github.com/jacobwilliams/astro-fortran.git" }
```

To generate the documentation using [FORD](https://github.com/Fortran-FOSS-Programmers/ford), run:

```
  ford astro-fortran.md
```

### Documentation

The latest API documentation can be found [here](https://jacobwilliams.github.io/astro-fortran/). This was generated from the source code using [FORD](https://github.com/Fortran-FOSS-Programmers/ford).

### License

The astro-fortran source code and related files and documentation are distributed under a permissive free software [license](https://github.com/jacobwilliams/astro-fortran/blob/master/LICENSE.txt) (BSD-style).

### See also

 * [SPICE](http://naif.jpl.nasa.gov/naif/toolkit.html)
 * [NOVAS](http://aa.usno.navy.mil/software/novas/novas_info.php)
 * [SOFA](http://www.iausofa.org)
 * [Fortran Astrodynamics Toolkit](https://github.com/jacobwilliams/Fortran-Astrodynamics-Toolkit)