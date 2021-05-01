
### Brief description

A refactoring of the IAU SOFA library to modern Fortran standards.

This is based on the 2019-07-22 Fortran 77 SOFA Release.

### Status

![CI Status](https://github.com/jacobwilliams/astro-fortran/actions/workflows/CI.yml/badge.svg)

## Building

A [FoBiS](https://github.com/szaghi/FoBiS) configuration file (`astro-fortran.fobis`) is also provided that can also build the library and examples. Use the `mode` flag to indicate what to build. For example:

  * To build all the examples using gfortran: `FoBiS.py build -f astro-fortran.fobis -mode tests-gnu`
  * To build all the examples using ifort: `FoBiS.py build -f astro-fortran.fobis -mode tests-intel`
  * To build a static library using gfortran: `FoBiS.py build -f astro-fortran.fobis -mode static-gnu`
  * To build a static library using ifort: `FoBiS.py build -f astro-fortran.fobis -mode static-intel`

  The full set of modes are: `static-gnu`, `static-gnu-debug`, `static-intel`, `static-intel-debug`, `shared-gnu`, `shared-gnu-debug`, `shared-intel`, `shared-intel-debug`, `tests-gnu`, `tests-gnu-debug`, `tests-intel`, `tests-intel-debug`

  To generate the documentation using [ford](https://github.com/Fortran-FOSS-Programmers/ford), run: ```FoBis.py rule --execute makedoc -f astro-fortran.fobis```

  To run the test programs, run: ```FoBis.py rule --execute tests -f astro-fortran.fobis```

## Documentation

The latest API documentation can be found [here](http://jacobwilliams.github.io/astro-fortran/). This was generated from the source code using [FORD](https://github.com/Fortran-FOSS-Programmers/ford) (note that the included `build.sh` script will also generate these files).

### License

The astro-fortran source code and related files and documentation are distributed under a permissive free software [license](https://github.com/jacobwilliams/astro-fortran/blob/master/LICENSE.txt) (BSD-style).

### See also

 * [SPICE](http://naif.jpl.nasa.gov/naif/toolkit.html)
 * [NOVAS](http://aa.usno.navy.mil/software/novas/novas_info.php)
 * [SOFA](http://www.iausofa.org)
 * [Fortran Astrodynamics Toolkit](https://github.com/jacobwilliams/Fortran-Astrodynamics-Toolkit)
