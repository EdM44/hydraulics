<!-- NEWS.md is generated from NEWS.Rmd. Please edit that file -->

## hydraulics 0.7.0

- Update vignette and DESCRIPTION to replace package docxtools with
  formatdown
- Add option to manningc to allow n to vary with depth
- Modify the spec_energy_trap plotting function to include more than one
  added depth line
- Update the vignette to include new capabilities

## hydraulics 0.6.1

- Modifications for R 4.2 change from warning to error for if()
  statements with a condition of length greater than one

## hydraulics 0.6.0

- Corrected erroneous unit conversion in atmtemp function
- Update for variable consistency between pump and hardy-cross equations
  (r to K)
- Added function to use the direct step method for water profile
  calculation
- Added a function to find the sequent depth for a hydraulic jump

## hydraulics 0.5.0

- Added functions for specific weight, atmospheric properties, and
  summary tables
- Corrected dens function that incorrectly returned specific weight with
  Eng units
- Updated the vignette to show new functions

## hydraulics 0.4.1

- Added functions for surface tension and bulk modulus of elasticity
- Corrected typos
- Updated the vignette

## hydraulics 0.4.0

- Added functions to solve for flows in a pipe network with Hardy-Cross
  method
- Added a saturated vapor pressure function for water properties
- Updated the vignette to demonstrate the new functions

## hydraulics 0.3.0

- Added functions to fit a pump characteristic curve and system curve
- Updated the vignette to demonstrate the new functions
- Improved plotting of specific energy diagrams again

## hydraulics 0.2.4

- Improved plotting of specific energy diagrams

## hydraulics 0.2.3

- Added capabilities to calculate optimal trapezoidal channel width and
  depth
- Added illustrations to vignette for new functions
- Corrected equations for Colebrook formula

## hydraulics 0.2.2

- Added the ability to use the `units` package with most functions
- Added a detailed vignette to demonstrate more capabilities of the
  package

## hydraulics 0.2.1

- Expanded and where necessary corrected documentation for all functions
- Changed name of colebrook_f to colebrook for alignment with function
  name
- Added function to calculate critical depth for a circular pipe

## hydraulics 0.2.0 (CRAN commit a3f7f8df5d)

- Added functionality to solve for absolute roughness in presure pipe
  flow
- Added manningc function to include partially filled pipe solutions
- Added manningt function to include trapezoidal (including rectangular
  and triangular) open-channel flow solutions
- Added plotting of circle and trapezoid cross-sections for open
  channels
- Added spec_energy_trap function to plot a specific energy diagram for
  trapezoidal channels

## hydraulics 0.1.0

- Initial Release
