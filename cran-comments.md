## Resubmission

First attempt succeeded in compiling in Windows and Fedora Linux:
* Build ID: hydraulics_0.1.0.tar.gz-8d55df00b25e430bb61ec2a799aaeb02
* Platform: Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* Build ID: hydraulics_0.1.0.tar.gz-a3874ec0081d4f2e8fb6869d47b722e2
* Platform: Fedora Linux, R-devel, clang, gfortran
But had an error on the Ubuntu compilation
* Build ID: hydraulics_0.1.0.tar.gz-5838b23129044df8ae0cf40ed870d357
* Platform: Ubuntu Linux 16.04 LTS, R-release, GCC
  + checking package dependencies ... ERROR
  + Package suggested but not available: ‘iemisc’

Revision: Since no functionality from the iemisc package is used it
was removed as a suggest.

## Test environments
* local OS X install, R 3.6.1
* Windows (using devtools::check_win_devel, platform: x86_64-w64-mingw32 (64-bit), R version 4.0.0 alpha (2020-03-26 r78078)

## R CMD check results

* From Local OSX check:
  + 0 errors ✓ | 0 warnings ✓ | 1 note x
  + R CMD check succeeded

  + NOTE: Non-standard files/directories found at top level:
    ‘README.Rmd’ ‘cran-comments.md’ ‘hydraulics_0.1.0.pdf’

* From Windows check:
	+ 0 errors, 0 warnings, 2 notes
	+ Possibly mis-spelled words in DESCRIPTION: Weisbach (9:60)
	+ Non-standard files/directories found at top level:
	'README.Rmd' 'cran-comments.md' 'hydraulics_0.1.0.pdf'

## Downstream dependencies

None.
