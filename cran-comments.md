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
