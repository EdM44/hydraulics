##Submission of revised version: v 0.6.1 - December 6, 2022

* New version includes modifications to accommodate the change for R 4.2 where
  warnings are now errors for if() statements with a condition of length greater 
  than one

* Results from macOS Monterey 12.4 check:

  + ── R CMD check results ──────────────────────── hydraulics 0.6.1 ────
  + Duration: 1m 11.4s

  + 0 errors ✔ | 0 warnings ✔ | 0 notes ✔

  + R CMD check succeeded
  
* From Windows check (devtools::check_win_devel()): 
  + using log directory 'd:/RCompile/CRANguest/R-devel/hydraulics.Rcheck'
  + using R Under development (unstable) (2022-12-05 r83406 ucrt)
  + using platform: x86_64-w64-mingw32 (64-bit)
  + using session charset: UTF-8
  + checking for file 'hydraulics/DESCRIPTION' ... OK
  + checking extension type ... Package
  + this is package 'hydraulics' version '0.6.1'
  + package encoding: UTF-8
  + checking CRAN incoming feasibility ... [17s] Note_to_CRAN_maintainers
  + Maintainer: 'Ed Maurer <emaurer@scu.edu>'
  + ...
  + checking for unstated dependencies in vignettes ... OK
  + checking package vignettes in 'inst/doc' ... OK
  + checking re-building of vignette outputs ... [15s] OK
  + checking PDF version of manual ... [24s] OK
  + checking HTML version of manual ... [4s] OK
  + checking for detritus in the temp directory ... OK
  + DONE
  + Status: OK
  + no errors, warnings, or notes

* From linux check using devtools::check_rhub(pkg='.',platforms = "fedora-gcc-devel")
  + ── hydraulics 0.6.1: NOTE

  + Build ID:   hydraulics_0.6.1.tar.gz-4a347917bbae479aa2ce68cf635f28d2
  + Platform:   Fedora Linux, R-devel, GCC
  + Submitted:  1h 7m 38.1s ago
  + Build time: 1h 7m 28.4s

  + checking HTML version of manual ... NOTE
  + Skipping checking HTML validation: no command 'tidy' found
  + Skipping checking math rendering: package 'V8' unavailable

  + 0 errors ✔ | 0 warnings ✔ | 1 note ✖  + ...

* The html version of the manual was validated on other operating systems but for
  some reason generates a note on fedora. The documentation does not have a "tidy" 
  command so it is unclear what this note refers to.

* Downstream dependencies: None.

