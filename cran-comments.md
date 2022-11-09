##Submission of revised version: v 0.6.0 - November 8, 2022

* New version includes additional functions and corrects a bug
* New release after only one month is for compatibility with new bookdown document

* Results from macOS Monterey 12.4 check:

  + ── R CMD check results ─────────── hydraulics 0.6.0 ────
  + Duration: 1m 9.7s

  + 0 errors ✔ | 0 warnings ✔ | 0 notes ✔

  + R CMD check succeeded
  
* From Windows check (devtools::check_win_devel()): 
  
  + using log directory 'd:/RCompile/CRANguest/R-devel/hydraulics.Rcheck'
  + using R Under development (unstable) (2022-10-11 r83083 ucrt)
  + using platform: x86_64-w64-mingw32 (64-bit)
  + using session charset: UTF-8
  + checking for file 'hydraulics/DESCRIPTION' ... OK
  + checking extension type ... Package
  + this is package 'hydraulics' version '0.6.0'
  + package encoding: UTF-8
  + checking CRAN incoming feasibility ... [12s] Note_to_CRAN_maintainers
  + Maintainer: 'Ed Maurer <emaurer@scu.edu>'
  + ...
  + DONE
  + Status: OK
  + no errors, warnings, or notes

* Downstream dependencies: None.
