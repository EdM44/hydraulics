##Submission of revised version: v 0.7.2 - October 9, 2025

* New version is a minor update to remove the deprecated ggplot2 aes_string
  function from moody.R. This corrects plotting errors that had resulted.

* Results from macOS Sequoia 15.5 check:

  + ── R CMD check results ─────────────────── hydraulics 0.7.2 ────
  + Duration: 28.9s
  + 0 errors ✔ | 0 warnings ✔ | 0 notes ✔


  + From Windows check (usethis::use_github_action("check-standard"):
  ── R CMD check ─────────────────────────────────────────────────────────────────
  + using log directory 'D:/a/hydraulics/hydraulics/check/hydraulics.Rcheck'
  + using R version 4.5.1 (2025-06-13 ucrt)
  + using platform: x86_64-w64-mingw32
  + R was compiled by
  +    gcc.exe (GCC) 14.2.0
  +    GNU Fortran (GCC) 14.2.0
  + running under: Windows Server 2022 x64 (build 26100)
  + using session charset: UTF-8
  + using options '--no-manual --as-cran'
  + checking for file 'hydraulics/DESCRIPTION' ... OK
  + checking extension type ... Package
  + this is package 'hydraulics' version '0.7.2'
  + ...
  + DONE
  + Status: 4 NOTEs
  + All notes are related to the presence of the "revdep" directory, created 
  + by the revdep_check package. That directory is excluded from the R package
  + by having ^revdep$ in the .Rbuildignore file.
  
  + Badges for R-CMD-check.yaml are all passing for Mac-OS, Windows, and 3 
  + versions of Ubuntu as displayed at https://github.com/EdM44/hydraulics

* Downstream dependencies: iemisc.
  + Results from revdepcheck::revdep_check(num_workers = 4)
  + Installing CRAN version of hydraulics
  + Installing DEV version of hydraulics
  + CHECK ────────────────────────────────────────────────────────────── 1 packages
  + ✔ iemisc 1.0.4                           ── E: 0     | W: 0     | N: 0                                                
  + OK: 1  
  + BROKEN: 0
