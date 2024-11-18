##Submission of revised version: v 0.7.1 - November 17, 2024

* New version includes correction to equation in pumpcurve.R and updates
  to vignette to accommodate deprecated functions in formatdown package

* Results from macOS Monterey 12.4 check:

  + ── R CMD check results ───────────────────────── hydraulics 0.7.1 ────
  + Duration: 42.4s
  + 0 errors ✔ | 0 warnings ✔ | 0 notes ✔
  + R CMD check succeeded
r
  + From Windows check (devtools::check_win_devel()): 
  + using R Under development (unstable) (2024-11-15 r87338 ucrt)
  + using platform: x86_64-w64-mingw32
  + R was compiled by
    gcc.exe (GCC) 13.3.0
    GNU Fortran (GCC) 13.3.0
  + running under: Windows Server 2022 x64 (build 20348)
  + using session charset: UTF-8
  + checking for file 'hydraulics/DESCRIPTION' ... OK
  + checking extension type ... Package
  + this is package 'hydraulics' version '0.7.1'
  + ...
  + checking package vignettes ... OK
  + checking re-building of vignette outputs ... OK
  + checking PDF version of manual ... [16s] OK
  + checking HTML version of manual ... OK
  + DONE
  + Status: OK

* From linux check using rhub::rhub_check(platforms = "ubuntu-release")
  + ── hydraulics 0.7.0: NOTE

  + Build ID:   hydraulics_0.7.0.tar.gz-9c3db248b9ae44f1bb0baf8e072fc9bf
  + Platform:   Fedora Linux, R-devel, GCC
  + Submitted:  45m 7.3s ago
  + Build time: 44m 38.4s
  + Maintainer: ‘Ed Maurer <emaurer@scu.edu>’
  + Possibly misspelled words in DESCRIPTION:
    Maurer (20:19)
  + Found the following (possibly) invalid URLs:
    URL: https://www.gouldspumps.com
      From: inst/doc/hydraulics_vignette.html
      Status: Error
  + checking HTML version of manual ... NOTE
    Skipping checking HTML validation: no command 'tidy' found
    Skipping checking math rendering: package 'V8' unavailable

0 errors ✔ | 0 warnings ✔ | 2 notes ✖

* The html version of the manual was validated on other operating systems but for
  some reason generates a note on fedora. The documentation does not have a "tidy" 
  command so it is unclear what the first note refers to. No 'V8' package is used.
  My name is spoelled correctly in the DESCRIPTION. The URL flagged as (possibly)
  invalid is correct and works correctly.

* Downstream dependencies: iemisc.
  + Results from revdepcheck::revdep_check(num_workers = 4)
  + Installing CRAN version of hydraulics
  + Installing DEV version of hydraulics
  + CHECK ────────────────────────────────────────────────────────────── 1 packages
  + ✔ iemisc 1.0.4                           ── E: 0     | W: 0     | N: 0                                                
  + OK: 1   

