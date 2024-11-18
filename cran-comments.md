##Submission of revised version: v 0.7.1 - November 17, 2024

* New version includes correction to equation in pumpcurve.R and updates
  to vignette to accommodate deprecated functions in formatdown package

* Results from macOS Monterey 12.4 check:

  + ── R CMD check results ───────────────────────── hydraulics 0.7.1 ────
  + Duration: 42.4s
  + 0 errors ✔ | 0 warnings ✔ | 0 notes ✔
  + R CMD check succeeded

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
  + using log directory ‘/__w/hydraulics/hydraulics/check/hydraulics.Rcheck’
  + using R version 4.4.2 (2024-10-31)
  + using platform: x86_64-pc-linux-gnu
  + R was compiled by
    gcc (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0
    GNU Fortran (Ubuntu 11.4.0-1ubuntu1~22.04) 11.4.0
  + running under: Ubuntu 22.04.5 LTS
  + using session charset: UTF-8
  + using options ‘--no-manual --as-cran’
  + checking for file ‘hydraulics/DESCRIPTION’ ... OK
  + checking extension type ... Package
  + this is package ‘hydraulics’ version ‘0.7.1’
  + ...
  + checking package vignettes ... OK
  + checking re-building of vignette outputs ... [5s/4s] OK
  + checking for non-standard things in the check directory ... OK
  + checking for detritus in the temp directory ... OK
  + DONE

  + Status: OK

* Downstream dependencies: iemisc.
  + Results from revdepcheck::revdep_check(num_workers = 4)
  + Installing CRAN version of hydraulics
  + Installing DEV version of hydraulics
  + CHECK ────────────────────────────────────────────────────────────── 1 packages
  + ✔ iemisc 1.0.4                           ── E: 0     | W: 0     | N: 0                                                
  + OK: 1  
  + BROKEN: 0

