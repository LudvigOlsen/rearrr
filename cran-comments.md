## Test environments
* local OS X install, R 3.6.1
* local OS X install, R 4.2.1
* ubuntu 16.04 (on travis-ci), R 3.6.3, 4.0.0 and devel
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Notes on conditional use of suggested packages

I have gone through each use of the suggested packages and made them conditional on the package being installed. I did not manage to test this locally, as running ```devtools::check(env_vars = c(`_R_CHECK_DEPENDS_ONLY_`="true"))``` did not fail without the changes, like what you reported. If it still fails, please let me know exactly how to run this check :-)