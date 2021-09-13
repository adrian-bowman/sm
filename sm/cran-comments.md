* This update allows the package can now be installed on systems which do not have the tcltk package available.  Some minor big fixes are also included.

## Test environments
* local R installation, R 4.0.2
* win-builder (oldrelease)
* win-builder (release)
* win-builder (devel)
* Rhub

## R CMD check results

0 errors | 0 warnings | 0 notes

* On R-Hub platform Fedora Linux, R-devel, clang, gfortran, a strange error is reported, triggered by an example in the h.select function.  There is little detail given and the example works in all other environments, so the problemn on this platform is difficult to investigate.
