
<!-- PROJECT SHIELDS -->

[![Contributors][contributors-shield]][contributors-url]
[![Forks][forks-shield]][forks-url]
[![Stargazers][stars-shield]][stars-url]
[![Issues][issues-shield]][issues-url]
[![MS-PL License][license-shield]][license-url]
[![LinkedIn][linkedin-shield]][linkedin-url]



<!-- PROJECT LOGO -->
<br />
<p align="center">
  <a href="https://github.com/lisp-stat/lla">
    <img src="https://lisp-stat.dev/images/stats-image.svg" alt="Logo" width="80" height="80">
  </a>

  <h3 align="center">LLA</h3>

  <p align="center">
  A linear algebra library for Common Lisp
	<br />
    <a href="https://lisp-stat.dev/docs/manuals/lla/"><strong>Explore the docs »</strong></a>
    <br />
    <br />
    <a href="https://github.com/lisp-stat/lla/issues">Report Bug</a>
    ·
    <a href="https://github.com/lisp-stat/lla/issues">Request Feature</a>
    ·
    <a href="https://lisp-stat.github.io/lla/">Reference Manual</a>
  </p>
</p>



<!-- TABLE OF CONTENTS -->
<details open="open">
  <summary><h2 style="display: inline-block">Table of Contents</h2></summary>
  <ol>
    <li>
      <a href="#about-the-project">About the Project</a>
      <ul>
        <li><a href="#objectives">Objectives</a></li>
        <li><a href="#built-with">Built With</a></li>
      </ul>
    </li>
    <li>
      <a href="#getting-started">Getting Started</a>
      <ul>
        <li><a href="#prerequisites">Prerequisites</a></li>
        <li><a href="#installation">Installation</a></li>
      </ul>
    </li>
    <li><a href="#acknowledgements">Acknowledgements</a></li>
    <li><a href="#roadmap">Roadmap</a></li>
	<li><a href="#resources">Resources</a></li>
    <li><a href="#contributing">Contributing</a></li>
    <li><a href="#license">License</a></li>
    <li><a href="#contact">Contact</a></li>
    <li><a href="#issues">Known Issues</a></li>
  </ol>
</details>



<!-- ABOUT THE PROJECT -->
## About the Project

LLA is a high-level Common Lisp library built on on [BLAS](http://www.netlib.org/blas/) and [LAPACK](http://www.netlib.org/lapack/), but providing a more abstract interface with the purpose of freeing the user from low-level concerns and reducing the number of bugs in numerical code.


### Objectives

- High-level, user friendly interface that hides the details.

`(solve a b)` should return $X$, from $AX=B$, regardless of whether $A$ is a dense matrix, an $LU$ decomposition, or something else; similarly, $X$ should be a vector/matrix when $B$ is. Users should not need to memorize names like `DGESV`, especially when CLOS makes it so easy to deal with these things. Also, you don't need to make sure that all arguments are of the same type (eg complex-double): LLA will find the common supertype for elements and convert if necessary.

- Stay in Lisp and expose the innards of the objects as much as possible.

LLA aims to take advantage of CL's high level facilities such as CLOS and memory management. Data is kept in Lisp arrays instead of foreign arrays, so you can access it directly using `aref` etc. You also benefit from garbage collection and all the clever stuff that comes with the GC. If you need more memory, just increase the heap size.

- Keeping it simple.

Currently, LLA sources amount to less than 3000 lines of code (not including tests). The small size should make maintenance easier and bugs more rare (hopefully).

- Speed is important, but reliability comes first.

Only optimize when necessary, and do extensive testing afterwards. Most of the speed comes from your LAPACK library anyway. Most linear algebra operations are $O(N^\alpha)$ with $\alpha > 1$, frequently $\alpha > 2$. That said, copying to memory is optimized, and in the long run LLA should make use of your implementation's array pinning features (when available).

### Built With

* [anaphora](https://github.com/tokenrove/anaphora)
* [alexandria](https://gitlab.common-lisp.net/alexandria/alexandria)
* [let-plus](https://github.com/sharplispers/let-plus)
* [cffi](https://github.com/cffi/cffi)
* [numerical-utilities](https://github.com/Lisp-Stat/numerical-utilities)
* [select](https://github.com/Lisp-Stat/select)


# Getting Started

Certain features of LLA can be configured before loading using the plist `*lla-configuration*` in the `CL-USER` package (for example, on SBCL you would do it in your `~/.sbclrc`).  The following properties are supported:

- `:libraries`

    A list of objects, passed directly to `cffi:load-foreign-library`. You can use strings, paths, or even symbols if you have defined these libraries using `cffi:define-foreign-library`. If you don't define this, a reasonable platform-dependent default will be used. See the next section for details.

- `:int64`

    This makes LLA use 64-bit integers for array dimensions, pivot indices and other integer values passed to BLAS/LAPACK functions. **Only use this if you are sure that your libraries have been compiled with 64-bit integers**. The fact that you have a 64-bit platform does not necessarily mean that this is the case, in fact, it is still quite rare. Unless told otherwise, LLA expects BLAS/LAPACK to use the [(L)LP64 model](http://en.wikipedia.org/wiki/64-bit#64-bit_data_models) for integers -- that is to say, integer types in FORTRAN are 32 bit.

- `:efficiency-warnings`

    Enable the **possibility** of efficiency warnings at compile time. You still have to set the appropriate flags, but without this option, they won't even be checked. There are two properties you can set: `:array-type` and `:array-conversion`. The first warns whenever an array has to be walked elementwise to determine its type, the second when some arrays need to be converted to a common type.

    Example:

    ```lisp
    (defparameter cl-user:*lla-configuration*
      '(:efficiency-warnings (:array-type :array-conversion)))
    ```

    before loading LLA, and

    ```lisp
    (let ((lla:*lla-efficiency-warning-array-type* t)
          (lla:*lla-efficiency-warning-array-conversion* t))
       (code that you want to check))
    ```

## Prerequisites

LLA needs BLAS and LAPACK shared libraries to work.  When it comes to loading libraries, LLA tries to pick a sensible default for each platform, but in case it fails, you need to tell LLA where the libraries are before loading.

You can do this by putting something like this in your startup script (eg `~/.sbclrc`, the symbol needs to be in the package `cl-user`):

```lisp
(defparameter cl-user:*lla-configuration*
  '(:libraries ("/usr/lib/atlas-base/atlas/libblas.so.3"
                "/usr/lib/atlas-base/libatlas.so.3")))
```

### Obtaining BLAS & LAPACK libraries

MacOS (darwin) has BLAS and LAPACK available by default.  For Linux, use your package manager.  On MS Windows, [OpenBLAS](https://github.com/xianyi/OpenBLAS) is known to work and the project provides pre-compiled binaries for MS Windows.  You can either configure your PATH environment to contain the libopenblas.dll, or set the library as above.

## Installation

### Getting the source

To make the system accessible to [ASDF](https://common-lisp.net/project/asdf/) (a build facility, similar to `make` in the C world), clone the repository in a directory ASDF knows about.  By default the `common-lisp` directory in your home directory is known. Create this if it doesn't already exist and then:

1. Clone the repository
```sh
cd ~/common-lisp && \
git clone https://github.com/Lisp-Stat/lla.git
```
2. Reset the ASDF source-registry to find the new system (from the REPL)
   ```lisp
   (asdf:clear-source-registry)
   ```
3. Load the system
   ```lisp
   (asdf:load-system :lla)
   ```

If you have installed the slime ASDF extensions, you can invoke this with a comma (',') from the slime REPL.

### Getting dependencies

To get the third party systems that LLA depends on, you can use a dependency manager, such as [Quicklisp](https://www.quicklisp.org/beta/) or [CLPM](https://www.clpm.dev/) Once installed, get the dependencies with either of:

```lisp
(clpm-client:sync :sources "clpi") ;sources may vary
```

```lisp
(ql:quickload :lla)
```

You need do this only once. After obtaining the dependencies, you can load the system with `ASDF` as described above without first syncing sources.


## Acknowledgements

LLA was was written by Tamas Papp, inspired by packages written by AJ Rossini, Rif, Mark Hoemmen and others.  Gábor Melis made substantial contributions to the library, especially the low-level pinning interface and the destructive BLAS routines.

## Roadmap

-   write optimized pinning interfaces
-   improve documentation
-   write more tests (especially randomized ones, develop macros for that)
-   write a tutorial


<!-- CONTRIBUTING -->
## Contributing

Contributions are what make the open source community such an amazing place to be learn, inspire, and create. Any contributions you make are **greatly appreciated**. Please see [CONTRIBUTING](CONTRIBUTING.md) for details on the code of conduct, and the process for submitting pull requests.

<!-- LICENSE -->
## License

Distributed under the MS-PL License. See [LICENSE](LICENSE) for more information.

<!-- CONTACT -->
## Contact

Project Link: [https://github.com/lisp-stat/lla](https://github.com/lisp-stat/lla)



<!-- MARKDOWN LINKS & IMAGES -->
<!-- https://www.markdownguide.org/basic-syntax/#reference-style-links -->
[contributors-shield]: https://img.shields.io/github/contributors/lisp-stat/lla.svg?style=for-the-badge
[contributors-url]: https://github.com/lisp-stat/lla/graphs/contributors
[forks-shield]: https://img.shields.io/github/forks/lisp-stat/lla.svg?style=for-the-badge
[forks-url]: https://github.com/lisp-stat/lla/network/members
[stars-shield]: https://img.shields.io/github/stars/lisp-stat/lla.svg?style=for-the-badge
[stars-url]: https://github.com/lisp-stat/lla/stargazers
[issues-shield]: https://img.shields.io/github/issues/lisp-stat/lla.svg?style=for-the-badge
[issues-url]: https://github.com/lisp-stat/lla/issues
[license-shield]: https://img.shields.io/github/license/lisp-stat/lla.svg?style=for-the-badge
[license-url]: https://github.com/lisp-stat/lla/blob/master/LICENSE
[linkedin-shield]: https://img.shields.io/badge/-LinkedIn-black.svg?style=for-the-badge&logo=linkedin&colorB=555
[linkedin-url]: https://www.linkedin.com/company/symbolics/


## Known Issues
If you are using Emacs with slime on MS Windows, there is a [bug](https://github.com/slime/slime/issues/756) in slime where trying to load a shared library using CFFI will hang slime.  The workaround is to switch to the `*inferior-lisp*` emacs buffer and press enter once or twice.  After than, slime will unhang and CFFI will continue to load the BLAS libraries.


