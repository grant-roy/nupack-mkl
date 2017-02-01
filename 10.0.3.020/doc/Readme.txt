------------------------------------------------------------------------

                      README Information for

            Intel(R) Math Kernel Library 10.0 for Linux* OS

------------------------------------------------------------------------



Introduction

The Intel(R) Math Kernel Library (Intel(R) MKL) 
provides developers of scientific, engineering and financial software
with a set of linear algebra routines, fast Fourier transforms, and 
vectorized math and random number generation functions, all optimized
for the latest Intel(R) Pentium(R) 4 processors, Intel(R) Xeon(R) 
processors with Streaming SIMD Extensions 3 (SSE3) and Intel(R) 
Extended Memory 64 Technology (Intel(R) EM64T), and Intel(R) 
Itanium(R) 2 processors. This software also performs well on non-Intel
(x86) processors.

Intel MKL provides linear algebra functionality with 
LAPACK (solvers and eigensolvers) plus level 1, 2, and 3 BLAS offering
the vector, vector-matrix, and matrix-matrix operations needed for 
complex mathematical software.  Users who prefer FORTRAN 90/95 
programming language obtain opportunity to call LAPACK driver and 
computational subroutines via specially designed interfaces with reduced
numbers of arguments.  Intel MKL provides ScaLAPACK 
(SCAlable LAPACK) and support functionality including the Basic Linear 
Algebra Communications Subprograms (BLACS) and the Parallel Basic Linear
Algebra Subprograms (PBLAS).  For solving sparse systems of equations,
Intel MKL now provides a direct sparse solver as well as
an iterative sparse solver as well as a supporting set of sparse BLAS 
(level 1, 2, and 3).  Intel MKL offers multidimensional
discrete Fourier transforms (1D, 2D, 3D) with mixed radix support (not 
limited to sizes of powers of 2). In addition, distributed versions 
of these function are now provided for use on clusters. Intel MKL
also includes a set of vectorized transcendental 
functions (called the Vector Math Library (VML)) offering both greater
performance and excellent accuracy compared to the libm (scalar) 
functions for most of the processors.  The Vector Statistical Library 
(VSL) offers high performance vectorized random number generators for a
number of probability distributions as well as convolution and 
correlation routines. Intel MKL also includes a set of functions which
perform basic operations (+, -, *, /) that act on intervals of floating
point numbers. This interval arithmetic package also includes a set of
linear solvers. The BLAS, LAPACK, direct sparse solver (DSS), and FFT 
functions in Intel MKL are threaded using OpenMP*. All of Intel MKL is 
thread-safe.



Product Directories

Following successful installation, the files associated with Intel 
MKL 10.0 will be installed on your host system. The
following directory map indicates the structure of the default Intel MKL 
installation directory (<install-dir>) and identifies 
the file types stored in each subdirectory. 

/opt/intel/mkl/10.0.xxx 
(xxx is the package number, for example, /opt/intel/mkl/10.0.039)

+---benchmarks
|
+---doc
|
+---examples
|
+---include
|
+---interfaces
|
+---man
|
+---lib
|    +---32
|    +---64
|    +---em64t
|
+---tests
|
+---tools



Other Sources of Information

Prior to product installation, you can view this file (Readme.txt), 
the product Release Notes (Release_Notes.htm), and the product 
Installation Guide (Install.txt).  After installation, these files are
still available, but they are located in the <install-dir>/doc 
subdirectory as well as at the same level as the executable installation 
file. Refer to the following locations for essential information: 



Installation Information

The Installation Guide (Install.txt) at 
<install-dir>/doc/Install.txt or at the same level as 
the installation executable provides detailed installation information
in case a normal installation did not happen.



Technical Support

Contact Intel technical support primarily through your Intel(R) 
Premier Support account and the related web site at 
https://premier.intel.com/.  If you have questions or problems getting
started with the Intel(R) Math Kernel Library, please 
contact support at 

  https://registrationcenter.intel.com/support/.



Release Information

Refer to the Release_Notes.htm file at this level (pre-installation)
or at <install-dir>/doc/Release_Notes.htm for System 
Requirements, Known Limitations, and other links and information.



Product Documentation

An HTML file with links to all product documentation for Intel 
MKL is available after product installation. This 
file is available at <install-dir>/doc/Doc_index.htm.



Getting Started

Once installation is complete, you can check it by executing the procedures 
in the "Getting Started" chapter of the "Intel(R) Math Kernel Library 
for Linux* OS User's Guide" document and gain understanding 
of the product operation by further reading the guide. The document is 
available at <install-dir>/doc/userguide.pdf.



Other Intel Products

You can find out about other Intel software development products 
through the Intel web site at

  http://www.intel.com/software/products/
  

  
  
Disclaimer and Legal Information

INFORMATION IN THIS DOCUMENT IS PROVIDED IN CONNECTION WITH INTEL(R) 
PRODUCTS. NO LICENSE, EXPRESS OR IMPLIED, BY ESTOPPEL OR OTHERWISE, 
TO ANY INTELLECTUAL PROPERTY RIGHTS IS GRANTED BY THIS DOCUMENT. 
EXCEPT AS PROVIDED IN INTEL'S TERMS AND CONDITIONS OF SALE FOR SUCH 
PRODUCTS, INTEL ASSUMES NO LIABILITY WHATSOEVER, AND INTEL DISCLAIMS 
ANY EXPRESS OR IMPLIED WARRANTY, RELATING TO SALE AND/OR USE OF 
INTEL PRODUCTS INCLUDING LIABILITY OR WARRANTIES RELATING TO FITNESS 
FOR A PARTICULAR PURPOSE, MERCHANTABILITY, OR INFRINGEMENT OF ANY 
PATENT, COPYRIGHT OR OTHER INTELLECTUAL PROPERTY RIGHT. 
UNLESS OTHERWISE AGREED IN WRITING BY INTEL, THE INTEL PRODUCTS ARE 
NOT DESIGNED NOR INTENDED FOR ANY APPLICATION IN WHICH THE FAILURE 
OF THE INTEL PRODUCT COULD CREATE A SITUATION WHERE PERSONAL INJURY 
OR DEATH MAY OCCUR.
Intel may make changes to specifications and product descriptions at 
any time, without notice. Designers must not rely on the absence or 
characteristics of any features or instructions marked "reserved" or 
"undefined." Intel reserves these for future definition and shall 
have no responsibility whatsoever for conflicts or incompatibilities 
arising from future changes to them. The information here is subject 
to change without notice. Do not finalize a design with this 
information. 
The products described in this document may contain design defects or 
errors known as errata which may cause the product to deviate from 
published specifications. Current characterized errata are available 
on request. 
Contact your local Intel sales office or your distributor to obtain 
the latest specifications and before placing your product order.
Copies of documents which have an order number and are referenced in 
this document, or other Intel literature, may be obtained by calling 
1-800-548-4725, or by visiting Intel's Web Site, located at 
http://www.intel.com/.

Intel processor numbers are not a measure of performance. Processor 
numbers differentiate features within each processor family, not 
across different processor families. See 
http://www.intel.com/products/processor_number for details.

BunnyPeople, Celeron, Celeron Inside, Centrino, Centrino logo, Core 
Inside, FlashFile, i960, InstantIP, Intel, Intel logo, Intel386, 
Intel486, Intel740, IntelDX2, IntelDX4, IntelSX2, Intel Core, Intel 
Inside, Intel Inside logo, Intel. Leap ahead., Intel. Leap ahead. logo, 
Intel NetBurst, Intel NetMerge, Intel NetStructure, Intel SingleDriver, 
Intel SpeedStep, Intel StrataFlash, Intel Viiv, Intel vPro, Intel 
XScale, IPLink, Itanium, Itanium Inside, MCS, MMX, Oplus, OverDrive, 
PDCharm, Pentium, Pentium Inside, skoool, Sound Mark, The Journey 
Inside, VTune, Xeon, and Xeon Inside are trademarks of Intel 
Corporation in the U.S. and other countries.


* Other names and brands may be claimed as the property of others.


Copyright (C) 2005 - 2008, Intel Corporation. All rights reserved.