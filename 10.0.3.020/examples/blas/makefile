##******************************************************************************
##                              INTEL CONFIDENTIAL
##  Copyright(C) 2001-2008 Intel Corporation. All Rights Reserved.
##  The source code contained  or  described herein and all documents related to
##  the source code ("Material") are owned by Intel Corporation or its suppliers
##  or licensors.  Title to the  Material remains with  Intel Corporation or its
##  suppliers and licensors. The Material contains trade secrets and proprietary
##  and  confidential  information of  Intel or its suppliers and licensors. The
##  Material  is  protected  by  worldwide  copyright  and trade secret laws and
##  treaty  provisions. No part of the Material may be used, copied, reproduced,
##  modified, published, uploaded, posted, transmitted, distributed or disclosed
##  in any way without Intel's prior express written permission.
##  No license  under any  patent, copyright, trade secret or other intellectual
##  property right is granted to or conferred upon you by disclosure or delivery
##  of the Materials,  either expressly, by implication, inducement, estoppel or
##  otherwise.  Any  license  under  such  intellectual property  rights must be
##  express and approved by Intel in writing.
##
##******************************************************************************
##  Content:
##      Intel(R) Math Kernel Library BLAS examples creation and run
##******************************************************************************

help:
	@echo Usage: make {lib32/so32/lib64/so64/libem64t/soem64t} [function=name]
	@echo [compiler=compiler_name] [interface=interface_name] [threading=threading_name]
	@echo [parallel=parallel_name] [omp=omp_name] [gnu_path=gnu_lib_path]
	@echo
	@echo name           - function name. Please see blas.lst file.
	@echo
	@echo compiler_name  - can be gnu or intel. Default value is intel.
	@echo                  Intel \(R\) Fortran Compiler as default.
	@echo                  If compiler=gnu then GNU gfortran compiler will be used.
	@echo                  To use g77 add FC=g77. FC=g77 cannot be used with ilp64 interface.
	@echo
	@echo interface_name - can be lp64 or ilp64 for em64t and ia64. Default value is lp64.
	@echo
	@echo threading_name - can be parallel or sequential. Default value is parallel.
	@echo
	@echo parallel_name  - can be intel or gnu. Default value is intel.
	@echo
	@echo omp_name       - can be guide or iomp5 if parallel=intel or
	@echo                  can be iomp5 or gomp if parallel=gnu.
	@echo                  Default value is guide in first case and iomp5 in second case.
	@echo
	@echo gnu_lib_path   - If you are using gnu threading layer, specify path to GNU libraries,
	@echo                  such as libgomp and libgfortran, with gnu_path variable.
	@echo                  Default value is /usr/lib.

##------------------------------------------------------------------------------
## examples of using:
##
## make lib32 function=dgemm     - build by Intel(R) Fortran Compiler (as default) and
##                               run DGEMM  example  for  32-bit  applications,
##                               static linking
##
## make so32 compiler=gnu FC=g77 - build by g77 GNU Fortran  compiler  and  run
##                               all examples of MKL for 32-bit applications, dynamic
##                               linking
##
## make lib64 compiler=gnu       - build by gfortran GNU  Fortran  compiler  and  run  all
##                               examples of MKL for Intel(R) Itanium(R) processor family
##                               applications, static linking
##
## make  so64                    - build by Intel(R) Fortran Compiler (as default) and
##                               run all examples of MKL for Intel(R) Itanium(R) processor
##                               family applications, dynamic linking
##------------------------------------------------------------------------------

include blas.lst

ifndef function
   function = $(BLAS)
endif

ifneq ($(compiler),gnu)
   override compiler=intel
endif

ifneq ($(interface),ilp64)
   override interface=lp64
endif

ifneq ($(threading),sequential)
   override threading=parallel
endif

ifneq ($(parallel),gnu)
   override parallel=intel
endif

ifeq ($(parallel),gnu)
   ifneq ($(omp),gomp)
   override omp=iomp5
   endif
else
   ifneq ($(omp),iomp5)
   override omp=guide
   endif
endif

ifndef $(gnu_path)
   gnu_path=/usr/lib
endif

RES = $(addsuffix x.res ,$(function))
MKL_PATH = $(subst examples/blas,lib/$(_IA),$(PWD))

ifeq ($(interface),ilp64)
   IFACE_LIB=$(MKL_PATH)/libmkl_$(IFACE_COMP_PART)_ilp64.$(EXT)
   FOPTS = -i8
   ifeq ($(compiler),gnu)
      override FC=gfortran
      FOPTS=-fdefault-integer-8
   endif
else
   IFACE_LIB=$(MKL_PATH)/libmkl_$(IFACE_COMP_PART)_lp64.$(EXT)
   FOPTS =
endif

ifeq ($(compiler),gnu)
   FC=gfortran
   ifeq ($(FC),gfortran)
      IFACE_COMP_PART=gf
   else
      IFACE_COMP_PART=intel
   endif
else
   override FC=ifort
   IFACE_COMP_PART=intel
endif

ifeq ($(parallel),gnu)
   IFACE_THREADING_PART=gnu
   GFORTRAN_LIB=-L$(gnu_path) -lgfortran
else
   IFACE_THREADING_PART=intel
   GFORTRAN_LIB=
endif

ifeq ($(_IA),32)
   IFACE_LIB=$(MKL_PATH)/libmkl_$(IFACE_COMP_PART).$(EXT)
   FOPTS =
endif

ifeq ($(threading),sequential)
   THREADING_LIB=$(MKL_PATH)/libmkl_sequential.$(EXT)
   OMP_LIB =
   GFORTRAN_LIB=
else
   THREADING_LIB=$(MKL_PATH)/libmkl_$(IFACE_THREADING_PART)_thread.$(EXT)
   ifeq ($(omp),gomp)
      OMP_LIB = -L$(gnu_path) -l$(omp)
   else
      OMP_LIB = -L$(MKL_PATH) -l$(omp)
   endif
endif

CORE_LIB=$(MKL_PATH)/libmkl_core.$(EXT)

MKL_LIBS=$(IFACE_LIB) $(THREADING_LIB) $(CORE_LIB) $(OMP_LIB)

ifeq ($(_IA),32)
   ifeq ($(threading),parallel)
      RES_DIR=_results/$(compiler)_$(threading)_$(parallel)_$(omp)_$(_IA)_$(RES_EXT)$Z
   else
   RES_DIR=_results/$(compiler)_$(threading)_$(_IA)_$(RES_EXT)$Z
   endif
else
   ifeq ($(threading),parallel)
      RES_DIR=_results/$(compiler)_$(interface)_$(threading)_$(parallel)_$(omp)_$(_IA)_$(RES_EXT)$Z
   else
      RES_DIR=_results/$(compiler)_$(interface)_$(threading)_$(_IA)_$(RES_EXT)$Z
   endif
endif

lib32:
	$(MAKE) $(RES) EXT=a _IA=32 RES_EXT=lib
so32:
	$(MAKE) $(RES) EXT=so _IA=32 RES_EXT=so
libem64t:
	$(MAKE) $(RES) EXT=a _IA=em64t RES_EXT=lib
soem64t:
	$(MAKE) $(RES) EXT=so _IA=em64t RES_EXT=so
lib64:
	$(MAKE) $(RES) EXT=a _IA=64 RES_EXT=lib
so64:
	$(MAKE) $(RES) EXT=so _IA=64 RES_EXT=so

#-------------------------------------------------------------------------------

vpath %.f source

$(RES): %.res: %.f
	mkdir -p ./$(RES_DIR)
	export LD_LIBRARY_PATH=$(MKL_PATH):$(LD_LIBRARY_PATH):$(gnu_path); $(FC) $(FOPTS) -w $< source/common_func.f $(MKL_LIBS) -lpthread $(GFORTRAN_LIB) -o $(RES_DIR)/$*.out
	export LD_LIBRARY_PATH=$(MKL_PATH):$(LD_LIBRARY_PATH):$(gnu_path); $(RES_DIR)/$*.out <data/$*.d >$(RES_DIR)/$@
#-------------------------------------------------------------------------------
