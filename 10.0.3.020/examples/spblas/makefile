##******************************************************************************
##                              INTEL CONFIDENTIAL
##  Copyright(C) 2005-2008 Intel Corporation. All Rights Reserved.
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
##      Intel(R) Math Kernel Library SPARSE BLAS examples creation and run
##******************************************************************************

help:
	@echo Usage: make {lib32/so32/lib64/so64/libem64t/soem64t} [function=name]
	@echo [compiler=compiler_name] [interface=interface_name] [threading=threading_name]
	@echo
	@echo name     - function name. Please see macro SPBLAS below
	@echo
	@echo compiler_name     - can be gnu or intel. Default value is intel.
	@echo                     Intel \(R\) Fortran and C Compilers as default.
	@echo                     If compiler=gnu then GNU gfortran compiler will be used.
	@echo                     To use g77 add FC=g77. FC=g77 cannot be used with ilp64 interface.
	@echo
	@echo interface_name    - can be lp64 or ilp64 for em64t and ia64. Default value is lp64.
	@echo
	@echo threading_name    - can be parallel or sequential. Default value is parallel.

##------------------------------------------------------------------------------
## examples of using:
##
## make lib32 function=dcoo - build by  Intel(R) Fortran Compiler  (as default) and
##                            run DCOO example for  32-bit applications,  static
##                            linking
##
## make so32 compiler=gnu       - build by GNU Fortran compiler and run all examples of
##                         MKL for 32-bit applications, dynamic linking
##
## make  so64            - build  by Intel(R) Fortran Compiler (as default) and run
##                         all  examples  of MKL for Intel(R) Itanium(R) processor family
##                         applications, dynamic linking
##------------------------------------------------------------------------------

SPBLAS_F = dcoo dcsc dcsr ddia dsky dbsr
SPBLAS_C = cspblas_dbsr cspblas_dcsr cspblas_dcsc cspblas_dcoo

ifndef function
function = $(SPBLAS_C) $(SPBLAS_F)
endif

ifndef compiler
compiler=intel
endif

ifndef interface
interface=lp64
endif

ifndef threading
threading=parallel
endif

RES_C = $(addsuffix .res ,$(SPBLAS_C))
RES_F = $(addsuffix .res ,$(SPBLAS_F))

RES = $(addsuffix .res ,$(function))
MKL_PATH = $(subst examples/spblas,lib/$(_IA),$(PWD))

ifeq ($(compiler),gnu)
CC = gcc
CCOPTS = -w -I../../include
FC=gfortran
FCOPTS = -w -fno-second-underscore -ffixed-line-length-132
ifeq ($(FC),gfortran)
IFACE_COMP_PART=gf
else
IFACE_COMP_PART=intel
endif
IFACE_THREADING_PART=intel
else
override CC = icc
CCOPTS = -w -I../../include
override FC=ifort
FCOPTS = -w -132
IFACE_COMP_PART=intel
IFACE_THREADING_PART=intel
endif
IFACE_COMP_PART=intel
IFACE_THREADING_PART=intel

ifeq ($(interface),ilp64)
IFACE_LIB=$(MKL_PATH)/libmkl_$(IFACE_COMP_PART)_ilp64.$(EXT)
FOPTS = -i8
ifeq ($(compiler),gnu)
override FC=gfortran
FOPTS=-fdefault-integer-8
endif
COPTS= -DMKL_ILP64
else
IFACE_LIB=$(MKL_PATH)/libmkl_$(IFACE_COMP_PART)_lp64.$(EXT)
FOPTS =
endif

ifeq ($(_IA),32)
IFACE_LIB=$(MKL_PATH)/libmkl_$(IFACE_COMP_PART).$(EXT)
FOPTS =
COPTS=
endif

ifeq ($(threading),sequential)
THREADING_LIB=$(MKL_PATH)/libmkl_sequential.$(EXT)
LGUIDE =
else
THREADING_LIB=$(MKL_PATH)/libmkl_$(IFACE_THREADING_PART)_thread.$(EXT)
LGUIDE = $(MKL_PATH)/libguide.$(EXT)
endif

CORE_LIB=$(MKL_PATH)/libmkl_core.$(EXT)

MKL_LIBS=$(IFACE_LIB) -Wl,--start-group $(THREADING_LIB) $(CORE_LIB) -Wl,--end-group $(LGUIDE)

ifeq ($(_IA),32)
RES_DIR=_results/$(compiler)_$(threading)_$(_IA)_$(RES_EXT)$Z
else
RES_DIR=_results/$(compiler)_$(interface)_$(threading)_$(_IA)_$(RES_EXT)$Z
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

vpath %.c source
vpath %.f source

$(RES_C): %.res: %.c
	mkdir -p ./$(RES_DIR)
	$(CC) $(CCOPTS) $(COPTS) $< -L$(MKL_PATH) $(MKL_LIBS) -lpthread -lm -o $(RES_DIR)/$*.out
	export LD_LIBRARY_PATH=$(MKL_PATH):$(LD_LIBRARY_PATH); $(RES_DIR)/$*.out >$(RES_DIR)/$@

$(RES_F): %.res: %.f
	mkdir -p ./$(RES_DIR)
	$(FC) -w $(FCOPTS) $(FOPTS) $< -L$(MKL_PATH) $(MKL_LIBS) -lpthread -o $(RES_DIR)/$*.out
	export LD_LIBRARY_PATH=$(MKL_PATH):$(LD_LIBRARY_PATH); $(RES_DIR)/$*.out >$(RES_DIR)/$@
#-------------------------------------------------------------------------------
