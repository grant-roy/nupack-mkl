##******************************************************************************
##                              INTEL CONFIDENTIAL
##  Copyright(C) 2006-2008 Intel Corporation. All Rights Reserved.
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
##      Wrapper library building.
##      This library allows to use MKL DFTI routines through FFTW interface.
##******************************************************************************

help:
	@echo Usage: make {lib32/lib64/libem64t} [PRECISION=MKL_DOUBLE/MKL_SINGLE]
	@echo [function=name] [compiler=compiler_name] [threading=threading_name]
	@echo
	@echo name           - function name. Please see fftw2xf.lst file
	@echo
	@echo compiler_name  - can be gnu or intel. Default value is intel.
	@echo                  Intel \(R\) Fortran Compiler as default,
	@echo                  Intel\(R\) C Compiler will be used to build wrappers library.
	@echo                  If compiler=gnu then GNU gfortran compiler will be used.
	@echo                  To use g77 add FC=g77.
	@echo
	@echo threading_name - can be parallel or sequential. Default value is parallel.
	@echo PRECISION=MKL_DOUBLE - for double precision data, default
	@echo PRECISION=MKL_SINGLE - for single precision data

##------------------------------------------------------------------------------
## examples of using:
##
## make lib64 compiler=gnu        - compile  by GNU Fortran compiler for double precision data and  build library  for
##                           Intel(R) Itanium(R)-based applications
##
## make lib64 PRECISION=MKL_SINGLE compiler=gnu  - compile  by GNU Fortran compiler for float precision data and  build library  for
##                           Intel(R) Itanium(R)-based applications
##
## make libem64t           - compile by Intel(R) Fortran Compiler and  build  library  for  Intel(R) EM64T  processor
##                           family applications
##------------------------------------------------------------------------------

include fftw2xf.lst

ifneq ($(PRECISION),MKL_SINGLE)
override PRECISION = MKL_DOUBLE
T = _double
else
T = _single
endif

ifndef function
function = $(DFT_$(PRECISION))
endif

ifneq ($(compiler),gnu)
override compiler=intel
endif

ifndef threading
threading = parallel
endif

COMMON = source/mkl_fftw_examples_support.f

RES = $(addsuffix .res ,$(function))
MKL_PATH = $(subst examples/fftw2xf,lib/$(_IA),$(PWD))

ifeq ($(compiler),gnu)
FC=gfortran
IFACE_COMP_PART=intel
IFACE_THREADING_PART=intel
else
override FC=ifort
IFACE_COMP_PART=intel
IFACE_THREADING_PART=intel
endif

ifeq ($(FC),gfortran)
FCOPTS = -w -D$(PRECISION) -I../../include -I../../include/fftw -I./source -fsecond-underscore
else
FCOPTS = -w -D$(PRECISION) -I../../include -I../../include/fftw
endif

ifeq ($(_IA),32)
IFACE_LIB=$(MKL_PATH)/libmkl_$(IFACE_COMP_PART).$(EXT)
else
IFACE_LIB=$(MKL_PATH)/libmkl_$(IFACE_COMP_PART)_lp64.$(EXT)
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

RES_DIR=_results/$(compiler)_$(threading)_$(_IA)_$(RES_EXT)$T$Z

lib32:
	$(MAKE) wrap_lib _IA=32
	$(MAKE) $(RES) _IA=32 EXT=a RES_EXT=lib
libem64t:
	$(MAKE) wrap_lib _IA=em64t
	$(MAKE) $(RES) _IA=em64t EXT=a RES_EXT=lib
lib64:
	$(MAKE) wrap_lib _IA=64
	$(MAKE) $(RES) _IA=64 EXT=a RES_EXT=lib

wrap_lib:
	cd ../../interfaces/fftw2xf; $(MAKE) lib$(_IA) PRECISION=$(PRECISION) compiler=$(compiler)
#-------------------------------------------------------------------------------
vpath %.f source
vpath %.o $(RES_DIR)

$(RES): %.res: %.f
	mkdir -p ./$(RES_DIR)
	$(FC) $(FCOPTS) $(FOPTS) $< $(COMMON) -L$(MKL_PATH) -lfftw2xf_$(compiler) $(MKL_LIBS) -lpthread -lm -o $(RES_DIR)/$*.out
	export LD_LIBRARY_PATH=$(MKL_PATH):$(LD_LIBRARY_PATH); $(RES_DIR)/$*.out >$(RES_DIR)/$@

#-------------------------------------------------------------------------------
