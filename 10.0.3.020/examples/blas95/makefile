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
##      Intel(R) Math Kernel Library BLAS95 examples creation and run
##******************************************************************************

help:
	@echo Usage: make {lib32/so32/lib64/so64/libem64t/soem64t} [function=name]
	@echo [interface=interface_name] [threading=threading_name]
	@echo
	@echo name              - function name. Please see blas95.lst file
	@echo
	@echo Intel\(R\) Fortran Compiler as default
	@echo
	@echo interface_name    - can be lp64 or ilp64 for em64t and ia64. Default value is lp64.
	@echo
	@echo threading_name    - can be parallel or sequential. Default value is parallel.

##------------------------------------------------------------------------------
## examples of using:
##
## make lib32         - build by Intel(R) Fortran Compiler (as default) and run all
##                      examples of MKL for 32-bit applications, static linking
##
## make lib64         - build  by Intel(R) Fortran Compiler  (as default)  and  run
##                      all  examples  of  MKL  for Intel(R) Itanium(R)  processor family
##                      applications, static linking
##------------------------------------------------------------------------------

include blas95.lst

ifndef function
function = $(SRC_F90)
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

RES = $(addsuffix .res ,$(function))
MOD = $(addsuffix .mod ,$(IFACE_F90))

MKL_PATH = $(subst examples/blas95,lib/$(_IA),$(PWD))

override FC=ifort
IFACE_COMP_PART=intel
IFACE_THREADING_PART=intel

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

ifeq ($(_IA),32)
IFACE_LIB=$(MKL_PATH)/libmkl_$(IFACE_COMP_PART).$(EXT)
FOPTS =
endif

ifeq ($(threading),sequential)
THREADING_LIB=$(MKL_PATH)/libmkl_sequential.$(EXT)
LGUIDE =
else
THREADING_LIB=$(MKL_PATH)/libmkl_$(IFACE_THREADING_PART)_thread.$(EXT)
LGUIDE = $(MKL_PATH)/libguide.$(EXT)
endif

CORE_LIB=$(MKL_PATH)/libmkl_core.$(EXT)

MKL_LIBS=$(IFACE_LIB) $(THREADING_LIB) $(CORE_LIB) $(LGUIDE)

ifeq ($(_IA),32)
RES_DIR=_results/$(compiler)_$(threading)_$(_IA)_$(RES_EXT)$Z
else
RES_DIR=_results/$(compiler)_$(interface)_$(threading)_$(_IA)_$(RES_EXT)$Z
endif

.PHONY: ../../lib/32/libmkl_blas95.a ../../lib/em64t/libmkl_blas95.a ../../lib/64/libmkl_blas95.a

lib32:	../../lib/32/libmkl_blas95.a
	$(MAKE) $(MOD) $(RES) EXT=a _IA=32 RES_EXT=lib
	rm -f *.mod *.o
so32:	../../lib/32/libmkl_blas95.a
	$(MAKE) $(MOD) $(RES) EXT=so _IA=32 RES_EXT=so
	rm -f *.mod *.o
libem64t:../../lib/em64t/libmkl_blas95.a
	$(MAKE) $(MOD) $(RES) EXT=a _IA=em64t RES_EXT=lib
	rm -f *.mod *.o
soem64t:../../lib/em64t/libmkl_blas95.a
	$(MAKE) $(MOD) $(RES) EXT=so _IA=em64t RES_EXT=so
	rm -f *.mod *.o
lib64:	../../lib/64/libmkl_blas95.a
	$(MAKE) $(MOD) $(RES) EXT=a _IA=64 RES_EXT=lib
	rm -f *.mod *.o
so64:	../../lib/64/libmkl_blas95.a
	$(MAKE) $(MOD) $(RES) EXT=so _IA=64 RES_EXT=so
	rm -f *.mod *.o

../../lib/32/libmkl_blas95.a:
	cd ../../interfaces/blas95; make lib32

../../lib/em64t/libmkl_blas95.a:
	cd ../../interfaces/blas95; make libem64t interface=$(interface)

../../lib/64/libmkl_blas95.a:
	cd ../../interfaces/blas95; make lib64 interface=$(interface)

#-------------------------------------------------------------------------------

vpath %.f90 source
vpath %.f90 ../../include

$(MOD): %.mod: %.f90
	mkdir -p ./$(RES_DIR)
	$(FC) $(FOPTS) -w -c $<

$(RES): %.res: %.f90
	mkdir -p ./$(RES_DIR)
	$(FC) $(FOPTS) -w $< source/common_func.f -L$(MKL_PATH) -lmkl_blas95 $(MKL_LIBS) -lpthread -o $(RES_DIR)/$(*F).out
	export LD_LIBRARY_PATH=$(MKL_PATH):$(LD_LIBRARY_PATH); $(RES_DIR)/$(*F).out <data/$(*F).d >$(RES_DIR)/$(@F)
#-------------------------------------------------------------------------------
