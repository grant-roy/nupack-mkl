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
##      Intel(R) Math Kernel Library Fortran-style DFT examples creation and run
##******************************************************************************

help:
	@echo Usage: make {lib32/so32/lib64/so64/libem64t/soem64t} [function=name]
	@echo [compiler=compiler_name] [interface=interface_name] [threading=threading_name]
	@echo
	@echo name              - function name. Please see dftf.lst file
	@echo
	@echo compiler_name     - can be gnu or intel. Default value is intel.
	@echo                     Intel \(R\) Fortran Compiler as default.
	@echo                     If compiler=gnu then GNU gfortran compiler will be used.
	@echo
	@echo interface_name    - can be lp64 or ilp64 for em64t and ia64. Default value is lp64.
	@echo
	@echo threading_name    - can be parallel or sequential. Default value is parallel.

##------------------------------------------------------------------------------
## examples of using:
##
## make lib32 function=complex_1d_double_ex1   - build by Intel(R) Fortran Compiler
##                                               (as    default)     and     run
##                                               COMPLEX_1D_DOUBLE_EX1   example
##                                               for 32-bit applications, static
##                                               linking
##
## make so32              - build by Intel(R) Fortran Compiler (as default) and run
##                          examples  of MKL  for  32-bit  applications, dynamic
##                          linking
##
## make lib64             - build by Intel(R) Fortran Compiler (as default) and run
##                          all examples of MKL  for Intel(R) Itanium(R) processor family
##                          applications, static linking
##
## make so64              - build by Intel(R) Fortran Compiler (as default) and run
##                          all examples of MKL  for Intel(R) Itanium(R) processor family
##                          applications, dynamic linking
##------------------------------------------------------------------------------

include dftf.lst

ifndef function
function = $(DFT)
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

COMMON = $(addsuffix .o ,$(SUPPORT))

RES = $(addsuffix .res ,$(function))
MKL_PATH = $(subst examples/dftf,lib/$(_IA),$(PWD))
ifeq ($(compiler),gnu)
FC=gfortran
ifeq ($(FC),gfortran)
IFACE_COMP_PART=gf
else
IFACE_COMP_PART=intel
endif
IFACE_THREADING_PART=intel
else
override FC=ifort
IFACE_COMP_PART=intel
IFACE_THREADING_PART=intel
endif

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

MKL_LIBS=$(IFACE_LIB) -Wl,--start-group $(THREADING_LIB) $(CORE_LIB) -Wl,--end-group $(LGUIDE)

ifeq ($(_IA),32)
RES_DIR=_results/$(compiler)_$(threading)_$(_IA)_$(RES_EXT)$Z
else
RES_DIR=_results/$(compiler)_$(interface)_$(threading)_$(_IA)_$(RES_EXT)$Z
endif

del_obj:
	rm -fr *.o *.mod
lib32: del_obj
	$(MAKE) $(COMMON) $(RES) _IA=32 EXT=a RES_EXT=lib
so32: del_obj
	$(MAKE) $(COMMON) $(RES) _IA=32 EXT=so RES_EXT=so
libem64t: del_obj
	$(MAKE) $(COMMON) $(RES) _IA=em64t EXT=a RES_EXT=lib
soem64t: del_obj
	$(MAKE) $(COMMON) $(RES) _IA=em64t EXT=so RES_EXT=so
lib64: del_obj
	$(MAKE) $(COMMON) $(RES) _IA=64 EXT=a RES_EXT=lib
so64: del_obj
	$(MAKE) $(COMMON) $(RES) _IA=64 EXT=so RES_EXT=so

#-------------------------------------------------------------------------------

vpath %.f90 ../../include
vpath %.f90 source

$(COMMON): %.o: %.f90
	mkdir -p ./$(RES_DIR)
	$(FC) $(FOPTS) -w -c $< -o $@

$(RES): %.res: %.f90
	mkdir -p ./$(RES_DIR)
	$(FC) $(FOPTS) -w $(COMMON) $< -L$(MKL_PATH) $(MKL_LIBS) -lpthread -o $(RES_DIR)/$*.out
	export LD_LIBRARY_PATH=$(MKL_PATH):$(LD_LIBRARY_PATH); $(RES_DIR)/$*.out <data/$*.d >$(RES_DIR)/$@
#-------------------------------------------------------------------------------
