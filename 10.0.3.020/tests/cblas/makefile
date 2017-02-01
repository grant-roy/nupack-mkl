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
##      Intel(R) Math Kernel Library CBLAS tests creation and run
##******************************************************************************

help:
	@echo Usage: make {lib32/so32/lib64/so64/libem64t/soem64t} [test=name]
	@echo [compiler=compiler_name] [interface=interface_name] [threading=threading_name]
	@echo
	@echo name     - test name. Please see cblas.lst file
	@echo
	@echo compiler_name     - can be gnu or intel. Default value is intel.
	@echo                     Intel \(R\) Fortran Compiler as default.
	@echo                     If compiler=gnu then GNU gfortran compiler will be used.
	@echo                     To use g77 add FC=g77. FC=g77 cannot be used with ilp64 interface.
	@echo
	@echo interface_name    - can be lp64 or ilp64 for em64t and ia64. Default value is lp64.
	@echo
	@echo threading_name    - can be parallel or sequential. Default value is parallel.

##------------------------------------------------------------------------------
## examples of using:
##
## make lib32 test=dblat3      - build  by  Intel(R) Fortran Compiler  (as default)
##                               and  run  DBLAT3 test for 32-bit  applications,
##                               static linking
##
## make so32 compiler=gnu             - build by GNU Fortran compiler and run all tests
##                               of MKL for 32-bit applications, dynamic linking
##
## make lib64 compiler=gnu            - build by GNU Fortran compiler and run all tests
##                               of MKL for  Intel(R) Itanium(R)  processor family
##                               applications, static linking
##
## make so64                   - build  by  Intel(R) Fortran Compiler  (as default)
##                               and  run  all  tests  of  MKL   for  Intel(R) Itanium(R)
##                               processor family applications, dynamic linking
##------------------------------------------------------------------------------

include cblas.lst

ifndef test
test = $(CBLAS)
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

RES = $(addsuffix .res,$(test))
MKL_PATH = $(subst tests/cblas,lib/$(_IA),$(PWD))

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

MKL_LIBS=$(IFACE_LIB) $(THREADING_LIB) $(CORE_LIB) $(LGUIDE)

ifeq ($(_IA),32)
RES_DIR=_results/$(compiler)_$(threading)_$(_IA)_$(RES_EXT)$Z
else
RES_DIR=_results/$(compiler)_$(interface)_$(threading)_$(_IA)_$(RES_EXT)$Z
endif

lib32:
	$(MAKE) $(RES) _IA=32    EXT=a  RES_EXT=lib PLATF=ia32
so32:
	$(MAKE) $(RES) _IA=32    EXT=so RES_EXT=so  PLATF=ia32
libem64t:
	$(MAKE) $(RES) _IA=em64t EXT=a  RES_EXT=lib PLATF=em64t
soem64t:
	$(MAKE) $(RES) _IA=em64t EXT=so RES_EXT=so  PLATF=em64t
lib64:
	$(MAKE) $(RES) _IA=64    EXT=a  RES_EXT=lib PLATF=ipf
so64:
	$(MAKE) $(RES) _IA=64    EXT=so RES_EXT=so  PLATF=ipf

#-------------------------------------------------------------------------------

vpath %.f ../blas/source

$(RES): %.res: %.f
	mkdir -p ./$(RES_DIR)
	$(FC) $(FOPTS) -w -O0 $< source/libcwrap_$(PLATF).a -L$(MKL_PATH) $(MKL_LIBS) -lpthread -o $(RES_DIR)/$*.out
	export LD_LIBRARY_PATH=$(MKL_PATH):$(LD_LIBRARY_PATH); if [ -e ../blas/in/$*.in ]; \
	then $(RES_DIR)/$*.out <../blas/in/$*.in >$(RES_DIR)/$@; \
	else $(RES_DIR)/$*.out >$(RES_DIR)/$@; fi
	if [ -e $*.sum ]; then cp -f $*.sum $(RES_DIR)/$@; fi
	rm -f $*.s* $(RES_DIR)/*.o
#-------------------------------------------------------------------------------
