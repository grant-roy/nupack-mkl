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
##
##  Content:
##      Intel(R) Math Kernel Library
##      Interval Interval Arithmetic Solver (IAS) examples building and running
##
##******************************************************************************

help:
	@echo Usage: make {lib32/so32/lib64/so64/libem64t/soem64t} [function=name]
	@echo [threading=threading_name]
	@echo name     - function name, look at the file ias.lst for details
	@echo            Intel\(R\) Fortran Compiler is used by default
	@echo
	@echo threading_name    - can be parallel or sequential. Default value is parallel.

##------------------------------------------------------------------------------
##
## examples of using:
##
## make lib32 function=digegas - build by Intel(R) Fortran Compiler (as default)
##                               and run a DIGEGAS example for 32-bit
##                               applications, static linking
##
## make lib64                  - build  and  run  all  the  examples  of  MKL
##                               for Intel(R) Itanium(R) processor family applications,
##                               static linking
##
##------------------------------------------------------------------------------

include ias.lst

ifndef function
function = $(IAS)
endif

ifndef compiler
compiler=intel
endif
override compiler=intel

ifndef interface
interface=lp64
endif
override interface=lp64

ifndef threading
threading=parallel
endif

RES = $(addsuffix x.res ,$(function))
MOD = $(addsuffix .mod ,$(INTERVAL_IFACE))

MKL_PATH = $(subst examples/interval,lib/$(_IA),$(PWD))
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
SOLVER_IFACE=_lp64
endif

ifeq ($(_IA),32)
IFACE_LIB=$(MKL_PATH)/libmkl_$(IFACE_COMP_PART).$(EXT)
FOPTS =
SOLVER_IFACE=
endif

ifeq ($(threading),sequential)
THREADING_LIB=$(MKL_PATH)/libmkl_sequential.$(EXT)
SOLVER_THREAD=_sequential
LGUIDE =
else
THREADING_LIB=$(MKL_PATH)/libmkl_$(IFACE_THREADING_PART)_thread.$(EXT)
SOLVER_THREAD=
LGUIDE = $(MKL_PATH)/libguide.$(EXT)
endif

SOLVER_LIB=$(MKL_PATH)/libmkl_solver$(SOLVER_IFACE)$(SOLVER_THREAD).a
CORE_LIB=$(MKL_PATH)/libmkl_core.$(EXT)

MKL_LIBS=$(SOLVER_LIB) $(IFACE_LIB) -Wl,--start-group $(THREADING_LIB) $(CORE_LIB) -Wl,--end-group $(LGUIDE)

ifeq ($(_IA),32)
RES_DIR=_results/$(compiler)_$(threading)_$(_IA)_$(RES_EXT)$Z
else
RES_DIR=_results/$(compiler)_$(interface)_$(threading)_$(_IA)_$(RES_EXT)$Z
endif

lib32: clean
	$(MAKE) $(MOD) $(RES) _IA=32 EXT=a RES_EXT=lib
so32: clean
	$(MAKE) $(MOD) $(RES) _IA=32 EXT=so RES_EXT=so
libem64t: clean
	$(MAKE) $(MOD) $(RES) _IA=em64t EXT=a RES_EXT=lib
soem64t: clean
	$(MAKE) $(MOD) $(RES) _IA=em64t EXT=so RES_EXT=so
lib64: clean
	$(MAKE) $(MOD) $(RES) _IA=64 EXT=a RES_EXT=lib
so64: clean
	$(MAKE) $(MOD) $(RES) _IA=64 EXT=so RES_EXT=so
clean:
	rm -f *.mod *.o

#-------------------------------------------------------------------------------


vpath %.f90 source
vpath %.f90 ../../include

$(MOD): %.mod: %.f90
	$(FC) $(FOPTS) -w -c $<

$(RES): %.res: %.f90
	mkdir -p ./$(RES_DIR)
	$(FC) $(FOPTS) -w $< -L$(MKL_PATH) $(MKL_LIBS) -lpthread -o $(RES_DIR)/$*.out
	export LD_LIBRARY_PATH=$(MKL_PATH):$(LD_LIBRARY_PATH); $(RES_DIR)/$*.out <data/$*.d >$(RES_DIR)/$@

#-------------------------------------------------------------------------------
