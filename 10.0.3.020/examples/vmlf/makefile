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
##      Intel(R) Math Kernel Library VML examples creation and run
##******************************************************************************

help:
	@echo Usage: make {lib32/so32/lib64/so64/libem64t/soem64t} [function=name]
	@echo [interface=interface_name] [threading=threading_name]
	@echo name     - function name. Please see vml.lst file
	@echo                     Intel \(R\) Fortran compiler as default.
	@echo interface_name    - can be lp64 or ilp64 for em64t and ia64. Default value is lp64.
	@echo threading_name    - can be parallel or sequential. Default value is parallel.

##------------------------------------------------------------------------------
## examples of using:
##
## make lib32 function=vssin - build by Intel(R) Fortran compiler (as default)
##                             and run VSSIN example for 32-bit applications,
##                             static linking
##
## make so32 - build by Intel(R) Fortran compiler (as default) and run all examples of MKL
##             for 32-bit applications, dynamic linking
##
## make lib64 - build by Intel(R) Fortran compiler (as default) and run all examples of MKL
##              for Intel(R) Itanium(R) processor family applications, static
##              linking
##
## make so64 - build by Intel(R) Fortran compiler (as default) and run all examples of MKL
##             for Intel(R) Itanium(R) processor family applications, dynamic
##             linking
##------------------------------------------------------------------------------

include vml.lst

ifndef function
function = $(VML)
endif

ifndef compiler
compiler=intel
endif

ifndef interface
interface=lp64
endif

ifndef threading
threading=parralel
endif

RES = $(addsuffix .res ,$(function))
MKL_PATH = $(subst examples/vmlf,lib/$(_IA),$(PWD))

#ifeq ($(compiler),gnu)
FC=ifort
IFACE_COMP_PART=intel
IFACE_THREADING_PART=intel
#endif

ifeq ($(interface),ilp64)
IFACE_LIB=$(MKL_PATH)/libmkl_$(IFACE_COMP_PART)_ilp64.$(EXT)
FOPTS = -w -w95 -fpp1 -vec-report0 -i8
else
IFACE_LIB=$(MKL_PATH)/libmkl_$(IFACE_COMP_PART)_lp64.$(EXT)
FOPTS = -w -w95 -fpp1 -vec-report0
ifeq ($(_IA),32)
IFACE_LIB=$(MKL_PATH)/libmkl_$(IFACE_COMP_PART).$(EXT)
FOPTS = -w -w95 -fpp1 -vec-report0
endif
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
RES_DIR_ESSL=_results_essl/$(compiler)_$(threading)_$(_IA)_$(RES_EXT)$Z
else
RES_DIR=_results/$(compiler)_$(interface)_$(threading)_$(_IA)_$(RES_EXT)$Z
RES_DIR_ESSL=_results_essl/$(compiler)_$(interface)_$(threading)_$(_IA)_$(RES_EXT)$Z
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

%.res: ./source/%.f
	mkdir -p ./$(RES_DIR)
	$(FC) $(FOPTS) -module ./$(RES_DIR) -I../../include $< -o $(RES_DIR)/$*.out $(MKL_LIBS) -lpthread -lm
	export LD_LIBRARY_PATH=$(MKL_PATH):$(LD_LIBRARY_PATH); $(RES_DIR)/$*.out >$(RES_DIR)/$@
#-------------------------------------------------------------------------------
