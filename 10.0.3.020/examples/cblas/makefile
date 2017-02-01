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
##      Intel(R) Math Kernel Library CBLAS examples creation and run
##******************************************************************************

help:
	@echo Usage: make {lib32/so32/lib64/so64/libem64t/soem64t} [function=name]
	@echo [compiler=compiler_name] [interface=interface_name] [threading=threading_name]
	@echo
	@echo name     - function name. Please see cblas.lst file
	@echo
	@echo compiler_name     - can be gnu or intel. Default value is intel.
	@echo            Intel\(R\) C Compiler as default
	@echo
	@echo interface_name    - can be lp64 or ilp64 for em64t and ia64. Default value is lp64.
	@echo
	@echo threading_name    - can be parallel or sequential. Default value is parallel.

##------------------------------------------------------------------------------
## examples of using:
##
## make lib32 function=cblas_dgemm  - build  by  Intel(R) C Compiler  (as default)
##                                    and  run  CBLAS_DGEMM  example  for 32-bit
##                                    applications, static linking
##
## make so32 compiler=gnu                  - build  by  GNU  C  compiler  and  run  all
##                                    examples of MKL  for 32-bit  applications,
##                                    dynamic linking
##
## make lib64 compiler=gnu                 - build  by  GNU  C  compiler  and  run  all
##                                    examples  of MKL  for Intel(R) Itanium(R) processor
##                                    family applications, static linking
##
## make so64                        - build  by  Intel(R) C Compiler  (as default)
##                                    and run all examples of MKL for Intel(R) Itanium(R)
##                                    processor  family  applications,   dynamic
##                                    linking
##------------------------------------------------------------------------------

include cblas.lst

ifndef function
function = $(CBLAS)
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

RES = $(addsuffix x.res ,$(function))

MKL_PATH = $(subst examples/cblas,lib/$(_IA),$(PWD))
ifeq ($(compiler),gnu)
CC=gcc
IFACE_COMP_PART=intel
IFACE_THREADING_PART=intel
else
override CC=icc
IFACE_COMP_PART=intel
IFACE_THREADING_PART=intel
endif

ifeq ($(interface),ilp64)
IFACE_LIB=$(MKL_PATH)/libmkl_$(IFACE_COMP_PART)_ilp64.$(EXT)
COPTS = -DMKL_ILP64
else
IFACE_LIB=$(MKL_PATH)/libmkl_$(IFACE_COMP_PART)_lp64.$(EXT)
COPTS =
endif

ifeq ($(_IA),32)
IFACE_LIB=$(MKL_PATH)/libmkl_$(IFACE_COMP_PART).$(EXT)
COPTS =
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
	$(MAKE) $(RES) _IA=32 EXT=a RES_EXT=lib
so32:
	$(MAKE) $(RES) _IA=32 EXT=so RES_EXT=so
libem64t:
	$(MAKE) $(RES) _IA=em64t EXT=a RES_EXT=lib
soem64t:
	$(MAKE) $(RES) _IA=em64t EXT=so RES_EXT=so
lib64:
	$(MAKE) $(RES) _IA=64 EXT=a RES_EXT=lib
so64:
	$(MAKE) $(RES) _IA=64 EXT=so RES_EXT=so

#-------------------------------------------------------------------------------

vpath %.c source

$(RES): %.res: %.c
	mkdir -p ./$(RES_DIR)
	$(CC) $(COPTS) -w -I../../include $< source/common_func.c -L$(MKL_PATH) $(MKL_LIBS) -lpthread -lm -o $(RES_DIR)/$*.out
	export LD_LIBRARY_PATH=$(MKL_PATH):$(LD_LIBRARY_PATH); $(RES_DIR)/$*.out data/$*.d >$(RES_DIR)/$@
#-------------------------------------------------------------------------------
