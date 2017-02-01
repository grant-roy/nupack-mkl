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
	@echo [compiler=compiler_name]
	@echo
	@echo compiler_name     - can be gnu or intel. Default value is intel.
	@echo            Intel\(R\) C Compiler as default

##------------------------------------------------------------------------------
## examples of using:
##
## make lib64 compiler=gnu - compile by GNU C compiler for double precision data
##                           and build library for Intel(R) Itanium(R)-based applications
##
## make lib64 PRECISION=MKL_SINGLE compiler=gnu  - compile  by GNU C compiler for float
##                                          precision data and build library for
##                                          Intel(R) Itanium(R)-based applications
##
## make libem64t              - compile by Intel(R) C Compiler  and  build  library
##                              for Intel(R) EM64T processor family applications
##------------------------------------------------------------------------------

include fftw2xf.lst

ifneq ($(PRECISION),MKL_SINGLE)
override PRECISION=MKL_DOUBLE
endif

WRP = $(addsuffix .o ,$(WRAP))

ifeq ($(compiler),gnu)
override CC=gcc -D_GNU
else
override compiler=intel
override CC=icc
endif

lib32:
	$(MAKE) clean ../../lib/32/libfftw2xf_$(compiler).a _IA=32
libem64t:
	$(MAKE) clean ../../lib/em64t/libfftw2xf_$(compiler).a _IA=em64t
lib64:
	$(MAKE) clean ../../lib/64/libfftw2xf_$(compiler).a _IA=64

../../lib/$(_IA)/libfftw2xf_$(compiler).a: $(WRP)
	ar rs ../../lib/$(_IA)/libfftw2xf_$(compiler).a $^
	rm -f *.o
clean:
	rm -f ../../lib/$(_IA)/libfftw2xf_$(compiler).a *.o

#-------------------------------------------------------------------------------

vpath %.c wrappers

$(WRP): %.o: %.c
	$(CC) $(COPTS) -c -w -D$(PRECISION) -I../../include -I../../include/fftw $< -o $@

#-------------------------------------------------------------------------------
