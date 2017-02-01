##******************************************************************************
##                              INTEL CONFIDENTIAL
##  Copyright(C) 2007-2008 Intel Corporation. All Rights Reserved.
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

help:
	@echo
	@echo Usage:
	@echo make {so32/soem64t/so64/lib32/libem64t/lib64} [function=name] [compiler=compiler_name]
	@echo
	@echo Targets:
	@echo so32     - link with dynamic MKL libraries for IA32
	@echo soem64t  - link with dynamic MKL libraries for Intel\(R\) EM64T
	@echo so64     - link with dynamic MKL libraries for Intel\(R\) Itanium\(R\)
	@echo lib32    - link with static MKL libraries for IA32
	@echo libem64t - link with static MKL libraries for Intel\(R\) EM64T
	@echo lib64    - link with static MKL libraries for Intel\(R\) Itanium\(R\)
	@echo
	@echo name     - function name, see examples.lst for function names
	@echo
	@echo compiler_name - can be gnu or intel, Intel\(R\) C Compiler as default.
	@echo

##------------------------------------------------------------------------------
## examples of using:
##
## nmake lib32       - build by Intel(R) C Compiler (as default), static linking
##                     for 32-bit applications
##
## nmake so32 compiler=gnu  - build by GNU C compiler, dynamic linking for 32-bit
##                     applications
##
## nmake lib64 compiler=gnu - build by GNU C compiler, static linking for Intel(R) Itanium(R)
##                     processor family applications
##
## nmake so64        - build by Intel(R) C Compiler (as default), dynamic linking
##                     for  Intel(R) Itanium(R) processor family applications
##------------------------------------------------------------------------------

include examples.lst

ifndef function
function = $(CBLAS) $(ESSL) $(DFTI) $(VSL) $(VML) $(LAPACK)
endif

ifndef compiler
compiler = intel
endif

CC = icc

ifeq ($(compiler),gnu)
CC = gcc $(OPT_M32)
endif

JAVA    = java
JAVAC   = javac
JAVAH   = javah
JAVADOC = javadoc

MKL_ROOT = $(subst examples/java,,$(PWD))
MKL_PATH = $(MKL_ROOT)/lib/$(_IA)

RES = $(addsuffix .res ,$(function))

lib32:
	make build  D=$@ _IA=32 MKL_LIB="-Wl,--start-group $(MKL_ROOT)/lib/32/libmkl_intel.a $(MKL_ROOT)/lib/32/libmkl_intel_thread.a $(MKL_ROOT)/lib/32/libmkl_core.a -Wl,--end-group " OPT_M32=-m32
	make $(RES) D=$@ _IA=32
so32:
	make build  D=$@ _IA=32 MKL_LIB="-lmkl_intel -lmkl_intel_thread -lmkl_lapack -lmkl_core" OPT_M32=-m32
	make $(RES) D=$@ _IA=32
libem64t:
	make build  D=$@ _IA=em64t MKL_LIB="-Wl,--start-group $(MKL_ROOT)/lib/em64t/libmkl_intel_lp64.a $(MKL_ROOT)/lib/em64t/libmkl_intel_thread.a $(MKL_ROOT)/lib/em64t/libmkl_core.a -Wl,--end-group "
	make $(RES) D=$@ _IA=em64t
soem64t:
	make build  D=$@ _IA=em64t MKL_LIB="-lmkl_intel_lp64 -lmkl_intel_thread -lmkl_lapack -lmkl_core"
	make $(RES) D=$@ _IA=em64t
lib64:
	make build  D=$@ _IA=64 MKL_LIB="-Wl,--start-group $(MKL_ROOT)/lib/64/libmkl_intel_lp64.a $(MKL_ROOT)/lib/64/libmkl_intel_thread.a $(MKL_ROOT)/lib/64/libmkl_core.a -Wl,--end-group "
	make $(RES) D=$@ _IA=64
so64:
	make build  D=$@ _IA=64 MKL_LIB="-lmkl_intel_lp64 -lmkl_intel_thread -lmkl_lapack -lmkl_core"
	make $(RES) D=$@ _IA=64

##------------------------------------------------------------------------------

DIR_BIN     = bin/$D_$(compiler)
DIR_CLASSES = classes
DIR_INCLUDE = include
DIR_DOCS    = docs

WRAPPERS_BIN     = $(DIR_BIN)/libmkl_java_stubs.so
WRAPPERS_CLASSES = $(DIR_CLASSES)/com/intel/mkl/CBLAS.class
WRAPPERS_INCLUDE = $(DIR_INCLUDE)/com_intel_mkl_CBLAS.h
EXAMPLES_CLASSES = $(DIR_CLASSES)/dgemm.class
DOCS_INDEX       = $(DIR_DOCS)/index.html

BUILD_ALL = $(WRAPPERS_BIN) $(WRAPPERS_CLASSES) $(WRAPPERS_INCLUDE) $(EXAMPLES_CLASSES) $(DOCS_INDEX)

.PHONY: clean build

build: $(BUILD_ALL)

$(WRAPPERS_BIN): $(WRAPPERS_INCLUDE)
	@echo
	@echo Making libraries
	@echo
	mkdir -p ./$(DIR_BIN)
	$(CC) -shared -fPIC -o $(DIR_BIN)/libmkl_java_stubs.so \
	    wrappers/*.c $(MKL_ROOT)/examples/vslc/essl/vsl_wrappers/*.c \
	    -Iwrappers -I$(MKL_ROOT)/examples/vslc/essl/vsl_wrappers \
	    -I$(DIR_INCLUDE) -I$(MKL_ROOT)/include \
	    -I$(JAVA_HOME)/include -I$(JAVA_HOME)/include/linux \
	    -L$(MKL_PATH) $(MKL_LIB) -lguide -lpthread -lm

$(WRAPPERS_INCLUDE): $(WRAPPERS_CLASSES)
	@echo
	@echo Making include headers for C/C++
	@echo
	mkdir -p ./$(DIR_INCLUDE)
	$(JAVAH) -d $(DIR_INCLUDE) -classpath $(DIR_CLASSES) com.intel.mkl.CBLAS
	$(JAVAH) -d $(DIR_INCLUDE) -classpath $(DIR_CLASSES) com.intel.mkl.ESSL
	$(JAVAH) -d $(DIR_INCLUDE) -classpath $(DIR_CLASSES) com.intel.mkl.DFTI
	$(JAVAH) -d $(DIR_INCLUDE) -classpath $(DIR_CLASSES) com.intel.mkl.DFTIDESCRIPTORHANDLE
	$(JAVAH) -d $(DIR_INCLUDE) -classpath $(DIR_CLASSES) com.intel.mkl.VSLStreamStatePtr
	$(JAVAH) -d $(DIR_INCLUDE) -classpath $(DIR_CLASSES) com.intel.mkl.VMLNative
	$(JAVAH) -d $(DIR_INCLUDE) -classpath $(DIR_CLASSES) com.intel.mkl.LAPACKNative

$(WRAPPERS_CLASSES):
	@echo
	@echo Making wrapper classes
	@echo
	mkdir -p ./$(DIR_CLASSES)
	$(JAVAC) -d $(DIR_CLASSES) wrappers/*.java

$(EXAMPLES_CLASSES): $(WRAPPERS_CLASSES)
	@echo
	@echo Making example classes
	@echo
	mkdir -p ./$(DIR_CLASSES)
	$(JAVAC) -classpath $(DIR_CLASSES) -d $(DIR_CLASSES) examples/*.java

$(DOCS_INDEX):
	@echo
	@echo Making documentation
	@echo
	mkdir -p ./$(DIR_DOCS)
	$(JAVADOC) -d $(DIR_DOCS) wrappers/*.java examples/*.java

##------------------------------------------------------------------------------

RES_DIR = _results/$D_$(compiler)

.SUFFIXES:: .java .res

%.res: ./examples/%.java
	@echo
	@echo Executing: $@
	@echo
	mkdir -p ./$(RES_DIR)
ifeq ($(_IA),32)
	export LD_PRELOAD=$(MKL_PATH)/libmkl_intel.so:$(MKL_PATH)/libmkl_intel_thread.so:$(MKL_PATH)/libmkl_core.so:$(MKL_PATH)/libguide.so ;\
	export LD_LIBRARY_PATH=$(MKL_PATH):${LD_LIBRARY_PATH} ;\
	$(JAVA) -classpath $(DIR_CLASSES) -Djava.library.path=$(DIR_BIN) \
	    $(subst .res,,$@) 2>&1 >$(RES_DIR)/$@
else
	export LD_PRELOAD=$(MKL_PATH)/libmkl_intel_lp64.so:$(MKL_PATH)/libmkl_intel_thread.so:$(MKL_PATH)/libmkl_core.so:$(MKL_PATH)/libguide.so ;\
	export LD_LIBRARY_PATH=$(MKL_PATH):${LD_LIBRARY_PATH} ;\
	$(JAVA) -classpath $(DIR_CLASSES) -Djava.library.path=$(DIR_BIN) \
	    $(subst .res,,$@) 2>&1 >$(RES_DIR)/$@

endif
##------------------------------------------------------------------------------

clean:
	-rm -r bin
	-rm -r classes
	-rm -r include
	-rm -r docs
	-rm -r _results
