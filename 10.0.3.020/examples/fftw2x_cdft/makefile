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
##  Intel(R) Math Kernel Library
##  C-style Cluster DFT wrappers MPI FFTW examples creation and run
##******************************************************************************

help:
	@echo Usage: make {lib32/lib64/libem64t} [mpi=intel1/intel2/intel3/mpich/mpich2/openmpi]
	@echo [compiler=intel/gnu] [workdir=path] [mpidir=path]
	@echo [PRECISION=FFTW_ENABLE_DOUBLE/FFTW_ENABLE_FLOAT]
	@echo
	@echo mpi=intel1 - using Intel MPI 1.x, default
	@echo mpi=intel2 - using Intel MPI 2.x
	@echo mpi=intel3 - using Intel MPI 3.x
	@echo mpi=mpich  - using MPICH 1.2.x
	@echo mpi=mpich2 - using MPICH2 1.0.x
	@echo mpi=openmpi - using OpenMPI 1.2
	@echo
	@echo compiler=intel - using Intel\(R\) C Compiler, default
	@echo compiler=gnu   - using GNU C compiler
	@echo
	@echo workdir=path - path to work directory, which is accessible from any node.
	@echo If parameter is omitted, executable files and results will be located in
	@echo current directory.
	@echo
	@echo PRECISION=FFTW_ENABLE_DOUBLE - for double precision data, default
	@echo PRECISION=FFTW_ENABLE_FLOAT - for float precision data
	@echo mpidir=path - path to MPI installation directory. MPI scripts are taken
	@echo from mpidir/bin or mpidir/bin64 for IntelMPI and Intel\(R\) EM64T. If this directory
	@echo is in PATH you can omit mpidir parameter. When using MPICH, make sure
	@echo that MPICH was compiled by same compiler as used to build examples.
	@echo
	@echo Set environment variables LD_LIBRARY_PATH and etc properly before testing.
##*****************************************************************************
## examples of using:
##
## make lib64 mpi=intel2 compiler=intel
##				- compile and run wrappers examples
##				using Intel MPI 2.0,Intel(R) C Compiler 8.x or 9.x,
##				for double precision data for
##				Intel(R) Itanium(R)-based applications
##
## make lib32 PRECISION=FFTW_ENABLE_FLOAT mpi=mpich mpidir=/opt/mpich compiler=gnu
##				- compile and run wrappers examples using
##				MPICH 1.2.x from /opt/mpich, GNU C compiler
##				for float precision data for 32-bit applications
##*****************************************************************************


include cdft_wrappers.lst

example = $(WRAPPER)

ifndef mpi
mpi = intel2
endif
ifndef compiler
compiler = intel
endif
ifndef workdir
workdir = .
endif

ifndef PRECISION
PRECISION = FFTW_ENABLE_DOUBLE
endif

ifeq ($(PRECISION),FFTW_ENABLE_FLOAT)
FL = -DFFTW_ENABLE_FLOAT
PR = _SINGLE
PR_L = MKL_SINGLE
endif

ifeq ($(PRECISION),FFTW_ENABLE_DOUBLE)
FL =
PR = _DOUBLE
PR_L = MKL_DOUBLE
endif

RES = $(addsuffix .res,$(example))

MKL_PATH = $(subst examples/fftw2x_cdft,lib/$(_IA),$(PWD))
LGUIDE = -lguide
COPTS =


ifeq ($(_IA),32)
IFACE_LIB=$(MKL_PATH)/libmkl_intel.$(EXT)
BLACS_PART=
else
IFACE_LIB=$(MKL_PATH)/libmkl_intel_lp64.$(EXT)
BLACS_PART=_ilp64
endif

THREADING_LIB=$(MKL_PATH)/libmkl_intel_thread.$(EXT)
LGUIDE = $(MKL_PATH)/libguide.$(EXT)

CORE_LIB=$(MKL_PATH)/libmkl_core.$(EXT)

MKL_LIBS= -Wl,--start-group $(MKL_PATH)/libmkl_cdft_core.$(EXT) $(IFACE_LIB) $(THREADING_LIB) $(CORE_LIB) -Wl,--end-group $(LGUIDE)

ifeq ($(_IA),32)
RES_DIR=_results/$(compiler)_$(_IA)_$(RES_EXT)$Z
else
RES_DIR=_results/$(compiler)_$(_IA)_$(RES_EXT)$Z
endif

lib32:
	$(MAKE) wrap_lib _IA=32
	$(MAKE) $(RES) _IA=32 Ibin=bin EXT=a RES_EXT=lib
libem64t:
	$(MAKE) wrap_lib _IA=em64t
	$(MAKE) $(RES) _IA=em64t Ibin=bin64 EXT=a RES_EXT=lib
lib64:
	$(MAKE) wrap_lib _IA=64
	$(MAKE) $(RES) _IA=64 Ibin=bin EXT=a RES_EXT=lib

wrap_lib:
	cd $(MKL_ROOT)/interfaces/fftw2x_cdft; $(MAKE) lib$(_IA) mpi=$(mpi) compiler=$(compiler) mpidir=$(mpidir) PRECISION=$(PR_L)

ifeq ($(mpi),mpich2)
ifeq ($(compiler),intel)
CS = mpicc -cc=icc
endif
ifeq ($(compiler),gnu)
CS = mpicc -cc=gcc
endif
RS = mpiexec -n 2
ifdef mpidir
_CS = $(mpidir)/bin/$(CS)
_RS = $(mpidir)/bin/$(RS)
else
_CS = $(CS)
_RS = $(RS)
endif
Bs = _intelmpi20
endif

ifeq ($(mpi),intel1)
ifeq ($(compiler),intel)
CS = mpiicc -nocompchk
endif
ifeq ($(compiler),gnu)
CS = mpicc
endif
RS = mpiexec -n 2
ifdef mpidir
_CS = $(mpidir)/$(Ibin)/$(CS)
_RS = $(mpidir)/$(Ibin)/$(RS)
else
_CS = $(CS)
_RS = $(RS)
endif
Bs = _intelmpi
endif

ifeq ($(mpi),intel2)
ifeq ($(compiler),intel)
CS = mpiicc -nocompchk
endif
ifeq ($(compiler),gnu)
CS = mpicc
endif
RS = mpiexec -n 2
ifdef mpidir
_CS = $(mpidir)/$(Ibin)/$(CS)
_RS = $(mpidir)/$(Ibin)/$(RS)
else
_CS = $(CS)
_RS = $(RS)
endif
Bs = _intelmpi20
endif

ifeq ($(mpi),intel3)
ifeq ($(compiler),intel)
CS = mpiicc
endif
ifeq ($(compiler),gnu)
CS = mpicc
endif
RS = mpiexec -n 2
ifdef mpidir
_CS = $(mpidir)/$(Ibin)/$(CS)
_RS = $(mpidir)/$(Ibin)/$(RS)
else
_CS = $(CS)
_RS = $(RS)
endif
Bs = _intelmpi20
endif

ifeq ($(mpi),mpich)
ifeq ($(compiler),intel)
CS = mpicc -cc=icc
endif
ifeq ($(compiler),gnu)
CS = mpicc -cc=gcc
endif
RS = mpirun -np 2
ifdef mpidir
_CS = $(mpidir)/bin/$(CS)
_RS = $(mpidir)/bin/$(RS)
else
_CS = $(CS)
_RS = $(RS)
endif
DelF = rm $(RES_DIR)/*.o
endif

ifeq ($(mpi),openmpi)
ifeq ($(compiler),intel)
CS = mpicc -cc=icc $(COPTS)
endif
ifeq ($(compiler),gnu)
CS = mpicc -cc=gcc  $(COPTS)
endif
RS = mpiexec -np 2
ifdef mpidir
_CS = $(mpidir)/bin/$(CS)
_RS = $(mpidir)/bin/$(RS)
else
_CS = $(CS)
_RS = $(RS)
endif
Bs = _openmpi
DelF = rm $(RES_DIR)/*.o
endif
MKL_ROOT = $(PWD)/../..

vpath %.c $(PWD)/source

$(RES): %.res: %.c
	mkdir -p $(RES_DIR)

	cd $(RES_DIR);$(_CS) $(FL) $< -I $(MKL_ROOT)/include -I $(MKL_ROOT)/include/fftw -c -o $*.o
	cd $(RES_DIR);$(_CS) $(FL) $*.o -O0 -L $(MKL_ROOT)/lib/$(_IA) -lfftw2x_cdft$(PR) $(MKL_LIBS) -lmkl_blacs$(Bs)$(BLACS_PART) -lpthread -lm -o $*.exe
	$(_RS) $(RES_DIR)/$*.exe > $(RES_DIR)/$@;
	-$(DelF)
#-------------------------------------------------------------------------------


