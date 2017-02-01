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
##  Intel(R) Math Kernel Library C-style Cluster DFT examples creation and run
##******************************************************************************

help:
	@echo Usage: make {lib32/lib64/libem64t} [example=name] [mpi=intel1/intel2/intel3/mpich/mpich2/openmpi]
	@echo [compiler=intel/gnu] [interface=interface_name] [workdir=path] [mpidir=path]
	@echo
	@echo example=name - name of example. Please see names in cdftc.lst file.
	@echo If parameter is omitted, all examples will be compiled.
	@echo
	@echo mpi=intel1 - using Intel MPI 1.x, default
	@echo mpi=intel2 - using Intel MPI 2.x
	@echo mpi=intel3 - using Intel MPI 3.x
	@echo mpi=mpich  - using MPICH 1.2.x
	@echo mpi=mpich2 - using MPICH2 1.0.x
	@echo mpi=openmpi - using OpenMPI 1.2
	@echo
	@echo compiler=intel - using Intel \(R\) C Compiler, default
	@echo compiler=gnu   - using GNU C compiler
	@echo interface_name    - can be lp64 or ilp64 for em64t and ia64. Default value is lp64.
	@echo
	@echo workdir=path - path to work directory, which is accessible from any node.
	@echo If parameter is omitted, executable files and results will be located in
	@echo current directory.
	@echo
	@echo mpidir=path - path to MPI installation directory. MPI scripts are taken
	@echo from mpidir/bin or mpidir/bin64 for IntelMPI and Intel\(R\) EM64T. If this directory
	@echo is in PATH you can omit mpidir parameter. When using MPICH, make sure
	@echo that MPICH was compiled by same compiler as used to build examples.
	@echo
	@echo Set environment variables LD_LIBRARY_PATH and etc properly before testing.

##------------------------------------------------------------------------------
## examples of using:
##
## make lib32 example=dm_complex_2d_double_ex1 workdir=/share
##                             - build and run DM_COMPLEX_2D_DOUBLE_EX1
##                               example for 32-bit applications in
##                               dir /share, static linking, using
##                               Intel MPI 1.0, Intel(R) C Compiler
##
## make lib64 mpi=mpich compiler=gnu mpidir=/opt/mpich
##                             - build and run all examples of CDFT for
##                               Intel(R) Itanium(R) processor family applications
##                               in current dir, static linking, using
##                               MPICH 1.2.x from /opt/mpich, GNU C compiler
##------------------------------------------------------------------------------

include cdftc.lst

ifndef example
example = $(CDFT)
endif

ifndef mpi
mpi = intel1
endif

ifndef compiler
compiler = intel
endif

ifndef workdir
workdir = .
endif

ifndef interface
interface=lp64
endif

override threading=parallel

RES = $(addsuffix .res,$(example))

MKL_PATH = $(subst examples/cdftc,lib/$(_IA),$(PWD))
LGUIDE = -lguide
COPTS =

IFACE_COMP_PART=intel
IFACE_THREADING_PART=intel

ifeq ($(interface),ilp64)
IFACE_LIB=$(MKL_PATH)/libmkl_$(IFACE_COMP_PART)_ilp64.$(EXT)
COPTS = -DMKL_ILP64
BLACS_PART=_ilp64
else
IFACE_LIB=$(MKL_PATH)/libmkl_$(IFACE_COMP_PART)_lp64.$(EXT)
COPTS =
BLACS_PART=_ilp64
endif

ifeq ($(_IA),32)
IFACE_LIB=$(MKL_PATH)/libmkl_$(IFACE_COMP_PART).$(EXT)
COPTS =
BLACS_PART=
endif

THREADING_LIB=$(MKL_PATH)/libmkl_$(IFACE_THREADING_PART)_thread.$(EXT)
LGUIDE = $(MKL_PATH)/libguide.$(EXT)

CORE_LIB=$(MKL_PATH)/libmkl_core.$(EXT)

MKL_LIBS= -Wl,--start-group $(MKL_PATH)/libmkl_cdft_core.a $(IFACE_LIB) $(THREADING_LIB) $(CORE_LIB) -Wl,--end-group $(LGUIDE)

ifeq ($(_IA),32)
RES_DIR=_results/$(compiler)_$(_IA)_$(RES_EXT)$Z
else
RES_DIR=_results/$(compiler)_$(interface)_$(_IA)_$(RES_EXT)$Z
endif

lib32:
	$(MAKE) $(RES) _IA=32 Ibin=bin EXT=a RES_EXT=lib
libem64t:
	$(MAKE) $(RES) _IA=em64t Ibin=bin64 EXT=a RES_EXT=lib
lib64:
	$(MAKE) $(RES) _IA=64 Ibin=bin EXT=a RES_EXT=lib

ifeq ($(mpi),mpich2)
ifeq ($(compiler),intel)
CS = mpicc -cc=icc $(COPTS)
endif
ifeq ($(compiler),gnu)
CS = mpicc -cc=gcc $(COPTS)
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
CS = mpiicc -nocompchk $(COPTS)
endif
ifeq ($(compiler),gnu)
CS = mpicc $(COPTS)
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
CS = mpiicc -nocompchk $(COPTS)
endif
ifeq ($(compiler),gnu)
CS = mpicc $(COPTS)
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
CS = mpiicc $(COPTS)
endif
ifeq ($(compiler),gnu)
CS = mpicc $(COPTS)
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
CS = mpicc -cc=icc $(COPTS)
endif
ifeq ($(compiler),gnu)
CS = mpicc -cc=gcc  $(COPTS)
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

COMMON = $(PWD)/source/cdft_example_support.c
MKL_ROOT = $(PWD)/../..

vpath %.c $(PWD)/source

$(RES): %.res: %.c
	mkdir -p $(RES_DIR)
	cd $(RES_DIR);$(_CS) $(COMMON) -I $(MKL_ROOT)/include -c -o cdft_example_support.o
	cd $(RES_DIR);$(_CS) $< -I $(MKL_ROOT)/include -c -o $*.o
	cd $(RES_DIR);$(_CS) $*.o cdft_example_support.o -L $(MKL_PATH) $(MKL_LIBS) $(MKL_PATH)/libmkl_blacs$(Bs)$(BLACS_PART).$(EXT) -lpthread -lm -o $*.exe
	$(_RS) $(RES_DIR)/$*.exe data/$*.dat > $(RES_DIR)/$@
	-$(DelF)
#-------------------------------------------------------------------------------
