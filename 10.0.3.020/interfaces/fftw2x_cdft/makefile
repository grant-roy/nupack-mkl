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
##      This library allows to use MKL CDFT routines through MPI FFTW interface.
##******************************************************************************

help:
	@echo Usage: make {lib32/lib64/libem64t} [mpi=intel1/intel2/intel3/mpich/mpich2/openmpi]
	@echo [compiler=intel/gnu]  [mpidir=path] [PRESICION=MKL_DOUBLE/MKL_SINGLE]
	@echo
	@echo mpi=intel1 - using Intel MPI 1.0.x, default
	@echo mpi=intel2 - using Intel MPI 2.0.x
	@echo mpi=intel3 - using Intel MPI 3.0.x
	@echo mpi=mpich  - using MPICH 1.2.x
	@echo mpi=mpich2 - using MPICH2 1.0.x
	@echo mpi=openmpi - using OpenMPI 1.2
	@echo
	@echo compiler=intel - using Intel\(R\) C Compiler, default
	@echo compiler=gnu   - using GNU C compiler
	@echo
	@echo mpidir=path - path to MPI installation directory. MPI scripts are taken
	@echo from mpidir/bin or mpidir/bin64 for IntelMPI and Intel\(R\) EM64T. If this directory
	@echo is in PATH you can omit mpidir parameter. When using MPICH, make sure
	@echo that MPICH was compiled by same compiler as used to build examples.
	@echo
	@echo precision=MKL_DOUBLE - for double-precision data, default
	@echo precision=MKL_SINGLE - for float-precision data
	@echo
	@echo Set environment variables LD_LIBRARY_PATH and etc properly before testing.

##---------------------------------------------------------------------------------
## examples of using:
##
## make lib64 mpi=intel2 compiler=intel
##					- compile using Intel MPI 2.0,Intel(R) C Compiler 8.x or 9.x,
##					for double precision data and build library for
##					Intel(R) Itanium(R)-based applications
##
## make lib32 PRECISION=MKL_SINGLE mpi=mpich mpidir=/opt/mpich compiler=gnu
##					- compile using MPICH 1.2.x from /opt/mpich, GNU C compiler
##					for float precision data and build library for
##					32-bit applications
##----------------------------------------------------------------------------------

include fftw2x_cdft.lst

MKL=../..

ifndef PRECISION
PRECISION = MKL_DOUBLE
endif

ifeq ($(PRECISION),MKL_DOUBLE)
PRE=_DOUBLE
else
PRE=_SINGLE
endif

ifndef mpi
mpi = intel2
endif

ifndef compiler
compiler = intel
endif


WRP = $(addsuffix .o ,$(WRAP))

lib32:
	$(MAKE) clean _IA=32
	$(MAKE) $(MKL)/lib/32/libfftw2x_cdft$(PRE).a _IA=32 Ibin=bin
libem64t:
	$(MAKE) clean _IA=em64t
	$(MAKE) $(MKL)/lib/em64t/libfftw2x_cdft$(PRE).a _IA=em64t Ibin=bin64
lib64:
	$(MAKE) clean _IA=64
	$(MAKE) $(MKL)/lib/64/libfftw2x_cdft$(PRE).a _IA=64 Ibin=bin

$(MKL)/lib/$(_IA)/libfftw2x_cdft$(PRE).a: $(WRP)
	ar rs $(MKL)/lib/$(_IA)/libfftw2x_cdft$(PRE).a $^
	rm -f *.o
clean:
	rm -f $(MKL)/lib/$(_IA)/libfftw2x_cdft$(PRE).a *.o

ifeq ($(mpi),intel1)
ifeq ($(compiler),intel)
CS = mpiicc -nocompchk
endif
ifeq ($(compiler),gnu)
CS = mpicc
endif
ifdef mpidir
_CS = $(mpidir)/$(Ibin)/$(CS)
else
_CS = $(CS)
endif
endif

ifeq ($(mpi),intel2)
ifeq ($(compiler),intel)
CS = mpiicc -nocompchk
endif
ifeq ($(compiler),gnu)
CS = mpicc
endif
ifdef mpidir
_CS = $(mpidir)/$(Ibin)/$(CS)
else
_CS = $(CS)
endif
endif

ifeq ($(mpi),intel3)
ifeq ($(compiler),intel)
CS = mpiicc
endif
ifeq ($(compiler),gnu)
CS = mpicc
endif
ifdef mpidir
_CS = $(mpidir)/$(Ibin)/$(CS)
else
_CS = $(CS)
endif
endif

ifeq ($(mpi),mpich)
ifeq ($(compiler),intel)
CS = mpicc -cc=icc
endif
ifeq ($(compiler),gnu)
CS = mpicc -cc=gcc
endif
ifdef mpidir
_CS = $(mpidir)/bin/$(CS)
else
_CS = $(CS)
endif
endif

ifeq ($(mpi),mpich2)
ifeq ($(compiler),intel)
CS = mpicc -cc=icc
endif
ifeq ($(compiler),gnu)
CS = mpicc -cc=gcc
endif
ifdef mpidir
_CS = $(mpidir)/bin/$(CS)
else
_CS = $(CS)
endif
endif

ifeq ($(mpi),openmpi)
ifeq ($(compiler),intel)
CS = mpicc -cc=icc 
endif
ifeq ($(compiler),gnu)
CS = mpicc -cc=gcc 
endif
ifdef mpidir
_CS = $(mpidir)/bin/$(CS)
else
_CS = $(CS)
endif
endif

vpath %.c wrappers

$(WRP): %.o: %.c
	$(_CS) -c -D$(PRECISION) -I  $(MKL)/include  $< -o $@

