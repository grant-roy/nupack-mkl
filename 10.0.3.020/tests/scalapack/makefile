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
##      Intel(R) Math Kernel Library ScaLAPACK tests creation and run
##
##******************************************************************************
ROOTdir=$(shell pwd)
include make.inc

.PHONY : check

help:
	@echo "Usage:"
	@echo
	@echo "make [help]"
	@echo "   print this help."
	@echo
	@echo "make <targets> [mpi=mpi_name] [compiler=compiler_name]"
	@echo "      [interface=interface_name] [LIBdir=<libdir>] [MPIdir=<mpidir>]"
	@echo "   makes tests. Make sure environment is set properly before testing."
	@echo "   If variables are not set, the defaults are:"
	@echo "      mpi=mpich-1.2.x - test with MPICH-1.2.x"
	@echo "      compiler=intel - test with Intel (R) compilers"
	@echo "      LIBdir=../../lib/32,64 or em64t - path to MKL binaries"
	@echo "      MPIdir not set - let the system locate MPI scripts like mpirun,"
	@echo "          set PATH, LD_LIBRARY_PATH properly before testing."
	@echo "   Variables could be set to:"
	@echo "      mpi=intelmpi - test with Intel MPI 1.x."
	@echo "          Don't forget to start Intel MPI 1.x daemon before testing."
	@echo "      mpi=intelmpi20 - test with Intel MPI 2.x."
	@echo "          Don't forget to start Intel MPI 2.x daemon before testing."
	@echo "      mpi=intelmpi30 - test with Intel MPI 3.x."
	@echo "          Don't forget to start Intel MPI 3.x daemon before testing."
	@echo "      mpi=openmpi - test with Open MPI 1.2"
	@echo "      compiler=gnu - test with GNU g77 compiler, ilp64 libraries shouldn't be tested with gnu compiler."
	@echo "      compiler=gfortran - test with GNU gfortran compiler, ilp64 libraries shouldn't be tested with gnu compiler."
	@echo "      compiler=intel - test with Intel (R) compilers"
	@echo "      LIBdir=<MKLdir> - path to MKL binaries, if they're not in"
	@echo "          ../../lib/32(64,em64t)."
	@echo "      MPIdir=<mpidir> - path to MPI. Specify MPIdir if it's not in"
	@echo "          the PATH. MPI scripts are taken from <mpidir>/bin (or bin64"
	@echo "          in case of Intel(R) EM64T)."
	@echo "          When testing with MPICH-1.2.x, make sure that MPICH was"
	@echo "          compiled by the same compiler as used to build the tests."
	@echo "      interface_name - can be lp64 or ilp64 for em64t and ia64. Default value is lp64."
	@echo "   Targets:"
	@echo "      lib32,lib64,libem64t,so32,so64,soem64t - to link with static/dynamic MKL"
	@echo "                     for IA-32/Intel(R) Itanium(R)/Intel(R) EM64T processor family"
	@echo "      clean - clean all objects"
	@echo "      cleanexe - clean all executables and test results"
	@echo "      cleanall - clean + cleanexe"
	@echo

check:
ifeq ($(compiler),gnu)
ifeq ($(interface),ilp64)
   $(error "ilp64 libraries shouldn't be tested with gnu compiler")
endif
endif
ifeq ($(compiler),gfortran)
ifeq ($(interface),ilp64)
   $(error "ilp64 libraries shouldn't be tested with gfortran compiler")
endif
ifeq ($(mpi),intelmpi)
   $(error "gfortran shouldn't be used with Intel MPI 1.x")
endif
ifeq ($(mpi),openmpi)
   $(error "gfortran shouldn't be used with Open MPI 1.2")
endif
endif

lib32: check ; (cd $(SRCdir); $(MAKE) arch=ia32 dynamic=no exe run)
lib64: check ; (cd $(SRCdir); $(MAKE) arch=ipf dynamic=no exe run)
libem64t: check ; (cd $(SRCdir); $(MAKE) arch=em64t dynamic=no exe run)
so32: check ; (cd $(SRCdir); $(MAKE) arch=ia32 dynamic=yes exe run)
so64: check ; (cd $(SRCdir); $(MAKE) arch=ipf dynamic=yes exe run)
soem64t: check ; (cd $(SRCdir); $(MAKE) arch=em64t dynamic=yes exe run)
cleanall: clean cleanexe
cleanexe: ; (cd $(SRCdir); $(MAKE) cleanexe)
clean: ; (cd $(SRCdir); $(MAKE) clean)
