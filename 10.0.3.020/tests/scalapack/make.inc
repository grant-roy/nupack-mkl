# vim: syntax=make
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
############################################################################
#  Identifiers
#
#  arch
#  mpi
#  comp
#  opt
#  IDE
#  interface
############################################################################
# Intel architecture (must be defined)
#arch=ia32
# MPI
ifneq ("$(mpi)","intelmpi")
ifneq ("$(mpi)","intelmpi20")
ifneq ("$(mpi)","intelmpi30")
ifneq ("$(mpi)","openmpi")
mpi=mpich-1.2.x
endif
endif
endif
endif
# Compiler
comp=intel
ifeq ($(compiler),gnu)
comp=gnu
endif
ifeq ($(compiler),gfortran)
comp=gnu
endif

ifndef interface
interface = lp64
endif

ifeq ($(interface),ilp64)
ADD_IFACE = _ilp64
IFACE_PART = _ilp64
COPTS = -DMKL_ILP64
FOPTS = -i8
else
ADD_IFACE = _lp64
IFACE_PART = _lp64
COPTS =
FOPTS =
endif

ifeq ($(arch),ia32)
ADD_IFACE =
IFACE_PART =
COPTS =
FOPTS =
endif

# Optimization level
opt=noopt
# executable identifier
IDE=_$(arch)_$(mpi)_$(comp)_$(opt)$(ADD_IFACE)
ifeq ($(compiler),gfortran)
IDE=_$(arch)_$(mpi)_$(compiler)_$(opt)$(ADD_IFACE)
endif
############################################################################
#  Paths
#
#  ROOTdir
#  LIBdir
#  SRCdir
############################################################################
#ROOTdir
ifeq ($(LIBdir),)
	ifeq ($(arch),ia32)
		LIBpath=-L$(ROOTdir)/../../lib/32
	endif
	ifeq ($(arch),ipf)
		LIBpath=-L$(ROOTdir)/../../lib/64
	endif
	ifeq ($(arch),em64t)
		LIBpath=-L$(ROOTdir)/../../lib/em64t
	endif
else
	LIBpath=-L$(LIBdir)
endif
SRCdir=source/TESTING
MKL_PATH=$(patsubst -L%,%,$(LIBpath))
############################################################################
#  The libraries to be linked to
#
#  LIBS
############################################################################
# BLACS
ifeq ($(mpi),intelmpi)
BLACSLIB=-lmkl_blacs_intelmpi$(IFACE_PART)
else
ifeq ($(mpi),intelmpi20)
BLACSLIB=-lmkl_blacs_intelmpi20$(IFACE_PART)
else
ifeq ($(mpi),intelmpi30)
BLACSLIB=-lmkl_blacs_intelmpi20$(IFACE_PART)
else
ifeq ($(mpi),openmpi)
BLACSLIB=-lmkl_blacs_openmpi$(IFACE_PART)
else
BLACSLIB=-lmkl_blacs$(IFACE_PART)
endif
endif
endif
endif
FBLACSLIB=$(BLACSLIB)
# ScaLAPACK
ifeq ($(arch),ia32)
SCALAPACKLIB= $(MKL_PATH)/libmkl_scalapack_core.a
else
SCALAPACKLIB= $(MKL_PATH)/libmkl_scalapack$(IFACE_PART).a
endif
# LAPACK and BLAS
LAPACKLIB=
ifeq ($(dynamic),yes)
MKL_LIBS=-L$(MKL_PATH) $(MKL_PATH)/libmkl_intel$(ADD_IFACE).so -Wl,--start-group $(MKL_PATH)/libmkl_sequential.so \
$(MKL_PATH)/libmkl_lapack.so $(MKL_PATH)/libmkl_core.so -Wl,--end-group -lpthread
#BLASLIB=-lmkl -lguide -lpthread
else
MKL_LIBS=-L$(MKL_PATH) $(MKL_PATH)/libmkl_intel$(ADD_IFACE).a -Wl,--start-group $(MKL_PATH)/libmkl_sequential.a \
$(MKL_PATH)/libmkl_core.a -Wl,--end-group -lpthread
endif
# All libraries to link
LIBS=$(LIBpath) $(TOOLSLIB) $(SCALAPACKLIB) $(FBLACSLIB) $(MKL_LIBS) $(MPILIB)

############################################################################
#  Tools
#
#  MAKE
#  F77
#  F77FLAGS
#  MPIRUN
#  F77LOADER
#  RMOBJ
############################################################################
# Make options
MAKE := $(MAKE) ROOTdir=$(ROOTdir) LIBdir=$(LIBdir) MPIdir=$(MPIdir) arch=$(arch) mpi=$(mpi) comp=$(comp) opt=$(opt) dynamic=$(dynamic) interface=$(interface)
# Fortran, mpirun
ifeq ($(MPIdir),)
ifeq ("$(findstring intelmpi,$(mpi))","intelmpi")
ifeq ($(comp),gnu)
F77=mpif77 $(FOPTS)
endif
ifeq ($(compiler),gfortran)
F77=mpif90 $(FOPTS) 
endif
ifeq ($(comp),intel)
F77=mpiifort -nocompchk $(FOPTS)
endif
MPIRUN=mpiexec -n 4
else
F77=mpif77 $(FOPTS)
MPIRUN=mpirun -np 4
endif
else
ifeq ("$(findstring intelmpi,$(mpi))","intelmpi")
ifeq ($(comp),gnu)
F77=$(MPIdir)/bin/mpif77 $(FOPTS)
ifeq ($(arch),em64t)
F77=$(MPIdir)/bin64/mpif77 $(FOPTS)
endif
endif
ifeq ($(compiler),gfortran) 
F77=$(MPIdir)/bin/mpif90 $(FOPTS)
ifeq ($(arch),em64t)
F77=$(MPIdir)/bin64/mpif90 $(FOPTS)
endif
endif 
ifeq ($(comp),intel)
F77=$(MPIdir)/bin/mpiifort -nocompchk $(FOPTS)
ifeq ($(arch),em64t)
F77=$(MPIdir)/bin64/mpiifort -nocompchk $(FOPTS)
endif
endif
MPIRUN=$(MPIdir)/bin/mpiexec -n 4
ifeq ($(arch),em64t)
MPIRUN=$(MPIdir)/bin64/mpiexec -n 4
endif
else
F77=$(MPIdir)/bin/mpif77 $(FOPTS)
MPIRUN=$(MPIdir)/bin/mpirun -np 4
endif
endif
F77LOADER=$(F77)
# Optimization
ifeq ($(opt),opt)
OPTS=-O2
else	# noopt
OPTS=-O0
endif
# Compiler flags
F77FLAGS=-w -u $(OPTS)
ifeq ($(comp),gnu)
F77FLAGS=$(OPTS) -ff90 -fno-globals -Wno-globals
endif
ifeq ($(compiler),gfortran)
F77FLAGS=
endif
# Remove objects variable
RMOBJ=rm -f *.o core work.pc*

