##******************************************************************************
##                              INTEL CONFIDENTIAL
##  Copyright(C) 2004-2008 Intel Corporation. All Rights Reserved.
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
##      Intel(R) Math Kernel Library SOLVER examples creation and run
##******************************************************************************

help:
	@echo Usage: make {lib32/lib64/libem64t} [function=name]
	@echo [compiler=compiler_name] [interface=interface_name] [threading=threading_name]
	@echo
	@echo name     - function name. Please see macro SOLVER
	@echo
	@echo compiler_name     - can be gnu or intel. Default value is intel.
	@echo                     Intel \(R\) Compilers as default.
	@echo                     If compiler=gnu then GNU gfortran compiler will be used.
	@echo                     To use g77 add FC=g77. FC=g77 cannot be used with ilp64 interface.
	@echo
	@echo interface_name    - can be lp64 or ilp64 for em64t and ia64. Default value is lp64.
	@echo
	@echo threading_name    - can be parallel or sequential. Default value is parallel.

##------------------------------------------------------------------------------
## examples of using:
##
## make lib32 function=dss_sym_c  - build by Intel(R) C Compiler and Intel(R) Fortran Compiler (as default) and
##                                  run DSS_SYM example for 32-bit applications,
##                                  static linking
##
## make lib64 compiler=gnu       - build by GNU compilers and run all examples of MKL
##                          for Intel(R) Itanium(R) processor family applications, static
##                          linking
##------------------------------------------------------------------------------

SOLVER_C   = dss_sym_c pardiso_sym_c pardiso_unsym_c cg_jacobi_precon_c cg_no_precon_c cg_ssor_precon_c cg_st_criteria_c fgmres_full_funct_c fgmres_no_precon_c fgmres_st_criterion_c \
ex_nlsqp_bc_c ex_nlsqp_c dcsrilu0_exampl1 dcsrilut_exampl1
SOLVER_F   = dss_sym_f pardiso_sym_f pardiso_unsym_f cg_jacobi_precon cg_no_precon cg_ssor_precon cg_st_criteria fgmres_full_funct_f fgmres_no_precon_f fgmres_st_criterion_f \
ex_nlsqp_bc_f ex_nlsqp_f dcsrilu0_exampl2 cg_mrhs cg_mrhs_precond cg_mrhs_stop_crt dcsrilut_exampl2
ifeq ($(compiler),gnu)
ifeq ($(FC),g77)
SOLVER_F90 =
else
SOLVER_F90 = dss_sym_f90
endif
else
SOLVER_F90 = dss_sym_f90
endif


ifndef function
function = $(SOLVER_C) $(SOLVER_F) $(SOLVER_F90)
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
RES_C   = $(addsuffix .res ,$(SOLVER_C))
RES_F   = $(addsuffix .res ,$(SOLVER_F))
RES_F90 = $(addsuffix .res ,$(SOLVER_F90))

RES = $(addsuffix .res ,$(function))
MKL_PATH = $(subst examples/solver,lib/$(_IA),$(PWD))

ifeq ($(compiler),gnu)
CC = gcc
CCOPTS = -w -I../../include
FC=gfortran
FCOPTS = -w -I../../include -fno-second-underscore -x f77-cpp-input -ffixed-line-length-132
GF_F90=-ffree-form
ifeq ($(FC),g77)
FCOPTS = -w -I../../include -fno-second-underscore -x f77-cpp-input -ffixed-line-length-132
endif
FPPSTOP=-x none
ifeq ($(FC),gfortran)
IFACE_COMP_PART=gf
else
IFACE_COMP_PART=intel
endif
IFACE_THREADING_PART=intel
else
override CC = icc
CCOPTS = -w -I../../include
override FC=ifort
FCOPTS = -w -I../../include -fpp
IFACE_COMP_PART=intel
IFACE_THREADING_PART=intel
endif
IFACE_COMP_PART=intel
IFACE_THREADING_PART=intel

ifeq ($(interface),ilp64)
IFACE_LIB=$(MKL_PATH)/libmkl_$(IFACE_COMP_PART)_ilp64.$(EXT)
FOPTS = -i8
SOLVER_INTERFACE=_ilp64
ifeq ($(compiler),gnu)
override FC=gfortran
FOPTS=-fdefault-integer-8
endif
COPTS= -DMKL_ILP64
else
IFACE_LIB=$(MKL_PATH)/libmkl_$(IFACE_COMP_PART)_lp64.$(EXT)
FOPTS =
SOLVER_INTERFACE=_lp64
COPTS=
endif

ifeq ($(_IA),32)
IFACE_LIB=$(MKL_PATH)/libmkl_$(IFACE_COMP_PART).$(EXT)
FOPTS =
SOLVER_INTERFACE=
COPTS=
endif

ifeq ($(threading),sequential)
THREADING_LIB=$(MKL_PATH)/libmkl_sequential.$(EXT)
LGUIDE =
SOLVER_THREADING=_sequential
else
THREADING_LIB=$(MKL_PATH)/libmkl_$(IFACE_THREADING_PART)_thread.$(EXT)
LGUIDE = $(MKL_PATH)/libguide.$(EXT)
SOLVER_THREADING=
endif

CORE_LIB=$(MKL_PATH)/libmkl_core.$(EXT)

ifeq ($(EXT),so)
MKL_LAPACKLIB=$(MKL_PATH)/libmkl_lapack.$(EXT)
else
MKL_LAPACKLIB=
endif

SOLVER_LIB=$(MKL_PATH)/libmkl_solver$(SOLVER_INTERFACE)$(SOLVER_THREADING).$(EXT)

MKL_LIBS= $(SOLVER_LIB) $(IFACE_LIB) -Wl,--start-group $(THREADING_LIB) $(MKL_LAPACKLIB) $(CORE_LIB) -Wl,--end-group $(LGUIDE)

ifeq ($(_IA),32)
RES_DIR=_results/$(compiler)_$(threading)_$(_IA)_$(RES_EXT)$Z
else
RES_DIR=_results/$(compiler)_$(interface)_$(threading)_$(_IA)_$(RES_EXT)$Z
endif

lib32:
	$(MAKE) $(RES) EXT=a _IA=32 RES_EXT=lib
libem64t:
	$(MAKE) $(RES) EXT=a _IA=em64t RES_EXT=lib
lib64:
	$(MAKE) $(RES) EXT=a _IA=64 RES_EXT=lib

#-------------------------------------------------------------------------------

vpath %.c source
vpath %.f source
vpath %.f90 source

$(RES_C): %.res: %.c
	mkdir -p ./$(RES_DIR)
	$(CC) $(CCOPTS) $(COPTS) $< -L$(MKL_PATH) $(MKL_LIBS) -lpthread -lm -o $(RES_DIR)/$*.out
	export LD_LIBRARY_PATH=$(MKL_PATH):$(LD_LIBRARY_PATH); $(RES_DIR)/$*.out >$(RES_DIR)/$@

$(RES_F): %.res: %.f
	mkdir -p ./$(RES_DIR)
	$(FC) $(FCOPTS) $(FOPTS) $< $(FPPSTOP) -L$(MKL_PATH) $(MKL_LIBS) -lpthread -lm -o $(RES_DIR)/$*.out
	export LD_LIBRARY_PATH=$(MKL_PATH):$(LD_LIBRARY_PATH); $(RES_DIR)/$*.out >$(RES_DIR)/$@

$(RES_F90): %.res: %.f90
	mkdir -p ./$(RES_DIR)
	$(FC) $(GF_F90) $(FCOPTS) $(FOPTS) $< $(FPPSTOP) -L$(MKL_PATH) $(MKL_LIBS) -lpthread -lm -o $(RES_DIR)/$*.out
	export LD_LIBRARY_PATH=$(MKL_PATH):$(LD_LIBRARY_PATH); $(RES_DIR)/$*.out >$(RES_DIR)/$@; rm *.mod
#-------------------------------------------------------------------------------
