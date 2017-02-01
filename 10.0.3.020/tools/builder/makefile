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
##      Intel(R) Math Kernel Library Custom Shared Object builder
##
##******************************************************************************

all:
	@echo "Custom Shared Object builder."
	@echo "Usage: make {ia32/ipf/em64t} [options]"
	@echo "[interface=interface_name] [threading=threading_name]"
	@echo "target ia32 for ia32, ipf for Intel(R) Itanium(R) processor family"
	@echo "em64t is used for Intel(R) Xeon(R) processor with Intel(R) EM64T."
	@echo "options:"
	@echo "export=def_file (default: user_list. File with functions list)"
	@echo "name=so_name (default: mkl_custom.so)"
	@echo "xerbla=user_xerbla.o (default: no user xerbla)"
	@echo "interface_name - can be lp64 or ilp64 for em64t and ia64. Default value is lp64."
	@echo "threading_name - can be parallel or sequential. Default value is parallel."

##------------------------------------------------------------------------------
##
## examples of using:
## make ia32 - build custom shared object for ia32
##   Function list from "user_list". SO name is mkl_custom.so
##   No special xerbla is used.
##
## make ipf export=my_func_list.txt name=my_so - build so for Intel(R) Itanium(R)
## processor family.
##   Function list from my_func_list.txt. SO name is my_so.so.
##   No special xerbla is used.
##
##------------------------------------------------------------------------------

## Path to mkl libraries (can be edited by user)

MKL_ROOT = $(subst tools/builder,,$(PWD))

mkl32_libpath=$(MKL_ROOT)/lib/32/
mklem64t_libpath=$(MKL_ROOT)/lib/em64t/
mkl64_libpath=$(MKL_ROOT)/lib/64/

##------------------------------------------------------------------------------
## No changes below this line !
##------------------------------------------------------------------------------

#ifndef export
export=user_list
#endif

#ifndef name
name="mkl_custom"
#endif

ifndef interface
interface=lp64
endif

ifndef threading
threading=parallel
endif

IFACE_COMP_PART=intel
IFACE_THREADING_PART=intel

ifeq ($(interface),ilp64)
IFACE_LIB=libmkl_$(IFACE_COMP_PART)_ilp64.a
else
IFACE_LIB=libmkl_$(IFACE_COMP_PART)_lp64.a
endif

ifeq ($(threading),sequential)
THREADING_LIB=libmkl_sequential.a
else
THREADING_LIB=libmkl_$(IFACE_THREADING_PART)_thread.a
endif

CORE_LIB=libmkl_core.a

func_list := $(addprefix -u ,$(shell cat $(export)))


ia32:
	gcc -shared  -Bdynamic $(func_list) \
	$(xerbla) $(mkl32_libpath)/libmkl_intel.a $(mkl32_libpath)/$(THREADING_LIB) \
	$(mkl32_libpath)/$(CORE_LIB) -L$(mkl32_libpath) -lguide  -o $(name).so

em64t:
	gcc -shared  -Bdynamic $(func_list) \
	$(xerbla) $(mklem64t_libpath)/$(IFACE_LIB) $(mklem64t_libpath)/$(THREADING_LIB) \
	$(mklem64t_libpath)/$(CORE_LIB)  -L$(mklem64t_libpath) -lguide -o $(name).so

ipf:
	gcc -shared -Bdynamic $(func_list) \
	$(xerbla) $(mkl64_libpath)/$(IFACE_LIB) $(mkl64_libpath)/$(THREADING_LIB) \
	$(mkl64_libpath)/$(CORE_LIB) -L$(mkl64_libpath) -lguide -o $(name).so

