/*******************************************************************************
!                             INTEL CONFIDENTIAL
!   Copyright(C) 2007-2008 Intel Corporation. All Rights Reserved.
!   The source code contained  or  described herein and all documents related to
!   the source code ("Material") are owned by Intel Corporation or its suppliers
!   or licensors.  Title to the  Material remains with  Intel Corporation or its
!   suppliers and licensors. The Material contains trade secrets and proprietary
!   and  confidential  information of  Intel or its suppliers and licensors. The
!   Material  is  protected  by  worldwide  copyright  and trade secret laws and
!   treaty  provisions. No part of the Material may be used, copied, reproduced,
!   modified, published, uploaded, posted, transmitted, distributed or disclosed
!   in any way without Intel's prior express written permission.
!   No license  under any  patent, copyright, trade secret or other intellectual
!   property right is granted to or conferred upon you by disclosure or delivery
!   of the Materials,  either expressly, by implication, inducement, estoppel or
!   otherwise.  Any  license  under  such  intellectual property  rights must be
!   express and approved by Intel in writing.
!******************************************************************************/

#include "com_intel_mkl_CBLAS.h"

/*
// Stubs for:
//  . cblas_sgemm
//  . cblas_dgemm
//  . cblas_cgemm
//  . cblas_zgemm
*/

/* sgemm and cgemm */
#define DATAKIND_SINGLE
#include "CBLAS_gemm.h"
#undef  DATAKIND_SINGLE

/* dgemm and zgemm */
#define DATAKIND_DOUBLE
#include "CBLAS_gemm.h"
#undef  DATAKIND_DOUBLE

/*
// Stubs for:
//  . cblas_sgemv
//  . cblas_dgemv
//  . cblas_cgemv
//  . cblas_zgemv
*/

/* sgemv and cgemv */
#define DATAKIND_SINGLE
#include "CBLAS_gemv.h"
#undef  DATAKIND_SINGLE

/* dgemv and zgemv */
#define DATAKIND_DOUBLE
#include "CBLAS_gemv.h"
#undef  DATAKIND_DOUBLE

/*
// Stubs for:
//  . cblas_sdot
//  . cblas_ddot
//  . cblas_cdotc_sub
//  . cblas_zdotc_sub
//  . cblas_cdotu_sub
//  . cblas_zdotu_sub
*/

/* sdot, cdotc, and cdotu */
#define DATAKIND_SINGLE
#include "CBLAS_dot.h"
#undef  DATAKIND_SINGLE

/* ddot, zdotc, and zdotu */
#define DATAKIND_DOUBLE
#include "CBLAS_dot.h"
#undef  DATAKIND_DOUBLE
