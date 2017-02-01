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

#include "mkl_cblas.h"

typedef enum {ClapackJobN=201, ClapackJobV=202, ClapackJobA=203, ClapackJobS=204, ClapackJobO=205} CLAPACK_JOB;

int clapack_cgesv(int n, int nrhs, void* a,  int lda, int* ipiv, void* b,  int ldb);
int clapack_sgesv(int n, int nrhs, float* a, int lda, int* ipiv, float* b, int ldb);
int clapack_dgesv(int n, int nrhs, double* a,int lda, int* ipiv, double* b,int ldb);
int clapack_zgesv(int n, int nrhs, void* a,  int lda, int* ipiv, void* b,  int ldb);

int clapack_ssyev(CLAPACK_JOB jobz, CBLAS_UPLO uplo, int n, float *a, int lda, float *w);
int clapack_dsyev(CLAPACK_JOB jobz, CBLAS_UPLO uplo, int n, double *a, int lda, double *w);

int clapack_sgeev(CLAPACK_JOB jobvl, CLAPACK_JOB jobvr, int n, float *a, int lda, float *wr, float *wi, float *vl, int ldvl, float *vr, int ldvr);
int clapack_dgeev(CLAPACK_JOB jobvl, CLAPACK_JOB jobvr, int n, double *a, int lda, double *wr, double *wi, double *vl, int ldvl, double *vr, int ldvr);
int clapack_cgeev(CLAPACK_JOB jobvl, CLAPACK_JOB jobvr, int n, void *a, int lda, void *w, void *vl, int ldvl, void *vr, int ldvr);
int clapack_zgeev(CLAPACK_JOB jobvl, CLAPACK_JOB jobvr, int n, void *a, int lda, void *w, void *vl, int ldvl, void *vr, int ldvr);

int clapack_sgesvd(CLAPACK_JOB jobu, CLAPACK_JOB jobvt, int m, int n, float *a, int lda, float *s, float *u, int ldu, float *vt, int ldvt, float *sd);
int clapack_dgesvd(CLAPACK_JOB jobu, CLAPACK_JOB jobvt, int m, int n, double *a, int lda, double *s, double *u, int ldu, double *vt, int ldvt, double *sd);
int clapack_cgesvd(CLAPACK_JOB jobu, CLAPACK_JOB jobvt, int m, int n, void *a, int lda, float *s, void *u, int ldu, void *vt, int ldvt, float *sd);
int clapack_zgesvd(CLAPACK_JOB jobu, CLAPACK_JOB jobvt, int m, int n, void *a, int lda, double *s, void *u, int ldu, void *vt, int ldvt, double *sd);
