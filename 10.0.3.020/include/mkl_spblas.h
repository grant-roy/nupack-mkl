/*******************************************************************************
!                             INTEL CONFIDENTIAL
!   Copyright(C) 2005-2008 Intel Corporation. All Rights Reserved.
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
!
!*******************************************************************************
!  Content:
!    Intel(R) Math Kernel Library (MKL) interface for Sparse BLAS level 2,3 routines
!******************************************************************************/

#ifndef _MKL_SPBLAS_H_
#define _MKL_SPBLAS_H_

#include "mkl_types.h"

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


/* Sparse BLAS Level2 lower case */
void mkl_scsrgemv(char *transa, MKL_INT *m, float *a, MKL_INT *ia,  MKL_INT *ja, float *x,  float *y);

void mkl_dcsrmv(char *transa, MKL_INT *m, MKL_INT *k, double *alpha, char *matdescra, double  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, double *x, double *beta, double *y);
void mkl_dcsrsv(char *transa, MKL_INT *m, double *alpha, char *matdescra, double  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, double *x, double *y);
void mkl_dcsrgemv(char *transa, MKL_INT *m, double *a, MKL_INT *ia,  MKL_INT *ja, double *x,  double *y);
void mkl_cspblas_dcsrgemv(char *transa, MKL_INT *m, double *a, MKL_INT *ia,  MKL_INT *ja, double *x,  double *y);
void mkl_dcsrsymv(char *uplo, MKL_INT *m, double *a, MKL_INT *ia,  MKL_INT *ja, double *x,  double *y);
void mkl_cspblas_dcsrsymv(char *uplo, MKL_INT *m, double *a, MKL_INT *ia,  MKL_INT *ja, double *x,  double *y);
void mkl_dcsrtrsv(char *uplo, char *transa, char *diag, MKL_INT *m, double *a, MKL_INT *ia,  MKL_INT *ja, double *x,  double *y);
void mkl_cspblas_dcsrtrsv(char *uplo, char *transa, char *diag, MKL_INT *m, double *a, MKL_INT *ia,  MKL_INT *ja, double *x,  double *y);

void mkl_dcscmv(char *transa, MKL_INT *m, MKL_INT *k, double *alpha, char *matdescra, double  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, double *x, double *beta, double *y);
void mkl_dcscsv(char *transa, MKL_INT *m, double *alpha, char *matdescra, double  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, double *x, double *y);

void mkl_dcoomv(char *transa, MKL_INT *m, MKL_INT *k, double *alpha, char *matdescra, double  *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, double *x, double *beta, double *y);
void mkl_dcoosv(char *transa, MKL_INT *m, double *alpha, char *matdescra, double  *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, double *x, double *y);
void mkl_dcoogemv(char *transa, MKL_INT *m, double *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, double *x,  double *y);
void mkl_cspblas_dcoogemv(char *transa, MKL_INT *m, double *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, double *x,  double *y);
void mkl_dcoosymv(char *uplo, MKL_INT *m, double *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, double *x,  double *y);
void mkl_cspblas_dcoosymv(char *uplo, MKL_INT *m, double *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, double *x,  double *y);
void mkl_dcootrsv(char *uplo, char *transa, char *diag, MKL_INT *m, double *val, MKL_INT *rowind, MKL_INT *colind, MKL_INT *nnz, double *x,  double *y);
void mkl_cspblas_dcootrsv(char *uplo, char *transa, char *diag, MKL_INT *m, double *val, MKL_INT *rowind, MKL_INT *colind, MKL_INT *nnz, double *x,  double *y);

void mkl_ddiamv (char *transa, MKL_INT *m, MKL_INT *k, double *alpha, char *matdescra, double  *val, MKL_INT *lval, MKL_INT *idiag,  MKL_INT *ndiag, double *x, double *beta, double *y);
void mkl_ddiasv (char *transa, MKL_INT *m, double *alpha, char *matdescra, double  *val, MKL_INT *lval, MKL_INT *idiag,  MKL_INT *ndiag, double *x, double *y);
void mkl_ddiagemv(char *transa, MKL_INT *m, double *val, MKL_INT *lval,  MKL_INT *idiag, MKL_INT *ndiag, double *x,  double *y);
void mkl_ddiasymv(char *uplo, MKL_INT *m, double *val, MKL_INT *lval,  MKL_INT *idiag, MKL_INT *ndiag, double *x,  double *y);
void mkl_ddiatrsv(char *uplo, char *transa, char *diag, MKL_INT *m, double *val, MKL_INT *lval,  MKL_INT  *idiag, MKL_INT *ndiag, double *x,  double *y);

void mkl_dskymv (char *transa, MKL_INT *m, MKL_INT *k, double *alpha, char *matdescra, double  *val, MKL_INT *pntr, double *x, double *beta, double *y);
void mkl_dskysv(char *transa, MKL_INT *m, double *alpha, char *matdescra, double  *val, MKL_INT *pntr,  double *x, double *y);

void mkl_dbsrmv (char *transa, MKL_INT *m, MKL_INT *k, MKL_INT *lb, double *alpha, char *matdescra, double  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, double *x, double *beta, double *y);
void mkl_dbsrsv(char *transa, MKL_INT *m, MKL_INT *lb, double *alpha, char *matdescra, double  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, double *x, double *y);
void mkl_dbsrgemv(char *transa, MKL_INT *m, MKL_INT *lb, double *a, MKL_INT *ia,  MKL_INT *ja, double *x,  double *y);
void mkl_cspblas_dbsrgemv(char *transa, MKL_INT *m, MKL_INT *lb, double *a, MKL_INT *ia,  MKL_INT *ja, double *x,  double *y);
void mkl_dbsrsymv(char *uplo, MKL_INT *m, MKL_INT *lb, double *a, MKL_INT *ia,  MKL_INT *ja, double *x,  double *y);
void mkl_cspblas_dbsrsymv(char *uplo, MKL_INT *m, MKL_INT *lb, double *a, MKL_INT *ia,  MKL_INT *ja, double *x,  double *y);
void mkl_dbsrtrsv(char *uplo, char *transa, char *diag, MKL_INT *m, MKL_INT *lb, double *a, MKL_INT *ia,  MKL_INT *ja, double *x,  double *y);
void mkl_cspblas_dbsrtrsv(char *uplo, char *transa, char *diag, MKL_INT *m, MKL_INT *lb, double *a, MKL_INT *ia,  MKL_INT *ja, double *x,  double *y);
/* Sparse BLAS Level3 lower case */

void mkl_dcsrmm(char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, double *alpha, char *matdescra, double  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, double *b, MKL_INT *ldb, double *beta, double *c, MKL_INT *ldc);
void mkl_dcsrsm(char *transa, MKL_INT *m, MKL_INT *n, double *alpha, char *matdescra, double  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, double *b, MKL_INT *ldb,  double *c, MKL_INT *ldc);

void mkl_dcscmm(char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, double *alpha, char *matdescra, double  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, double *b, MKL_INT *ldb, double *beta, double *c, MKL_INT *ldc);
void mkl_dcscsm(char *transa, MKL_INT *m, MKL_INT *n, double *alpha, char *matdescra, double  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, double *b, MKL_INT *ldb,  double *c, MKL_INT *ldc);

void mkl_dcoomm(char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, double *alpha, char *matdescra, double  *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, double *b, MKL_INT *ldb, double *beta, double *c, MKL_INT *ldc);
void mkl_dcoosm(char *transa, MKL_INT *m, MKL_INT *n, double *alpha, char *matdescra, double  *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, double *b, MKL_INT *ldb,  double *c, MKL_INT *ldc);

void mkl_ddiamm (char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, double *alpha, char *matdescra, double  *val, MKL_INT *lval, MKL_INT *idiag,  MKL_INT *ndiag, double *b, MKL_INT *ldb, double *beta, double *c, MKL_INT *ldc);
void mkl_ddiasm (char *transa, MKL_INT *m, MKL_INT *n, double *alpha, char *matdescra, double  *val, MKL_INT *lval, MKL_INT *idiag,  MKL_INT *ndiag, double *b, MKL_INT *ldb, double *c, MKL_INT *ldc);

void mkl_dskysm (char *transa, MKL_INT *m, MKL_INT *n, double *alpha, char *matdescra, double  *val, MKL_INT *pntr,  double *b, MKL_INT *ldb, double *c, MKL_INT *ldc);
void mkl_dskymm (char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, double *alpha, char *matdescra, double  *val, MKL_INT *pntr, double *b, MKL_INT *ldb, double *beta, double *c, MKL_INT *ldc);

void mkl_dbsrmm(char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_INT *lb, double *alpha, char *matdescra, double  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, double *b, MKL_INT *ldb, double *beta, double *c, MKL_INT *ldc);
void mkl_dbsrsm(char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *lb, double *alpha, char *matdescra, double  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, double *b, MKL_INT *ldb,  double *c, MKL_INT *ldc);

/* Upper case declaration */
/* Sparse BLAS Level2 upper case */
void MKL_SCSRGEMV(char *transa, MKL_INT *m, float *a, MKL_INT *ia,  MKL_INT *ja, float *x,  float *y);

void MKL_DCSRMV (char *transa, MKL_INT *m, MKL_INT *k, double *alpha, char *matdescra, double  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, double *x, double *beta, double *y);
void MKL_DCSRSV(char *transa, MKL_INT *m, double *alpha, char *matdescra, double  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, double *x, double *y);
void MKL_DCSRGEMV(char *transa, MKL_INT *m, double *a, MKL_INT *ia,  MKL_INT *ja, double *x,  double *y);
void MKL_CSPBLAS_DCSRGEMV(char *transa, MKL_INT *m, double *a, MKL_INT *ia,  MKL_INT *ja, double *x,  double *y);
void MKL_DCSRSYMV(char *uplo, MKL_INT *m, double *a, MKL_INT *ia,  MKL_INT *ja, double *x,  double *y);
void MKL_CSPBLAS_DCSRSYMV(char *uplo, MKL_INT *m, double *a, MKL_INT *ia,  MKL_INT *ja, double *x,  double *y);
void MKL_DCSRTRSV(char *uplo, char *transa, char *diag, MKL_INT *m, double *a, MKL_INT *ia,  MKL_INT *ja, double *x,  double *y);
void MKL_CSPBLAS_DCSRTRSV(char *uplo, char *transa, char *diag, MKL_INT *m, double *a, MKL_INT *ia,  MKL_INT *ja, double *x,  double *y);

void MKL_DCSCMV(char *transa, MKL_INT *m, MKL_INT *k, double *alpha, char *matdescra, double  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, double *x, double *beta, double *y);
void MKL_DCSCSV(char *transa, MKL_INT *m, double *alpha, char *matdescra, double  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, double *x, double *y);

void MKL_DCOOMV(char *transa, MKL_INT *m, MKL_INT *k, double *alpha, char *matdescra, double  *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, double *x, double *beta, double *y);
void MKL_DCOOSV(char *transa, MKL_INT *m, double *alpha, char *matdescra, double  *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, double *x, double *y);
void MKL_DCOOGEMV(char *transa, MKL_INT *m, double *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, double *x,  double *y);
void MKL_CSPBLAS_DCOOGEMV(char *transa, MKL_INT *m, double *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, double *x,  double *y);
void MKL_DCOOSYMV(char *uplo, MKL_INT *m, double *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, double *x,  double *y);
void MKL_CSPBLAS_DCOOSYMV(char *uplo, MKL_INT *m, double *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, double *x,  double *y);
void MKL_DCOOTRSV(char *uplo, char *transa, char *diag, MKL_INT *m, double *val, MKL_INT *rowind, MKL_INT *colind, MKL_INT *nnz, double *x,  double *y);
void MKL_CSPBLAS_DCOOTRSV(char *uplo, char *transa, char *diag, MKL_INT *m, double *val, MKL_INT *rowind, MKL_INT *colind, MKL_INT *nnz, double *x,  double *y);

void MKL_DDIAMV (char *transa, MKL_INT *m, MKL_INT *k, double *alpha, char *matdescra, double  *val, MKL_INT *lval, MKL_INT *idiag,  MKL_INT *ndiag, double *x, double *beta, double *y);
void MKL_DDIASV (char *transa, MKL_INT *m, double *alpha, char *matdescra, double  *val, MKL_INT *lval, MKL_INT *idiag,  MKL_INT *ndiag, double *x, double *y);
void MKL_DDIAGEMV(char *transa, MKL_INT *m, double *val, MKL_INT *lval,  MKL_INT *idiag, MKL_INT *ndiag, double *x,  double *y);
void MKL_DDIASYMV(char *uplo, MKL_INT *m, double *val, MKL_INT *lval,  MKL_INT *idiag, MKL_INT *ndiag, double *x,  double *y);
void MKL_DDIATRSV(char *uplo, char *transa, char *diag, MKL_INT *m, double *val, MKL_INT *lval,  MKL_INT  *idiag, MKL_INT *ndiag, double *x,  double *y);

void MKL_DSKYMV (char *transa, MKL_INT *m, MKL_INT *k, double *alpha, char *matdescra, double  *val, MKL_INT *pntr, double *x, double *beta, double *y);
void MKL_DSKYSV(char *transa, MKL_INT *m, double *alpha, char *matdescra, double  *val, MKL_INT *pntr,  double *x, double *y);

void MKL_DBSRMV (char *transa, MKL_INT *m, MKL_INT *k, MKL_INT *lb, double *alpha, char *matdescra, double  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, double *x, double *beta, double *y);
void MKL_DBSRSV(char *transa, MKL_INT *m, MKL_INT *lb, double *alpha, char *matdescra, double  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, double *x, double *y);
void MKL_DBSRGEMV(char *transa, MKL_INT *m, MKL_INT *lb, double *a, MKL_INT *ia,  MKL_INT *ja, double *x,  double *y);
void MKL_CSPBLAS_DBSRGEMV(char *transa, MKL_INT *m, MKL_INT *lb, double *a, MKL_INT *ia,  MKL_INT *ja, double *x,  double *y);
void MKL_DBSRSYMV(char *uplo, MKL_INT *m, MKL_INT *lb, double *a, MKL_INT *ia,  MKL_INT *ja, double *x,  double *y);
void MKL_CSPBLAS_DBSRSYMV(char *uplo, MKL_INT *m, MKL_INT *lb, double *a, MKL_INT *ia,  MKL_INT *ja, double *x,  double *y);
void MKL_DBSRTRSV(char *uplo, char *transa, char *diag, MKL_INT *m, MKL_INT *lb, double *a, MKL_INT *ia,  MKL_INT *ja, double *x,  double *y);
void MKL_CSPBLAS_DBSRTRSV(char *uplo, char *transa, char *diag, MKL_INT *m, MKL_INT *lb, double *a, MKL_INT *ia,  MKL_INT *ja, double *x,  double *y);

/* Sparse BLAS Level3 upper case */

void MKL_DCSRMM(char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, double *alpha, char *matdescra, double  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, double *b, MKL_INT *ldb, double *beta, double *c, MKL_INT *ldc);
void MKL_DCSRSM(char *transa, MKL_INT *m, MKL_INT *n, double *alpha, char *matdescra, double  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, double *b, MKL_INT *ldb,  double *c, MKL_INT *ldc);

void MKL_DCSCMM(char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, double *alpha, char *matdescra, double  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, double *b, MKL_INT *ldb, double *beta, double *c, MKL_INT *ldc);
void MKL_DCSCSM(char *transa, MKL_INT *m, MKL_INT *n, double *alpha, char *matdescra, double  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, double *b, MKL_INT *ldb,  double *c, MKL_INT *ldc);

void MKL_DCOOMM(char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, double *alpha, char *matdescra, double  *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, double *b, MKL_INT *ldb, double *beta, double *c, MKL_INT *ldc);
void MKL_DCOOSM(char *transa, MKL_INT *m, MKL_INT *n, double *alpha, char *matdescra, double  *val, MKL_INT *rowind,  MKL_INT *colind, MKL_INT *nnz, double *b, MKL_INT *ldb,  double *c, MKL_INT *ldc);

void MKL_DDIAMM (char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, double *alpha, char *matdescra, double  *val, MKL_INT *lval, MKL_INT *idiag,  MKL_INT *ndiag, double *b, MKL_INT *ldb, double *beta, double *c, MKL_INT *ldc);
void MKL_DDIASM (char *transa, MKL_INT *m, MKL_INT *n, double *alpha, char *matdescra, double  *val, MKL_INT *lval, MKL_INT *idiag,  MKL_INT *ndiag, double *b, MKL_INT *ldb, double *c, MKL_INT *ldc);

void MKL_DSKYSM (char *transa, MKL_INT *m, MKL_INT *n, double *alpha, char *matdescra, double  *val, MKL_INT *pntr,  double *b, MKL_INT *ldb, double *c, MKL_INT *ldc);
void MKL_DSKYMM (char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, double *alpha, char *matdescra, double  *val, MKL_INT *pntr, double *b, MKL_INT *ldb, double *beta, double *c, MKL_INT *ldc);

void MKL_DBSRMM(char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *k, MKL_INT *lb, double *alpha, char *matdescra, double  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, double *b, MKL_INT *ldb, double *beta, double *c, MKL_INT *ldc);
void MKL_DBSRSM(char *transa, MKL_INT *m, MKL_INT *n, MKL_INT *lb, double *alpha, char *matdescra, double  *val, MKL_INT *indx,  MKL_INT *pntrb, MKL_INT *pntre, double *b, MKL_INT *ldb,  double *c, MKL_INT *ldc);
/* MKL support */

void MKLGetVersion(MKLVersion *ver);
void MKLGetVersionString(char * buffer, int len);
void MKL_FreeBuffers(void);


#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* _MKL_SPBLAS_H_ */
