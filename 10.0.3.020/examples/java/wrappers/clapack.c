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

/*
 * Subsidiary cblas-like interface for Java wrappers.
 *
 * These functions:
 *   - hide work arrays,
 *   - replace chars arguments with integers ones.
 * Also "info" is returned as function value.
 */

#include "mkl_lapack.h"
#include "clapack.h"
#include <stdlib.h>

int clapack_cgesv(int n, int nrhs, void* a, int lda, int* ipiv, void* b, int ldb) {
	int info;
	cgesv(&n, &nrhs, (MKL_Complex8*) a, &lda, ipiv, (MKL_Complex8*) b, &ldb, &info);
	return info;
}

int clapack_sgesv(int n, int nrhs, float* a, int lda, int* ipiv, float* b ,int ldb) {
	int info;
	sgesv(&n, &nrhs, a, &lda, ipiv, b, &ldb, &info);
	return info;
}


int clapack_dgesv(int n, int nrhs, double* a,int lda, int* ipiv, double* b, int ldb) {
	int info;
	dgesv(&n, &nrhs, a, &lda, ipiv, b, &ldb, &info);
	return info;
}


int clapack_zgesv(int n, int nrhs, void* a, int lda, int* ipiv, void* b, int ldb) {
	int info;
	zgesv(&n, &nrhs, (MKL_Complex16*) a, &lda, ipiv, (MKL_Complex16*) b, &ldb, &info);
	return info;
}

int clapack_ssyev(CLAPACK_JOB jobz, CBLAS_UPLO uplo, int n, float *a, int lda, float *w) {
	int info;
	int lwork = -1;
	float *work;
	float i_work;
	char c_jobz, c_uplo;
	if(jobz == ClapackJobN) {
		c_jobz = 'n';
	} else if(jobz == ClapackJobV) {
		c_jobz = 'v';
	} else {
		return -1;
	}
	if(uplo == CblasUpper) {
		c_uplo = 'u';
	} else if(uplo == CblasLower) {
		c_uplo = 'l';
	} else {
		return -2;
	}
	ssyev(&c_jobz, &c_uplo, &n, a, &lda, w, &i_work, &lwork, &info);
	if(info != 0) {
		return info;
	}
	lwork = (int)i_work;
	work = malloc(lwork*sizeof(float));
	if(work == NULL) {
		return -8;
	}
	ssyev(&c_jobz, &c_uplo, &n, a, &lda, w, work, &lwork, &info);
	free(work);
	return info;
}

int clapack_dsyev(CLAPACK_JOB jobz, CBLAS_UPLO uplo, int n, double *a, int lda, double *w) {
	int info;
	int lwork = -1;
	double *work;
	double i_work;
	char c_jobz, c_uplo;
	if(jobz == ClapackJobN) {
		c_jobz = 'n';
	} else if(jobz == ClapackJobV) {
		c_jobz = 'v';
	} else {
		return -1;
	}
	if(uplo == CblasUpper) {
		c_uplo = 'u';
	} else if(uplo == CblasLower) {
		c_uplo = 'l';
	} else {
		return -2;
	}
	dsyev(&c_jobz, &c_uplo, &n, a, &lda, w, &i_work, &lwork, &info);
	if(info != 0) {
		return info;
	}
	lwork = (int)i_work;
	work = malloc(lwork*sizeof(double));
	if(work == NULL) {
		return -8;
	}
	dsyev(&c_jobz, &c_uplo, &n, a, &lda, w, work, &lwork, &info);
	free(work);
	return info;
}

int clapack_sgeev(CLAPACK_JOB jobvl, CLAPACK_JOB jobvr, int n, float *a, int lda, float *wr, float *wi, float *vl, int ldvl, float *vr, int ldvr) {
	int info;
	int lwork = -1;
	float *work;
	float i_work;
	char c_jobvl, c_jobvr;
	if(jobvl == ClapackJobN) {
		c_jobvl = 'n';
	} else if(jobvl == ClapackJobV) {
		c_jobvl = 'v';
	} else {
		return -1;
	}
	if(jobvr == ClapackJobN) {
		c_jobvr = 'n';
	} else if(jobvr == ClapackJobV) {
		c_jobvr = 'v';
	} else {
		return -2;
	}
	sgeev(&c_jobvl, &c_jobvr, &n, a, &lda, wr, wi, vl, &ldvl, vr, &ldvr, &i_work, &lwork, &info);
	if(info != 0) {
		return info;
	}
	lwork = (int)i_work;
	work = malloc(lwork*sizeof(float));
	if(work == NULL) {
		return -13;
	}
	sgeev(&c_jobvl, &c_jobvr, &n, a, &lda, wr, wi, vl, &ldvl, vr, &ldvr, work, &lwork, &info);
	free(work);
	return info;
}

int clapack_dgeev(CLAPACK_JOB jobvl, CLAPACK_JOB jobvr, int n, double *a, int lda, double *wr, double *wi, double *vl, int ldvl, double *vr, int ldvr) {
	int info;
	int lwork = -1;
	double *work;
	double i_work;
	char c_jobvl, c_jobvr;
	if(jobvl == ClapackJobN) {
		c_jobvl = 'n';
	} else if(jobvl == ClapackJobV) {
		c_jobvl = 'v';
	} else {
		return -1;
	}
	if(jobvr == ClapackJobN) {
		c_jobvr = 'n';
	} else if(jobvr == ClapackJobV) {
		c_jobvr = 'v';
	} else {
		return -2;
	}
	dgeev(&c_jobvl, &c_jobvr, &n, a, &lda, wr, wi, vl, &ldvl, vr, &ldvr, &i_work, &lwork, &info);
	if(info != 0) {
		return info;
	}
	lwork = (int)i_work;
	work = malloc(lwork*sizeof(double));
	if(work == NULL) {
		return -13;
	}
	dgeev(&c_jobvl, &c_jobvr, &n, a, &lda, wr, wi, vl, &ldvl, vr, &ldvr, work, &lwork, &info);
	free(work);
	return info;
}

int clapack_cgeev(CLAPACK_JOB jobvl, CLAPACK_JOB jobvr, int n, void *a, int lda, void *w, void *vl, int ldvl, void *vr, int ldvr) {
	int info;
	int lwork = -1;
	MKL_Complex8* work;
	float *rwork;
	MKL_Complex8 i_work;
	char c_jobvl, c_jobvr;
	if(jobvl == ClapackJobN) {
		c_jobvl = 'n';
	} else if(jobvl == ClapackJobV) {
		c_jobvl = 'v';
	} else {
		return -1;
	}
	if(jobvr == ClapackJobN) {
		c_jobvr = 'n';
	} else if(jobvr == ClapackJobV) {
		c_jobvr = 'v';
	} else {
		return -2;
	}
	cgeev(&c_jobvl, &c_jobvr, &n, (MKL_Complex8*) a, &lda, (MKL_Complex8*) w, (MKL_Complex8*) vl, &ldvl, (MKL_Complex8*) vr, &ldvr, &i_work, &lwork, NULL, &info);
	if(info != 0) {
		return info;
	}
	lwork = (int)i_work.real;
	work = malloc(lwork*sizeof(float)*2);
	if(work == NULL) {
		return -12;
	}
	rwork = malloc(2*n*sizeof(float));
	if(rwork == NULL) {
		free(work);
		return -12;
	}
	cgeev(&c_jobvl, &c_jobvr, &n, (MKL_Complex8*) a, &lda, (MKL_Complex8*) w, (MKL_Complex8*) vl, &ldvl, (MKL_Complex8*) vr, &ldvr, work, &lwork, rwork, &info);
	free(work);
	free(rwork);
	return info;
}

int clapack_zgeev(CLAPACK_JOB jobvl, CLAPACK_JOB jobvr, int n, void *a, int lda, void *w, void *vl, int ldvl, void *vr, int ldvr) {
	int info;
	int lwork = -1;
	MKL_Complex16* work;
	double *rwork;
	MKL_Complex16 i_work;
	char c_jobvl, c_jobvr;
	if(jobvl == ClapackJobN) {
		c_jobvl = 'n';
	} else if(jobvl == ClapackJobV) {
		c_jobvl = 'v';
	} else {
		return -1;
	}
	if(jobvr == ClapackJobN) {
		c_jobvr = 'n';
	} else if(jobvr == ClapackJobV) {
		c_jobvr = 'v';
	} else {
		return -2;
	}
	zgeev(&c_jobvl, &c_jobvr, &n, (MKL_Complex16*) a, &lda, (MKL_Complex16*) w, (MKL_Complex16*) vl, &ldvl, (MKL_Complex16*) vr, &ldvr, &i_work, &lwork, NULL, &info);
	if(info != 0) {
		return info;
	}
	lwork = (int)i_work.real;
	work = malloc(lwork*sizeof(double)*2);
	if(work == NULL) {
		return -12;
	}
	rwork = malloc(2*n*sizeof(double));
	if(rwork == NULL) {
		free(work);
		return -12;
	}
	zgeev(&c_jobvl, &c_jobvr, &n, (MKL_Complex16*) a, &lda, (MKL_Complex16*) w, (MKL_Complex16*) vl, &ldvl, (MKL_Complex16*) vr, &ldvr, work, &lwork, rwork, &info);
	free(work);
	free(rwork);
	return info;
}

int clapack_sgesvd(CLAPACK_JOB jobu, CLAPACK_JOB jobvt, int m, int n, float *a, int lda, float *s, float *u, int ldu, float *vt, int ldvt, float *sd) {
	int info;
	int lwork = -1;
	float *work;
	float i_work;
	char c_jobu, c_jobvt;
	int i, minmn;
	if(jobu == ClapackJobN) {
		c_jobu = 'n';
	} else if(jobu == ClapackJobA) {
		c_jobu = 'a';
	} else if(jobu == ClapackJobS) {
		c_jobu = 's';
	} else if(jobu == ClapackJobO) {
		c_jobu = 'o';
	} else {
		return -1;
	}
	if(jobvt == ClapackJobN) {
		c_jobvt = 'n';
	} else if(jobvt == ClapackJobA) {
		c_jobvt = 'a';
	} else if(jobvt == ClapackJobS) {
		c_jobvt = 's';
	} else if(jobvt == ClapackJobO) {
		c_jobvt = 'o';
	} else {
		return -2;
	}
	sgesvd(&c_jobu, &c_jobvt, &m, &n, a, &lda, s, u, &ldu, vt, &ldvt, &i_work, &lwork, &info);
	if(info != 0) {
		return info;
	}
	lwork = (int)i_work;
	work = malloc(lwork*sizeof(float));
	if(work == NULL) {
		return -13;
	}
	sgesvd(&c_jobu, &c_jobvt, &m, &n, a, &lda, s, u, &ldu, vt, &ldvt, work, &lwork, &info);
	if(info > 0) {
		minmn = (m<n) ? m : n;
		for(i=1;i<minmn;i++) {
			sd[i-1] = work[i];
		}
	}
	free(work);
	return info;
}

int clapack_dgesvd(CLAPACK_JOB jobu, CLAPACK_JOB jobvt, int m, int n, double *a, int lda, double *s, double *u, int ldu, double *vt, int ldvt, double *sd) {
	int info;
	int lwork = -1;
	double *work;
	double i_work;
	char c_jobu, c_jobvt;
	int i, minmn;
	if(jobu == ClapackJobN) {
		c_jobu = 'n';
	} else if(jobu == ClapackJobA) {
		c_jobu = 'a';
	} else if(jobu == ClapackJobS) {
		c_jobu = 's';
	} else if(jobu == ClapackJobO) {
		c_jobu = 'o';
	} else {
		return -1;
	}
	if(jobvt == ClapackJobN) {
		c_jobvt = 'n';
	} else if(jobvt == ClapackJobA) {
		c_jobvt = 'a';
	} else if(jobvt == ClapackJobS) {
		c_jobvt = 's';
	} else if(jobvt == ClapackJobO) {
		c_jobvt = 'o';
	} else {
		return -2;
	}
	dgesvd(&c_jobu, &c_jobvt, &m, &n, a, &lda, s, u, &ldu, vt, &ldvt, &i_work, &lwork, &info);
	if(info != 0) {
		return info;
	}
	lwork = (int)i_work;
	work = malloc(lwork*sizeof(double));
	if(work == NULL) {
		return -13;
	}
	dgesvd(&c_jobu, &c_jobvt, &m, &n, a, &lda, s, u, &ldu, vt, &ldvt, work, &lwork, &info);
	if(info > 0) {
		minmn = (m<n) ? m : n;
		for(i=1;i<minmn;i++) {
			sd[i-1] = work[i];
		}
	}
	free(work);
	return info;
}

int clapack_cgesvd(CLAPACK_JOB jobu, CLAPACK_JOB jobvt, int m, int n, void *a, int lda, float *s, void *u, int ldu, void *vt, int ldvt, float *sd) {
	int info;
	int lwork = -1;
	MKL_Complex8* work;
	MKL_Complex8 i_work;
	float *rwork;
	char c_jobu, c_jobvt;
	int i, minmn = (m<n) ? m : n;
	if(jobu == ClapackJobN) {
		c_jobu = 'n';
	} else if(jobu == ClapackJobA) {
		c_jobu = 'a';
	} else if(jobu == ClapackJobS) {
		c_jobu = 's';
	} else if(jobu == ClapackJobO) {
		c_jobu = 'o';
	} else {
		return -1;
	}
	if(jobvt == ClapackJobN) {
		c_jobvt = 'n';
	} else if(jobvt == ClapackJobA) {
		c_jobvt = 'a';
	} else if(jobvt == ClapackJobS) {
		c_jobvt = 's';
	} else if(jobvt == ClapackJobO) {
		c_jobvt = 'o';
	} else {
		return -2;
	}
	cgesvd(&c_jobu, &c_jobvt, &m, &n, (MKL_Complex8*) a, &lda, s, (MKL_Complex8*) u, &ldu, (MKL_Complex8*) vt, &ldvt, &i_work, &lwork, NULL, &info);
	if(info != 0) {
		return info;
	}
	lwork = (int)i_work.real;
	work = malloc(2*lwork*sizeof(float));
	if(work == NULL) {
		return -13;
	}
	rwork = malloc(5*minmn*sizeof(float));
	if(rwork == NULL) {
		free(work);
		return -13;
	}
	cgesvd(&c_jobu, &c_jobvt, &m, &n, a, &lda, s, u, &ldu, vt, &ldvt, work, &lwork, rwork, &info);
	if(info > 0) {
		for(i=0;i<minmn-1;i++) {
			sd[i] = rwork[i];
		}
	}
	free(work);
	free(rwork);
	return info;
}

int clapack_zgesvd(CLAPACK_JOB jobu, CLAPACK_JOB jobvt, int m, int n, void *a, int lda, double *s, void *u, int ldu, void *vt, int ldvt, double *sd) {
	int info;
	int lwork = -1;
	MKL_Complex16* work;
	MKL_Complex16 i_work;
	double *rwork;
	char c_jobu, c_jobvt;
	int i, minmn = (m<n) ? m : n;
	if(jobu == ClapackJobN) {
		c_jobu = 'n';
	} else if(jobu == ClapackJobA) {
		c_jobu = 'a';
	} else if(jobu == ClapackJobS) {
		c_jobu = 's';
	} else if(jobu == ClapackJobO) {
		c_jobu = 'o';
	} else {
		return -1;
	}
	if(jobvt == ClapackJobN) {
		c_jobvt = 'n';
	} else if(jobvt == ClapackJobA) {
		c_jobvt = 'a';
	} else if(jobvt == ClapackJobS) {
		c_jobvt = 's';
	} else if(jobvt == ClapackJobO) {
		c_jobvt = 'o';
	} else {
		return -2;
	}
	zgesvd(&c_jobu, &c_jobvt, &m, &n, (MKL_Complex16*) a, &lda, s, (MKL_Complex16*) u, &ldu, (MKL_Complex16*) vt, &ldvt, &i_work, &lwork, NULL, &info);
	if(info != 0) {
		return info;
	}
	lwork = (int)i_work.real;
	work = malloc(2*lwork*sizeof(double));
	if(work == NULL) {
		return -13;
	}
	rwork = malloc(5*minmn*sizeof(double));
	if(rwork == NULL) {
		free(work);
		return -13;
	}
	zgesvd(&c_jobu, &c_jobvt, &m, &n, (MKL_Complex16*) a, &lda, s, (MKL_Complex16*) u, &ldu, (MKL_Complex16*) vt, &ldvt, work, &lwork, rwork, &info);
	if(info > 0) {
		for(i=0;i<minmn-1;i++) {
			sd[i] = rwork[i];
		}
	}
	free(work);
	free(rwork);
	return info;
}
