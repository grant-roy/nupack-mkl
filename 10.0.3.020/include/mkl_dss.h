/*******************************************************************************
*                              INTEL CONFIDENTIAL
*   Copyright(C) 2004-2008 Intel Corporation. All Rights Reserved.
*   The source code contained  or  described herein and all documents related to
*   the source code ("Material") are owned by Intel Corporation or its suppliers
*   or licensors.  Title to the  Material remains with  Intel Corporation or its
*   suppliers and licensors. The Material contains trade secrets and proprietary
*   and  confidential  information of  Intel or its suppliers and licensors. The
*   Material  is  protected  by  worldwide  copyright  and trade secret laws and
*   treaty  provisions. No part of the Material may be used, copied, reproduced,
*   modified, published, uploaded, posted, transmitted, distributed or disclosed
*   in any way without Intel's prior express written permission.
*   No license  under any  patent, copyright, trade secret or other intellectual
*   property right is granted to or conferred upon you by disclosure or delivery
*   of the Materials,  either expressly, by implication, inducement, estoppel or
*   otherwise.  Any  license  under  such  intellectual property  rights must be
*   express and approved by Intel in writing.
*
********************************************************************************
*   Content : MKL DSS C header file
*
*           Contains main datatypes, prototypes and constants definition
*
********************************************************************************
*/
#if !defined( __MKL_DSS_H )

#define __MKL_DSS_H

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/*
** Basic data types
*/

typedef void *             _MKL_DSS_HANDLE_t;
typedef char               _CHARACTER_t;
typedef char               _CHARACTER_STR_t;
#ifdef MKL_ILP64
typedef long long int      _INTEGER_t;
#else
typedef int                _INTEGER_t;
#endif
#if !defined( _WIN64 )
typedef long               _LONG_t;
#else
typedef long long          _LONG_t;
#endif
typedef float              _REAL_t;
typedef double             _DOUBLE_PRECISION_t;
#define _DoubleComplexType struct { double r, i; }
typedef _DoubleComplexType _DOUBLE_COMPLEX_t;

/*
** MKL_DSS_DEFAULTS
*/

#define MKL_DSS_DEFAULTS   0



/*
** Out-of-core level option definitions
*/

#define MKL_DSS_OOC_VARIABLE 1024
#define MKL_DSS_OOC_STRONG   2048

/*
** Message level option definitions
*/

#define MKL_DSS_MSG_LVL_SUCCESS	-2147483647
#define MKL_DSS_MSG_LVL_DEBUG  	-2147483646
#define MKL_DSS_MSG_LVL_INFO   	-2147483645
#define MKL_DSS_MSG_LVL_WARNING	-2147483644
#define MKL_DSS_MSG_LVL_ERROR  	-2147483643
#define MKL_DSS_MSG_LVL_FATAL  	-2147483642

/*
** Termination level option definitions
*/

#define MKL_DSS_TERM_LVL_SUCCESS	1073741832
#define MKL_DSS_TERM_LVL_DEBUG  	1073741840
#define MKL_DSS_TERM_LVL_INFO   	1073741848
#define MKL_DSS_TERM_LVL_WARNING	1073741856
#define MKL_DSS_TERM_LVL_ERROR  	1073741864
#define MKL_DSS_TERM_LVL_FATAL  	1073741872

/*
** Structure option definitions
*/

#define MKL_DSS_SYMMETRIC          	536870976
#define MKL_DSS_SYMMETRIC_STRUCTURE	536871040
#define MKL_DSS_NON_SYMMETRIC      	536871104

/*
** Reordering option definitions
*/

#define MKL_DSS_AUTO_ORDER   	268435520
#define MKL_DSS_MY_ORDER     	268435584
#define MKL_DSS_OPTION1_ORDER	268435648

/*
** Factorization option definitions
*/

#define MKL_DSS_POSITIVE_DEFINITE          	134217792
#define MKL_DSS_INDEFINITE                 	134217856
#define MKL_DSS_HERMITIAN_POSITIVE_DEFINITE	134217920
#define MKL_DSS_HERMITIAN_INDEFINITE       	134217984

/*
** Return status values
*/

#define MKL_DSS_SUCCESS        	0
#define MKL_DSS_ZERO_PIVOT     	-1
#define MKL_DSS_OUT_OF_MEMORY  	-2
#define MKL_DSS_FAILURE        	-3
#define MKL_DSS_ROW_ERR        	-4
#define MKL_DSS_COL_ERR        	-5
#define MKL_DSS_TOO_FEW_VALUES 	-6
#define MKL_DSS_TOO_MANY_VALUES	-7
#define MKL_DSS_NOT_SQUARE     	-8
#define MKL_DSS_STATE_ERR      	-9
#define MKL_DSS_INVALID_OPTION 	-10
#define MKL_DSS_OPTION_CONFLICT	-11
#define MKL_DSS_MSG_LVL_ERR    	-12
#define MKL_DSS_TERM_LVL_ERR   	-13
#define MKL_DSS_STRUCTURE_ERR  	-14
#define MKL_DSS_REORDER_ERR    	-15
#define MKL_DSS_VALUES_ERR     	-16
#define MKL_DSS_STATISTICS_INVALID_MATRIX    	-17
#define MKL_DSS_STATISTICS_INVALID_STATE    	-18
#define MKL_DSS_STATISTICS_INVALID_STRING    	-19

/*
** Function prototypes for DSS routines
*/

#if defined( _WIN32 ) || defined( _WIN64 )
#define dss_create_ 		DSS_CREATE
#define dss_define_structure_ 	DSS_DEFINE_STRUCTURE
#define dss_delete_ 		DSS_DELETE
#define dss_factor_complex_ 	DSS_FACTOR_COMPLEX
#define dss_factor_real_ 	DSS_FACTOR_REAL
#define dss_reorder_ 		DSS_REORDER
#define dss_solve_complex_ 	DSS_SOLVE_COMPLEX
#define dss_solve_real_ 	DSS_SOLVE_REAL
#define dss_statistics_ 	DSS_STATISTICS
#else
#define DSS_CREATE              dss_create_
#define DSS_DEFINE_STRUCTURE    dss_define_structure_
#define DSS_DELETE              dss_delete_
#define DSS_FACTOR_COMPLEX      dss_factor_complex_
#define DSS_FACTOR_REAL         dss_factor_real_
#define DSS_REORDER             dss_reorder_
#define DSS_SOLVE_COMPLEX       dss_solve_complex_
#define DSS_SOLVE_REAL          dss_solve_real_
#define DSS_STATISTICS          dss_statistics_
#endif

extern _INTEGER_t dss_create_(_MKL_DSS_HANDLE_t *,
	 _INTEGER_t const *);
extern _INTEGER_t dss_define_structure_(_MKL_DSS_HANDLE_t *,
	 _INTEGER_t const *, _INTEGER_t const *, _INTEGER_t const *,
	 _INTEGER_t const *, _INTEGER_t const *, _INTEGER_t const *);
extern _INTEGER_t dss_reorder_(_MKL_DSS_HANDLE_t *, _INTEGER_t const *,
	 _INTEGER_t const *);
extern _INTEGER_t dss_factor_real_(_MKL_DSS_HANDLE_t *,
	 _INTEGER_t const *, _DOUBLE_PRECISION_t const *);
extern _INTEGER_t dss_factor_complex_(_MKL_DSS_HANDLE_t *,
	 _INTEGER_t const *, _DOUBLE_COMPLEX_t const *);
extern _INTEGER_t dss_solve_real_(_MKL_DSS_HANDLE_t *,
	 _INTEGER_t const *, _DOUBLE_PRECISION_t const *, _INTEGER_t const *,
	 _DOUBLE_PRECISION_t *);
extern _INTEGER_t dss_solve_complex_(_MKL_DSS_HANDLE_t *,
	 _INTEGER_t const *, _DOUBLE_COMPLEX_t const *, _INTEGER_t const *,
	 _DOUBLE_COMPLEX_t *);
extern _INTEGER_t dss_statistics_( _MKL_DSS_HANDLE_t *, _INTEGER_t const *,
	 _CHARACTER_STR_t const *, _DOUBLE_PRECISION_t *);
extern _INTEGER_t dss_delete_(_MKL_DSS_HANDLE_t const *,
	 _INTEGER_t const *);

/*
** In order to promote portability and to avoid having most users deal with these issues, the C header
** file mkl_dss.h provides a set of macros and type definitions that are intended to hide the
** inter-language calling conventions and provide an interface to the DSS that appears natural for
** C/C++.
*/

#define dss_create(handle, opt) dss_create_(&(handle), &(opt))
#define dss_define_structure(handle, opt, rowIndex, nRows, rCols,\
	 columns, nNonZeros) dss_define_structure_(&(handle), &(opt),\
	 (rowIndex), &(nRows), &(rCols), (columns), &(nNonZeros))
#define dss_reorder(handle, opt, perm) dss_reorder_(&(handle), &(opt),\
	 (perm))
#define dss_factor_real(handle, opt,\
	 rValues) dss_factor_real_(&(handle), &(opt), (rValues))
#define dss_factor_complex(handle, opt,\
	 rValues) dss_factor_complex_(&(handle), &(opt), (rValues))
#define dss_solve_real(handle, opt, rRhsValues, nRhs,\
	 rSolValues) dss_solve_real_(&(handle), &(opt), (rRhsValues), &(nRhs),\
	 (rSolValues))
#define dss_solve_complex(handle, opt, rRhsValues, nRhs,\
	 rSolValues) dss_solve_complex_(&(handle), &(opt), (rRhsValues),\
	 &(nRhs), (rSolValues))
#define dss_statistics(handle, opt, stat, ret) \
	 dss_statistics_(&(handle), &(opt), (stat), (ret))
#define dss_delete(handle, opt) dss_delete_(&(handle), &(opt))

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif
