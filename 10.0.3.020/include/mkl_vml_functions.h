/* file: mkl_vml_functions.h */
/*
//                             INTEL CONFIDENTIAL
//  Copyright(C) 2006-2008 Intel Corporation. All Rights Reserved.
//  The source code contained  or  described herein and all documents related to
//  the source code ("Material") are owned by Intel Corporation or its suppliers
//  or licensors.  Title to the  Material remains with  Intel Corporation or its
//  suppliers and licensors. The Material contains trade secrets and proprietary
//  and  confidential  information of  Intel or its suppliers and licensors. The
//  Material  is  protected  by  worldwide  copyright  and trade secret laws and
//  treaty  provisions. No part of the Material may be used, copied, reproduced,
//  modified, published, uploaded, posted, transmitted, distributed or disclosed
//  in any way without Intel's prior express written permission.
//  No license  under any  patent, copyright, trade secret or other intellectual
//  property right is granted to or conferred upon you by disclosure or delivery
//  of the Materials,  either expressly, by implication, inducement, estoppel or
//  otherwise.  Any  license  under  such  intellectual property  rights must be
//  express and approved by Intel in writing.
*/
/*
//++
//  User-level VML function declarations
//--
*/

#ifndef __MKL_VML_FUNCTIONS_H__
#define __MKL_VML_FUNCTIONS_H__

#include "mkl_vml_types.h"

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/*
//++
//  EXTERNAL API MACROS.
//  Used to construct VML function declaration. Change them if you are going to
//  provide different API for VML functions.
//--
*/
#define _Vml_Api(rtype,name,arg) extern rtype           name    arg;
#define _vml_api(rtype,name,arg) extern rtype           name##_ arg;
#define _VML_API(rtype,name,arg) extern rtype           name##_ arg;

/*
//++
//  VML ELEMENTARY FUNCTION DECLARATIONS.
//--
*/
/* Absolute value: r[i] = |a[i]| */
_VML_API(void,VSABS,(MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDABS,(MKL_INT *n, const double a[], double r[]))
_vml_api(void,vsabs,(MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdabs,(MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsAbs,(MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdAbs,(MKL_INT n,  const double a[], double r[]))

/* Complex absolute value: r[i] = |a[i]| */
_VML_API(void,VCABS,(MKL_INT *n, const MKL_Complex8  a[], float  r[]))
_VML_API(void,VZABS,(MKL_INT *n, const MKL_Complex16 a[], double r[]))
_vml_api(void,vcabs,(MKL_INT *n, const MKL_Complex8  a[], float  r[]))
_vml_api(void,vzabs,(MKL_INT *n, const MKL_Complex16 a[], double r[]))
_Vml_Api(void,vcAbs,(MKL_INT n,  const MKL_Complex8  a[], float  r[]))
_Vml_Api(void,vzAbs,(MKL_INT n,  const MKL_Complex16 a[], double r[]))

/* Addition: r[i] = a[i] + b[i] */
_VML_API(void,VSADD,(MKL_INT *n, const float  a[], const float  b[], float  r[]))
_VML_API(void,VDADD,(MKL_INT *n, const double a[], const double b[], double r[]))
_vml_api(void,vsadd,(MKL_INT *n, const float  a[], const float  b[], float  r[]))
_vml_api(void,vdadd,(MKL_INT *n, const double a[], const double b[], double r[]))
_Vml_Api(void,vsAdd,(MKL_INT n,  const float  a[], const float  b[], float  r[]))
_Vml_Api(void,vdAdd,(MKL_INT n,  const double a[], const double b[], double r[]))

/* Complex addition: r[i] = a[i] + b[i] */
_VML_API(void,VCADD,(MKL_INT *n, const MKL_Complex8  a[], const MKL_Complex8  b[], MKL_Complex8  r[]))
_VML_API(void,VZADD,(MKL_INT *n, const MKL_Complex16 a[], const MKL_Complex16 b[], MKL_Complex16 r[]))
_vml_api(void,vcadd,(MKL_INT *n, const MKL_Complex8  a[], const MKL_Complex8  b[], MKL_Complex8  r[]))
_vml_api(void,vzadd,(MKL_INT *n, const MKL_Complex16 a[], const MKL_Complex16 b[], MKL_Complex16 r[]))
_Vml_Api(void,vcAdd,(MKL_INT n,  const MKL_Complex8  a[], const MKL_Complex8  b[], MKL_Complex8  r[]))
_Vml_Api(void,vzAdd,(MKL_INT n,  const MKL_Complex16 a[], const MKL_Complex16 b[], MKL_Complex16 r[]))

/* Subtraction: r[i] = a[i] - b[i] */
_VML_API(void,VSSUB,(MKL_INT *n, const float  a[], const float  b[], float  r[]))
_VML_API(void,VDSUB,(MKL_INT *n, const double a[], const double b[], double r[]))
_vml_api(void,vssub,(MKL_INT *n, const float  a[], const float  b[], float  r[]))
_vml_api(void,vdsub,(MKL_INT *n, const double a[], const double b[], double r[]))
_Vml_Api(void,vsSub,(MKL_INT n,  const float  a[], const float  b[], float  r[]))
_Vml_Api(void,vdSub,(MKL_INT n,  const double a[], const double b[], double r[]))

/* Complex subtraction: r[i] = a[i] - b[i] */
_VML_API(void,VCSUB,(MKL_INT *n, const MKL_Complex8  a[], const MKL_Complex8  b[], MKL_Complex8  r[]))
_VML_API(void,VZSUB,(MKL_INT *n, const MKL_Complex16 a[], const MKL_Complex16 b[], MKL_Complex16 r[]))
_vml_api(void,vcsub,(MKL_INT *n, const MKL_Complex8  a[], const MKL_Complex8  b[], MKL_Complex8  r[]))
_vml_api(void,vzsub,(MKL_INT *n, const MKL_Complex16 a[], const MKL_Complex16 b[], MKL_Complex16 r[]))
_Vml_Api(void,vcSub,(MKL_INT n,  const MKL_Complex8  a[], const MKL_Complex8  b[], MKL_Complex8  r[]))
_Vml_Api(void,vzSub,(MKL_INT n,  const MKL_Complex16 a[], const MKL_Complex16 b[], MKL_Complex16 r[]))

/* Reciprocal: r[i] = 1.0 / a[i] */
_VML_API(void,VSINV,(MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDINV,(MKL_INT *n, const double a[], double r[]))
_vml_api(void,vsinv,(MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdinv,(MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsInv,(MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdInv,(MKL_INT n,  const double a[], double r[]))

/* Square Root: r[i] = a[i]^0.5 */
_VML_API(void,VSSQRT,(MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDSQRT,(MKL_INT *n, const double a[], double r[]))
_vml_api(void,vssqrt,(MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdsqrt,(MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsSqrt,(MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdSqrt,(MKL_INT n,  const double a[], double r[]))

/* Complex Square Root: r[i] = a[i]^0.5 */
_VML_API(void,VCSQRT,(MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_VML_API(void,VZSQRT,(MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_vml_api(void,vcsqrt,(MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_vml_api(void,vzsqrt,(MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_Vml_Api(void,vcSqrt,(MKL_INT n,  const MKL_Complex8  a[], MKL_Complex8  r[]))
_Vml_Api(void,vzSqrt,(MKL_INT n,  const MKL_Complex16 a[], MKL_Complex16 r[]))

/* Reciprocal Square Root: r[i] = 1/a[i]^0.5 */
_VML_API(void,VSINVSQRT,(MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDINVSQRT,(MKL_INT *n, const double a[], double r[]))
_vml_api(void,vsinvsqrt,(MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdinvsqrt,(MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsInvSqrt,(MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdInvSqrt,(MKL_INT n,  const double a[], double r[]))

/* Cube Root: r[i] = a[i]^(1/3) */
_VML_API(void,VSCBRT,(MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDCBRT,(MKL_INT *n, const double a[], double r[]))
_vml_api(void,vscbrt,(MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdcbrt,(MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsCbrt,(MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdCbrt,(MKL_INT n,  const double a[], double r[]))

/* Reciprocal Cube Root: r[i] = 1/a[i]^(1/3) */
_VML_API(void,VSINVCBRT,(MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDINVCBRT,(MKL_INT *n, const double a[], double r[]))
_vml_api(void,vsinvcbrt,(MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdinvcbrt,(MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsInvCbrt,(MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdInvCbrt,(MKL_INT n,  const double a[], double r[]))

/* Squaring: r[i] = a[i]^2 */
_VML_API(void,VSSQR,(MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDSQR,(MKL_INT *n, const double a[], double r[]))
_vml_api(void,vssqr,(MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdsqr,(MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsSqr,(MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdSqr,(MKL_INT n,  const double a[], double r[]))

/* Exponential Function: r[i] = e^a[i] */
_VML_API(void,VSEXP,(MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDEXP,(MKL_INT *n, const double a[], double r[]))
_vml_api(void,vsexp,(MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdexp,(MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsExp,(MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdExp,(MKL_INT n,  const double a[], double r[]))

/* : r[i] = e^(a[i]-1) */
_VML_API(void,VSEXPM1,(MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDEXPM1,(MKL_INT *n, const double a[], double r[]))
_vml_api(void,vsexpm1,(MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdexpm1,(MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsExpm1,(MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdExpm1,(MKL_INT n,  const double a[], double r[]))

/* Complex Exponential Function: r[i] = e^a[i] */
_VML_API(void,VCEXP,(MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_VML_API(void,VZEXP,(MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_vml_api(void,vcexp,(MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_vml_api(void,vzexp,(MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_Vml_Api(void,vcExp,(MKL_INT n,  const MKL_Complex8  a[], MKL_Complex8  r[]))
_Vml_Api(void,vzExp,(MKL_INT n,  const MKL_Complex16 a[], MKL_Complex16 r[]))

/* Logarithm (base e): r[i] = ln(a[i]) */
_VML_API(void,VSLN,(MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDLN,(MKL_INT *n, const double a[], double r[]))
_vml_api(void,vsln,(MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdln,(MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsLn,(MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdLn,(MKL_INT n,  const double a[], double r[]))

/* Complex Logarithm (base e): r[i] = ln(a[i]) */
_VML_API(void,VCLN,(MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_VML_API(void,VZLN,(MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_vml_api(void,vcln,(MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_vml_api(void,vzln,(MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_Vml_Api(void,vcLn,(MKL_INT n,  const MKL_Complex8  a[], MKL_Complex8  r[]))
_Vml_Api(void,vzLn,(MKL_INT n,  const MKL_Complex16 a[], MKL_Complex16 r[]))

/* Logarithm (base 10): r[i] = lg(a[i]) */
_VML_API(void,VSLOG10,(MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDLOG10,(MKL_INT *n, const double a[], double r[]))
_vml_api(void,vslog10,(MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdlog10,(MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsLog10,(MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdLog10,(MKL_INT n,  const double a[], double r[]))

/* Complex Logarithm (base 10): r[i] = lg(a[i]) */
_VML_API(void,VCLOG10,(MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_VML_API(void,VZLOG10,(MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_vml_api(void,vclog10,(MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_vml_api(void,vzlog10,(MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_Vml_Api(void,vcLog10,(MKL_INT n,  const MKL_Complex8  a[], MKL_Complex8  r[]))
_Vml_Api(void,vzLog10,(MKL_INT n,  const MKL_Complex16 a[], MKL_Complex16 r[]))

/* : r[i] = log(1+a[i]) */
_VML_API(void,VSLOG1P,(MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDLOG1P,(MKL_INT *n, const double a[], double r[]))
_vml_api(void,vslog1p,(MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdlog1p,(MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsLog1p,(MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdLog1p,(MKL_INT n,  const double a[], double r[]))

/* Cosine: r[i] = cos(a[i]) */
_VML_API(void,VSCOS,(MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDCOS,(MKL_INT *n, const double a[], double r[]))
_vml_api(void,vscos,(MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdcos,(MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsCos,(MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdCos,(MKL_INT n,  const double a[], double r[]))

/* Complex Cosine: r[i] = ccos(a[i]) */
_VML_API(void,VCCOS,(MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_VML_API(void,VZCOS,(MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_vml_api(void,vccos,(MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_vml_api(void,vzcos,(MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_Vml_Api(void,vcCos,(MKL_INT n,  const MKL_Complex8  a[], MKL_Complex8  r[]))
_Vml_Api(void,vzCos,(MKL_INT n,  const MKL_Complex16 a[], MKL_Complex16 r[]))

/* Sine: r[i] = sin(a[i]) */
_VML_API(void,VSSIN,(MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDSIN,(MKL_INT *n, const double a[], double r[]))
_vml_api(void,vssin,(MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdsin,(MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsSin,(MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdSin,(MKL_INT n,  const double a[], double r[]))

/* Complex Sine: r[i] = sin(a[i]) */
_VML_API(void,VCSIN,(MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_VML_API(void,VZSIN,(MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_vml_api(void,vcsin,(MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_vml_api(void,vzsin,(MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_Vml_Api(void,vcSin,(MKL_INT n,  const MKL_Complex8  a[], MKL_Complex8  r[]))
_Vml_Api(void,vzSin,(MKL_INT n,  const MKL_Complex16 a[], MKL_Complex16 r[]))

/* Tangent: r[i] = tan(a[i]) */
_VML_API(void,VSTAN,(MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDTAN,(MKL_INT *n, const double a[], double r[]))
_vml_api(void,vstan,(MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdtan,(MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsTan,(MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdTan,(MKL_INT n,  const double a[], double r[]))

/* Complex Tangent: r[i] = tan(a[i]) */
_VML_API(void,VCTAN,(MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_VML_API(void,VZTAN,(MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_vml_api(void,vctan,(MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_vml_api(void,vztan,(MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_Vml_Api(void,vcTan,(MKL_INT n,  const MKL_Complex8  a[], MKL_Complex8  r[]))
_Vml_Api(void,vzTan,(MKL_INT n,  const MKL_Complex16 a[], MKL_Complex16 r[]))

/* Hyperbolic Cosine: r[i] = ch(a[i]) */
_VML_API(void,VSCOSH,(MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDCOSH,(MKL_INT *n, const double a[], double r[]))
_vml_api(void,vscosh,(MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdcosh,(MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsCosh,(MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdCosh,(MKL_INT n,  const double a[], double r[]))

/* Complex Hyperbolic Cosine: r[i] = ch(a[i]) */
_VML_API(void,VCCOSH,(MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_VML_API(void,VZCOSH,(MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_vml_api(void,vccosh,(MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_vml_api(void,vzcosh,(MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_Vml_Api(void,vcCosh,(MKL_INT n,  const MKL_Complex8  a[], MKL_Complex8  r[]))
_Vml_Api(void,vzCosh,(MKL_INT n,  const MKL_Complex16 a[], MKL_Complex16 r[]))

/* Hyperbolic Sine: r[i] = sh(a[i]) */
_VML_API(void,VSSINH,(MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDSINH,(MKL_INT *n, const double a[], double r[]))
_vml_api(void,vssinh,(MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdsinh,(MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsSinh,(MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdSinh,(MKL_INT n,  const double a[], double r[]))

/* Complex Hyperbolic Sine: r[i] = sh(a[i]) */
_VML_API(void,VCSINH,(MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_VML_API(void,VZSINH,(MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_vml_api(void,vcsinh,(MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_vml_api(void,vzsinh,(MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_Vml_Api(void,vcSinh,(MKL_INT n,  const MKL_Complex8  a[], MKL_Complex8  r[]))
_Vml_Api(void,vzSinh,(MKL_INT n,  const MKL_Complex16 a[], MKL_Complex16 r[]))

/* Hyperbolic Tangent: r[i] = th(a[i]) */
_VML_API(void,VSTANH,(MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDTANH,(MKL_INT *n, const double a[], double r[]))
_vml_api(void,vstanh,(MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdtanh,(MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsTanh,(MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdTanh,(MKL_INT n,  const double a[], double r[]))

/* Complex Hyperbolic Tangent: r[i] = th(a[i]) */
_VML_API(void,VCTANH,(MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_VML_API(void,VZTANH,(MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_vml_api(void,vctanh,(MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_vml_api(void,vztanh,(MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_Vml_Api(void,vcTanh,(MKL_INT n,  const MKL_Complex8  a[], MKL_Complex8  r[]))
_Vml_Api(void,vzTanh,(MKL_INT n,  const MKL_Complex16 a[], MKL_Complex16 r[]))

/* Arc Cosine: r[i] = arccos(a[i]) */
_VML_API(void,VSACOS,(MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDACOS,(MKL_INT *n, const double a[], double r[]))
_vml_api(void,vsacos,(MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdacos,(MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsAcos,(MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdAcos,(MKL_INT n,  const double a[], double r[]))

/* Complex Arc Cosine: r[i] = arccos(a[i]) */
_VML_API(void,VCACOS,(MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_VML_API(void,VZACOS,(MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_vml_api(void,vcacos,(MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_vml_api(void,vzacos,(MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_Vml_Api(void,vcAcos,(MKL_INT n,  const MKL_Complex8  a[], MKL_Complex8  r[]))
_Vml_Api(void,vzAcos,(MKL_INT n,  const MKL_Complex16 a[], MKL_Complex16 r[]))

/* Arc Sine: r[i] = arcsin(a[i]) */
_VML_API(void,VSASIN,(MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDASIN,(MKL_INT *n, const double a[], double r[]))
_vml_api(void,vsasin,(MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdasin,(MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsAsin,(MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdAsin,(MKL_INT n,  const double a[], double r[]))

/* Complex Arc Sine: r[i] = arcsin(a[i]) */
_VML_API(void,VCASIN,(MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_VML_API(void,VZASIN,(MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_vml_api(void,vcasin,(MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_vml_api(void,vzasin,(MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_Vml_Api(void,vcAsin,(MKL_INT n,  const MKL_Complex8  a[], MKL_Complex8  r[]))
_Vml_Api(void,vzAsin,(MKL_INT n,  const MKL_Complex16 a[], MKL_Complex16 r[]))

/* Arc Tangent: r[i] = arctan(a[i]) */
_VML_API(void,VSATAN,(MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDATAN,(MKL_INT *n, const double a[], double r[]))
_vml_api(void,vsatan,(MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdatan,(MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsAtan,(MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdAtan,(MKL_INT n,  const double a[], double r[]))

/* Complex Arc Tangent: r[i] = arctan(a[i]) */
_VML_API(void,VCATAN,(MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_VML_API(void,VZATAN,(MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_vml_api(void,vcatan,(MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_vml_api(void,vzatan,(MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_Vml_Api(void,vcAtan,(MKL_INT n,  const MKL_Complex8  a[], MKL_Complex8  r[]))
_Vml_Api(void,vzAtan,(MKL_INT n,  const MKL_Complex16 a[], MKL_Complex16 r[]))

/* Hyperbolic Arc Cosine: r[i] = arcch(a[i]) */
_VML_API(void,VSACOSH,(MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDACOSH,(MKL_INT *n, const double a[], double r[]))
_vml_api(void,vsacosh,(MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdacosh,(MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsAcosh,(MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdAcosh,(MKL_INT n,  const double a[], double r[]))

/* Complex Hyperbolic Arc Cosine: r[i] = arcch(a[i]) */
_VML_API(void,VCACOSH,(MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_VML_API(void,VZACOSH,(MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_vml_api(void,vcacosh,(MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_vml_api(void,vzacosh,(MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_Vml_Api(void,vcAcosh,(MKL_INT n,  const MKL_Complex8  a[], MKL_Complex8  r[]))
_Vml_Api(void,vzAcosh,(MKL_INT n,  const MKL_Complex16 a[], MKL_Complex16 r[]))

/* Hyperbolic Arc Sine: r[i] = arcsh(a[i]) */
_VML_API(void,VSASINH,(MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDASINH,(MKL_INT *n, const double a[], double r[]))
_vml_api(void,vsasinh,(MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdasinh,(MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsAsinh,(MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdAsinh,(MKL_INT n,  const double a[], double r[]))

/* Complex Hyperbolic Arc Sine: r[i] = arcsh(a[i]) */
_VML_API(void,VCASINH,(MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_VML_API(void,VZASINH,(MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_vml_api(void,vcasinh,(MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_vml_api(void,vzasinh,(MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_Vml_Api(void,vcAsinh,(MKL_INT n,  const MKL_Complex8  a[], MKL_Complex8  r[]))
_Vml_Api(void,vzAsinh,(MKL_INT n,  const MKL_Complex16 a[], MKL_Complex16 r[]))

/* Hyperbolic Arc Tangent: r[i] = arcth(a[i]) */
_VML_API(void,VSATANH,(MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDATANH,(MKL_INT *n, const double a[], double r[]))
_vml_api(void,vsatanh,(MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdatanh,(MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsAtanh,(MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdAtanh,(MKL_INT n,  const double a[], double r[]))

/* Complex Hyperbolic Arc Tangent: r[i] = arcth(a[i]) */
_VML_API(void,VCATANH,(MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_VML_API(void,VZATANH,(MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_vml_api(void,vcatanh,(MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_vml_api(void,vzatanh,(MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_Vml_Api(void,vcAtanh,(MKL_INT n,  const MKL_Complex8  a[], MKL_Complex8  r[]))
_Vml_Api(void,vzAtanh,(MKL_INT n,  const MKL_Complex16 a[], MKL_Complex16 r[]))

/*  Error Function: r[i] = erf(a[i]) */
_VML_API(void,VSERF,(MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDERF,(MKL_INT *n, const double a[], double r[]))
_vml_api(void,vserf,(MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vderf,(MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsErf,(MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdErf,(MKL_INT n,  const double a[], double r[]))

/*  Inverse Error Function: r[i] = erf(a[i]) */
_VML_API(void,VSERFINV,(MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDERFINV,(MKL_INT *n, const double a[], double r[]))
_vml_api(void,vserfinv,(MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vderfinv,(MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsErfInv,(MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdErfInv,(MKL_INT n,  const double a[], double r[]))

/*   */
_VML_API(void,VSHYPOT,(MKL_INT *n, const float  a[], const float  b[], float  r[]))
_VML_API(void,VDHYPOT,(MKL_INT *n, const double a[], const double b[], double r[]))
_vml_api(void,vshypot,(MKL_INT *n, const float  a[], const float  b[], float  r[]))
_vml_api(void,vdhypot,(MKL_INT *n, const double a[], const double b[], double r[]))
_Vml_Api(void,vsHypot,(MKL_INT n,  const float  a[], const float  b[], float  r[]))
_Vml_Api(void,vdHypot,(MKL_INT n,  const double a[], const double b[], double r[]))

/*  Complementary Error Function: r[i] = 1 - erf(a[i]) */
_VML_API(void,VSERFC,(MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDERFC,(MKL_INT *n, const double a[], double r[]))
_vml_api(void,vserfc,(MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vderfc,(MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsErfc,(MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdErfc,(MKL_INT n,  const double a[], double r[]))

/* Arc Tangent of a/b: r[i] = arctan(a[i]/b[i]) */
_VML_API(void,VSATAN2,(MKL_INT *n, const float  a[], const float  b[], float  r[]))
_VML_API(void,VDATAN2,(MKL_INT *n, const double a[], const double b[], double r[]))
_vml_api(void,vsatan2,(MKL_INT *n, const float  a[], const float  b[], float  r[]))
_vml_api(void,vdatan2,(MKL_INT *n, const double a[], const double b[], double r[]))
_Vml_Api(void,vsAtan2,(MKL_INT n,  const float  a[], const float  b[], float  r[]))
_Vml_Api(void,vdAtan2,(MKL_INT n,  const double a[], const double b[], double r[]))

/* Multiplicaton: r[i] = a[i] * b[i] */
_VML_API(void,VSMUL,(MKL_INT *n, const float  a[], const float  b[], float  r[]))
_VML_API(void,VDMUL,(MKL_INT *n, const double a[], const double b[], double r[]))
_vml_api(void,vsmul,(MKL_INT *n, const float  a[], const float  b[], float  r[]))
_vml_api(void,vdmul,(MKL_INT *n, const double a[], const double b[], double r[]))
_Vml_Api(void,vsMul,(MKL_INT n,  const float  a[], const float  b[], float  r[]))
_Vml_Api(void,vdMul,(MKL_INT n,  const double a[], const double b[], double r[]))

/* Complex multiplication: r[i] = a[i] * b[i] */
_VML_API(void,VCMUL,(MKL_INT *n, const MKL_Complex8  a[], const MKL_Complex8  b[], MKL_Complex8  r[]))
_VML_API(void,VZMUL,(MKL_INT *n, const MKL_Complex16 a[], const MKL_Complex16 b[], MKL_Complex16 r[]))
_vml_api(void,vcmul,(MKL_INT *n, const MKL_Complex8  a[], const MKL_Complex8  b[], MKL_Complex8  r[]))
_vml_api(void,vzmul,(MKL_INT *n, const MKL_Complex16 a[], const MKL_Complex16 b[], MKL_Complex16 r[]))
_Vml_Api(void,vcMul,(MKL_INT n,  const MKL_Complex8  a[], const MKL_Complex8  b[], MKL_Complex8  r[]))
_Vml_Api(void,vzMul,(MKL_INT n,  const MKL_Complex16 a[], const MKL_Complex16 b[], MKL_Complex16 r[]))

/* Division: r[i] = a[i] / b[i] */
_VML_API(void,VSDIV,(MKL_INT *n, const float  a[], const float  b[], float  r[]))
_VML_API(void,VDDIV,(MKL_INT *n, const double a[], const double b[], double r[]))
_vml_api(void,vsdiv,(MKL_INT *n, const float  a[], const float  b[], float  r[]))
_vml_api(void,vddiv,(MKL_INT *n, const double a[], const double b[], double r[]))
_Vml_Api(void,vsDiv,(MKL_INT n,  const float  a[], const float  b[], float  r[]))
_Vml_Api(void,vdDiv,(MKL_INT n,  const double a[], const double b[], double r[]))

/* Complex division: r[i] = a[i] / b[i] */
_VML_API(void,VCDIV,(MKL_INT *n, const MKL_Complex8  a[], const MKL_Complex8  b[], MKL_Complex8  r[]))
_VML_API(void,VZDIV,(MKL_INT *n, const MKL_Complex16 a[], const MKL_Complex16 b[], MKL_Complex16 r[]))
_vml_api(void,vcdiv,(MKL_INT *n, const MKL_Complex8  a[], const MKL_Complex8  b[], MKL_Complex8  r[]))
_vml_api(void,vzdiv,(MKL_INT *n, const MKL_Complex16 a[], const MKL_Complex16 b[], MKL_Complex16 r[]))
_Vml_Api(void,vcDiv,(MKL_INT n,  const MKL_Complex8  a[], const MKL_Complex8  b[], MKL_Complex8  r[]))
_Vml_Api(void,vzDiv,(MKL_INT n,  const MKL_Complex16 a[], const MKL_Complex16 b[], MKL_Complex16 r[]))

/* Power Function: r[i] = a[i]^b[i] */
_VML_API(void,VSPOW,(MKL_INT *n, const float  a[], const float  b[], float  r[]))
_VML_API(void,VDPOW,(MKL_INT *n, const double a[], const double b[], double r[]))
_vml_api(void,vspow,(MKL_INT *n, const float  a[], const float  b[], float  r[]))
_vml_api(void,vdpow,(MKL_INT *n, const double a[], const double b[], double r[]))
_Vml_Api(void,vsPow,(MKL_INT n,  const float  a[], const float  b[], float  r[]))
_Vml_Api(void,vdPow,(MKL_INT n,  const double a[], const double b[], double r[]))

/* Complex Power Function: r[i] = a[i]^b[i] */
_VML_API(void,VCPOW,(MKL_INT *n, const MKL_Complex8  a[], const MKL_Complex8  b[], MKL_Complex8  r[]))
_VML_API(void,VZPOW,(MKL_INT *n, const MKL_Complex16 a[], const MKL_Complex16 b[], MKL_Complex16 r[]))
_vml_api(void,vcpow,(MKL_INT *n, const MKL_Complex8  a[], const MKL_Complex8  b[], MKL_Complex8  r[]))
_vml_api(void,vzpow,(MKL_INT *n, const MKL_Complex16 a[], const MKL_Complex16 b[], MKL_Complex16 r[]))
_Vml_Api(void,vcPow,(MKL_INT n,  const MKL_Complex8  a[], const MKL_Complex8  b[], MKL_Complex8  r[]))
_Vml_Api(void,vzPow,(MKL_INT n,  const MKL_Complex16 a[], const MKL_Complex16 b[], MKL_Complex16 r[]))

/* Power Function: r[i] = a[i]^(3/2) */
_VML_API(void,VSPOW3O2,(MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDPOW3O2,(MKL_INT *n, const double a[], double r[]))
_vml_api(void,vspow3o2,(MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdpow3o2,(MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsPow3o2,(MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdPow3o2,(MKL_INT n,  const double a[], double r[]))

/* Power Function: r[i] = a[i]^(2/3) */
_VML_API(void,VSPOW2O3,(MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDPOW2O3,(MKL_INT *n, const double a[], double r[]))
_vml_api(void,vspow2o3,(MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdpow2o3,(MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsPow2o3,(MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdPow2o3,(MKL_INT n,  const double a[], double r[]))

/* Power Function with Fixed Degree: r[i] = a[i]^b */
_VML_API(void,VSPOWX,(MKL_INT *n, const float  a[], const float  *b, float  r[]))
_VML_API(void,VDPOWX,(MKL_INT *n, const double a[], const double *b, double r[]))
_vml_api(void,vspowx,(MKL_INT *n, const float  a[], const float  *b, float  r[]))
_vml_api(void,vdpowx,(MKL_INT *n, const double a[], const double *b, double r[]))
_Vml_Api(void,vsPowx,(MKL_INT n,  const float  a[], const float   b, float  r[]))
_Vml_Api(void,vdPowx,(MKL_INT n,  const double a[], const double  b, double r[]))

/* Complex Power Function with Fixed Degree: r[i] = a[i]^b */
_VML_API(void,VCPOWX,(MKL_INT *n, const MKL_Complex8  a[], const MKL_Complex8  *b, MKL_Complex8  r[]))
_VML_API(void,VZPOWX,(MKL_INT *n, const MKL_Complex16 a[], const MKL_Complex16 *b, MKL_Complex16 r[]))
_vml_api(void,vcpowx,(MKL_INT *n, const MKL_Complex8  a[], const MKL_Complex8  *b, MKL_Complex8  r[]))
_vml_api(void,vzpowx,(MKL_INT *n, const MKL_Complex16 a[], const MKL_Complex16 *b, MKL_Complex16 r[]))
_Vml_Api(void,vcPowx,(MKL_INT n,  const MKL_Complex8  a[], const MKL_Complex8   b, MKL_Complex8  r[]))
_Vml_Api(void,vzPowx,(MKL_INT n,  const MKL_Complex16 a[], const MKL_Complex16  b, MKL_Complex16 r[]))

/* Sine & Cosine: r1[i] = sin(a[i]), r2[i]=cos(a[i]) */
_VML_API(void,VSSINCOS,(MKL_INT *n, const float  a[], float  r1[], float  r2[]))
_VML_API(void,VDSINCOS,(MKL_INT *n, const double a[], double r1[], double r2[]))
_vml_api(void,vssincos,(MKL_INT *n, const float  a[], float  r1[], float  r2[]))
_vml_api(void,vdsincos,(MKL_INT *n, const double a[], double r1[], double r2[]))
_Vml_Api(void,vsSinCos,(MKL_INT n,  const float  a[], float  r1[], float  r2[]))
_Vml_Api(void,vdSinCos,(MKL_INT n,  const double a[], double r1[], double r2[]))

/*  */
_VML_API(void,VSCEIL,(MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDCEIL,(MKL_INT *n, const double a[], double r[]))
_vml_api(void,vsceil,(MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdceil,(MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsCeil,(MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdCeil,(MKL_INT n,  const double a[], double r[]))

/*  */
_VML_API(void,VSFLOOR,(MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDFLOOR,(MKL_INT *n, const double a[], double r[]))
_vml_api(void,vsfloor,(MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdfloor,(MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsFloor,(MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdFloor,(MKL_INT n,  const double a[], double r[]))

/*  */
_VML_API(void,VSMODF,(MKL_INT *n, const float  a[], float  r1[], float  r2[]))
_VML_API(void,VDMODF,(MKL_INT *n, const double a[], double r1[], double r2[]))
_vml_api(void,vsmodf,(MKL_INT *n, const float  a[], float  r1[], float  r2[]))
_vml_api(void,vdmodf,(MKL_INT *n, const double a[], double r1[], double r2[]))
_Vml_Api(void,vsModf,(MKL_INT n,  const float  a[], float  r1[], float  r2[]))
_Vml_Api(void,vdModf,(MKL_INT n,  const double a[], double r1[], double r2[]))

/*  */
_VML_API(void,VSNEARBYINT,(MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDNEARBYINT,(MKL_INT *n, const double a[], double r[]))
_vml_api(void,vsnearbyint,(MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdnearbyint,(MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsNearbyInt,(MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdNearbyInt,(MKL_INT n,  const double a[], double r[]))

/*  */
_VML_API(void,VSRINT,(MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDRINT,(MKL_INT *n, const double a[], double r[]))
_vml_api(void,vsrint,(MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdrint,(MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsRint,(MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdRint,(MKL_INT n,  const double a[], double r[]))

/*  */
_VML_API(void,VSROUND,(MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDROUND,(MKL_INT *n, const double a[], double r[]))
_vml_api(void,vsround,(MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdround,(MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsRound,(MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdRound,(MKL_INT n,  const double a[], double r[]))

/*  */
_VML_API(void,VSTRUNC,(MKL_INT *n, const float  a[], float  r[]))
_VML_API(void,VDTRUNC,(MKL_INT *n, const double a[], double r[]))
_vml_api(void,vstrunc,(MKL_INT *n, const float  a[], float  r[]))
_vml_api(void,vdtrunc,(MKL_INT *n, const double a[], double r[]))
_Vml_Api(void,vsTrunc,(MKL_INT n,  const float  a[], float  r[]))
_Vml_Api(void,vdTrunc,(MKL_INT n,  const double a[], double r[]))

/* : r[i] =  */
_VML_API(void,VCCONJ,(MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_VML_API(void,VZCONJ,(MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_vml_api(void,vcconj,(MKL_INT *n, const MKL_Complex8  a[], MKL_Complex8  r[]))
_vml_api(void,vzconj,(MKL_INT *n, const MKL_Complex16 a[], MKL_Complex16 r[]))
_Vml_Api(void,vcConj,(MKL_INT n,  const MKL_Complex8  a[], MKL_Complex8  r[]))
_Vml_Api(void,vzConj,(MKL_INT n,  const MKL_Complex16 a[], MKL_Complex16 r[]))

/* : r[i] =  */
_VML_API(void,VCMULBYCONJ,(MKL_INT *n, const MKL_Complex8  a[], const MKL_Complex8  b[], MKL_Complex8  r[]))
_VML_API(void,VZMULBYCONJ,(MKL_INT *n, const MKL_Complex16 a[], const MKL_Complex16 b[], MKL_Complex16 r[]))
_vml_api(void,vcmulbyconj,(MKL_INT *n, const MKL_Complex8  a[], const MKL_Complex8  b[], MKL_Complex8  r[]))
_vml_api(void,vzmulbyconj,(MKL_INT *n, const MKL_Complex16 a[], const MKL_Complex16 b[], MKL_Complex16 r[]))
_Vml_Api(void,vcMulByConj,(MKL_INT n,  const MKL_Complex8  a[], const MKL_Complex8  b[], MKL_Complex8  r[]))
_Vml_Api(void,vzMulByConj,(MKL_INT n,  const MKL_Complex16 a[], const MKL_Complex16 b[], MKL_Complex16 r[]))

/* : r[i] =  */
_VML_API(void,VCCIS,(MKL_INT *n, const float  a[], MKL_Complex8  r[]))
_VML_API(void,VZCIS,(MKL_INT *n, const double a[], MKL_Complex16 r[]))
_vml_api(void,vccis,(MKL_INT *n, const float  a[], MKL_Complex8  r[]))
_vml_api(void,vzcis,(MKL_INT *n, const double a[], MKL_Complex16 r[]))
_Vml_Api(void,vcCIS,(MKL_INT n,  const float  a[], MKL_Complex8  r[]))
_Vml_Api(void,vzCIS,(MKL_INT n,  const double a[], MKL_Complex16 r[]))


/*
//++
//  VML PACK FUNCTION DECLARATIONS.
//--
*/
/* Positive Increment Indexing */
_VML_API(void,VSPACKI,(MKL_INT *n, const float  a[], const MKL_INT * incra, float  y[]))
_VML_API(void,VDPACKI,(MKL_INT *n, const double a[], const MKL_INT * incra, double y[]))
_vml_api(void,vspacki,(MKL_INT *n, const float  a[], const MKL_INT * incra, float  y[]))
_vml_api(void,vdpacki,(MKL_INT *n, const double a[], const MKL_INT * incra, double y[]))
_Vml_Api(void,vsPackI,(MKL_INT n,  const float  a[], const MKL_INT   incra, float  y[]))
_Vml_Api(void,vdPackI,(MKL_INT n,  const double a[], const MKL_INT   incra, double y[]))

/* Index Vector Indexing */
_VML_API(void,VSPACKV,(MKL_INT *n, const float  a[], const MKL_INT ia[], float  y[]))
_VML_API(void,VDPACKV,(MKL_INT *n, const double a[], const MKL_INT ia[], double y[]))
_vml_api(void,vspackv,(MKL_INT *n, const float  a[], const MKL_INT ia[], float  y[]))
_vml_api(void,vdpackv,(MKL_INT *n, const double a[], const MKL_INT ia[], double y[]))
_Vml_Api(void,vsPackV,(MKL_INT n,  const float  a[], const MKL_INT ia[], float  y[]))
_Vml_Api(void,vdPackV,(MKL_INT n,  const double a[], const MKL_INT ia[], double y[]))

/* Mask Vector Indexing */
_VML_API(void,VSPACKM,(MKL_INT *n, const float  a[], const MKL_INT ma[], float  y[]))
_VML_API(void,VDPACKM,(MKL_INT *n, const double a[], const MKL_INT ma[], double y[]))
_vml_api(void,vspackm,(MKL_INT *n, const float  a[], const MKL_INT ma[], float  y[]))
_vml_api(void,vdpackm,(MKL_INT *n, const double a[], const MKL_INT ma[], double y[]))
_Vml_Api(void,vsPackM,(MKL_INT n,  const float  a[], const MKL_INT ma[], float  y[]))
_Vml_Api(void,vdPackM,(MKL_INT n,  const double a[], const MKL_INT ma[], double y[]))

/*
//++
//  VML UNPACK FUNCTION DECLARATIONS.
//--
*/
/* Positive Increment Indexing */
_VML_API(void,VSUNPACKI,(MKL_INT *n, const float  a[], float  y[], const MKL_INT * incry))
_VML_API(void,VDUNPACKI,(MKL_INT *n, const double a[], double y[], const MKL_INT * incry))
_vml_api(void,vsunpacki,(MKL_INT *n, const float  a[], float  y[], const MKL_INT * incry))
_vml_api(void,vdunpacki,(MKL_INT *n, const double a[], double y[], const MKL_INT * incry))
_Vml_Api(void,vsUnpackI,(MKL_INT n,  const float  a[], float  y[], const MKL_INT incry  ))
_Vml_Api(void,vdUnpackI,(MKL_INT n,  const double a[], double y[], const MKL_INT incry  ))

/* Index Vector Indexing */
_VML_API(void,VSUNPACKV,(MKL_INT *n, const float  a[], float  y[], const MKL_INT iy[]   ))
_VML_API(void,VDUNPACKV,(MKL_INT *n, const double a[], double y[], const MKL_INT iy[]   ))
_vml_api(void,vsunpackv,(MKL_INT *n, const float  a[], float  y[], const MKL_INT iy[]   ))
_vml_api(void,vdunpackv,(MKL_INT *n, const double a[], double y[], const MKL_INT iy[]   ))
_Vml_Api(void,vsUnpackV,(MKL_INT n,  const float  a[], float  y[], const MKL_INT iy[]   ))
_Vml_Api(void,vdUnpackV,(MKL_INT n,  const double a[], double y[], const MKL_INT iy[]   ))

/* Mask Vector Indexing */
_VML_API(void,VSUNPACKM,(MKL_INT *n, const float  a[], float  y[], const MKL_INT my[]   ))
_VML_API(void,VDUNPACKM,(MKL_INT *n, const double a[], double y[], const MKL_INT my[]   ))
_vml_api(void,vsunpackm,(MKL_INT *n, const float  a[], float  y[], const MKL_INT my[]   ))
_vml_api(void,vdunpackm,(MKL_INT *n, const double a[], double y[], const MKL_INT my[]   ))
_Vml_Api(void,vsUnpackM,(MKL_INT n,  const float  a[], float  y[], const MKL_INT my[]   ))
_Vml_Api(void,vdUnpackM,(MKL_INT n,  const double a[], double y[], const MKL_INT my[]   ))


/*
//++
//  VML ERROR HANDLING FUNCTION DECLARATIONS.
//--
*/
/* Set VML Error Status */
_VML_API(int,VMLSETERRSTATUS,(MKL_INT * status))
_vml_api(int,vmlseterrstatus,(MKL_INT * status))
_Vml_Api(int,vmlSetErrStatus,(MKL_INT   status))

/* Get VML Error Status */
_VML_API(int,VMLGETERRSTATUS,(void))
_vml_api(int,vmlgeterrstatus,(void))
_Vml_Api(int,vmlGetErrStatus,(void))

/* Clear VML Error Status */
_VML_API(int,VMLCLEARERRSTATUS,(void))
_vml_api(int,vmlclearerrstatus,(void))
_Vml_Api(int,vmlClearErrStatus,(void))

/* Set VML Error Callback Function */
_VML_API(VMLErrorCallBack,VMLSETERRORCALLBACK,(VMLErrorCallBack func))
_vml_api(VMLErrorCallBack,vmlseterrorcallback,(VMLErrorCallBack func))
_Vml_Api(VMLErrorCallBack,vmlSetErrorCallBack,(VMLErrorCallBack func))

/* Get VML Error Callback Function */
_VML_API(VMLErrorCallBack,VMLGETERRORCALLBACK,(void))
_vml_api(VMLErrorCallBack,vmlgeterrorcallback,(void))
_Vml_Api(VMLErrorCallBack,vmlGetErrorCallBack,(void))

/* Reset VML Error Callback Function */
_VML_API(VMLErrorCallBack,VMLCLEARERRORCALLBACK,(void))
_vml_api(VMLErrorCallBack,vmlclearerrorcallback,(void))
_Vml_Api(VMLErrorCallBack,vmlClearErrorCallBack,(void))


/*
//++
//  VML MODE FUNCTION DECLARATIONS.
//--
*/
/* Set VML Mode */
_VML_API(unsigned int,VMLSETMODE,(unsigned MKL_INT *newmode))
_vml_api(unsigned int,vmlsetmode,(unsigned MKL_INT *newmode))
_Vml_Api(unsigned int,vmlSetMode,(unsigned MKL_INT  newmode))

/* Get VML Mode */
_VML_API(unsigned int,VMLGETMODE,(void))
_vml_api(unsigned int,vmlgetmode,(void))
_Vml_Api(unsigned int,vmlGetMode,(void))


#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __MKL_VML_FUNCTIONS_H__ */
