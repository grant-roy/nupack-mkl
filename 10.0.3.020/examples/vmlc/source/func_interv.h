/*******************************************************************************
!                             INTEL CONFIDENTIAL
!  Copyright(C) 2001-2008 Intel Corporation. All Rights Reserved.
!  The source code contained  or  described herein and all documents related to
!  the source code ("Material") are owned by Intel Corporation or its suppliers
!  or licensors.  Title to the  Material remains with  Intel Corporation or its
!  suppliers and licensors. The Material contains trade secrets and proprietary
!  and  confidential  information of  Intel or its suppliers and licensors. The
!  Material  is  protected  by  worldwide  copyright  and trade secret laws and
!  treaty  provisions. No part of the Material may be used, copied, reproduced,
!  modified, published, uploaded, posted, transmitted, distributed or disclosed
!  in any way without Intel's prior express written permission.
!  No license  under any  patent, copyright, trade secret or other intellectual
!  property right is granted to or conferred upon you by disclosure or delivery
!  of the Materials,  either expressly, by implication, inducement, estoppel or
!  otherwise.  Any  license  under  such  intellectual property  rights must be
!  express and approved by Intel in writing.
!
!*******************************************************************************
!  Content:
!    service file for examples on VML functions
!******************************************************************************/

#define VEC_LEN 11

#define DEPS 0.1
#define SEPS 0.1

#define __SINV_BEG -10000.0
#define __SINV_END 10000.0

#define __SDIV_BEG -10000.0
#define __SDIV_END 10000.0

#define __SSQRT_BEG 0.0
#define __SSQRT_END 10000.0

#define __SINVSQRT_BEG 0.1
#define __SINVSQRT_END 10000.0

#define __SPOW_BEG 0.1
#define __SPOW_END 7.0

#define __SPOW2O3_BEG 1.0
#define __SPOW2O3_END 7.0

#define __SPOW3O2_BEG 1.0
#define __SPOW3O2_END 7.0

#define __SSQR_BEG  1.0
#define __SSQR_END 70.0

#define __SSUB_BEG -10000.0
#define __SSUB_END  10000.0

#define __SADD_BEG -10000.0
#define __SADD_END  10000.0

#define __SCIS_BEG -10000.0
#define __SCIS_END  10000.0

#define __SMUL_BEG -100.0
#define __SMUL_END  100.0

#define __SCONJ_BEG -100.0
#define __SCONJ_END  100.0

#define __SMULBYCONJ_BEG -100.0
#define __SMULBYCONJ_END  100.0

#define __SABS_BEG -1000.0
#define __SABS_END  1000.0

#define __SPOWX_BEG 0.1
#define __SPOWX_END 7.0

#define __SERF_BEG 1.0
#define __SERF_END 4.0

#define __SERFC_BEG 1.0
#define __SERFC_END 4.0

#define __SEXP_BEG -17.0
#define __SEXP_END 18.0

#define __SEXPM1_BEG -17.0
#define __SEXPM1_END 18.0

#define __SLN_BEG 0.1
#define __SLN_END 10000.0

#define __SLOG10_BEG 0.1
#define __SLOG10_END 10000.0

#define __SLOG1P_BEG 0.1
#define __SLOG1P_END 10000.0

#define __SCOS_BEG -10000.0
#define __SCOS_END 10000.0

#define __SSIN_BEG -10000.0
#define __SSIN_END 10000.0

#define __SSINCOS_BEG -10000.0
#define __SSINCOS_END 10000.0

#define __STAN_BEG -10000.0
#define __STAN_END 10000.0

#define __SACOS_BEG -0.9
#define __SACOS_END +0.9

#define __SASIN_BEG -1.0
#define __SASIN_END +1.0

#define __SATAN_BEG -10000.0
#define __SATAN_END 10000.0

#define __SATAN2_BEG -10000.0
#define __SATAN2_END 10000.0

#define __SCOSH_BEG -7.0
#define __SCOSH_END 7.0

#define __SSINH_BEG -7.0
#define __SSINH_END 7.0

#define __STANH_BEG -10.0
#define __STANH_END 10.0

#define __SACOSH_BEG 3.5
#define __SACOSH_END 10000.0

#define __SASINH_BEG 3.0
#define __SASINH_END 10000.0

#define __SATANH_BEG 0.75
#define __SATANH_END 0.9

#define __SCBRT_BEG 0.0
#define __SCBRT_END 10000.0

#define __SINVCBRT_BEG 0.1
#define __SINVCBRT_END 10000.0

#define __SHYPOT_BEG 1.001
#define __SHYPOT_END 10000.0

#define __SERFINV_BEG 0.01
#define __SERFINV_END 0.99

#define __SCEIL_BEG -2.00
#define __SCEIL_END  2.00

#define __SFLOOR_BEG -2.00
#define __SFLOOR_END  2.00

#define __SMODF_BEG -2.00
#define __SMODF_END  2.00

#define __SNEARBYINT_BEG -2.00
#define __SNEARBYINT_END  2.00

#define __SRINT_BEG -2.00
#define __SRINT_END  2.00

#define __SROUND_BEG -2.00
#define __SROUND_END  2.00

#define __STRUNC_BEG -2.00
#define __STRUNC_END  2.00

#define __DINV_BEG -10000.0
#define __DINV_END 10000.0

#define __DDIV_BEG -100.0
#define __DDIV_END 100.0

#define __DSQRT_BEG 0.0
#define __DSQRT_END 10000.0

#define __DINVSQRT_BEG 0.1
#define __DINVSQRT_END 10000.0

#define __DPOW_BEG 0.1
#define __DPOW_END 7.0

#define __DPOW2O3_BEG 1.0
#define __DPOW2O3_END 7.0

#define __DPOW3O2_BEG 1.0
#define __DPOW3O2_END 7.0

#define __DSQR_BEG  1.0
#define __DSQR_END 70.0

#define __DSUB_BEG -10000.0
#define __DSUB_END  10000.0

#define __DADD_BEG -10000.0
#define __DADD_END  10000.0

#define __DCIS_BEG -10000.0
#define __DCIS_END  10000.0

#define __DMUL_BEG -100.0
#define __DMUL_END  100.0

#define __DCONJ_BEG -100.0
#define __DCONJ_END  100.0

#define __DMULBYCONJ_BEG -100.0
#define __DMULBYCONJ_END  100.0

#define __DABS_BEG -1000.0
#define __DABS_END  1000.0

#define __DPOWX_BEG 0.1
#define __DPOWX_END 7.0

#define __DERF_BEG 0.5
#define __DERF_END 5.9

#define __DERFC_BEG 0.5
#define __DERFC_END 5.9

#define __DEXP_BEG -17.0
#define __DEXP_END 17.0

#define __DEXPM1_BEG -17.0
#define __DEXPM1_END 17.0

#define __DLN_BEG 0.1
#define __DLN_END 10000.0

#define __DLOG10_BEG 0.1
#define __DLOG10_END 10000.0

#define __DLOG1P_BEG 0.1
#define __DLOG1P_END 10000.0

#define __DCOS_BEG -10000.0
#define __DCOS_END 10000.0

#define __DSIN_BEG -10000.0
#define __DSIN_END 10000.0

#define __DSINCOS_BEG -10000.0
#define __DSINCOS_END 10000.0

#define __DTAN_BEG -10000.0
#define __DTAN_END 10000.0

#define __DACOS_BEG -0.9
#define __DACOS_END +0.9

#define __DASIN_BEG -1.0
#define __DASIN_END +1.0

#define __DATAN_BEG -10000.0
#define __DATAN_END +10000.0

#define __DATAN2_BEG -10000.0
#define __DATAN2_END +10000.0

#define __DCOSH_BEG -7.0
#define __DCOSH_END 7.0

#define __DSINH_BEG -7.0
#define __DSINH_END 7.0

#define __DTANH_BEG -20.0
#define __DTANH_END 20.0

#define __DACOSH_BEG 1.001
#define __DACOSH_END 10000.0

#define __DASINH_BEG 0.002
#define __DASINH_END 10000.0

#define __DATANH_BEG 0.001
#define __DATANH_END 0.9

#define __DCBRT_BEG 0.0
#define __DCBRT_END 10000.0

#define __DINVCBRT_BEG 0.1
#define __DINVCBRT_END 10000.0

#define __DHYPOT_BEG 1.001
#define __DHYPOT_END 10000.0

#define __DERFINV_BEG 0.01
#define __DERFINV_END 0.99

#define __DCEIL_BEG -2.00
#define __DCEIL_END  2.00

#define __DFLOOR_BEG -2.00
#define __DFLOOR_END  2.00

#define __DMODF_BEG -2.00
#define __DMODF_END  2.00

#define __DNEARBYINT_BEG -2.00
#define __DNEARBYINT_END  2.00

#define __DRINT_BEG -2.00
#define __DRINT_END  2.00

#define __DROUND_BEG -2.00
#define __DROUND_END  2.00

#define __DTRUNC_BEG -2.00
#define __DTRUNC_END  2.00
