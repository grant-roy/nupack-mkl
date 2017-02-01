/*******************************************************************************
!                              INTEL CONFIDENTIAL
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
!*******************************************************************************
!   function    : fftw_plan_r2r_1d - wrapper for interface FFTW function
!   Content     : wrapper from this function to DFTI MKL computation functions
!               : for double precision real data
!   input data  : parameters for DFT
!   output data : pointer to FFTW plan
!******************************************************************************/
#include "fftw3.h"
#include "fftw3_mkl.h"
#include "tt_mkl.h"
#include "mkl_trig_transforms.h"

fftw_plan
fftw_plan_r2r_1d(int n, double *in, double *out, fftw_r2r_kind kind,
                 unsigned flags)
{
    fftw_plan_mkl*  fftw_desc_mkl;
    DFTI_DESCRIPTOR_HANDLE handle = 0;
    TT_FFTW_MKL_HANDLE tt_desc_mkl;
    double *dpar;
    MKL_INT    *ipar;
    MKL_INT tt_type,ir,n_1,nn;

    fftw_desc_mkl = (fftw_plan_mkl*)malloc(sizeof(fftw_plan_mkl));
    if (fftw_desc_mkl == NULL) return NULL;
    tt_desc_mkl = (TT_FFTW_MKL_HANDLE)malloc(sizeof(TT_FFTW_MKL_HANDLE));
    if (tt_desc_mkl == NULL) return NULL;

    nn = (MKL_INT)n;

    switch(kind)
    {
        /*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/
    case FFTW_RODFT00:
        {         /*--------------------------------------------------------------------------------*/
            printf("\n============================================================================\n");
            printf("FFTW2MKL WARNING: To avoid losses in performance, it is highly recommended to\n");
            printf("to use MKL_RODFT00 instead of FFTW_RODFT00 flag! Please be aware that\n");
            printf("MKL_RODFT00 flag requires larger by one element input/output arrays with\n");
            printf("the first element equals to zero. For more details please refer to\n");
            printf("Trigonometric Transform Routines Section of MKL Manual.\n");
            printf("============================================================================\n");

            tt_type = 0;

            n_1 = nn + 1 ;
            dpar=(double*)malloc((3*n_1/2+2)*sizeof(double));
            ipar=(int*)malloc((128)*sizeof(int));
            d_init_trig_transform(&n_1,&tt_type,ipar,dpar,&ir);
            if (ir != 0)
            {
                printf("\n============================================================================\n");
                printf("FFTW2MKL FATAL ERROR: MKL TT initialization has failed with status=%d\n",(int)ir);
                printf("Please refer to the Trigonometric Transform Routines Section of MKL Manual\n");
                printf("to find what went wrong...\n");
                printf("============================================================================\n");
                return NULL;
            }
            ipar[10] = 1;
            ipar[14] = n_1-1;
            ipar[15] = 0; /* Boolean for FFTW_RODFT00 / MKL_RODFT00*/
            d_commit_trig_transform(out,&handle,ipar,dpar,&ir);
            if (ir != 0)
            {
                printf("\n============================================================================\n");
                printf("FFTW2MKL FATAL ERROR: MKL TT commit step has failed with status=%d\n",(int)ir);
                printf("Please refer to the Trigonometric Transform Routines Section of MKL Manual\n");
                printf("to find what went wrong...\n");
                printf("============================================================================\n");
                return NULL;
            }
            fftw_desc_mkl->mkl_desc = handle;
            fftw_desc_mkl->in = (void*)in;
            fftw_desc_mkl->out = (void*)out;
            fftw_desc_mkl->sign = FFTW_BACKWARD;

            tt_desc_mkl->ipar = (void*)ipar;
            tt_desc_mkl->dpar = (void*)dpar;
            fftw_desc_mkl->tt_fftw_mkl = tt_desc_mkl;

            return (fftw_plan)fftw_desc_mkl;
        }
        /*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/
    case FFTW_REDFT00:
        {
            tt_type = 1;

            n_1 = nn - 1 ;
            dpar=(double*)malloc((3*n_1/2+2)*sizeof(double));
            ipar=(int*)malloc((128)*sizeof(int));
            d_init_trig_transform(&n_1,&tt_type,ipar,dpar,&ir);
            if (ir != 0)
            {
                printf("\n============================================================================\n");
                printf("FFTW2MKL FATAL ERROR: MKL TT initialization has failed with status=%d\n",(int)ir);
                printf("Please refer to the Trigonometric Transform Routines Section of MKL Manual\n");
                printf("to find what went wrong...\n");
                printf("============================================================================\n");
                return NULL;
            }
            ipar[10] = 1;
            ipar[14] = n_1+1;

            d_commit_trig_transform(out,&handle,ipar,dpar,&ir);
            if (ir != 0)
            {
                printf("\n============================================================================\n");
                printf("FFTW2MKL FATAL ERROR: MKL TT commit step has failed with status=%d\n",(int)ir);
                printf("Please refer to the Trigonometric Transform Routines Section of MKL Manual\n");
                printf("to find what went wrong...\n");
                printf("============================================================================\n");
                return NULL;
            }
            fftw_desc_mkl->mkl_desc = handle;
            fftw_desc_mkl->in = (void*)in;
            fftw_desc_mkl->out = (void*)out;
            fftw_desc_mkl->sign = FFTW_BACKWARD;

            tt_desc_mkl->ipar = (void*)ipar;
            tt_desc_mkl->dpar = (void*)dpar;
            fftw_desc_mkl->tt_fftw_mkl = tt_desc_mkl;

            return (fftw_plan)fftw_desc_mkl;
        }
        /*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/
    case FFTW_REDFT10:
        {
            tt_type = 2;

            n_1 = nn;
            dpar=(double*)malloc((3*n_1/2+2)*sizeof(double));
            ipar=(int*)malloc((128)*sizeof(int));
            d_init_trig_transform(&n_1,&tt_type,ipar,dpar,&ir);
            if (ir != 0)
            {
                printf("\n============================================================================\n");
                printf("FFTW2MKL FATAL ERROR: MKL TT initialization has failed with status=%d\n",(int)ir);
                printf("Please refer to the Trigonometric Transform Routines Section of MKL Manual\n");
                printf("to find what went wrong...\n");
                printf("============================================================================\n");
                return NULL;
            }
            ipar[10] = 1;
            ipar[14] = n_1;

            d_commit_trig_transform(out,&handle,ipar,dpar,&ir);
            if (ir != 0)
            {
                printf("\n============================================================================\n");
                printf("FFTW2MKL FATAL ERROR: MKL TT commit step has failed with status=%d\n",(int)ir);
                printf("Please refer to the Trigonometric Transform Routines Section of MKL Manual\n");
                printf("to find what went wrong...\n");
                printf("============================================================================\n");
                return NULL;
            }
            fftw_desc_mkl->mkl_desc = handle;
            fftw_desc_mkl->in = (void*)in;
            fftw_desc_mkl->out = (void*)out;
            fftw_desc_mkl->sign = FFTW_BACKWARD;

            tt_desc_mkl->ipar = (void*)ipar;
            tt_desc_mkl->dpar = (void*)dpar;
            fftw_desc_mkl->tt_fftw_mkl = tt_desc_mkl;

            return (fftw_plan)fftw_desc_mkl;
        }
        /*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/
    case FFTW_REDFT11:
        {
            tt_type = 4;

            n_1 = nn;
            dpar=(double*)malloc((5*n_1/2+2)*sizeof(double));
            ipar=(int*)malloc((128)*sizeof(int));
            d_init_trig_transform(&n_1,&tt_type,ipar,dpar,&ir);
            if (ir != 0)
            {
                printf("\n============================================================================\n");
                printf("FFTW2MKL FATAL ERROR: MKL TT initialization has failed with status=%d\n",(int)ir);
                printf("Please refer to the Trigonometric Transform Routines Section of MKL Manual\n");
                printf("to find what went wrong...\n");
                printf("============================================================================\n");
                return NULL;
            }
            ipar[10] = 1;
            ipar[14] = n_1;

            d_commit_trig_transform(out,&handle,ipar,dpar,&ir);
            if (ir != 0)
            {
                printf("\n============================================================================\n");
                printf("FFTW2MKL FATAL ERROR: MKL TT commit step has failed with status=%d\n",(int)ir);
                printf("Please refer to the Trigonometric Transform Routines Section of MKL Manual\n");
                printf("to find what went wrong...\n");
                printf("============================================================================\n");
                return NULL;
            }
            fftw_desc_mkl->mkl_desc = handle;
            fftw_desc_mkl->in = (void*)in;
            fftw_desc_mkl->out = (void*)out;
            fftw_desc_mkl->sign = FFTW_BACKWARD;

            tt_desc_mkl->ipar = (void*)ipar;
            tt_desc_mkl->dpar = (void*)dpar;
            fftw_desc_mkl->tt_fftw_mkl = tt_desc_mkl;

            return (fftw_plan)fftw_desc_mkl;
        }
        /*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/
    case FFTW_REDFT01:
        {
            tt_type = 2;

            n_1 = nn;
            dpar=(double*)malloc((3*n_1/2+2)*sizeof(double));
            ipar=(int*)malloc((128)*sizeof(int));
            d_init_trig_transform(&n_1,&tt_type,ipar,dpar,&ir);
            if (ir != 0)
            {
                printf("\n============================================================================\n");
                printf("FFTW2MKL FATAL ERROR: MKL TT initialization has failed with status=%d\n",(int)ir);
                printf("Please refer to the Trigonometric Transform Routines Section of MKL Manual\n");
                printf("to find what went wrong...\n");
                printf("============================================================================\n");
                return NULL;
            }
            ipar[10] = 1;
            ipar[14] = n_1;

            d_commit_trig_transform(out,&handle,ipar,dpar,&ir);
            if (ir != 0)
            {
                printf("\n============================================================================\n");
                printf("FFTW2MKL FATAL ERROR: MKL TT commit step has failed with status=%d\n",(int)ir);
                printf("Please refer to the Trigonometric Transform Routines Section of MKL Manual\n");
                printf("to find what went wrong...\n");
                printf("============================================================================\n");
                return NULL;
            }
            fftw_desc_mkl->mkl_desc = handle;
            fftw_desc_mkl->in = (void*)in;
            fftw_desc_mkl->out = (void*)out;
            fftw_desc_mkl->sign = FFTW_FORWARD;

            tt_desc_mkl->ipar = (void*)ipar;
            tt_desc_mkl->dpar = (void*)dpar;
            fftw_desc_mkl->tt_fftw_mkl = tt_desc_mkl;

            return (fftw_plan)fftw_desc_mkl;
        }
        /*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/
    case FFTW_RODFT11:
        {
            tt_type = 5;

            n_1 = nn;
            dpar=(double*)malloc((5*n_1/2+2)*sizeof(double));
            ipar=(int*)malloc((128)*sizeof(int));
            d_init_trig_transform(&n_1,&tt_type,ipar,dpar,&ir);
            if (ir != 0)
            {
                printf("\n============================================================================\n");
                printf("FFTW2MKL FATAL ERROR: MKL TT initialization has failed with status=%d\n",(int)ir);
                printf("Please refer to the Trigonometric Transform Routines Section of MKL Manual\n");
                printf("to find what went wrong...\n");
                printf("============================================================================\n");
                return NULL;
            }
            ipar[10] = 1;
            ipar[14] = n_1;

            d_commit_trig_transform(out,&handle,ipar,dpar,&ir);
            if (ir != 0)
            {
                printf("\n============================================================================\n");
                printf("FFTW2MKL FATAL ERROR: MKL TT commit step has failed with status=%d\n",(int)ir);
                printf("Please refer to the Trigonometric Transform Routines Section of MKL Manual\n");
                printf("to find what went wrong...\n");
                printf("============================================================================\n");
                return NULL;
            }
            fftw_desc_mkl->mkl_desc = handle;
            fftw_desc_mkl->in = (void*)in;
            fftw_desc_mkl->out = (void*)out;
            fftw_desc_mkl->sign = FFTW_BACKWARD;

            tt_desc_mkl->ipar = (void*)ipar;
            tt_desc_mkl->dpar = (void*)dpar;
            fftw_desc_mkl->tt_fftw_mkl = tt_desc_mkl;

            return (fftw_plan)fftw_desc_mkl;
        }

        /*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/
    case FFTW_RODFT01:
        {
            tt_type = 3;

            n_1 = nn;
            dpar=(double*)malloc((5*n_1/2+2)*sizeof(double));
            ipar=(int*)malloc((128)*sizeof(int));
            d_init_trig_transform(&n_1,&tt_type,ipar,dpar,&ir);
            if (ir != 0)
            {
                printf("\n============================================================================\n");
                printf("FFTW2MKL FATAL ERROR: MKL TT initialization has failed with status=%d\n",(int)ir);
                printf("Please refer to the Trigonometric Transform Routines Section of MKL Manual\n");
                printf("to find what went wrong...\n");
                printf("============================================================================\n");
                return NULL;
            }
            ipar[10] = 1;
            ipar[14] = n_1;

            d_commit_trig_transform(out,&handle,ipar,dpar,&ir);
            if (ir != 0)
            {
                printf("\n============================================================================\n");
                printf("FFTW2MKL FATAL ERROR: MKL TT commit step has failed with status=%d\n",(int)ir);
                printf("Please refer to the Trigonometric Transform Routines Section of MKL Manual\n");
                printf("to find what went wrong...\n");
                printf("============================================================================\n");
                return NULL;
            }
            fftw_desc_mkl->mkl_desc = handle;
            fftw_desc_mkl->in = (void*)in;
            fftw_desc_mkl->out = (void*)out;
            fftw_desc_mkl->sign = FFTW_FORWARD;

            tt_desc_mkl->ipar = (void*)ipar;
            tt_desc_mkl->dpar = (void*)dpar;
            fftw_desc_mkl->tt_fftw_mkl = tt_desc_mkl;

            return (fftw_plan)fftw_desc_mkl;
        }
        /*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/
    case FFTW_RODFT10:
        {
            tt_type = 3;

            n_1 = nn;
            dpar=(double*)malloc((5*n_1/2+2)*sizeof(double));
            ipar=(int*)malloc((128)*sizeof(int));
            d_init_trig_transform(&n_1,&tt_type,ipar,dpar,&ir);
            if (ir != 0)
            {
                printf("\n============================================================================\n");
                printf("FFTW2MKL FATAL ERROR: MKL TT initialization has failed with status=%d\n",(int)ir);
                printf("Please refer to the Trigonometric Transform Routines Section of MKL Manual\n");
                printf("to find what went wrong...\n");
                printf("============================================================================\n");;
                return NULL;
            }
            ipar[10] = 1;
            ipar[14] = n_1;

            d_commit_trig_transform(out,&handle,ipar,dpar,&ir);
            if (ir != 0)
            {
                printf("\n============================================================================\n");
                printf("FFTW2MKL FATAL ERROR: MKL TT commit step has failed with status=%d\n",(int)ir);
                printf("Please refer to the Trigonometric Transform Routines Section of MKL Manual\n");
                printf("to find what went wrong...\n");
                printf("============================================================================\n");
                return NULL;
            }
            fftw_desc_mkl->mkl_desc = handle;
            fftw_desc_mkl->in = (void*)in;
            fftw_desc_mkl->out = (void*)out;
            fftw_desc_mkl->sign = FFTW_BACKWARD;

            tt_desc_mkl->ipar = (void*)ipar;
            tt_desc_mkl->dpar = (void*)dpar;
            fftw_desc_mkl->tt_fftw_mkl = tt_desc_mkl;

            return (fftw_plan)fftw_desc_mkl;
        }

        /*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/
    case MKL_RODFT00:
        {
            tt_type = 0;

            n_1 = nn + 1 ;
            dpar=(double*)malloc((3*n_1/2+2)*sizeof(double));
            ipar=(int*)malloc((128)*sizeof(int));
            d_init_trig_transform(&n_1,&tt_type,ipar,dpar,&ir);
            if (ir != 0)
            {
                printf("\n============================================================================\n");
                printf("FFTW2MKL FATAL ERROR: MKL TT initialization has failed with status=%d\n",(int)ir);
                printf("Please refer to the Trigonometric Transform Routines Section of MKL Manual\n");
                printf("to find what went wrong...\n");
                printf("============================================================================\n");
                return NULL;
            }
            ipar[10] = 1;
            ipar[14] = n_1;
            ipar[15] = 1;
            d_commit_trig_transform(out,&handle,ipar,dpar,&ir);
            if (ir != 0)
            {
                printf("\n============================================================================\n");
                printf("FFTW2MKL FATAL ERROR: MKL TT commit step has failed with status=%d\n",(int)ir);
                printf("Please refer to the Trigonometric Transform Routines Section of MKL Manual\n");
                printf("to find what went wrong...\n");
                printf("============================================================================\n");
                return NULL;
            }
            fftw_desc_mkl->mkl_desc = handle;
            fftw_desc_mkl->in = (void*)in;
            fftw_desc_mkl->out = (void*)out;
            fftw_desc_mkl->sign = FFTW_BACKWARD;

            tt_desc_mkl->ipar = (void*)ipar;
            tt_desc_mkl->dpar = (void*)dpar;
            fftw_desc_mkl->tt_fftw_mkl = tt_desc_mkl;

            return (fftw_plan)fftw_desc_mkl;
        }

        /*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/
    default:
        printf("\n============================================================================\n");
        printf("FFTW2MKL FATAL ERROR: Wrong flag denoting the type of the r2r transform.\n");
        printf("Please use one of FFTW_R[E,O]DFT{0,1][0,1] or MKL_RODFT00...\n");
        printf("============================================================================\n");
        return NULL;
    }
}
