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
!   function    : fftw_execute - interface FFTW execute function
!   Content     : wrapper from this function to MKL DFTI/TT computation functions
!                 for double precision complex or real data
!   input data  : pointer to FFTW plan
!*******************************************************************************
*/

#include "fftw3.h"
#include "fftw3_mkl.h"
#include "mkl_trig_transforms.h"
extern void tt_fftw_norm(double in[], double out[], 
                  DFTI_DESCRIPTOR_HANDLE handle,
                  MKL_INT ipar[], double dpar[], MKL_INT *stat);
extern void tt_fftw_in_to_out(double *in, double *out,MKL_INT n);

void fftw_execute(const fftw_plan plan)
{
    MKL_INT ir;
    fftw_plan_mkl*  fftw_desc_mkl;
    DFTI_DESCRIPTOR_HANDLE handle = 0;
    
    fftw_desc_mkl = (fftw_plan_mkl*)plan;
    
    if ((fftw_desc_mkl->tt_fftw_mkl)==(NULL))
    {
        if (fftw_desc_mkl->sign == FFTW_FORWARD)
        {
            if (fftw_desc_mkl->in == fftw_desc_mkl->out)
            {
                DftiComputeForward( fftw_desc_mkl->mkl_desc, fftw_desc_mkl->in);
            }
            else
            {
                DftiComputeForward( fftw_desc_mkl->mkl_desc, fftw_desc_mkl->in,
                                    fftw_desc_mkl->out);
            }
        }
        else
        {
            if (fftw_desc_mkl->in == fftw_desc_mkl->out)
            {
                DftiComputeBackward( fftw_desc_mkl->mkl_desc, fftw_desc_mkl->in);
            }
            else
            {
                DftiComputeBackward( fftw_desc_mkl->mkl_desc, fftw_desc_mkl->in,
                                     fftw_desc_mkl->out);
            }
        }
    }
    else
    {
        handle = fftw_desc_mkl->mkl_desc;
        
        if (fftw_desc_mkl->sign == FFTW_BACKWARD)
        {
            if ((((fftw_desc_mkl->tt_fftw_mkl)->ipar[5])==0)&(((fftw_desc_mkl->tt_fftw_mkl)->ipar[15])==0))
            {
                tt_fftw_norm((double*)fftw_desc_mkl->in,(double*)fftw_desc_mkl->out,handle,((fftw_desc_mkl->tt_fftw_mkl)->ipar),((fftw_desc_mkl->tt_fftw_mkl)->dpar),&ir);
                if (ir)
                {
                    printf("\n============================================================================\n");
                    printf("FFTW2MKL FATAL ERROR: MKL TT backward transform has failed with status=%d\n",(int)ir);
                    printf("Please refer to the Trigonometric Transform Routines Section of MKL Manual\n");
                    printf("to find what went wrong...\n");
                    printf("============================================================================\n");
                    return;
                }
            }
            else
            {
                if (fftw_desc_mkl->in != fftw_desc_mkl->out)
                {
                    tt_fftw_in_to_out((double*)fftw_desc_mkl->in,(double*)fftw_desc_mkl->out,((fftw_desc_mkl->tt_fftw_mkl)->ipar[14]));
                }
                
                d_backward_trig_transform((double*)fftw_desc_mkl->out,&handle,((fftw_desc_mkl->tt_fftw_mkl)->ipar),((fftw_desc_mkl->tt_fftw_mkl)->dpar),&ir);
                if (ir)
                {
                    printf("\n============================================================================\n");
                    printf("FFTW2MKL FATAL ERROR: MKL TT backward transform has failed with status=%d\n",(int)ir);
                    printf("Please refer to the Trigonometric Transform Routines Section of MKL Manual\n");
                    printf("to find what went wrong...\n");
                    printf("============================================================================\n");
                    return;
                }
            }
        }
        else
        {
            if (fftw_desc_mkl->in != fftw_desc_mkl->out)
            {
                tt_fftw_in_to_out((double*)fftw_desc_mkl->in,(double*)fftw_desc_mkl->out,((fftw_desc_mkl->tt_fftw_mkl)->ipar[14]));
            }
            
            d_forward_trig_transform((double*)fftw_desc_mkl->out,&handle,((fftw_desc_mkl->tt_fftw_mkl)->ipar),((fftw_desc_mkl->tt_fftw_mkl)->dpar),&ir);
            if (ir)
            {
                printf("\n============================================================================\n");
                printf("FFTW2MKL FATAL ERROR: MKL TT forward transform has failed with status=%d\n",(int)ir);
                printf("Please refer to the Trigonometric Transform Routines Section of MKL Manual\n");
                printf("to find what went wrong...\n");
                printf("============================================================================\n");
                return;
                
            }
        }
    }
    return;
}
