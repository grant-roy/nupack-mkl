/*******************************************************************************
!                             INTEL CONFIDENTIAL
!   Copyright(C) 2006-2008 Intel Corporation. All Rights Reserved.
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

#include "mkl_vsl.h"

#include <stdio.h>
#include <stdlib.h>

void scorf(
    int init,
    float h[], int inc1h,
    float x[], int inc1x, int inc2x,
    float y[], int inc1y, int inc2y,
    int nh, int nx, int m, int iy0, int ny,
    void* aux1, int naux1, void* aux2, int naux2)
{
    int status,error,i;
    VSLCorrTaskPtr task;

    if (init != 0)
        return; /* ignore aux1, aux2 */

    vslsCorrNewTaskX1D(&task,
        VSL_CORR_MODE_FFT,nh,nx,ny,
        h,inc1h);
    vslCorrSetStart(task, &iy0);

    /* task is implicitly committed at i==0 */
    for (i=0; i<m; i++) {
        float* xi = &x[inc2x * i];
        float* yi = &y[inc2y * i];
        status = vslsCorrExecX1D(task,
            xi,inc1x, yi,inc1y);
        /* check status later */
    }

    error = vslCorrDeleteTask(&task);

    if (status != VSL_STATUS_OK) {
        printf("ERROR: scorf(): bad status=%d\n",status);
        exit(1);
    }
    if (error != 0) {
        printf("ERROR: scorf(): failed to destroy the task descriptor\n");
        exit(1);
    }
}
