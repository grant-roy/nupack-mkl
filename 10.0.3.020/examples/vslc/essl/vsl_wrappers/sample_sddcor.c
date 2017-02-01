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

void sddcor(
    float h[], int inch,
    float x[], int incx,
    float y[], int incy,
    int nh, int nx, int iy0, int ny, int id)
{
    int status, error;
    VSLCorrTaskPtr task, task_ptr=&task;

    vslsCorrNewTask1D(task_ptr,VSL_CORR_MODE_DIRECT,nh,nx,ny);
    vslCorrSetStart(task,&iy0);
    vslCorrSetDecimation(task,&id);
    vslCorrSetInternalPrecision(task,VSL_CORR_PRECISION_DOUBLE);
    status = vslsCorrExec1D(task,h,inch,x,incx,y,incy);

    error = vslCorrDeleteTask(task_ptr);

    if (status != VSL_STATUS_OK) {
        printf("ERROR: sddcor(): bad status=%d\n",status);
        exit(1);
    }
    if (error != 0) {
        printf("ERROR: sddcor(): failed to destroy the task descriptor\n");
        exit(1);
    }
}
