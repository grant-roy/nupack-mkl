/*******************************************************************************
!                              INTEL CONFIDENTIAL
!   Copyright(C) 2003-2008 Intel Corporation. All Rights Reserved.
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
!   Content:
!       MKL DFTI example program (C interface)
!*******************************************************************************
*/

#include <stdio.h>
#include "mkl_dfti.h"

void dfti_example_status_print(MKL_LONG status)
{
    MKL_LONG class_error;
    char*   error_message;

    class_error = DftiErrorClass(status, DFTI_ERROR_CLASS);
    if (! class_error) {
        printf
             ("Error Status is not a member of Predefined Error Class\n");
    } else {
        error_message = DftiErrorMessage(status);
        printf("error_message = %s \n", error_message);
    }/* if (! class_error) */
    return;
}/* dfti_status_print */
