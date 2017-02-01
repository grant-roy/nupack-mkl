!*******************************************************************************
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
!    vsldConvExec  Example Program Text
!*******************************************************************************

      INCLUDE 'mkl_vsl.fi'

      PROGRAM MKL_VSL_TEST
      USE MKL_VSL_TYPE
      USE MKL_VSL
      IMPLICIT NONE
!
      TYPE(VSL_CONV_TASK) task
      INTEGER(KIND=4) status,i
      INTEGER mode,rank
      INTEGER xshape(1),yshape(1),zshape(1)
      INTEGER xstride(1),ystride(1),zstride(1)
      REAL(KIND=8) x(100),y(1000),z(1099)
      LOGICAL ok
!
      DATA rank/1/
      DATA xshape/100/, yshape/1000/, zshape/1099/
      DATA xstride/1/, ystride/1/, zstride/1/
      DATA x/100*0D0/, y/1000*0D0/
!
      ok = .TRUE.
      PRINT *, 'EXAMPLE executing a convolution task'
!
      mode = VSL_CONV_MODE_AUTO

      status = vsldconvnewtask(task,mode,rank,xshape,yshape,zshape)
!
      status = vsldconvexec(task,x,xstride,y,ystride,z,zstride)
!
      IF (status .NE. VSL_STATUS_OK) THEN
         PRINT *, 'ERROR: bad status: ',status
         PRINT *, 'EXAMPLE FAILED'
         ok = .FALSE.
      END IF
!
      DO i=1,1099
         IF (z(i) .NE. 0) THEN
            PRINT *, 'ERROR: wrong result: i=',i,', z(i)=',z(i)
            ok = .FALSE.
         END IF
      END DO
!
      IF (ok) THEN
         PRINT *, 'EXAMPLE PASSED'
         STOP
      ELSE
         PRINT *, 'EXAMPLE FAILED'
         STOP 1
      END IF
!
      END
