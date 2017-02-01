!*******************************************************************************
!                              INTEL CONFIDENTIAL
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
!*******************************************************************************

      INCLUDE 'mkl_vsl.fi'

      SUBROUTINE SCONF(init,                                            &
     &                 h,inc1h,x,inc1x,inc2x,y,inc1y,inc2y,             &
     &                 nh,nx,m,iy0,ny,                                  &
     &                 aux1,naux1,aux2,naux2)
      USE MKL_VSL_TYPE
      USE MKL_VSL
      IMPLICIT NONE
!
      INTEGER(KIND=4) init,inc2x,inc2y,m,iy0
      INTEGER inc1x,inc1y,inc1h,nh,nx,ny
      REAL(KIND=4) h(*),x(inc2x,*),y(inc2y,*)
      INTEGER(KIND=4) naux1,naux2
      REAL aux1(*),aux2(*)
!
      TYPE(VSL_CONV_TASK) task
      INTEGER(KIND=4) status,error,i
      INTEGER start(1)
      INTEGER mode
!
!     ignore aux1, aux2:
      IF (init .NE. 0) RETURN
!
      start(1) = iy0
      mode = VSL_CONV_MODE_FFT
      status = vslsconvnewtaskx1d(task,mode,nh,nx,ny,h,inc1h)
      status = vslconvsetstart(task,start)
!
!     task is implicitly committed at i==1
      DO i=1,m
      status = vslsconvexecx1d(task, x(1,i),inc1x, y(1,i),inc1y)
      END DO
!
      error  = vslconvdeletetask(task)
!
      IF (status .NE. VSL_STATUS_OK) THEN
      PRINT *, 'ERROR: sconf(): bad status=',status
      STOP 1
      END IF
!
      IF (error .NE. 0) THEN
      PRINT *, 'ERROR: sconf(): failed to destroy the task descriptor'
      STOP 1
      END IF
!
      RETURN
      END
