!*******************************************************************************
!                              INTEL CONFIDENTIAL
!   Copyright(C) 1999-2008 Intel Corporation. All Rights Reserved.
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
!  Content:
!      S D O T  Example Program Text
!*******************************************************************************

      program  SDOT_MAIN
*
      integer    n, incx, incy
      integer    xmax, ymax
      parameter  (xmax=20, ymax=20)
      real       sx(xmax), sy(ymax), res
      integer    i
*       Intrinsic Functions
      intrinsic  abs
*       External Subroutines
      external   SDOT, PrintVectorS
      real       SDOT
*
*       Executable Statements
*
      print*
      print*,'   S D O T  EXAMPLE PROGRAM'
*
*       Read input data from input file
      read*
      read*, n
      read*, incx, incy
      if ( ((1+(n-1)*abs(incx)).gt.xmax).or.
     $     ((1+(n-1)*abs(incy)).gt.ymax) ) then
        print*, ' Insufficient memory for arrays'
        goto 99
      end if
      read*, (sx(i),i=1,1+(n-1)*abs(incx))
      read*, (sy(i),i=1,1+(n-1)*abs(incy))
*
*       Print input data
      print*
      print*, '     INPUT DATA'
      print 100, n
      call PrintVectorS(0,n,sx,incx,'X ')
      call PrintVectorS(0,n,sy,incy,'Y ')
*
*       Call SDOT subroutine
      res = SDOT(n,sx,incx,sy,incy)
*
      print*
      print*, '     OUTPUT DATA'
      print 101, res

  99  continue
 100  format(7x,'N=',i2)
 101  format(7x,'SDOT = ',f6.3)
      stop
      end
