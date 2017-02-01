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
!      I D A M I N  Example Program Text
!*******************************************************************************

      program  IDAMIN_MAIN
*
      integer          n, incx
      integer          xmax
      parameter       (xmax=10)
      double precision x(xmax)
      integer          i, index
*       Intrinsic Functions
      intrinsic        abs
*       External Subroutines
      external         IDAMIN, PrintVectorD
      integer          IDAMIN
*
*       Executable Statements
*
      print*
      print*,'   I D A M I N  EXAMPLE PROGRAM'
*       Read input data from input file
      read*
      read*, n, incx
      if ((1+(n-1)*abs(incx)).gt.xmax) then
          print*, ' Insufficient memory for arrays'
          goto 99
      end if
      read*, (x(i),i=1,1+(n-1)*abs(incx))
*
*       Print input data
      print*
      print*, '     INPUT DATA'
      print 100, n
      call PrintVectorD(0,n,x,incx,'X ')
*
*       Call IDAMIN subroutine
      index = IDAMIN(n,x,incx)
*
      print*
      print*, '     OUTPUT DATA'
      print 101, index

  99  continue
 100  format(7x,'N=',i2)
 101  format(10x,'IDAMIN = ',i3)
      stop
      end
