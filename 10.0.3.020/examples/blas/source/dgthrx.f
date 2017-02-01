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
!      D G T H R  Example Program Text
!*******************************************************************************

      program DGTHR_MAIN
*
      integer          nmax, ymax
      parameter        (nmax=10, ymax=10)
      integer          n
      integer          indx(nmax)
      double precision x(nmax), y(ymax)

      integer          i, indmax

*       External Subroutines
      external         DGTHR, MaxValue, PrintVectorD
*
*       Executable Statements
*
      print*
      print*,'   D G T H R  EXAMPLE PROGRAM'
*
*       Read input data from input file
      read*
      read*, n
      if (n.gt.nmax) then
        print*, ' Insufficient memory for arrays'
        goto 99
      end if
      read*, (indx(i),i=1,n)
      indmax = MaxValue(n,indx)
      if (indmax.gt.ymax) then
        print*, ' Insufficient memory for arrays'
        goto 99
      end if
      read*, (y(i),i=1,indmax)
*
*       Print input data
      print*
      print*, '     INPUT DATA'
      print 100, n
      print 101, (indx(i),i=1,n)
      call PrintVectorD(1,indmax,y,1,'Y ')
*
*        Call DGTHR subroutine
      call  DGTHR(n,y,x,indx)
*
      print*
      print*, '     OUTPUT DATA'
      call PrintVectorD(1,n,x,1,'X ')

  99  continue
 100  format(7x,'N=',i2)
 101  format(7x,'VECTOR INDX'/10x,10(i2,1x))
      stop
      end
