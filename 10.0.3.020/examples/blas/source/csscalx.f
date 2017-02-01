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
!      C S S C A L  Example Program Text
!*******************************************************************************

      program CSSCAL_MAIN
*
      integer      n, incx
      integer      nmax
      parameter    (nmax=10)
      complex      cx(nmax)
      real         sa
      integer      i
*       External Subroutines
      external     CSSCAL, PrintVectorC
*
*   Executable Statementcs
*
      print*
      print*,'   C S S C A L  EXAMPLE PROGRAM'
*
*       Read input data from input file
      read*
      read*, n, incx
      read*, sa
      if ( (1+(n-1)*abs(incx)).gt.nmax) then
        print*, ' Insufficient memory for arrays'
        goto 99
      end if
      read*, (cx(i),i=1,1+(n-1)*abs(incx))
*
*       Print input data
      print*
      print*, '     INPUT DATA'
      print 100, n
      print 101, sa
      call PrintVectorC(0,n,cx,incx,'X ')
*
*      Call CSSCAL subroutine
      call CSSCAL(n,sa,cx,incx)
*
      print*
      print*, '     OUTPUT DATA'
      call PrintVectorC(1,n,cx,incx,'X ')

  99  continue
 100  format(7x,'N=',i2)
 101  format(7x,'SA=',f5.2)
      stop
      end
