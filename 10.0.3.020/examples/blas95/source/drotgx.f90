!*******************************************************************************
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
!  Content:
!      D R O T G  Example Program Text
!*******************************************************************************

      program  DROTG_MAIN

      use mkl95_precision, only: wp => dp
      use mkl95_blas, only: rotg

      implicit none

      real(wp) :: c, s
      real(wp) :: sa, sb

!     Executable Statements

      print*
      print*,'   D R O T G  EXAMPLE PROGRAM'

!     Read input data from input file
      read*
      read*, sa, sb

!     Print input data
      print*
      print*, '     INPUT DATA'
      print 100, sa, sb

!     Call DROTG subroutine
      call ROTG(sa, sb, c, s)

      print*
      print*, '     OUTPUT DATA'
      print 100, sa, sb
      print 101, c,  s

 100  format(7x,'SA=',f6.3,'  SB=',f6.3)
 101  format(7x,' C=',f6.3,'   S=',f6.3)
      stop
      end
