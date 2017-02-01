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
!      S R O T M G  Example Program Text
!*******************************************************************************

      program  SROTMG_MAIN
*
      real       param(5)
      real       sd1, sd2, sx1, sy1
*       External Subroutines
      external   SROTMG
*
*       Executable Statements
*
      print*
      print*,'   S R O T M G  EXAMPLE PROGRAM'
*       Read input data from input file
      read*
      read*, sd1, sd2, sx1, sy1
*
*       Print input data
      print*
      print*, '     INPUT DATA'
      print 100, sd1, sd2, sx1, sy1
*
*       Call SROTMG subroutine
      call SROTMG(sd1,sd2,sx1,sy1,param)
*
      print*
      print*, '     OUTPUT DATA'
      print 101, sd1, sd2, sx1
      print 102, (param(i), i=1,5)
*
 100  format(7x,'SD1=',f4.1,'  SD2=',f4.1,'  SX1=',f4.1,
     $       '  SY1=',f4.1)
 101  format(7x,'SD1=',f5.2,'  SD2=',f5.2,'  SX1=',f5.2)
 102  format(7x,'VECTOR PARAM'/9x,5(f6.2,1x))
      stop
      end
