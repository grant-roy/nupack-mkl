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
!      Z R O T G  Example Program Text
!*******************************************************************************

      program  ZROTG_MAIN
*
      double precision  c
      complex*16        s, ca, cb
*       External Subroutines
      external          ZROTG
*
*       Executable Statements
*
      print*
      print*,'   Z R O T G  EXAMPLE PROGRAM'
*
*       Read input data from input file
      read*
      read*, ca, cb
*
*       Print input data
      print*
      print*, '     INPUT DATA'
      print 100, ca, cb
*
*       Call ZROTG subroutine
      call ZROTG(ca,cb,c,s)
*
      print*
      print*, '     OUTPUT DATA'
      print 101, ca
      print 102, c,  s
*
 100  format(7x,'CA=(',f6.3,',',f6.3,')  CB=(',f6.3,',',f6.3,')')
 101  format(7x,'CA=(',f6.3,',',f6.3,')')
 102  format(7x,' C= ',f6.3,11x,'S=(',f6.3,',',f6.3,')')
      stop
      end
