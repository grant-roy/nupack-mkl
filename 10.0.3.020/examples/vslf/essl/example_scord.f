!*******************************************************************************
!                             INTEL CONFIDENTIAL
!  Copyright(C) 2006-2008 Intel Corporation. All Rights Reserved.
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
!    scord  Example Program Text
!*******************************************************************************

      EXTERNAL   SCORD
      INTEGER  INCH, INCX, INCY
      INTEGER  NH,   NX,   NY
      INTEGER(KIND=4)  IY0
      REAL(KIND=4)     H(3), X(15), Y(24)
      INTEGER(KIND=4)  I
      REAL(KIND=8)     R10
      INTEGER(KIND=4)  R1(6)
      INTEGER(KIND=4)  R2(3)
      DATA       R1 /2, 5, 8, 11, 14, 5 /
      DATA       R2 / 5, 8, 11 /
!************* Initialize data *****/
      R10 = 2.0E-5
      INCH = 2
      INCX = 3
      INCY = 4
      NH   = 2
      NX   = 5
      NY   = 6
      DO I = 1, NH
         H(1+(I-1)*INCH) = I
      END DO
      PRINT *, '   '
      DO I = 1, NX
         X(1+(I-1)*INCX) = I
      END DO

!************* 1-st Sample **********/

      IY0 = -1

!************* Call scond ***********/

      CALL SCORD( H, INCH, X, INCX, Y, INCY, NH, NX, IY0, NY )

!************* Printing results *****/

      PRINT *, ' 1-ST SAMPLE OF SCORD.'
      PRINT *, '----------------------'
      PRINT *, 'PARAMETERS:'
      PRINT 10, '    INCH = ',INCH,'    INCX = ',INCX,'    INCY = ',INCY
      PRINT 10, '    NH   = ',NH,  '    NX   = ',NX,  '    NY   = ',NY
      PRINT 10, '    IY0  = ',IY0
      DO I = 1, NH
         PRINT 11, ' H( ',1+(I-1)*INCH,') = ',H(1+(I-1)*INCH)
      END DO
      PRINT *, ' '
      DO I = 1, NX
         PRINT 11, ' X( ',1+(I-1)*INCX,') = ',X(1+(I-1)*INCX)
      END DO
      PRINT *, ' '
      PRINT *, 'RESULTS:'
      PRINT *, '---------------------------'
      DO I = 1, NY
         PRINT 11, ' Y( ',1+(I-1)*INCY,') = ',Y(1+(I-1)*INCY)
      END DO
      PRINT *, ' '
      DO I = 1, 6
         IF(ABS(Y(1+(I-1)*INCY)-R1(I)) .GT. R10) THEN
         PRINT *, 'ERROR: wrong result: Y(',1+(I-1)*INCY,')'
         PRINT *, '---------------------------'
         PRINT 10, ' TEST FAILED '
         PRINT *, '---------------------------'
         STOP 1
         ENDIF
      END DO
!**************** 2-nd Sample of scord.**********/

      IY0 = 0
      NY  = 3

!************* Call scord *****/

      CALL SCORD( H, INCH, X, INCX, Y, INCY, NH, NX, IY0, NY )

!************* Printing results *****/

      PRINT *, ' 2-ND SAMPLE OF SCORD.'
      PRINT *, '----------------------'
      PRINT *, 'PARAMETERS:'
      PRINT 10, '    INCH = ',INCH,'    INCX = ',INCX,'    INCY = ',INCY
      PRINT 10, '    NH   = ',NH,  '    NX   = ',NX,  '    NY   = ',NY
      PRINT 10, '    IY0  = ',IY0
      DO I = 1, NH
         PRINT 11, ' H( ',1+(I-1)*INCH,') = ',H(1+(I-1)*INCH)
      END DO
      PRINT *, ' '
      DO I = 1, NX
         PRINT 11, ' X( ',1+(I-1)*INCX,') = ',X(1+(I-1)*INCX)
      END DO
      PRINT *, ' '
      PRINT *, 'RESULTS:'
      PRINT *, '---------------------------'
      DO I = 1, NY
         PRINT 11, ' Y( ',1+(I-1)*INCY,') = ',Y(1+(I-1)*INCY)
      END DO
      DO I = 1, 3
         IF(ABS(Y(1+(I-1)*INCY)-R2(I)) .GT. R10) THEN
         PRINT *, 'ERROR: wrong result: Y(',1+(I-1)*INCY,')'
         PRINT *, '---------------------------'
         PRINT 10, ' TEST FAILED '
         PRINT *, '---------------------------'
         STOP 1
         ENDIF
      END DO
      PRINT *, ' '
      PRINT *, '---------------------------'
      PRINT 10, ' TEST PASSED '
      PRINT *, '---------------------------'
10    FORMAT(A,I4,A,I4,A,I4)
11    FORMAT(A,I2,A,F5.2)

      END
