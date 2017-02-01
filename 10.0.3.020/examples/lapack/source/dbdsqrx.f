*     DBDSQR (F08MEF) Example Program Text
*     Mark 16 Release. NAG Copyright 1992.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, LDVT, LDU, LDC
      PARAMETER        (NMAX=8,LDVT=NMAX,LDU=NMAX,LDC=1)
      DOUBLE PRECISION ZERO, ONE
      PARAMETER        (ZERO=0.0D0,ONE=1.0D0)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, INFO, N
      CHARACTER        UPLO
*     .. Local Arrays ..
      DOUBLE PRECISION C(LDC,1), D(NMAX), E(NMAX-1), U(LDU,NMAX),
     +                 VT(LDVT,NMAX), WORK(4*NMAX-4)
*     .. External Subroutines ..
      EXTERNAL         DBDSQR, F06QHF, X04CAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'DBDSQR Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.LE.NMAX) THEN
*
*        Read B from data file
*
         READ (NIN,*) (D(I),I=1,N)
         READ (NIN,*) (E(I),I=1,N-1)
*
         READ (NIN,*) UPLO
*
*        Initialise U and VT to be the unit matrix
*
         CALL F06QHF('General',N,N,ZERO,ONE,U,LDU)
*
         CALL F06QHF('General',N,N,ZERO,ONE,VT,LDVT)
*
*        Calculate the SVD of B
*
         CALL DBDSQR(UPLO,N,N,N,0,D,E,VT,LDVT,U,LDU,C,LDC,WORK,INFO)
*
         WRITE (NOUT,*)
         IF (INFO.GT.0) THEN
            WRITE (NOUT,*) 'Failure to converge.'
         ELSE
*
*           Print singular values, left & right singular vectors
*
            WRITE (NOUT,*) 'Singular values'
            WRITE (NOUT,99999) (D(I),I=1,N)
            WRITE (NOUT,*)
            IFAIL = 0
*
            CALL X04CAF('General',' ',N,N,VT,LDVT,
     +                  'Right singular vectors, by row',IFAIL)
*
            WRITE (NOUT,*)
            IFAIL = 0
*
            CALL X04CAF('General',' ',N,N,U,LDU,
     +                  'Left singular vectors, by column',IFAIL)
*
         END IF
      END IF
      STOP
*
99999 FORMAT (3X,(8F8.4))
      END
