*     CHSEQR (F08PSE) Example Program Text
*     Mark 16 Release. NAG Copyright 1992.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, LDH, LWORK, LDZ
      PARAMETER        (NMAX=8,LDH=NMAX,LWORK=NMAX,LDZ=NMAX)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, INFO, J, N
*     .. Local Arrays ..
      COMPLEX          H(LDH,NMAX), W(NMAX), WORK(LWORK), Z(LDZ,NMAX)
      CHARACTER        CLABS(1), RLABS(1)
*     .. External Subroutines ..
      EXTERNAL         CHSEQR, X04DBE
*     .. Intrinsic Functions ..
      INTRINSIC        AIMAG, REAL
*     .. Executable Statements ..
      WRITE (NOUT,*) 'CHSEQR Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.LE.NMAX) THEN
*
*        Read H from data file
*
         READ (NIN,*) ((H(I,J),J=1,N),I=1,N)
*
*        Calculate the eigenvalues and Schur factorization of H
*
         CALL CHSEQR('Schur form','Initialize Z',N,1,N,H,LDH,W,Z,LDZ,
     +               WORK,LWORK,INFO)
*
         WRITE (NOUT,*)
         IF (INFO.GT.0) THEN
            WRITE (NOUT,*) 'Failure to converge.'
         ELSE
            WRITE (NOUT,*) 'Eigenvalues'
*            WRITE (NOUT,99999) (' (',REAL(W(I)),',',AIMAG(W(I)),')',
*     +        I=1,N)
            WRITE (NOUT,99999) 
     *            (' (',REAL(W(I)),',',AIMAG(W(I)),')', I=1,N)
*
*           Print Schur form
*
            WRITE (NOUT,*)
            IFAIL = 0
*
            CALL X04DBE('General',' ',N,N,H,LDH,'Bracketed','F7.4',
     +                  'Schur form','Integer',RLABS,'Integer',CLABS,80,
     +                  0,IFAIL)
*
*           Print Schur vectors
*
            WRITE (NOUT,*)
            IFAIL = 0
*
            CALL X04DBE('General',' ',N,N,Z,LDZ,'Bracketed','F7.4',
     +                  'Schur vectors of H','Integer',RLABS,'Integer',
     +                  CLABS,80,0,IFAIL)
*
         END IF
      END IF
      STOP
*
99999 FORMAT ((3X,4(A,F7.4,A,F7.4,A,:)))
      END
