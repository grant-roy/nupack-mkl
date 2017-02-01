*     CTRSYL (F08QVE) Example Program Text
*     Mark 16 Release. NAG Copyright 1992.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          MMAX, NMAX, LDA, LDB, LDC
      PARAMETER        (MMAX=8,NMAX=8,LDA=MMAX,LDB=NMAX,LDC=MMAX)
*     .. Local Scalars ..
      REAL             SCALE
      INTEGER          I, IFAIL, INFO, J, M, N
*     .. Local Arrays ..
      COMPLEX          A(LDA,MMAX), B(LDB,NMAX), C(LDC,NMAX)
      CHARACTER        CLABS(1), RLABS(1)
*     .. External Subroutines ..
      EXTERNAL         CTRSYL, X04DBE
*     .. Executable Statements ..
      WRITE (NOUT,*) 'CTRSYL Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) M, N
      IF (M.LE.MMAX .AND. N.LE.NMAX) THEN
*
*        Read A, B and C from data file
*
         READ (NIN,*) ((A(I,J),J=1,M),I=1,M)
         READ (NIN,*) ((B(I,J),J=1,N),I=1,N)
         READ (NIN,*) ((C(I,J),J=1,N),I=1,M)
*
*        Solve the Sylvester equation A*X + X*B = C for X
*
         CALL CTRSYL('No transpose','No transpose',1,M,N,A,LDA,B,LDB,C,
     +               LDC,SCALE,INFO)
*
*        Print the solution matrix X
*
         WRITE (NOUT,*)
         IFAIL = 0
*
         CALL X04DBE('General',' ',M,N,C,LDC,'Bracketed','F7.4',
     +               'Solution matrix X','Integer',RLABS,'Integer',
     +               CLABS,80,0,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,99999) 'SCALE = ', SCALE
      END IF
      STOP
*
99999 FORMAT (1X,A,1P,E10.2)
      END
