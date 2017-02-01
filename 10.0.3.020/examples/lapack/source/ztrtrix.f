*     ZTRTRI (F07TWF) Example Program Text
*     Mark 16 Release. NAG Copyright 1993.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, LDA
      PARAMETER        (NMAX=8,LDA=NMAX)
      CHARACTER        DIAG
      PARAMETER        (DIAG='N')
*     .. Local Scalars ..
      INTEGER          I, IFAIL, INFO, J, N
      CHARACTER        UPLO
*     .. Local Arrays ..
      COMPLEX*16       A(LDA,NMAX)
      CHARACTER        CLABS(1), RLABS(1)
*     .. External Subroutines ..
      EXTERNAL         X04DBF, ZTRTRI
*     .. Executable Statements ..
      WRITE (NOUT,*) 'ZTRTRI Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.LE.NMAX) THEN
*
*        Read A from data file
*
         READ (NIN,*) UPLO
         IF (UPLO.EQ.'U') THEN
            READ (NIN,*) ((A(I,J),J=I,N),I=1,N)
         ELSE IF (UPLO.EQ.'L') THEN
            READ (NIN,*) ((A(I,J),J=1,I),I=1,N)
         END IF
*
*        Compute inverse of A
*
         CALL ZTRTRI(UPLO,DIAG,N,A,LDA,INFO)
*
*        Print inverse
*
         WRITE (NOUT,*)
         IFAIL = 0
         CALL X04DBF(UPLO,DIAG,N,N,A,LDA,'Bracketed','F7.4','Inverse',
     +               'Integer',RLABS,'Integer',CLABS,80,0,IFAIL)
      END IF
      STOP
*
      END
