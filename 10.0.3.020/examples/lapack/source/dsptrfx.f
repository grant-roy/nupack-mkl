*     DSPTRF (F07PDF) Example Program Text
*     Mark 15 Release. NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX
      PARAMETER        (NMAX=8)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, INFO, J, N
      CHARACTER        UPLO
*     .. Local Arrays ..
      DOUBLE PRECISION AP(NMAX*(NMAX+1)/2)
      INTEGER          IPIV(NMAX)
*     .. External Subroutines ..
      EXTERNAL         DSPTRF, X04CCF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'DSPTRF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.LE.NMAX) THEN
*
*        Read A from data file
*
         READ (NIN,*) UPLO
         IF (UPLO.EQ.'U') THEN
            READ (NIN,*) ((AP(I+J*(J-1)/2),J=I,N),I=1,N)
         ELSE IF (UPLO.EQ.'L') THEN
            READ (NIN,*) ((AP(I+(2*N-J)*(J-1)/2),J=1,I),I=1,N)
         END IF
*
*        Factorize A
*
         CALL DSPTRF(UPLO,N,AP,IPIV,INFO)
*
         WRITE (NOUT,*)
*
*        Print details of factorization
*
         IFAIL = 0
*
         CALL X04CCF(UPLO,'Nonunit',N,AP,'Details of factorization',
     +               IFAIL)
*
*        Print pivot indices
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'IPIV'
         WRITE (NOUT,99999) (IPIV(I),I=1,N)
*
         IF (INFO.NE.0) WRITE (NOUT,*) 'The factor D is singular'
*
      END IF
      STOP
*
99999 FORMAT ((3X,7I11))
      END
