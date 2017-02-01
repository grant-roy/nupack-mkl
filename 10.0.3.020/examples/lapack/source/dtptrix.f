*     DTPTRI (F07UJF) Example Program Text
*     Mark 15 Release. NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX
      PARAMETER        (NMAX=8)
      CHARACTER        DIAG
      PARAMETER        (DIAG='N')
*     .. Local Scalars ..
      INTEGER          I, IFAIL, INFO, J, N
      CHARACTER        UPLO
*     .. Local Arrays ..
      DOUBLE PRECISION AP(NMAX*(NMAX+1)/2)
*     .. External Subroutines ..
      EXTERNAL         DTPTRI, X04CCF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'DTPTRI Example Program Results'
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
*        Compute inverse of A
*
         CALL DTPTRI(UPLO,DIAG,N,AP,INFO)
*
*        Print inverse
*
         WRITE (NOUT,*)
         IFAIL = 0
         CALL X04CCF(UPLO,DIAG,N,AP,'Inverse',IFAIL)
      END IF
      STOP
*
      END
