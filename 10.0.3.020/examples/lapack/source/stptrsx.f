*     STPTRS (F07UEE) Example Program Text
*     Mark 15 Release. NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, NRHMAX, LDB
      PARAMETER        (NMAX=8,NRHMAX=NMAX,LDB=NMAX)
      CHARACTER        TRANS, DIAG
      PARAMETER        (TRANS='N',DIAG='N')
*     .. Local Scalars ..
      INTEGER          I, IFAIL, INFO, J, N, NRHS
      CHARACTER        UPLO
*     .. Local Arrays ..
      REAL             AP(NMAX*(NMAX+1)/2), B(LDB,NRHMAX)
*     .. External Subroutines ..
      EXTERNAL         STPTRS, X04CAE
*     .. Executable Statements ..
      WRITE (NOUT,*) 'STPTRS Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, NRHS
      IF (N.LE.NMAX .AND. NRHS.LE.NRHMAX) THEN
*
*        Read A and B from data file
*
         READ (NIN,*) UPLO
         IF (UPLO.EQ.'U') THEN
            READ (NIN,*) ((AP(I+J*(J-1)/2),J=I,N),I=1,N)
         ELSE IF (UPLO.EQ.'L') THEN
            READ (NIN,*) ((AP(I+(2*N-J)*(J-1)/2),J=1,I),I=1,N)
         END IF
         READ (NIN,*) ((B(I,J),J=1,NRHS),I=1,N)
*
*        Compute solution
*
         CALL STPTRS(UPLO,TRANS,DIAG,N,NRHS,AP,B,LDB,INFO)
*
*        Print solution
*
         WRITE (NOUT,*)
         IF (INFO.EQ.0) THEN
            IFAIL = 0
            CALL X04CAE('General',' ',N,NRHS,B,LDB,'Solution(s)',IFAIL)
         ELSE
            WRITE (NOUT,*) 'A is singular'
         END IF
      END IF
      STOP
*
      END
