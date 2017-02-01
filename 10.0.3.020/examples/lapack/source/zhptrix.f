*     ZHPTRI (F07PWF) Example Program Text
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
      COMPLEX*16       AP(NMAX*(NMAX+1)/2), WORK(NMAX)
      INTEGER          IPIV(NMAX)
      CHARACTER        CLABS(1), RLABS(1)
*     .. External Subroutines ..
      EXTERNAL         ZHPTRF, ZHPTRI, X04DDF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'ZHPTRI Example Program Results'
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
         CALL ZHPTRF(UPLO,N,AP,IPIV,INFO)
*
         WRITE (NOUT,*)
         IF (INFO.EQ.0) THEN
*
*           Compute inverse of A
*
            CALL ZHPTRI(UPLO,N,AP,IPIV,WORK,INFO)
*
*           Print inverse
*
            IFAIL = 0
            CALL X04DDF(UPLO,'Nonunit',N,AP,'Bracketed','F7.4',
     +                  'Inverse','Integer',RLABS,'Integer',CLABS,80,0,
     +                  IFAIL)
         ELSE
            WRITE (NOUT,*) 'The factor D is singular'
         END IF
      END IF
      STOP
*
      END
