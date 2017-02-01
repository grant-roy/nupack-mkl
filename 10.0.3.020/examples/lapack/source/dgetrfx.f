*     DGETRF (F07ADF) Example Program Text
*     Mark 15 Release. NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          MMAX, NMAX, LDA
      PARAMETER        (MMAX=8,NMAX=8,LDA=MMAX)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, INFO, J, M, N
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX)
      INTEGER          IPIV(NMAX)
*     .. External Subroutines ..
      EXTERNAL         DGETRF
*     .. Intrinsic Functions ..
      INTRINSIC        MIN
*     .. Executable Statements ..
      WRITE (NOUT,*) 'DGETRF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) M, N
      IF (M.LE.MMAX .AND. N.LE.NMAX) THEN
*
*        Read A from data file
*
         READ (NIN,*) ((A(I,J),J=1,N),I=1,M)
*
*        Factorize A
*
         CALL DGETRF(M,N,A,LDA,IPIV,INFO)
*
*        Print details of factorization
*
         WRITE (NOUT,*)
         IFAIL = 0
         CALL X04CAF('General',' ',M,N,A,LDA,'Details of factorization',
     +               IFAIL)

*        Print pivot indices

         WRITE (NOUT,*)
         WRITE (NOUT,*) 'IPIV'
         WRITE (NOUT,99999) (IPIV(I),I=1,MIN(M,N))
*
         IF (INFO.NE.0) WRITE (NOUT,*) 'The factor U is singular'
*
      END IF
      STOP
*
99999 FORMAT ((3X,7I11))
      END
