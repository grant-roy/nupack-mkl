*     ZGETRF (F07ARF) Example Program Text
*     Mark 15 Release. NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          MMAX, NMAX, LDA
      PARAMETER        (MMAX=8,NMAX=8,LDA=MMAX)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, INFO, J, M, N
*     .. Local Arrays ..
      COMPLEX*16       A(LDA,NMAX)
      INTEGER          IPIV(NMAX)
      CHARACTER        CLABS(1), RLABS(1)
*     .. External Subroutines ..
      EXTERNAL         ZGETRF, X04DBF
*     .. Intrinsic Functions ..
      INTRINSIC        MIN
*     .. Executable Statements ..
      WRITE (NOUT,*) 'ZGETRF Example Program Results'
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
         CALL ZGETRF(M,N,A,LDA,IPIV,INFO)
*
*        Print details of factorization
*
         WRITE (NOUT,*)
         IFAIL = 0
         CALL X04DBF('General',' ',M,N,A,LDA,'Bracketed','F7.4',
     +               'Details of factorization','Integer',RLABS,
     +               'Integer',CLABS,80,0,IFAIL)
*
*        Print pivot indices
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'IPIV'
         WRITE (NOUT,99999) (IPIV(I),I=1,MIN(M,N))
*
         IF (INFO.NE.0) WRITE (NOUT,*) 'The factor U is singular'
*
      END IF
      STOP
*
99999 FORMAT ((1X,I12,3I18))
      END
