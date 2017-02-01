*     ZGETRI (F07AWF) Example Program Text
*     Mark 15 Release. NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, LDA, LWORK
      PARAMETER        (NMAX=8,LDA=NMAX,LWORK=64*NMAX)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, INFO, J, N
*     .. Local Arrays ..
      COMPLEX*16       A(LDA,NMAX), WORK(LWORK)
      INTEGER          IPIV(NMAX)
      CHARACTER        CLABS(1), RLABS(1)
*     .. External Subroutines ..
      EXTERNAL         ZGETRF, ZGETRI, X04DBF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'ZGETRI Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N
      IF (N.LE.NMAX) THEN
*
*        Read A from data file
*
         READ (NIN,*) ((A(I,J),J=1,N),I=1,N)
*
*        Factorize A
*
         CALL ZGETRF(N,N,A,LDA,IPIV,INFO)
*
         WRITE (NOUT,*)
         IF (INFO.EQ.0) THEN
*
*           Compute inverse of A
*
            CALL ZGETRI(N,A,LDA,IPIV,WORK,LWORK,INFO)
*
*           Print inverse
*
            IFAIL = 0
            CALL X04DBF('General',' ',N,N,A,LDA,'Bracketed','F7.4',
     +                  'Inverse','Integer',RLABS,'Integer',CLABS,80,0,
     +                  IFAIL)
         ELSE
            WRITE (NOUT,*) 'The factor U is singular'
         END IF
      END IF
      STOP
*
      END
