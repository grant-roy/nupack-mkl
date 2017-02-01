*     DORMQR (F08AEF) Example Program Text
*     Mark 16 Release. NAG Copyright 1992.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          MMAX, NMAX, LDA, LDB, NRHMAX, LWORK
      PARAMETER        (MMAX=8,NMAX=8,LDA=MMAX,LDB=MMAX,NRHMAX=NMAX,
     +                 LWORK=64*NMAX)
      DOUBLE PRECISION ONE
      PARAMETER        (ONE=1.0D0)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, INFO, J, M, N, NRHS
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), B(LDB,NRHMAX), TAU(NMAX),
     +                 WORK(LWORK)
*     .. External Subroutines ..
      EXTERNAL         DGEQRF, DORMQR, DTRSM, X04CAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'DORMQR Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) M, N, NRHS
      IF (M.LE.MMAX .AND. N.LE.NMAX .AND. M.GE.N .AND. NRHS.LE.NRHMAX)
     +    THEN
*
*        Read A and B from data file
*
         READ (NIN,*) ((A(I,J),J=1,N),I=1,M)
         READ (NIN,*) ((B(I,J),J=1,NRHS),I=1,M)
*
*        Compute the QR factorization of A
*
         CALL DGEQRF(M,N,A,LDA,TAU,WORK,LWORK,INFO)
*
*        Compute C = (Q**T)*B, storing the result in B
*
         CALL DORMQR('Left','Transpose',M,NRHS,N,A,LDA,TAU,B,LDB,WORK,
     +               LWORK,INFO)
*
*        Compute least-squares solution by backsubstitution in R*X = C
*
         CALL DTRSM('Left','Upper','No transpose','Non-Unit',N,NRHS,ONE,
     +              A,LDA,B,LDB)
*
*        Print least-squares solution(s)
*
         WRITE (NOUT,*)
         IFAIL = 0
*
         CALL X04CAF('General',' ',N,NRHS,B,LDB,
     +               'Least-squares solution(s)',IFAIL)
*
      END IF
      STOP
      END
