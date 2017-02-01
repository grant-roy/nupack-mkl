*     DGELQF (F08AHF) Example Program Text
*     Mark 16 Release. NAG Copyright 1992.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          MMAX, NMAX, LDA, LDB, NRHMAX, LWORK
      PARAMETER        (MMAX=8,NMAX=8,LDA=MMAX,LDB=NMAX,NRHMAX=NMAX,
     +                 LWORK=64*NMAX)
      DOUBLE PRECISION ZERO, ONE
      PARAMETER        (ZERO=0.0D0,ONE=1.0D0)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, INFO, J, M, N, NRHS
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), B(LDB,NRHMAX), TAU(NMAX),
     +                 WORK(LWORK)
*     .. External Subroutines ..
      EXTERNAL         DGELQF, DORMLQ, DTRSM, F06QHF, X04CAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'DGELQF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) M, N, NRHS
      IF (M.LE.MMAX .AND. N.LE.NMAX .AND. M.LE.N .AND. NRHS.LE.NRHMAX)
     +    THEN
*
*        Read A and B from data file
*
         READ (NIN,*) ((A(I,J),J=1,N),I=1,M)
         READ (NIN,*) ((B(I,J),J=1,NRHS),I=1,M)
*
*        Compute the LQ factorization of A
*
         CALL DGELQF(M,N,A,LDA,TAU,WORK,LWORK,INFO)
*
*        Solve L*Y = B, storing the result in B
*
         CALL DTRSM('Left','Lower','No transpose','Non-Unit',M,NRHS,ONE,
     +              A,LDA,B,LDB)
*
*        Set rows (M+1) to N of B to zero
*
         IF (M.LT.N) CALL F06QHF('General',N-M,NRHS,ZERO,ZERO,B(M+1,1),
     +                           LDB)
*
*        Compute minimum-norm solution X = (Q**T)*B in B
*
         CALL DORMLQ('Left','Transpose',N,NRHS,M,A,LDA,TAU,B,LDB,WORK,
     +               LWORK,INFO)
*
*        Print minimum-norm solution(s)
*
         WRITE (NOUT,*)
         IFAIL = 0
*
         CALL X04CAF('General',' ',N,NRHS,B,LDB,
     +               'Minimum-norm solution(s)',IFAIL)
*
      END IF
      STOP
      END
