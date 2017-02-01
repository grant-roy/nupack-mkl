*     SGBRFS (F07BHE) Example Program Text
*     Mark 15 Release. NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      REAL             ZERO
      PARAMETER        (ZERO=0.0)
      INTEGER          NMAX, NRHMAX, KLMAX, KUMAX, LDAB, LDAFB, LDB, LDX
      PARAMETER        (NMAX=8,NRHMAX=NMAX,KLMAX=8,KUMAX=8,
     +                 LDAB=KLMAX+KUMAX+1,LDAFB=2*KLMAX+KUMAX+1,
     +                 LDB=NMAX,LDX=NMAX)
      CHARACTER        TRANS
      PARAMETER        (TRANS='N')
*     .. Local Scalars ..
      INTEGER          I, IFAIL, INFO, J, K, KL, KU, N, NRHS
*     .. Local Arrays ..
      REAL             AB(LDAB,NMAX), AFB(LDAFB,NMAX), B(LDB,NRHMAX),
     +                 BERR(NRHMAX), FERR(NRHMAX), WORK(3*NMAX),
     +                 X(LDX,NMAX)
      INTEGER          IPIV(NMAX), IWORK(NMAX)
*     .. External Subroutines ..
      EXTERNAL         F06QFE, F06QHE, SGBRFS, SGBTRF, SGBTRS, X04CAE
*     .. Intrinsic Functions ..
      INTRINSIC        MAX, MIN
*     .. Executable Statements ..
      WRITE (NOUT,*) 'SGBRFS Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, NRHS, KL, KU
      IF (N.LE.NMAX .AND. NRHS.LE.NRHMAX .AND. KL.LE.KLMAX .AND. KU.LE.
     +    KUMAX) THEN
*
*        Set A to zero to avoid referencing uninitialized elements
*
         CALL F06QHE('General',KL+KU+1,N,ZERO,ZERO,AB,LDAB)
*
*        Read A and B from data file, and copy A to AFB and B to X
*
         K = KU + 1
         READ (NIN,*) ((AB(K+I-J,J),J=MAX(I-KL,1),MIN(I+KU,N)),I=1,
     +     N)
         READ (NIN,*) ((B(I,J),J=1,NRHS),I=1,N)
*
         CALL F06QFE('General',KL+KU+1,N,AB,LDAB,AFB(KL+1,1),LDAFB)
*
         CALL F06QFE('General',N,NRHS,B,LDB,X,LDX)
*
*        Factorize A in the array AFB
*
         CALL SGBTRF(N,N,KL,KU,AFB,LDAFB,IPIV,INFO)
*
         WRITE (NOUT,*)
         IF (INFO.EQ.0) THEN
*
*           Compute solution in the array X
*
            CALL SGBTRS(TRANS,N,KL,KU,NRHS,AFB,LDAFB,IPIV,X,LDX,INFO)
*
*           Improve solution, and compute backward errors and
*           estimated bounds on the forward errors
*
            CALL SGBRFS(TRANS,N,KL,KU,NRHS,AB,LDAB,AFB,LDAFB,IPIV,B,LDB,
     +                  X,LDX,FERR,BERR,WORK,IWORK,INFO)
*
*           Print solution
*
            IFAIL = 0
*
            CALL X04CAE('General',' ',N,NRHS,X,LDX,'Solution(s)',IFAIL)
*
            WRITE (NOUT,*)
            WRITE (NOUT,*) 'Backward errors (machine-dependent)'
            WRITE (NOUT,99999) (BERR(J),J=1,NRHS)
            WRITE (NOUT,*)
     +        'Estimated forward error bounds (machine-dependent)'
            WRITE (NOUT,99999) (FERR(J),J=1,NRHS)
         ELSE
            WRITE (NOUT,*) 'The factor U is singular'
         END IF
      END IF
      STOP
*
99999 FORMAT ((3X,1P,7E11.1))
      END
