*     DGBTRS (F07BEF) Example Program Text
*     Mark 15 Release. NAG Copyright 1991.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, KLMAX, KUMAX, LDAB, NRHMAX, LDB
      PARAMETER        (NMAX=8,KLMAX=8,KUMAX=8,LDAB=2*KLMAX+KUMAX+1,
     +                 NRHMAX=NMAX,LDB=NMAX)
      CHARACTER        TRANS
      PARAMETER        (TRANS='N')
*     .. Local Scalars ..
      INTEGER          I, IFAIL, INFO, J, K, KL, KU, N, NRHS
*     .. Local Arrays ..
      DOUBLE PRECISION AB(LDAB,NMAX), B(LDB,NRHMAX)
      INTEGER          IPIV(NMAX)
*     .. External Subroutines ..
      EXTERNAL         DGBTRF, DGBTRS, X04CAF
*     .. Intrinsic Functions ..
      INTRINSIC        MAX, MIN
*     .. Executable Statements ..
      WRITE (NOUT,*) 'DGBTRS Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, NRHS, KL, KU
      IF (N.LE.NMAX .AND. NRHS.LE.NRHMAX .AND. KL.LE.KLMAX .AND. KU.LE.
     +    KUMAX) THEN
*
*        Read A and B from data file
*
         K = KL + KU + 1
         READ (NIN,*) ((AB(K+I-J,J),J=MAX(I-KL,1),MIN(I+KU,N)),I=1,N)
         READ (NIN,*) ((B(I,J),J=1,NRHS),I=1,N)
*
*        Factorize A
*
         CALL DGBTRF(N,N,KL,KU,AB,LDAB,IPIV,INFO)
*
         WRITE (NOUT,*)
         IF (INFO.EQ.0) THEN
*
*           Compute solution
*
            CALL DGBTRS(TRANS,N,KL,KU,NRHS,AB,LDAB,IPIV,B,LDB,INFO)
*
*           Print solution
*
            IFAIL = 0
*
            CALL X04CAF('General',' ',N,NRHS,B,LDB,'Solution(s)',IFAIL)
*
         ELSE
            WRITE (NOUT,*) 'The factor U is singular'
         END IF
      END IF
      STOP
*
      END
