*     SORGBR (F08KFE) Example Program Text
*     Mark 16 Release. NAG Copyright 1992.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          MMAX, NMAX, LDA, LDVT, LDU, LDC, LWORK
      PARAMETER        (MMAX=8,NMAX=8,LDA=MMAX,LDVT=NMAX,LDU=MMAX,
     +                 LDC=NMAX,LWORK=64*(MMAX+NMAX))
*     .. Local Scalars ..
      INTEGER          I, IC, IFAIL, INFO, J, M, N
*     .. Local Arrays ..
      REAL             A(LDA,NMAX), C(LDC,NMAX), D(NMAX), E(NMAX-1),
     +                 TAUP(NMAX), TAUQ(NMAX), U(LDU,NMAX),
     +                 VT(LDVT,NMAX), WORK(LWORK)
*     .. External Subroutines ..
      EXTERNAL         F06QFE, SBDSQR, SGEBRD, SORGBR, X04CAE
*     .. Executable Statements ..
      WRITE (NOUT,*) 'SORGBR Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      DO 20 IC = 1, 2
         READ (NIN,*) M, N
         IF (M.LE.MMAX .AND. N.LE.NMAX) THEN
*
*           Read A from data file
*
            READ (NIN,*) ((A(I,J),J=1,N),I=1,M)
*
*           Reduce A to bidiagonal form
*
            CALL SGEBRD(M,N,A,LDA,D,E,TAUQ,TAUP,WORK,LWORK,INFO)
*
            IF (M.GE.N) THEN
*
*              Copy A to VT and U
*
               CALL F06QFE('Upper',N,N,A,LDA,VT,LDVT)
*
               CALL F06QFE('Lower',M,N,A,LDA,U,LDU)
*
*              Form P**T explicitly, storing the result in VT
*
               CALL SORGBR('P',N,N,M,VT,LDVT,TAUP,WORK,LWORK,INFO)
*
*              Form Q explicitly, storing the result in U
*
               CALL SORGBR('Q',M,N,N,U,LDU,TAUQ,WORK,LWORK,INFO)
*
*              Compute the SVD of A
*
               CALL SBDSQR('Upper',N,N,M,0,D,E,VT,LDVT,U,LDU,C,LDC,WORK,
     +                     INFO)
*
*              Print singular values, left & right singular vectors
*
               WRITE (NOUT,*)
               WRITE (NOUT,*) 'Example 1: singular values'
               WRITE (NOUT,99999) (D(I),I=1,N)
               WRITE (NOUT,*)
               IFAIL = 0
*
               CALL X04CAE('General',' ',N,N,VT,LDVT,
     +                     'Example 1: right singular vectors, by row',
     +                     IFAIL)
*
               WRITE (NOUT,*)
*
               CALL X04CAE('General',' ',M,N,U,LDU,
     +                     'Example 1: left singular vectors, by column'
     +                     ,IFAIL)
*
            ELSE
*
*              Copy A to VT and U
*
               CALL F06QFE('Upper',M,N,A,LDA,VT,LDVT)
*
               CALL F06QFE('Lower',M,M,A,LDA,U,LDU)
*
*              Form P**T explicitly, storing the result in VT
*
               CALL SORGBR('P',M,N,M,VT,LDVT,TAUP,WORK,LWORK,INFO)
*
*              Form Q explicitly, storing the result in U
*
               CALL SORGBR('Q',M,M,N,U,LDU,TAUQ,WORK,LWORK,INFO)
*
*              Compute the SVD of A
*
               CALL SBDSQR('Lower',M,N,M,0,D,E,VT,LDVT,U,LDU,C,LDC,WORK,
     +                     INFO)
*
*              Print singular values, left & right singular vectors
*
               WRITE (NOUT,*)
               WRITE (NOUT,*) 'Example 2: singular values'
               WRITE (NOUT,99999) (D(I),I=1,M)
               WRITE (NOUT,*)
               IFAIL = 0
*
               CALL X04CAE('General',' ',M,N,VT,LDVT,
     +                     'Example 2: right singular vectors, by row',
     +                     IFAIL)
*
               WRITE (NOUT,*)
*
               CALL X04CAE('General',' ',M,M,U,LDU,
     +                     'Example 2: left singular vectors, by column'
     +                     ,IFAIL)
*
            END IF
         END IF
   20 CONTINUE
      STOP
*
99999 FORMAT (3X,(8F8.4))
      END
