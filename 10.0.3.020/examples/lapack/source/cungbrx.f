*     CUNGBR (F08KTE) Example Program Text
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
      COMPLEX          A(LDA,NMAX), C(LDC,NMAX), TAUP(NMAX), TAUQ(NMAX),
     +                 U(LDU,NMAX), VT(LDVT,NMAX), WORK(LWORK)
      REAL             D(NMAX), E(NMAX-1), RWORK(4*NMAX-4)
      CHARACTER        CLABS(1), RLABS(1)
*     .. External Subroutines ..
      EXTERNAL         CBDSQR, CGEBRD, CUNGBR, F06TFE, X04DBE
*     .. Executable Statements ..
      WRITE (NOUT,*) 'CUNGBR Example Program Results'
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
            CALL CGEBRD(M,N,A,LDA,D,E,TAUQ,TAUP,WORK,LWORK,INFO)
*
            IF (M.GE.N) THEN
*
*              Copy A to VT and U
*
               CALL F06TFE('Upper',N,N,A,LDA,VT,LDVT)
*
               CALL F06TFE('Lower',M,N,A,LDA,U,LDU)
*
*              Form P**H explicitly, storing the result in VT
*
               CALL CUNGBR('P',N,N,M,VT,LDVT,TAUP,WORK,LWORK,INFO)
*
*              Form Q explicitly, storing the result in U
*
               CALL CUNGBR('Q',M,N,N,U,LDU,TAUQ,WORK,LWORK,INFO)
*
*              Compute the SVD of A
*
               CALL CBDSQR('Upper',N,N,M,0,D,E,VT,LDVT,U,LDU,C,LDC,
     +                     RWORK,INFO)
*
*              Print singular values, left & right singular vectors
*
               WRITE (NOUT,*)
               WRITE (NOUT,*) 'Example 1: singular values'
               WRITE (NOUT,99999) (D(I),I=1,N)
               WRITE (NOUT,*)
               IFAIL = 0
*
               CALL X04DBE('General',' ',N,N,VT,LDVT,'Bracketed','F7.4',
     +                     'Example 1: right singular vectors, by row',
     +                     'Integer',RLABS,'Integer',CLABS,80,0,IFAIL)
*
               WRITE (NOUT,*)
*
               CALL X04DBE('General',' ',M,N,U,LDU,'Bracketed','F7.4',
     +                     'Example 1: left singular vectors, by column'
     +                     ,'Integer',RLABS,'Integer',CLABS,80,0,IFAIL)
*
            ELSE
*
*              Copy A to VT and U
*
               CALL F06TFE('Upper',M,N,A,LDA,VT,LDVT)
*
               CALL F06TFE('Lower',M,M,A,LDA,U,LDU)
*
*              Form P**H explicitly, storing the result in VT
*
               CALL CUNGBR('P',M,N,M,VT,LDVT,TAUP,WORK,LWORK,INFO)
*
*              Form Q explicitly, storing the result in U
*
               CALL CUNGBR('Q',M,M,N,U,LDU,TAUQ,WORK,LWORK,INFO)
*
*              Compute the SVD of A
*
               CALL CBDSQR('Lower',M,N,M,0,D,E,VT,LDVT,U,LDU,C,LDC,
     +                     RWORK,INFO)
*
*              Print singular values, left & right singular vectors
*
               WRITE (NOUT,*)
               WRITE (NOUT,*) 'Example 2: singular values'
               WRITE (NOUT,99999) (D(I),I=1,M)
               WRITE (NOUT,*)
               IFAIL = 0
*
               CALL X04DBE('General',' ',M,N,VT,LDVT,'Bracketed','F7.4',
     +                     'Example 2: right singular vectors, by row',
     +                     'Integer',RLABS,'Integer',CLABS,80,0,IFAIL)
*
               WRITE (NOUT,*)
*
               CALL X04DBE('General',' ',M,M,U,LDU,'Bracketed','F7.4',
     +                     'Example 2: left singular vectors, by column'
     +                     ,'Integer',RLABS,'Integer',CLABS,80,0,IFAIL)
*
            END IF
         END IF
   20 CONTINUE
      STOP
*
99999 FORMAT (8X,4(F7.4,11X,:))
      END
