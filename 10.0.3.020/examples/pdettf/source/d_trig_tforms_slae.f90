!*******************************************************************************
!                              INTEL CONFIDENTIAL
!   Copyright(C) 2006-2008 Intel Corporation. All Rights Reserved.
!   The source code contained  or  described herein and all documents related to
!   the source code ("Material") are owned by Intel Corporation or its suppliers
!   or licensors.  Title to the  Material remains with  Intel Corporation or its
!   suppliers and licensors. The Material contains trade secrets and proprietary
!   and  confidential  information of  Intel or its suppliers and licensors. The
!   Material  is  protected  by  worldwide  copyright  and trade secret laws and
!   treaty  provisions. No part of the Material may be used, copied, reproduced,
!   modified, published, uploaded, posted, transmitted, distributed or disclosed
!   in any way without Intel's prior express written permission.
!   No license  under any  patent, copyright, trade secret or other intellectual
!   property right is granted to or conferred upon you by disclosure or delivery
!   of the Materials,  either expressly, by implication, inducement, estoppel or
!   otherwise.  Any  license  under  such  intellectual property  rights must be
!   express and approved by Intel in writing.
!
!*******************************************************************************
!  Content:
!  Double precision Fortran90 test example for trigonometric transforms
!*******************************************************************************
!
! This example gives the solution of the linear algebraic system,
! corresponding to the three-point approximation of the 1D differential
! problems with the equation  -u"+u=f(x), 0<x<1, and with 3 types of
! boundary conditions:
! u(0)=u(1)=0 (DD case), or u'(0)=u'(1)=0 (NN case), or u'(0)=u(1)=0 (ND case)

program d_tt_example_slae

  use mkl_dfti
  use mkl_trig_transforms

  implicit none

  integer n, i, k, tt_type
  integer ir, ipar(128)
  parameter (n=8)
  double precision pi, xi, c
  double precision c1, c2, c3, c4, c5, c6
! NOTE: This example uses shorter dpar array of size 3n/2+2 instead of 5n/2+2 
! as only sine, cosine, and staggered cosine transforms are used. More details
! can be found in Chapter 13 of MKL Manual. 
  double precision u(n+1), f(n+1), dpar(3*n/2+2), lambda(n+1)
  parameter (pi=3.14159265358979324D0)
  type(dfti_descriptor), pointer :: handle

! Printing the header for the example
  print *, ''
  print *, ' Example of use of MKL Trigonometric Transforms'
  print *, ' **********************************************'
  print *, ''
  print *, ' This example gives the the exact solutions of the linear algebraic '
  print *, ' systems, corresponding to the three-point approximations of the 1D '
  print *, ' differential problems with the equation -u"+u=f(x), 0<x<1, '
  print *, ' and with 3 types of boundary conditions:'
  print *, ' DD case: u(0)=u(1)=0,'
  print *, ' NN case: u''(0)=u''(1)=0,'
  print *, ' ND case: u''(0)=u(1)=0.'
  print *, ' -----------------------------------------------------------------'
  print *, ' The error expected to be close to the machine zero in all 3 cases'
  print *, ' if everything is OK'
  print *, ' -----------------------------------------------------------------'
  print *, '                   DOUBLE PRECISION COMPUTATIONS                  '
  print *, ' ================================================================='
  print *, ''

  do i=0,2
! Varying the type of the transform
    tt_type=i

! Computing test solutions u(x)
    do k=1,n+1
      xi=1.0D0*(k-1)/n
! The exact solution with Dirichlet-Dirichlet boundary conditions
      if (tt_type.eq.0) c=dsin(pi*xi)*dexp(xi)
! The exact solution with Neumann-Neumann boundary conditions
      if (tt_type.eq.1) c=(pi*dcos(pi*xi)-dsin(pi*xi))*dexp(xi)
! The exact solution with Neumann-Dirichlet boundary conditions
      if (tt_type.eq.2) c=dcos(0.5D0*pi*xi)*(dexp(xi)-xi)
        u(k)=c
    end do
! Computing the right-hand side f(x) that corresponds to the finite element approximation
! and provides the exact solution computed above of the resulting algebraic system
    do k=2,n
      f(k)=(2.0D0+1.0D0/(n**2))*u(k)-u(k-1)-u(k+1)
    end do
    if (tt_type.eq.0) then
! The Dirichlet boundary conditions
      f(1)=0.0D0
      f(n+1)=0.0D0
    end if
    if (tt_type.eq.1) then
! The Neumann boundary conditions
      f(1)=(2.0D0+1.0D0/(n**2))*u(1)-2.0D0*u(2)
      f(n+1)=(2.0D0+1.0D0/(n**2))*u(n+1)-2.0D0*u(n)
    end if
    if (tt_type.eq.2) then
! The mixed Neumann-Dirichlet boundary conditions
      f(1)=(2.0D0+1.0D0/(n**2))*u(1)-2.0D0*u(2)
      f(n+1)=0.0D0
    end if

! Computing the eigenvalues for the three-point finite-difference problem
    if (tt_type.eq.0.or.tt_type.eq.1) then
      do k=1,n+1
        lambda(k)=(2.0D0*dsin(0.5D0*pi*(k-1)/n))**2+1.0D0/(n**2)
      end do
    end if
    if (tt_type.eq.2) then
      do k=1,n+1
        lambda(k)=(2.0D0*dsin(0.25D0*pi*(2*k-1)/n))**2+1.0D0/(n**2)
      end do
    end if

! Computing the solution of 1D problem using trigonometric transforms
! First we initialize the transform
    CALL D_INIT_TRIG_TRANSFORM(n,tt_type,ipar,dpar,ir)
    if (ir.ne.0) goto 99
! Then we commit the transform. Note that the data in f will be changed at this stage !
! If you want to keep them, save them in some other array before the call to the routine
    CALL D_COMMIT_TRIG_TRANSFORM(f,handle,ipar,dpar,ir)
    if (ir.ne.0) goto 99
! Now we can apply trigonometric transform
    CALL D_FORWARD_TRIG_TRANSFORM(f,handle,ipar,dpar,ir)
    if (ir.ne.0) goto 99

! Scaling the solution by the eigenvalues
    do k=1,n+1
      f(k)=f(k)/lambda(k)
    end do

! Now we can apply trigonometric transform once again as ONLY input vector f has changed
    CALL D_BACKWARD_TRIG_TRANSFORM(f,handle,ipar,dpar,ir)
    if (ir.ne.0) goto 99
! Cleaning the memory used by handle
! Now we can use handle for other kind of trigonometric transform
    CALL FREE_TRIG_TRANSFORM(handle,ipar,ir)
    if (ir.ne.0) goto 99

! Performing the error analysis
    c1=0.0D0
    c2=0.0D0
    c3=0.0D0
    do k=1,n+1
! Computing the absolute value of the exact solution
      c4=dabs(u(k))
! Computing the absolute value of the computed solution
! Note that the solution is now in place of the former right-hand side !
      c5=dabs(f(k))
! Computing the absolute error
      c6=dabs(f(k)-u(k))
! Computing the maximum among the above 3 values c4-c6
      if (c4.gt.c1) c1=c4
      if (c5.gt.c2) c2=c5
      if (c6.gt.c3) c3=c6
    end do

! Printing the results
    if (tt_type.eq.0) then
      print *, 'The computed solution of DD problem is'
    end if
    if (tt_type.eq.1) then
      print *, 'The computed solution of NN problem is'
    end if
    if (tt_type.eq.2) then
      print *, 'The computed solution of ND problem is'
    endif
    print *, ''
    do k=1,n+1
      write(*,11) k,f(k)
    end do
    print *, ''
    write(*,12) c3/c1
    print *, ''
    if (c3/c1.ge.1.0D-14) then
      print *, 'The computed solution seems to be inaccurate.'
      goto 99
    endif
! End of the loop over the different kind of transforms and problems
  end do

! Success message to print if everything is OK
  print *, 'This example has successfully PASSED through all steps of computation!'
! Jumping over failure message
  goto 1
! Failure message to print if something went wrong
99    continue
      print *, 'FAILED to compute the solution(s)...'

1     continue

! Print formats
11    format(1x,'u(',I1,')=',F6.3)
12    format(1x,'Error=',E10.3)

! End of the example code
end
