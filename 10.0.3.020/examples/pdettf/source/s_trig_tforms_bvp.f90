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
!  Single precision Fortran90 test example for trigonometric transforms
!*******************************************************************************
!
! This example gives the solution of the 1D differential problems
! with the equation  -u"+u=f(x), 0<x<1, and with 3 types of boundary conditions:
! u(0)=u(1)=0 (DD case), or u'(0)=u'(1)=0 (NN case), or u'(0)=u(1)=0 (ND case)


program s_tt_example_bvp

  use mkl_dfti
  use mkl_trig_transforms

  implicit none

  integer n, i, k,j, tt_type
  integer ir, ipar(128)
  parameter (n=8)
  real pi, xi
  real c1, c2, c3, c4, c5, c6
! NOTE: This example uses shorter spar array of size 3n/2+2 instead of 5n/2+2 
! as only sine, cosine, and staggered cosine transforms are used. More details
! can be found in Chapter 13 of MKL Manual. 
  real u(n+1), f(n+1), spar(3*n/2+2), lambda(n+1)
  parameter (pi=3.14159265358979324E0)
  type(dfti_descriptor), pointer :: handle

! Printing the header for the example
  print *, ''
  print *, ' Example of use of MKL Trigonometric Transforms'
  print *, ' **********************************************'
  print *, ''
  print *, ' This example gives the solution of the 1D differential problems'
  print *, ' with the equation -u"+u=f(x), 0<x<1, '
  print *, ' and with 3 types of boundary conditions:'
  print *, ' DD case: u(0)=u(1)=0,'
  print *, ' NN case: u''(0)=u''(1)=0,'
  print *, ' ND case: u''(0)=u(1)=0.'
  print *, ' -----------------------------------------------------------------------'
  print *, ' In general, the error should be of order O(1.0/n**2)'
  print *, ' For this example, the value of n is', n
  print *, ' The approximation error should be of order 0.5E-01, if everything is OK'
  print *, ' -----------------------------------------------------------------------'
  print *, '                      SINGLE PRECISION COMPUTATIONS                     '
  print *, ' ======================================================================='
  print *, ''

  do i=0,2
! Varying the type of the transform
    tt_type=i
! Computing test solution u(x)
    do k=1,n+1
      xi=1.0E0*(k-1)/n
      u(k)=sin(pi*xi)**2
    end do
! Computing the right-hand side f(x)
    do k=1,n+1
      f(k)=(4.0E0*(pi**2)+1.0E0)*u(k)-2.0E0*(pi**2)
    end do
! Computing the right-hand side for the algebraic system
    do k=1,n+1
      f(k)=f(k)/(n**2)
    end do
    if (tt_type.eq.0) then
! The Dirichlet boundary conditions
      f(1)=0.0E0
      f(n+1)=0.0E0
    end if
    if (tt_type.eq.2) then
! The mixed Neumann-Dirichlet boundary conditions
      f(n+1)=0.0E0
    end if

! Computing the eigenvalues for the three-point finite-difference problem
    if (tt_type.eq.0.or.tt_type.eq.1) then
      do k=1,n+1
        lambda(k)=(2.0E0*sin(0.5E0*pi*(k-1)/n))**2+1.0E0/(n**2)
      end do
    end if
    if (tt_type.eq.2) then
      do k=1,n+1
        lambda(k)=(2.0E0*sin(0.25E0*pi*(2*k-1)/n))**2+1.0E0/(n**2)
      end do
    end if

! Computing the solution of 1D problem using trigonometric transforms
! First we initialize the transform
    CALL S_INIT_TRIG_TRANSFORM(n,tt_type,ipar,spar,ir)
    if (ir.ne.0) goto 99
! Then we commit the transform. Note that the data in f will be changed at this stage !
! If you want to keep them, save them in some other array before the call to the routine
    CALL S_COMMIT_TRIG_TRANSFORM(f,handle,ipar,spar,ir)
    if (ir.ne.0) goto 99
! Now we can apply trigonometric transform
    CALL S_FORWARD_TRIG_TRANSFORM(f,handle,ipar,spar,ir)
    if (ir.ne.0) goto 99

! Scaling the solution by the eigenvalues
    do k=1,n+1
       f(k)=f(k)/lambda(k)
    end do

! Now we can apply trigonometric transform once again as ONLY input vector f has changed
    CALL S_BACKWARD_TRIG_TRANSFORM(f,handle,ipar,spar,ir)
    if (ir.ne.0) goto 99
! Cleaning the memory used by handle
! Now we can use handle for other KIND of trigonometric transform
    CALL FREE_TRIG_TRANSFORM(handle,ipar,ir)
    if (ir.ne.0) goto 99

! Performing the error analysis
    c1=0.0E0
    c2=0.0E0
    c3=0.0E0
    do k=1,n+1
! Computing the absolute value of the exact solution
      c4=abs(u(k))
! Computing the absolute value of the computed solution
! Note that the solution is now in place of the former right-hand side !
      c5=abs(f(k))
! Computing the absolute error
      c6=abs(f(k)-u(k))
! Computing the maximum among the above 3 values c4-c6
      if (c4.gt.c1) c1=c4
      if (c5.gt.c2) c2=c5
      if (c6.gt.c3) c3=c6
    end do

! Printing the results
    if (tt_type.eq.0) then
      print *, 'The computed solution of DD problem is'
    endif
    if (tt_type.eq.1) then
      print *, 'The computed solution of NN problem is'
    endif
    if (tt_type.eq.2) then
      print *, 'The computed  solution of ND problem is'
    endif
    print *, ''
    do k=1,n+1
      write(*,11) k,f(k)
    end do
    print *, ''
    write(*,12) c3/c1
    print *, ''
    if (c3/c1.ge.5.0E-2) then
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
12    format(1x,'Relative error =',E10.3)

! End of the example code
end
