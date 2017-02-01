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
!  Fortran-90 single precision example of solving Helmholtz problem on a full
!  sphere using MKL Poisson Library
!
!*******************************************************************************

!
program s_sph_with_poles
! Include modules defined by mkl_poisson.f90 and mkl_dfti.f90 header files
use mkl_poisson

implicit none

integer np,nt
! Note that the size of the transform np must be even !!!
parameter(np=8,nt=8)
real pi
parameter(pi=3.14159265358979324E0)

real ap,bp,hp,at,bt,ht,q,lp,lt,theta_i,ct,c1
real  u(np+1,nt+1),f(np+1,nt+1)
type(DFTI_DESCRIPTOR), pointer :: handle_s, handle_c
integer stat
real spar(5*np/2+nt+10)
integer ip,it,i
integer ipar(128)

! Printing the header for the example
      print *, ''
      print *, ' Example of use of MKL Poisson Library'
      print *, ' **********************************************'
      print *, ''
      print *, ' This example gives the solution of Helmholtz problem on a whole sphere'
      print *, ' 0<p<2*pi, 0<t<pi, with Helmholtz coefficient q=1 and right-hand side'
      print *, ' f(p,t)=3*cos(t)'
      print *, ' -----------------------------------------------------------------------'
      print *, ' In general, the error should be of order O(1.0/np^2+1.0/nt^2)'
      print '(1x,a,I1)', ' For this example, the value of np=nt is ', np
      print *, ' The approximation error should be of order 0.15E+0, if everything is OK'
      print *, ' -----------------------------------------------------------------------'
      print *, ' Note that np should be even to solve the PERIODIC problem!'
      print *, ' -----------------------------------------------------------------------'
      print *, '                      SINGLE PRECISION COMPUTATIONS                     '
      print *, ' ======================================================================='
      print *, ''

! Defining the rectangular domain on a sphere 0<p<2*pi, 0<t<pi for Helmholtz Solver on a sphere
! Poisson Library will automatically detect that this problem is on a whole sphere!
ap=0.0E0
bp=2*pi
at=0.0E0
bt=pi

! Setting the coefficient q to 1.0E0 for Helmholtz problem
! If you like to solve Poisson problem, please set q to 0.0E0
q=1.0E0

! Computing the mesh size hp in phi-direction
lp=bp-ap
hp=lp/np
! Computing the mesh size ht in theta-direction
lt=bt-at
ht=lt/nt

! Filling in the values of the TRUE solution u(p,t)=cos(t)
! in the mesh points into the array u
! Filling in the right-hand side f(p,t)=3*cos(t)
! in the mesh points into the array f.
! We choose the right-hand side to correspond to the TRUE solution of Helmholtz equation.
! Here we are using the mesh sizes hp and ht computed before to compute
! the coordinates (phi_i,theta_i) of the mesh points
do it=1,nt+1
   do ip=1,np+1
      theta_i=ht*(it-1)
      ct=cos(theta_i)
      u(ip,it)=ct
      f(ip,it)=(2.0E0+q)*ct
   enddo
enddo

! Initializing ipar array to make it free from garbage
do i=1,128
   ipar(i)=0
enddo

! Initializing simple data structures of Poisson Library for Helmholtz Solver on a sphere
! As we are looking for the solution on a whole sphere, this is a PERIDOC problem
! Therefore, the routines ending with "_P" are used to find the solution
   call S_INIT_SPH_P(ap,bp,at,bt,np,nt,q,ipar,spar,stat)
   if (stat.ne.0) goto 999

! Initializing complex data structures of Poisson Library for Helmholtz Solver on a sphere
! NOTE: Right-hand side f may be altered after the Commit step. If you want to keep it,
! you should save it in another memory location!
   call S_COMMIT_SPH_P(f,handle_s,handle_c,ipar,spar,stat)
   if (stat.ne.0) goto 999

! Computing the approximate solution of Helmholtz problem on a whole sphere
   call S_SPH_P(f,handle_s,handle_c,ipar,spar,stat)
   if (stat.ne.0) goto 999

! Cleaning the memory used by handle_s and handle_c
   call FREE_SPH_P(handle_s,handle_c,ipar,stat)
   if (stat.ne.0) goto 999
! Now we can use handle_s and handle_c to solve another Helmholtz problem after a proper initialization

! Printing the results
write(*,10) np
write(*,11) nt
print *, ''
! Watching the error along the line phi=hp
ip=2
c1 = 0.0
do it=1,nt+1
   write(*,12) (ip-1)*hp, (it-1)*ht, f(ip,it)-u(ip,it)
   if (abs(f(ip,it)-u(ip,it)).ge.c1) c1 = abs(f(ip,it)-u(ip,it))
enddo
print *, ''

if (c1.ge.0.15E+0) then
   print *, 'The computed solution seems to be inaccurate.'
   goto 999
endif

! Success message to print if everything is OK
	print *, ' Single precision Helmholtz example on a whole sphere has successfully'
	print *, ' PASSED through all steps of computation!'
! Jumping over failure message
    go to 1
! Failure message to print if something went wrong
999 print *, 'Single precision Helmholtz example on a whole sphere FAILED to compute the solution...'

1 continue

10    format(1x,'The number of mesh intervals in phi-direction is np=',I1)
11    format(1x,'The number of mesh intervals in theta-direction is nt=',I1)
12    format(1x,'In the mesh point (',F5.3,',',F5.3,') the error between the computed and the true solution is equal to ', E10.3)

! End of the example code
end
