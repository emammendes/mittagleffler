subroutine call_mlfv(res,alf,bet,z,fi)
! Wrapping function for the MLFV -- Mittag-Leffler function.
!
complex(8), intent(out) :: res
complex(8), intent(in) :: z
real(8), intent(in) :: alf,bet
integer, intent(in) :: fi

complex(8) :: mlfv
external :: mlfv

res=mlfv(alf,bet,z,fi)

end subroutine call_mlfv

complex(8) recursive function mlfv(alpha,beta,z,fi)  result(res)
! ============================================================================
! MLFV -- Mittag-Leffler function. 
!           MLFV (alpha,beta,Z,P) is the Mittag-Leffler function E{alpha,beta}(Z) 
!           evaluated with accuracy 10^(-fi) for Z. 
!           alpha and beta are real scalars, P is integer ::, Z is complex.
!   
!   Created by Davide Verotta on 3/11/10.
!   After MatLAB code C (C) 2001-2009 Igor Podlubny, Martin Kacenak
!   Copyright 2010 UCSF. All rights reserved.
!
!   Modified by Eduardo Mendes on 5/14/15.
!   
! ============================================================================
implicit none

! Declaration of Variables
!
real(8) :: alpha,beta
complex(8) :: z,newsum,term,aux,a1,a2,oldsum
integer :: fi
real(8) :: r0,rc,angz
real(8) :: eps,aaz
complex(8) :: zn
real(8), parameter :: pi = 3.1415926535897932384626434D0
complex(8), parameter :: ic=(0.d0,1.d0)
logical :: l1,l2,l3,l4
integer :: k,m,h
character(1) :: PN,KN
complex(8) :: rombint
external :: rombint
! 
! Initiliazation of some variables
a1=0d0
a2=0d0
newsum=0d0
oldsum=0d0
res=0d0
!
!   I have commented the folowing lines since the R source will do the job
!if (alpha.lt.0)then
!  print *,"alpha must be greater than zero"
!  return
!else if (beta.gt.5) then
!  print *,"beta must be smaller than 5"
!  return
!else if (alpha.gt.5) then
!  print *,"alpha must be smaller than 5"
!  return
!endif
!				
PN='P'
KN='K'
!
if (beta.LT.0) then
  rc=log(10.d0**(-fi)*pi/(6.D0*(-beta+2)*(-beta*2)**(-beta)))
  rc=(-2.D0*rc)**alpha
else 
  rc=(-2.D0*log(10.d0**(-fi)*pi/6.D0))**alpha
end if
r0=max(1.d0,2.d0*abs(z),rc)
angz=atan2(dimag(z),dble(z))
aaz=abs(angz)
if (alpha.EQ.1d0 .AND. beta.EQ.1d0) then
  res=exp(z)
  return
endif
l1=((alpha.LT.1 .AND. abs(z).LE.1))
l2=(1.LE.alpha.AND.alpha.lt.2)
l3=(abs(z).LE.floor(20/(2.1-alpha)**(5.5-2*alpha)))
l4=(alpha.GE.2d0) .AND. (abs(z).LE.50d0)
!
if (l1.OR.l2.AND.L3.OR.l4) then
  oldsum=0d0
  k=0
  do while ((alpha*k+beta).LE.0 )
    k=k+1
  end do
  newsum=z**k/gamma(alpha*k+beta)
! double summation because z can be negative
  do while(newsum .NE. oldsum) 
    oldsum=newsum
    k=k+1
    term=z**k/gamma(alpha*k+beta)
    newsum=newsum+term
    k=k+1
    term=z**k/gamma(alpha*k+beta)
    newsum=newsum+term
  end do
  res=newsum
  return
endif
! the matlab function fix rounds toward zero, can  use floor since alpha is positive
if (alpha.LE.1.AND.abs(z).LE.floor(5*alpha+10)) then
  if ((aaz.GT.pi*alpha).AND.(abs(aaz-(pi*alpha)).GT.10**(-fi)))then
    if (beta.LE.1) then
      res=rombint(KN,0.D0,r0,fi,alpha,beta,z,0.d0)
    else
      eps=1
      aux=rombint(PN,-pi*alpha,pi*alpha,fi,alpha,beta,z,eps)
      res=rombint(KN,eps,r0,fi,alpha,beta,z,0.d0)+aux
    end if
  else if (aaz.LT.pi*alpha.AND.abs(aaz-(pi*alpha)).GT.10**(-fi))then
    if (beta.LE.1) then
      aux=(z**((1-beta)/alpha))*(exp(z**(1/alpha))/alpha)
      res=rombint(KN,0.D0,r0,fi,alpha,beta,z,0.d0)+aux
    else
      eps=abs(z)/2d0
      aux=rombint(KN,eps,r0,fi,alpha,beta,z,0.d0)
      aux=aux+rombint(PN,-pi*alpha,pi*alpha,fi,alpha,beta,z,eps)
      res=aux+(z**((1.D0-beta)/alpha))*(exp(z**(1.D0/alpha))/alpha)
    end if
  else
    eps=abs(z)+0.5D0
    aux=rombint(PN,-pi*alpha,pi*alpha,fi,alpha,beta,z,eps)
    res=rombint(KN,eps,r0,fi,alpha,beta,z,0.d0)+aux
  end if
  return
endif
!
if (alpha.LE.1) then 
  if (aaz<(pi*alpha/2.D0+min(pi,pi*alpha))/2.D0) then 
    newsum=(z**((1.D0-beta)/alpha))*exp(z**(1.D0/alpha))/alpha
    do k=1,floor(fi/log10(abs(z)))
      newsum=newsum-((z**(-k))/gamma(beta-alpha*k))
    end do
    res=newsum
  else 
    newsum=0d0
    do k=1,floor(fi/log10(abs(z)))
      newsum=newsum-((z**(-k))/gamma(beta-alpha*k))
    end do
    res=newsum
  end if
else
  if (alpha.GE.2) then
    m=floor(alpha/2.D0)
    aux=0d0
! recursive call
    do h=0,m
      zn=(z**(1.D0/(m+1)))*exp((2.D0*pi*ic*h)/(m+1))
      aux=aux+mlfv(alpha/(m+1),beta,zn,fi)
    end do
    res=(1.D0/(m+1))*aux
  else
! recursive call
    a1=mlfv(alpha/2,beta,sqrt(z),fi)
    a2=mlfv(alpha/2,beta,-sqrt(z),fi)
    res=(a1+a2)/2d0
  end if
end if

end function mlfv

! ============================================================================
! Romber Integration For Auxillary functions
! ============================================================================
complex(8) function rombint(funfcn,a,b,order,v1,v2,v3,v4)
implicit none
!	  
character(1) :: funfcn
integer :: order,iorder,ipower,i,j,k
real(8) :: a,b,v1,v2,v4,hh
complex(8) :: v3,a1,a2,auxsum
complex(8), dimension(2,20) :: rom
complex(8) :: kk,pp
external :: kk,pp
!
iorder=order
!
if(funfcn.EQ."K") iorder = 6
!
!if(order.gt.20)then
!  print *,"need to increase size of matrix r which is 8, order is",iorder
!  stop
!end if
!
do i=1,2
  do j=1,iorder
    rom(i,j)=0.D0
  end do
end do
hh=b-a
if(funfcn.EQ.'K')then
  a1=kk(a,v1,v2,v3)
  a2=kk(b,v1,v2,v3)
else
  a1=pp(a,v1,v2,v3,v4)
  a2=pp(b,v1,v2,v3,v4)
end if
rom(1,1)=hh*(a1+a2)/2.d0
!
ipower=1
do i=2,iorder
  auxsum=0.d0
  do j=1,ipower
    if(funfcn.EQ.'K')then
      a1=kk((a+hh*(j-0.5d0)),v1,v2,v3)
    else
      a1=pp((a+hh*(j-0.5d0)),v1,v2,v3,v4)
    endif
    auxsum=auxsum+a1
  end do
  rom(2,1)=(rom(1,1)+hh*auxsum)/2.d0
  do k=1,(i-1)
    rom(2,k+1)=((4**k)*rom(2,k)-rom(1,k))/((4**k)-1)
  end do
  do j=0,(i-1)
    rom(1,j+1)=rom(2,j+1)
  end do
  ipower=ipower*2
  hh=hh/2d0
end do
rombint=rom(1,iorder)
!	
end function rombint
! ============================================================================
complex(8) function kk(r,alfa,beta,z)
implicit none
real(8) :: r,alfa,beta
complex(8) :: z,w
real(8), parameter :: pi = 3.1415926535897932384626434D0
!
w=r**((1-beta)/alfa)*exp(-r**(1/alfa))*(r*sin(pi*(1.d0-beta))-z*sin(pi*(1.d0-beta+alfa)))&
   &/(pi*alfa*(r**2-2.d0*r*z*cos(pi*alfa)+z**2))
kk=w
end function kk
! ============================================================================
complex(8) function pp(r,alpha,beta,z,epsn)
implicit none
real(8) :: r,alpha,beta,epsn
complex(8) :: z,w
real(8), parameter :: pi = 3.1415926535897932384626434D0
complex(8), parameter :: ic=(0.d0,1.d0)
!
w=(epsn**(1.d0/alpha))*sin(r/alpha)+r*(1.d0+(1.d0-beta)/alpha)
pp=((epsn**(1.d0+(1.d0-beta)/alpha))/(2.d0*pi*alpha))*((exp((epsn**(1/alpha))*cos(r/alpha))*&
   &(cos(w)+ic*sin(w))))/(epsn*exp(ic*r)-z)
!
end function pp

