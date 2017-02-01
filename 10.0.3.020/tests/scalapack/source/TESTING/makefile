##******************************************************************************
##                              INTEL CONFIDENTIAL
##  Copyright(C) 2001-2008 Intel Corporation. All Rights Reserved.
##  The source code contained  or  described herein and all documents related to
##  the source code ("Material") are owned by Intel Corporation or its suppliers
##  or licensors.  Title to the  Material remains with  Intel Corporation or its
##  suppliers and licensors. The Material contains trade secrets and proprietary
##  and  confidential  information of  Intel or its suppliers and licensors. The
##  Material  is  protected  by  worldwide  copyright  and trade secret laws and
##  treaty  provisions. No part of the Material may be used, copied, reproduced,
##  modified, published, uploaded, posted, transmitted, distributed or disclosed
##  in any way without Intel's prior express written permission.
##  No license  under any  patent, copyright, trade secret or other intellectual
##  property right is granted to or conferred upon you by disclosure or delivery
##  of the Materials,  either expressly, by implication, inducement, estoppel or
##  otherwise.  Any  license  under  such  intellectual property  rights must be
##  express and approved by Intel in writing.
##
##******************************************************************************
##  Content:
##      Intel(R) Math Kernel Library ScaLAPACK tests creation and run
##
##******************************************************************************
include ../../make.inc

all: exe #run

exe:
	( cd LIN ; $(MAKE) comp=$(comp) opt=$(opt) all )
	( cd EIG ; $(MAKE) comp=$(comp) opt=$(opt) all )

clean:
	( cd LIN ; $(MAKE) clean )
	( cd EIG ; $(MAKE) clean )
	$(RMOBJ)
cleanexe: ; rm -f x* *.txt core
cleanall: clean cleanexe

test_sp=yes
ifeq "$(arch)" "em64t"
ifeq "$(comp)" "gnu"
test_sp=no
endif
endif

ifeq "$(test_sp)" "no"
run: dat \
dlu$(IDE).txt    zlu$(IDE).txt \
ddblu$(IDE).txt  zdblu$(IDE).txt \
ddtlu$(IDE).txt  zdtlu$(IDE).txt \
dgblu$(IDE).txt  zgblu$(IDE).txt \
dllt$(IDE).txt   zllt$(IDE).txt \
dpbllt$(IDE).txt zpbllt$(IDE).txt \
dptllt$(IDE).txt zptllt$(IDE).txt \
dinv$(IDE).txt   zinv$(IDE).txt \
dqr$(IDE).txt    zqr$(IDE).txt \
dls$(IDE).txt    zls$(IDE).txt \
dhrd$(IDE).txt   zhrd$(IDE).txt \
dtrd$(IDE).txt   ztrd$(IDE).txt \
dbrd$(IDE).txt   zbrd$(IDE).txt \
dsep$(IDE).txt   zsep$(IDE).txt \
dgsep$(IDE).txt  zgsep$(IDE).txt \
dsvd$(IDE).txt \
dnep$(IDE).txt   znep$(IDE).txt \
                 zevc$(IDE).txt
else
run: dat \
slu$(IDE).txt    dlu$(IDE).txt    clu$(IDE).txt    zlu$(IDE).txt \
sdblu$(IDE).txt  ddblu$(IDE).txt  cdblu$(IDE).txt  zdblu$(IDE).txt \
sdtlu$(IDE).txt  ddtlu$(IDE).txt  cdtlu$(IDE).txt  zdtlu$(IDE).txt \
sgblu$(IDE).txt  dgblu$(IDE).txt  cgblu$(IDE).txt  zgblu$(IDE).txt \
sllt$(IDE).txt   dllt$(IDE).txt   cllt$(IDE).txt   zllt$(IDE).txt \
spbllt$(IDE).txt dpbllt$(IDE).txt cpbllt$(IDE).txt zpbllt$(IDE).txt \
sptllt$(IDE).txt dptllt$(IDE).txt cptllt$(IDE).txt zptllt$(IDE).txt \
sinv$(IDE).txt   dinv$(IDE).txt   cinv$(IDE).txt   zinv$(IDE).txt \
sqr$(IDE).txt    dqr$(IDE).txt    cqr$(IDE).txt    zqr$(IDE).txt \
sls$(IDE).txt    dls$(IDE).txt    cls$(IDE).txt    zls$(IDE).txt \
shrd$(IDE).txt   dhrd$(IDE).txt   chrd$(IDE).txt   zhrd$(IDE).txt \
strd$(IDE).txt   dtrd$(IDE).txt   ctrd$(IDE).txt   ztrd$(IDE).txt \
sbrd$(IDE).txt   dbrd$(IDE).txt   cbrd$(IDE).txt   zbrd$(IDE).txt \
ssep$(IDE).txt   dsep$(IDE).txt   csep$(IDE).txt   zsep$(IDE).txt \
sgsep$(IDE).txt  dgsep$(IDE).txt  cgsep$(IDE).txt  zgsep$(IDE).txt \
ssvd$(IDE).txt   dsvd$(IDE).txt \
snep$(IDE).txt   dnep$(IDE).txt   cnep$(IDE).txt   znep$(IDE).txt \
                                  cevc$(IDE).txt   zevc$(IDE).txt
endif

slu$(IDE).txt    : xslu$(IDE)    ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1
dlu$(IDE).txt    : xdlu$(IDE)    ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1
clu$(IDE).txt    : xclu$(IDE)    ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1
zlu$(IDE).txt    : xzlu$(IDE)    ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1

sdblu$(IDE).txt  : xsdblu$(IDE)  ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1
ddblu$(IDE).txt  : xddblu$(IDE)  ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1
cdblu$(IDE).txt  : xcdblu$(IDE)  ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1
zdblu$(IDE).txt  : xzdblu$(IDE)  ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1

sdtlu$(IDE).txt  : xsdtlu$(IDE)  ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1
ddtlu$(IDE).txt  : xddtlu$(IDE)  ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1
cdtlu$(IDE).txt  : xcdtlu$(IDE)  ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1
zdtlu$(IDE).txt  : xzdtlu$(IDE)  ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1

sgblu$(IDE).txt  : xsgblu$(IDE)  ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1
dgblu$(IDE).txt  : xdgblu$(IDE)  ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1
cgblu$(IDE).txt  : xcgblu$(IDE)  ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1
zgblu$(IDE).txt  : xzgblu$(IDE)  ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1

sllt$(IDE).txt   : xsllt$(IDE)   ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1
dllt$(IDE).txt   : xdllt$(IDE)   ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1
cllt$(IDE).txt   : xcllt$(IDE)   ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1
zllt$(IDE).txt   : xzllt$(IDE)   ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1

spbllt$(IDE).txt : xspbllt$(IDE) ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1
dpbllt$(IDE).txt : xdpbllt$(IDE) ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1
cpbllt$(IDE).txt : xcpbllt$(IDE) ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1
zpbllt$(IDE).txt : xzpbllt$(IDE) ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1

sptllt$(IDE).txt : xsptllt$(IDE) ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1
dptllt$(IDE).txt : xdptllt$(IDE) ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1
cptllt$(IDE).txt : xcptllt$(IDE) ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1
zptllt$(IDE).txt : xzptllt$(IDE) ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1

sinv$(IDE).txt   : xsinv$(IDE)   ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1
dinv$(IDE).txt   : xdinv$(IDE)   ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1
cinv$(IDE).txt   : xcinv$(IDE)   ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1
zinv$(IDE).txt   : xzinv$(IDE)   ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1

sqr$(IDE).txt    : xsqr$(IDE)    ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1
dqr$(IDE).txt    : xdqr$(IDE)    ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1
cqr$(IDE).txt    : xcqr$(IDE)    ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1
zqr$(IDE).txt    : xzqr$(IDE)    ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1

sls$(IDE).txt    : xsls$(IDE)    ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1
dls$(IDE).txt    : xdls$(IDE)    ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1
cls$(IDE).txt    : xcls$(IDE)    ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1
zls$(IDE).txt    : xzls$(IDE)    ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1

shrd$(IDE).txt   : xshrd$(IDE)   ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1
dhrd$(IDE).txt   : xdhrd$(IDE)   ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1
chrd$(IDE).txt   : xchrd$(IDE)   ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1
zhrd$(IDE).txt   : xzhrd$(IDE)   ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1

strd$(IDE).txt   : xstrd$(IDE)   ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1
dtrd$(IDE).txt   : xdtrd$(IDE)   ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1
ctrd$(IDE).txt   : xctrd$(IDE)   ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1
ztrd$(IDE).txt   : xztrd$(IDE)   ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1

sbrd$(IDE).txt   : xsbrd$(IDE)   ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1
dbrd$(IDE).txt   : xdbrd$(IDE)   ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1
cbrd$(IDE).txt   : xcbrd$(IDE)   ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1
zbrd$(IDE).txt   : xzbrd$(IDE)   ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1

ssep$(IDE).txt   : xssep$(IDE)   ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1
dsep$(IDE).txt   : xdsep$(IDE)   ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1
csep$(IDE).txt   : xcsep$(IDE)   ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1
zsep$(IDE).txt   : xzsep$(IDE)   ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1

sgsep$(IDE).txt  : xsgsep$(IDE)  ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1
dgsep$(IDE).txt  : xdgsep$(IDE)  ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1
cgsep$(IDE).txt  : xcgsep$(IDE)  ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1
zgsep$(IDE).txt  : xzgsep$(IDE)  ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1

ssvd$(IDE).txt   : xssvd$(IDE)   ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1
dsvd$(IDE).txt   : xdsvd$(IDE)   ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1

snep$(IDE).txt   : xsnep$(IDE)   ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1
dnep$(IDE).txt   : xdnep$(IDE)   ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1
cnep$(IDE).txt   : xcnep$(IDE)   ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1
znep$(IDE).txt   : xznep$(IDE)   ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1

cevc$(IDE).txt   : xcevc$(IDE)   ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1
zevc$(IDE).txt   : xzevc$(IDE)   ; mkdir -p ../../_results/$(IDE) ; $(MPIRUN) ./$< 1>../../_results/$(IDE)/$@ 2>&1

dat: BLLT.dat BLU.dat BRD.dat EVC.dat HRD.dat INV.dat LLT.dat \
     LS.dat LU.dat NEP.dat QR.dat SEP.dat SVD.dat TRD.dat

xslu$(IDE)    : ; ( cd LIN ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )
xdlu$(IDE)    : ; ( cd LIN ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )
xclu$(IDE)    : ; ( cd LIN ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )
xzlu$(IDE)    : ; ( cd LIN ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )

xsdblu$(IDE)  : ; ( cd LIN ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )
xddblu$(IDE)  : ; ( cd LIN ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )
xcdblu$(IDE)  : ; ( cd LIN ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )
xzdblu$(IDE)  : ; ( cd LIN ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )

xsdtlu$(IDE)  : ; ( cd LIN ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )
xddtlu$(IDE)  : ; ( cd LIN ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )
xcdtlu$(IDE)  : ; ( cd LIN ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )
xzdtlu$(IDE)  : ; ( cd LIN ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )

xsgblu$(IDE)  : ; ( cd LIN ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )
xdgblu$(IDE)  : ; ( cd LIN ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )
xcgblu$(IDE)  : ; ( cd LIN ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )
xzgblu$(IDE)  : ; ( cd LIN ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )

xsllt$(IDE)   : ; ( cd LIN ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )
xdllt$(IDE)   : ; ( cd LIN ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )
xcllt$(IDE)   : ; ( cd LIN ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )
xzllt$(IDE)   : ; ( cd LIN ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )

xspbllt$(IDE) : ; ( cd LIN ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )
xdpbllt$(IDE) : ; ( cd LIN ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )
xcpbllt$(IDE) : ; ( cd LIN ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )
xzpbllt$(IDE) : ; ( cd LIN ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )

xsptllt$(IDE) : ; ( cd LIN ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )
xdptllt$(IDE) : ; ( cd LIN ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )
xcptllt$(IDE) : ; ( cd LIN ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )
xzptllt$(IDE) : ; ( cd LIN ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )

xsinv$(IDE)   : ; ( cd LIN ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )
xdinv$(IDE)   : ; ( cd LIN ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )
xcinv$(IDE)   : ; ( cd LIN ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )
xzinv$(IDE)   : ; ( cd LIN ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )

xsqr$(IDE)    : ; ( cd LIN ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )
xdqr$(IDE)    : ; ( cd LIN ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )
xcqr$(IDE)    : ; ( cd LIN ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )
xzqr$(IDE)    : ; ( cd LIN ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )

xsls$(IDE)    : ; ( cd LIN ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )
xdls$(IDE)    : ; ( cd LIN ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )
xcls$(IDE)    : ; ( cd LIN ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )
xzls$(IDE)    : ; ( cd LIN ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )

xshrd$(IDE)   : ; ( cd EIG ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )
xdhrd$(IDE)   : ; ( cd EIG ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )
xchrd$(IDE)   : ; ( cd EIG ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )
xzhrd$(IDE)   : ; ( cd EIG ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )

xstrd$(IDE)   : ; ( cd EIG ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )
xdtrd$(IDE)   : ; ( cd EIG ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )
xctrd$(IDE)   : ; ( cd EIG ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )
xztrd$(IDE)   : ; ( cd EIG ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )

xsbrd$(IDE)   : ; ( cd EIG ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )
xdbrd$(IDE)   : ; ( cd EIG ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )
xcbrd$(IDE)   : ; ( cd EIG ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )
xzbrd$(IDE)   : ; ( cd EIG ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )

xssep$(IDE)   : ; ( cd EIG ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )
xdsep$(IDE)   : ; ( cd EIG ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )
xcsep$(IDE)   : ; ( cd EIG ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )
xzsep$(IDE)   : ; ( cd EIG ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )

xsgsep$(IDE)  : ; ( cd EIG ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )
xdgsep$(IDE)  : ; ( cd EIG ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )
xcgsep$(IDE)  : ; ( cd EIG ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )
xzgsep$(IDE)  : ; ( cd EIG ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )

xssvd$(IDE)   : ; ( cd EIG ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )
xdsvd$(IDE)   : ; ( cd EIG ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )

xsnep$(IDE)   : ; ( cd EIG ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )
xdnep$(IDE)   : ; ( cd EIG ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )
xcnep$(IDE)   : ; ( cd EIG ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )
xznep$(IDE)   : ; ( cd EIG ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )

xcevc$(IDE)   : ; ( cd EIG ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )
xzevc$(IDE)   : ; ( cd EIG ; $(MAKE) comp=$(comp) opt=$(opt) ../$@ )

