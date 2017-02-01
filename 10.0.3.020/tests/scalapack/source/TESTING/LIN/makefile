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

include ../../../make.inc

sluexe = ../xslu$(IDE)
dluexe = ../xdlu$(IDE)
cluexe = ../xclu$(IDE)
zluexe = ../xzlu$(IDE)

sdbluexe = ../xsdblu$(IDE)
ddbluexe = ../xddblu$(IDE)
cdbluexe = ../xcdblu$(IDE)
zdbluexe = ../xzdblu$(IDE)

sdtluexe = ../xsdtlu$(IDE)
ddtluexe = ../xddtlu$(IDE)
cdtluexe = ../xcdtlu$(IDE)
zdtluexe = ../xzdtlu$(IDE)

sgbluexe = ../xsgblu$(IDE)
dgbluexe = ../xdgblu$(IDE)
cgbluexe = ../xcgblu$(IDE)
zgbluexe = ../xzgblu$(IDE)

slltexe = ../xsllt$(IDE)
dlltexe = ../xdllt$(IDE)
clltexe = ../xcllt$(IDE)
zlltexe = ../xzllt$(IDE)

spblltexe = ../xspbllt$(IDE)
dpblltexe = ../xdpbllt$(IDE)
cpblltexe = ../xcpbllt$(IDE)
zpblltexe = ../xzpbllt$(IDE)

sptlltexe = ../xsptllt$(IDE)
dptlltexe = ../xdptllt$(IDE)
cptlltexe = ../xcptllt$(IDE)
zptlltexe = ../xzptllt$(IDE)

sinvexe = ../xsinv$(IDE)
dinvexe = ../xdinv$(IDE)
cinvexe = ../xcinv$(IDE)
zinvexe = ../xzinv$(IDE)

sqrexe = ../xsqr$(IDE)
dqrexe = ../xdqr$(IDE)
cqrexe = ../xcqr$(IDE)
zqrexe = ../xzqr$(IDE)

slsexe = ../xsls$(IDE)
dlsexe = ../xdls$(IDE)
clsexe = ../xcls$(IDE)
zlsexe = ../xzls$(IDE)

smatgen = psmatgen.o pmatgeninc.o
dmatgen = pdmatgen.o pmatgeninc.o
cmatgen = pcmatgen.o pmatgeninc.o
zmatgen = pzmatgen.o pmatgeninc.o

slinchk = pslaschk.o pslafchk.o
dlinchk = pdlaschk.o pdlafchk.o
clinchk = pclaschk.o pclafchk.o
zlinchk = pzlaschk.o pzlafchk.o

slu = psludriver.o psluinfo.o psgetrrv.o $(smatgen) $(slinchk)
dlu = pdludriver.o pdluinfo.o pdgetrrv.o $(dmatgen) $(dlinchk)
clu = pcludriver.o pcluinfo.o pcgetrrv.o $(cmatgen) $(clinchk)
zlu = pzludriver.o pzluinfo.o pzgetrrv.o $(zmatgen) $(zlinchk)

sdblu = psdbdriver.o psdbinfo.o psdblaschk.o psdbmv1.o psbmatgen.o $(smatgen)
ddblu = pddbdriver.o pddbinfo.o pddblaschk.o pddbmv1.o pdbmatgen.o $(dmatgen)
cdblu = pcdbdriver.o pcdbinfo.o pcdblaschk.o pcdbmv1.o pcbmatgen.o $(cmatgen)
zdblu = pzdbdriver.o pzdbinfo.o pzdblaschk.o pzdbmv1.o pzbmatgen.o $(zmatgen)

sdtlu = psdtdriver.o psdtinfo.o psdtlaschk.o psdbmv1.o psbmatgen.o $(smatgen)
ddtlu = pddtdriver.o pddtinfo.o pddtlaschk.o pddbmv1.o pdbmatgen.o $(dmatgen)
cdtlu = pcdtdriver.o pcdtinfo.o pcdtlaschk.o pcdbmv1.o pcbmatgen.o $(cmatgen)
zdtlu = pzdtdriver.o pzdtinfo.o pzdtlaschk.o pzdbmv1.o pzbmatgen.o $(zmatgen)

sgblu = psgbdriver.o psgbinfo.o psdblaschk.o psgbmv1.o psbmatgen.o $(smatgen)
dgblu = pdgbdriver.o pdgbinfo.o pddblaschk.o pdgbmv1.o pdbmatgen.o $(dmatgen)
cgblu = pcgbdriver.o pcgbinfo.o pcdblaschk.o pcgbmv1.o pcbmatgen.o $(cmatgen)
zgblu = pzgbdriver.o pzgbinfo.o pzdblaschk.o pzgbmv1.o pzbmatgen.o $(zmatgen)


sllt = pslltdriver.o pslltinfo.o pspotrrv.o $(smatgen) $(slinchk)
dllt = pdlltdriver.o pdlltinfo.o pdpotrrv.o $(dmatgen) $(dlinchk)
cllt = pclltdriver.o pclltinfo.o pcpotrrv.o $(cmatgen) $(clinchk)
zllt = pzlltdriver.o pzlltinfo.o pzpotrrv.o $(zmatgen) $(zlinchk)

spbllt = pspbdriver.o pspbinfo.o pspblaschk.o pspbmv1.o psbmatgen.o $(smatgen)
dpbllt = pdpbdriver.o pdpbinfo.o pdpblaschk.o pdpbmv1.o pdbmatgen.o $(dmatgen)
cpbllt = pcpbdriver.o pcpbinfo.o pcpblaschk.o pcpbmv1.o pcbmatgen.o $(cmatgen)
zpbllt = pzpbdriver.o pzpbinfo.o pzpblaschk.o pzpbmv1.o pzbmatgen.o $(zmatgen)

sptllt = psptdriver.o psptinfo.o psptlaschk.o pspbmv1.o psbmatgen.o $(smatgen)
dptllt = pdptdriver.o pdptinfo.o pdptlaschk.o pdpbmv1.o pdbmatgen.o $(dmatgen)
cptllt = pcptdriver.o pcptinfo.o pcptlaschk.o pcpbmv1.o pcbmatgen.o $(cmatgen)
zptllt = pzptdriver.o pzptinfo.o pzptlaschk.o pzpbmv1.o pzbmatgen.o $(zmatgen)

sinv = psinvdriver.o psinvinfo.o psinvchk.o $(smatgen)
dinv = pdinvdriver.o pdinvinfo.o pdinvchk.o $(dmatgen)
cinv = pcinvdriver.o pcinvinfo.o pcinvchk.o $(cmatgen)
zinv = pzinvdriver.o pzinvinfo.o pzinvchk.o $(zmatgen)

sqr = psqrdriver.o psqrinfo.o psgeqrrv.o psgeqlrv.o psgelqrv.o \
      psgerqrv.o   pstzrzrv.o pslafchk.o $(smatgen)
dqr = pdqrdriver.o pdqrinfo.o pdgeqrrv.o pdgeqlrv.o pdgelqrv.o \
      pdgerqrv.o   pdtzrzrv.o pdlafchk.o $(dmatgen)
cqr = pcqrdriver.o pcqrinfo.o pcgeqrrv.o pcgeqlrv.o pcgelqrv.o \
      pcgerqrv.o   pctzrzrv.o pclafchk.o $(cmatgen)
zqr = pzqrdriver.o pzqrinfo.o pzgeqrrv.o pzgeqlrv.o pzgelqrv.o \
      pzgerqrv.o   pztzrzrv.o pzlafchk.o $(zmatgen)

sls = pslsdriver.o pslsinfo.o psqrt13.o psqrt14.o psqrt16.o \
      psqrt17.o $(smatgen)
dls = pdlsdriver.o pdlsinfo.o pdqrt13.o pdqrt14.o pdqrt16.o \
      pdqrt17.o $(dmatgen)
cls = pclsdriver.o pclsinfo.o pcqrt13.o pcqrt14.o pcqrt16.o \
      pcqrt17.o $(cmatgen)
zls = pzlsdriver.o pzlsinfo.o pzqrt13.o pzqrt14.o pzqrt16.o \
      pzqrt17.o $(zmatgen)


all : single double complex complex16

single: $(sluexe) $(sdbluexe) $(sdtluexe) $(sgbluexe) $(slltexe) \
	$(spblltexe) $(sptlltexe) $(sinvexe) $(sqrexe) $(slsexe)

double: $(dluexe) $(ddbluexe) $(ddtluexe) $(dgbluexe) $(dlltexe) \
	$(dpblltexe) $(dptlltexe) $(dinvexe) $(dqrexe) $(dlsexe)

complex: $(cluexe) $(cdbluexe) $(cdtluexe) $(cgbluexe) $(clltexe) \
	$(cpblltexe) $(cptlltexe) $(cinvexe) $(cqrexe) $(clsexe)

complex16: $(zluexe) $(zdbluexe) $(zdtluexe) $(zgbluexe) $(zlltexe) \
	$(zpblltexe) $(zptlltexe) $(zinvexe) $(zqrexe) $(zlsexe)

$(sluexe): $(slu) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(dluexe): $(dlu) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(cluexe): $(clu) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(zluexe): $(zlu) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(sdbluexe): $(sdblu) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(ddbluexe): $(ddblu) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(cdbluexe): $(cdblu) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(zdbluexe): $(zdblu) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(sdtluexe): $(sdtlu) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(ddtluexe): $(ddtlu) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(cdtluexe): $(cdtlu) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(zdtluexe): $(zdtlu) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(sgbluexe): $(sgblu) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(dgbluexe): $(dgblu) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(cgbluexe): $(cgblu) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(zgbluexe): $(zgblu) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(slltexe): $(sllt) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(dlltexe): $(dllt) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(clltexe): $(cllt) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(zlltexe): $(zllt) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(spblltexe): $(spbllt) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(dpblltexe): $(dpbllt) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(cpblltexe): $(cpbllt) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(zpblltexe): $(zpbllt) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(sptlltexe): $(sptllt) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(dptlltexe): $(dptllt) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(cptlltexe): $(cptllt) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(zptlltexe): $(zptllt) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(sinvexe): $(sinv) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(dinvexe): $(dinv) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(cinvexe): $(cinv) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(zinvexe): $(zinv) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(sqrexe): $(sqr) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(dqrexe): $(dqr) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(cqrexe): $(cqr) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(zqrexe): $(zqr) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(slsexe): $(sls) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(dlsexe): $(dls) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(clsexe): $(cls) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(zlsexe): $(zls) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)

clean: ; $(RMOBJ)

.f.o: ; $(F77) -c $(F77FLAGS) $<

