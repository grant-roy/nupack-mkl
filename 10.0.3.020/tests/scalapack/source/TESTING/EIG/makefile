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

shrdexe = ../xshrd$(IDE)
dhrdexe = ../xdhrd$(IDE)
chrdexe = ../xchrd$(IDE)
zhrdexe = ../xzhrd$(IDE)

strdexe = ../xstrd$(IDE)
dtrdexe = ../xdtrd$(IDE)
ctrdexe = ../xctrd$(IDE)
ztrdexe = ../xztrd$(IDE)

sbrdexe = ../xsbrd$(IDE)
dbrdexe = ../xdbrd$(IDE)
cbrdexe = ../xcbrd$(IDE)
zbrdexe = ../xzbrd$(IDE)


ssepexe = ../xssep$(IDE)
dsepexe = ../xdsep$(IDE)
csepexe = ../xcsep$(IDE)
zsepexe = ../xzsep$(IDE)

sgsepexe = ../xsgsep$(IDE)
dgsepexe = ../xdgsep$(IDE)
cgsepexe = ../xcgsep$(IDE)
zgsepexe = ../xzgsep$(IDE)

ssvdexe = ../xssvd$(IDE)
dsvdexe = ../xdsvd$(IDE)

snepexe = ../xsnep$(IDE)
dnepexe = ../xdnep$(IDE)
cnepexe = ../xcnep$(IDE)
znepexe = ../xznep$(IDE)

cevcexe = ../xcevc$(IDE)
zevcexe = ../xzevc$(IDE)

smatgen = psmatgen.o pmatgeninc.o
dmatgen = pdmatgen.o pmatgeninc.o
cmatgen = pcmatgen.o pmatgeninc.o
zmatgen = pzmatgen.o pmatgeninc.o

sbrd = psbrddriver.o psbrdinfo.o psgebdrv.o pslafchk.o $(smatgen)
dbrd = pdbrddriver.o pdbrdinfo.o pdgebdrv.o pdlafchk.o $(dmatgen)
cbrd = pcbrddriver.o pcbrdinfo.o pcgebdrv.o pclafchk.o $(cmatgen)
zbrd = pzbrddriver.o pzbrdinfo.o pzgebdrv.o pzlafchk.o $(zmatgen)

shrd = pshrddriver.o pshrdinfo.o psgehdrv.o pslafchk.o $(smatgen)
dhrd = pdhrddriver.o pdhrdinfo.o pdgehdrv.o pdlafchk.o $(dmatgen)
chrd = pchrddriver.o pchrdinfo.o pcgehdrv.o pclafchk.o $(cmatgen)
zhrd = pzhrddriver.o pzhrdinfo.o pzgehdrv.o pzlafchk.o $(zmatgen)

strd = pstrddriver.o psttrdtester.o pslatran.o pstrdinfo.o pssytdrv.o \
       pslafchk.o xpjlaenv.o $(smatgen)
dtrd = pdtrddriver.o pdttrdtester.o pdlatran.o pdtrdinfo.o pdsytdrv.o \
       pdlafchk.o xpjlaenv.o $(dmatgen)
ctrd = pctrddriver.o pcttrdtester.o pclatran.o pctrdinfo.o pchetdrv.o \
       pclafchk.o xpjlaenv.o $(cmatgen)
ztrd = pztrddriver.o pzttrdtester.o pzlatran.o pztrdinfo.o pzhetdrv.o \
       pzlafchk.o xpjlaenv.o $(zmatgen)

ssvd =  pssvddriver.o pslagge.o pssvdchk.o pssvdcmp.o pssvdtst.o $(smatgen)
dsvd =  pdsvddriver.o pdlagge.o pdsvdchk.o pdsvdcmp.o pdsvdtst.o $(dmatgen)

ssep =  psseptst.o pssepsubtst.o pssepchk.o pssepqtq.o pslagsy.o \
        pslatms.o  pslasizesep.o pslasizesyevx.o pssepdriver.o \
        pssepreq.o pssepinfo.o pslasizesyev.o \
        pssqpsubtst.o pslasizesqp.o pssdpsubtst.o $(smatgen)
dsep =  pdseptst.o pdsepsubtst.o pdsepchk.o pdsepqtq.o pdlagsy.o \
        pdlatms.o  pdlasizesep.o pdlasizesyevx.o pdsepdriver.o \
        pdsepreq.o pdsepinfo.o pdlasizesyev.o \
        pdsqpsubtst.o pdlasizesqp.o pdsdpsubtst.o $(dmatgen)
csep =  pcseptst.o pcsepsubtst.o pcsepchk.o pcsepqtq.o pclagsy.o \
        pclatms.o  pclasizesep.o pclasizeheevx.o pcsepdriver.o \
        pcsepreq.o pssepinfo.o pcsdpsubtst.o $(cmatgen)
zsep =  pzseptst.o pzsepsubtst.o pzsepchk.o pzsepqtq.o pzlagsy.o \
        pzlatms.o  pzlasizesep.o pzlasizeheevx.o pzsepdriver.o \
        pzsepreq.o pdsepinfo.o pzsdpsubtst.o $(zmatgen)

sgsep =  psgseptst.o psgsepsubtst.o psgsepchk.o pslagsy.o \
         pslatms.o pslasizesyevx.o pslasizegsep.o pslasizesep.o \
         psgsepdriver.o psgsepreq.o pssepinfo.o $(smatgen)
dgsep =  pdgseptst.o pdgsepsubtst.o pdgsepchk.o pdlagsy.o \
         pdlatms.o pdlasizesyevx.o pdlasizegsep.o pdlasizesep.o \
         pdgsepdriver.o pdgsepreq.o pdsepinfo.o $(dmatgen)
cgsep =  pcgseptst.o pcgsepsubtst.o pcgsepchk.o pclagsy.o \
         pclatms.o pclasizegsep.o pclasizeheevx.o pclasizesep.o \
         pcgsepdriver.o pcgsepreq.o pssepinfo.o $(cmatgen)
zgsep =  pzgseptst.o pzgsepsubtst.o pzgsepchk.o pzlagsy.o \
         pzlatms.o pzlasizegsep.o pzlasizeheevx.o pzlasizesep.o \
         pzgsepdriver.o pzgsepreq.o pdsepinfo.o $(zmatgen)

snep =  psnepdriver.o psnepinfo.o psnepfchk.o $(smatgen)
dnep =  pdnepdriver.o pdnepinfo.o pdnepfchk.o $(dmatgen)
cnep =  pcnepdriver.o pcnepinfo.o pcnepfchk.o $(cmatgen)
znep =  pznepdriver.o pznepinfo.o pznepfchk.o $(zmatgen)

cevc =  pcevcdriver.o pcevcinfo.o pcget22.o $(cmatgen)
zevc =  pzevcdriver.o pzevcinfo.o pzget22.o $(zmatgen)


all : single double complex complex16

single:    $(shrdexe) $(strdexe) $(sbrdexe) $(ssepexe) $(sgsepexe) \
           $(snepexe) $(ssvdexe)
double:    $(dhrdexe) $(dtrdexe) $(dbrdexe) $(dsepexe) $(dgsepexe) \
           $(dnepexe) $(dsvdexe)
complex:   $(chrdexe) $(ctrdexe) $(cbrdexe) $(csepexe) $(cgsepexe) \
           $(cnepexe) $(cevcexe)
complex16: $(zhrdexe) $(ztrdexe) $(zbrdexe) $(zsepexe) $(zgsepexe) \
           $(znepexe) $(zevcexe)

$(sbrdexe) : $(sbrd) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(dbrdexe) : $(dbrd) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(cbrdexe) : $(cbrd) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(zbrdexe) : $(zbrd) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(shrdexe) : $(shrd) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(dhrdexe) : $(dhrd) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(chrdexe) : $(chrd) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(zhrdexe) : $(zhrd) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(strdexe) : $(strd) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(dtrdexe) : $(dtrd) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(ctrdexe) : $(ctrd) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(ztrdexe) : $(ztrd) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(ssvdexe) : $(ssvd) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(dsvdexe) : $(dsvd) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(ssepexe) : $(ssep) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(dsepexe) : $(dsep) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(csepexe) : $(csep) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(zsepexe) : $(zsep) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(sgsepexe) : $(sgsep) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(dgsepexe) : $(dgsep) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(cgsepexe) : $(cgsep) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(zgsepexe) : $(zgsep) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(snepexe) : $(snep) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(dnepexe) : $(dnep) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(cnepexe) : $(cnep) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(znepexe) : $(znep) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(cevcexe) : $(cevc) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)
$(zevcexe) : $(zevc) ; $(F77LOADER) $(F77LOADOPTS) -o $@ $^ $(LIBS)

clean: ; $(RMOBJ)

.f.o: ; $(F77) -c $(F77FLAGS) $<
