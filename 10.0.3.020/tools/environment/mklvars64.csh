#! /bin/csh
    
set MKLROOT="/opt/intel/mkl/10.0.3.020"

if ($?INCLUDE) then
    setenv INCLUDE "${MKLROOT}/include:$INCLUDE"
else
    setenv INCLUDE "${MKLROOT}/include"
endif

if ($?LD_LIBRARY_PATH) then
    setenv LD_LIBRARY_PATH "${MKLROOT}/lib/64:$LD_LIBRARY_PATH"
else
    setenv LD_LIBRARY_PATH "${MKLROOT}/lib/64"
endif

if ($?MANPATH) then
    setenv MANPATH "${MKLROOT}/man:$MANPATH"
else
    setenv MANPATH "${MKLROOT}/man:`manpath`"
endif

if ($?LIBRARY_PATH) then
    setenv LIBRARY_PATH "${MKLROOT}/lib/64:$LIBRARY_PATH"
else
    setenv LIBRARY_PATH "${MKLROOT}/lib/64"
endif

if ($?CPATH) then
    setenv CPATH "${MKLROOT}/include:$CPATH"
else
    setenv CPATH "${MKLROOT}/include"
endif

if ($?FPATH) then
    setenv FPATH "${MKLROOT}/include:$FPATH"
else
    setenv FPATH "${MKLROOT}/include"
endif
