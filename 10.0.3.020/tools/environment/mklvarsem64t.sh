#! /bin/sh

export MKLROOT="/opt/intel/mkl/10.0.3.020"

if [ -z "${INCLUDE}" ]
then
    export INCLUDE="${MKLROOT}/include"
else
    export INCLUDE="${MKLROOT}/include:$INCLUDE"
fi

if [ -z "${LD_LIBRARY_PATH}" ]
then
    export LD_LIBRARY_PATH="${MKLROOT}/lib/em64t"
else
    export LD_LIBRARY_PATH="${MKLROOT}/lib/em64t:$LD_LIBRARY_PATH"
fi

if [ -z "${MANPATH}" ]
then
    export MANPATH="${MKLROOT}/man:$(manpath)"
else
    export MANPATH="${MKLROOT}/man:$MANPATH"
fi

if [ -z "${LIBRARY_PATH}" ]
then
    export LIBRARY_PATH="${MKLROOT}/lib/em64t"
else
    export LIBRARY_PATH="${MKLROOT}/lib/em64t:$LIBRARY_PATH"
fi

if [ -z "${CPATH}" ]
then
    export CPATH="${MKLROOT}/include"
else
    export CPATH="${MKLROOT}/include:$CPATH"
fi

if [ -z "${FPATH}" ]
then
    export FPATH="${MKLROOT}/include"
else
    export FPATH="${MKLROOT}/include:$FPATH"
fi
