#!/bin/bash
# Copyright 2005 - 2008, Intel Corporation, All Rights Reserved.
# This script uninstalls Intel(R) Performance Libraries and related products.




function ABS_PATH() {

    local path="$1"
    local cwd

    if [[ "${path:0:1}" != "/" ]]; then
        cwd=$( pwd ) || DIE "Cannot get name of current directory (?!)."
        if [[ "${cwd:-1:1}" == "/" ]]; then
            path="$cwd$path"
        else
            path="$cwd/$path"
        fi
    fi
    RS="$path"

} # function abs_path


function REL_TO_ABS(){
    if echo "$1" | egrep -e ";" -e "'" &>/dev/null; then
        return 1
    fi

    local rv=
    eval local path=$1
    local link_skip_mode="$2"
    local global_flag=
    local resolved_link=

    [ "x$link_skip_mode" == "x" ] && link_skip_mode=0

    local IS_VALID=`echo "$path" | grep "^/"`
    if [ -z "$IS_VALID" ]; then
        path=$(pwd)"/$path"
    fi    

    path=$(echo "$path" | sed 's/\/\/*/\//g')

    local last_dir="$(basename "$path")"
    local prev_dir=$(dirname "$path")
    if [ ! -d "$path" ]; then
        if [ ! -d "$prev_dir" ]; then
            REL_TO_ABS "$prev_dir"
            last_dir=$(echo "$last_dir" | sed 's/\/*$//')
	    RS="$RS/$last_dir"
        else
            last_dir=$(echo "$last_dir" | sed 's/\/*$//')
            GET_PATH_TO_DIRECTORY "$prev_dir"
            if [ "$RS" = "/" ]; then
                RS="$RS$last_dir"
            else
                RS="$RS/$last_dir"
            fi
            rv=0
        fi
    else
        GET_PATH_TO_DIRECTORY "$path"        
        rv=$?
    fi

    if [ "$rv" = "0" ] && [ "$link_skip_mode" == "0" ]; then
        while [ 0 ]; do
	    if [[ -L "$RS" ]]; then
		resolved_link=$(readlink "$RS")
		global_flag=$(echo "$resolved_link" | grep "^/" 2>/dev/null)
		if [ -z "$global_flag" ]; then
		    RS="$(dirname "$RS")/$resolved_link"
		else
		    RS="$resolved_link"
		fi
	    else
		break
	    fi
	done
        prev_dir=$(dirname "$RS")
        last_dir=$(basename "$RS")
        while [ "$prev_dir" != "/" ]; do
    	    if [[ -L "$prev_dir" ]]; then
		resolved_link=$(readlink "$prev_dir")
		global_flag=$(echo "$resolved_link" | grep "^/" 2>/dev/null)
		if [ -z "$global_flag" ]; then
		    prev_dir="$(dirname "$prev_dir")/$resolved_link"
		else
		    prev_dir="$resolved_link"
		fi
	    fi
            last_dir="$(basename "$prev_dir")/$last_dir"
            prev_dir=$(dirname "$prev_dir")
        done
        RS="$prev_dir$last_dir"
	REL_TO_ABS "$RS" 1
    fi
    return $rv
}



function GET_PATH_TO_DIRECTORY() {
    local path="$1"
    local prev_dir=
    local last_dir=
    (cd "$path" &>/dev/null)
     if [ "$?" = "0" ]; then
         path=$(cd "$path" &>/dev/null; pwd)
     else
         prev_dir=$(dirname "$path")
         last_dir=$(basename "$path")
         local flag=0;
         while [ "$flag" != "1" ]; do
             (cd "$prev_dir" &>/dev/null)
             [[ "$?" = "0" ]] && path="$prev_dir/$last_dir" && flag=1
             last_dir="$(basename "$prev_dir")/$last_dir"
             prev_dir=$(dirname "$prev_dir")
         done  
     fi
     if [ "$path" != "/" ]; then
         RS="$path"
         return 0
     else
         return 1
     fi
}





function RPM_INIT(){
	[ ${RPM_CHECK} -eq 0 ] || return ${ERR_OK}
	rpm -q rpm &> /dev/null

	
	if [ $? -ne 0 ] ; then
		LOG "Cannot get shared lock on RPM Database"
		return ${ERR_RPM_LOCK}
	fi

	RPM_CHECK=1
	
	return ${ERR_OK}
} # RPM_INIT(){   




function SPLIT() {

    local delimiter="$1"
    local string="$2"
    local ifs="$IFS"
    local -a result

    [[ $# -eq 2 ]] || CROAK "$FUNCNAME() expects 2 arguments."
    [[ "${#delimiter}" -eq 1 ]] || CROAK "$FUNCNAME(): The 1st argument must be a single char."

    IFS="$delimiter"; result=($string); IFS="$ifs"
    RA=( "${result[@]}" )

} # function split




function RPM_INFO(){
	local rpm_name=$1
	local rpm_tag=$2
	
	[ $RPM_CHECK -eq 1 ] || RPM_INIT ; local err=$?
	[ $err -eq ${ERR_OK} ] || return $err
	
	local opt="-q"
	if [ -f "$rpm_name" ]; then
		LOG "Using file query for package - '$rpm_name'"
		opt="-qp"
	fi #if[ -x $rpm_name ]; then
	
	local rpm_out_count=`rpm $opt $rpm_name 2>&1 | wc -l`
	if [ $rpm_out_count -eq 1 ] ; then 
		local info=`rpm $opt $rpm_name --qf %{$rpm_tag} 2>&1`
		if ! echo $info | grep installed &>/dev/null ; then
			echo $info
			LOG "Package search: '$rpm_name' => '$rpm_tag' => '$info' "
			return $ERR_OK;
		fi #if ! echo $info | grep installed &>/dv/null ; then
	else
		local out=
                local i=1
			local info=`rpm $opt $rpm_name --qf %{$rpm_tag}"\n" 2>&1 | sed $i'!d'`
			out="$out$info"
                echo $info
		LOG "Multiple package search: '$rpm_name' => '$rpm_tag' => '$out' "
		return ${ERR_OK}
	fi

	LOG "Package '$rpm_name' not found"
	
	return ${ERR_RPM_NO_PACKAGE}
} # RPM_INFO(){






function READ_DEFAULT_ANSWER(){	
	local param=$1
        local non_verbose=$2
	local retvalue=''
	LOG "silent:read_default_answer(): Trying to find the parameter '$param'"
	CONFIG_GET_VALUE $CONFIG_PREFIX $CONFIG_SECTION $param
	retvalue=$RS
	if [ "x$retvalue" == "x" ]; then
	    [[ "$non_verbose" = "1" ]] || WARN "Parameter \"$param\" was not found"
	    RS=''
	else
	    RS=$retvalue
	fi
} #read_default_answer(){


			                                                                                
function SET_DEFAULT_ANSWER(){	
	local question="$1"
	local value="$2"
	
	LOG "Set default answer for:"
	LOG_ARGS "$@"
	if IS_COMMAND_LINE_OPTION_EXISTS silent; then
	    LOG "Failed, silent mode does not support dynamic default answers."
	else
	    CONFIG_SET_VALUE $CONFIG_PREFIX $CONFIG_SECTION $question "$value"
	fi
} #read_default_answer(){



function SET_CONFIG_FILE(){
	if IS_COMMAND_LINE_OPTION_EXISTS silent; then
    	    CONFIG_FILENAME=`GET_COMMAND_LINE_OPTION_VALUE silent` || return 1
	    ABS_PATH $CONFIG_FILENAME
	    CONFIG_FILENAME=$RS
	    CONFIG_READ_FILE $CONFIG_PREFIX $CONFIG_FILENAME
	    LOG "CONFIG is set to '$CONFIG_FILENAME'"
	else
	    LOG "Silent mode was not enabled"
	fi	
} # set_config_file()



function KEEP_ANSWERS(){
	if IS_COMMAND_LINE_OPTION_EXISTS duplicate; then
    	    local dup_filename=`GET_COMMAND_LINE_OPTION_VALUE duplicate`
	    ABS_PATH $dup_filename
	    dup_filename=$RS
	    CONFIG_WRITE_FILE $CONFIG_PREFIX $dup_filename
	    LOG "Duplicate was performed into '$dup_filename'"
	else
	    LOG "Duplicate mode was not enabled"
	fi
}




function IS_ONE_OF() {

    [[ $# -ge 1 ]] || CROAK "$FUNCNAME() expects at least one argument."

    local item="$1"
    shift 1

    local iter
    for iter in "$@"; do
        if [[ "$item" == "$iter" ]]; then
            return 0
        fi
    done
    return 1

} # is_one_of






function GET_USER_INTERACTIVE_ANSWER() {
	local question_id=$1
    local question="$2"
    local USE_READLINE="$3"
	local l_answer=

	LOG "interact:get_user_interactive_answer(): getting answer on the question with id '$question_id'"
	if IS_COMMAND_LINE_OPTION_EXISTS silent ; then
		READ_DEFAULT_ANSWER $question_id
		l_answer=$RS
	else
    		if [ "x$USE_READLINE" != "xNO" ]; then
                    read -e -p "$question" l_answer
                else
                    read -p "$question" l_answer   
                fi
                if [ "${l_answer}x" == "x" ] ; then 	# user has pressed <Enter>
		    LOG "${0##*/}:$LINENO: interact:get_user_interactive_answer(): user entered empty answer"
		    LOG "${0##*/}:$LINENO: interact:get_user_interactive_answer(): using default answer"
		    READ_DEFAULT_ANSWER $question_id
		    l_answer=$RS
		fi
	fi
	LOG "interact:get_user_interactive_answer(): the answer is '$l_answer'"
	RS=$l_answer
} # get_user_interactive_answer



function READ_YES_NO_ANSWER ()
{
    local QUESTION=$1
    local question_id=$2
    local YES_ANSWER="y yes yea yeah yep ok"	# these answers are recognized as 'Yes'
    local NO_ANSWER="n no not nop nope"		# these answers are recognized as 'No'
    local DEFAULT_ANSWER=

    if ! IS_COMMAND_LINE_OPTION_EXISTS silent; then
		READ_DEFAULT_ANSWER $question_id
		DEFAULT_ANSWER=$RS
    fi

    while [ 1 -eq 1 ] ; do
        IS_COMMAND_LINE_OPTION_EXISTS silent || DISPLAY_QUESTION "" "$QUESTION" "Yes/No" "$DEFAULT_ANSWER"
        GET_USER_INTERACTIVE_ANSWER $question_id
        yesno="$(echo $RS | tr -s A-Z a-z)"
        if FIND_STRING $yesno ";" "$YES_ANSWER";then
	    LOG "interact: READ_YES_NO_ANSWER (): the answer is Yes"
            RS="yes"
            return 0
        else
            if FIND_STRING $yesno ";" "$NO_ANSWER";then
		LOG "interact: READ_YES_NO_ANSWER (): the answer is No"
                RS="no"
                return 1
            fi
	    if IS_COMMAND_LINE_OPTION_EXISTS silent;then
		LOG "interact: READ_YES_NO_ANSWER (): couldn't recognize the answer: config file parameter '$question_id' has wrong value '$answer'"
		return 2
	    fi
        fi
    done
}



function WAIT_ENTER_KEY ()
{
    IS_COMMAND_LINE_OPTION_EXISTS silent || read
}



function SAY() {
    local opts=""
    local log="log"
    while [[ "$1" == -[a-zA-Z] ]]; do
        case "$1" in
            -L ) log="";;
            *  ) opts="$opt $1";;
        esac
        shift 1
    done
    [[ "$log" != "" ]] && LOG_ARGS "$@"
    local message
    for message in "$@"; do
        echo $opts "$message"
    done
} # function say



function TELL() {
    LOG_ARGS "$@"
    set_tell_fmt
    if [ "x$_TELL_FMT_" != "x" ]; then
        echo "$*" | "$_TELL_FMT_" -s -w "$TELL_WIDTH"
    else
        echo "$*"
    fi
} # function tell

function set_tell_fmt() {
	[ "x$_SET_FMT_" == "xset" ] && return 0
	RUN which fmt &>/dev/null
	if [ $? == 0 ]; then 
		declare -r _TELL_FMT_="fmt" 
	else 
		declare -r _TELL_FMT_=
		WARN "Unable to find command \"fmt\". Some messages may be too long." 
	fi
	_SET_FMT_="set"
}



function ASK() {

    [[ $# -ge 2 && $# -le 4 ]] || CROAK "$FUNCNAME() expects from 2 to 4 arguments."
    LOG_ARGS "$@"

    local text="$1"
    local prompt="$2"      # Question itself.
    local options="$3"     # List of possible answers, delimited with '/'.
    local default="$4"     # Default answer to be used if user pressed Enter immediately.
    local answer

    SPLIT "/" "$options"
    local -a opt_array
    opt_arr=( "${RA[@]}" )

    if [[ "$options" != "" ]]; then
        prompt="$prompt ($options)"
    fi
    if [[ "$default" != "" ]]; then
        prompt="$prompt [$default]"
    fi

    if [[ "$text" != "" ]]; then
        SAY -L -e "$text"
    fi
    while :; do
        read -e -p "$prompt: " answer
        LOG "read: <$answer>."
        if [[ "x$answer" == "x" ]]; then
            answer="$default"
        fi
        if [[ "$options" == "" ]] || IS_ONE_OF "$answer" "${opt_arr[@]}"; then
            SAY -L ""
            RS="$answer"
            LOG "ret: <$answer>"
            return
        fi
        MAKE_LIST "${opt_arr[@]}"
        SAY "Please enter $RS."
    done

} # function ask



function DISPLAY_QUESTION(){
	
	local intro=$1
	local q=$2
	local da=$3
	local rda=$4
	
	IS_COMMAND_LINE_OPTION_EXISTS silent && return ${ERR_OK}
	LOG_ARGS $@

	[ "x$intro" != "x" ] && echo $intro
	echo -n "$q"
	[ ! -z "$da" ] && echo -n " ( $da )"
	[ ! -z "$rda" ] && echo -n " [ $rda ]"
	echo -n ": "
}



function FIND_STRING()
{
    local s=$1
    local a=
    while [ 1 == 1 ]; do
		shift
		[ "$1" == ";" ] && break
    done
	shift
    LOG "Checking '$s' against this list: $@"
    for a in $@
    do
        if [ "x$s" == "x$a" ];then
            LOG "Found"
            return ${ERR_OK}
        fi
    done
    LOG "Haven't found '$s'"
    return 1
}




function COMPARE_VERSION(){
	local A=$1
	local B=$2
        local COMPARE_RESULT=0
        local INDEX=1
        local CA="1"
        local CB="2"

        if [ $(echo $A | grep -v "\.") ] && [ $(echo $B | grep -v "\.") ]; then
		if [ "$A" -gt "$B" ] ; then
		        COMPARE_RESULT=1
		elif [ "$B" -gt "$A" ] ; then
		        COMPARE_RESULT=-1
	        fi # if [ "$A" -gt "$B" ] ; then
		return $COMPARE_RESULT
        fi
       


        while [ "$CA" != "" ] && [ "$CB" != "" ] ; do
		CA=$(echo $A | cut -d'.' -f${INDEX})
		CB=$(echo $B | cut -d'.' -f${INDEX})
		if [ "$CA" != "" ] && [ "$CB" = "" ] ; then
	    		COMPARE_RESULT=1
		elif [ "$CA" = "" ] && [ "$CB" != "" ] ; then
			COMPARE_RESULT=-1
		elif [ "$CA" != "" ] && [ "$CB" != "" ] ; then
	    		if [ "$CA" -gt "$CB" ] ; then
				COMPARE_RESULT=1
			elif [ "$CB" -gt "$CA" ] ; then
				COMPARE_RESULT=-1
	    		fi # if [ "$CA" -gt "$CB" ] ; then
	    		if [ $COMPARE_RESULT -ne 0 ] ; then
				break
	        	fi # if [ "$COMPARE_RESULT" -ne "0" ] ; then
		fi # if [ "$CA" != "" ] && [ "$CB" = "" ] ; then
		INDEX=$(($INDEX+1))
	done #while [ "$CA" != "" ] && [ "$CB" != "" ] ; do
        return $COMPARE_RESULT
	
} # COMPARE_VERSION(){


	



function JOIN() {


    local joiner="$1"
    shift 1

    JOIN3 "" "" "$joiner" "$@"

} # function join



function JOIN3() {


    local prefix="$1"
    local suffix="$2"
    local joiner="$3"
    shift 3

    local result=""
    if [[ $# -gt 0 ]]; then
        local first; local -a rest
        first="$prefix$1$suffix"; shift 1      # The first item processed separately.
        rest=( "$@" )                          # Copy all the items (but the first).
        rest=( "${rest[@]/#/$joiner$prefix}" ) # Prepend items with joiner and prefix.
        rest=( "${rest[@]/%/$suffix}" )        # Appemd items with suffix.
        local ifs="$IFS"
        IFS=""
        result="$first${rest[*]}"              # And join all the items together.
        IFS="$ifs"
    fi

    RS="$result"

} # function join3





function DIE() {
    local opts=""
    local log="log"
    local prefix="ERROR: "
    while [[ "$1" == -[a-zA-Z] ]]; do
        case "$1" in
            -L ) log="";;
            -P ) prefix="";;
            *  ) opts="$opt $1";;
        esac
        shift 1
    done
    [[ "$log" != "" ]] && LOG "warn:" "$@" "(end)"
    local message
    for message in "$@"; do
        echo $opts "$prefix$message" 1>&2
    done
    BYE 1
} # function die



function BYE() {

    local code="$1"

    [[ -z "$code" ]] && code="0"
    LOG "bye ($code)."

    exit "$code"

} # function bye

function _croak_() {

    local level=$(( $1 + 2 ))
    shift 1

    DIE "Internal error in function \"${FUNCNAME[$level]}\", file \"${BASH_SOURCE[$level]}\", line ${BASH_LINENO[$level-1]}:" \
        "$@" "Please report."

} # function _croak_

function croak0() {

    _croak_ 0 "$@"

} # function croak0



function CROAK() {

    _croak_ 1 "$@"

} # function croak



function INIT_LOG() {

    if [ "x$LOG_F" == "x" ]; then
	local param=$1
	if [ "x$param" == "x" ]; then
	    WARN -L "LOG file was not specified. Logging will be disabled."
	else
	    LOG_F=$param
	fi
    fi
    
    [ -e "$LOG_F" ] || RUN :>"$LOG_F" 
    echo "-+ Logging in shell wrapper is started with LOG=$LOG_F +-" >> "$LOG_F"
    if [ $? != 0 ]; then
	WARN -L "Can not write log file \"$LOG_F\". Logging will be disabled."
	unset LOG_F
    fi

} # function init_log



function LOG() {

    if [[ -n "$LOG_F" ]]; then
        local time_stamp=$( date +'%a %b %d %T' )
        local line
        for line in "$@"; do
            { echo "$time_stamp: $line" >> "$LOG_F"; } 2> /dev/null
            if [[ $? -ne 0 ]]; then
                WARN -L "Can not write log file \"$LOG_F\"."
                unset LOG_F  # Unset LOG variable not to log any messages any more.
                break
            fi
        done
    fi

} # function log



function LOG_ARGS() {

    local func="${FUNCNAME[1]}"

    JOIN3 "<" ">" ", " "$@"
    if [[ "$RS" == "" ]]; then
        LOG "$func."
    else
        LOG "$func: $RS."
    fi

} # function log_args



function LOG_VARS() {

    local name str
    local -a vars=()
    for name in "$@"; do
        vars["${#vars[@]}"]="$name=\"${!name}\""
    done
    JOIN ", " "${vars[@]}"
    LOG "vars: $RS."

} # function log_vars



function LOG_FILE() {

    local file="$1"
    local line

    LOG \
        "file <$file>:" \
        "+-------------------------------------------------------------------------------"
    if [[ -f "$file" ]]; then
        while read line; do
            LOG "| $line"
        done < "$file"
    else
        LOG "file not found"
    fi
    LOG "+-------------------------------------------------------------------------------"

} # function log_file



function WARN() {
    local opts=""
    local log="log"
    local prefix="WARNING: "
    while [[ "$1" == -[a-zA-Z] ]]; do
        case "$1" in
            -L ) log="";;
            -P ) prefix="";;
            *  ) opts="$opt $1";;
        esac
        shift 1
    done
    [[ "$log" != "" ]] && LOG_ARGS "$@"
    local message
    for message in "$@"; do
        echo $opts "$prefix$message" 1>&2
    done
} # function warn






function SAVE_COMMAND_LINE(){
	CMD_STR=$@
	declare -r CMD_STR   
} #SAVE_COMMAND_LINE


			                                                                                
function IS_COMMAND_LINE_OPTION_EXISTS(){
                                            
	local cmd=$1                                    
	if echo $CMD_STR | egrep -i "[[:space:]]*\-\-$cmd" &> /dev/null ; then
		return ${ERR_OK}
	fi
	
	return 1
} # is_command_line_option_exists()



function GET_COMMAND_LINE_OPTION_VALUE(){
	
	local cmd=$1
	local err=0;
	
	IS_COMMAND_LINE_OPTION_EXISTS $cmd
	err=$?
	
	[ $err -eq ${ERR_OK} ] || return 1
		
        RS=$(echo $CMD_STR | sed s/.*--$cmd[[:blank:]]*//g | sed 's/[[:blank:]]*--.*$//g')
        [[ -z "$RS" ]] && return 1
        echo $RS
	
} # get_command_line_option_value() {





function PKG_CHECK(){
	local package=$1
	local pack_ver=$2
	local pack_ver_rel=$3
	local pack='';
	local err=$ERR_OK;
	
	local equal=`echo $pack_ver_rel | sed s/[^e]*e[^e]*//g`
	local greater=`echo $pack_ver_rel | sed s/[^g]*g[^g]*//g`
	local less=`echo $pack_ver_rel | sed s/[^l]*l[^l]*//g`

	local seq=1
	local sgt=1
	local slt=1

	if [ "$pack_ver_rel" = ne ] ; then
		pack=`RPM_INFO $package name`
		err=$?
			
		if [ $err -eq ${ERR_RPM_NO_PACKAGE} ]  ; then 
			return ${ERR_OK}; 
		elif [ $err -ne ${ERR_OK} ] ; then
			return $err;
		fi # if [ $err -eq ${ERR_RPM_NO_PACKAGE} ]  ; then
			
		COMPARE_VERSION `RPM_INFO $package version` $pack_ver
			
		if [ $? -ne 0 ] ; then 
			return ${ERR_OK}; 
		fi # if [ $? -ne 0 ] ; then
		
	fi # if [ "$pack_ver_rel" = ne ] ; then

	if [ "$pack_ver_rel" = eq ] || [ "$pack_ver_rel" = ge ] ||[ "$pack_ver_rel" = lt ] ||[ "$pack_ver_rel" = le ] ||[ "$pack_ver_rel" = gt ] ; then 
		pack=`RPM_INFO $package name`; 
		[ $? -eq ${ERR_OK} ] || err=1; 
		if [ $err -eq ${ERR_OK} ] ; then 
			COMPARE_VERSION `RPM_INFO $package version` $pack_ver
			local error=$?

			if test $equal"x" = "x" ; then
				[ $error -eq 0 ] && seq=0 
			fi # if test $equal"x" = "x" ; then
			if test $greater"x" = "x" ; then
				[ $error -eq 1 ] && sgt=0
			fi # if test $greater"x" = "x" ; then
			if [ $less"x" = "x" ] ; then
				[ $error -eq 255 ] && slt=0
			fi # if [ $less"x" = "x" ] ; then
		fi # if [ $err -eq ${ERR_OK} ] ; then
	fi # if [ "$pack_ver_rel" = eq ] || [ "$pack_ver_rel" = ge...
	

	if [ $err -eq ${ERR_OK} ]; then 
		err=1
		case $pack_ver_rel in
			eq ) [ $seq -ne 0 ] || err=0 ;;
			ge ) [ $seq -ne 0 ] || err=0 ; [ $sgt -eq 1 ] || err=0 ;;
			le ) [ $seq -ne 0 ] || err=0 ; [ $slt -eq 1 ] || err=0 ;;
			gt ) [ $seq -eq 1 ] && [ $sgt -eq 0 ] && err=0 ;;
			lt ) [ $seq -eq 1 ] && [ $slt -eq 0 ] && err=0 ;;
		esac # if [ $err -eq ${ERR_OK} ]; then
	fi # if [ $err -eq ${ERR_OK} ]; then

	return $err

} # PKG_CHECK(){





function ARC_GET(){
	arch_tool="uname -m"
	if $($arch_tool | egrep 'i.86' > /dev/null) ; then
		IA='IA32';
		PLATFORM=x32;
	elif $($arch_tool | grep ia64 > /dev/null) ; then 
		IA='IPF'
		PLATFORM=x64
	elif $($arch_tool | grep x86_64 > /dev/null) ; then
		IA='EM64T'
		PLATFORM=x32_64
	else
		LOG "Unknown arch found: $(uname -m)"
		return ${ERR_UNKNOWN_ARC}
	fi # if [ arch | egrep 'i?86' > /dev/null ] ; then
	return ${ERR_OK}
} # ARC_GET(){





declare INTEL_SDP_PRODUCTS_DB=intel_sdp_products.db # TBD...
declare INTEL_SDP_PRODUCTS_DB_LOCAL=''
declare NONRPM_DB_MODE=''
declare NONRPM_DB_PREFIX="$HOME/intel"

function NONRPM_SET_DB_MODE(){
    [ "x$NONRPM_DB_MODE" == "x" ] || return 0
    LOG "NONRPM_SET_DB_MODE"

    INTEL_SDP_PRODUCTS_DB="$NONRPM_DB_PREFIX/$INTEL_SDP_PRODUCTS_DB"
    LOG "INTEL_SDP_PRODUCTS_DB set to $INTEL_SDP_PRODUCTS_DB"

    if IS_COMMAND_LINE_OPTION_EXISTS use-new-db; then
	NONRPM_DB_MODE="both"
    elif IS_COMMAND_LINE_OPTION_EXISTS ignore-old-db; then
	NONRPM_DB_MODE="new"
    else
	NONRPM_DB_MODE="old"
    fi
    LOG "NONRPM_DB_MODE set to $NONRPM_DB_MODE"
    
    if IS_COMMAND_LINE_OPTION_EXISTS nonrpm; then 
	if [ -e "$NONRPM_DB_PREFIX" ]; then
	    if [ ! -d "$NONRPM_DB_PREFIX" ]; then
		LOG "$NONRPM_DB_PREFIX exists and this is not a directory"
		DIE "$NONRPM_DB_PREFIX exists and this is not a directory"
	    fi
	else
	    mkdir -p "$NONRPM_DB_PREFIX" &>/dev/null
	    if [ "$?" != "0" ]; then
		LOG "Unable to create a directory $NONRPM_DB_PREFIX"
		DIE "Unable to create a directory $NONRPM_DB_PREFIX"
	    fi
	fi  
    fi
    
    if [ "$NONRPM_DB_MODE" != "old" ]; then
	LOG "Using INTEL_SDP_PRODUCTS_DB_LOCAL="
	if [ "x$INSTALL_HOST_ID" != "x" ]; then
	    LOG "via INSTALL_HOST_ID var"
	    INTEL_SDP_PRODUCTS_DB_LOCAL="$NONRPM_DB_PREFIX/intel_sdp_products_$INSTALL_HOST_ID.db"
	else
	    LOG "via <hostname>"
	    RS=$(hostname)
	    if [ "$?" != "0" ]; then
		WARN "Unable to define host name, 'only-old-db' mode will be used for Non-rpm functionality."
		INTEL_SDP_PRODUCTS_DB_LOCAL=''
		NONRPM_DB_MODE="old"
	    else
		INTEL_SDP_PRODUCTS_DB_LOCAL="$NONRPM_DB_PREFIX/intel_sdp_products_$RS.db"
	    fi
	fi
	LOG "INTEL_SDP_PRODUCTS_DB_LOCAL set to $INTEL_SDP_PRODUCTS_DB_LOCAL"
	if IS_COMMAND_LINE_OPTION_EXISTS nonrpm; then
	    [ -e "$INTEL_SDP_PRODUCTS_DB_LOCAL" ] || RUN echo -n "" 2>/dev/null 1>$INTEL_SDP_PRODUCTS_DB_LOCAL
	fi
    fi
    
    if IS_COMMAND_LINE_OPTION_EXISTS nonrpm; then
	if [ "$NONRPM_DB_MODE" == "old" ] || [ "$NONRPM_DB_MODE" == "both" ]; then 
	    [ -e "$INTEL_SDP_PRODUCTS_DB" ] || RUN echo -n "" 2>/dev/null 1>$INTEL_SDP_PRODUCTS_DB
	fi
    fi
    
    return 0
}



function NONRPM_GET_BUFFER(){
    
    LOG "NONRPM_GET_BUFFER"
    NONRPM_SET_DB_MODE
    
    local result=''
    local rst=1
    case $NONRPM_DB_MODE in
    new)
	LOG "case new"
	result=$(cat "$INTEL_SDP_PRODUCTS_DB_LOCAL" 2>/dev/null)
	rst=$?
	;;
    both)
	LOG "case both"
	result=$(cat "$INTEL_SDP_PRODUCTS_DB" "$INTEL_SDP_PRODUCTS_DB_LOCAL" 2>/dev/null | sort | uniq)
	rst=$?
	;;
    *)
	LOG "case any"
	result=$(cat "$INTEL_SDP_PRODUCTS_DB" 2>/dev/null)
	rst=$?
	;;
    esac

    if IS_COMMAND_LINE_OPTION_EXISTS nonrpm; then
	[ "$rst" == "0" ] || return 1
    fi
    
    LOG "result:"
    LOG "$result"

    RS=$result
    return 0
}



function NONRPM_DB_ENTRY_CHECK_SYNTAX() {
    local entry="$1"
    local regexp='<:[^:]*:intel-[a-z_0-9-]+-[0-9]+(\.[0-9]+)+([abpet]u?)?-[0-9]+\.?[a-z_0-9]+\.rpm:[^:]*:[^:]*:>'
    return $(echo "$entry" | grep -E -x "$regexp" > /dev/null) \
        || FATAL_EXIT "Unexpected error."
} # NONRPM_DB_ENTRY_CHECK_SYNTAX



function NONRPM_DB_ENTRY_GET_FIELD() {
    local entry="$1"
    local field="$2"
    RS=''
	if ! NONRPM_DB_ENTRY_CHECK_SYNTAX "$entry"; then
        return 1
    fi
    if [ "$field" -lt 2 ] || [ "$field" -gt 5 ]; then
        return 2
    fi
    local result=$(echo "$entry" | cut -d':' -f"$field") \
        || FATAL_EXIT "Unexpected error"
    RS=$result
    return 0
} # NONRPM_DB_ENTRY_GET_FIELD


function NONRPM_DB_ENTRY_GET_RPMNAME() {
    NONRPM_DB_ENTRY_GET_FIELD "$1" 2
    return $?
} # NONRPM_DB_ENTRY_GET_RPMNAME

function NONRPM_DB_ENTRY_GET_RPMFILE() {
    NONRPM_DB_ENTRY_GET_FIELD "$1" 3
    return $?
} # NONRPM_DB_ENTRY_GET_RPMFILE

function NONRPM_DB_ENTRY_GET_INSTALLDIR() {
    NONRPM_DB_ENTRY_GET_FIELD "$1" 4
    return $?
} # NONRPM_DB_ENTRY_GET_INSTALLDIR

function NONRPM_DB_ENTRY_GET_LOGFILE() {
    NONRPM_DB_ENTRY_GET_FIELD "$1" 5
    return $?
} # NONRPM_DB_ENTRY_GET_LOGFILE



function NONRPM_DB_ENTRY_ADD() {

    LOG "NONRPM_DB_ENTRY_ADD"
    LOG_ARGS $@
    local entry="$1"

    NONRPM_SET_DB_MODE

    if ! NONRPM_DB_ENTRY_CHECK_SYNTAX "$entry"; then
        return 1
    fi

    local db_content=
    if [ "$NONRPM_DB_MODE" == "old" ] || [ "$NONRPM_DB_MODE" == "both" ]; then
	db_content=$(cat "$INTEL_SDP_PRODUCTS_DB" 2>/dev/null)
	echo "$entry" > "$INTEL_SDP_PRODUCTS_DB"
	[ "x$db_content" == "x" ] || echo "$db_content" >> "$INTEL_SDP_PRODUCTS_DB"
	if [ $? -ne 0 ]; then
	    return 2
	fi
    fi

    if [ "$NONRPM_DB_MODE" == "new" ] || [ "$NONRPM_DB_MODE" == "both" ]; then
	db_content=$(cat "$INTEL_SDP_PRODUCTS_DB_LOCAL" 2>/dev/null)
	echo "$entry" > "$INTEL_SDP_PRODUCTS_DB_LOCAL"
	[ "x$db_content" == "x" ] || echo "$db_content" >> "$INTEL_SDP_PRODUCTS_DB_LOCAL"
	if [ $? -ne 0 ]; then
	    return 2
	fi
    fi
    
    return 0
    
} # NONRPM_DB_ENTRY_ADD



function NONRPM_DB_ENTRY_REMOVE() {

    LOG "NONRPM_DB_ENTRY_REMOVE"
    LOG_ARGS $@
    local entry="$1"

    NONRPM_SET_DB_MODE

    if ! NONRPM_DB_ENTRY_CHECK_SYNTAX "$entry"; then
        return 1
    fi

    if [ "$NONRPM_DB_MODE" == "old" ] || [ "$NONRPM_DB_MODE" == "both" ]; then
	cp -p "$INTEL_SDP_PRODUCTS_DB" "$INTEL_SDP_PRODUCTS_DB~" \
	    || DIE "Unable to create backup copy of \"$INTEL_SDP_PRODUCTS_DB\" file."
	grep -F -v -x "$entry" "$INTEL_SDP_PRODUCTS_DB~" > "$INTEL_SDP_PRODUCTS_DB"
	local rc=$?
	[ $rc -le 1 ] || DIE "Unable to overwrite \"$INTEL_SDP_PRODUCTS_DB\" file."
	chmod --reference="$INTEL_SDP_PRODUCTS_DB~" "$INTEL_SDP_PRODUCTS_DB" \
	    || DIE "Unable to change permissions on \"$INTEL_SDP_PRODUCTS_DB\" file."
	rm -f "$INTEL_SDP_PRODUCTS_DB~" &>/dev/null
    fi
    
    if [ "$NONRPM_DB_MODE" == "new" ] || [ "$NONRPM_DB_MODE" == "both" ]; then
	cp -p "$INTEL_SDP_PRODUCTS_DB_LOCAL" "$INTEL_SDP_PRODUCTS_DB_LOCAL~" \
	    || DIE "Unable to create backup copy of \"$INTEL_SDP_PRODUCTS_DB_LOCAL\" file."
	grep -F -v -x "$entry" "$INTEL_SDP_PRODUCTS_DB_LOCAL~" > "$INTEL_SDP_PRODUCTS_DB_LOCAL"
	local rc=$?
	[ $rc -le 1 ] || DIE "Unable to overwrite \"$INTEL_SDP_PRODUCTS_DB_LOCAL\" file."
	chmod --reference="$INTEL_SDP_PRODUCTS_DB_LOCAL~" "$INTEL_SDP_PRODUCTS_DB_LOCAL" \
	    || DIE "Unable to change permissions on \"$INTEL_SDP_PRODUCTS_DB_LOCAL\" file."
	rm -f "$INTEL_SDP_PRODUCTS_DB_LOCAL~" &>/dev/null
    fi
    
    return 0

} # NONRPM_DB_ENTRY_REMOVE



function NONRPM_DB_IS_PACKAGE_INSTALLED() {

    LOG "NONRPM_DB_FIND_FILE_OWNER"
    LOG_ARGS $@
    local package="$1"
    local rc

    NONRPM_GET_BUFFER
    if [ "$?" != "0" ]; then
	DIE "Unable to obtain non-rpm DB content"
    fi

    echo $RS | grep ":$package" &>/dev/null
    rc=$?
    if [ $rc -ge 2 ]; then
        DIE "Unexpected error."
	RS=''
    fi

    RS=$rc
    return $rc
} # NONRPM_DB_IS_PACKAGE_INSTALLED



function NONRPM_DB_FIND_FILE_OWNER() {

    LOG "NONRPM_DB_FIND_FILE_OWNER"
    LOG_ARGS $@
    local file="$1"
    local entry
    local log_file
    local owner

    NONRPM_GET_BUFFER
    if [ "$?" != "0" ]; then
	DIE "Unable to obtain non-rpm DB content"
    fi

    local buffer=$RS
    for entry in $buffer; do
	NONRPM_DB_ENTRY_GET_LOGFILE "$entry"
        log_file=$RS
        if [ $? -eq 0 ] && [ -f "$log_file" ]; then
            owner=$(grep -F -x -l "$file" "$log_file")
            if [ $? -ge 2 ]; then
                DIE "Unexpected error."
            fi
            if [ -n "$owner" ]; then
                RS=$entry
                return 0
            fi
        fi
    done
    
    RS=''
    return 1

} # NONRPM_DB_FIND_FILE_OWNER


function NONRPM_DB_ENTRY_FIND_BY_RPMNAME() {

    LOG "NONRPM_DB_ENTRY_FIND_BY_RPMNAME"
    LOG_ARGS $@
    local rpmname=$(basename "$1")
    local entry
    local r_entries=

    NONRPM_GET_BUFFER
    if [ "$?" != "0" ]; then
	DIE "Unable to obtain non-rpm DB content"
    fi

    local buffer=$RS
    for entry in $buffer; do
	NONRPM_DB_ENTRY_GET_RPMNAME "$entry"
        if [ "$rpmname" == "$RS" ]; then
	    LOG "Entry $entry found"
            RS="$entry"
	    return 0
        fi
    done
    RS=''
    return 1

} # NONRPM_DB_ENTRY_FIND_BY_RPMNAME



function NONRPM_DB_ENTRY_FIND_BY_RPMFILE() {

    LOG "NONRPM_DB_ENTRY_FIND_BY_RPMFILE"
    LOG_ARGS $@
    local rpmfile=$(basename "$1")
    local entry
    local r_entries=

    NONRPM_GET_BUFFER
    if [ "$?" != "0" ]; then
	DIE "Unable to obtain non-rpm DB content"
    fi

    local buffer=$RS
    for entry in $buffer; do
	NONRPM_DB_ENTRY_GET_RPMFILE "$entry"
        if [ "$rpmfile" == "$RS" ]; then
	    LOG "Entry $entry found"
            r_entries="$r_entries $entry"
        fi
    done
    RS=$r_entries
    return 0

} # NONRPM_DB_ENTRY_FIND_BY_RPMFILE



function NONRPM_DB_FIND_BY_INSTALLDIR() {

    LOG "NONRPM_DB_FIND_BY_INSTALLDIR"
    LOG_ARGS $@
    local installdir="$1"
    local entry

    NONRPM_GET_BUFFER
    if [ "$?" != "0" ]; then
	DIE "Unable to obtain non-rpm DB content"
    fi

    local buffer=$RS
    for entry in $buffer; do
	NONRPM_DB_ENTRY_GET_INSTALLDIR "$entry"
        if [ "$installdir" == "$RS" ]; then
	    LOG "Entry $entry found"
            RS=$entry
            return 0
        fi
    done
    
    RS=''
    return 1

} # NONRPM_DB_FIND_BY_INSTALLDIR



function NONRPM_DB_CHECK_SHARED_FILES() {
    local install_dir="$1"
    local log_file="$2"

    local entry
    local line
    local shared=""

    NONRPM_GET_BUFFER
    if [ "$?" != "0" ]; then
	DIE "Unable to obtain non-rpm DB content"
    fi

    local buffer=$RS
    for entry in $buffer; do
	NONRPM_DB_ENTRY_GET_INSTALLDIR "$entry"
	if [ "$install_dir" == "$RS" ]; then
	    NONRPM_DB_ENTRY_GET_LOGFILE "$entry"
	    [ "x$RS" == "x$log_file" ] || shared="$RS $shared"
	fi
    done
    
    local rt
    tac "$log_file" | \
    while read line; do
	rt=1
	if [ "x$shared" != "x" ]; then
	    for i in "$shared"; do
		$(cat $i | grep $line &>/dev/null)
		rt=$?
		[ $rt == 0 ] && break
	    done
	fi
	if [ $rt == 0 ]; then
	    LOG "Shared violation $line"
	    continue
	fi 
        if [[ -h "$line" || -f "$line" ]]; then
            rm -f "$line"
        elif [ -d "$line" ]; then
            rmdir --ignore-fail-on-non-empty "$line"
        else
            echo "Warning: installed \"$line\" file not found"
        fi	
    done
}



function NONRPM_DB_GET_RPMBASED_FIELD(){
	local entry=""$1
	local field=""$2

	LOG "NONRPM_DB_GET_RPMBASED_FIELD:"
	LOG_ARGS $@
	NONRPM_DB_ENTRY_GET_FIELD $entry "2"
	local rpm_name=$RS
	NONRPM_DB_ENTRY_GET_INSTALLDIR "$entry"
	local dir=$RS
	
	LOG "Try to extract $dir/.scripts/$field.$rpm_name"
	if [ -f "$dir/.scripts/$field.$rpm_name" ]; then
		LOG_FILE "$dir/.scripts/$field.$rpm_name"
		RS=$(cat "$dir/.scripts/$field.$rpm_name")
		return 0 
	else
		RS=''
		LOG "$field does not exist in script dir"
	fi
	return 1
}




function CONFIG_CLEAR() {

    local file="$1"

    eval "unset \${!$prefix__*}"  # Unset all the variables starting with prefix.

} # function config_clear



function CONFIG_READ_FILE() {

    LOG_ARGS "$@"
    local clear="yes"

    while [[ "$1" == -[a-zA-Z] ]]; do
        case "$1" in
            -C ) clear="no";;
            *  ) CROAK "$FUNCNAME(): illegal option: $1";;
        esac
        shift 1
    done

    [[ $# -eq 2 ]] || CROAK "$FUNCNAME() expects 2 arguments"

    local prefix="$1"
    local file="$2"

    [[ -e "$file" ]] || DIE "File \"$file\" does not exist."
    [[ -f "$file" ]] || DIE "\"$file\" is not a file."
    [[ -r "$file" ]] || DIE "File \"$file\" is not readable."

    [[ "$clear" == "yes" ]] && CONFIG_CLEAR "$prefix"

    local i=0
    local section
    local name
    local value
    while read line; do
        i=$(( $i + 1 ))
        if [[ "$line" == \#* ]]; then
            : # Skip comments
        elif [[ "$line" == \[*\] ]]; then
            section="$line"
            section="${section#[}"  # Strip opening bracket.
            section="${section%]}"  # Strip closing bracket.
        elif [[ "$line" == *=* ]]; then
            name="${line%%=*}"      # Strip value.
            value="${line#*=}"      # Strip name.
            eval "${prefix}__${section}__${name}=\"\$value\""
        elif [[ "$line" == "" ]]; then
            : # Ignore empty lines.
        else
            DIE "Error in config file \"$file\" at line $i."
        fi
    done < "$file"

} # function config_read_file



function CONFIG_GET_VALUE() {

    local prefix="$1"
    local section="$2"
    local name="$3"

    eval "RS=\"\${${prefix}__${section}__${name}}\""

} # function config_get_value



function CONFIG_SET_VALUE() {

    local prefix="$1"
    local section="$2"
    local name="$3"
    local value="$4"

    eval "${prefix}__${section}__${name}=\"$value\""

} # function config_set_value



function CONFIG_GET_NAMES() {

    local prefix="$1"
    local section="$2"

    eval "RA=( \${!${prefix}__${section}__*} )"
    RA=( "${RA[@]#${prefix}__${section}__}" )   # Strip prefix and section names.

} # function config_get_names



function CONFIG_GET_SECTIONS() {

    local prefix="$1"

    local -a sections

    local -a vars
    eval "vars=( \${!${prefix}__*} )"
    vars=( "${vars[@]#${prefix}__}" )   # Strip prefix.
    vars=( "${vars[@]%%__*}" )          # Strip names.

    local var
    sections=()
    for var in "${vars[@]}"; do
        if ! IS_ONE_OF "$var" "${sections[@]}"; then
	    sections[${#sections[@]}]="$var"
        fi
    done

    RA=( "${sections[@]}" )

} # function config_get_sections



function CONFIG_WRITE_FILE() {

    local prefix="$1"
    local file="$2"

    [[ -e "$file" ]] || $(echo "" 2>/dev/null 1>$file) || DIE "Cannot create duplicate file \"$file\"."
    [[ -f "$file" ]] || DIE "Duplicate path \"$file\" is not a file."
    [[ -w "$file" ]] || DIE "Duplicate file \"$file\" is not writable."

    {
        local -a sections names
        local section name value
        CONFIG_GET_SECTIONS "$prefix"; sections=( "${RA[@]}" )

        for section in "${sections[@]}"; do
            echo "[$section]"
            CONFIG_GET_NAMES "$prefix" "$section"; names=( "${RA[@]}" )
            for name in "${names[@]}"; do
                CONFIG_GET_VALUE "$prefix" "$section" "$name"; value="$RS"
                echo "$name=$value"
            done
            echo ""
        done
    } > "$file"

} # function config_write_file




function RUN() {

    [[ $# -ge 1 ]] || CROAK "$FUNCNAME() expects at least one argument."

    local rc
    LOG_ARGS "$@"
    "$@"
    rc=$?
    LOG "ret: $rc."
    return $rc

} # function run




function TOUCH_SPACE(){
	
	local dir_to_check=$1
	local dir_end_path='hags7823782318#@123kjhknmnzxmnz'
	local err=

        [[ -L "$dir_to_check" ]] && dir_to_check=$(readlink "$dir_to_check")

	if [ -e "$dir_to_check" ] ; then
	    if [ -d "$dir_to_check" ]; then
		if [ -w "$dir_to_check" ] ; then
			RUN mkdir "$dir_to_check/$dir_end_path" &> /dev/null
			err=$?
			if [ $err -eq ${ERR_OK} ] ; then
				RUN rmdir "$dir_to_check/$dir_end_path" &> /dev/null
			fi # if [ $err -eq ${ERR_OK} ] ; then
		else
			err=1
		fi # if [ -w "$dir_to_check" ] ; then
	    else
		err=1
	    fi
	else
		TOUCH_SPACE "`dirname "$dir_to_check"`" "$dir_end_path"
		err=$?
	fi # if [ -d "$dir_to_check" ] ; then

	LOG "Access for write $dir_to_check/$dir_end_path, exit code: $err"
	
	return $err
} #TOUCH_SPACE(){



function CHECK_FREE_SPACE(){
    local path="$1"
    local needed_space="$2"	# Disc space in MegaBytes, should be integer
    
    LOG "CHECK_FREE_SPACE:"
    LOG_ARGS "$@"
    
    local base=$path

    while [ ! -d "$base" ] ; do
        base=$(dirname "$base")
    done
        
    local available=0
    available=$(df -Pm "$base" 2>/dev/null | tail -n1 | tr -s [:space:] | cut -d' ' -f4)
    RS=$?

    [ "$RS" == "0" ] || return 1
    
    LOG "Available disc space on this mount is $available"
    
    if [ "$needed_space" -le "$available" ]; then
	return 0
    else 
	LOG "Trere is no enough space on this mount"
        return 1
    fi
}



function CHECK_DESTINATION(){
	local path="$1"
	local needed_space="$2"
    local verbose=$3
	local lrs=1
	local owner=
	local summary=
	local syntax_check=

	LOG "CHECK_DESTINATION"
	LOG_ARGS "$@"

	if echo "$path" | grep " " &> /dev/null ; then
		WARN "Incorrect directory name. Directory name can't contain spaces."
		RS="$path"
		return 1
	fi
	if echo "$path" | grep '\\' &> /dev/null ; then
		WARN "Incorrect directory name. Directory name can't contain '\\' symbols."
		RS="$path"
		return 1
	fi
	if echo "$path" | grep "	" &> /dev/null ; then
		WARN "Incorrect directory name. Directory name can't contain tabs."
		RS="$path"
		return 1
	fi


	syntax_check=$(echo "$path" | sed -e"s/[0-9]//g" | sed -e"s/[a-z]//g" | sed -e"s/[A-Z]//g" | sed -e"s/\///g" | sed -e"s/\.//g" | sed -e"s/-//g" | sed -e"s/_//g" | sed -e"s/~//g" | sed -e"s/\\$//g" 2>/dev/null)
	if [ "x$syntax_check" != "x" ] ; then
		WARN "Incorrect directory name. You can use only [a-z], [A-Z], [0-9], '-', '_' and '.' symbols."
		RS="$path"
		return 1
	fi

	REL_TO_ABS "$path"
	if [ $? -ne 0 ]; then
	    WARN "Incorrect directory name."
	    RS="$path"
	    return 1
	fi
	path="$RS"

	local check=$(echo "$path" | grep "^/")
	if [ -z "$check" ]; then
		WARN "Incorrect directory name."
		return 1
	fi

    if [ -f "$path" ]; then
        WARN "Incorrect directory name. '$path' exists but it is not a directory."
	    RS="$path"
        return 1
    fi

	TOUCH_SPACE "$path"
	lrs=$?
	
	if [ $lrs != 0 ]; then
	    WARN "Installation has no permissions to write to destination directory."
	    RS="$path"
	    return 1
	fi

    if [ -n "$needed_space" ]; then
	    CHECK_FREE_SPACE "$path" $needed_space
	    lrs=$?
	
	    if [ $lrs != 0 ]; then
	        WARN "Specified filesystem has no enough free space ($needed_space Mb)."
		    RS="$path"
	        return 1
	    fi
    fi

	lrs=1
	if [ -e "$path" ]; then
		if [ -d "$path" ]; then
			if [ -n "$RPM_INSTALLATION" ]; then
				owner=$(rpm -qf $path 2>/dev/null)
				lrs=$?
				[ "$lrs" == "0" ] && summary=$(rpm -q "$owner" --qf %{summary} )
				[ "x$summary" == "x" ] && summary=$owner
			fi
			if [ $lrs != 0 ]; then
				NONRPM_DB_FIND_BY_INSTALLDIR "$path"
				local owner=$RS
				if [ "x$owner" != "x" ]; then
					NONRPM_DB_GET_RPMBASED_FIELD $owner "SUMMARY"
					local rst=$?
					summary=$RS
		                        [ $rst == 0 ] || summary="$owner"
					lrs=0
				else
				        lrs=1
				fi
			fi
			if [ $lrs == 0 ]; then
				[[ "$verbose" != "1" ]] && WARN "Destination directory already exists and owned by $summary."
				RS="$path"
				return 1
			else
				if [ "$verbose" != "1" ]; then 
                    WARN "Destination directory already exists."
				    RS="$path"
				    return 2
                fi
			fi
        fi
	fi
	
	RS="$path"	
		
	return 0
}
	



function MAKE_TEMP_FILE() {

    [[ $# -eq 0 ]] || CROAK "$FUNCNAME() does not expect any arguments."
    LOG_ARGS "$@"
    local temp
    temp=$( RUN mktemp -q "/tmp/install.XXXXXXXX" ) || DIE "Can not create temporary file."
    LOG "ret: <$temp>."
    RS="$temp"

} # create_temp_file



function MAKE_TEMP_DIR() {

    [[ $# -eq 0 ]] || CROAK "$FUNCNAME() does not expect any arguments."
    LOG_ARGS "$@"
    local temp
    temp=$( RUN mktemp -q -d "/tmp/install.XXXXXXXX" ) || DIE "Can not create temporary dir."
    LOG "ret: <$temp>."
    RS="$temp"

} # create_temp_dir




function RPM_CONFIG(){
	
	[ $RPM_CHECK -eq 1 ] || RPM_INIT ; local err=$?	# RPM tool hasn't been checked yet, perform the check
	[ $err -eq ${ERR_OK} ] || return $err

	[ ${RPM_CONFIGURED} -eq -1 ] || return ${RPM_CONFIGURED}

	ARC_GET

	local rpms="4.2.1(x64) 4.1 4.0.2 3.0.5 4.2.2(x64)"
	local rpmi='';

	for rpmi in $rpms ; do
		LOG "Check if RPM supports relocateable packages - $rpmi"
		local ver=`echo $rpmi | sed s/\(.*\)//g`
		local arc=`echo $rpmi | sed s/.*\(//g | sed s/\)//g`
		if [ "$arc" = "$PLATFORM" ] || [ $arc"x" = $ver"x" ] ; then
			PKG_CHECK rpm $ver eq
			if [ $? -eq 0 ] ; then
				LOG "Non-relocatable version of RPM. RPM version: $ver, ARC: $arc"
				PREFIX='';
				RPM_CONFIGURED=${ERR_RPM_NOT_RELOCATABLE}
				return ${ERR_RPM_NOT_RELOCATABLE}
			fi # if [ $? -eq 0 ] ; then
		fi # if [ "$arc" = "$PLATFORM" ] ; then
	done # for rpmi in $rpms ; do
	RPM_CONFIGURED=${ERR_OK};
	return ${ERR_OK}
} # RPM_CONFIG(){






function CHECK_COMMANDS(){
        [ "x$(type -p sed 2>&1)" != "x" ] \
            || DIE "Unable to find 'sed' command, please add its location to your PATH."

        local CHECK_CMD=$(echo "sed egrep cp chmod uniq id rpm rm wc cut uname mkdir rmdir readlink mktemp basename date cpio find cat tac ls gunzip" | sed -e"s/rpm //g")
        local c=''
        for c in ${CHECK_CMD} ; do
	    type -p $c &>/dev/null
	    if [ $? -ne 0 ] ; then
                if [ -f "/etc/mvl-release" ] && [ "$c" == "chkconfig" ] ; then
                : # there is no chkconfig command on MontaVista* CGE Linux*
                else
		    echo "ERROR: unable to find command '$c'."
		    echo "Please add the location to the above commands to your PATH and re-run the script"
		    echo -n "Press Enter to continue."
		    WAIT_ENTER_KEY
		    exit 1
                fi
	    fi
	done
}

ERR_OK=0

ERR_RPM_NO_PACKAGE=10
	ERR_UNKNOWN_ARC=11
	ERR_RPM_UNINSTALL=12
	ERR_RPM_NOT_RELOCATABLE=13
	ERR_RPM_LOCK=14

declare RS
	declare -a RA
	declare _USER_ID_=
	PLATFORM=''
	IA=''
	declare CONFIG_FILENAME=''
	declare CONFIG=''
	CMD_STR=''
	declare LOG_F=
	declare -r TELL_WIDTH=80
	declare _SET_FMT_=
	RPM_CONFIGURED=-1
	RPM_CHECK=0



function USER_ID() {

    if [[ "$_USER_ID_" == "" ]]; then
        _USER_ID_=$( id -u ) || DIE "\"id\" failed."
        LOG "User id is \"$_USER_ID_\"."
    fi
    RS="$_USER_ID_"

} # function user_id



function CHECK_ROOT_PRIVILEDGES() {

        USER_ID
        if [ $_USER_ID_ -ne 0 ] ; then
            if [ ! -w /dev ] ; then
                local current_dir=`pwd`
                WARN "Super-user or \"root\" privileges are required in order to continue."
		IS_COMMAND_LINE_OPTION_EXISTS silent && return 1
                echo -n "Please enter \"root\" "
                exec su -c "cd '$current_dir' && /bin/sh '${SCRIPT}' $@" # always do the fork.
                echo "" # should never get here
                return 1
            fi
        fi

}




function UNINSTALL_RPM(){

	local ChosenRPM=$1
	shift
	local uninstall_options=$@
	local fcode=$ERR_OK

	RPM_CONFIG
	    
	local pack_ver=`RPM_INFO $ChosenRPM VERSION`
	fcode=$?
    
	[ $fcode -eq ${ERR_OK} ] || return $fcode
    
	MAKE_TEMP_FILE
    	local TEMPFILE=$RS
	local err=$?
	[ $err -eq ${ERR_OK} ] || return $err
	
	rpm -vv -e $ChosenRPM $uninstall_options 2>&1 &> $TEMPFILE
	local err=$?
	
	if [ $err -ne 0 ] ; then 
		local RPM_LOGS=$(sed 's/^/    /g' $TEMPFILE)
		LOG "Uninstallation of the $ChosenRPM. RPM logs: \n$RPM_LOGS"
		fcode=$ERR_RPM_UNINSTALL
	else
		LOG "Uninstallation of the $ChosenRPM has succeeded."
		fcode=${ERR_OK}
	fi # if [ ! "$RPMERROR" = 0 ] ; then 

	rm -f $TEMPFILE
	RS=$fcode
	return $fcode

} #UNINSTALL_RPM(){





function NONRPM_INSTALL_PACKAGE() {

    local rpm_path="$1"     # A path to rpm file to be installed.
    local dst_dir="$2"      # A path to directory to install package to.
    local edit_uninstall_mode=""$3	# A mode to fix uninstall script for overwrite multiply installation
    [ "x$edit_uninstall_mode" == "x" ] && edit_uninstall_mode=1 	# To fix as per default
    local touch_dest=""$4
    [ "x$touch_dest" == "x" ] && touch_dest=1
	local noscrits=$5

    [[ -z "$dst_dir" ]] && dst_dir=$("$RPM_EXTR_TOOL" -qp --qf %{PREFIXES} "$rpm_path")
    dst_dir=$(echo "$dst_dir" | sed -e"s/\/\{1,\}/\//g")
    dst_dir=$(echo "$dst_dir" | sed -e"s/\/\{1,\}$//g")
    local db="$INTEL_SDP_PRODUCTS_DB"
    local db_dir=$(dirname "$db") || DIE "Unexpected error."
    local cur_dir=$(pwd) || DIE "Unable to find current directory."
    local rpm_file=$(basename "$rpm_path") || DIE "Unexpected error."

    local rpm_prefix=$("$RPM_EXTR_TOOL" -qp --qf %{PREFIXES} "$rpm_path") \
        || DIE "Unexpected errror."
    local rpm_name=$("$RPM_EXTR_TOOL" -qp --qf %{NAME} "$rpm_path") \
        || DIE "Unexpected errror."
    local log_file=".$rpm_file"_$(date +'%d%m%y_%H%M%S').log \
        || DIE "Unexpected errror."
    local src_dir="$dst_dir/tmp12345qwexyz/${rpm_prefix}"

    if [ $touch_dest == 1 ]; then
		[ -e "$dst_dir" ] && rm -rf "$dst_dir" &>/dev/null
		[ -e "$dst_dir" ] || mkdir -p "$dst_dir" &>/dev/null
    fi

    local script_dir="$dst_dir/.scripts"
    mkdir -p "$script_dir" || DIE "Can't create directory \"$script_dir\""
    local script body
    for script in PREIN POSTIN PREUN POSTUN SUMMARY; do
        body=$("$RPM_EXTR_TOOL" -qp --qf "%{$script}" "$rpm_path") || DIE "Can't extract \"$script\" script"
        if [[ "$body" != "(none)" ]]; then
             echo "$body" > "$script_dir/$script.$rpm_name" || DIE "Can't write file \"$script_dir/$script.$rpm_name\""
        fi
    done

    if [ "$noscripts" != "1" ]; then
        if [[ -e "$script_dir/PREIN.$rpm_name" ]]; then
            env RPM_INSTALL_PREFIX="$dst_dir" /bin/sh "$script_dir/PREIN.$rpm_name"
        fi
    fi

    mkdir "$dst_dir/tmp12345qwexyz" &>/dev/null
    cd "$dst_dir/tmp12345qwexyz"

    "$RPM_EXTR_TOOL" "$cur_dir/$rpm_file" | gunzip --quiet | cpio --quiet -idmu \
        || DIE "Unable to extract files from \"$rpm_file\" to temp filebuf."

    if [ $edit_uninstall_mode == 1 ]; then
		find "$dst_dir/tmp12345qwexyz/$rpm_prefix" -name uninstall*.sh > uninstall.lst \
			|| DIE "Unexpected error."
		local -a lines=( $( wc -l uninstall.lst ) ) \
			|| DIE "Unexpected error."
		[ "${lines[0]}" -eq "1" ] \
			|| DIE "Unexpected error."
		rm -f lines.tmp \
			|| DIE "Unexpected error."
		local uninstall=$(cat "uninstall.lst") \
			|| DIE "Unexpected error."
		cp -p "$uninstall" "uninstall.sh.bak" \
			|| DIE "Unable to copy \"$uninstall\" file to \"uninstall.sh.bak\"."
		chmod u+w "$uninstall" \
			|| DIE "Unable to change mode on \"$uninstall\" file."
		sed s@'^RPM_INSTALLATION=1$'@'RPM_INSTALLATION='@g "uninstall.sh.bak" > "$uninstall" \
			|| DIE "Unable to write \"$uninstall\" file."
		chmod --reference="uninstall.sh.bak" "$uninstall" \
			|| DIE "Unable to change mode on \"$uninstall\" file."
		rm -f "uninstall.sh.bak" &>/dev/null
		rm -f "uninstall.lst" &>/dev/null
    fi

    "$RPM_EXTR_TOOL" "$cur_dir/$rpm_file" | gunzip --quiet | cpio --quiet -t | sed s@"^\.$rpm_prefix"@"$dst_dir"@g | sed 's/^\.//' > "$cur_dir/$log_file" \
        || DIE "Unable to create log file."
    mkdir -p "$db_dir" \
        || DIE "Unable to create \"$db_dir\" directory."
    mv "$cur_dir/$log_file" "$db_dir" \
        || DIE "Unable to copy \"$tmp_dir/$log_file\" file to \"$db_dir\" directory."
    NONRPM_DB_ENTRY_ADD "<:$rpm_name:$rpm_file:$dst_dir:$db_dir/$log_file:>" \
        || DIE "Cannot add entry to database."
    local list=$(find "$dst_dir/tmp12345qwexyz" -type f | grep "tmp12345qwexyz$rpm_prefix" -v)
    for entry in $list; do
        cp -f $entry `echo $entry | sed 's/^.*tmp12345qwexyz//'`
    done
    cd "$src_dir" \
        || DIE "Unable to change directory to \"$src_dir\"."
    if [ $touch_dest == 1 ]; then
    	mv -f * "$dst_dir" \
		|| DIE "Unable to move files to \"$dst_dir\" directory."
    else
    	cp -prf * "$dst_dir" \
        	|| DIE "Unable to copy files to \"$dst_dir\" directory."
    fi
    rm -rf "$dst_dir/tmp12345qwexyz"
    cd "$cur_dir" \
        || DIE "Unable to change directory to \"$cur_dir\"."

    if [ "$noscripts" != "1" ]; then
        if [[ -e "$script_dir/POSTIN.$rpm_name" ]]; then
            env RPM_INSTALL_PREFIX="$dst_dir" /bin/sh "$script_dir/POSTIN.$rpm_name"
        fi
    fi

    return ${ERR_OK}
} # NONRPM_INSTALL_PACKAGE



function NONRPM_UNINSTALL_PACKAGE() {

    local entry="$1"
    local noscripts=$2

    if ! NONRPM_DB_ENTRY_CHECK_SYNTAX "$entry"; then
        echo "Specified db entry to uninstall does not look like a valid one. Probably, internal error."
        return 1
    fi

    NONRPM_DB_ENTRY_GET_LOGFILE "$entry"
    local log_file=$RS \
        || DIE "Unexpected error"
    if [ ! -f "$log_file" ]; then
	    echo "Uninstallation cannot continue for this component: Missing \"$log_file\"."
	    return 1
    fi

    NONRPM_DB_ENTRY_GET_INSTALLDIR "$entry"
    local install_dir=$RS \
        || DIE "Unexpected error"
    if [ ! -d "$install_dir" ]; then
	   echo "Uninstallation cannot continue for this component: Missing \"$install_dir\" directory."
	   return 1
    fi

    local script_dir="$install_dir/.scripts"
    if [ "$noscripts" != "1" ]; then
        NONRPM_DB_ENTRY_GET_FIELD "$entry" 2
        local rpm_name=$RS

        if [[ -f "$script_dir/PREUN.$rpm_name" ]]; then
            env RPM_INSTALL_PREFIX="$install_dir" /bin/sh "$script_dir/PREUN.$rpm_name"
        fi
    fi

    NONRPM_DB_CHECK_SHARED_FILES "$install_dir" "$log_file"
    
    if [ "$noscripts" != "1" ]; then
        if [[ -f "$script_dir/POSTUN.$rpm_name" ]]; then
            env RPM_INSTALL_PREFIX="$install_dir" /bin/sh "$script_dir/POSTUN.$rpm_name"
        fi
    fi

    local script
    for script in PREIN POSTIN PREUN POSTUN SUMMARY; do
        rm -f "$script_dir/$script.$rpm_name"
    done

    if [ "x$(ls $script_dir 2>/dev/null)" == "x" ]; then
	LOG "Nonrpm script dir is emplty. Will be deleted."
	rm -rf "$script_dir"
    else
	LOG "Nonrpm script dir is not emplty."
    fi

    if [[ -d "$install_dir" ]]; then
        rmdir --ignore-fail-on-non-empty "$install_dir"
    fi

    rm -f "$log_file"
    NONRPM_DB_ENTRY_REMOVE "$entry"

    RS=0
    return 0

} # NONRPM_UNINSTALL_PACKAGE




FATAL_EXIT() {
    DIE $@
}

IS_INSTALLED()
{

    local package="$1"
    local product
    local rc
    if [ -n "$RPM_INSTALLATION" ]; then
        if [[ "$package" == *.rpm ]]; then 
            product=$(rpm -qp "$package")
        else
            product="$package"
        fi    
        rpm -qa 2>/dev/null | grep "$product" &>/dev/null
        rc=$?
    fi
    return $rc

}



UNINSTALL_PRODUCT() {
    
    local rc=0
    local subject=$1
    local mode=""$2
    [ "x$mode" == "" ] && mode=1
    local qID=""$3
    local crypto_pack=""


    if [ -n "$RPM_INSTALLATION" ]; then
        CHOSEN_FULL_NAME=$(rpm -q "$subject" --qf %{summary})
    else
	NONRPM_DB_ENTRY_GET_FIELD "$subject" "2"
	local rpm_name=$RS
	NONRPM_DB_ENTRY_GET_INSTALLDIR "$subject"
        summary_dir="$RS/.scripts"

        if [[ -f "$summary_dir/SUMMARY.$rpm_name" ]]; then
            CHOSEN_FULL_NAME="$(cat "$summary_dir/SUMMARY.$rpm_name")"
        else
	    NONRPM_DB_ENTRY_GET_RPMFILE "$subject"
            CHOSEN_FULL_NAME=$RS # TBD.
        fi
    
    fi    

    if IS_COMMAND_LINE_OPTION_EXISTS silent; then
	rc=0
    else
	if [ "$mode" == "1" ] ; then
	    SET_DEFAULT_ANSWER $qID "Yes"
	    READ_YES_NO_ANSWER "Do you really want to uninstall $CHOSEN_FULL_NAME?" "$qID"
    	    rc=$?
	else
    	    rc=0
	fi
    fi    

    if [ $rc == 0 ]; then
        IS_COMMAND_LINE_OPTION_EXISTS silent || echo "Uninstalling $CHOSEN_FULL_NAME..."
        if [ -n "$RPM_INSTALLATION" ]; then
            UNINSTALL_RPM "$subject" -e
            rc=$?
        else
            NONRPM_UNINSTALL_PACKAGE "$subject"
            rc=$?
        fi    
        if [ $rc -eq 0 ]; then
            IS_COMMAND_LINE_OPTION_EXISTS silent || echo "  Successful."
	    return 1
        else
            IS_COMMAND_LINE_OPTION_EXISTS silent || echo "  Unsuccessful."
            return 0
        fi
    fi
    if [ $rc == 1 ]; then
	IS_COMMAND_LINE_OPTION_EXISTS silent || echo "Uninstallation was declined by user."
	return 2
    fi
    LOG "couldn't recognize user's answer on the question id '$qID' - probably wrong entry value in the config file"
    return 0
}
declare CONFIG_PREFIX='SILENT'
declare CONFIG_SECTION='MKL'

SCRIPT="$0"
RPM_INSTALLATION=1
CHECK_COMMANDS
SAVE_COMMAND_LINE $@
PRINT_ERROR=0

uid=$(id -u)
if [ $? != 0 ]; then
    WARN "Unable to detect effective user id. Your id will be set to noroot."
    uid=1
fi
    
if [ $uid == 0 ]; then
    NONRPM_DB_PREFIX=/opt/intel
else
    NONRPM_DB_PREFIX="$HOME/intel"
fi
NONRPM_SET_DB_MODE

CURRDIR="$(pwd)"
postfix=$(dirname "$SCRIPT")
[ "x$postfix" == "x." ] && postfix=""
if [ "x$(echo "$postfix" | egrep ^/)" != "x" ]; then
    CURRDIR=$postfix
    postfix=""
fi

rs=$(pushd $CURRDIR/$postfix 2>/dev/null)
[ $? != 0 ] && RMINSTALLDIR=""
for i in $rs; do    
    RMINSTALLDIR=$i
    break
done
if [ "x$(echo "$RMINSTALLDIR" | egrep ^~)" != "x" ]; then
    RMINSTALLDIR="$HOME$(echo "$RMINSTALLDIR" | sed -e"s/^~//g")"
fi
RMUPPERDIR=$(dirname "$RMINSTALLDIR")

SCRIPT_ARGS=""
while [ $# -ne 0 ]
do
    SCRIPT_ARGS="$SCRIPT_ARGS \"$1\""
    shift
done

if [ -n "$RPM_INSTALLATION" ]; then
    UNINSTALL_LOG_FILE=/var/log/mkl_uninstall.log
else
    UNINSTALL_LOG_FILE=~/mkl_uninstall.log
fi
INIT_LOG "$UNINSTALL_LOG_FILE"

if [ -n "$RPM_INSTALLATION" ]; then
    if [ "x$(rpm)" == "x" ] ; then        
        DIE "ERROR: Unable to find rpm tool, please add its location to your PATH and restart uninstallation"
    fi
fi

if [ -n "$RPM_INSTALLATION" ]; then   
    if ! IS_COMMAND_LINE_OPTION_EXISTS noroot; then
        if ! CHECK_ROOT_PRIVILEDGES "$SCRIPT_ARGS"; then
            DIE "Uninstallation failed"
        fi
    fi
fi    

if [ -n "$RPM_INSTALLATION" ]; then
    RPM_LIST=$(echo $(rpm -qf "$0" | sort -r))
    CHOSEN_RPM=$(echo "$RPM_LIST" | cut -d' ' -f1)
else
    uninstall=$(cd $(dirname "$0") && pwd)
    REL_TO_ABS "$uninstall"
    [ $? -eq 0 ] && uninstall="$RS"
    uninstall="$uninstall"/$(basename $0)
    NONRPM_DB_FIND_FILE_OWNER "$uninstall"
    CHOSEN_RPM=$RS
fi


IS_INSTALLED=$(echo "$CHOSEN_RPM" | grep intel)
if [ "$IS_INSTALLED" = "" ]; then 
    if ! IS_COMMAND_LINE_OPTION_EXISTS silent; then
	echo "Nothing to uninstall. Press Enter to exit..."
	WAIT_ENTER_KEY
    fi
    exit 1
fi

lmode="1"
IS_COMMAND_LINE_OPTION_EXISTS default && lmode="0" 
UNINSTALL_PRODUCT "$CHOSEN_RPM" $lmode "WANT_TO_UNINSTALL_PRODUCT"
case $? in
0)
    DIE
    ;;
1)
    ;;
2)
    exit 1
    ;;
esac

if ! IS_COMMAND_LINE_OPTION_EXISTS silent; then
    if [ -n "$LOG_FILE_NAME" ]; then
        echo "$CHOSEN_FULL_NAME was successfully uninstalled. Please see \"$LOG_FILE_NAME\" for uninstallation details."
    fi
    echo "Press Enter to continue."
    WAIT_ENTER_KEY
fi

rmdir --ignore-fail-on-non-empty "$RMINSTALLDIR" 2>/dev/null
rmdir --ignore-fail-on-non-empty "$RMUPPERDIR" 2>/dev/null
