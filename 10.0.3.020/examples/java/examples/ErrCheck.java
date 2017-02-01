/*******************************************************************************
!                             INTEL CONFIDENTIAL
!  Copyright(C) 2007-2008 Intel Corporation. All Rights Reserved.
!  The source code contained  or  described herein and all documents related to
!  the source code ("Material") are owned by Intel Corporation or its suppliers
!  or licensors.  Title to the  Material remains with  Intel Corporation or its
!  suppliers and licensors. The Material contains trade secrets and proprietary
!  and  confidential  information of  Intel or its suppliers and licensors. The
!  Material  is  protected  by  worldwide  copyright  and trade secret laws and
!  treaty  provisions. No part of the Material may be used, copied, reproduced,
!  modified, published, uploaded, posted, transmitted, distributed or disclosed
!  in any way without Intel's prior express written permission.
!  No license  under any  patent, copyright, trade secret or other intellectual
!  property right is granted to or conferred upon you by disclosure or delivery
!  of the Materials,  either expressly, by implication, inducement, estoppel or
!  otherwise.  Any  license  under  such  intellectual property  rights must be
!  express and approved by Intel in writing.
!
!******************************************************************************/

import com.intel.mkl.*;

public final class ErrCheck {
	private ErrCheck() {}
	public static void CheckVslError(int num) throws Exception
	{
		String error_message;
		switch(num) {
		case VSL.ERROR_FEATURE_NOT_IMPLEMENTED:
			error_message = "Error: this feature not implemented yet (code " + num + ").";
			break;
		case VSL.ERROR_UNKNOWN:
			error_message = "Error: unknown error (code " + num + ").";
			break;
		case VSL.ERROR_BADARGS:
			error_message = "Error: bad arguments (code " + num + ").";
			break;
		case VSL.ERROR_MEM_FAILURE:
			error_message = "Error: memory failure. Memory allocation problem maybe (code " + num + ").";
			break;
		case VSL.ERROR_NULL_PTR:
			error_message = "Error: null pointer (code " + num + ").";
			break;
		case VSL.ERROR_INVALID_BRNG_INDEX:
			error_message = "Error: invalid BRNG index (code " + num + ").";
			break;
		case VSL.ERROR_LEAPFROG_UNSUPPORTED:
			error_message = "Error: leapfrog initialization is unsupported (code " + num + ").";
			break;
		case VSL.ERROR_SKIPAHEAD_UNSUPPORTED:
			error_message = "Error: skipahead initialization is unsupported (code " + num + ").";
			break;
		case VSL.ERROR_BRNGS_INCOMPATIBLE:
			error_message = "Error: BRNGs are not compatible for the operation (code " + num + ").";
			break;
		case VSL.ERROR_BAD_STREAM:
			error_message = "Error: random stream is invalid (code " + num + ").";
			break;
		case VSL.ERROR_BRNG_TABLE_FULL:
			error_message = "Error: table of registered BRNGs is full (code " + num + ").";
			break;
		case VSL.ERROR_BAD_STREAM_STATE_SIZE:
			error_message = "Error: value in StreamStateSize field is bad (code " + num + ").";
			break;
		case VSL.ERROR_BAD_WORD_SIZE:
			error_message = "Error: value in WordSize field is bad (code " + num + ").";
			break;
		case VSL.ERROR_BAD_NSEEDS:
			error_message = "Error: value in NSeeds field is bad (code " + num + ").";
			break;
		case VSL.ERROR_BAD_NBITS:
			error_message = "Error: value in NBits field is bad (code " + num + ").";
			break;
		case VSL.ERROR_BAD_UPDATE:
			error_message = "Error: number of updated entries in buffer is invalid (code " + num + ").";
			break;
		case VSL.ERROR_NO_NUMBERS:
			error_message = "Error: zero number of updated entries in buffer (code " + num + ").";
			break;
		case VSL.ERROR_INVALID_ABSTRACT_STREAM:
			error_message = "Error: abstract random stream is invalid (code " + num + ").";
			break;
		case VSL.ERROR_FILE_CLOSE:
			error_message = "Error: can`t close file (code " + num + ").";
			break;
		case VSL.ERROR_FILE_OPEN:
			error_message = "Error: can`t open file (code " + num + ").";
			break;
		case VSL.ERROR_FILE_WRITE:
			error_message = "Error: can`t write to file (code " + num + ").";
			break;
		case VSL.ERROR_FILE_READ:
			error_message = "Error: can`t read from file (code " + num + ").";
			break;
		case VSL.ERROR_BAD_FILE_FORMAT:
			error_message = "Error: file format is unknown (code " + num + ").";
			break;
		case VSL.ERROR_UNSUPPORTED_FILE_VER:
			error_message = "Error: unsupported file version (code " + num + ").";
			break;
		default:
			error_message = "Unknown errcode " + num + ").";
			break;
		}
		if( num < 0 ) {
			throw new Exception(error_message);
		}
	}
}
