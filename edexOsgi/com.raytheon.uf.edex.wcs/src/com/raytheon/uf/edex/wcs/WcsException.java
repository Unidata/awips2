/*****************************************************************************************
 * COPYRIGHT (c), 2006, RAYTHEON COMPANY
 * ALL RIGHTS RESERVED, An Unpublished Work 
 *
 * RAYTHEON PROPRIETARY
 * If the end user is not the U.S. Government or any agency thereof, use
 * or disclosure of data contained in this source code file is subject to
 * the proprietary restrictions set forth in the Master Rights File.
 *
 * U.S. GOVERNMENT PURPOSE RIGHTS NOTICE
 * If the end user is the U.S. Government or any agency thereof, this source
 * code is provided to the U.S. Government with Government Purpose Rights.
 * Use or disclosure of data contained in this source code file is subject to
 * the "Government Purpose Rights" restriction in the Master Rights File.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * Use or disclosure of data contained in this source code file is subject to
 * the export restrictions set forth in the Master Rights File.
 ******************************************************************************************/

package com.raytheon.uf.edex.wcs;

import com.raytheon.uf.edex.ogc.common.OgcException;

/**
 * TODO - Class comment here
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 
 * </pre>
 * 
 * @author
 * @version 1
 */

public class WcsException extends Exception {

	public enum Code {
		InvalidFormat, InvalidCRS, LayerNotDefined, StyleNotDefined, LayerNotQueryable, InvalidPoint, CurrentUpdateSequence, InvalidUpdateSequence, MissingDimensionValue, InvalidDimensionValue, OperationNotSupported, MissingParameterValue, InvalidParameterValue, NoApplicableCode, UnsupportedCombination, NotEnoughStorage, InvalidRequest, InternalServerError
	}

	private static final long serialVersionUID = 8797482743733419169L;

	protected Code code;

	public WcsException(OgcException ogc) {
		this(Code.valueOf(ogc.getCode().toString()), ogc.getMessage());
	}

	public WcsException(Code code) {
		super();
		this.code = code;
	}

	public WcsException(Code code, String message) {
		super(message);
		this.code = code;
	}

	public WcsException(Code code, Throwable cause) {
		super(cause);
		this.code = code;
	}

	public WcsException(Code code, String message, Throwable cause) {
		super(message, cause);
		this.code = code;
	}

	public Code getCode() {
		return code;
	}

	public void setCode(Code code) {
		this.code = code;
	}

}
