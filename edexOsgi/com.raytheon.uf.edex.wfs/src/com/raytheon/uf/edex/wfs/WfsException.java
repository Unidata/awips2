/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.edex.wfs;

import com.raytheon.uf.edex.ogc.common.OgcException;

/**
 * Exception which includes WFS error codes
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 22, 2011            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class WfsException extends Exception {

	public enum Code {
        INVALID_REQUEST, InvalidParameterValue, CannotLockAllFeatures, DuplicateStoredQueryIdValue, DuplicateStoredQueryParameterName, FeaturesNotLocked, InvalidLockId, InvalidValue, LockHasExpired, OperationParsingFailed, OperationProcessingFailed, ResponseCacheExpired, OperationNotSupported, OptionNotSupported, MissingParameterValue
	}

	private static final long serialVersionUID = 8797482743733419169L;

	protected Code code;

	public WfsException(OgcException e) {
		super(e.getMessage());
		switch (e.getCode()) {
		case InternalServerError:
            this.code = Code.OperationProcessingFailed;
			break;
		case InvalidRequest:
        case OperationNotSupported:
        case MissingParameterValue:
        case MissingDimensionValue:
			this.code = Code.INVALID_REQUEST;
			break;
		case InvalidParameterValue:
        case InvalidCRS:
        case InvalidDimensionValue:
        case InvalidFormat:
        case LayerNotDefined:
			this.code = Code.InvalidParameterValue;
			break;
        default:
            this.code = Code.OperationProcessingFailed;
            break;
		}
	}

	public WfsException(Code code) {
		this.code = code;
	}

	public WfsException(Code code, String message) {
		super(message);
		this.code = code;
	}

	public WfsException(Code code, Throwable cause) {
		super(cause);
		this.code = code;
	}

	public WfsException(Code code, String message, Throwable cause) {
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
