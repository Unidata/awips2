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
package com.raytheon.uf.edex.ogc.common;


/**
 * Exception class containing OGC specific error codes.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2011            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class OgcException extends Exception {

	private static final long serialVersionUID = -5832661027013919871L;

	public enum Code {
        InvalidFormat, InvalidCRS, LayerNotDefined, MissingDimensionValue, InvalidDimensionValue, OperationNotSupported, MissingParameterValue, InvalidParameterValue, InternalServerError, InvalidRequest, VersionNegotiationFailed
	}
	
	protected Code code;

	public OgcException(Code code) {
		super();
		this.code = code;
	}

	public OgcException(Code code, String message) {
		super(message);
		this.code = code;
	}

	public OgcException(Code code, Throwable cause) {
		super(cause);
		this.code = code;
	}

	public OgcException(Code code, String message, Throwable cause) {
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
