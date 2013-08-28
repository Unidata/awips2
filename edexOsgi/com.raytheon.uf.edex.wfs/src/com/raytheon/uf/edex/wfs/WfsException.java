/*
 * The following software products were developed by Raytheon:
 *
 * ADE (AWIPS Development Environment) software
 * CAVE (Common AWIPS Visualization Environment) software
 * EDEX (Environmental Data Exchange) software
 * uFrame™ (Universal Framework) software
 *
 * Copyright (c) 2010 Raytheon Co.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/org/documents/epl-v10.php
 *
 *
 * Contractor Name: Raytheon Company
 * Contractor Address:
 * 6825 Pine Street, Suite 340
 * Mail Stop B8
 * Omaha, NE 68106
 * 402.291.0100
 *
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 22, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.edex.wfs;

import com.raytheon.uf.edex.ogc.common.OgcException;

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
			this.code = Code.INVALID_REQUEST;
			break;
		case InvalidParameterValue:
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
