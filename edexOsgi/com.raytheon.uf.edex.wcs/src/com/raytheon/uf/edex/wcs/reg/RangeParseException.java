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
 * May 26, 2011            bclement     Initial creation
 *
 */
package com.raytheon.uf.edex.wcs.reg;

/**
 * TODO Add Description
 * 
 * @author bclement
 * @version 1.0
 */
public class RangeParseException extends Exception {

	private static final long serialVersionUID = 2555591340601297507L;

	/**
	 * 
	 */
	public RangeParseException() {
		super();
	}

	/**
	 * @param message
	 * @param cause
	 */
	public RangeParseException(String message, Throwable cause) {
		super(message, cause);
	}

	/**
	 * @param message
	 */
	public RangeParseException(String message) {
		super(message);
	}

	/**
	 * @param cause
	 */
	public RangeParseException(Throwable cause) {
		super(cause);
	}

}
