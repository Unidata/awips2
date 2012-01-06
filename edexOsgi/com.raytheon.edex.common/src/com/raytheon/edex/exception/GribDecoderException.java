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


package com.raytheon.edex.exception;

import com.raytheon.uf.edex.core.EdexException;


/**
 * GRIB decoder exception class
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 06/14/06                garmendariz Initial check-in 
 * 
 * </pre>
 *
 * @author garmendariz
 * @version 1.0
 */
public class GribDecoderException extends EdexException {
	
	/**
	 * Default serial version id
	 */
	private static final long serialVersionUID = 1L;

	/**
	 * Constructor that accepts only a string message
	 * 
	 * @param aMsg	The exception message
	 */
	public GribDecoderException(String aMsg) {
		super(aMsg);
	}

	/**
	 * Constructor that accepts a message and a cause
	 * @param aMsg	An exception message
	 * @param e	A message
	 */
	public GribDecoderException(String aMsg, Exception e) {
		super(aMsg, e);
	}
	
}
