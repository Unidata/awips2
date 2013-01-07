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
* Nov 30, 2011            ekladstrup     Initial creation
*
*/ 
package com.raytheon.uf.edex.wcs;

import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * TODO Add Description
 *
 * @author ekladstrup
 * @version 1.0	
 */
@DynamicSerialize
public class WcsCoverageThriftReponse implements ISerializableObject {

	@DynamicSerializeElement
	private IDataRecord dataRecord;
	@DynamicSerializeElement
	private String crsString;
	@DynamicSerializeElement
	private String timeString;
	@DynamicSerializeElement
	private String envelopeString;

	/**
	 * @return the dataRecord
	 */
	public IDataRecord getDataRecord() {
		return dataRecord;
	}

	/**
	 * @param dataRecord
	 *            the dataRecord to set
	 */
	public void setDataRecord(IDataRecord dataRecord) {
		this.dataRecord = dataRecord;
	}

	/**
	 * @return the crsString
	 */
	public String getCrsString() {
		return crsString;
	}

	/**
	 * @param crsString
	 *            the crsString to set
	 */
	public void setCrsString(String crsString) {
		this.crsString = crsString;
	}

	/**
	 * @return the timeString
	 */
	public String getTimeString() {
		return timeString;
	}

	/**
	 * @param timeString
	 *            the timeString to set
	 */
	public void setTimeString(String timeString) {
		this.timeString = timeString;
	}

	/**
	 * @return the envelopeString
	 */
	public String getEnvelopeString() {
		return envelopeString;
	}

	/**
	 * @param envelopeString
	 *            the envelopeString to set
	 */
	public void setEnvelopeString(String envelopeString) {
		this.envelopeString = envelopeString;
	}

}
