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
