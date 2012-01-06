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
package com.raytheon.edex.plugin.shef.data;

import com.raytheon.uf.common.dataplugin.shef.util.ShefConstants;

/**
 * This class handles the bit manipulation for the QC function 
 * of the SHEF decoder.
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 3/19/08      387        M. Duff     Initial Creation.	
 * 
 * </pre>
 *
 * @author mduff
 * @version 1.0	
 */

public class QC {
	/** The quality code */
    private long qualityCode = ShefConstants.QC_DEFAULT;
    
    /** The bit position */
    private long bitPosition = 0;
    
    /** The setting for the bit */
    private long setting = 1;

	/**
	 * @return the qualityCode
	 */
	public long getQualityCode() {
		return qualityCode;
	}

	/**
	 * @param qualityCode the qualityCode to set
	 */
	public void setQualityCode(long qualityCode) {
		this.qualityCode = qualityCode;
	}

	/**
	 * @return the bitPosition
	 */
	public long getBitPosition() {
		return bitPosition;
	}

	/**
	 * @param bitPosition the bitPosition to set
	 */
	public void setBitPosition(long bitPosition) {
		this.bitPosition = bitPosition;
	}

	/**
	 * @return the setting
	 */
	public long getSetting() {
		return setting;
	}

	/**
	 * @param setting the setting to set
	 */
	public void setSetting(long setting) {
		this.setting = setting;
	}

    
}
