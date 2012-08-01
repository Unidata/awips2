package com.raytheon.uf.edex.plugin.ffmp.common;

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

/**
 * FFTIData
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * July 16, 2012            dhladky     Initial creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class FFTIData {
	
	private Double gap = 0.0;
	
	private Double duration = 0.0;
	
	private String name = null;
	
	private String unit = null;
	
	private boolean isReset = true;

	/**
     * @param name
     *            the name to set
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * @return the name
     */
    public String getName() {
        return name;
    }
	
	public Double getGap() {
        return gap;
    }

    public void setGap(Double gap) {
        this.gap = gap;
    }
    
    public Double getDuration() {
        return duration;
    }

    public void setDuration(Double duration) {
        this.duration = duration;
    }
        
    /**
     * @param unit
     *            the unit to set
     */
    public void setUnit(String unit) {
        this.unit = unit;
    }

    /**
     * @return the unit
     */
    public String getUnit() {
        return unit;
    }
    
    /**
     * Change status
     * @param isReset
     */
    public void reset(boolean isReset) {
    	this.isReset = isReset;
    }
    
    /**
     * Check status
     * @return
     */
    public boolean isReset() {
    	return isReset;
    }

}
