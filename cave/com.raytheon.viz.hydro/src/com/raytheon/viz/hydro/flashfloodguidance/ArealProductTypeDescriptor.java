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
package com.raytheon.viz.hydro.flashfloodguidance;

import com.raytheon.viz.hydrocommon.constants.FFGConstants.ArealDisplayMode;
import com.raytheon.viz.hydrocommon.constants.FFGConstants.PrecipType;
import com.raytheon.viz.hydrocommon.constants.FFGConstants.ResolutionLevel;

/**
 * Areal product type descriptor.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 29, 2009 3298       mpduff      Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class ArealProductTypeDescriptor {
    private ArealDisplayMode mode;

    private PrecipType precipType;

    private ResolutionLevel resolutionLevel;

    /**
     * @return the mode
     */
    public ArealDisplayMode getMode() {
        return mode;
    }

    /**
     * @param mode
     *            the mode to set
     */
    public void setMode(ArealDisplayMode mode) {
        this.mode = mode;
    }

    /**
     * @return the precipType
     */
    public PrecipType getPrecipType() {
        return precipType;
    }

    /**
     * @param precipType
     *            the precipType to set
     */
    public void setPrecipType(PrecipType precipType) {
        this.precipType = precipType;
    }

    /**
     * @return the resolutionLevel
     */
    public ResolutionLevel getResolutionLevel() {
        return resolutionLevel;
    }

    /**
     * @param resolutionLevel
     *            the resolutionLevel to set
     */
    public void setResolutionLevel(ResolutionLevel resolutionLevel) {
        this.resolutionLevel = resolutionLevel;
    }
}
