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
package com.raytheon.viz.core.slice.request;

import com.raytheon.uf.viz.core.style.level.SingleLevel;
import com.raytheon.viz.core.slice.request.HeightScale.ScaleType;

/**
 * Slice request for vertical representation of data
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 11, 2007            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public abstract class VerticalRequest extends SliceRequest {

    protected SingleLevel[] levels;

    protected ScaleType scale;

    /**
     * @return the levels
     */
    public SingleLevel[] getLevels() {
        return levels;
    }

    /**
     * @param levels
     *            the levels to set
     */
    public void setLevels(SingleLevel[] levels) {
        this.levels = levels;
    }

    /**
     * @return the scale
     */
    public ScaleType getScale() {
        return scale;
    }

    /**
     * @param scale
     *            the scale to set
     */
    public void setScale(ScaleType scale) {
        this.scale = scale;
    }

}
