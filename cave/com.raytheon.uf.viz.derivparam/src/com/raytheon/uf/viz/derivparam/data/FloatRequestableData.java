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
package com.raytheon.uf.viz.derivparam.data;

import com.raytheon.uf.viz.derivparam.inv.TimeAndSpace;

/**
 * RequestableData object for a single constant float value.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 15, 2010            rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class FloatRequestableData extends AbstractRequestableData {

    private Float value;

    public FloatRequestableData(Float value) {
        this.value = value;
        this.dataTime = TimeAndSpace.TIME_AGNOSTIC;
        this.space = TimeAndSpace.SPACE_AGNOSTIC;
    }

    public Float getDataValue() {
        return value;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.grid.util.AbstractRequestableData#getDataValue()
     */
    @Override
    public Float getDataValue(Object arg) {
        return getDataValue();
    }

}
