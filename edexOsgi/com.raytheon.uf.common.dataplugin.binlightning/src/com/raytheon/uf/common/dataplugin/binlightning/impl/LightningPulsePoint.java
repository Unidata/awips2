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
package com.raytheon.uf.common.dataplugin.binlightning.impl;

import java.util.Calendar;

/**
 * Lightning pulse information
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 3, 2014  3226      bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class LightningPulsePoint extends BaseLightningPoint {

    private final LtgPulseType type;

    public LightningPulsePoint(LtgPulseType type) {
        super();
        this.type = type;
    }

    public LightningPulsePoint(Calendar time, LtgPulseType type) {
        super(time);
        this.type = type;
    }

    public LightningPulsePoint(double latitude, double longitude,
            Calendar time, LtgPulseType type) {
        super(latitude, longitude, time);
        this.type = type;
    }

    /**
     * @return the type
     */
    public LtgPulseType getType() {
        return type;
    }

}
