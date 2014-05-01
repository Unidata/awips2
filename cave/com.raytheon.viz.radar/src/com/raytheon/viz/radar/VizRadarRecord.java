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
package com.raytheon.viz.radar;

import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.RadarStoredData;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 16, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public abstract class VizRadarRecord extends RadarRecord {

    private static final long serialVersionUID = 4497342055829648033L;

    public VizRadarRecord() {
        super();
    }

    public VizRadarRecord(RadarRecord that) {
        super(that);
    }

    /**
     * getStoredData should attempt to retrieve the data from edex/hdf5
     */
    public abstract RadarStoredData getStoredData();

    /**
     * getStoredDataAsync should attempt to retrieve data from edex/hdf5 on a
     * different thread, and return null until the data request is complete.
     * 
     * @return the storedData, or null if it is still loading in a nother thread
     */
    public abstract RadarStoredData getStoredDataAsync();

    @Override
    public Object[] getDecodedThresholds() {
        Object[] decodedThresholds = super.getDecodedThresholds();
        if (decodedThresholds == null || decodedThresholds[0] == null) {
            // setting thresholds also sets decoded thresholds.
            setThresholds(getThresholds());
            decodedThresholds = super.getDecodedThresholds();
        }
        return decodedThresholds;
    }

}
