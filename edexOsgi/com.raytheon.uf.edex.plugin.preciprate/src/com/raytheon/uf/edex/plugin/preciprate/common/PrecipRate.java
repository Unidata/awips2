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
package com.raytheon.uf.edex.plugin.preciprate.common;

import java.util.Map;

import com.raytheon.uf.common.dataplugin.preciprate.PrecipRateRecord;
import com.raytheon.uf.common.dataplugin.radar.util.RadarConstants.DHRValues;
import com.raytheon.uf.common.dataplugin.radar.util.RadarRecordUtil;
import com.raytheon.uf.common.monitor.scan.ScanUtils;

public class PrecipRate {

    public PrecipRateConfig prc = null;

    public PrecipRateRecord rec = null;

    public Map<DHRValues, Double> dhrMap = null;

    public PrecipRate(PrecipRateConfig prc, PrecipRateRecord rec) {
        this.prc = prc;
        this.rec = rec;
        processPrecipRate();
    }

    /**
     * Do the actual heavy lifting.
     */
    private void processPrecipRate() {

        if (prc.getDHR() != null) {
            this.dhrMap = RadarRecordUtil.getDHRValues(prc.getDHR());
            float[] precipRate = ScanUtils
                    .processDHR(prc.getDHR(), getDhrMap());

            byte[] bytes = new byte[precipRate.length];
            // convert to bytes
            for (int radial = 0; radial < rec.getNumRadials(); radial++) {
                for (int bin = 0; bin < rec.getNumBins(); bin++) {
                    Float val = precipRate[(rec.getNumBins() * radial) + bin] * 10;
                    byte b = (int)0;
					if (val > 0.0) {
						val = val + 1;
						b = val.byteValue();
					}
					
					bytes[(rec.getNumBins() * radial) + bin] = b;
                }
            }

            rec.setRawData(bytes);
        }
    }

    /**
     * Gets the array
     * 
     * @return
     */
    public PrecipRateRecord getPrecipRate() {
        return rec;
    }

    /**
     * Gets the DHR ZR values
     * 
     * @return
     */
    public Map<DHRValues, Double> getDhrMap() {
        return dhrMap;
    }

}
