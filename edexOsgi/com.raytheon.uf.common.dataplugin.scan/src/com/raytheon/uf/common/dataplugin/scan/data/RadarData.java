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
package com.raytheon.uf.common.dataplugin.scan.data;

import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.util.RadarDataInterrogator;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * RadarData class to hold RadarRecord map for SCAN Products.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 9, 2010  5098       grichard    Initial creation.
 * 02/01/13     1569        D. Hladky  removed XML where not needed
 * 
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */


@DynamicSerialize
public class RadarData implements ISerializableObject {

    private final Map<String, RadarRecord> radarMap;

    /**
     * Public constructor
     */
    public RadarData() {
        radarMap = new HashMap<String, RadarRecord>();
    }

    /**
     * Check for type
     * 
     * @param type
     * @return
     */
    public boolean isType(String type) {
        boolean key = false;
        if (radarMap.keySet().contains(type)) {
            key = true;
        }
        return key;
    }

    /**
     * Setter for Radar Record
     * 
     * @param prodType
     *            -- product type
     * @param rr
     *            -- radar record
     */
    public void setRadarRecord(String prodType, RadarRecord rr) {
        radarMap.put(prodType, rr);
    }

    /**
     * Getter for Radar Record
     * 
     * @param prodType
     *            -- product type
     * @return a radar record
     */
    public RadarRecord getRadarRecord(String prodType) {
        return radarMap.get(prodType);
    }

    /**
     * Gets the value for the record
     * 
     * @param prodType
     * @param coor
     * @return
     * @throws VizException
     */
    public double getValue(String prodType, Coordinate coor) {
        double value = 0.0;
        int dataValue = new RadarDataInterrogator(getRadarRecord(prodType))
                .getDataValue(coor);
        value = dataValue;
        return value;
    }

    /**
     * Gets the value decoded for the record
     * 
     * @param prodType
     * @param coor
     * @return
     * @throws VizException
     */
    public double getDecodedValue(String prodType, Coordinate coor) {
        double value = 0.0;
        RadarRecord record = getRadarRecord(prodType);
        int dataValue = new RadarDataInterrogator(record).getDataValue(coor);

        try {
            if (record.getNumLevels() <= 16) {
                Object thresh1 = record.getDecodedThreshold(dataValue);
                if (thresh1 instanceof Float) {
                    value = ((Float) thresh1).shortValue();
                    if (dataValue < 15) {
                        Object thresh2 = getRadarRecord(prodType)
                                .getDecodedThreshold(dataValue + 1);
                        if (thresh2 instanceof Float) {
                            value = ((Float) thresh2).shortValue();
                        }
                    }
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return value;
    }

}
