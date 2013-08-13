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
package com.raytheon.viz.radar.util;

import java.io.File;
import java.io.FileNotFoundException;
import java.text.ParseException;
import java.util.HashMap;
import java.util.Map;

import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;

import com.raytheon.uf.common.dataplugin.radar.RadarDataKey;
import com.raytheon.uf.common.dataplugin.radar.RadarDataPoint;
import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.level3.DMDPacket.DMDAttributeIDs;
import com.raytheon.uf.common.dataplugin.radar.level3.generic.GenericDataComponent;
import com.raytheon.uf.common.dataplugin.radar.util.RadarDataRetriever;
import com.raytheon.uf.common.dataplugin.radar.util.RadarRecordUtil;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.wxmath.ZToPsa;
import com.raytheon.uf.viz.core.HDF5Util;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * ??? ??, ????            xxxxxxxx     Initial creation
 * Aug 14, 2013  #2262     dgilling     Use new wxmath method for ztopsa.
 * 
 * </pre>
 * 
 * @author xxxxxxxx
 * @version 1.0
 */
public class DmdTools {

    public static final String[] levels3d = { "LowLyr", "MaxShear", "MaxWind",
            "HiLyr" };

    private static Map<String, String> parameterMap = new HashMap<String, String>();
    static {
        parameterMap.put("LowLyr:ShrMag", "ll_shear");
        parameterMap.put("MaxShear:ShrMag", "max_shear");
        parameterMap.put("LowLyr:wDiv", "ll_convergence");
        parameterMap.put("MidLyr:wDiv", "ml_convergence");
        parameterMap.put("LowLyr:g2gsh", "ll_gtg_vel_dif");
        parameterMap.put("LowLyr:RRV", "ll_rot_vel");
        parameterMap.put("MaxWind:RRV", "max_rot_vel");
        parameterMap.put("LowLyr:diam", "ll_diam");
        parameterMap.put("Layer:sRank", "strength_rank");
        parameterMap.put("LowLyr:GH", "base");
        parameterMap.put("MaxWind:GH", "height_max_rot_vel");
        parameterMap.put("MaxShear:GH", "height_max_shear");
        parameterMap.put("HiLyr:GH", "base");
        parameterMap.put("LowLyr:P", "base");
        parameterMap.put("MaxWind:P", "height_max_rot_vel");
        parameterMap.put("MaxShear:P", "height_max_shear");
        parameterMap.put("HiLyr:P", "base");
    }

    public static Number getParameter(RadarRecord record, String featureId,
            String level, String parameter) {
        String key = level + ":" + parameter;
        String property = parameterMap.get(key);
        String str = RadarRecordUtil.getFeatureValue(record, featureId,
                property);
        Float value = null;
        if (str != null && !str.isEmpty()) {
            value = Float.parseFloat(str);
        }
        if ((key.equals("HiLyr:P") || key.equals("HiLyr:GH")) && value != null) {
            str = RadarRecordUtil.getFeatureValue(record, featureId, "depth");
            if (str != null && !str.isEmpty()) {
                value = value + Float.parseFloat(str);
            } else {
                value = null;
            }
        }
        if (parameter.equals("P") && value != null) {
            value = ZToPsa.ztopsa(value);
        }
        if (value == null) {
            return -9999;
        }
        return value;
    }

    public static String getUnitString(RadarRecord record, String featureId,
            String level, String parameter) {
        if (parameter.equals("P")) {
            return "hPa";
        }
        String key = level + ":" + parameter;
        String property = parameterMap.get(key);

        String rval = "";
        for (RadarDataKey curLatLon : record.getSymbologyData().keySet()) {
            RadarDataPoint currPoint = record.getSymbologyData().get(curLatLon);

            for (Integer type : currPoint.getDisplayGenericPointData().keySet()) {
                for (GenericDataComponent currComponent : currPoint
                        .getDisplayGenericPointData().get(type).values()) {
                    if (featureId.equalsIgnoreCase(currComponent
                            .getValue(DMDAttributeIDs.MESO_ID.toString()))) {
                        return currComponent.getUnits(property);
                    }
                }
            }
        }
        return rval;
    }

    public static Unit<?> getUnit(RadarRecord record, String featureId,
            String level, String parameter) {
        try {
            return (Unit<?>) UnitFormat.getUCUMInstance().parseObject(
                    getUnitString(record, featureId, level, parameter));
        } catch (ParseException e) {
            return Unit.ONE;
        }
    }

    public static void retrieveFromDataStore(RadarRecord record)
            throws VizException {
        if (record.getSymbologyBlock() != null) {
            return;
        }
        File loc = HDF5Util.findHDF5Location(record);
        IDataStore dataStore = DataStoreFactory.getDataStore(loc);
        try {
            RadarDataRetriever.populateRadarRecord(dataStore, record);
        } catch (FileNotFoundException e) {
            throw new VizException(
                    "Error Retrieving DMD Data from Radar Record", e);
        } catch (StorageException e) {
            throw new VizException(
                    "Error Retrieving DMD Data from Radar Record", e);
        }
    }

}
