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
package com.raytheon.uf.edex.plugin.bufrobs.util;

import com.raytheon.edex.plugin.sfcobs.SfcObsPointDataTransform;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.sfcobs.ObsCommon;
import com.raytheon.uf.common.pointdata.PointDataView;

/**
 * Utility to populate an obs record using the point data view. This is
 * necessary because the shef converter looks for data in the record instead of
 * the point data view.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 4, 2014  2906      bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class RecordPopulator {

    SfcObsPointDataTransform transform = new SfcObsPointDataTransform();

    /**
     * 
     */
    public RecordPopulator() {
    }

    /**
     * @param records
     * @return
     */
    public PluginDataObject[] populate(PluginDataObject[] records) {
        for (PluginDataObject pdo : records) {
            if (pdo instanceof ObsCommon) {
                populateObs((ObsCommon) pdo);
            }
        }
        return records;
    }

    /**
     * Populate record fields using the point data view
     * 
     * @param obs
     */
    public static void populateObs(ObsCommon obs) {
        PointDataView pdv = obs.getPointDataView();
        obs.setTemp(pdv.getNumber(SfcObsPointDataTransform.TEMPERATURE)
                .doubleValue());
        obs.setDwpt(pdv.getNumber(SfcObsPointDataTransform.DEWPOINT)
                .doubleValue());
        obs.setWindSpeed(pdv.getNumber(SfcObsPointDataTransform.WIND_SPEED)
                .doubleValue());
        obs.setWindDirection(pdv.getNumber(SfcObsPointDataTransform.WIND_DIR)
                .intValue());
        obs.setWindGust(pdv.getNumber(SfcObsPointDataTransform.WIND_GUST)
                .doubleValue());
        obs.setPeakWindSpeed(pdv.getNumber(
                SfcObsPointDataTransform.PEAK_WIND_SPEED).doubleValue());
        obs.setPeakWindTime(pdv.getNumber(
                SfcObsPointDataTransform.PEAK_WIND_SPEED_TIME).longValue());
        obs.setPeakWindDir(pdv
                .getNumber(SfcObsPointDataTransform.PEAK_WIND_DIR).intValue());

        obs.setPressureSealevel(pdv.getNumber(
                SfcObsPointDataTransform.SEA_LEVEL_PRESS).intValue());
        obs.setPressureAltimeter(pdv.getNumber(
                SfcObsPointDataTransform.ALTIMETER).intValue());
        obs.setPressureStation(pdv.getNumber(
                SfcObsPointDataTransform.STATION_PRESS).intValue());
        obs.setPressChange3Hr(pdv.getNumber(
                SfcObsPointDataTransform.PRESS_CHANGE_3HR).doubleValue());
        obs.setPressChangeChar(pdv.getNumber(
                SfcObsPointDataTransform.PRESS_CHANGE_CHAR).intValue());

        obs.setHorzVisibility(pdv
                .getNumber(SfcObsPointDataTransform.VISIBILITY).intValue());
        obs.setWx_present(pdv.getNumber(SfcObsPointDataTransform.WX_PRESENT)
                .intValue());
        obs.setPresWeather(pdv.getString(SfcObsPointDataTransform.PRES_WEATHER));
        obs.setWx_past_1(pdv.getNumber(SfcObsPointDataTransform.WX_PAST_1)
                .intValue());
        obs.setWx_past_2(pdv.getNumber(SfcObsPointDataTransform.WX_PAST_2)
                .intValue());

        obs.setTotalCloudCover(pdv.getNumber(
                SfcObsPointDataTransform.CLOUD_AMOUNT_TOT).intValue());
        obs.setCloudBaseHeight(pdv.getNumber(
                SfcObsPointDataTransform.CLOUD_HGT_LOW).intValue());
        obs.setLowCloudType(pdv.getNumber(
                SfcObsPointDataTransform.CLOUD_TYPE_LOW).intValue());
        obs.setMidCloudType(pdv.getNumber(
                SfcObsPointDataTransform.CLOUD_TYPE_MID).intValue());
        obs.setHighCloudType(pdv.getNumber(
                SfcObsPointDataTransform.CLOUD_TYPE_HI).intValue());

        obs.setWind10mSpeed(pdv.getNumber(
                SfcObsPointDataTransform.WIND_SPD_EQUIV_10M).doubleValue());
        obs.setWind20mSpeed(pdv.getNumber(
                SfcObsPointDataTransform.WIND_SPD_EQUIV_20M).doubleValue());

        // pdv.setInt(ICE_CODE,record.getIceCode);
        obs.setSeaTemp(pdv.getNumber(SfcObsPointDataTransform.SEA_SFC_TEMP)
                .doubleValue());
        obs.setWetBulb(pdv.getNumber(SfcObsPointDataTransform.WET_BULB)
                .doubleValue());

        obs.setPlatformDirection(pdv.getNumber(
                SfcObsPointDataTransform.PLATFORM_DIR).intValue());
        obs.setPlatformMovement(pdv.getNumber(
                SfcObsPointDataTransform.PLATFORM_SPD).doubleValue());

        obs.setWindWaveHeight(pdv.getNumber(
                SfcObsPointDataTransform.WIND_WV_HGT).doubleValue());
        obs.setWindWavePeriod(pdv
                .getNumber(SfcObsPointDataTransform.WIND_WV_PD).intValue());

        obs.setWaveHeight(pdv.getNumber(SfcObsPointDataTransform.WV_HGT)
                .doubleValue());
        obs.setWavePeriod(pdv.getNumber(SfcObsPointDataTransform.WV_PD)
                .intValue());
        obs.setWaveSteepness(pdv.getNumber(
                SfcObsPointDataTransform.WV_STEEPNESS).doubleValue());

        obs.setHighResWaveHeight(pdv.getNumber(
                SfcObsPointDataTransform.HI_RES_WV_HGT).doubleValue());

        obs.setPrimarySwellWaveDir(pdv.getNumber(
                SfcObsPointDataTransform.PRI_SWELL_WV_DIR).doubleValue());
        obs.setPrimarySwellWavePeriod(pdv.getNumber(
                SfcObsPointDataTransform.PRI_SWELL_WV_PD).intValue());
        obs.setPrimarySwellWaveHeight(pdv.getNumber(
                SfcObsPointDataTransform.PRI_SWELL_WV_HGT).doubleValue());

        obs.setSecondarySwellWaveDir(pdv.getNumber(
                SfcObsPointDataTransform.SEC_SWELL_WV_DIR).doubleValue());
        obs.setSecondarySwellWavePeriod(pdv.getNumber(
                SfcObsPointDataTransform.SEC_SWELL_WV_PD).intValue());
        obs.setSecondarySwellWaveHeight(pdv.getNumber(
                SfcObsPointDataTransform.SEC_SWELL_WV_HGT).doubleValue());
    }

}
