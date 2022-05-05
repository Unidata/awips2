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
package com.raytheon.edex.plugin.shef.util;

import java.io.Serializable;
import java.util.Arrays;
import java.util.List;

import com.raytheon.edex.plugin.shef.data.ShefData;
import com.raytheon.uf.common.dataplugin.shef.tables.Agricultural;
import com.raytheon.uf.common.dataplugin.shef.tables.AgriculturalId;
import com.raytheon.uf.common.dataplugin.shef.tables.Discharge;
import com.raytheon.uf.common.dataplugin.shef.tables.DischargeId;
import com.raytheon.uf.common.dataplugin.shef.tables.Evaporation;
import com.raytheon.uf.common.dataplugin.shef.tables.EvaporationId;
import com.raytheon.uf.common.dataplugin.shef.tables.Fishcount;
import com.raytheon.uf.common.dataplugin.shef.tables.FishcountId;
import com.raytheon.uf.common.dataplugin.shef.tables.Gatedam;
import com.raytheon.uf.common.dataplugin.shef.tables.GatedamId;
import com.raytheon.uf.common.dataplugin.shef.tables.Ground;
import com.raytheon.uf.common.dataplugin.shef.tables.GroundId;
import com.raytheon.uf.common.dataplugin.shef.tables.Height;
import com.raytheon.uf.common.dataplugin.shef.tables.HeightId;
import com.raytheon.uf.common.dataplugin.shef.tables.Ice;
import com.raytheon.uf.common.dataplugin.shef.tables.IceId;
import com.raytheon.uf.common.dataplugin.shef.tables.Lake;
import com.raytheon.uf.common.dataplugin.shef.tables.LakeId;
import com.raytheon.uf.common.dataplugin.shef.tables.Moisture;
import com.raytheon.uf.common.dataplugin.shef.tables.MoistureId;
import com.raytheon.uf.common.dataplugin.shef.tables.Power;
import com.raytheon.uf.common.dataplugin.shef.tables.PowerId;
import com.raytheon.uf.common.dataplugin.shef.tables.Pressure;
import com.raytheon.uf.common.dataplugin.shef.tables.PressureId;
import com.raytheon.uf.common.dataplugin.shef.tables.Radiation;
import com.raytheon.uf.common.dataplugin.shef.tables.RadiationId;
import com.raytheon.uf.common.dataplugin.shef.tables.Snow;
import com.raytheon.uf.common.dataplugin.shef.tables.SnowId;
import com.raytheon.uf.common.dataplugin.shef.tables.Temperature;
import com.raytheon.uf.common.dataplugin.shef.tables.TemperatureId;
import com.raytheon.uf.common.dataplugin.shef.tables.Waterquality;
import com.raytheon.uf.common.dataplugin.shef.tables.WaterqualityId;
import com.raytheon.uf.common.dataplugin.shef.tables.Weather;
import com.raytheon.uf.common.dataplugin.shef.tables.WeatherId;
import com.raytheon.uf.common.dataplugin.shef.tables.Wind;
import com.raytheon.uf.common.dataplugin.shef.tables.WindId;
import com.raytheon.uf.common.dataplugin.shef.tables.Yunique;
import com.raytheon.uf.common.dataplugin.shef.tables.YuniqueId;
import com.raytheon.uf.common.dataplugin.shef.util.ParameterCode.PhysicalElement;
import com.raytheon.uf.common.dataplugin.shef.util.ParameterCode.PhysicalElementCategory;

/**
 * Utility used to identify the potential ihfs data record that the obs_pe
 * stored procedure may insert/update.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 14, 2017 6554       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */

public final class ObsPEPDOLookupUtil {

    private static final List<String> PRESSURE_PE = Arrays.asList(
            new String[] { PhysicalElement.PRESSURE_ATMOSPHERIC.getCode(),
                    PhysicalElement.PRESSURE_ATMOSPHERIC_3HR.getCode(),
                    PhysicalElement.PRESSURE_CHARACTERISTIC.getCode(),
                    PhysicalElement.PRESSURE_SEA_LEVEL.getCode() });

    private ObsPEPDOLookupUtil() {
    }

    public static PDOLookupReturn lookupPDO(final String pe, final String locId,
            final ShefData shefData) {
        if (pe == null) {
            return null;
        }
        Class<?> daoClass = null;
        Serializable id = null;
        final String peFirstChar = pe.substring(0, 1);
        if (PhysicalElementCategory.HEIGHT.getCode().equals(peFirstChar)) {
            daoClass = Height.class;
            HeightId heightId = new HeightId();
            heightId.setLid(locId);
            heightId.setPe(pe);
            heightId.setDur(shefData.getDurationValue());
            heightId.setTs(shefData.getTypeSource().getCode());
            heightId.setExtremum(shefData.getExtremum().getCode());
            heightId.setObstime(shefData.getObservationTimeObj());
            id = heightId;
        } else if (PRESSURE_PE.contains(pe)) {
            daoClass = Pressure.class;
            PressureId pressureId = new PressureId();
            pressureId.setLid(locId);
            pressureId.setPe(pe);
            pressureId.setDur(shefData.getDurationValue());
            pressureId.setTs(shefData.getTypeSource().getCode());
            pressureId.setExtremum(shefData.getExtremum().getCode());
            pressureId.setObstime(shefData.getObservationTimeObj());
            id = pressureId;
        } else if (PhysicalElementCategory.DISCHARGE.getCode()
                .equals(peFirstChar)) {
            daoClass = Discharge.class;
            DischargeId dischargeId = new DischargeId();
            dischargeId.setLid(locId);
            dischargeId.setPe(pe);
            dischargeId.setDur(shefData.getDurationValue());
            dischargeId.setTs(shefData.getTypeSource().getCode());
            dischargeId.setExtremum(shefData.getExtremum().getCode());
            dischargeId.setObstime(shefData.getObservationTimeObj());
            id = dischargeId;
        } else if (PhysicalElementCategory.SNOW.getCode().equals(peFirstChar)) {
            daoClass = Snow.class;
            SnowId snowId = new SnowId();
            snowId.setLid(locId);
            snowId.setPe(pe);
            snowId.setDur(shefData.getDurationValue());
            snowId.setTs(shefData.getTypeSource().getCode());
            snowId.setExtremum(shefData.getExtremum().getCode());
            snowId.setObstime(shefData.getObservationTimeObj());
            id = snowId;
        } else if (PhysicalElementCategory.TEMPERATURE.getCode()
                .equals(peFirstChar)) {
            daoClass = Temperature.class;
            TemperatureId temperatureId = new TemperatureId();
            temperatureId.setLid(locId);
            temperatureId.setPe(pe);
            temperatureId.setDur(shefData.getDurationValue());
            temperatureId.setTs(shefData.getTypeSource().getCode());
            temperatureId.setExtremum(shefData.getExtremum().getCode());
            temperatureId.setObstime(shefData.getObservationTimeObj());
            id = temperatureId;
        } else if (PhysicalElementCategory.WIND.getCode().equals(peFirstChar)) {
            daoClass = Wind.class;
            WindId windId = new WindId();
            windId.setLid(locId);
            windId.setPe(pe);
            windId.setDur(shefData.getDurationValue());
            windId.setTs(shefData.getTypeSource().getCode());
            windId.setExtremum(shefData.getExtremum().getCode());
            windId.setObstime(shefData.getObservationTimeObj());
            id = windId;
        } else if (PhysicalElementCategory.AGRICULTURE.getCode()
                .equals(peFirstChar)) {
            daoClass = Agricultural.class;
            AgriculturalId agriculturalId = new AgriculturalId();
            agriculturalId.setLid(locId);
            agriculturalId.setPe(pe);
            agriculturalId.setDur(shefData.getDurationValue());
            agriculturalId.setTs(shefData.getTypeSource().getCode());
            agriculturalId.setExtremum(shefData.getExtremum().getCode());
            agriculturalId.setObstime(shefData.getObservationTimeObj());
            id = agriculturalId;
        } else if (PhysicalElementCategory.EVAPORATION.getCode()
                .equals(peFirstChar)) {
            daoClass = Evaporation.class;
            EvaporationId evaporationId = new EvaporationId();
            evaporationId.setLid(locId);
            evaporationId.setPe(pe);
            evaporationId.setDur(shefData.getDurationValue());
            evaporationId.setTs(shefData.getTypeSource().getCode());
            evaporationId.setExtremum(shefData.getExtremum().getCode());
            evaporationId.setObstime(shefData.getObservationTimeObj());
            id = evaporationId;
        } else if (PhysicalElementCategory.FISH_COUNT.getCode()
                .equals(peFirstChar)) {
            daoClass = Fishcount.class;
            FishcountId fishcountId = new FishcountId();
            fishcountId.setLid(locId);
            fishcountId.setPe(pe);
            fishcountId.setDur(shefData.getDurationValue());
            fishcountId.setTs(shefData.getTypeSource().getCode());
            fishcountId.setExtremum(shefData.getExtremum().getCode());
            fishcountId.setObstime(shefData.getObservationTimeObj());
            id = fishcountId;
        } else if (PhysicalElementCategory.GROUND.getCode()
                .equals(peFirstChar)) {
            daoClass = Ground.class;
            GroundId groundId = new GroundId();
            groundId.setLid(locId);
            groundId.setPe(pe);
            groundId.setDur(shefData.getDurationValue());
            groundId.setTs(shefData.getTypeSource().getCode());
            groundId.setExtremum(shefData.getExtremum().getCode());
            groundId.setObstime(shefData.getObservationTimeObj());
            id = groundId;
        } else if (PhysicalElementCategory.ICE.getCode().equals(peFirstChar)) {
            daoClass = Ice.class;
            IceId iceId = new IceId();
            iceId.setLid(locId);
            iceId.setPe(pe);
            iceId.setDur(shefData.getDurationValue());
            iceId.setTs(shefData.getTypeSource().getCode());
            iceId.setExtremum(shefData.getExtremum().getCode());
            iceId.setObstime(shefData.getObservationTimeObj());
            id = iceId;
        } else if (PhysicalElementCategory.LAKE.getCode().equals(peFirstChar)) {
            daoClass = Lake.class;
            LakeId lakeId = new LakeId();
            lakeId.setLid(locId);
            lakeId.setPe(pe);
            lakeId.setDur(shefData.getDurationValue());
            lakeId.setTs(shefData.getTypeSource().getCode());
            lakeId.setExtremum(shefData.getExtremum().getCode());
            lakeId.setObstime(shefData.getObservationTimeObj());
            id = lakeId;
        } else if (PhysicalElementCategory.MOISTURE.getCode()
                .equals(peFirstChar)) {
            daoClass = Moisture.class;
            MoistureId moistureId = new MoistureId();
            moistureId.setLid(locId);
            moistureId.setPe(pe);
            moistureId.setDur(shefData.getDurationValue());
            moistureId.setTs(shefData.getTypeSource().getCode());
            moistureId.setExtremum(shefData.getExtremum().getCode());
            moistureId.setObstime(shefData.getObservationTimeObj());
            id = moistureId;
        } else if (PhysicalElementCategory.DAM.getCode().equals(peFirstChar)) {
            daoClass = Gatedam.class;
            GatedamId gatedamId = new GatedamId();
            gatedamId.setLid(locId);
            gatedamId.setPe(pe);
            gatedamId.setDur(shefData.getDurationValue());
            gatedamId.setTs(shefData.getTypeSource().getCode());
            gatedamId.setExtremum(shefData.getExtremum().getCode());
            gatedamId.setObstime(shefData.getObservationTimeObj());
            id = gatedamId;
        } else if (PhysicalElementCategory.RADIATION.getCode()
                .equals(peFirstChar)) {
            daoClass = Radiation.class;
            RadiationId radiationId = new RadiationId();
            radiationId.setLid(locId);
            radiationId.setPe(pe);
            radiationId.setDur(shefData.getDurationValue());
            radiationId.setTs(shefData.getTypeSource().getCode());
            radiationId.setExtremum(shefData.getExtremum().getCode());
            radiationId.setObstime(shefData.getObservationTimeObj());
            id = radiationId;
        } else if (PhysicalElementCategory.GENERATION.getCode()
                .equals(peFirstChar)) {
            daoClass = Power.class;
            PowerId powerId = new PowerId();
            powerId.setLid(locId);
            powerId.setPe(pe);
            powerId.setDur(shefData.getDurationValue());
            powerId.setTs(shefData.getTypeSource().getCode());
            powerId.setExtremum(shefData.getExtremum().getCode());
            powerId.setObstime(shefData.getObservationTimeObj());
            id = powerId;
        } else if (PhysicalElementCategory.WATER.getCode()
                .equals(peFirstChar)) {
            daoClass = Waterquality.class;
            WaterqualityId waterqualityId = new WaterqualityId();
            waterqualityId.setLid(locId);
            waterqualityId.setPe(pe);
            waterqualityId.setDur(shefData.getDurationValue());
            waterqualityId.setTs(shefData.getTypeSource().getCode());
            waterqualityId.setExtremum(shefData.getExtremum().getCode());
            waterqualityId.setObstime(shefData.getObservationTimeObj());
            id = waterqualityId;
        } else if (PhysicalElementCategory.WEATHER.getCode()
                .equals(peFirstChar)) {
            daoClass = Weather.class;
            WeatherId weatherId = new WeatherId();
            weatherId.setLid(locId);
            weatherId.setPe(pe);
            weatherId.setDur(shefData.getDurationValue());
            weatherId.setTs(shefData.getTypeSource().getCode());
            weatherId.setExtremum(shefData.getExtremum().getCode());
            weatherId.setObstime(shefData.getObservationTimeObj());
            id = weatherId;
        } else if (PhysicalElementCategory.STATION.getCode()
                .equals(peFirstChar)) {
            daoClass = Yunique.class;
            YuniqueId yuniqueId = new YuniqueId();
            yuniqueId.setLid(locId);
            yuniqueId.setPe(pe);
            yuniqueId.setDur(shefData.getDurationValue());
            yuniqueId.setTs(shefData.getTypeSource().getCode());
            yuniqueId.setExtremum(shefData.getExtremum().getCode());
            yuniqueId.setObstime(shefData.getObservationTimeObj());
            id = yuniqueId;
        }
        return new PDOLookupReturn(daoClass, id);
    }
}