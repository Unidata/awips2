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

package com.raytheon.uf.common.dataplugin.satellite.units;

import javax.measure.quantity.Dimensionless;
import javax.measure.quantity.Length;
import javax.measure.quantity.Temperature;
import javax.measure.quantity.Velocity;
import javax.measure.unit.SI;
import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;

import com.raytheon.uf.common.dataplugin.satellite.units.generic.GenericPixel;
import com.raytheon.uf.common.dataplugin.satellite.units.goes.PercentOfNormalTPWPixel;
import com.raytheon.uf.common.dataplugin.satellite.units.goes.PolarPrecipWaterPixel;
import com.raytheon.uf.common.dataplugin.satellite.units.goes.SounderCloudAmountPixel;
import com.raytheon.uf.common.dataplugin.satellite.units.goes.SounderCloudTopHeightPixel;
import com.raytheon.uf.common.dataplugin.satellite.units.goes.SounderLiftedIndexPixel;
import com.raytheon.uf.common.dataplugin.satellite.units.goes.SounderPrecipWaterPixel;
import com.raytheon.uf.common.dataplugin.satellite.units.goes.SounderSkinTempPixel;
import com.raytheon.uf.common.dataplugin.satellite.units.ir.IRPixel;
import com.raytheon.uf.common.dataplugin.satellite.units.water.PrecipPixel;
import com.raytheon.uf.common.dataplugin.satellite.units.water.RainfallRatePixel;

/**
 * Contains references to units used by satellite data
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 4, 2007             njensen     Initial creation
 * Mar 23, 2009     2086   jsanchez    Updated RainfallRatePixel to be velocity.
 *                                     Added PolarPrecipWaterPixel.
 * Jun 20, 2013     2122   mschenke    Added alias for degrees celsius to "C"
 * 
 * </pre>
 * 
 * @author njensen
 */
public class SatelliteUnits {

    private SatelliteUnits() {

    }

    public static final Unit<Temperature> IR_PIXEL = new IRPixel();

    public static final Unit<Length> PRECIP_PIXEL = new PrecipPixel();

    public static final Unit<Velocity> RAINFALL_RATE_PIXEL = new RainfallRatePixel();

    public static final Unit<Dimensionless> SOUNDER_CLOUD_AMOUNT_PIXEL = new SounderCloudAmountPixel();

    public static final Unit<Length> SOUNDER_CLOUD_HEIGHT_PIXEL = new SounderCloudTopHeightPixel();

    public static final Unit<Temperature> SOUNDER_LIFTED_INDEX_PIXEL = new SounderLiftedIndexPixel();

    public static final Unit<Length> SOUNDER_PRECIP_WATER_PIXEL = new SounderPrecipWaterPixel();

    public static final Unit<Length> POLAR_PRECIP_WATER_PIXEL = new PolarPrecipWaterPixel();

    public static final Unit<Temperature> SOUNDER_SKIN_TEMP_PIXEL = new SounderSkinTempPixel();

    public static final Unit<Dimensionless> PERCENT_PIXEL = new PercentOfNormalTPWPixel();

    public static final Unit<Dimensionless> GENERIC_PIXEL = new GenericPixel();

    public static void register() {
        UnitFormat.getUCUMInstance().alias(SI.KELVIN, "kelvin");
        UnitFormat.getUCUMInstance().alias(SI.CELSIUS, "C");
        UnitFormat.getUCUMInstance().label(SatelliteUnits.IR_PIXEL, "IRPixel");
        UnitFormat.getUCUMInstance().label(SatelliteUnits.PRECIP_PIXEL,
                "PrecipPixel");
        UnitFormat.getUCUMInstance().label(SatelliteUnits.RAINFALL_RATE_PIXEL,
                "RainfallRatePixel");
        UnitFormat.getUCUMInstance().label(
                SatelliteUnits.SOUNDER_CLOUD_AMOUNT_PIXEL,
                "SounderCloudAmountPixel");
        UnitFormat.getUCUMInstance().label(
                SatelliteUnits.SOUNDER_CLOUD_HEIGHT_PIXEL,
                "SounderCloudTopHeightPixel");
        UnitFormat.getUCUMInstance().label(
                SatelliteUnits.SOUNDER_LIFTED_INDEX_PIXEL,
                "SounderLiftedIndexPixel");
        UnitFormat.getUCUMInstance().label(
                SatelliteUnits.SOUNDER_PRECIP_WATER_PIXEL,
                "SounderPrecipWaterPixel");
        UnitFormat.getUCUMInstance().label(
                SatelliteUnits.POLAR_PRECIP_WATER_PIXEL,
                "PolarPrecipWaterPixel");
        UnitFormat.getUCUMInstance().label(
                SatelliteUnits.SOUNDER_SKIN_TEMP_PIXEL, "SounderSkinTempPixel");
        UnitFormat.getUCUMInstance().label(SatelliteUnits.GENERIC_PIXEL,
                "GenericPixel");
        UnitFormat.getUCUMInstance().label(SatelliteUnits.PERCENT_PIXEL,
                "PercentOfNormalTPWPixel");
    }

}
