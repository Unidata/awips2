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

import java.text.ParsePosition;
import java.util.Map;

import javax.measure.Unit;
import javax.measure.format.ParserException;

import com.raytheon.uf.common.dataplugin.satellite.SatelliteConstants;
import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.dataplugin.satellite.units.goes.PolarPrecipWaterPixel;
import com.raytheon.uf.common.dataplugin.satellite.units.water.BlendedTPWPixel;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

import tec.uom.se.AbstractUnit;
import tec.uom.se.format.SimpleUnitFormat;

/**
 * Utility class for dealing with satellite record units.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date         Ticket# Engineer    Description
 * ------------ ------- ----------- -------------------------------------------
 * Jun 06, 2018 7310    mapeters    Initial creation (extracted from
 *                                  SatDataRetriever)
 * Mar 30, 2020 8083    randerso    Default getRecordUnits() to ONE instead of null
 * May  6, 2020 8083    tgurney     Revert previous change under this ticket,
 *                                  breaks D2D display
 *
 * </pre>
 *
 * @author mapeters
 */
public class SatelliteUnitsUtil {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SatelliteUnitsUtil.class);

    /**
     * Extracts the record units from the given satellite record
     *
     * @param record
     * @return the record units
     */
    public static Unit<?> getRecordUnit(SatelliteRecord record) {
        Unit<?> recordUnit = null;
        String physicalElement = record.getPhysicalElement();

        if (record.getUnits() != null && !record.getUnits().isEmpty()) {
            try {
                recordUnit = SimpleUnitFormat
                        .getInstance(SimpleUnitFormat.Flavor.ASCII)
                        .parseProductUnit(record.getUnits(),
                                new ParsePosition(0));
            } catch (ParserException e) {
                statusHandler.warn(
                        "Unable to parse satellite units: " + record.getUnits(),
                        e);
            }
        }

        if (physicalElement.equals(SatelliteConstants.PRECIP)) {
            String creatingEntity = record.getCreatingEntity();
            if (creatingEntity.equals(SatelliteConstants.DMSP)
                    || creatingEntity.equals(SatelliteConstants.POES)) {
                recordUnit = new PolarPrecipWaterPixel();
            } else if (creatingEntity.equals(SatelliteConstants.MISC)) {
                recordUnit = new BlendedTPWPixel();
            }
        }

        return recordUnit;
    }

    /**
     * Extracts the data units for the data record given the PDO's base unit
     *
     * @param recordUnit
     * @param dataRecord
     * @return
     */
    public static Unit<?> getDataUnit(Unit<?> recordUnit,
            IDataRecord dataRecord) {
        Unit<?> units = recordUnit != null ? recordUnit : AbstractUnit.ONE;
        Map<String, Object> attrs = dataRecord.getDataAttributes();
        if (attrs != null) {
            Number offset = (Number) attrs.get(SatelliteRecord.SAT_ADD_OFFSET);
            Number scale = (Number) attrs.get(SatelliteRecord.SAT_SCALE_FACTOR);

            if (offset != null) {
                double offsetVal = offset.doubleValue();
                if (offsetVal != 0.0) {
                    units = units.shift(offsetVal);
                }
            }
            if (scale != null) {
                double scaleVal = scale.doubleValue();
                if (scaleVal != 0.0) {
                    units = units.multiply(scaleVal);
                }
            }
        }
        return units;
    }

    /**
     * Determines whether or not the data within the given record is signed
     *
     * @param record
     * @return whether or not record's data is signed
     */
    public static boolean isSigned(IDataRecord record) {
        Map<String, Object> attrs = record.getDataAttributes();
        if (attrs != null) {
            Boolean signed = (Boolean) attrs
                    .get(SatelliteRecord.SAT_SIGNED_FLAG);
            if (signed != null && signed) {
                return true;
            }
        }
        return false;
    }
}
