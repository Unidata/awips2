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

import java.text.ParseException;
import java.text.ParsePosition;
import java.util.Map;

import javax.measure.unit.Unit;
import javax.measure.unit.UnitFormat;

import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.dataplugin.satellite.units.goes.PolarPrecipWaterPixel;
import com.raytheon.uf.common.dataplugin.satellite.units.water.BlendedTPWPixel;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Utility class for dealing with satellite record units.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date         Ticket# Engineer    Description
 * ------------ ------- ----------- -------------------------------------------
 * Jul 31, 2018 6389    mapeters    Initial creation (extracted from
 *                                  SatDataRetriever)
 *
 * </pre>
 *
 * @author mapeters
 */
public class SatelliteUnitsUtil {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SatelliteUnitsUtil.class);

    private static final String DMSP = "DMSP";

    private static final String POES = "POES-NPOESS";

    private static final String MISC = "Miscellaneous";

    private static final String PRECIP = "Sounder Based Derived Precipitable Water (PW)";

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
                recordUnit = UnitFormat.getUCUMInstance().parseProductUnit(
                        record.getUnits(), new ParsePosition(0));
            } catch (ParseException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Unable to parse satellite units: " + record.getUnits(),
                        e);
            }
        }

        if (physicalElement.equals(PRECIP)) {
            String creatingEntity = record.getCreatingEntity();
            if (creatingEntity.equals(DMSP) || creatingEntity.equals(POES)) {
                recordUnit = new PolarPrecipWaterPixel();
            } else if (creatingEntity.equals(MISC)) {
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
        Unit<?> units = recordUnit != null ? recordUnit : Unit.ONE;
        Map<String, Object> attrs = dataRecord.getDataAttributes();
        if (attrs != null) {
            Number offset = (Number) attrs.get(SatelliteRecord.SAT_ADD_OFFSET);
            Number scale = (Number) attrs.get(SatelliteRecord.SAT_SCALE_FACTOR);

            if (offset != null) {
                double offsetVal = offset.doubleValue();
                if (offsetVal != 0.0) {
                    units = units.plus(offsetVal);
                }
            }
            if (scale != null) {
                double scaleVal = scale.doubleValue();
                if (scaleVal != 0.0) {
                    units = units.times(scaleVal);
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
