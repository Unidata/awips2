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
package com.raytheon.uf.edex.plugin.goesr.decoder.lookup;

import java.util.Map;

import ucar.nc2.Attribute;
import ucar.nc2.NetcdfFile;
import ucar.nc2.Variable;

import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.edex.netcdf.description.exception.InvalidDescriptionException;
import com.raytheon.uf.edex.plugin.goesr.description.data.GoesrDataDescription;

/**
 * Utility class for Goes-R Decoding.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 3, 2015  5059       nabowle     Initial creation
 * 
 * </pre>
 * 
 * @author nabowle
 * @version 1.0
 */

public class GoesrUtils {

    private GoesrUtils() {
    }

    /**
     * When in the Course of decoding records, it becomes necessary for one
     * value to be sent from the {@link DataDescription} to an attribute of a
     * data record, it can be added to the
     * {@link IDataRecord#getDataAttributes()} and be used as though it were an
     * attribute on the netcdf file.
     */
    public static Object getAttributeFromRecord(SatelliteRecord record,
            String name) {
        if (record == null) {
            return null;
        }
        IDataRecord dataRecord = (IDataRecord) record.getMessageData();
        if (dataRecord == null) {
            return null;
        }
        Map<String, Object> attributes = dataRecord.getDataAttributes();
        if (attributes == null) {
            return null;
        }
        return attributes.get(name);
    }

    public static Number getFillValue(Variable variable, Object data) {
        Number fillValue = 0;
        Attribute attr = variable.findAttribute("_FillValue");
        if (attr != null) {
            fillValue = attr.getNumericValue();
            attr = variable.findAttribute("_Unsigned");
            if (attr != null) {
                boolean unsigned = attr.getStringValue().equals("true");
                if (unsigned && fillValue.intValue() < 0) {
                    if (data instanceof byte[]) {
                        fillValue = 0xFF & (fillValue.intValue());
                    } else if (data instanceof short[]) {
                        fillValue = 0xFFFF & (fillValue.intValue());
                    }
                }
            }
        }
        return fillValue;
    }

    public static String getUnits(NetcdfFile file,
            GoesrDataDescription dataDesc, Variable variable)
            throws InvalidDescriptionException {
        String units = null;
        if (dataDesc.getUnits() != null) {
            units = dataDesc.getUnits().getString(file);
            if ("1".equals(units)) {
                units = null;
            }
        }
    
        if (units == null) {
            Attribute attr = variable.findAttribute("units");
            if (attr != null) {
                units = attr.getStringValue();
                if ("1".equals(units)) {
                    units = null;
                }
            }
        }
        return units;
    }
}
