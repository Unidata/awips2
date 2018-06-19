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
package com.raytheon.edex.plugin.grib.decoderpostprocessors;

import java.util.Arrays;
import java.util.List;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.SI;

import com.raytheon.edex.plugin.grib.exception.GribException;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;

/**
 * 
 * Adjusts data values on Lifted Index to store as Celcius values.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 18, 2010            bsteffen     Initial creation
 * Mar 28, 2010  2874      bsteffen     Deprecated
 * Jan 07, 2016  DR17308   MPorricelli  Reinstated 
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class LiftedIndexPostProcessor extends DecoderPostProcessor {

    private static final List<String> LIFTED_INDEX_PARAMETERS = Arrays.asList(
            "SLI", "BLI", "PLI");

    private static final String KELVIN_UNIT_STRING = "K";

    private static final float MAX_VALID = 150;

    private static final UnitConverter k2c = SI.KELVIN
            .getConverterTo(SI.CELSIUS);

    @Override
    public GridRecord[] process(GridRecord record) throws GribException {
        // Change units of Lifted Index to match the data
        if (LIFTED_INDEX_PARAMETERS.contains(record.getParameter()
                .getAbbreviation())
                && KELVIN_UNIT_STRING.equals(record.getParameter()
                        .getUnitString())) {
            float[] data = (float[]) record.getMessageData();
            for (int i = 0; i < data.length; i++) {
                if (data[i] > MAX_VALID) {
                    data[i] = (float) k2c.convert(data[i]);
                }
            }
        }
        return new GridRecord[] { record };
    }

}
