package com.raytheon.edex.plugin.grib.decoderpostprocessors;

import java.util.Arrays;
import java.util.List;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.SI;

import com.raytheon.edex.plugin.grib.exception.GribException;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;

/**
 * 
 * Adjusts data values on Lifted Index to match the units. Lifted index should
 * always be centered around 0 however some sources go through a Celsius to
 * Kelvin conversion which results in data centered around 273.15, this attempts
 * to detect and revert this conversion.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 18, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class LiftedIndexPostProcessor implements IDecoderPostProcessor {

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
