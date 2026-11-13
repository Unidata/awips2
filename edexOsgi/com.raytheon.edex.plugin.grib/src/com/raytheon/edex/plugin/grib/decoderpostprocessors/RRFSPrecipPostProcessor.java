/**
 * This software was developed and / or modified by NSF Unidata
 **/

package com.raytheon.edex.plugin.grib.decoderpostprocessors;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.edex.plugin.grib.exception.GribException;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.parameter.Parameter;

/**
 * Pre-processes RRFS-NA cumulative precipitation fields by identifying total precipitation
 * accumulated from forecast hour zero and normalizing their varying parameter abbreviations
 * (e.g., TP3hr, TP6hr, TP9hr) to a common TPrun parameter, allowing the existing 
 * precipitation post-processor to calculate 3-, 6-, 12-, and 24-hour accumulations while 
 * preserving the original running totals.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#     Engineer        Description
 * ------------ ----------  -----------     --------------------------
 * Nov 10, 2026             Tiffany Meyer   Initial Creation
 *
 * </pre>
 *
 * @author bphillip
 * @version 1
 */

public class RRFSPrecipPostProcessor extends DecoderPostProcessor {

    private static final Pattern TP_PATTERN =
            Pattern.compile("TP(\\d+)hr");

    @Override
    public GridRecord[] process(GridRecord record)
            throws GribException {

        String abbreviation =
                record.getParameter().getAbbreviation();

        Matcher matcher = TP_PATTERN.matcher(abbreviation);

        if (!matcher.matches()) {
            return new GridRecord[] { record };
        }

        int accumulationHours =
                Integer.parseInt(matcher.group(1));

        int forecastSeconds =
                record.getDataTime().getFcstTime();

        // Only normalize cumulative precipitation fields
        // whose accumulation begins at forecast hour zero.
        if (forecastSeconds != accumulationHours * 3600) {
            return new GridRecord[] { record };
        }

        Parameter parameter = new Parameter(
                "TPrun",
                "Total Precipitation Since Model Initialization",
                record.getParameter().getUnit());

        record.setParameter(parameter);

        // Force regeneration of the data URI after the
        // parameter abbreviation has changed.
        record.setDataURI(null);

        return new GridRecord[] { record };
    }
}