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

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.NonSI;
import javax.measure.unit.SI;

import com.raytheon.edex.plugin.grib.exception.GribException;
import com.raytheon.uf.common.comm.CommunicationException;
import com.raytheon.uf.common.dataplugin.grid.GridRecord;
import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.LevelFactory;
import com.raytheon.uf.common.parameter.Parameter;

/**
 * 
 * Converts PSurge 2 data from the grib decoder to the parameter definitions
 * expected by GFE/D2D. There are several odd behaviors in the grib data/decoder
 * that must be corrected.
 * 
 * <ul>
 * <li>The 10% exceedance and all height products arrive with the same grib
 * parameter number(192 from 4.2.10.3.table) so the grib decoder can't tell the
 * difference and gives them all the same name, units, and base abbreviation.
 * This will parse the parameter abbreviation to define the parameter more
 * accurately.
 * <li>The grib decoder blindly appends the duration to the parameter
 * abbreviation which means each time in the cumulative sequence is a new
 * parameter abbreviation. This will find cumulative parameters and give them
 * all the same parameter definition.
 * <li>The grib decoder assigns both TPCSurgeProb data and PHISH data the same
 * datasetID because they have the exact same grib model identifiers. The only
 * way to tell the difference is to look at the level: PHISH data is always at a
 * FHAG level and TPCSurgeProb is always at a SFC level. This remaps all FHAG
 * data to the PHISH model and a Surface level.
 * <li>The grib decoder uses the grib standard units which define the
 * probability surge heights as fractional meter values but the data was
 * originally calculated as probability in feet. This renames the parameters to
 * use the whole number feet instead of fractional meters.
 * <li>The grib file specifies the probabilities of surge below a certain height
 * but they are supposed to be displayed as the probability of exceeding the
 * height. This will switch around the probabilities in the parameter
 * abbreviations.
 * </ul>
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Sep 30, 2013  2390     bsteffen    Rewrite for PSurge 2
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 2.0
 */
public class TPCSurgeProbPostProcessor implements IDecoderPostProcessor {

    private static final Pattern SURGE_PCT_PATTERN = Pattern
            .compile("Surge([0-9]{2})pct([0-9]{1,3})hr");

    private static final Pattern SURGE_HGT_PATTERN = Pattern
            .compile("Surge([0-9]{1,2}\\.[0-9]{1,2})m([0-9]{1,3})hr");

    private static final UnitConverter METERS2FEET = SI.METER
            .getConverterTo(NonSI.FOOT);

    private static final UnitConverter HOURS2SECONDS = NonSI.HOUR
            .getConverterTo(SI.SECOND);

    @Override
    public GridRecord[] process(GridRecord record) throws GribException {

        Parameter param = record.getParameter();
        String paramAbbrev = param.getAbbreviation();
        String paramName = param.getName();
        String paramUnitStr = param.getUnitString();
        int hours;
        Matcher pctMatch = SURGE_PCT_PATTERN.matcher(paramAbbrev);
        Matcher hgtMatch = SURGE_HGT_PATTERN.matcher(paramAbbrev);
        if (pctMatch.matches()) {
            int pct = Integer.parseInt(pctMatch.group(1));
            hours = Integer.parseInt(pctMatch.group(2));

            /* Switch from percent below value to percent above */
            pct = 100 - pct;

            paramAbbrev = "Surge" + pct + "pct";
            paramName = "Surge " + pct + "% Exceedance Ht";
            paramUnitStr = "m";
        } else if (hgtMatch.matches()) {
            double m = Double.parseDouble(hgtMatch.group(1));

            hours = Integer.parseInt(hgtMatch.group(2));
            int ft = (int) Math.round(METERS2FEET.convert(m));

            paramAbbrev = "PSurge" + ft + "ft";
            paramName = "Prob of Surge > " + ft + " ft";
            paramUnitStr = "%";
        } else {
            return new GridRecord[] { record };

        }

        record.getInfo().setId(null);
        record.setDataURI(null);

        /* Map everything to surface. */
        Level level = record.getLevel();
        if (level.getMasterLevel().getName().equals("FHAG")) {
            record.getInfo().setDatasetId("PHISH");
            try {
                level = LevelFactory.getInstance().getLevel("SFC",
                        level.getLevelonevalue(), level.getLeveltwovalue());
            } catch (CommunicationException e) {
                throw new GribException("Error retrieving level information", e);
            }
            record.setLevel(level);
        }

        List<GridRecord> result = new ArrayList<GridRecord>();
        int seconds = (int) HOURS2SECONDS.convert(hours);
        /* Grab cumulative records */
        if (record.getDataTime().getFcstTime() == seconds) {
            GridRecord cumRecord = new GridRecord(record);
            cumRecord.setParameter(new Parameter(paramAbbrev + "Run",
                    "Cumulative " + paramName, paramUnitStr));
            cumRecord.setMessageData(record.getMessageData());
            cumRecord.setOverwriteAllowed(true);
            result.add(cumRecord);
        }

        /*
         * The 0-6hr cumulative record is also the first incremental record so
         * store it in both places. It looks like we will actually be receiving
         * two records for this data but in my test data they are 100% binary
         * identical so there is no way to differentiate and we just have to
         * double store twice(redundantly).
         */
        if (result.isEmpty() || hours == 6) {
            record.setParameter(new Parameter(paramAbbrev + hours + "hr", hours
                    + " Hour " + paramName,
                    paramUnitStr));
            record.setOverwriteAllowed(true);
            result.add(record);
        }
        return result.toArray(new GridRecord[0]);
    }
}
