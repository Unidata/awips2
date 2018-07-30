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
package com.raytheon.uf.edex.plugin.loctables.util.handlers;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.edex.decodertools.time.TimeTools;
import com.raytheon.uf.edex.plugin.loctables.util.TableHandler;
import com.raytheon.uf.edex.plugin.loctables.util.store.ObStationRow;

/**
 * Handles parsing of station files. Specific implementation override parseLine
 * to handle transformation.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 16, 2010            jkorman     Initial creation
 * Sep 18, 2014 #3627      mapeters    Updated deprecated {@link TimeTools} usage.
 * Oct 12, 2015 4911       rjpeter     Refactored.
 * Jul 14, 2016 5744       mapeters    Removed unused constant.
 * </pre>
 * 
 * @author jkorman
 */

public abstract class AbstractTableHandler implements TableHandler {

    public static final int STA_NORMAL = 0;

    public static final int ERR_RES_DISPOSED = -100;

    public static final String DIRECTIVE_STATUS_DIR = "#!!STATUS_DIR=";

    private static Pattern LATLON = Pattern
            .compile("(\\d{1,3})(( +\\d{2})( +\\d{2})?)?([NESW])");

    public static final String COMMENT = "#";

    protected Logger logger = LoggerFactory.getLogger(getClass());

    final String handlerName;

    /**
     * 
     * @param name
     * @param storeStrategy
     */
    AbstractTableHandler(String name) {
        handlerName = name;
    }

    /**
     * Parse a line of data and return an ObStationRow.
     * 
     * @param data
     * @return
     */
    abstract protected ObStationRow parseLine(String data);

    @Override
    public List<ObStationRow> process(LocalizationFile locFile)
            throws IOException, LocalizationException {

        if (locFile == null) {
            logger.error("Cannot process null file reference.");
            return null;
        }

        List<ObStationRow> rval = new ArrayList<>(5000);
        logger.info(handlerName + "Handler [" + locFile + "]");

        try (BufferedReader reader = new BufferedReader(new InputStreamReader(
                locFile.openInputStream()))) {
            String line = null;
            while ((line = reader.readLine()) != null) {
                if (checkLine(line)) {
                    ObStationRow row = parseLine(line);

                    if (row != null) {
                        rval.add(row);
                    }
                }
            }
        }

        return rval;
    }

    /**
     * Determine if the specified data is a valid line. Skips empty and
     * commented lines.
     * 
     * @param data
     *            A line from file
     * @return True if line should be parsed, false otherwise.
     */
    public boolean checkLine(String data) {
        boolean rval = true;
        if ((data == null) || data.trim().startsWith(COMMENT)
                || (data.trim().isEmpty())) {
            rval = false;
        }
        return rval;
    }

    /**
     * Convert a latitude or longitude value in degrees, minutes, seconds (EWNS)
     * to a double value.
     * 
     * @param value
     * @return
     */
    public final Double cvtLatLon(String value) {
        Double latlon = null;
        if (value != null) {
            Matcher m = LATLON.matcher(value);
            if (m.find()) {
                double lalo = -9999;
                String s = m.group(1);
                lalo = Double.parseDouble(s);
                s = m.group(3);
                if (s != null) {
                    double mm = Double.parseDouble(s);
                    lalo += (mm / 60);
                    s = m.group(4);
                    if (s != null) {
                        mm = Double.parseDouble(s);
                        lalo += (mm / 3600);
                    }
                }
                s = m.group(5);
                if ("N".equals(s)) {
                    latlon = lalo;
                } else if ("E".equals(s)) {
                    latlon = lalo;
                } else if ("S".equals(s)) {
                    latlon = lalo * -1;
                } else if ("W".equals(s)) {
                    latlon = lalo * -1;
                }
            }
        }
        return latlon;
    }

    /**
     * 
     * @param value
     * @return
     */
    public static final Integer getInt(String value) {
        Integer retValue = null;
        try {
            retValue = new Integer(value);
        } catch (NumberFormatException nfe) {
            // Nothing - return null
        }
        return retValue;
    }

    /**
     * 
     * @param value
     * @param defaultValue
     * @return
     */
    public static final Integer getInt(String value, Integer defaultValue) {
        Integer retValue = getInt(value);
        if (retValue == null) {
            retValue = defaultValue;
        }
        return retValue;
    }

    /**
     * 
     * @param value
     * @return
     */
    public static final Double getDouble(String value) {
        Double retValue = null;
        try {
            retValue = new Double(value);
        } catch (NumberFormatException nfe) {
            // Nothing - return null
        }
        return retValue;
    }

    /**
     * 
     * @param value
     * @return
     */
    public static final Double getDouble(String value, Double defaultValue) {
        Double retValue = getDouble(value);
        if (retValue == null) {
            retValue = defaultValue;
        }
        return retValue;
    }
}
