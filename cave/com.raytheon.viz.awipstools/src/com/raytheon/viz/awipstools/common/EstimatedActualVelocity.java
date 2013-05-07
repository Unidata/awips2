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
package com.raytheon.viz.awipstools.common;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Calculates estimated actual velocity and formats this output.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 10/08/2010   5953       bgonzale    refactored EAV code out of layer class.
 * 05/02/2013   DR 14587   D. Friedman Use base velocity.
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class EstimatedActualVelocity {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(EstimatedActualVelocity.class);

    public static final String NO_DATA = "NO DATA";

    public static final String EAV_PREFIX = "EAV:";

    private static final int MAX_EAV_ANGLE = 80;

    private static final double DEG_TO_RAD = 0.017453;

    private static final String EAV_CONFIG_FILE = "radar/eavConfigTable.txt";

    public static final String EST_ACT_VEL_LOCATION_EDITABLE = "Estimated Act Vel (Editable)";

    public static final String EST_ACT_VEL_LOCATION = "Estimated Act Vel";

    private static String NUMERIC_VALUE_KEY = "numericValue";

    private static String BASE_VELOCITY_NUMERIC_VALUE_KEY = "baseVelocity-numericValue";

    private List<EAVConfig> eavList = new ArrayList<EAVConfig>();

    private int _dirForward;

    private int _dirReverse;

    private double _axisDx;

    private double _axisDy;

    /**
     * @param data
     * @param props
     * @param descriptor
     */
    public EstimatedActualVelocity() {
        try {
            parseEAVFile();
        } catch (IOException e) {
            statusHandler.handle(Priority.PROBLEM, "Cannot load EAV correctly",
                    e);
        }
    }

    /**
     * Called from the calculate label function with both coordinates, thus
     * doing some math to figure out values to send and format the EAV labels
     * 
     * @param startCoor
     * @param endCoor
     * @return
     * @throws VizException
     */
    public String getEAVValues(Coordinate startCoor,
            Map<String, Object> startDataMap, Coordinate endCoor,
            Map<String, Object> endDataMap) throws VizException {
        if (startCoor.equals(endCoor)) {
            return "TOO CLOSE";
        }

        double v1 = getVals(startDataMap);
        double v2 = getVals(endDataMap);

        startCoor = xform(startDataMap);
        endCoor = xform(endDataMap);

        if (startCoor == null || endCoor == null) {
            return NO_DATA;
        }

        double x1 = startCoor.x * Math.sin(startCoor.y);
        double y1 = startCoor.x * Math.cos(startCoor.y);
        double x2 = endCoor.x * Math.sin(endCoor.y);
        double y2 = endCoor.x * Math.cos(endCoor.y);

        _axisDx = x2 - x1;
        _axisDy = y2 - y1;
        _dirReverse = (int) (Math.atan2(_axisDx, _axisDy) / DEG_TO_RAD);

        if (_dirReverse < 0) {
            _dirReverse += 360;
        }
        if (_dirReverse >= 360) {
            _dirReverse -= 360;
        }

        _dirForward = _dirReverse >= 180 ? _dirReverse - 180
                : _dirReverse + 180;

        double mag = Math.sqrt(_axisDx * _axisDx + _axisDy * _axisDy);
        _axisDx /= mag;
        _axisDy /= mag;
        if (((v1 > 1e36) && (v2 > 1e36)) || ((v1 == 0) && (v2 == 0))) {
            return NO_DATA;
        }
        double cosAngle1 = (_axisDx * x1 + _axisDy * y1)
                / Math.sqrt(x1 * x1 + y1 * y1);
        double cosAngle2 = (_axisDx * x2 + _axisDy * y2)
                / Math.sqrt(x2 * x2 + y2 * y2);
        return EAV_PREFIX + formatEAVData(cosAngle1, v1) + "  " + EAV_PREFIX
                + formatEAVData(cosAngle2, v2);
    }

    public String getEAVValue(Coordinate coord, Map<String, Object> dataMap) {
        try {
            coord = xform(dataMap);
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error parsing coordinate from data map.", e);
            return NO_DATA;
        }

        if (coord == null) {
            return NO_DATA;
        }

        double x2 = coord.x * Math.sin(coord.y);
        double y2 = coord.x * Math.cos(coord.y);
        double v = getVals(dataMap);

        if (v > 1e36 || v == 0) {
            return NO_DATA;
        }
        double cosAngle = (_axisDx * x2 + _axisDy * y2)
                / Math.sqrt(x2 * x2 + y2 * y2);

        return EAV_PREFIX + formatEAVData(cosAngle, v);
    }

    /**
     * Gets the raw values for each coordinate
     * 
     * @param coor
     * @return
     * @throws VizException
     */
    private double getVals(Map<String, Object> dataMap) {
        // first get numeric value at the referenced coordinate
        if (dataMap != null) {
            if (dataMap.get("Mnemonic") != null
                    && (dataMap.get("Mnemonic").equals("V")
                            || dataMap.get("Mnemonic").equals("HV") || dataMap
                            .get("Mnemonic").equals("SRM"))
                    && (dataMap.get(BASE_VELOCITY_NUMERIC_VALUE_KEY) != null ||
                            dataMap.get(NUMERIC_VALUE_KEY) != null)) {
                String s = (String) dataMap.get(BASE_VELOCITY_NUMERIC_VALUE_KEY);
                if (s == null)
                    s = (String) dataMap.get(NUMERIC_VALUE_KEY);
                return Double.parseDouble(s);
            }
        }
        return 0;
    }

    /**
     * Convert a coordinate from LatLon to AzimuthRange
     * 
     * @param coor
     *            a LatLon coordinate
     * @return a rangeAzimuthCoord
     * @throws VizException
     */
    private Coordinate xform(Map<String, Object> dataMap) throws VizException {
        if (dataMap != null) {
            if (dataMap.containsKey("Azimuth") && dataMap.containsKey("Range")) {
                double azimuth = Double.parseDouble((String) dataMap
                        .get("Azimuth"));
                double range = Double.parseDouble(((String) dataMap
                        .get("Range")).replace("nm", ""));
                // At this point range is in nautical miles and azimuth is
                // in degrees
                // units of range don't seem to matter, azimuth must be
                // radians.
                return new Coordinate(range, Math.toRadians(azimuth));
            }
        }
        return null;
    }

    /**
     * Gets the file from localization.
     * 
     * @return The file object, null if file does not exist
     * @throws IOException
     */
    private File getFileFromLocalization() throws IOException {
        File file = null;
        try {
            IPathManager pm = PathManagerFactory.getPathManager();
            file = pm.getFile(pm.getContext(LocalizationType.COMMON_STATIC,
                    LocalizationLevel.SITE), EAV_CONFIG_FILE);
            if (file.exists() && file.canRead()) {
                return file;
            }
        } catch (Exception e) {
            e.printStackTrace();
        }

        return null;
    }

    /**
     * Parses the eav config file, should be done only at construct time.
     * 
     * <pre>
     * File format:
     *      angle    formatting prefix   rounding value
     * </pre>
     * 
     * @throws IOException
     */
    private void parseEAVFile() throws IOException {
        /*
         * Each line in eavConfigTable.txt contains up to three space-delimited
         * items: an angle, a formatting prefix, and a rounding value. These are
         * used to format the EAV display and sample strings. If a line does not
         * have a useable angle from 0.0 to 90.0 as the first item it will be
         * ignored. It is assumed the lines have increasing angle values. A line
         * with less than three items, a rounding value <=0, or an angle
         * >MAX_EAV_ANGLE (80 deg; defined in RadarPVImageDepict.C) will be
         * treated as the last line, which is used to supply the angle threshold
         * and output string for those cases where the direction is too close to
         * perpendicular to the radial to allow the algorithm to generate a
         * number. A backquote is changed to a space.
         */
        if (eavList.size() == 0) {
            File file = getFileFromLocalization();
            double minCos = Math.cos(MAX_EAV_ANGLE * DEG_TO_RAD);
            double angle = 999;
            int roundingValue = 1;

            EAVConfig eavptr = new EAVConfig();
            eavList.add(eavptr);
            eavptr.cosAngle = angle;
            eavptr.prefix = "~";
            eavptr.round = roundingValue;

            if ((file != null) && file.canRead()) {
                BufferedReader in = new BufferedReader(new FileReader(file));
                String line;
                while ((line = in.readLine()) != null) {
                    if (line.startsWith("//") || line.startsWith("#")) {
                        continue;
                    }
                    String[] parts = line.split("\\s+");
                    if (parts.length == 3) {
                        angle = Double.parseDouble(parts[0]);
                        roundingValue = Integer.parseInt(parts[2]);
                    } else if (parts.length > 0) {
                        angle = Double.parseDouble(parts[0]);
                    }
                    // if angle not between 0 and 90 line is ignored
                    if ((parts.length < 1) || (angle < 0) || (angle > 90)) {
                        continue;
                    }

                    if ((parts.length == 3) && (roundingValue > 25)) {
                        continue;
                    }

                    angle = Math.cos(angle * DEG_TO_RAD);
                    if (angle <= eavptr.cosAngle) {
                        eavptr = new EAVConfig();
                        eavList.add(eavptr);
                    }

                    eavptr.cosAngle = angle;

                    if (parts.length == 1) {
                        eavptr.prefix = "***";
                    } else {
                        eavptr.prefix = parts[1].replaceAll("`", " ");
                        eavptr.prefix = parts[1];
                    }

                    eavptr.round = roundingValue;
                    if ((parts.length <= 2) || (roundingValue <= 0)
                            || (eavptr.cosAngle < minCos)) {
                        if (eavptr.cosAngle < minCos) {
                            eavptr.cosAngle = minCos;
                        }
                        eavptr.round = -1;
                        break;
                    }
                }
                in.close();
                if (eavptr == null || eavptr.round > 0) {
                    eavptr = new EAVConfig();
                    eavptr.cosAngle = minCos;
                    eavptr.prefix = "***";
                    eavptr.round = -1;
                }
            }
        }
    }

    /**
     * Takes the values of each and determines what to output to the labels
     * 
     * @param value
     * @param cosAngle
     * @return
     */
    private String formatEAVData(double cosAngle, double value) {
        if (value > 1e36 || value == 0.0) {
            return "???";
        }
        String wrk = "";
        int m = eavList.size();
        EAVConfig eavptr = eavList.get(--m);

        if ((-cosAngle <= eavptr.cosAngle) && (cosAngle <= eavptr.cosAngle)) {
            return eavptr.prefix;
        }

        while (true) {
            if (m > 0) {
                eavptr = eavList.get(--m);
                if (m == 0) {
                    break;
                }
                if ((-cosAngle <= eavptr.cosAngle)
                        && (cosAngle <= eavptr.cosAngle)) {
                    break;
                }
            }
        }
        int dir = _dirForward;
        value = value / cosAngle;

        if (value < 0) {
            value = -value;
            dir = _dirReverse;
        }

        if (eavptr.round == 1) {
            wrk = String.format("%1d@%s%dkts", dir, eavptr.prefix,
                    Math.round(value));
        } else {
            value = eavptr.round * (int) (0.5 + value / eavptr.round);
            if (eavptr.round < 0) {
                wrk = String.format("%1d@%s%.1fkts", dir, eavptr.prefix, value);
            } else {
                wrk = String.format("%1d@%s%dkts", dir, eavptr.prefix,
                        Math.round(value));
            }
        }
        return wrk;
    }

    public static boolean hasEAV(Map<String, Object> m) {
        return (m != null
                && m.get("Mnemonic") != null
                && (m.get("Mnemonic").equals("V")
                        || m.get("Mnemonic").equals("HV") || m.get("Mnemonic")
                        .equals("SRM")) && m.containsKey("numericValue")
                && m.containsKey("Azimuth") && m.containsKey("Range"));
    }

    public void setPrimaryEav(EstimatedActualVelocity eavOfPrimaryVelocityLayer) {
        // uses these values from the primary eav, may need to calculate
        // separately instead of reusing.
        this._axisDx = eavOfPrimaryVelocityLayer._axisDx;
        this._axisDy = eavOfPrimaryVelocityLayer._axisDy;
        this._dirForward = eavOfPrimaryVelocityLayer._dirForward;
        this._dirReverse = eavOfPrimaryVelocityLayer._dirReverse;
    }

    /**
     * Class for holding each line of the config file to determine the label
     * output
     * 
     * <pre>
     * 
     * SOFTWARE HISTORY
     * Date         Ticket#    Engineer    Description
     * ------------ ---------- ----------- --------------------------
     * 
     * 
     * </pre>
     * 
     * @author mnash
     * @version 1.0
     */
    private class EAVConfig {
        private double cosAngle;

        protected String prefix;

        protected double round;
    }
}
