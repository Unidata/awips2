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
package com.raytheon.uf.common.dataplugin.radar.util;

import java.io.IOException;

/**
 * TODO Add Description
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class RadarInfo {
    private int productCode;

    private int numLevels;

    private int layer;

    private int resolution; // in meters (file is in km)

    private int range; // in meters (file is in km)

    private String mnemonic;

    private String name;

    private String nameFormat;
    
    private String abrevNameFormat;

    private String format;

    private boolean elevation;

    private boolean altitude;

    private boolean azimuthRange;

    private boolean speedDir;

    private String displayModes;

    private boolean xSection;

    private boolean hourSpan;

    private boolean bigSpeedDir;

    private boolean mapAndSegment;

    private int awipsProdId;

    private String unit;

    public int getProductCode() {
        return productCode;
    }

    public int getNumLevels() {
        return numLevels;
    }

    public int getLayer() {
        return layer;
    }

    public int getResolution() {
        return resolution;
    }

    public String getMnemonic() {
        return mnemonic;
    }

    public String getName() {
        return name;
    }

    public String getNameFormat() {
        return nameFormat;
    }

    public String getAbrevNameFormat() {
        return abrevNameFormat;
    }

    public String getFormat() {
        return format;
    }

    public boolean isElevation() {
        return elevation;
    }

    public boolean isAltitude() {
        return altitude;
    }

    public boolean isAzimuthRange() {
        return azimuthRange;
    }

    public boolean isSpeedDir() {
        return speedDir;
    }

    public String getDisplayModes() {
        return displayModes;
    }

    public boolean isXSection() {
        return xSection;
    }

    public boolean isHourSpan() {
        return hourSpan;
    }

    public boolean isBigSpeedDir() {
        return bigSpeedDir;
    }

    public boolean isMapAndSegment() {
        return mapAndSegment;
    }

    public int getAwipsProdId() {
        return awipsProdId;
    }

    public String getUnit() {
        return unit;
    }

    public String getDescription() {
        String s = "";
        if (numLevels > 0) {
            s += numLevels + " Level ";
        }
        if (range > 0) {
            s += (int) Math.round(range / 1852.0) + " nm ";
        }
        s += name;
        return s;
    }

    public RadarInfo(String s) throws IOException {
        String[] tokens = s.trim().split(" *\\| *");

        if (tokens.length >= 19) {
            int index = -1;
            productCode = Integer.parseInt(tokens[++index]);
            numLevels = Integer.parseInt(tokens[++index]);
            layer = Integer.parseInt(tokens[++index]);
            resolution = (int) (Double.parseDouble(tokens[++index]) * 1000);
            range = Integer.parseInt(tokens[++index]);
            mnemonic = tokens[++index];
            name = tokens[++index].trim();
            nameFormat = tokens[++index].trim();
            abrevNameFormat = tokens[++index].trim();
            format = tokens[++index];
            elevation = ("y".equals(tokens[++index]));
            altitude = ("y".equals(tokens[++index]));
            azimuthRange = ("y".equals(tokens[++index]));
            speedDir = ("y".equals(tokens[++index]));
            displayModes = tokens[++index];
            xSection = ("y".equals(tokens[++index]));
            hourSpan = ("y".equals(tokens[++index]));
            bigSpeedDir = ("y".equals(tokens[++index]));
            mapAndSegment = ("y".equals(tokens[++index]));
            awipsProdId = Integer.parseInt(tokens[++index]);
            unit = (tokens.length > ++index ? tokens[index] : null);
        } else {
            throw new IOException("Error parsing radar info");
        }
    }

    @Override
    public String toString() {
        String s = String
                .format(
                        "%3d | %3d | %1d | %-4s | %3d | %-4s | %-32s | %-11s | %-11s | %-7s | %s | %s | %s | %s | %-2s | %s | %s | %s | %s |%2d | %s",
                        productCode, numLevels, layer, resolution, range,
                        mnemonic, name, nameFormat, abrevNameFormat, format, (elevation ? "y" : " "),
                        (altitude ? "y" : " "), (azimuthRange ? "y" : " "),
                        (speedDir ? "y" : " "), displayModes, (xSection ? "y"
                                : " "), (hourSpan ? "y" : " "),
                        (bigSpeedDir ? "y" : " "), (mapAndSegment ? "y" : " "),
                        awipsProdId, (unit == null ? "" : unit));
        return s;
    }

    public int getRange() {
        return range;
    }
}
