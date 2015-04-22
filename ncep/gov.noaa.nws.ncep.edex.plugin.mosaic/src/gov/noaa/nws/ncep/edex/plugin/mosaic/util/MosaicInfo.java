
package gov.noaa.nws.ncep.edex.plugin.mosaic.util;

import java.io.IOException;

/**
 * Decoder implementation for mosaic dictionary
 * 
 * Date         Ticket#         Engineer    Description
 * ------------ ----------      ----------- --------------------------
 * 09/2009      143				L. Lin     	Initial coding
 * 1/2011		143				T. Lee		Used prod name from mosaicInfo.txt
 * </pre>
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * @author L. Lin
 * @version 1.0
 */

public class MosaicInfo {
    private int productCode;

    private int numLevels;

    private int layer;

    private int resolution; // in meters (file is in km)

    private int range; // in meters (file is in km)

    private String prodName;

    private String name;

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

    public String getProdName() {
        return prodName;
    }

    public String getName() {
        return name;
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

    public MosaicInfo(String s) throws IOException {
        String[] tokens = s.trim().split(" *\\| *");

    	//get mosaicInfo.txt to MosaicInfoDict
        if (tokens.length >= 18) {
            productCode = Integer.parseInt(tokens[0]);
            numLevels = Integer.parseInt(tokens[1]);
            layer = Integer.parseInt(tokens[2]);
            resolution = (int) (Double.parseDouble(tokens[3]) * 1000);
            range = Integer.parseInt(tokens[4]) * 1000;
            prodName = tokens[5];
            name = tokens[6];
            format = tokens[7];
            elevation = ("y".equals(tokens[8]));
            altitude = ("y".equals(tokens[9]));
            azimuthRange = ("y".equals(tokens[10]));
            speedDir = ("y".equals(tokens[11]));
            displayModes = tokens[12];
            xSection = ("y".equals(tokens[13]));
            hourSpan = ("y".equals(tokens[14]));
            bigSpeedDir = ("y".equals(tokens[15]));
            mapAndSegment = ("y".equals(tokens[16]));
            awipsProdId = Integer.parseInt(tokens[17]);
            unit = (tokens.length > 18 ? tokens[18] : null);
        } else {
            throw new IOException("Error parsing mosaic info");
        }
    }

    @Override
    public String toString() {
        String s = String
                .format(
                        "%3d | %3d | %1d | %-4s | %3d | %-4s | %-32s | %-7s | %s | %s | %s | %s | %-2s | %s | %s | %s | %s |%2d | %s",
                        productCode, numLevels, layer, resolution, range,
                        prodName, name, format, (elevation ? "y" : " "),
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
