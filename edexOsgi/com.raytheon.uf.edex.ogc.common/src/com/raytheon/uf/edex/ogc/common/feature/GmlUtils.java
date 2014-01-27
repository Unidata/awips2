/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.ogc.common.feature;

import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.uf.edex.ogc.common.InvalidVersionException;
import com.raytheon.uf.edex.ogc.common.Version;
import com.raytheon.uf.edex.ogc.common.http.MimeType;

/**
 * Utility methods and constants for GML version parsing and comparison
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 30, 2012            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class GmlUtils {

    public static final MimeType GML31_TYPE = new MimeType(
            "application/gml+xml; version=3.1");

    public static final MimeType GML32_TYPE = new MimeType(
            "application/gml+xml; version=3.2");

    public static final MimeType GML311_OLD_TYPE = new MimeType(
            "text/xml; subtype=\"gml/3.1.1\"");

    public static final MimeType GML321_OLD_TYPE = new MimeType(
            "text/xml; subtype=\"gml/3.2.1\"");

    public static final MimeType GML31_VND_TYPE = new MimeType(
            "application/vnd.ogc.gml; version=3.1");

    public static final MimeType GML32_VND_TYPE = new MimeType(
            "application/vnd.ogc.gml; version=3.2");

    protected static final Pattern subPattern = Pattern
            .compile("^\\s*([a-zA-Z]+)/([0-9\\.]+).*$");

    protected static final Pattern versionPattern = Pattern
            .compile("^\\s*([0-9]+)\\.([0-9]+)\\.?([0-9]+)?.*$");

    protected static final Map<Version, MimeType> versionMap;
    static {
        versionMap = new HashMap<Version, MimeType>();
        versionMap.put(new Version(3, 1, 0), GML31_TYPE);
        versionMap.put(new Version(3, 2, 0), GML32_TYPE);
    }

    /**
     * @param type
     * @return true if type is a form of gml
     */
    public static boolean isGml(MimeType type){
        if ( "application".equalsIgnoreCase(type.getType())){
            if ("gml+xml".equalsIgnoreCase(type.getSubtype())) {
                return true;
            }
            if ( "vnd.ogc.gml".equalsIgnoreCase(type.getSubtype())){
                return true;
            }
        } else if ("text".equalsIgnoreCase(type.getType())
                && "xml".equals(type.getSubtype())) {
            String param = type.getParam("subtype");
            if (param != null && param.toLowerCase().startsWith("gml")) {
                return true;
            }
        }
        return false;
    }

    /**
     * @param type
     * @return null if type isn't gml or version is not found
     */
    public static String getGmlVersion(MimeType type) {
        if (!isGml(type)) {
            return null;
        }
        String param = type.getParam("version");
        if (param != null) {
            return param;
        }
        param = type.getParam("subtype");
        if (param != null) {
            Matcher m = subPattern.matcher(param);
            if (m.matches()) {
                return m.group(2);
            }
        }
        return null;
    }

    /**
     * @param left
     * @param right
     * @return true if both are gml types with versions and those versions have
     *         the same major and minor versions
     */
    public static boolean areCompatible(MimeType left, MimeType right){
        String leftVStr = getGmlVersion(left);
        String rightVStr = getGmlVersion(right);
        if ( leftVStr == null || rightVStr == null){
            return false;
        }
        try {
            Version leftVersion = new Version(leftVStr);
            Version rightVersion = new Version(rightVStr);
            return leftVersion.equalsMajorMinor(rightVersion);
        } catch (InvalidVersionException e) {
            return false;
        }
    }

    /**
     * Find the supported GML version that matches the arguments major and minor
     * version
     * 
     * @param type
     * @return null if no match found
     */
    public static MimeType getMatchingGmlVersion(MimeType type) {
        String vstr = getGmlVersion(type);
        if (vstr == null) {
            return null;
        }
        try{
            Version version = new Version(vstr).majorMinorOnly();
            return versionMap.get(version);
        } catch (InvalidVersionException e){
            return null;
        }
    }
}
