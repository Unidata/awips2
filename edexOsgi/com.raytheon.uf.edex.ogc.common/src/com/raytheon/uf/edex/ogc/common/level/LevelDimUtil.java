/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.ogc.common.level;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.uf.common.dataplugin.level.Level;
import com.raytheon.uf.common.dataplugin.level.MasterLevel;
import com.raytheon.uf.edex.ogc.common.OgcException;
import com.raytheon.uf.edex.ogc.common.OgcException.Code;

/**
 * Utility methods for parsing and formatting level names for OGC metadata
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 18, 2013            bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class LevelDimUtil {

    public static final String LEVEL_DIM_PREFIX = "LEVEL_";

    public static final Pattern levelPattern = Pattern
            .compile("^(-?[0-9]*\\.?[0-9]+)(_(-?[0-9]*\\.?[0-9]+))?(.*)?$");

    /**
     * @param dimName
     * @param value
     * @param defaultUnits
     * @return null if not parsed
     * @throws OgcException
     */
    public static Level parseLevel(String dimName, String value,
            String defaultUnits) throws OgcException {
        String name = dimName.substring(LEVEL_DIM_PREFIX.length());
        Matcher m = levelPattern.matcher(value);
        if (!m.matches()) {
            return null;
        }
        Level rval = new Level();
        MasterLevel master = new MasterLevel(name);
        rval.setMasterLevel(master);
        rval.setLevelonevalue(parseWithDefault(m.group(1), Level.INVALID_VALUE));
        rval.setLeveltwovalue(parseWithDefault(m.group(3), Level.INVALID_VALUE));
        String units = m.group(4) == null || m.group(4).isEmpty() ? defaultUnits
                : m.group(4);
        if (units != null) {
            master.setUnitString(units);
        } else {
            master.setUnitString(defaultUnits);
        }
        return rval;
    }
    
    /**
     * @param input
     * @param defaultVal
     * @return default if input is null/empty
     * @throws OgcException
     */
    private static double parseWithDefault(String input, double defaultVal)
            throws OgcException {
        if (input == null || input.isEmpty()) {
            return defaultVal;
        }
        try {
            return Double.parseDouble(input);
        } catch (Exception e) {
            throw new OgcException(Code.InvalidParameterValue,
                    "Invalid level value: " + input, e);
        }
    }

    /**
     * Return formatted level value
     * 
     * @param level
     * @return
     */
    public static String formatLevelValue(Level level) {
        String rval = level.getLevelOneValueAsString();
        if (level.isLevelTwoValid()) {
            rval += "_" + level.getLevelTwoValueAsString();
        }
        return rval;
    }

}
