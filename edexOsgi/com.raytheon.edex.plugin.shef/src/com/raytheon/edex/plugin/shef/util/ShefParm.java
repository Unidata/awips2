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
package com.raytheon.edex.plugin.shef.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;

/**
 * This class reads the SHEFPARM data file contents and stores them in this
 * object for later use.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 18, 2008 387        M. Duff     Initial Version.
 * Dec 16, 2015 5166       kbisanz     Update logging to use SLF4J
 * Jan 10, 2018 5049       mduff       Made class not so static.
 * 
 * </pre>
 * 
 * @author mduff
 */

public class ShefParm {
    private static final String PLUGIN_NAME = "shef";

    private static final String PROPFILE_NAME = "SHEFPARM";

    private Map<String, Double> physicalElementConversion;

    private Map<String, Short> durationCodeValues;

    private Map<String, Integer> typeSourceCodes;

    private Map<String, Integer> extremumCodes;

    private Map<String, Float> probabilityCodeValues;

    private Map<String, String> sendCodesDuration;

    private Map<String, Integer> dataQualifierCodes;

    private int MAX_ERRORS;

    private static final Logger log = LoggerFactory
            .getLogger(com.raytheon.edex.plugin.shef.util.ShefParm.class);;

    private int fileSection = 0;

    public ShefParm() {
        durationCodeValues = new HashMap<>();
        typeSourceCodes = new HashMap<>();
        extremumCodes = new HashMap<>();
        probabilityCodeValues = new HashMap<>();
        sendCodesDuration = new HashMap<>();
        dataQualifierCodes = new HashMap<>();
        physicalElementConversion = new HashMap<>();
        MAX_ERRORS = 0;
    }

    /**
     * Get a Physical Element conversion factor
     * 
     * @param key
     *            - Physical Element
     * @return - the conversion factor
     */
    public Double getPhysicalElementConversionFactor(String key) {
        return physicalElementConversion.get(key);
    }

    /**
     * Get a Duration Code value
     * 
     * @param key
     *            - Duration Code
     * @return - the Duration Code value
     */
    public Short getDurationCodeValue(String key) {
        return durationCodeValues.get(key);
    }

    /**
     * Check the Type Source Code
     * 
     * @param key
     *            - Type Source
     * @return - 1 if valid code, null if invalid
     */
    public Integer getTypeSourceCode(String key) {
        return typeSourceCodes.get(key);
    }

    /**
     * Check the Extremum Code
     * 
     * @param key
     *            - Extremum Code
     * @return - 1 if valid code, null if invalid
     */
    public Integer getExtremumCode(String key) {
        return extremumCodes.get(key);
    }

    /**
     * Get the Probability Code Value
     * 
     * @param key
     *            - Probability Code
     * @return - Probability Code's value
     */
    public Float getProbabilityCodeValue(String key) {
        return probabilityCodeValues.get(key);
    }

    /**
     * Get a Send Code or Duration default value for these special cases
     * 
     * @param key
     *            - Code
     * @return - Default Values
     */
    public String getSendCodeDurationDefaults(String key) {
        return sendCodesDuration.get(key);
    }

    /**
     * Check the Data Qualifier Code
     * 
     * @param key
     *            - Extremum Code
     * @return - 1 if valid code, null if invalid
     */
    public Integer getDataQualifierCodes(String key) {
        return dataQualifierCodes.get(key);
    }

    /**
     * Get the maximum number of errors defined
     * 
     * @return - the maximum number of errors
     */
    public Integer getMaxErrors() {
        return MAX_ERRORS;
    }

    /**
     * Populate the values from the file.
     */
    public void populate() {

        PathManager pathMgr = (PathManager) PathManagerFactory.getPathManager();

        LocalizationContext ctx = pathMgr.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
        File baseDir = pathMgr.getFile(ctx, PLUGIN_NAME);
        File srcFile = new File(baseDir, PROPFILE_NAME);

        try (BufferedReader in = new BufferedReader(new FileReader(srcFile))) {
            String line = null;
            while ((line = in.readLine()) != null) {
                processLine(line);
            }
            in.close();
        } catch (IOException ioe) {
            log.error("Error loading " + PROPFILE_NAME, ioe);
        }
    }

    private void processLine(String line) {
        String[] pair = null;
        if (line.startsWith("$")) {
            return;
        } else if (line.startsWith("*")) {
            if (line.startsWith("**")) {
                fileSection = 999;
            } else {
                fileSection = Integer.parseInt(line.substring(1, 2));
            }
            return;
        } else {
            if ("SHEFPARM".equals(line)) {
                return;
            }
            switch (fileSection) {
            case 1:
                pair = line.split("\\s+");
                physicalElementConversion.put(pair[0],
                        Double.parseDouble(pair[1]));
                break;
            case 2:
                pair = line.split("\\s+");
                durationCodeValues.put(pair[0], Short.parseShort(pair[1]));
                break;
            case 3:
                pair = line.split("\\s+");
                typeSourceCodes.put(pair[0], Integer.parseInt(pair[1]));
                break;
            case 4:
                pair = line.split("\\s+");
                extremumCodes.put(pair[0], Integer.parseInt(pair[1]));
                break;
            case 5:
                pair = line.split("\\s+");
                probabilityCodeValues.put(pair[0], Float.parseFloat(pair[1]));
                break;
            case 6:
                pair = line.split("\\s+");
                // Took out expansion code - that was incorrect.
                sendCodesDuration.put(pair[0], pair[1]);
                break;
            case 7:
                dataQualifierCodes.put(line.trim(), 1);
                break;
            case 999:
                MAX_ERRORS = Integer.parseInt(line.trim());
                break;
            }
        }
    }
}
