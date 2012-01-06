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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

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
 * 04/18/2008   387        M. Duff     Initial Version.	
 * 
 * </pre>
 * 
 * @author mduff
 * @version 1.0
 */

public class ShefParm {
	static {
        log = LogFactory.getLog(com.raytheon.edex.plugin.shef.util.ShefParm.class);
        physicalElementConversion = new HashMap<String, Double>();
        durationCodeValues = new HashMap<String, Short>();
        typeSourceCodes = new HashMap<String, Integer>();
        extremumCodes = new HashMap<String, Integer>();
        probabilityCodeValues = new HashMap<String, Float>();
        sendCodesDuration = new HashMap<String, String>();
        dataQualifierCodes = new HashMap<String, Integer>();
        MAX_ERRORS = 0;
		populate();
	}
	
	private static Map<String, Double> physicalElementConversion;
	private static Map<String, Short> durationCodeValues;
	private static Map<String, Integer> typeSourceCodes;
	private static Map<String, Integer> extremumCodes;
	private static Map<String, Float> probabilityCodeValues;
	private static Map<String, String> sendCodesDuration;
	private static Map<String, Integer> dataQualifierCodes;
	private static int MAX_ERRORS;
    private static final Log log;
    private static final String PLUGIN_NAME = "shef";
    private static final String PROPFILE_NAME = "SHEFPARM";
    
    private static int fileSection = 0;

    /**
	 * Get a Physical Element conversion factor
	 * 
	 * @param key - Physical Element
	 * @return - the conversion factor
	 */
	public static Double getPhysicalElementConversionFactor(String key) {
		return physicalElementConversion.get(key);
	}

	/**
	 * Get a Duration Code value
	 * 
	 * @param key - Duration Code
	 * @return - the Duration Code value
	 */
	public static Short getDurationCodeValue(String key) {
		return durationCodeValues.get(key);
	}
	
	/**
	 * Check the Type Source Code
	 * 
	 * @param key - Type Source
	 * @return - 1 if valid code, null if invalid
	 */
	public static Integer getTypeSourceCode(String key) {
		return typeSourceCodes.get(key);
	}
	
	/**
	 * Check the Extremum Code
	 * 
	 * @param key - Extremum Code
	 * @return - 1 if valid code, null if invalid
	 */
	public static Integer getExtremumCode(String key) {
		return extremumCodes.get(key);
	}
	
	/**
	 * Get the Probability Code Value
	 * 
	 * @param key - Probability Code
	 * @return - Probability Code's value
	 */
	public static Float getProbabilityCodeValue(String key) {
		return probabilityCodeValues.get(key);
	}
	
	/**
	 * Get a Send Code or Duration default value for these special cases
	 * 
	 * @param key - Code
	 * @return - Default Values
	 */
	public static String getSendCodeDurationDefaults(String key) {
		return sendCodesDuration.get(key);
	}
	
	/**
	 * Check the Data Qualifier Code
	 * 
	 * @param key - Extremum Code
	 * @return - 1 if valid code, null if invalid
	 */
	public static Integer getDataQualifierCodes(String key) {
		return dataQualifierCodes.get(key);
	}
	
	/**
	 * Get the maximum number of errors defined
	 * 
	 * @return - the maximum number of errors
	 */
	public static Integer getMaxErrors() {
		return MAX_ERRORS;
	}
	
	/**
	 * Populate the values from the file.
	 */
	private static void populate() {
	    
        PathManager pathMgr = (PathManager) PathManagerFactory.getPathManager();

        LocalizationContext ctx = pathMgr.getContext(LocalizationType.COMMON_STATIC, LocalizationLevel.BASE);
        File baseDir = pathMgr.getFile(ctx, PLUGIN_NAME);
        File srcFile = new File(baseDir, PROPFILE_NAME);

        BufferedReader in = null;
        try {
            in = new BufferedReader(new FileReader(srcFile));
            String line = null;
            while ((line = in.readLine()) != null) {
                processLine(line);
            }
            in.close();
        } catch (IOException ioe) {
            ioe.printStackTrace();
            log.error("Error loading " + PROPFILE_NAME);
        }
	}
	
	private static String expandPE(String pe) {
	    // 0123456
	    // ADZZZZZ
	    StringBuilder peCode = new StringBuilder("--IRZZ");
	    if((pe != null)&&(pe.length() >= 2)) {
	        for(int i = 0;i < pe.length() && (i < peCode.length());i++) {
	            peCode.setCharAt(i,pe.charAt(i));
	        }
	    }
	    char z4 = peCode.charAt(3);
        char z5 = peCode.charAt(5);
	    if('Z' == z4) {
	        if('Z' == z5) {
	            peCode.setCharAt(3, 'R');
	        } else {
	            // FIXME: This is an error
	        }
	    }
	    
	    return peCode.toString();
	}
	
	private static void processLine(String line) {
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
            if (line.equals("SHEFPARM")) {
                return;
            }
            switch (fileSection) {
            case 1:
                pair = line.split("\\s+");
                physicalElementConversion.put(pair[0], Double
                        .parseDouble(pair[1]));
                break;
            case 2:
                pair = line.split("\\s+");
                durationCodeValues.put(pair[0], Short
                        .parseShort(pair[1]));
                break;
            case 3:
                pair = line.split("\\s+");
                typeSourceCodes.put(pair[0], Integer
                        .parseInt(pair[1]));
                break;
            case 4:
                pair = line.split("\\s+");
                extremumCodes.put(pair[0], Integer
                        .parseInt(pair[1]));
                break;
            case 5:
                pair = line.split("\\s+");
                probabilityCodeValues.put(pair[0], Float
                        .parseFloat(pair[1]));
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

	public static final void main(String [] args) {
	    
	    
	    String s = expandPE("ADZZZZZ");
	    System.out.println(s);
	}
}
