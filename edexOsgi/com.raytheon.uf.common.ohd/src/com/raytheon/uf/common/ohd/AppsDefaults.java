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
package com.raytheon.uf.common.ohd;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.lang.reflect.Method;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import com.raytheon.uf.common.localization.ILocalizationPathObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;

/*
 * Created on Jul 10, 2003
 * 
 * Modified 12/28/04 to provide getInt() method.
 */

/*
 * @author Chip Gobs
 * 
 * This is the Java version of get_Apps_defaults.c
 */

/**
 * This class was adapted from ohd.hseb.util.AppsDefaults.
 * 
 * The constructor was modified to get the Apps_defaults files from the
 * localization server hierarchy.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 22, 2008            randerso     Initial creation
 * Apr  1, 2009            jelkins      added getTokens
 * Oct 19, 2012            bgonzale     App Context variable setup and token access.
 * Sep 22, 2014 3356       njensen      Fix constructor usage in viz finding VizApp class
 * Jan 26, 2016 5264       bkowal       Added validation of certain directories. Fixed
 *                                      warnings.
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */
public class AppsDefaults {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    private static final String Apps_defaults_FILENAME = Paths.get("hydro",
            "Apps_defaults").toString();

    private static final String RFR_OPEN = "$(";

    private static final String RFR_CLOSE = ")";

    private static final char DELIM = ':';

    private static final char COMMENT = '#';

    private static final char DOUBLE_QUOTE = '\"';

    private static final char SINGLE_QUOTE = '\'';

    private static final int RECUR_LIMIT = 40;

    private static Set<String> _trueSet = new HashSet<>();

    private static AppsDefaults instance;

    private static Class<?> dataDirClass;

    private final Properties _envProperties;

    private ILocalizationFile _appsDefaultsUserFile;

    private ILocalizationFile _appsDefaultsSiteFile;

    private ILocalizationFile _appsDefaultsNationalFile;

    private ILocalizationPathObserver appsDefaultsObserver;

    private Set<String> tokens = new HashSet<>();

    private Map<String, String> userMap;

    private Map<String, String> siteMap;

    private Map<String, String> baseMap;

    private static final Object LOCK = new Object();

    private static final String APP_CONTEXT = "APP_CONTEXT";

    static {
        _trueSet.add("true");
        _trueSet.add("on");
        _trueSet.add("yes");
        _trueSet.add("y");
        _trueSet.add("1");
    }

    /**
     * 
     */
    private AppsDefaults() {
        _envProperties = new Properties();
        this.checkAppsDefaults(System.getenv(), "Environment");
        _envProperties.putAll(System.getenv());

        /*
         * TODO this is nearly unmaintainable, figure out a better way to do it
         * that works in both CAVE and EDEX, perhaps through a combination of a
         * properties file and Spring. It's not even clear if apps_dir should be
         * coming from the data dir in CAVE or somewhere else, and it's not
         * clear how much it's used in CAVE.
         */
        if (_envProperties.get("EDEX_HOME") == null) {
            try {
                Method getDataDir = dataDirClass.getMethod("getDataDir");
                String shareDir = (String) getDataDir.invoke(null);
                _envProperties.put("apps_dir",
                        FileUtil.join(shareDir, "hydroapps"));
            } catch (Exception e) {
                statusHandler
                        .error("Failed to determine the location of the hydroapps share directory.",
                                e);
            }
        }

        updateDirectories();
        initialize();
    }

    private void initialize() {
        // Bypass the public update() method so we
        // force a file read.
        synchronized (LOCK) {
            Set<String> tokenSet = new HashSet<String>();
            userMap = new HashMap<>();
            if (_appsDefaultsUserFile.exists()) {
                statusHandler.info("USER file exists, updating map...");
                update(userMap, _appsDefaultsUserFile);
                tokenSet.addAll(userMap.keySet());
            }

            siteMap = new HashMap<>();
            if (_appsDefaultsSiteFile.exists()) {
                statusHandler.info("SITE file exists, updating map...");
                update(siteMap, _appsDefaultsSiteFile);
                tokenSet.addAll(siteMap.keySet());
            }

            baseMap = new HashMap<>();
            if (_appsDefaultsNationalFile.exists()) {
                statusHandler.info("BASE file exists, updating map...");
                update(baseMap, _appsDefaultsNationalFile);
                tokenSet.addAll(baseMap.keySet());
            }

            if (userMap.isEmpty() == false) {
                checkAppsDefaults(userMap, _appsDefaultsUserFile.toString());
            }
            if (siteMap.isEmpty() == false) {
                checkAppsDefaults(siteMap, _appsDefaultsSiteFile.toString());
            }
            if (baseMap.isEmpty() == false) {
                checkAppsDefaults(baseMap, _appsDefaultsNationalFile.toString());
            }

            tokens = tokenSet;
        }
    }

    private void checkAppsDefaults(Map<String, String> propertiesMap,
            String containingFile) {
        statusHandler.info("Validating Apps Defaults properties in: "
                + containingFile + " ...");
        for (String key : AppsDefaultsDirKeys.DIR_KEYS_TO_VALIDATE) {
            String valueToValidate = propertiesMap.get(key);
            if (valueToValidate == null) {
                continue;
            }
            if (AppsDefaultsDirKeys.validateDirectory(expandReferBacks(
                    valueToValidate, 0))) {
                continue;
            }
            /*
             * invalid value specified. sanitize the input and inform the users
             * that apps defaults requires an update.
             */
            propertiesMap.remove(key);
            StringBuilder sb = new StringBuilder("Illegal property: ")
                    .append(valueToValidate)
                    .append(" specified for Apps Defaults key: ")
                    .append(key)
                    .append(" in file: ")
                    .append(containingFile)
                    .append(". Property has been ignored. Valid values must map to a location within one of the specified directories: ")
                    .append(AppsDefaultsDirKeys.getValidRootsAsString())
                    .append(".");
            statusHandler.error(sb.toString());
        }
        statusHandler
                .info("Finished validation of Apps Defaults properties in: "
                        + containingFile + ".");
    }

    private void updateDirectories() {
        _appsDefaultsUserFile = null;
        _appsDefaultsSiteFile = null;
        _appsDefaultsNationalFile = null;

        IPathManager pm = PathManagerFactory.getPathManager();
        _appsDefaultsUserFile = pm.getLocalizationFile(pm.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.USER),
                Apps_defaults_FILENAME);
        if (_appsDefaultsUserFile.exists()) {
            statusHandler.info("Setting user Apps_defaults file: "
                    + _appsDefaultsUserFile);
        } else {
            statusHandler.info("No user Apps_defaults file found.");
        }

        _appsDefaultsSiteFile = pm.getLocalizationFile(pm.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE),
                Apps_defaults_FILENAME);
        if (_appsDefaultsSiteFile.exists()) {
            statusHandler.info("Setting site Apps_defaults file: "
                    + _appsDefaultsSiteFile);
        } else {
            statusHandler.warn("No site Apps_defaults file found.");
        }

        _appsDefaultsNationalFile = pm.getLocalizationFile(pm.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.BASE),
                Apps_defaults_FILENAME);
        if (_appsDefaultsNationalFile.exists()) {
            statusHandler.info("Setting base Apps_defaults file: "
                    + _appsDefaultsNationalFile);
        } else {
            statusHandler.error("No base Apps_defaults file found.");
        }

        if (this.appsDefaultsObserver == null) {
            appsDefaultsObserver = new ILocalizationPathObserver() {
                @Override
                public void fileChanged(ILocalizationFile file) {
                    statusHandler
                            .info("Detected change in Apps_defaults file: "
                                    + file.toString() + " ...");
                    initialize();
                }
            };
            pm.addLocalizationPathObserver(Apps_defaults_FILENAME,
                    appsDefaultsObserver);
        }
    }

    /**
     * 
     * @return
     */
    public static synchronized AppsDefaults getInstance() {
        if (instance == null) {
            instance = new AppsDefaults();
        }
        return instance;
    }

    /**
     * 
     * @param map
     * @param file
     */
    private void update(Map<String, String> map, ILocalizationFile file) {
        if (file != null && file.exists()) {
            BufferedReader reader = null;
            try {
                statusHandler.info("Reading " + file + " into AppsDefaults");
                reader = new BufferedReader(new InputStreamReader(
                        file.openInputStream()));
                String line = null;
                NameValuePair pair = null;

                while ((line = reader.readLine()) != null) {
                    pair = parseLine(line);
                    if (pair != null) {
                        if (pair.getValue() != null) {
                            map.put(pair.getName(), pair.getValue());
                        }
                    }
                }
            } catch (Exception e) {
                statusHandler.error("Error reading file " + file, e);
            } finally {
                try {
                    if (reader != null) {
                        reader.close();
                    }
                } catch (IOException e) {
                    statusHandler.error("Error closing file " + file, e);
                }
            }
        }
    }

    /**
     * 
     * @return a set of all tokens found in the Apps_defaults files
     */
    public Set<String> getTokens() {
        return tokens;
    }

    /**
     * Get the value of a specified token.
     * 
     * @param tokenName
     *            The name of the token.
     * @param defaultValue
     *            The default string value of the token.
     * @return The value of the token, or its default value.
     */
    public int getInt(String tokenName, int defaultValue) {
        int returnValue = defaultValue;

        try {
            String tokenValue = getToken(tokenName);
            if (tokenValue != null) {
                returnValue = Integer.parseInt(tokenValue);
            }
        } catch (Throwable e) {
            returnValue = defaultValue;
        }

        return returnValue;
    }

    /**
     * Get the double value of a specified token.
     * 
     * @param tokenName
     *            The name of the token.
     * @param defaultValue
     *            The default string value of the token.
     * @return The value of the token, or its default value.
     */
    public double getDouble(String tokenName, double defaultValue) {
        double returnValue = defaultValue;

        try {
            String tokenValue = getToken(tokenName);
            if (tokenValue != null) {
                returnValue = Double.parseDouble(tokenValue);
            }
        } catch (Throwable e) {
            returnValue = defaultValue;
        }

        return returnValue;
    }

    /**
     * Get the float value of a specified token.
     * 
     * @param tokenName
     *            The name of the token.
     * @param defaultValue
     *            The default string value of the token.
     * @return The value of the token, or its default value.
     */
    public float getFloat(String tokenName, float defaultValue) {
        float returnValue = defaultValue;

        try {
            String tokenValue = getToken(tokenName);
            if (tokenValue != null) {
                returnValue = Float.parseFloat(tokenValue);
            }
        } catch (Throwable e) {
            returnValue = defaultValue;
        }

        return returnValue;
    }

    /**
     * Get the boolean value of a specified token.
     * 
     * @param tokenName
     *            The name of the token.
     * @param defaultValue
     *            The default string value of the token.
     * @param trueValueString
     *            A value to be used to signify true.
     * @return The value of the token, or its default value.
     */
    public boolean getBoolean(String tokenName, boolean defaultValue,
            String trueValueString) {
        _trueSet.add(trueValueString);
        return getBoolean(tokenName, defaultValue);
    }

    /**
     * Get the boolean value of a specified token.
     * 
     * @param tokenName
     *            The name of the token.
     * @param defaultValue
     *            The default string value of the token.
     * @return The value of the token, or its default value.
     */
    public boolean getBoolean(String tokenName, boolean defaultValue) {
        boolean returnValue = defaultValue;

        try {
            String tokenValue = getToken(tokenName);
            if (tokenValue != null) {
                if (_trueSet.contains(tokenValue.toLowerCase())) {
                    returnValue = true;
                } else {
                    returnValue = false;
                }

            }
        } catch (Throwable e) {
            returnValue = defaultValue;
        }

        return returnValue;
    }

    // -----------------------------------------------------

    /**
     * Get the value of a specified token.
     * 
     * @param tokenName
     *            The name of the token.
     * @param defaultValue
     *            The default string value of the token.
     * @return The value of the token, or its default value.
     */
    public String getToken(String tokenName, String defaultValue) {
        String tokenValue = getToken(tokenName);
        if (tokenValue == null) {
            tokenValue = defaultValue;
        }
        return tokenValue;
    }

    // -----------------------------------------------------

    /**
     * 
     * @param tokenName
     *            the token to get. if null, a list of tokens will be written to
     *            the tokens set.
     * @param recursionCount
     * @return
     */
    public String getToken(String tokenName) {
        String tokenValue = null;

        String envValue = null;
        if (tokenName != null) {
            envValue = _envProperties.getProperty(tokenName);
        }

        // if token is available as an environment variable, use its value
        if (envValue != null) {
            tokenValue = envValue;
        } else {
            String value = null;
            synchronized (LOCK) {
                value = userMap.get(tokenName);
                if (value == null) {
                    value = siteMap.get(tokenName);
                    if (value == null) {
                        value = baseMap.get(tokenName);
                    }
                }
            }
            tokenValue = value;
        } // end else

        tokenValue = expandReferBacks(tokenValue, 0);

        return tokenValue;
    } // end getToken()

    // -----------------------------------------------------
    private String expandReferBacks(String tokenValue, int recursionCount) {
        if (tokenValue != null) {
            while (thereAreReferBacks(tokenValue)) {
                tokenValue = expandFirstReferBack(tokenValue, recursionCount);
            }
        }

        return tokenValue;
    }

    // -----------------------------------------------------

    /**
     * Should this token value be expanded?
     * 
     * @param tokenValue
     *            A token value to inspect.
     * @return Should this token value be expanded?
     */
    private boolean thereAreReferBacks(String tokenValue) {
        boolean result = false;

        if (tokenValue.indexOf(RFR_OPEN) > -1) {
            result = true;
        }

        return result;
    }

    // -----------------------------------------------------

    /**
     * 
     */
    private String expandFirstReferBack(String tokenValue, int recursionCount) {

        int referBackStartIndex = tokenValue.indexOf(RFR_OPEN);
        int referBackEndIndex = tokenValue.indexOf(RFR_CLOSE);
        String beginning = "";
        String middle = null;
        String newTokenName = null;
        String end = "";

        if ((referBackStartIndex > -1) && (referBackEndIndex > -1)) {
            if (referBackStartIndex > 0) {
                beginning = tokenValue.substring(0, referBackStartIndex);
            }

            newTokenName = tokenValue.substring(
                    referBackStartIndex + RFR_OPEN.length(), referBackEndIndex);

            recursionCount++;

            if (recursionCount <= RECUR_LIMIT) {
                middle = getToken(newTokenName);
                recursionCount--;
            } else {
                middle = "ERROR_ERROR_ERROR";
                statusHandler
                        .error("You probably have a cycle in your Apps Defaults File's refer backs, please check it");
            }
            if ((referBackEndIndex + RFR_CLOSE.length()) < tokenValue.length()) {
                end = tokenValue.substring(
                        referBackEndIndex + RFR_CLOSE.length(),
                        tokenValue.length());
            }

            tokenValue = beginning + middle + end;
        }

        return tokenValue;
    }

    // -----------------------------------------------------

    /**
     * Parse one line of data from an Apps_Defaults file.
     * 
     * @param line
     *            The text of a single line of data.
     * @return
     */
    private NameValuePair parseLine(String line) {
        NameValuePair pair = null;
        int delimiterIndex = -1;

        String tokenName = null;
        String tokenValue = null;

        // find delimiter
        delimiterIndex = line.indexOf(DELIM);
        if (delimiterIndex > -1) {
            // there is a delimiter character on the line
            // find tokenName
            tokenName = findTokenName(line, delimiterIndex);

            if (tokenName != null) {
                tokenValue = findTokenValue(line, delimiterIndex);
                if (tokenValue != null) {
                    pair = new NameValuePair(tokenName, tokenValue);
                }
            }
            // end if found a delimiter
        } else {
            // there is no delimiter, so can't read a pair from this line
            pair = null;
        }

        return pair;
    }

    // -----------------------------------------------------

    /**
     * 
     */
    private String findTokenName(String line, int delimiterIndex) {
        String tokenName = null;
        boolean foundTokenName = false;
        boolean foundStartOfTokenName = false;
        boolean foundComment = false;
        StringBuilder tokenNameBuffer = new StringBuilder();

        for (int i = 0; ((i < delimiterIndex) && (!foundTokenName))
                && (!foundComment); i++) {
            char c = line.charAt(i);
            if (isWhiteSpace(c)) {
                // check to see if this is white space at the beginning or the
                // end
                // of the tokenName
                if (!foundStartOfTokenName) {
                    // this must beginning whitespace
                    // so ignore the whitespace
                } else {
                    // must be trailing whitespace
                    // the token is done;
                    tokenName = tokenNameBuffer.toString();
                    foundTokenName = true;
                }
                // end if isWhiteSpace
            } else if (isCommentStarter(c)) {
                // There can't be a valid tokenName, tokenValue pair here, then
                foundComment = true;

                // clear out the tokenNameVariables
                tokenName = null;

                // works in >= java 1.2,
                // tokenNameBuffer.delete(0, tokenNameBuffer.length());

                // works in java < 1.2, but the previous line is prefered
                tokenNameBuffer = new StringBuilder();
                foundStartOfTokenName = false;
                break; // exit loop
                // end isCommentStarter
            } else {
                // part of the tokenName
                tokenNameBuffer.append(c);
                foundStartOfTokenName = true;
            }

        } // end for

        if (foundStartOfTokenName) {
            tokenName = tokenNameBuffer.toString();
        }

        return tokenName;
    }

    // ----------------------------------------------------------------------

    private String findTokenValue(String line, int delimiterIndex) {
        String tokenValue = null;

        boolean foundTokenValue = false;
        boolean foundStartOfTokenValue = false;
        boolean foundComment = false;

        boolean foundSingleOpenQuote = false;
        boolean foundSingleCloseQuote = false;

        boolean foundDoubleOpenQuote = false;
        boolean foundDoubleCloseQuote = false;

        boolean error = false;

        StringBuffer tokenValueBuffer = new StringBuffer();

        for (int i = delimiterIndex + 1; ((i < line.length()) && (!foundTokenValue))
                && (!foundComment); i++) {
            char c = line.charAt(i);
            if (isWhiteSpace(c)) {
                // check to see if this is white space at the beginning or the
                // end
                // of the tokenValue
                if (!foundStartOfTokenValue) // this must be beginning
                // whitespace
                {
                    // so ignore the whitespace
                } else if ((foundSingleOpenQuote) && (!foundSingleCloseQuote)) {
                    tokenValueBuffer.append(c);
                    foundStartOfTokenValue = true;
                } else if ((foundDoubleOpenQuote) && (!foundDoubleCloseQuote)) {
                    tokenValueBuffer.append(c);
                    foundStartOfTokenValue = true;
                } else // must be trailing whitespace
                {
                    // the token value reading is done;
                    tokenValue = tokenValueBuffer.toString();
                    foundTokenValue = true;
                }
            } // end if isWhiteSpace

            else if (isCommentStarter(c)) {
                if (foundStartOfTokenValue) {
                    // this character is allowed in a tokenValue
                    tokenValueBuffer.append(c);
                } else { // error, there can't be a valid tokenValue
                    foundComment = true;

                    // clear out the tokenNameVariables
                    tokenValue = null;

                    // works in >= java 1.2,
                    // tokenValueBuffer.delete(0, tokenValueBuffer.length());

                    // works in java < 1.2, but the previous line is prefered
                    tokenValueBuffer = new StringBuffer();

                    error = true;
                }
            } // end isCommentStarter

            else if (isDelimiter(c)) {
                if (foundStartOfTokenValue) {
                    // this character is allowed in a tokenValue
                    tokenValueBuffer.append(c);
                } else { // error, there can't be a valid tokenValue

                    // clear out the tokenNameVariables
                    tokenValue = null;

                    // works in >= java 1.2,
                    // tokenValueBuffer.delete(0, tokenValueBuffer.length());

                    // works in java < 1.2, but the previous line is prefered
                    tokenValueBuffer = new StringBuffer();

                    error = true;
                    break; // exit loop
                }

            } else if (isSingleQuote(c)) {
                if (foundSingleOpenQuote) {
                    foundSingleCloseQuote = true;
                    foundTokenValue = true; // done
                } else {
                    foundSingleOpenQuote = true;
                }
            }

            else if (isDoubleQuote(c)) {
                if (foundDoubleOpenQuote) {
                    foundDoubleCloseQuote = true;
                    foundTokenValue = true; // done
                } else {
                    foundDoubleOpenQuote = true;
                }
            }

            else // part of the tokenValue
            {
                tokenValueBuffer.append(c);
                foundStartOfTokenValue = true;
            }

        } // end for

        if ((foundStartOfTokenValue) && (!error)) {
            tokenValue = tokenValueBuffer.toString();
        }

        return tokenValue;
    }

    // -----------------------------------------------------

    // -----------------------------------------------------

    //
    private static boolean isWhiteSpace(char c) {
        boolean result = false;

        if ((c == ' ') || (c == '\t')) {
            result = true;
        }

        return result;

    } // isWhiteSpace

    // -----------------------------------------------------
    private static boolean isCommentStarter(char c) {
        boolean result = false;

        if (c == COMMENT) {
            result = true;
        }

        return result;
    } // isCommentStarter

    // -------------------------------------------------------------
    private static boolean isDelimiter(char c) {
        boolean result = false;

        if (c == DELIM) {
            result = true;

        }

        return result;
    } // isDelimiter

    // -----------------------------------------------------------

    private static boolean isSingleQuote(char c) {
        boolean result = false;

        if (c == SINGLE_QUOTE) {
            result = true;
        }

        return result;
    } // isSingleQuote

    // -----------------------------------------------------

    private static boolean isDoubleQuote(char c) {
        boolean result = false;

        if (c == DOUBLE_QUOTE) {
            result = true;
        }

        return result;

    } // isDoubleQuote

    /**
     * <pre>
     * Set context using the callingContext in the APP_CONTEXT variable.
     *    if a token for the context is defined and is ON 
     *       then log run and return true.
     * 
     * If the token is defined and is OFF
     *    then log not run and return false.
     * </pre>
     * 
     * Used by java directly initiated by a cron.
     * 
     * @param callingContext
     *            calling context for the application.
     * @return true if context is ON; false otherwise.
     */
    public boolean setAppContext(Object callingContext) {
        String contextVar = setAppContextVar(callingContext, false);
        boolean isOn = getBoolean(contextVar, true);

        if (statusHandler.isPriorityEnabled(Priority.WARN)) {
            StringBuilder sb = new StringBuilder(
                    "App Execution Token for App Context ");
            sb.append(contextVar);
            sb.append(" is ");
            sb.append(isOn);
            statusHandler.warn(sb.toString());
        }

        return isOn;
    }

    /**
     * <pre>
     * If no token is defined for the expected app context
     *    or if the token is defined and is ON 
     *       then log run and return true.
     * 
     * If the token is defined and is OFF
     *    then log not run and return false.
     * </pre>
     * 
     * Used by java that was not directly initiated by a cron.
     * 
     * @param callingContext
     *            calling context for the application.
     * @return true if context is ON; false otherwise.
     */
    public boolean checkAppContext(Object callingContext) {
        String contextVar = setAppContextVar(callingContext, true);
        boolean isOn = getBoolean(contextVar, true);

        if (statusHandler.isPriorityEnabled(Priority.WARN)) {
            StringBuilder sb = new StringBuilder(
                    "App Execution Token for App Context ");
            sb.append(contextVar);
            sb.append(" is ");
            sb.append(isOn);
            statusHandler.warn(sb.toString());
        }

        return isOn;
    }

    /**
     * Set and return the APP_CONTEXT variable.
     * 
     * <pre>
     * Example:
     *    APP_CONTEXT for class C called by class B 
     *    which was called by class A:
     *       ClassA___ClassB___ClassC
     * </pre>
     * 
     * @param callingContext
     * @param useParentContext
     * @return context name
     */
    private String setAppContextVar(Object callingContext,
            boolean useParentContext) {
        String context = callingContext.getClass().getSimpleName();
        if (useParentContext) {
            String existingContext = System.getProperty(APP_CONTEXT, context);
            // Check if this context is a part of the existing context
            if (!existingContext.endsWith(context)) {
                StringBuilder sb = new StringBuilder(existingContext);
                context = sb.append(".").append(context).toString();
            } else {
                context = existingContext;
            }
        }
        System.setProperty(APP_CONTEXT, context);
        return context;
    }

    /**
     * Set the APP_CONTEXT variable in the processBuilder environment.
     * 
     * <pre>
     * Example:
     *    APP_CONTEXT for class C called by class B 
     *    which was called by class A:
     *       ClassA___ClassB___ClassC
     * </pre>
     * 
     * @param processBuilder
     */
    public void setAppContext(ProcessBuilder processBuilder) {
        String appContextVar = System.getProperty(APP_CONTEXT);
        if (appContextVar != null) {
            processBuilder.environment().put(APP_CONTEXT, appContextVar);
        }
    }

    /**
     * Sets the class used to determine the data dir, if necessary. See
     * AppsDefaults's private constructor. At present only intended for use by
     * CAVE since edex receives a corresponding environment variable.
     * 
     * @param dataDirClass
     */
    public static void setDataDirClass(Class<?> dataDirClass) {
        AppsDefaults.dataDirClass = dataDirClass;
    }

} // end class AppsDefaults
