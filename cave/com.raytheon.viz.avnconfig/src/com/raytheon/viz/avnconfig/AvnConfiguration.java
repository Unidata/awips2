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
package com.raytheon.viz.avnconfig;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;

import org.apache.commons.configuration.ConfigurationException;
import org.apache.commons.configuration.HierarchicalINIConfiguration;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.StringUtil;
import com.raytheon.uf.viz.core.localization.HierarchicalPreferenceStore;
import com.raytheon.viz.avnconfig.AvnConfigConstants.DataSource;
import com.raytheon.viz.avnconfig.AvnConfigConstants.RuleType;

/**
 * Contains the configuration for AVNFPS monitoring rules.
 * 
 * A "master copy" of the configuration is kept in memory for performance
 * reasons for dispatching alerts. This copy is read only...and is reloaded when
 * a non-master copy performs a save.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 12, 2009            avarani     Initial creation
 * Oct 29, 2010 7262       rferrel     Replaced depreciated class, improved
 *                                     parsing and allow msg & comment to have
 *                                     commas in the text.
 * Sep 27, 2011 10958      rferrel     Added checks for required fields in
 *                                     configuration files.
 * 
 * </pre>
 * 
 * @author avarani
 * @version 1.0
 */

public class AvnConfiguration {

    /**
     * To maintain compatibility with AWIPS I in the rules configuration files
     * the severity value must be at least 2.
     */
    private final static int MINIMUM_SEVERITY = 2;

    /**
     * The master-copy instance
     */
    private static AvnConfiguration masterCopy;

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AvnConfiguration.class);

    private ArrayList<MethodData> metarRules;

    private ArrayList<MethodData> ltgRules;

    private ArrayList<MethodData> rltgRules;

    private ArrayList<MethodData> gridsRules;

    private ArrayList<MethodData> ccfpRules;

    private ArrayList<MethodData> llwsRules;

    /**
     * Get a list of available rules for a given data source
     * 
     * @param source
     *            DataSource
     * @return ArrayList of available rules
     */
    public ArrayList<MethodData> getRules(DataSource source) {
        switch (source) {
        case mtrs:
            return metarRules;
        case ltg:
            return ltgRules;
        case rltg:
            return rltgRules;
        case grids:
            return gridsRules;
        case ccfp:
            return ccfpRules;
        case llws:
            return llwsRules;
        default:
            return null;
        }
    }

    /**
     * Set the list of available rules for a given source
     * 
     * @param source
     *            DataSource
     * @param rules
     *            ArrayList of available rules
     */
    private void setRules(DataSource source, ArrayList<MethodData> rules) {
        switch (source) {
        case mtrs:
            metarRules = rules;
            break;
        case ltg:
            ltgRules = rules;
            break;
        case rltg:
            rltgRules = rules;
            break;
        case grids:
            gridsRules = rules;
            break;
        case ccfp:
            ccfpRules = rules;
            break;
        case llws:
            llwsRules = rules;
            break;
        }
    }

    /**
     * Get the key's string array from store and combine into a comma separated
     * list.
     * 
     * @param store
     * @param key
     * @return value
     */
    private String getString(HierarchicalPreferenceStore store, String key) {
        String[] array = store.getStringArray(key);
        String prefix = "";
        StringBuffer value = new StringBuffer();
        for (String s : array) {
            value.append(prefix).append(s);
            prefix = ", ";
        }
        return value.toString();
    }

    /**
     * Load in information for each rule method in DataSource. Information is
     * used to populate the Rule Editor section.
     */
    private void reload() {
        HierarchicalPreferenceStore store = Activator.getDefault()
                .getPreferenceStore();

        // Load available rules lists
        for (DataSource source : DataSource.values()) {
            String[] Methods = source.getMethods();
            ArrayList<MethodData> list = new ArrayList<MethodData>();

            for (String method : Methods) {
                String keyBase = "rules." + source.toString() + "." + method;
                String type = store.getString(keyBase + ".type");
                String unique = store.getString(keyBase + ".unique");
                String[] args = store.getStringArray(keyBase + ".args");
                // The comment and msg may have commas in them.
                String msg = getString(store, keyBase + ".msg");
                String comment = getString(store, keyBase + ".comment");
                MethodData newMethod = new MethodData(method, comment, msg,
                        RuleType.valueOf(type), unique.equals("1"));
                String[] argValues = store.getStringArray(keyBase
                        + ".defaultValues");
                ArrayList<MethodArgData> methodArgsArray = new ArrayList<MethodArgData>();

                if (args.length == argValues.length) {
                    for (int i = 0; i < args.length; i++) {
                        methodArgsArray.add(new MethodArgData(args[i],
                                argValues[i].replace(';', ',')));
                    }
                } else {
                    System.err.println("Error in " + keyBase
                            + ": args array does not match argValues array.");
                }

                newMethod.setMethodArgsArray(methodArgsArray);
                list.add(newMethod);
            }

            setRules(source, list);
        }
    }

    /**
     * Load from preferences
     * 
     * @param isMasterCopy
     *            true if master copy (read only, automatically updated)
     * @return the configuration
     */
    public static AvnConfiguration load(boolean isMasterCopy) {
        if (isMasterCopy && masterCopy != null) {
            return masterCopy;
        }

        AvnConfiguration configuration = new AvnConfiguration();
        configuration.reload();

        if (isMasterCopy) {
            masterCopy = configuration;
        }

        return configuration;
    }

    /**
     * Save the monitoring rules to a localized SITE configuration file.
     * 
     * @param site
     *            - site ID rules are for.
     * @param source
     *            - the kind of data source
     * @param data
     *            - The array of rules
     * @throws ConfigurationException
     * @throws IOException
     * @throws LocalizationOpFailedException
     */
    public void setRules(String site, DataSource source,
            ArrayList<MethodData> data) throws ConfigurationException,
            IOException, LocalizationOpFailedException {
        String filepath = "aviation/config/tafs/" + site + "/"
                + source.getFilename();
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext context = pm.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.SITE);
        LocalizationFile lFile = pm.getLocalizationFile(context, filepath);
        File file = lFile.getFile();

        if (!file.getParentFile().exists()) {
            file.getParentFile().mkdirs();
        }

        HierarchicalINIConfiguration config = new HierarchicalINIConfiguration(
                file);
        config.setDelimiterParsingDisabled(true);
        int numActiveRules = 0;

        for (MethodData method : data) {
            String key = "rule_" + numActiveRules;
            config.setProperty(key + ".method", method.getMethodName());
            config.setProperty(key + ".msg", method.getMessage());
            config.setProperty(key + ".type", method.getType().toString());
            config.setProperty(key + ".unique",
                    Boolean.toString(method.getUnique()));
            config.setProperty(key + ".severity",
                    Integer.toString(method.getSeverity()));

            if (method.getMsgFromFile()) {
                config.setProperty(key + ".msgfromfile", Boolean.toString(true));
            }

            ArrayList<MethodArgData> args = method.getMethodArgsArray();

            for (MethodArgData arg : args) {
                config.setProperty(key + "." + arg.getArgName(),
                        arg.getArgValue());
            }

            numActiveRules++;
        }

        String activeRules = "";

        for (int i = 0; i < numActiveRules; i++) {
            activeRules += i;

            if (i != (numActiveRules - 1)) {
                activeRules += ",";
            }
        }

        config.setProperty("rules.active", activeRules);

        FileWriter writer = new FileWriter(file);
        config.save(writer);
        writer.close();
        lFile.save();
    }

    /**
     * Get an array of monitoring rules from the localize SITE file.
     * 
     * @param site
     *            - site ID rules are for.
     * @param source
     *            - The desired type of rules
     * @param maxSeverity
     *            - The maximum serverity allowed.
     * @return rules
     * @throws ConfigurationException
     * @throws IOException
     */
    public ArrayList<MethodData> getRules(String site, DataSource source,
            final int maxSeverity) throws ConfigurationException, IOException {
        ArrayList<MethodData> rules = new ArrayList<MethodData>();
        String filepath = "aviation/config/tafs/" + site + "/"
                + source.getFilename();
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationFile lFile = pm.getStaticLocalizationFile(filepath);

        if (lFile == null) {
            if (site.equals("XXXX")) {
                throw new IOException(
                        "Error: default monitoring rules not found.");
            } else {
                return getRules("XXXX", source, maxSeverity);
            }
        }

        File file = lFile.getFile();

        if (!file.exists()) {
            return getRules("XXXX", source, maxSeverity);
        } else {
            MethodData defaultRule = null;
            ArrayList<MethodData> defaultRules = getRules(source);
            HierarchicalINIConfiguration config = new HierarchicalINIConfiguration(
                    file);
            config.setDelimiterParsingDisabled(true);
            String aRules = config.getString("rules.active");
            if (aRules == null || aRules.trim().isEmpty()) {
                throw new ConfigurationException(file.getName()
                        + ", no list of active rules");
            }

            String[] activeRules = StringUtil.split(
                    config.getString("rules.active"), ",");

            for (String activeRule : activeRules) {
                String key = "rule_" + activeRule;
                String method = config.getString(key + ".method");

                if (method == null) {
                    throw new ConfigurationException(file.getName()
                            + " unable to find [" + key + "]");
                }

                for (MethodData aMethod : defaultRules) {
                    if (aMethod.getMethodName().equals(method)) {
                        defaultRule = aMethod;
                        break;
                    }
                }

                String msg = config.getString(key + ".msg");
                if (msg == null) {
                    msg = "";
                }
                String type = config.getString(key + ".type");
                String unique = config.getString(key + ".unique");

                if (unique == null) {
                    unique = Boolean.toString(defaultRule.getUnique());
                } else if (unique.equals("0") || unique.startsWith("F")
                        || unique.startsWith("f")) {
                    unique = "False";
                } else {
                    unique = "True";
                }

                if (type == null) {
                    type = defaultRule.getType().name();
                }

                MethodData newMethod = new MethodData(method,
                        defaultRule.getComment(), msg, RuleType.valueOf(type),
                        Boolean.parseBoolean(unique));
                String severity = config.getString(key + ".severity");
                String errMsg = null;
                Priority priority = Priority.ERROR;
                int sevIndex = -1;
                if (severity == null) {
                    sevIndex = defaultRule.getSeverity();
                    if (sevIndex < MINIMUM_SEVERITY) {
                        errMsg = String
                                .format("File \"%s\" missing severity in rule %s and default is bad value %d; using %d.",
                                        file.getAbsoluteFile(), key, sevIndex,
                                        MINIMUM_SEVERITY);
                        sevIndex = MINIMUM_SEVERITY;
                    } else if (sevIndex > maxSeverity) {
                        errMsg = String
                                .format("File \"%s\" missing severity in rule %s and default is bad value %d; using %d."
                                        + "\nThis may be caused by SyntaxMonitorCfg.xml not having enough colors listed in the tag <MonitorColors>",
                                        file.getAbsoluteFile(), key, sevIndex,
                                        maxSeverity);
                        sevIndex = maxSeverity;
                    } else {
                        priority = Priority.INFO;
                        errMsg = String
                                .format("File \"%s\" missing severity in rule %s using default %d",
                                        file.getAbsoluteFile(), key, sevIndex);
                    }
                } else {
                    try {
                        sevIndex = Integer.valueOf(severity);
                        if (sevIndex < MINIMUM_SEVERITY) {
                            errMsg = String
                                    .format("File \"%s\" in rule %s bad severity value %d; using %d.",
                                            file.getAbsoluteFile(), key,
                                            sevIndex, MINIMUM_SEVERITY);
                            sevIndex = MINIMUM_SEVERITY;
                        } else if (sevIndex > maxSeverity) {
                            errMsg = String
                                    .format("File \"%s\" in rule %s bad severity value %d; using %d."
                                            + "\nThis may be caused by SyntaxMonitorCfg.xml not having enough colors listed in the tag <MonitorColors>",
                                            file.getAbsoluteFile(), key,
                                            sevIndex, maxSeverity);
                            sevIndex = maxSeverity;
                        }
                    } catch (NumberFormatException ex) {
                        errMsg = String
                                .format("File \"%s\" in rule %s bad severity value %s; using %d.",
                                        file.getAbsoluteFile(), key, severity,
                                        MINIMUM_SEVERITY);
                        sevIndex = MINIMUM_SEVERITY;
                    }
                }

                newMethod.setSeverity(sevIndex);
                if (errMsg != null) {
                    statusHandler.handle(priority, errMsg);
                }
                String msgfromfile = config.getString(key + ".msgfromfile");

                if (msgfromfile != null) {
                    newMethod.setMsgFromFile(Boolean.parseBoolean(msgfromfile));
                }

                // Should always be a default value for the optional arguments.
                ArrayList<String> args = defaultRule.getArgs();
                ArrayList<String> defaultArgValues = defaultRule.getArgValues();

                for (int i = 0; i < args.size(); i++) {
                    String arg = args.get(i);
                    String argKey = key + "." + arg;
                    String argValue = "";

                    if (arg.equalsIgnoreCase("wx")) {
                        String[] argValues = StringUtil.split(
                                config.getString(argKey), ",");

                        StringBuffer argBuffer = new StringBuffer();
                        for (int j = 0; j < argValues.length; j++) {
                            if (argValues[j] == null || argValues[j].equals("")) {
                                continue;
                            }

                            argBuffer.append(argValues[j]).append(",");
                        }
                        argValue = argBuffer.toString();
                    } else {
                        argValue = config.getString(argKey);
                        if (argValue == null) {
                            argValue = defaultArgValues.get(i);
                        }
                    }

                    newMethod.addArgument(new MethodArgData(arg, argValue));
                }

                rules.add(newMethod);
            }
        }

        return rules;
    }
}
