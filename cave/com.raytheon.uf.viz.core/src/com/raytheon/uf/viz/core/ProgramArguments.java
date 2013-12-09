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
package com.raytheon.uf.viz.core;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.Platform;

/**
 * Class that parses application arguments and provides easy access to them
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 10, 2009            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class ProgramArguments {

    private static ProgramArguments instance = new ProgramArguments();

    private Map<String, Object> argumentMap;

    public static ProgramArguments getInstance() {
        return instance;
    }

    private ProgramArguments() {
        argumentMap = new HashMap<String, Object>();
        parseArgs();
    }

    private void parseArgs() {
        String[] args = Platform.getApplicationArgs();
        for (int i = 0; i < args.length; ++i) {
            String arg = args[i];
            if (arg.startsWith("-")) {
                // we have a key
                if (args.length > (i + 1)
                        && args[i + 1].startsWith("-") == false) {
                    argumentMap.put(arg, args[i + 1]);
                    ++i;
                } else {
                    argumentMap.put(arg, new Boolean(true));
                }
            }
        }
    }

    public String getString(String key) {
        Object val = argumentMap.get(key);
        if (val != null) {
            return val.toString();
        }
        return null;
    }

    public Boolean getBoolean(String key) {
        Object val = argumentMap.get(key);
        if (val == null) {
            return new Boolean(false);
        }
        return (Boolean) val;
    }

    public Double getDouble(String key) {
        String s = getString(key);
        try {
            return Double.parseDouble(s);
        } catch (Throwable t) {
            return null;
        }
    }

    public Integer getInteger(String key) {
        String s = getString(key);
        try {
            return Integer.parseInt(s);
        } catch (Throwable t) {
            return null;
        }
    }

    public void addString(String key, String value) {
        argumentMap.put(key, value);
    }
}
