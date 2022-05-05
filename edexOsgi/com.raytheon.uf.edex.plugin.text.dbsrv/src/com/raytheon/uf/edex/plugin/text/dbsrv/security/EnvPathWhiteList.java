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
package com.raytheon.uf.edex.plugin.text.dbsrv.security;

import java.io.File;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXB;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * This manages a list of directory paths associated with an environment
 * variable. The value associated with an environment variable must result in a
 * absolute file name that starts with one of the path names in the list.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 3, 2015  4492       rferrel     Initial creation
 * Jan 18, 2016 4562       tjensen     Moved from edex.plugin.text to 
 *                                     edex.plugin.text.dbsrv
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public class EnvPathWhiteList {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(EnvPathWhiteList.class);

    private final static String CONFIG_FILE = "environment/EnvWrapCfg.xml";

    /**
     * White list paths associated with an environment name.
     */
    private final Map<String, List<Path>> pathMap = new HashMap<>();

    public EnvPathWhiteList() {
        init();
    }

    /**
     * 
     * @param filename
     *            - A common static file name.
     */
    private void init() {
        EnvironmentWrapCfg envWrapCfg = null;
        try {
            IPathManager pm = PathManagerFactory.getPathManager();
            File path = pm.getStaticFile(CONFIG_FILE);

            envWrapCfg = JAXB.unmarshal(path, EnvironmentWrapCfg.class);
        } catch (Exception e) {
            statusHandler.handle(Priority.ERROR,
                    "Unable to parse the localized environment path configuration file: "
                            + CONFIG_FILE + ".", e);
            return;
        }

        // Perform Sanity Checks on configuration.
        StringBuilder message = new StringBuilder();
        Iterator<EnvironmentCfg> iter = envWrapCfg.getEnv().iterator();
        while (iter.hasNext()) {
            EnvironmentCfg envCfg = iter.next();
            String name = envCfg.getName();
            List<String> pathList = envCfg.getWhitePaths();

            if (pathList != null) {
                Iterator<String> pathIter = pathList.iterator();
                while (pathIter.hasNext()) {
                    String pathSting = pathIter.next();
                    Path path = null;
                    try {
                        path = Paths.get(pathSting).normalize();
                    } catch (InvalidPathException ex) {
                        path = null;
                    }
                    if ((path == null) || !path.isAbsolute()) {
                        message.append(name).append(" removing invalid path: ")
                                .append(pathSting).append("\n");
                        pathIter.remove();
                    } else {
                        List<Path> paths = pathMap.get(name);
                        if (paths == null) {
                            paths = new ArrayList<Path>(pathList.size());
                            pathMap.put(name, paths);
                        }
                        paths.add(path);
                    }
                }
            }
            if ((pathList == null) || pathList.isEmpty()) {
                message.append(name).append(" has no valid Paths.\n");
                iter.remove();
            }
        }
        if (message.length() > 0) {
            statusHandler.error(message.toString());
        }
    }

    /**
     * Obtain environment variable.
     * 
     * @param name
     *            - variable to obtain
     * 
     * @return value - null when not defined or fails validation
     */
    public String getenv(String name) {
        return getenv(name, null);
    }

    /**
     * Obtain environment variable when not defined use default value.
     * 
     * @param name
     * @param defaultValue
     * @return value - null when not defined and default value is null or fails
     *         validation
     */
    public String getenv(String name, String defaultValue) {
        String value = System.getenv(name);
        if (value == null) {
            if (defaultValue == null) {
                return null;
            }
            value = defaultValue;
        }

        if (!validate(name, value)) {
            statusHandler.error("The environment variable " + name
                    + " is an invalid value: " + value);
            return null;
        }
        return value;
    }

    /**
     * Validate the value for the environment variable.
     * 
     * @param name
     * @param value
     * @return true when value starts with a white list path
     */
    private boolean validate(String name, String value) {
        List<Path> paths = pathMap.get(name);
        if (paths != null) {
            Path valPath = null;
            try {
                valPath = Paths.get(value).normalize();
            } catch (InvalidPathException ex) {
                return false;
            }
            if (valPath != null) {
                for (Path path : paths) {
                    if (valPath.startsWith(path)) {
                        return true;
                    }
                }
            }
        }
        return false;
    }
}
