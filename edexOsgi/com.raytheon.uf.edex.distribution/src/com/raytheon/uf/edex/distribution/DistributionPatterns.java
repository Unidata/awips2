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
package com.raytheon.uf.edex.distribution;

import java.io.File;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * Container for the various Distribution patterns used by plugins.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 6, 2013  2327      rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class DistributionPatterns {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(DistributionPatterns.class);

    private static final DistributionPatterns instance = new DistributionPatterns();

    /**
     * Used to track file modified time to determine if a pattern set needs to
     * be reloaded.
     */
    private final ConcurrentMap<String, Long> modifiedTimes = new ConcurrentHashMap<String, Long>();

    /**
     * Patterns for the various plugins.
     */
    private final ConcurrentMap<String, RequestPatterns> patterns = new ConcurrentHashMap<String, RequestPatterns>();

    /**
     * Returns the singleton instance.
     * 
     * @return
     */
    public static DistributionPatterns getInstance() {
        return instance;
    }

    private DistributionPatterns() {
        refresh();
    }

    /**
     * Loads patterns from a distribution file for the specified plugin.
     * 
     * @param file
     *            The file containing the ingest patterns
     * @throws DistributionException
     *             If the modelFile cannot be deserialized
     */
    private RequestPatterns loadPatterns(File file)
            throws DistributionException {
        RequestPatterns patternSet = null;
        try {
            patternSet = SerializationUtil.jaxbUnmarshalFromXmlFile(
                    RequestPatterns.class, file.getPath());
        } catch (Exception e) {
            throw new DistributionException("File " + file.getAbsolutePath()
                    + " could not be unmarshalled.", e);
        }
        patternSet.compilePatterns();
        return patternSet;
    }

    /**
     * Lists the files in the distribution directory
     * 
     * @return An array of the files in the distribution directory
     */
    private Collection<File> getDistributionFiles() {
        IPathManager pathMgr = PathManagerFactory.getPathManager();

        LocalizationFile[] files = pathMgr.listFiles(
                pathMgr.getLocalSearchHierarchy(LocalizationType.EDEX_STATIC),
                "distribution", new String[] { ".xml" }, true, true);
        Map<String, File> distFiles = new HashMap<String, File>();
        for (LocalizationFile file : files) {
            if (distFiles.containsKey(file.getName()) == false) {
                distFiles.put(file.getName(), file.getFile());
            }
        }

        return distFiles.values();
    }

    /**
     * Refreshes the distribution patterns if a plugin's distribution pattern
     * file has been modified. This method is executed via a quartz timer every
     * five seconds
     */
    public void refresh() {
        for (File file : getDistributionFiles()) {
            String fileName = file.getName();
            Long modTime = modifiedTimes.get(fileName);
            if ((modTime == null)
                    || (modTime.longValue() != file.lastModified())) {
                // getDistributionFiles only returns files ending in .xml
                int index = fileName.lastIndexOf(".");
                String plugin = null;
                if (index > 0) {
                    plugin = fileName.substring(0, index);
                } else {
                    plugin = fileName;
                }

                try {
                    if (patterns.containsKey(plugin)) {
                        statusHandler
                                .info("Change to distribution file detected. "
                                        + fileName
                                        + " has been modified.  Reloading distribution patterns");
                    }
                    patterns.put(plugin, loadPatterns(file));
                    modifiedTimes.put(fileName, file.lastModified());
                } catch (DistributionException e) {
                    statusHandler.error(
                            "Error reloading distribution patterns from file: "
                                    + fileName, e);
                }
            }
        }
    }

    /**
     * Returns a list of plugins that are interested in the given header.
     * 
     * @param header
     * @return
     */
    public List<String> getMatchingPlugins(String header) {
        List<String> plugins = new LinkedList<String>();

        for (Map.Entry<String, RequestPatterns> entry : patterns.entrySet()) {
            if (entry.getValue().isDesiredHeader(header)) {
                plugins.add(entry.getKey());
            }
        }

        return plugins;
    }

    /**
     * Returns a list of plugins that are interested in the given header.
     * 
     * @param header
     * @param pluginsToCheck
     * @return
     */
    public List<String> getMatchingPlugins(String header,
            Collection<String> pluginsToCheck) {
        List<String> plugins = new LinkedList<String>();

        for (String plugin : pluginsToCheck) {
            RequestPatterns pattern = patterns.get(plugin);
            if ((pattern != null) && pattern.isDesiredHeader(header)) {
                plugins.add(plugin);
            }
        }

        return plugins;
    }

    /**
     * Returns true if there are patterns registered for the given plugin, false
     * otherwise.
     * 
     * @param pluginName
     * @return
     */
    public boolean hasPatternsForPlugin(String pluginName) {
        return patterns.containsKey(pluginName);
    }
}
