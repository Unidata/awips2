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
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import org.apache.camel.Exchange;
import org.apache.camel.Message;
import org.apache.camel.RecipientList;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * The purpose of this bean is to load a series of XML files from localization
 * for each plugin registering itself with this bean and route messages based on
 * those XML files. The route method will provide a list of destinations based
 * on a header (or filename) and the associated plugin specified regex patterns.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 16, 2009            brockwoo     Initial creation
 * 6/8/2010     4647       bphillip    Added automatic pattern refreshing
 * 09/01/2010   4293       cjeanbap    Logging of unknown Weather Products.
 * Feb 27, 2013 1638        mschenke   Cleaned up localization code to fix null pointer
 *                                     when no distribution files present
 * Mar 19, 2013 1794       djohnson    PatternWrapper is immutable, add toString() to it for debugging.
 * 
 * </pre>
 * 
 * @author brockwoo
 * @version 1.0
 */

public class DistributionSrv {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(DistributionSrv.class);

    private static class PatternWrapper {
        private final String plugin;

        private final RequestPatterns patterns;

        private final String route;

        private final String displayString;

        private PatternWrapper(String plugin, String route,
                RequestPatterns patterns) {
            this.plugin = plugin;
            this.route = route;
            this.patterns = patterns;
            this.displayString = createDisplayString();
        }

        private String createDisplayString() {
            StringBuilder sb = new StringBuilder();
            sb.append("plugin=").append(plugin).append(", ");
            sb.append("route=").append(route).append(", ");
            sb.append("patterns=").append(patterns);
            return sb.toString();
        }

        @Override
        public String toString() {
            return displayString;
        }
    }

    protected transient Log logger = LogFactory.getLog("Ingest");

    protected transient Log routeFailedLogger = LogFactory
            .getLog("RouteFailedLog");

    private final List<PatternWrapper> pluginPatterns = new ArrayList<PatternWrapper>(
            100);

    private final ConcurrentMap<String, PatternWrapper> patternMap = new ConcurrentHashMap<String, PatternWrapper>();

    private final ConcurrentMap<String, Long> modifiedTimes = new ConcurrentHashMap<String, Long>();

    public DistributionSrv() {
        for (File file : getDistributionFiles()) {
            modifiedTimes.put(file.getName(), file.lastModified());
        }
    }

    /**
     * Refreshes the distribution patterns if a plugin's distribution pattern
     * file has been modified. This method is executed via a quartz timer every
     * five seconds
     */
    public synchronized void refresh() {
        for (File file : getDistributionFiles()) {
            if (!file.getName().endsWith("~")
                    && modifiedTimes.containsKey(file.getName())
                    && (modifiedTimes.get(file.getName()) < file.lastModified())) {
                String plugin = file.getName().replace(".xml", "");
                PatternWrapper wrapper = patternMap.get(plugin);
                if (wrapper != null) {
                    try {
                        statusHandler
                                .handle(Priority.EVENTA,
                                        "Change to distribution file detected. "
                                                + file.getName()
                                                + " has been modified.  Reloading distribution patterns");
                        wrapper = new PatternWrapper(wrapper.plugin,
                                wrapper.route, loadPatterns(file, plugin));
                        patternMap.put(plugin, wrapper);
                        modifiedTimes.put(file.getName(), file.lastModified());
                    } catch (DistributionException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                "Error reloading distribution patterns from file: "
                                        + file.getName(), e);
                    }
                }
            }
        }
    }

    /**
     * Allows a plugin to register itself with this bean. Note: if the plugin
     * does not provide an XML or it is malformed, this will throw an exception.
     * 
     * @param pluginName
     *            the plugin name
     * @param destination
     *            a destination to send this message to
     * @return an instance of this bean
     * @throws EdexException
     */
    public DistributionSrv register(String pluginName, String destination)
            throws DistributionException {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext commonStaticBase = pathMgr.getContext(
                LocalizationContext.LocalizationType.EDEX_STATIC,
                LocalizationContext.LocalizationLevel.BASE);

        LocalizationContext siteStaticBase = pathMgr.getContext(
                LocalizationContext.LocalizationType.EDEX_STATIC,
                LocalizationContext.LocalizationLevel.SITE);

        String path = "";
        String sitePath = "";
        try {
            path = pathMgr.getFile(commonStaticBase,
                    "distribution" + File.separator + pluginName + ".xml")
                    .getCanonicalPath();
            sitePath = pathMgr.getFile(siteStaticBase,
                    "distribution" + File.separator + pluginName + ".xml")
                    .getCanonicalPath();
        } catch (IOException e) {
            throw new DistributionException(
                    "Plugin "
                            + pluginName
                            + " does not have an accompanying patterns file in localization.");
        }

        File modelFile = new File(path);
        File siteModelFile = new File(sitePath);
        RequestPatterns patterns = null;
        if (siteModelFile.exists()) {
            patterns = loadPatterns(siteModelFile, pluginName);
        } else if (modelFile.exists()) {
            patterns = loadPatterns(modelFile, pluginName);
        } else {
            patterns = new RequestPatterns();
        }
        PatternWrapper wrapper = new PatternWrapper(pluginName, destination,
                patterns);
        patternMap.put(wrapper.plugin, wrapper);
        pluginPatterns.add(wrapper);
        return this;
    }

    /**
     * Generates a list of destinations for this message based on the header (or
     * filename if the header is not available).
     * 
     * @param exchange
     *            The exchange object
     * @return a list of destinations
     */
    @RecipientList
    public List<String> route(Exchange exchange) {
        StringBuilder pluginNames = new StringBuilder();
        List<String> dest = new ArrayList<String>();
        Message in = exchange.getIn();
        String header = (String) in.getHeader("header");
        Object payload = in.getBody();
        String bodyString = null;
        if (payload instanceof byte[]) {
            bodyString = new String((byte[]) payload);
        } else if (payload instanceof String) {
            bodyString = (String) payload;
        }
        File file = new File(bodyString);
        if (!file.exists()) {
            logger.error("File does not exist : " + bodyString);
            exchange.getOut().setFault(true);
        } else {
            in.setHeader("ingestFileName", file.toString());
        }
        boolean unroutedFlag = true;
        if (header == null) {
            // No header entry so will try and use the filename instead
            header = (String) exchange.getIn().getBody();
        }
        for (PatternWrapper wrapper : pluginPatterns) {
            if (wrapper.patterns.isDesiredHeader(header)) {
                if (pluginNames.length() != 0) {
                    pluginNames.append(",");
                }
                pluginNames.append(wrapper.plugin);
                dest.add(wrapper.route);
                unroutedFlag = false;
            }
        }

        if (unroutedFlag) {
            // append WMO header/filename to failed route logger
            // using warn instead of error; app can continue
            routeFailedLogger.warn(header);
        }
        in.setHeader("pluginName", pluginNames.toString());
        return dest;
    }

    /**
     * Loads patterns from a distribution file for the specified plugin
     * 
     * @param modelFile
     *            The file containing the ingest patterns
     * @param pluginName
     *            The plugin name associated with the ingest patterns
     * @throws DistributionException
     *             If the modelFile cannot be deserialized
     */
    private RequestPatterns loadPatterns(File modelFile, String pluginName)
            throws DistributionException {
        RequestPatterns patternSet = null;
        try {
            patternSet = SerializationUtil
                    .jaxbUnmarshalFromXmlFile(RequestPatterns.class, modelFile.getPath());
        } catch (Exception e) {
            throw new DistributionException("File "
                    + modelFile.getAbsolutePath()
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
    private File[] getDistributionFiles() {
        IPathManager pathMgr = PathManagerFactory.getPathManager();

        LocalizationFile[] files = pathMgr.listFiles(
                pathMgr.getLocalSearchHierarchy(LocalizationType.EDEX_STATIC),
                "distribution", null, true, true);
        Map<String, File> distFiles = new HashMap<String, File>();
        for (LocalizationFile file : files) {
            if (distFiles.containsKey(file.getName()) == false) {
                distFiles.put(file.getName(), file.getFile());
            }
        }

        return distFiles.values().toArray(new File[0]);
    }
}
