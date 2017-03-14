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
package com.raytheon.uf.edex.plugin.manualIngest;

import java.io.File;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.TimeZone;

import org.apache.camel.Exchange;
import org.apache.camel.Processor;
import org.apache.commons.io.FileUtils;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.util.header.WMOHeaderFinder;
import com.raytheon.uf.common.wmo.WMOTimeParser;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.distribution.DistributionPatterns;

/**
 * A bean based on FileToString that will take a message generated from a file
 * endpoint and attempt to search for a WMO header inside the first 100 bytes.
 * If found, that string is set to the message header and passed on. If it is
 * not found, it will use the filename.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 28, 2009            brockwoo    Initial creation
 * Sep 03, 2013 2327       rjpeter     Added directory routing by plugin and date of product.
 * Apr 17, 2014 2942       skorolev    Updated throw exception in sendFileToIngest.
 * May 14, 2014 2536       bclement    removed TimeTools usage
 * Jul 10, 2014 2914       garmendariz Remove EnvProperties
 * 
 * </pre>
 * 
 * @author brockwoo
 * @version 1.0
 */

public class MessageGenerator implements Processor {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(MessageGenerator.class);

    private static String DIR = System.getProperty("data.archive.root")
            + File.separator + "manual";

    private static MessageGenerator instance = new MessageGenerator();

    private String ingestRoute = null;

    private final ThreadLocal<SimpleDateFormat> sdfs = new ThreadLocal<SimpleDateFormat>() {

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.ThreadLocal#initialValue()
         */
        @Override
        protected SimpleDateFormat initialValue() {
            SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMdd"
                    + File.separatorChar + "HH");
            sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
            return sdf;
        }

    };

    /**
     * Set of plugins that are not the primary decoder of the data. These are
     * secondary or additional information such as text, dhr, dpa, etc.
     */
    private final Set<String> secondaryPlugins = new HashSet<String>();

    public static MessageGenerator getInstance() {
        return instance;
    }

    public String getIngestRoute() {
        return ingestRoute;
    }

    public void setIngestRoute(String ingestRoute) {
        this.ingestRoute = ingestRoute;
    }

    /**
     * Register a secondary plugin, i.e. not the primary decoder of the data.
     * These are plugins that provide data in a different format oradditional
     * information such as text, dhr, dpa, etc.
     * 
     * @param plugin
     * @return
     */
    public MessageGenerator registerSecondaryPlugin(String plugin) {
        secondaryPlugins.add(plugin);
        return this;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.apache.camel.Processor#process(org.apache.camel.Exchange)
     */
    @Override
    public void process(Exchange arg0) throws Exception {
        File file = (File) arg0.getIn().getBody();
        if (file != null) {
            String messageHeader = WMOHeaderFinder.find(file);
            if (messageHeader == null) {
                messageHeader = file.getName();
            } else {
                messageHeader = messageHeader.trim();
            }
            arg0.getIn().setBody(file.toString());
            arg0.getIn().setHeader("header", messageHeader);
            arg0.getIn().setHeader("enqueueTime", System.currentTimeMillis());
        } else {
            // No file received
            arg0.getOut().setFault(true);
        }
    }

    /**
     * Copies the specified file to the archive directory.
     * 
     * @param inFile
     * @return
     * @throws IOException
     */
    public File copyFileToArchive(File inFile) throws IOException {
        StringBuilder path = new StringBuilder(inFile.getPath().length());
        path.append(DIR).append(File.separatorChar);

        // find header and determine file date
        Date fileTime = null;
        String header = WMOHeaderFinder.find(inFile);
        if (header == null) {
            header = inFile.getName();
        } else {
            header = header.trim();
            try {
                String dtg = WMOHeaderFinder.findDtg(header);
                Calendar headerTime = WMOTimeParser.findCurrentTime(dtg,
                        inFile.getName());
                if (headerTime != null) {
                    fileTime = headerTime.getTime();
                }
            } catch (Exception e) {
                statusHandler.error("Exception occurred parsing WMO Header", e);
            }
        }

        // determine the plugin
        List<String> plugins = DistributionPatterns.getInstance()
                .getMatchingPlugins(header);
        int numPlugins = plugins.size();
        if (numPlugins == 1) {
            path.append(plugins.get(0)).append(File.separatorChar);
        } else if (numPlugins > 1) {
            if (plugins.size() <= secondaryPlugins.size()) {
                // check for a non secondary plugin,
                String plugin = null;
                for (String pluginToCheck : plugins) {
                    if (!secondaryPlugins.contains(pluginToCheck)) {
                        plugin = pluginToCheck;
                        break;
                    }
                }

                if (plugin == null) {
                    // didn't find a non secondary plugin, just grab first
                    // plugin
                    plugin = plugins.get(0);
                }

                path.append(plugin).append(File.separatorChar);
            } else {
                // remove secondary and grab first one
                plugins.removeAll(secondaryPlugins);
                path.append(plugins.get(0)).append(File.separatorChar);
            }
        } else {
            path.append("unknown").append(File.separatorChar);
        }

        // append YYYYMMDD/HH
        if (fileTime == null) {
            // default to current time
            fileTime = SimulatedTime.getSystemTime().getTime();
        }
        path.append(sdfs.get().format(fileTime)).append(File.separatorChar);

        // Determine the sub-directory
        String inputPath = inFile.getParent();

        // Split on the manual directory to get the sub-directory
        String[] parts = inputPath.split("manual");
        if (parts.length > 1) {
            path.append(parts[1]);
        }

        File dir = new File(path.toString());

        if (!dir.exists()) {
            dir.mkdirs();
        }

        File newFile = new File(dir, inFile.getName());

        try {
            FileUtils.copyFile(inFile, newFile);
            statusHandler.handle(Priority.INFO,
                    "DataManual: " + inFile.getAbsolutePath());
        } catch (IOException e) {
            statusHandler.handle(Priority.ERROR, "Failed to copy file ["
                    + inFile.getAbsolutePath() + "] to archive dir", e);
            return null;
        }

        return newFile;
    }

    /**
     * Moves the specified file to the archive directory.
     * 
     * @param inFile
     * @return
     * @throws IOException
     */
    public File moveFileToArchive(File inFile) throws IOException {
        File newFile = copyFileToArchive(inFile);
        if (newFile != null) {
            inFile.delete();
        }
        return newFile;
    }

    /**
     * Copies a file to the archive directory and sends the path to the manual
     * ingest route.
     * 
     * @param inFile
     * @return
     */
    public boolean sendFileToIngest(String inFile) {
        return sendFileToIngest(inFile, ingestRoute);
    }

    /**
     * Copies a file to the archive directory and sends the path to the
     * specified route.
     * 
     * @param inFile
     * @param route
     * @return
     */
    public boolean sendFileToIngest(String inFile, String route) {
        boolean rval = true;

        try {
            File archiveFile = copyFileToArchive(new File(inFile));
            if (archiveFile == null) {
                throw new Exception("File " + inFile
                        + " has not been copied into archive.");
            }
            EDEXUtil.getMessageProducer().sendAsync(route,
                    archiveFile.getAbsolutePath());
        } catch (Exception e) {
            rval = false;
            statusHandler.handle(Priority.ERROR, "Failed to insert file ["
                    + inFile + "] into ingest stream", e);
        }

        return rval;
    }
}
