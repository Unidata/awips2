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
package com.raytheon.viz.ui.personalities.awips;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.PrintStream;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;

/**
 * Abstract class for logging cave performance metrics
 * 
 * Modified to add the log() method so as to make sure timestamp, host, and user name will be logged.  
 * - By Wufeng Zhou 07/26/2010
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 3, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class AbstractCavePerformanceMonitor {
	private final SimpleDateFormat TIMESTAMP_FORMAT = new SimpleDateFormat("yyyyMMdd HH:mm:ss.SSS");

    private PrintStream out;

    protected Map<String, Long> startTimeMap;

    protected Map<String, Integer> runCountMap;

    protected Map<String, Long> runTotalTimeMap;

    protected AbstractCavePerformanceMonitor(String fileName) {
        startTimeMap = new HashMap<String, Long>();
        runCountMap = new HashMap<String, Integer>();
        runTotalTimeMap = new HashMap<String, Long>();

        out = getPrintStream(fileName);
    }

    /**
     * @param fileName
     *            the name of the file to log to
     * 
     * @return
     */
    private PrintStream getPrintStream(String fileName) {
        SimpleDateFormat sdf = new SimpleDateFormat("yyyyMMdd");

        LocalizationContext ctx = PathManagerFactory.getPathManager()
                .getContext(LocalizationType.CAVE_STATIC,
                        LocalizationLevel.USER);

        File logFile = PathManagerFactory.getPathManager().getLocalizationFile(
                ctx,
                "logs" + File.separator + fileName + "-"
                        + sdf.format(new Date()) + ".log").getFile();
        if (logFile.getParentFile().exists() == false) {
            logFile.mkdirs();
        }

        try {
            PrintStream ps = new PrintStream(
                    new FileOutputStream(logFile, true), true);
            return ps;
        } catch (FileNotFoundException e) {
            e.printStackTrace();
            return System.out;
        }
    }

    /**
     * logg message along with timestamp, host, and user name information.  
     *  -added by Wufeng Zhou 07/26/2010 
     * @param message
     */
    protected void log(String message) {
    	String hostname = null;
    	try {
    		hostname = InetAddress.getLocalHost().getHostName();
    	} catch (UnknownHostException e) {
    		hostname = "unknown";
    	}
    	String timestamp = TIMESTAMP_FORMAT.format(new Date());
    	out.println(timestamp + " Host=" + hostname + ", User=" + System.getProperty("user.name") + ", " + message);
    }
    
    /**
     * printout exception stacktrace
     * @param e
     */
    protected void log(Exception e) {
    	e.printStackTrace(out);
    }
}
