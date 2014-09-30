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
/**
 * 
 */
package com.raytheon.edex.plugin.gfe.watch;

import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Set;
import java.util.TimeZone;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.edex.plugin.gfe.server.IFPServer;
import com.raytheon.edex.plugin.gfe.util.SendNotifications;
import com.raytheon.uf.common.dataplugin.gfe.server.notify.GfeNotification;
import com.raytheon.uf.common.dataplugin.gfe.server.notify.UserMessageNotification;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.edex.core.EdexException;

/**
 * If a WCL (watch county list) is ingested, this class will send a notification
 * to the GFE users alerting them that their WFO may be in the path of an
 * upcoming TO.A or SV.A.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * ??? ??, 20??            wldougher    Initial creation
 * Jun 09, 2014  #3268     dgilling     Ensure code works in multi-domain scenarios.
 * Jun 13, 2014  #3278     dgilling     Ensure temporary files get deleted.
 * 
 * </pre>
 * 
 * @author wldougher
 * @version 1.0
 */
public class WCLWatchSrv {

    private static final String ALERT_FORM = "Alert: " + "%1$s has arrived. "
            + "Please select ViewWCL and use %1$s. (Hazards menu)";

    private static final Pattern EXPIRE_TIME_PATTERN = Pattern
            .compile("(\\d{2})(\\d{2})(\\d{2})\\-");

    private static final String HYPHEN = Pattern.quote("-");

    private static final Pattern TORNADO_WATCH = Pattern
            .compile("\\.TORNADO WATCH");

    private static final Pattern UGC_FOLLOW_PATTERN = Pattern
            .compile("^(\\d{3})$");

    private static final Pattern UGC_NEW_PATTERN = Pattern
            .compile("^(([A-Z]{3})(\\d{3}))$");

    private static final Pattern UGC_PATTERN = Pattern.compile("\\d{3}\\-");

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(WCLWatchSrv.class);

    /**
     * Process a WCL watch, partially parsed and passed as a WclInfo object.
     * More parsing is performed on the input parameter, a summary script in
     * JSON format is written and moved into the "wcl" folder (whose location is
     * determined by localization). If any of the local site IDs are mentioned
     * in the WCL, a UserMessageNotification is created and sent to the user.
     * framework will use to alert the user. Otherwise, a message is written to
     * the log.
     * 
     * @param wclInfo
     *            The partially-parsed warning
     * @return null or a UserMessageNotification that will be handled by the SOA
     *         framework.
     * @throws EdexException
     *             When portions of the warning cannot be parsed (i.e., dates),
     *             or when there are problems generating the WCL script file.
     */
    public void handleWclWatch(WclInfo wclInfo) throws EdexException {
        statusHandler.debug("handleWclWatch started");
        Collection<String> sitesToNotify = WatchProductUtil
                .findAttnWFOs(wclInfo.getLines());
        Set<String> siteIDs = getSiteIDs();

        boolean doNotify = true;

        sitesToNotify.retainAll(siteIDs); // Keep shared IDs
        if (sitesToNotify.isEmpty()) {
            statusHandler.debug("WCL notification:  sites not in ATTN list");
            doNotify = false;
        }

        // Process the WCL regardless of whether we are sending a notice

        // Throw out every line which is not a UGC line
        List<String> finalUGCList = getUGCs(wclInfo);

        // Get the expiration time of the product
        Date expireTime = getExpireTime(wclInfo);

        // Get the issue time of the product
        Date issueTime = wclInfo.getIssueTime();

        // Get the watch type
        String watchType = getWatchType(wclInfo);

        // Get the WCL 'letter'
        String completeProductPil = wclInfo.getCompleteProductPil();

        // Create a dummy Procedure for export
        String wclStr = makeWclStr(finalUGCList, expireTime, issueTime,
                watchType);
        statusHandler.info("WCLData: " + wclStr);

        // Write dummy procedure to temp file
        File tmpFile = createTempWclFile(wclStr);

        // Move the file to the wcl folder
        // Rename it to <wclDir>/<completeProductPil>
        statusHandler.info("Placing WCL Procedure Utility in ifpServer ");
        try {
            makePermanent(tmpFile, completeProductPil, siteIDs);
        } finally {
            if (tmpFile != null) {
                tmpFile.delete();
            }
        }

        if (doNotify && wclInfo.getNotify()) {
            for (String siteID : sitesToNotify) {
                String msg = String.format(ALERT_FORM, completeProductPil);
                GfeNotification notify = new UserMessageNotification(msg,
                        Priority.CRITICAL, "GFE", siteID);
                SendNotifications.send(notify);
            }
        } else {
            statusHandler.info("Notification of WCL skipped");
        }

        statusHandler.debug("handleWclWatch() ending");
    }

    /**
     * Convert a temporary parsed WCL file to a permanent one by copying its
     * contents to the localization path cave_static.SITE/gfe/wcl/ for each of
     * the specified sites.
     * 
     * @param tmpFile
     *            The temporary file (may be {@code null})
     * @param completeProductPil
     *            The base name of the files to write.
     * @param siteIDs
     *            The set of siteIDs to write out the WCL data for.
     */
    protected void makePermanent(File tmpFile, String completeProductPil,
            Collection<String> siteIDs) {
        statusHandler.debug("makePermanent(" + tmpFile + ","
                + completeProductPil + ") started");
        if (tmpFile != null) {
            for (String siteID : siteIDs) {
                try {
                    File wclDir = getWclDir(siteID);
                    if (wclDir != null) {
                        File dest = new File(wclDir, completeProductPil);
                        FileUtil.copyFile(tmpFile, dest);
                        statusHandler.info("Wrote WCL "
                                + tmpFile.getAbsolutePath() + " to "
                                + dest.getAbsolutePath());
                    } else {
                        statusHandler
                                .error("Could not determine WCL directory for site "
                                        + siteID);
                    }
                } catch (IOException e) {
                    statusHandler.error("Could not copy temporary WCL file "
                            + tmpFile.getAbsolutePath()
                            + " to site directory for " + siteID, e);
                }
            }
        }

        statusHandler.debug("makePermanent(" + tmpFile + ","
                + completeProductPil + ") ending");
    }

    /**
     * Create a temporary file with the prefix "wcl" and the default suffix in
     * the default temporary file directory. Write all of wclStr into it.
     * 
     * @param wclStr
     *            the String containing the contents to write to the file
     * @return the File created.
     * @throws EdexException
     *             if the file cannot be written
     */
    protected File createTempWclFile(String wclStr) throws EdexException {
        File tmpFile = null;
        PrintStream wclOut = null;
        try {
            tmpFile = File.createTempFile("wcl", null);
            wclOut = new PrintStream(tmpFile);
            wclOut.println(wclStr);
        } catch (IOException e) {
            throw new EdexException("Error writing parsed WCL to file \""
                    + tmpFile.getAbsolutePath() + "\"", e);
        } finally {
            if (wclOut != null) {
                wclOut.close();
            }
        }
        return tmpFile;
    }

    /**
     * Create a string representing the parameters (parsed from a WclInfo) in
     * JSON format (identical to Python source, for data structures). The
     * expireTime and issueTime fields are converted to seconds for convenience
     * in Python.
     * 
     * @param finalUGCList
     *            the final UGCs in the WCL
     * @param expireTime
     *            the time the WCL expires, as a Date
     * @param issueTime
     *            the time the WCL was issued, as a Date
     * @param watchType
     *            the watch type String (either "SV.A" or "TO.A")
     * @return the String created from the parameters
     */
    protected String makeWclStr(List<String> finalUGCList, Date expireTime,
            Date issueTime, String watchType) {
        StringBuilder wclObj = new StringBuilder();
        wclObj.append("watchType = \"" + watchType + "\"\n");
        wclObj.append("finalUGCList = [");
        String sep = "";
        for (String ugc : finalUGCList) {
            wclObj.append(sep);
            wclObj.append("\"" + ugc + "\"");
            sep = ",";
        }
        wclObj.append("]\n");

        wclObj.append("expTime =");
        if (expireTime == null) {
            wclObj.append("None\n");
        } else {
            wclObj.append(expireTime.getTime() / 1000L);
            wclObj.append("\n");
        }

        wclObj.append("issueTime =");
        if (issueTime == null) {
            wclObj.append("None\n");
        } else {
            wclObj.append(issueTime.getTime() / 1000L);
            wclObj.append("\n");
        }
        String wclStr = wclObj.toString();
        return wclStr;
    }

    /**
     * Find the watch type in a WclInfo. This is "SV.A" unless any line matches
     * the TORNADO_WATCH pattern.
     * 
     * @param wclInfo
     *            The object representing the WCL watch
     * @return "TO.A" if any line contains the TORNADO_WATCH pattern "SV.A"
     *         otherwise
     */
    protected String getWatchType(WclInfo wclInfo) {
        String watchType = "SV.A";
        for (String line : wclInfo.getLines()) {
            if (TORNADO_WATCH.matcher(line).find()) {
                watchType = "TO.A";
                break;
            }
        }
        return watchType;
    }

    /**
     * @param wclInfo
     *            The object representing the WCL watch
     * @return the expiration time as a Date, or null if no line in wclInfo
     *         contains a match for EXPIRE_TIME_PATTERN.
     */
    protected Date getExpireTime(WclInfo wclInfo) {
        Date expireTime = null;
        for (String line : wclInfo.getLines()) {
            Matcher expireSearch = EXPIRE_TIME_PATTERN.matcher(line);
            if (expireSearch.find()) {
                int day = Integer.parseInt(expireSearch.group(1));
                int hour = Integer.parseInt(expireSearch.group(2));
                int minute = Integer.parseInt(expireSearch.group(3));
                Calendar cal = Calendar
                        .getInstance(TimeZone.getTimeZone("UTC"));
                cal.setTime(currentTime());
                int dom = cal.get(Calendar.DAY_OF_MONTH);
                cal.set(Calendar.DAY_OF_MONTH, day);
                cal.set(Calendar.HOUR_OF_DAY, hour);
                cal.set(Calendar.MINUTE, minute);
                cal.set(Calendar.MILLISECOND, 0);
                // Guess whether end time crossed a month boundary.
                if (day < (dom - 7)) {
                    cal.add(Calendar.MONTH, 1);
                }
                expireTime = cal.getTime();
                break;
            }
        }
        return expireTime;
    }

    /**
     * Get the current simulated time. Declared as a method so test code can
     * override it so EDEX and/or CAVE don't have to be started to test.
     * 
     * @return
     */
    protected Date currentTime() {
        return SimulatedTime.getSystemTime().getTime();
    }

    /**
     * Parse UGCs from the lines of wclInfo and return them as a List of
     * Strings.
     * 
     * @param wclInfo
     *            The object representing the WCL watch
     * @return the UGCs as a list of "SSS###" Strings, where SSS is a
     *         3-character state abbreviation and ### is a 3-digit zone code
     */
    protected List<String> getUGCs(WclInfo wclInfo) {
        StringBuilder ugcComposite = new StringBuilder();
        for (String line : wclInfo.getLines()) {
            // See if current line contains UGC_PATTERN
            if (UGC_PATTERN.matcher(line).find()) {
                ugcComposite.append(line);
            }
        }
        String[] ugcList = ugcComposite.toString().split(HYPHEN);

        // Process the list of UGC lines into a list of UGCs in full form
        // matching edit area names
        List<String> finalUGCList = new ArrayList<String>(ugcList.length);
        String state = null;
        for (String ugc : ugcList) {
            Matcher newGroup = UGC_NEW_PATTERN.matcher(ugc);
            if (newGroup.matches()) {
                state = newGroup.group(2);
                finalUGCList.add(newGroup.group(1));
            } else {
                Matcher followGroup = UGC_FOLLOW_PATTERN.matcher(ugc);
                if (followGroup.matches()) {
                    finalUGCList.add(state + followGroup.group(1));
                }
            }
        }
        return finalUGCList;
    }

    /**
     * Obtain the list of active sites. This was made into a method so that it
     * could be overridden in unit tests.
     * 
     * @return a Set of Strings representing the site IDs.
     */
    protected Set<String> getSiteIDs() {
        Set<String> siteIDs = IFPServer.getActiveSites();
        return siteIDs;
    }

    /**
     * Get the directory in which parsed WCLs should be placed. Like
     * getSiteIDs(), this is in a method rather than inline so that test code
     * can override it in subclasses.
     * 
     * @param siteID
     *            The siteID to write the WCL file for.
     * 
     * @return the directory, as a File.
     */
    protected File getWclDir(String siteID) {
        IPathManager pathManager = PathManagerFactory.getPathManager();
        LocalizationContext ctx = pathManager.getContextForSite(
                LocalizationType.CAVE_STATIC, siteID);
        String wclName = FileUtil.join("gfe", "wcl");
        File wclDir = pathManager.getFile(ctx, wclName);
        if (wclDir == null) {
            statusHandler.error("Path manager could not locate " + wclName);
        } else if (!wclDir.exists()) {
            wclDir.mkdir();
            statusHandler.info("Directory " + wclDir.getAbsolutePath()
                    + " created.");
        }
        return wclDir;
    }
}
