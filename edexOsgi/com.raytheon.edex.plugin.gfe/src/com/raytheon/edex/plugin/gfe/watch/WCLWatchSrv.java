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
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.edex.core.EDEXUtil;
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
 * Sep 25, 2014  #3661     randerso     Update WCLWatchSrv to use localization correctly
 * Sep 25, 2014, #3670     randerso     Send notification directly to AlertViz
 * 
 * </pre>
 * 
 * @author wldougher
 * @version 1.0
 */
public class WCLWatchSrv {

    private static final String ALERT_SINGLE = "Alert: "
            + "%1$s has arrived. "
            + "Please open a GFE session for site %2$s and select ViewWCL and use %1$s. (Hazards menu)";

    private static final String ALERT_MULTIPLE = "Alert: "
            + "%1$s has arrived. "
            + "Please open GFE sessions for each of the following sites %2$s and select ViewWCL and use %1$s. (Hazards menu)";

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
     * More parsing is performed on the input parameter, a summary script is
     * written and moved into the "wcl" folder (whose location is determined by
     * localization). If any of the active local site IDs are mentioned in the
     * WCL, a AlertViz message is created and sent to alert the user. Otherwise,
     * a message is written to the log.
     * 
     * @param wclInfo
     *            The partially-parsed warning
     * @throws EdexException
     *             When portions of the warning cannot be parsed (i.e., dates),
     *             or when there are problems generating the WCL script file.
     */
    public void handleWclWatch(WclInfo wclInfo) throws EdexException {
        statusHandler.debug("handleWclWatch started");
        Collection<String> sitesToNotify = WatchProductUtil
                .findAttnWFOs(wclInfo.getLines());
        Set<String> siteIDs = getSiteIDs();

        sitesToNotify.retainAll(siteIDs); // Keep shared IDs

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

        // Create a wcl info string
        String wclStr = makeWclStr(finalUGCList, expireTime, issueTime,
                watchType);
        statusHandler.info("WCLData: " + wclStr);

        // Write wcl info to files for required sites
        createWclFile(siteIDs, completeProductPil, wclStr);

        if (sitesToNotify.isEmpty()) {
            statusHandler
                    .debug("WCL notification: no acitve sites in ATTN list");
        } else if (wclInfo.getNotify()) {
            String msg;
            if (sitesToNotify.size() == 1) {
                msg = String.format(ALERT_SINGLE, completeProductPil,
                        sitesToNotify.iterator().next());
            } else {
                msg = String.format(ALERT_MULTIPLE, completeProductPil,
                        sitesToNotify.toString());
            }
            EDEXUtil.sendMessageAlertViz(Priority.CRITICAL,
                    "com.raytheon.edex.plugin.gfe", "GFE", "GFE", msg, msg,
                    null);
        } else {
            statusHandler.info("Notification of WCL skipped");
        }

        statusHandler.debug("handleWclWatch() ending");
    }

    /**
     * Save WCL info to the localization path cave_static.SITE/gfe/wcl/ for each
     * of the specified sites.
     * 
     * @param siteIDs
     *            list of site IDs to create files for
     * @param completeProductPil
     *            WCL pil (used for file name)
     * @param wclStr
     *            WCL info to be written to the file
     */
    protected void createWclFile(Collection<String> siteIDs,
            String completeProductPil, String wclStr) {
        IPathManager pathManager = PathManagerFactory.getPathManager();
        for (String siteID : siteIDs) {
            LocalizationContext ctx = pathManager.getContextForSite(
                    LocalizationType.CAVE_STATIC, siteID);
            LocalizationFile wclFile = pathManager.getLocalizationFile(ctx,
                    FileUtil.join("gfe", "wcl", completeProductPil));
            try (PrintStream wclOut = new PrintStream(
                    wclFile.openOutputStream())) {
                wclOut.println(wclStr);
                wclOut.close();
                wclFile.save();
            } catch (LocalizationException e) {
                statusHandler.error("Error writing WCL file to " + wclFile, e);
                continue;
            }
            statusHandler.info("Wrote WCL to " + wclFile);
        }
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
     * @return current simulated time
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
}
