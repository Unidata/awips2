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
package com.raytheon.viz.texteditor.scripting.runner;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.uf.common.dataplugin.text.AfosWmoIdDataContainer;
import com.raytheon.uf.common.dataplugin.text.request.GetPartialAfosIdRequest;
import com.raytheon.uf.common.dataplugin.text.util.AWIPSParser;
import com.raytheon.uf.common.site.SiteMap;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.viz.core.mode.CAVEMode;
import com.raytheon.viz.texteditor.msgs.IScriptRunnerObserver;
import com.raytheon.viz.texteditor.scripting.dialogs.util.TextDBUtilities;
import com.raytheon.viz.texteditor.scripting.dialogs.util.Utilities;

/**
 * Class providing the top level implementations of the special Text WS
 * scripting commands.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#    Engineer   Description
 * ------------- ---------- ---------- -----------------------------------------
 * Jun 29, 2009             mfegan     Initial creation
 * Jul 13, 2010  2187       cjeanbap   Add operational mode functionality
 * Feb 04, 2015  4086       njensen    Resurrected class
 * Jan 02, 2018  6804       tgurney    Run doEvents in separate UI thread task
 * Apr 18, 2018  DCS 19952  dfriedman  Added AWIPS ID query support.
 * Nov 01, 2018  7587       tgurney    Display script status continuously when
 *                                     sleeping for a specified time
 * Feb 26, 2019  7746       randerso   Change to use Path instead of String for
 *                                     TextWS script path. Code cleanup.
 * Mar 04, 2019  7601       tgurney    Move relative path resolution here from
 *                                     TextWsScriptRunnerObserver. remove
 *                                     runLocalFile
 *
 * </pre>
 *
 * @author mfegan
 */

public class TextWsCommands {
    private static final String TIME_FMT = "%1$tD %1$tT";

    private IScriptRunnerObserver observer = null;

    private boolean operationalMode = true;

    public TextWsCommands() {
        CAVEMode mode = CAVEMode.getMode();
        this.operationalMode = (CAVEMode.OPERATIONAL.equals(mode)
                || CAVEMode.PRACTICE.equals(mode) ? true : false);
    }

    public void setObserver(Object observer) {
        this.observer = (IScriptRunnerObserver) observer;
    }

    public String resolveScriptPath(String callingScript, String fileName) {
        Path script = Paths.get(fileName);
        Path workingDirectory = Paths.get(callingScript).getParent();
        Path scriptPath;
        // Attempt to resolve relative script path
        if (!script.isAbsolute()) {
            scriptPath = workingDirectory.resolve(script).normalize();

            /*
             * For backward compatibility:
             *
             * If script is not found in the workingDirectory and has no path
             * component, check the user's home directory.
             */
            if (!Files.exists(scriptPath) && (script.getParent() == null)) {
                scriptPath = Paths.get(System.getProperty("user.home"))
                        .resolve(script).normalize();

                /*
                 * If script is found in user's home, warn them to move it into
                 * the working directory or add the necessary path
                 */
                if (Files.exists(scriptPath)) {
                    observer.showErrorMessage(
                            "Script " + scriptPath + " should be moved to "
                                    + workingDirectory
                                    + " or called using a relative or absolute path",
                            null);
                }
            }
        } else {
            scriptPath = script;
        }
        return scriptPath.toString();
    }

    /**
     * Implements the Text Workstation script runner's {@code load(pid)} and
     * {@code loadawips(aid)} commands. Reads the latest product matching the
     * specified PID (or AWIPS ID) from the text database and sends it to the
     * observer for display.
     *
     * @param id
     *            the product specifier (PIL or AWIPS ID)
     * @param afos
     *            perform an AFOS query if true or an AWIPS query if false
     *
     * @throws Exception
     *             when any error occurs
     */
    public void loadTextProduct(String id, boolean afos) throws Exception {
        if (Utilities.isEmptyString(id)) {
            throw new Exception(
                    "no product ID provided -- unable to load product");
        }
        if (observer.isEditMode()) {
            throw new Exception(
                    "Cannot load product: text window in edit mode");
        }
        if (id.startsWith("E:") || id.startsWith("M:")) {
            throw new Exception(
                    "Cannot load product: cannot edit products while script is running");
        }
        observer.writeText(
                "--- requesting " + id + " from text database ---\n");
        String[] products;
        try {
            products = TextDBUtilities.readProductFromDatabase(id,
                    TextDBUtilities.TYPE_PROD, this.operationalMode, afos);
        } catch (Exception e) {
            observer.writeText(
                    "--- product \"" + id + "\" not available ---\n");
            observer.showErrorMessage("failure reading from database.", e);
            return;
        }
        if (products == null || products.length == 0) {
            observer.writeText(
                    "--- product \"" + id + "\" not available ---\n");
            observer.showScriptStatus(
                    "Requested product \"" + id + "\" not found in data base");
            return;
        }
        observer.postProductToEditor(products,
                afos ? new String[] { id } : new String[] {});

    }

    /**
     * Implements the Text Workstation script runner's
     * {@code readdb(pid,filename)} and {@code readdbawips(pid,filename)}
     * commands. Reads the latest product matching the pid and writes the
     * product to the specified file. Emulates the AWIPS I <em>textdb -rd
     * PIL</em> (or <em>textdb -rkd AWIPSID</em>) retrieval.
     *
     * @param id
     *            the AFOS PIL (or AWIPS ID) to retrieve
     * @param afos
     *            perform an AFOS query if true or an AWIPS query if false
     * @param filename
     *            path to the file to contain the results
     *
     * @throws Exception
     *             if an error occurs
     */
    public void saveProductToFile(String id, boolean afos, String filename)
            throws Exception {
        if (Utilities.isEmptyString(id)) {
            throw new Exception(
                    "no product ID provided -- unable to read product");
        }
        if (Utilities.isEmptyString(filename)) {
            throw new Exception(
                    "no file name provided -- unable to read product");
        }
        observer.writeText(
                "--- requesting " + id + " from text database ---\n");
        String[] products = null;
        try {
            products = TextDBUtilities.readProductFromDatabase(id,
                    TextDBUtilities.TYPE_INFO, this.operationalMode, afos);
        } catch (Exception e) {
            observer.writeText(
                    "--- product \"" + id + "\" not available ---\n");
            observer.showErrorMessage("failure reading from database.", e);
            return;
        }
        if (products == null || products.length == 0) {
            observer.writeText(
                    "--- product \"" + id + "\" not available ---\n");
            observer.showScriptStatus(
                    "Requested product \"" + id + "\" not found in data base");
            return;
        }
        int count = products.length;
        String ln = System.getProperty("line.separator", "\n");
        observer.writeText(
                "--- obtained " + count + " records for " + id + " ---\n");
        StringBuilder sb = new StringBuilder();
        for (String product : products) {
            sb.append(product).append(ln);
        }
        observer.writeText(
                "--- writing results for " + id + " to " + filename + " ---\n");
        try {
            Files.write(Paths.get(filename), sb.toString().getBytes());

        } catch (Exception e) {
            observer.writeText("--- cannot write to " + filename + " ---");
            observer.showErrorMessage("cannot write to " + filename, e);
        }
    }

    /**
     * Implements the Text Workstation script runner's
     * {@code writedb(pid,filename)} and {@code writedbawips(pid,filename)}
     * commands. Reads the contents of the specified file and posts the contents
     * to the text database using the specified product ID.
     *
     * @param id
     *            the product ID (AFOS PIL or AWIPS ID)
     * @param afos
     *            perform an AFOS query if true or an AWIPS query if false
     * @param filename
     *            the path to the data file
     *
     * @throws Exception
     *             if any problem occurs
     */
    public void readProductFromFile(String id, boolean afos, String filename)
            throws Exception {
        if (Utilities.isEmptyString(id)) {
            throw new Exception(
                    "no product ID provided -- unable to write product");
        }
        if (Utilities.isEmptyString(filename)) {
            throw new Exception(
                    "no file name provided -- unable to write product");
        }
        observer.writeText("--- reading product from " + filename + " ---\n");
        String contents = "";
        try {
            contents = new String(Files.readAllBytes(Paths.get(filename)));
        } catch (Exception e) {
            throw new Exception("cannot read from " + filename, e);
        }
        String pil = afos ? id : mapAwipsIDtoAfos(id);
        try {
            String result = TextDBUtilities.writeProductToDatabase(pil,
                    contents, this.operationalMode);
            observer.showScriptStatus(result);
        } catch (Exception e) {
            observer.showErrorMessage("failure writing to database ", e);
        }
    }

    /**
     * Map the given AWIPS ID to a unique AFOS PIL
     *
     * @param id
     *            the AWIPS ID
     *
     * @throws Exception
     *             if there is no (unique) mapping or any problem occurs
     */
    private String mapAwipsIDtoAfos(String id) throws Exception {
        GetPartialAfosIdRequest req = new GetPartialAfosIdRequest();
        AWIPSParser parser = new AWIPSParser(id, null);
        if (!parser.isValidCommand()) {
            throw new Exception(String.format("invalid AWIPS ID '%s'", id));
        }
        String site = parser.getSite();
        if (site == null) {
            site = SiteMap.getInstance().getSite4LetterId(
                    LocalizationManager.getInstance().getCurrentSite());
        }
        req.setCccc(site);
        req.setNnn(parser.getNnn());
        req.setXxx(parser.getXxx());
        AfosWmoIdDataContainer idResult = (AfosWmoIdDataContainer) ThriftClient
                .sendRequest(req);
        if (idResult.getIdList().size() != 1) {
            throw new Exception(String.format(
                    "failure writing to database: no (unique) mapping for AWIPS ID '%s'",
                    id));
        }
        return idResult.getIdList().get(0).getAfosid();
    }

    /**
     * Puts the script runner into a "safe" wait state. This state can be
     * interrupted by the user in one of two ways; 'Continue' and 'Cancel'.
     *
     * @throws Exception
     *             if an error occurs
     */
    public void waitIndefinate() throws Exception {
        observer.showScriptStatus("Waiting for user to continue...");
        observer.scriptWaiting();
        // indefinite sleep loop
        while (true) {
            if (observer.isCanceled() || observer.isContinue()) {
                break;
            }
            try {
                Thread.sleep(100);
                doEvents();
                observer.showScriptStatusIfEmpty(
                        "Waiting for user to continue...");
            } catch (InterruptedException e) {
                // nothing to do
            }
        }
        observer.scriptResumed();
    }

    /**
     * Waits until the specified number of minutes after the hour. the number of
     * minutes must be between 0 and 59 inclusive. If the specified time is less
     * than the current minutes after the hour, the delay is scheduled into the
     * next hour.
     *
     * @param time
     *            time delay after the hour
     *
     * @throws Exception
     *             in case of any error
     */
    public void waitUntilTime(int minToWait) throws Exception {
        if (minToWait < 0 || minToWait > 59) {
            throw new Exception(
                    "Invalid argument: expected integer between 0 and 59 but got \""
                            + minToWait + "\"");
        }
        /* determine when to end wait */
        Date date = (Date) SimulatedTime.getSystemTime().getTime().clone();
        Calendar target = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        target.setTime(date);
        int minPastHour = target.get(Calendar.MINUTE);
        if (minPastHour > minToWait) {
            // past target -- wait into next hour
            target.add(Calendar.HOUR_OF_DAY, 1);
        }
        target.set(Calendar.MINUTE, minToWait);
        target.set(Calendar.SECOND, 0);
        /* execute the safe sleep */
        safeSleep(target.getTime());
    }

    /**
     * Waits for the specified amount of time. The format of the time
     * specification is <em>HH:MM:SS</em>; resulting of a delay of up to 23hrs
     * 59min 59sec.
     *
     * @param time
     *            the amount of time to delay
     *
     * @throws Exception
     *             in case of any error
     */
    public void waitForTime(String time) throws Exception {
        /* parse/validate the argument */
        Pattern p = Pattern.compile("(\\d{2}):(\\d{2}):(\\d{2})");
        Matcher m = p.matcher(time);
        if (!m.matches()) {
            throw new Exception(
                    "Invalid argument: expected format HH:MM:SS but got \""
                            + time + "\"");
        }
        int hrs = 0;
        int mins = 0;
        int secs = 0;
        try {
            hrs = Integer.parseInt(m.group(1));
            mins = Integer.parseInt(m.group(2));
            secs = Integer.parseInt(m.group(3));
        } catch (NumberFormatException e) {
            throw new Exception(
                    "Invalid argument: expected format HH:MM:SS but got \""
                            + time + "\"",
                    e);
        }
        if (hrs < 0 || hrs > 23 || mins < 0 || mins > 59 || secs < 0
                || secs > 59) {
            throw new Exception(
                    "Invalid argument: expected format HH:MM:SS but got \""
                            + time + "\"");
        }
        /* delay time in seconds */
        int delay = 3600 * hrs + 60 * mins + secs;
        /* create a calendar representing the wait end time */
        Date now = (Date) SimulatedTime.getSystemTime().getTime().clone();
        Calendar date = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        date.setTime(now);
        date.add(Calendar.SECOND, delay);
        /* execute the safe sleep */
        safeSleep(date.getTime());
    }

    /**
     * Safely sleeps the specified number of seconds.
     *
     * @param sleepToTime
     *            provides the end time of the sleep
     *
     * @throws Exception
     *             if an error occurs
     */
    private void safeSleep(Date sleepToTime) throws Exception {
        // short circuit -- return if end time already past
        if (SimulatedTime.getSystemTime().getTime().after(sleepToTime)) {
            return;
        }
        observer.showScriptStatus("Waiting until "
                + String.format(TIME_FMT, sleepToTime) + " to proceed...");
        observer.scriptWaiting();
        while (SimulatedTime.getSystemTime().getTime().before(sleepToTime)) {
            if (observer.isCanceled() || observer.isContinue()) {
                break;
            }
            try {
                Thread.sleep(100);
                doEvents();
                // Keep script status visible
                observer.showScriptStatusIfEmpty(
                        "Waiting until " + String.format(TIME_FMT, sleepToTime)
                                + " to proceed...");
            } catch (InterruptedException e) {
                // nothing to do
            }
        }
        observer.scriptResumed();
    }

    /**
     * Turns results accumulation on in the Text Editor Window.
     *
     * @param flag
     *            true to start accumulation, false to stop accumulation
     *
     * @throws Exception
     *             if an error occurs
     */
    public void setAccumulation(boolean flag) throws Exception {
        if (observer.isEditMode()) {
            throw new Exception(
                    "Cannot set accumulate: text window in edit mode");
        }
        observer.writeText(
                "--- turning accumulation " + (flag ? "on" : "off") + " ---\n");
        observer.setAccumulation(flag);
    }

    /**
     * Clears the Text Editor Window
     *
     * @throws Exception
     *             if an error occurs
     */
    public void clearTextDisplay() throws Exception {
        if (observer.isEditMode()) {
            throw new Exception("Cannot clear: text window in edit mode");
        }
        observer.writeText("--- clearing text display window ---\n");
        observer.clearTextDisplay();
    }

    /**
     * Sends the specified text to the observer for display. This method is used
     * to cause output from Python's print command to be redirected to the
     * observer.
     *
     * @param text
     *            the text to display
     */
    public void writeText(String text) {
        observer.writeText(text);
    }

    /**
     * Sends the specified text to the observer for display. This method is used
     * to capture output from {@code stderr} in the a python script and redirect
     * it to the observer.
     *
     * @param errMsg
     *            the stderr text to display
     */
    public void writeError(String errMsg) {
        observer.scriptError();
        writeText(errMsg);
        observer.addStdErrMsg(errMsg);
    }

    /**
     * allows the script to request a refresh of the GUI
     */
    public final void doEvents() {
        VizApp.runSync(() -> {
            while (observer.getDisplay().readAndDispatch()) {
            }
        });
    }

    /**
     * Returns {@code true} is the user has canceled the script via a user
     * interface element. This method should be called periodically during loops
     * and pauses to determine if a user ordered cancel has occurred.
     */
    public boolean cancelRequested() {
        return observer.isCanceled();
    }
}
