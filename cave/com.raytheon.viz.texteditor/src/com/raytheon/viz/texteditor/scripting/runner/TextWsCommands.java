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

import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import jep.JepException;

import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.viz.core.mode.CAVEMode;
import com.raytheon.viz.texteditor.msgs.IScriptRunnerObserver;
import com.raytheon.viz.texteditor.scripting.dialogs.util.FileUtilities;
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
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 29, 2009            mfegan      Initial creation
 * Jul 13, 2010 2187       cjeanbap    Add operational mode functionality
 * 
 * </pre>
 * 
 * @author mfegan
 * @version 1.0
 */

public class TextWsCommands {
    private final String TIME_FMT = "%1$tD %1$tT";

    private String editor = "";

    private IScriptRunnerObserver observer = null;

    private boolean canceled = false;

    private boolean operationalMode = true;

    /**
     * 
     */
    public TextWsCommands() {
        CAVEMode mode = CAVEMode.getMode();
        this.operationalMode = (CAVEMode.OPERATIONAL.equals(mode)
                || CAVEMode.PRACTICE.equals(mode) ? true : false);
    }

    public void setEditor(String editor) {
        this.editor = editor;
    }

    public void setObserver(Object observer) {
        this.observer = (IScriptRunnerObserver) observer;
    }

    /**
     * Implements the Text Workstation script runner's {@code run(file)}
     * command. The file to execute must be in the current user's home
     * directory.
     * 
     * @param file
     *            name of the script to execute
     * 
     * @throws Exception
     *             if any problem occurs
     */
    public void runLocalFile(String file) throws JepException {
        String homeDir = System.getProperty("user.home");
        if (Utilities.isEmptyString(file)) {
            throw new JepException("no file specified -- unable to execute");
        }
        if (file.indexOf("/") != -1) {
            throw new JepException("expected local file but got \"" + file
                    + "\"");
        }

        String script = homeDir + "/" + file;
        // try {
        // script = FileUtilities.loadFileToString(homeDir, file);
        // } catch (IOException e) {
        // throw new Exception("could not open \"" + file + "\"",e);
        // }
        System.out.println(script);
        observer.executeTextScript(script);
    }

    /**
     * Implements the Text Workstation script runner's {@code load(pid)}
     * command. Reads the latest product matching the specified PID from the
     * text database and sends it to the observer for display.
     * 
     * @param pil
     *            the product ID (PID)
     * 
     * @throws Exception
     *             when any error occurs
     */
    public void loadTextProduct(String pil) throws Exception {
        if (Utilities.isEmptyString(pil)) {
            throw new Exception(
                    "no product ID provided -- unable to load product");
        }
        if (observer.isEditMode()) {
            throw new Exception("Cannot load product: text window in edit mode");
        }
        if (pil.startsWith("E:") || pil.startsWith("M:")) {
            throw new Exception(
                    "Cannot load product: cannot edit products while script is running");
        }
        observer.writeText("--- requesting " + pil
                + " from text database ---\n");
        String[] products;// = observer.getProductFromDatabase(pid);
        try {
            products = TextDBUtilities.readProductFromDatabase(pil,
                    TextDBUtilities.TYPE_PROD, this.operationalMode);
        } catch (Exception e) {
            observer.writeText("--- product \"" + pil
                    + "\" not available ---\n");
            observer.showErrorMessage("failure reading from database.", e);
            return;
        }
        if (products == null || products.length == 0) {
            observer.writeText("--- product \"" + pil
                    + "\" not available ---\n");
            observer.showScriptStatus("Requested product \"" + pil
                    + "\" not found in data base");
            return;
        }
        observer.postProductToEditor(products, new String[] { pil });
    }

    /**
     * Implements the Text Workstation script runner's
     * {@code readdb(pid,filename)} command. Reads the latest product matching
     * the pid and writes the product to the specified file. Emulates the AWIPS
     * I <em>textdb -rd PIL</em> retrieval.
     * 
     * @param pil
     *            the AFOS PIL to retrieve
     * @param filename
     *            path to the file to contain the results
     * 
     * @throws Exception
     *             if an error occurs
     */
    public void saveProductToFile(String pil, String filename) throws Exception {
        if (Utilities.isEmptyString(pil)) {
            throw new Exception(
                    "no product ID provided -- unable to read product");
        }
        if (Utilities.isEmptyString(filename)) {
            throw new Exception(
                    "no file name provided -- unable to read product");
        }
        observer.writeText("--- requesting " + pil
                + " from text database ---\n");
        String[] products = null;
        try {
            products = TextDBUtilities.readProductFromDatabase(pil,
                    TextDBUtilities.TYPE_INFO, this.operationalMode);
        } catch (Exception e) {
            observer.writeText("--- product \"" + pil
                    + "\" not available ---\n");
            observer.showErrorMessage("failure reading from database.", e);
            return;
        }
        if (products == null || products.length == 0) {
            observer.writeText("--- product \"" + pil
                    + "\" not available ---\n");
            observer.showScriptStatus("Requested product \"" + pil
                    + "\" not found in data base");
            return;
        }
        int count = products.length;
        String ln = System.getProperty("line.separator", "\n");
        observer.writeText("--- obtained " + count + " records for " + pil
                + " ---\n");
        StringBuffer sb = new StringBuffer();
        for (String product : products) {
            sb.append(product).append(ln);
        }
        observer.writeText("--- writing results for " + pil + " to " + filename
                + " ---\n");
        try {
            FileUtilities.writeStringToFile(filename, sb.toString());
        } catch (Exception e) {
            observer.writeText("--- cannot write to " + filename + " ---");
            observer.showErrorMessage("cannot write to " + filename, e);
        }
    }

    /**
     * Implements the Text Workstation script runner's
     * {@code writedb(pid,filename)} command. Reads the contents of the
     * specified file and posts the contents to the text database using the
     * specified product ID.
     * 
     * @param pil
     *            the product ID
     * @param filename
     *            the path to the data file
     * 
     * @throws Exception
     *             if any problem occurs
     */
    public void readProductFromFile(String pil, String filename)
            throws Exception {
        if (Utilities.isEmptyString(pil)) {
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
            contents = FileUtilities.loadFileToString(filename);
        } catch (Exception e) {
            throw new Exception("cannot read from " + filename);
        }
        try {
            String result = TextDBUtilities.writeProductToDatabase(pil,
                    contents, this.operationalMode);
            observer.showScriptStatus(result);
        } catch (Exception e) {
            observer.showErrorMessage("failure writing to database ", e);
        }
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
        observer.activateControls(false, true);
        // indefinite sleep loop
        while (true) {
            if (observer.cancelScript()) {
                canceled = true;
                break;
            } else if (observer.continueScript()) {
                break;
            }
            try {
                Thread.sleep(100);
                doEvents();
            } catch (InterruptedException e) {
                // nothing to do
            }
        }
        observer.activateControls(false, false);
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
                            + time + "\"", e);
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
        /* short circuit -- return if end time already past */
        if (SimulatedTime.getSystemTime().getTime().after(sleepToTime)) {
            return;
        }
        observer.activateControls(true, false);
        observer.showScriptStatus("Waiting until "
                + String.format(TIME_FMT, sleepToTime) + " to proceed...");
        while (SimulatedTime.getSystemTime().getTime().before(sleepToTime)) {
            if (observer.cancelScript()) {
                canceled = true;
                break;
            } else if (observer.skipWait()) {
                break;
            }
            try {
                Thread.sleep(100);
                doEvents();
            } catch (InterruptedException e) {
                // nothing to do
            }
        }
        observer.activateControls(false, false);
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
        observer.writeText("--- turning accumulation " + (flag ? "on" : "off")
                + " ---\n");
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
    public void doEvents() {
        while (observer.getDisplay().readAndDispatch()) {
        }
    }

    // /**
    // *
    // * @return
    // */
    // public boolean continueScript() {
    // return observer.continueScript();
    // }
    // /**
    // *
    // * @return
    // */
    // public boolean skipWait() {
    // return observer.skipWait();
    // }

    /**
     * Returns {@code true} is the user has canceled the script via a user
     * interface element. This method should be called periodically during loops
     * and pauses to determine if a user ordered cancel has occurred.
     */
    public boolean cancelScript() {
        return observer.cancelScript();
    }

    /**
     * Returns {@code true} if the script was canceled. This allows the Python
     * wrapper to properly relay the script cancellation to the script runner.
     * Note: this is not set by all commands.
     */
    public boolean isCanceled() {
        return canceled;
    }
}
