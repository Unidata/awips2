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
package com.raytheon.viz.texteditor.msgs;

import org.eclipse.swt.widgets.Display;

/**
 * The IScriptRunnerObserver interface specifies a method that allows the
 * ScriptRunner Plug-in to provide status messages in the status bar of the text
 * window.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * May 15, 2008 2104        grichard    Initial creation.
 * 
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */

public interface IScriptRunnerObserver {
    /**
     * Requests the observer to add the product to the display. This provides a
     * mechanism for the script to post a product to the editor window.
     * 
     * @param products
     *            array of text product to post
     * @param pils
     *            the PILs used to get products
     */
    void postProductToEditor(String[] products, String[] pils);

    /**
     * Requests the observer to execute the script. This provides a mechanism
     * for the script editor to send the script to the main text editing window
     * for execution.
     * 
     * @param script
     *            path to file containing the script to execute
     */
    void executeTextScript(String script);

    /**
     * Requests the observer display the status message. This is intended to
     * provide script status messages to the user. In AWIPS I, these are
     * displayed in the Text Editor Status bar; in AWIPS II, the intent is to
     * pass these to Alert Viz.
     * 
     * @param statusMsg
     *            the status message to display
     */
    void showScriptStatus(String statusMsg);

    /**
     * Requests the observer to display the error message. This is intended to
     * handle error messages/exceptions from the script. In AWIPS I, these are
     * displayed in a separate error display dialog; for AWIPS II, the intent is
     * to pass these to Alert Viz for display.
     * 
     * @param errorMsg
     *            the error message to display
     * @param cause
     *            throwable that triggered the error (may be null)
     */
    void showErrorMessage(String errorMsg, Throwable cause);

    /**
     * Requests the observer to turn on/off result accumulation. This allows the
     * script to control result accumulation.
     * 
     * @param flag
     *            specifies the accumulation state
     */
    void setAccumulation(boolean flag);

    /**
     * Requests the observer to clear its results display area.
     */
    void clearTextDisplay();

    /**
     * Requests the observer display the test. In AWIPS I, this was the output
     * of the TCL puts command and was redirected to the Script Output Window
     * (if displayed). For AWIPS II, this is a redirect of the Python print
     * statement; as in AWIPS I, it will be displayed only in the Script Output
     * Window.
     * 
     * @param text
     *            the text to write
     */
    void writeText(String text);

    /**
     * Requests that the listener add the line of text to the error message
     * buffer. Normally this is used to capture the stderr output of a script
     * for later display.
     * 
     * @param errMsg
     *            the line of text that is part of the error message
     */
    void addStdErrMsg(String errMsg);

    /**
     * Requests the listener to display the contents of the error message
     * buffer. (Use {@link #writeText(String)} to add text to the buffer.). The
     * specified {@code errMsg} is prepended to the buffer contents. The
     * listener will normally pass these to Alert Viz for display.
     * 
     * @param errMsg
     *            message to be displayed with error buffer
     */
    void writeErrMsg(String errMsg);

    /**
     * Requests that the listener manage the display of the Script Output
     * window.
     * 
     * @param visible
     *            flag; set true to display/enable the output window
     */
    void manageScriptOutputWindow(boolean visible);

    /**
     * Allows the script to signal the observer that either the 'Skip Wait' or
     * the 'Continue' controls should be activated.
     * 
     * @param skipWait
     *            state flag for the 'Skip Wait' controls
     * @param canContinue
     *            state flag for the 'Continue' controls
     */
    void activateControls(boolean skipWait, boolean canContinue);

    /**
     * Returns true if the script should exit the current indefinite loop
     */
    boolean continueScript();

    /**
     * Returns true if the script should skip the current wait.
     */
    boolean skipWait();

    /**
     * Returns true if the script should exit. Text WS defined statements should
     * check this flag prior to doing any work.
     */
    boolean cancelScript();

    /**
     * Returns the display associated with the observer.
     */
    Display getDisplay();

    /**
     * Allows the script runner to signal the observer when the script has
     * completed.
     */
    void scriptComplete();

    /**
     * Allows the script runner to signal the observer clear/reset the error
     * message buffer. Normally, this should be called just prior to executing
     * the script.
     */
    void clearErrBuffer();

    /**
     * Allows the script runner to signal the observer when the script has
     * written an error message.
     */
    void scriptError();

    /**
     * returns true if the script encountered an error in execution.
     */
    boolean isScriptError();

    /**
     * Returns true if the observer is in 'edit' mode, false otherwise.
     */
    boolean isEditMode();
}
