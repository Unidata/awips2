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

import com.raytheon.viz.texteditor.msgs.IScriptRunnerObserver;

/**
 * Implements the script runner for the Text WS. 
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 29, 2009 2373       mfegan     Initial creation
 *
 * </pre>
 *
 * @author mfegan
 * @version 1.0	
 */

public class TextWsScriptRunner {
    /** Script Observer. messages are sent to this object as the script executes. */
    private final IScriptRunnerObserver observer;
    /** the Text Editor ID token */
    private final String token;
    /** path to the script to execute */
    private String scriptPath;
    /** the script instance */
    private TextWsPythonScript runner = null;
    
    /**
     * Constructor.
     * @param observer
     * @param token
     */
    public TextWsScriptRunner(IScriptRunnerObserver observer, String token) {
        this.observer = observer;
        this.token = token;
    }
    /**
     * Executes the script at the specified path.
     * 
     * @param path location of the script to execute
     */
    public void executeScript(String path) {
        this.scriptPath = path;
        observer.getDisplay().asyncExec(new Runnable() {
            public void run() {
                try {
                    observer.clearErrBuffer();
                    observer.writeText("--- Running Script ---\n");
                    observer.showScriptStatus("Running script \"" + scriptPath + "\"");
                    runner = new TextWsPythonScript(observer, token);
                    runner.execute(scriptPath);
                    // check for error output
                    if (observer.isScriptError()) {
                        observer.writeErrMsg("Text WorkStation Scripting Error");
                        observer.writeText("--- Script Error! ---\n");
                    } else if (!runner.isCancelled()) {
                        observer.writeText("--- Script Complete ---\n");
                    } else {
                        observer.writeText("--- Script Cancelled ---\n");
                    }
                } catch (Throwable e) {
                    observer.writeText("--- Script Error! ---\n");
                    observer.showErrorMessage(
                            "Text WorkStation Scripting Error\n"
                                    + e.getMessage(), e);
                } finally {
                    if (runner != null) {
                        runner.dispose();
                    }
                    runner = null;
                    if (!observer.isScriptError()) {
                        observer.showScriptStatus("Script \"" + scriptPath + "\" complete");
                    }
                    observer.scriptComplete();
                }
            }
        });
    }
    public void dispose() {
        if (runner != null) {
            runner.dispose();
        }
    }
}
