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
package com.raytheon.viz.texteditor.scripting.dialogs;

/**
 * Defines the interface for interacting with the Script Editor
 * Dialog. These methods provide a safe interface for the Text
 * Editor window to communicate with the Script Editor.
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 7, 2009  2372      mfegan     Initial creation
 *
 * </pre>
 *
 * @author mfegan
 * @version 1.0	
 */

public interface IScriptEditor {
    /** 
     * array of script extension information
     */
    static final String[] SCRIPT_EXTNS = {"[^\\.]*","*.py","*.tcl","*.txt"};
    /**
     * array of script names
     */
    static final String[] SCRIPT_NAMES = {"All Scripts","Python Scripts","TCL Scripts","Text Files"};
    
    /**
     * Causes the previously created IScriptEditor object to open.
     * 
     * @return an object representing the "results" of any work
     *         done by this object -- may be null
     */
    Object open();
    /**
     * Requests the IScriptEditor object set the Script Output State
     * to the specified value.
     * 
     * @param state the new Script Output State
     */
    void setScriptOutputState(boolean state);
    /**
     * Notifies the IScriptEditor object that the script has completed
     * execution.
     */
    void scriptComplete();
    /**
     * Notifies the IScriptEditor object of the requested enabled state
     * of the 'Continue' and 'Skip Wait'. Setting a flag to {@code true}
     * requests that the control be enabled; setting it to {@code false}
     * requests that the control be disabled.
     *  
     * @param cont requested state of the 'Continue' controls
     * @param skip requested state of the 'Skip Wait' controls
     */
    void setScriptControls(boolean cont,boolean skip);
}
