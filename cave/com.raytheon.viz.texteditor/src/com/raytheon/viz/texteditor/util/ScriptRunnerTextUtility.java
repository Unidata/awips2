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
package com.raytheon.viz.texteditor.util;

import org.eclipse.swt.widgets.Display;

import com.raytheon.viz.texteditor.msgs.IScriptRunnerObserver;

/**
 * This class is a utility for the ScriptRunner Plugin.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 8, 2009  2104       grichard    Initial creation.
 * 
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */

public class ScriptRunnerTextUtility implements IScriptRunnerObserver {

    @Override
    public void showScriptStatus(String statusMsg) {
        // TODO Implement this method
    }

    @Override
    public void clearTextDisplay() {
        // TODO Auto-generated method stub

    }

    @Override
    public void setAccumulation(boolean flag) {
        // TODO Auto-generated method stub

    }

    @Override
    public boolean continueScript() {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public void manageScriptOutputWindow(boolean visible) {
        // TODO Auto-generated method stub

    }

    @Override
    public void showErrorMessage(String errorMsg, Throwable cause) {
        // TODO Auto-generated method stub

    }

    @Override
    public boolean skipWait() {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public void writeText(String text) {
        // TODO Auto-generated method stub

    }

    @Override
    public void executeTextScript(String script) {
        // TODO Auto-generated method stub

    }

    @Override
    public Display getDisplay() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void scriptComplete() {
        // TODO Auto-generated method stub

    }

    @Override
    public boolean cancelScript() {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public void activateControls(boolean skipWait, boolean canContinue) {
        // TODO Auto-generated method stub

    }

    @Override
    public boolean isEditMode() {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public void postProductToEditor(String[] products, String[] pils) {
        // TODO Auto-generated method stub

    }

    @Override
    public void scriptError() {
        // TODO Auto-generated method stub

    }

    @Override
    public void addStdErrMsg(String errMsg) {
        // TODO Auto-generated method stub

    }

    @Override
    public boolean isScriptError() {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public void writeErrMsg(String errMsg) {
        // TODO Auto-generated method stub

    }

    @Override
    public void clearErrBuffer() {
        // TODO Auto-generated method stub

    }

}
