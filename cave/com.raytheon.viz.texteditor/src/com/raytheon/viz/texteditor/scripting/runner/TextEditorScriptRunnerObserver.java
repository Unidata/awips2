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

import org.eclipse.swt.widgets.Display;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.texteditor.dialogs.TextEditorDialog;
import com.raytheon.viz.texteditor.msgs.IScriptRunnerObserver;

/**
 * Script runner observer for a TextEditorDialog
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Dec 22, 2017  6804     tgurney   Initial creation
 * Nov 01, 2018  7587     tgurney   Add showScriptStatusIfEmpty()
 * Nov 05, 2018  6804     tgurney   executeTextScript wait for the script to
 *                                  finish before returning
 * Nov 09, 2018  7587     tgurney   showScriptStatusIfEmpty - do not write
 *                                  status to the log
 * Feb 26, 2019  7746     randerso  Change to use Path instead of String for
 *                                  TextWS script path.
 * Mar 04, 2019  7601     tgurney   Remove executeTextScript. Implement
 *                                  new methods in IScriptRunnerObserver.
 *                                  Combine "Continue" and "Skip Wait"
 *                                  functionality.
 *
 *
 * </pre>
 *
 * @author tgurney
 */

public class TextEditorScriptRunnerObserver implements IScriptRunnerObserver {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    private volatile boolean continueFlag = false;

    private volatile boolean canceled = false;

    private TextEditorDialog dialog;

    public TextEditorScriptRunnerObserver(TextEditorDialog dialog) {
        this.dialog = dialog;
    }

    @Override
    public void postProductToEditor(String[] products, String[] pils) {
        VizApp.runSync(() -> {
            if (!dialog.isDisposed()) {
                dialog.postProductToEditor(products, pils);
            }
        });
    }

    @Override
    public void showScriptStatus(String statusMsg) {
        VizApp.runSync(() -> {
            if (!dialog.isDisposed()) {
                dialog.showScriptStatus(statusMsg, true);
            }
        });
    }

    @Override
    public void showScriptStatusIfEmpty(String statusMsg) {
        VizApp.runSync(() -> {
            if (!dialog.isDisposed() && dialog.getStatusMsg().isEmpty()) {
                dialog.showScriptStatus(statusMsg, false);
            }
        });
    }

    @Override
    public void showErrorMessage(String errorMsg, Throwable cause) {
        statusHandler.handle(Priority.PROBLEM, errorMsg, cause);
    }

    @Override
    public void setAccumulation(boolean flag) {
        VizApp.runSync(() -> {
            if (!dialog.isDisposed()) {
                dialog.setAccumulation(flag);
            }
        });
    }

    @Override
    public void clearTextDisplay() {
        VizApp.runSync(() -> {
            if (!dialog.isDisposed()) {
                dialog.clearTextEditor();
            }
        });
    }

    @Override
    public void writeText(String text) {
        VizApp.runSync(() -> {
            if (!dialog.isDisposed()) {
                dialog.writeText(text);
            }
        });
    }

    @Override
    public void addStdErrMsg(String errMsg) {
        VizApp.runSync(() -> {
            if (!dialog.isDisposed()) {
                dialog.addStdErrMsg(errMsg);
            }
        });
    }

    @Override
    public void writeErrMsg(String errMsg) {
        VizApp.runSync(() -> {
            if (!dialog.isDisposed()) {
                dialog.writeErrMsg(errMsg);
            }
        });
    }

    @Override
    public void setShowScriptOutput(boolean showScriptOutput) {
        VizApp.runSync(() -> {
            if (!dialog.isDisposed()) {
                dialog.setShowScriptOutput(showScriptOutput);
            }
        });
    }

    @Override
    public Display getDisplay() {
        return dialog.getDisplay();
    }

    @Override
    public void scriptStarted() {
        VizApp.runSync(() -> {
            if (!dialog.isDisposed()) {
                dialog.scriptStarted();
            }
        });
    }

    @Override
    public void scriptComplete() {
        VizApp.runSync(() -> {
            if (!dialog.isDisposed()) {
                dialog.scriptComplete();
            }
        });
    }

    @Override
    public void clearErrBuffer() {
        VizApp.runSync(() -> {
            if (!dialog.isDisposed()) {
                dialog.clearErrBuffer();
            }
        });
    }

    @Override
    public void scriptError() {
        VizApp.runSync(() -> {
            if (!dialog.isDisposed()) {
                dialog.scriptError();
            }
        });
    }

    @Override
    public boolean isScriptError() {
        return dialog.isScriptError();
    }

    @Override
    public boolean isEditMode() {
        return dialog.isEditMode();
    }

    @Override
    public boolean isContinue() {
        return continueFlag;
    }

    @Override
    public void doContinue() {
        continueFlag = true;
    }

    @Override
    public boolean isCanceled() {
        return canceled;
    }

    @Override
    public void cancel() {
        canceled = true;
    }

    @Override
    public void scriptWaiting() {
        VizApp.runSync(() -> {
            if (!dialog.isDisposed()) {
                dialog.setContinueEnabled(true);
            }
        });
    }

    @Override
    public void scriptResumed() {
        continueFlag = false;
        VizApp.runSync(() -> {
            if (!dialog.isDisposed()) {
                dialog.setContinueEnabled(false);
            }
        });
    }
}
