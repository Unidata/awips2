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
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 22, 2017 6804       tgurney     Initial creation
 * Nov  5, 2018 6804       tgurney     executeTextScript wait for the script to
 *                                     finish before returning
 *
 * </pre>
 *
 * @author tgurney
 */

public class TextEditorScriptRunnerObserver implements IScriptRunnerObserver {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    private TextEditorDialog dialog;

    private volatile boolean cancelScript;

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
    public void executeTextScript(String script) {
        ITextWsScriptController[] controller = new ITextWsScriptController[1];
        VizApp.runSync(() -> {
            if (!dialog.isDisposed()) {
                controller[0] = dialog.executeTextScript(script);
            }
        });
        if (controller[0] != null) {
            try {
                controller[0].waitFor();
            } catch (InterruptedException e) {
                // ignore
            }
        }
    }

    @Override
    public void showScriptStatus(String statusMsg) {
        VizApp.runSync(() -> {
            if (!dialog.isDisposed()) {
                dialog.showScriptStatus(statusMsg);
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
    public void manageScriptOutputWindow(boolean visible) {
        VizApp.runSync(() -> {
            if (!dialog.isDisposed()) {
                dialog.manageScriptOutputWindow(visible);
            }
        });
    }

    @Override
    public void activateControls(boolean skipWait, boolean canContinue) {
        VizApp.runSync(() -> {
            if (!dialog.isDisposed()) {
                dialog.activateControls(skipWait, canContinue);
            }
        });
    }

    @Override
    public boolean continueScript() {
        return dialog.continueScript();
    }

    @Override
    public boolean skipWait() {
        return dialog.skipWait();
    }

    @Override
    public boolean cancelScript() {
        return cancelScript;
    }

    @Override
    public Display getDisplay() {
        return dialog.getDisplay();
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
    public void doCancel() {
        cancelScript = true;
    }
}
