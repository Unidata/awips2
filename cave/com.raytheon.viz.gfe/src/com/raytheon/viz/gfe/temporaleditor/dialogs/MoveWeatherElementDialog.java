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

package com.raytheon.viz.gfe.temporaleditor.dialogs;

import java.util.List;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.temporaleditor.AbstractTemporalEditorBar;
import com.raytheon.viz.gfe.temporaleditor.TemporalEditor;
import com.raytheon.viz.gfe.temporaleditor.TemporalEditorUtil;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * Dialog to move a weather element
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05/19/09     #2159      rjpeter     Initial creation.
 * 11/14/2012   #1298      rferrel     Changes for non-blocking dialog.
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class MoveWeatherElementDialog extends CaveJFACEDialog {
    private Composite top;

    TemporalEditor temporalEditor;

    Parm parm;

    AbstractTemporalEditorBar sourceBar;

    List<AbstractTemporalEditorBar> destBars;

    AbstractTemporalEditorBar selectedBar = null;

    public MoveWeatherElementDialog(Shell parent,
            TemporalEditor temporalEditor, Parm parm,
            AbstractTemporalEditorBar sourceBar,
            List<AbstractTemporalEditorBar> destBars) {
        super(parent);
        this.setShellStyle(SWT.DIALOG_TRIM | SWT.APPLICATION_MODAL);
        this.temporalEditor = temporalEditor;
        this.parm = parm;
        this.sourceBar = sourceBar;
        this.destBars = destBars;
        selectedBar = sourceBar;
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        top = (Composite) super.createDialogArea(parent);

        GridLayout layout = new GridLayout();
        layout.numColumns = 1;
        top.setLayout(layout);

        initializeComponents();
        top.layout();

        return top;
    }

    private void initializeComponents() {
        GridData data = new GridData(SWT.BEGINNING, SWT.DEFAULT, true, false);
        final Button sourceButton = new Button(top, SWT.RADIO);
        sourceButton.setLayoutData(data);
        sourceButton.setText(getParmText(sourceBar));
        sourceButton.setSelection(true);
        sourceButton.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent arg0) {
            }

            public void widgetSelected(SelectionEvent arg0) {
                if (sourceButton.getSelection()) {
                    selectedBar = sourceBar;
                }
            }
        });

        if (destBars != null) {
            for (final AbstractTemporalEditorBar bar : destBars) {
                final Button b = new Button(top, SWT.RADIO);
                data = new GridData(SWT.BEGINNING, SWT.DEFAULT, true, false);
                b.setLayoutData(data);
                b.setText(getParmText(bar));
                b.setSelection(false);
                b.addSelectionListener(new SelectionListener() {
                    public void widgetDefaultSelected(SelectionEvent arg0) {
                    }

                    public void widgetSelected(SelectionEvent arg0) {
                        if (b.getSelection()) {
                            selectedBar = bar;
                        }
                    }
                });
            }
        }

        final Button aloneButton = new Button(top, SWT.RADIO);
        data = new GridData(SWT.BEGINNING, SWT.DEFAULT, true, false);
        aloneButton.setLayoutData(data);
        aloneButton.setText("Alone");
        aloneButton.setSelection(false);
        aloneButton.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent arg0) {
            }

            public void widgetSelected(SelectionEvent arg0) {
                if (aloneButton.getSelection()) {
                    selectedBar = null;
                }
            }
        });
    }

    /**
     * 
     * @param bar
     * @return
     */
    private String getParmText(AbstractTemporalEditorBar bar) {
        StringBuilder builder = new StringBuilder();
        for (Parm parm : bar.getParms()) {
            builder.append(TemporalEditorUtil.getTitleBarText(parm) + ", ");
        }
        builder.delete(builder.length() - 2, builder.length());
        return builder.toString();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets
     * .Shell)
     */
    @Override
    protected void configureShell(Shell shell) {
        super.configureShell(shell);
        shell.setText("Move Weather Element Dialog");
    }

    @Override
    protected void okPressed() {
        temporalEditor.moveParm(parm, sourceBar, selectedBar);
        super.okPressed();
    }

    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        createButton(parent, IDialogConstants.OK_ID, "OK", true);
        createButton(parent, IDialogConstants.CANCEL_ID, "Cancel", false);
    }
}
