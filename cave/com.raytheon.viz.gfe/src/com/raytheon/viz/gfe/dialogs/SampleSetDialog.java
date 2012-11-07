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
package com.raytheon.viz.gfe.dialogs;

import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.dataplugin.gfe.sample.SampleId;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * The sample set dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 	Mar 7, 2008					Eric Babin Initial Creation
 * Oct 24, 2012 1287       rferrel     Code clean up for non-blocking dialog.
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

public class SampleSetDialog extends CaveJFACEDialog {

    public final static int LOAD = 1;

    public final static int SAVE = 2;

    public final static int DELETE = 3;

    public final static int OK = 97;

    public final static int REMOVE = 98;

    public final static int REPLACE = 99;

    public final static int CANCEL = Window.CANCEL;

    private Composite top;

    private java.util.List<SampleId> samples;

    private List sampleSetList;

    private Text identifierField;

    private String sampleName;

    private int[] selectedSamples;

    private int returnCode = CANCEL;

    private int type;

    public SampleSetDialog(Shell parent, java.util.List<SampleId> samples,
            int type) {
        super(parent);
        this.setShellStyle(SWT.DIALOG_TRIM | SWT.MODELESS);
        this.samples = samples;
        this.type = type;
    }

    private void initializeComponents() {
        Label lab = new Label(top, SWT.NONE);
        lab.setText("Sample Set Name(s)");

        if (type == SampleSetDialog.LOAD) {
            sampleSetList = new List(top, SWT.BORDER | SWT.V_SCROLL
                    | SWT.H_SCROLL | SWT.MULTI);
        } else {
            sampleSetList = new List(top, SWT.BORDER | SWT.V_SCROLL
                    | SWT.H_SCROLL | SWT.SINGLE);
        }

        for (SampleId sample : samples) {
            sampleSetList.add(sample.getName());
        }

        sampleSetList.addSelectionListener(new SelectionListener() {
            public void widgetDefaultSelected(SelectionEvent e) {
            }

            public void widgetSelected(SelectionEvent e) {
                selectedSamples = sampleSetList.getSelectionIndices();
                if (identifierField != null
                        && sampleSetList.getSelectionIndex() != -1) {
                    identifierField.setText(sampleSetList.getItem(sampleSetList
                            .getSelectionIndex()));
                }
            }
        });

        GridData data = new GridData(SWT.FILL, SWT.FILL, true, true);
        sampleSetList.setLayoutData(data);
        if (type != SampleSetDialog.LOAD) {
            Label processOn = new Label(top, SWT.NONE);
            processOn.setText("Identifier");

            if (type == SampleSetDialog.DELETE) {
                identifierField = new Text(top, SWT.BORDER | SWT.READ_ONLY);
            } else {
                identifierField = new Text(top, SWT.BORDER);
            }
            identifierField.addModifyListener(new ModifyListener() {
                public void modifyText(ModifyEvent arg0) {
                    selectedSamples = sampleSetList.getSelectionIndices();
                    sampleName = identifierField.getText();
                }
            });
            identifierField
                    .setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
        }
    }

    @Override
    protected void buttonPressed(int buttonId) {
        returnCode = buttonId;

        super.buttonPressed(Window.OK);
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        top = (Composite) super.createDialogArea(parent);

        GridLayout layout = new GridLayout(1, true);
        top.setLayout(layout);

        initializeComponents();

        return top;
    }

    @Override
    public int getReturnCode() {
        return returnCode;
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

        if (type == SampleSetDialog.LOAD) {
            shell.setText("Load Sample Set");
        } else if (type == SampleSetDialog.SAVE) {
            shell.setText("Save Sample Set");
        } else if (type == SampleSetDialog.DELETE) {
            shell.setText("Delete Sample Set");
        }
    }

    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        if (type == SampleSetDialog.LOAD) {
            super.createButton(parent, OK, "Add", false);
            super.createButton(parent, REMOVE, "Remove", false);
            super.createButton(parent, REPLACE, "Replace", true);
            super.createButton(parent, CANCEL, "Cancel", true);
        } else if (type == SampleSetDialog.SAVE) {
            super.createButton(parent, OK, "Save", false);
            super.createButton(parent, CANCEL, "Cancel", false);
        } else if (type == SampleSetDialog.DELETE) {
            super.createButton(parent, OK, "Delete", false);
            super.createButton(parent, CANCEL, "Cancel", false);
        }
    }

    public int[] getSelectedSampleIdIndexes() {
        return selectedSamples;
    }

    public int getType() {
        return type;
    }

    public String getSampleName() {
        return sampleName;
    }

    public void setSampleName(String sampleName) {
        this.sampleName = sampleName;
    }

    public java.util.List<SampleId> getSamples() {
        return samples;
    }
}
