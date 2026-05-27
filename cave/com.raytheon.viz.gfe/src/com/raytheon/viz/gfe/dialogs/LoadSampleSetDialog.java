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

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.dataplugin.gfe.sample.SampleId;
import com.raytheon.viz.gfe.core.ISampleSetManager;
import com.raytheon.viz.gfe.core.ISampleSetManager.SampleSetLoadMode;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * Dialog for selecting sample sets to load
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 15, 2014  #3592     randerso    Re-implemented to match A1
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */
public class LoadSampleSetDialog extends CaveJFACEDialog {

    private static final int ADD_ID = IDialogConstants.CLIENT_ID;

    private static final int REMOVE_ID = IDialogConstants.CLIENT_ID + 1;

    private static final int REPLACE_ID = IDialogConstants.CLIENT_ID + 2;

    private ISampleSetManager sampleSetMgr;

    private String[] sets;

    private List sampleSetList;

    public LoadSampleSetDialog(ISampleSetManager sampleSetMgr, Shell parent) {
        super(parent);

        this.sampleSetMgr = sampleSetMgr;
        this.sets = this.sampleSetMgr.getInventoryAsStrings();
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        Composite comp = (Composite) super.createDialogArea(parent);

        Label label = new Label(comp, SWT.NONE);
        GridData layoutData = new GridData(SWT.DEFAULT, SWT.DEFAULT, false,
                false);
        label.setLayoutData(layoutData);
        label.setText("Sample Set Name(s)");

        sampleSetList = new List(comp, SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL
                | SWT.MULTI);
        layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);
        sampleSetList.setLayoutData(layoutData);
        sampleSetList.setItems(sets);

        return comp;
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
        shell.setText("Load Sample Set");
    }

    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        super.createButton(parent, ADD_ID, "Add", false);
        super.createButton(parent, REMOVE_ID, "Remove", false);
        super.createButton(parent, REPLACE_ID, "Replace", false);
        super.createButton(parent, IDialogConstants.CANCEL_ID, "Cancel", false);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
     */
    @Override
    protected void buttonPressed(int buttonId) {
        SampleSetLoadMode mode;
        switch (buttonId) {
        case ADD_ID:
            mode = SampleSetLoadMode.ADD;
            break;
        case REPLACE_ID:
            mode = SampleSetLoadMode.REPLACE;
            break;
        case REMOVE_ID:
            mode = SampleSetLoadMode.REMOVE;
            break;

        default:
            super.cancelPressed();
            return;
        }

        action(mode);
        super.okPressed();
    }

    private void action(SampleSetLoadMode mode) {
        for (int index : sampleSetList.getSelectionIndices()) {
            String name = sampleSetList.getItem(index);
            SampleId id = new SampleId(name);
            if (id.isValid()) {
                sampleSetMgr.loadSampleSet(id, mode);
            }

            /*
             * REPLACE with multiple entries needs to ADD after the first one to
             * have the effect of replacing the currently loaded Samples with
             * the set of selected samples. If actionType is REPLACE, set
             * actionType to ADD for second and subsequent entries.
             */
            if (mode.equals(SampleSetLoadMode.REPLACE)) {
                mode = SampleSetLoadMode.ADD;
            }

        }
    }
}
