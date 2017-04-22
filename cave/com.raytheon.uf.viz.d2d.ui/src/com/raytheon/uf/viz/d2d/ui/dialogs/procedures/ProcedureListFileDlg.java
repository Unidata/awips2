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
package com.raytheon.uf.viz.d2d.ui.dialogs.procedures;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.viz.ui.dialogs.localization.VizLocalizationFileListDlg;

/**
 * A dialog which displays a list of D2D procedures for opening, saving, or
 * deleting. The majority of the original ProcedureListFileDlg logic now exists
 * in the parent class:
 * {@link com.raytheon.viz.ui.dialogs.localization.VizLocalizationFileListDlg}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 2, 2015  4401       bkowal      Initial creation
 * Jun 30, 2015 4401       bkowal      Specify the localization level during construction.
 * 
 * </pre>
 * 
 * @author bkowal
 * @version 1.0
 */

public class ProcedureListFileDlg extends VizLocalizationFileListDlg {

    /** Frozen check box. */
    private Button frozenChk;

    private boolean frozen = false;

    /**
     * Constructor.
     * 
     * @param title
     * @param parent
     * @param mode
     * @param localizationDirectory
     */
    public ProcedureListFileDlg(String title, Shell parent, Mode mode,
            String localizationDirectory) {
        super(title, parent, mode, localizationDirectory, "procedures",
                LocalizationType.CAVE_STATIC);
    }

    @Override
    protected void createButtonComp(Composite mainComp) {
        // Add buttom comp
        Composite buttonComp = new Composite(mainComp, SWT.NONE);
        buttonComp.setLayout(new GridLayout(1, false));
        GridData gd = new GridData(SWT.CENTER, SWT.FILL, false, false);
        buttonComp.setLayoutData(gd);

        this.createOkButton(buttonComp);
        this.createCancelButton(buttonComp);

        if (mode == Mode.SAVE) {
            frozenChk = new Button(buttonComp, SWT.CHECK);
            frozenChk.setText("Freeze time");
            frozenChk.setSelection(frozen);
            frozenChk.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    frozen = !frozen;
                }
            });
        }
    }

    @Override
    protected boolean overwriteAllowed() {
        return ProcedureDlg.getDialog(localizationTF.getText()) == null;
    }

    /**
     * Method call to determine if frozen is selected.
     * 
     * @return True if frozen, false otherwise.
     */
    public boolean isFrozen() {
        return frozen;
    }
}