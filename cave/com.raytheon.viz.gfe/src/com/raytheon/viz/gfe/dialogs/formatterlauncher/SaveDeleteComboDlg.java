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
package com.raytheon.viz.gfe.dialogs.formatterlauncher;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.dataplugin.gfe.textproduct.CombinationsFileUtil;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.protectedfiles.ProtectedFileLookup;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * This class displays the Formatter Launcher Save/Delete combinations Dialog.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------
 * Mar 25, 2008  N/A      lvenable  Initial creation
 * Nov 08, 2012  1298     rferrel   Code cleanup for non-blocking dialog.
 * Oct 03, 2016  19293    randerso  Moved CombinationsFileUtil to common
 * Aug 07, 2017  6379     njensen   Use ProtectedFileLookup
 *
 * </pre>
 *
 * @author lvenable
 *
 */
public class SaveDeleteComboDlg extends CaveJFACEDialog {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(SaveDeleteComboDlg.class);

    private Map<String, Integer> comboDict;

    private List<String> mapNames;

    private String optionStr;

    private List<String> names;

    private List<String> protectedNames;

    private List<String> existingCombos;

    private org.eclipse.swt.widgets.List comboListBox;

    private Text comboName;

    public SaveDeleteComboDlg(Shell parent, List<String> mapNames,
            String optionStr, Map<String, Integer> comboDict) {
        super(parent);

        this.comboDict = comboDict;
        this.mapNames = mapNames;
        this.optionStr = optionStr;

        this.names = new ArrayList<>();
        this.protectedNames = new ArrayList<>();
        LocalizationFile[] lfs = CombinationsFileUtil.getSavedCombos();

        for (LocalizationFile lf : lfs) {
            String id = CombinationsFileUtil.fileToId(lf);
            String name = CombinationsFileUtil.fnToName(this.mapNames, id);
            if (name.isEmpty()) {
                continue;
            }
            if (!ProtectedFileLookup.isProtected(lf)) {
                this.names.add(name);
            } else {
                this.protectedNames.add(name);
            }
        }
    }

    @Override
    protected void configureShell(Shell newShell) {
        super.configureShell(newShell);

        if ("Save".equals(this.optionStr)) {
            newShell.setText("Save Combinations");
        } else {
            newShell.setText("Delete Combinations");
        }
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        Composite top = (Composite) super.createDialogArea(parent);

        comboListBox = new org.eclipse.swt.widgets.List(top,
                SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL | SWT.SINGLE);
        GridData layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        layoutData.heightHint = comboListBox.getItemHeight() * 10;
        comboListBox.setLayoutData(layoutData);

        Collections.sort(this.names);
        for (String name : names) {
            comboListBox.add(name);
        }
        this.existingCombos = this.names;
        comboListBox.setSelection(0);
        comboListBox.deselectAll();

        comboListBox.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                setSelectedBox();
            }
        });

        comboName = new Text(top, SWT.BORDER | SWT.SINGLE);
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        comboName.setLayoutData(layoutData);
        comboName.addModifyListener(new ModifyListener() {

            @Override
            public void modifyText(ModifyEvent e) {
                String entry = comboName.getText();

                if (existingCombos.contains(entry)) {
                    comboListBox.setSelection(new String[] { entry });
                } else {
                    comboListBox.deselectAll();
                }
            }
        });

        comboName.setFocus();

        return top;
    }

    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        createButton(parent, IDialogConstants.OK_ID, this.optionStr, true);
        createButton(parent, IDialogConstants.CANCEL_ID,
                IDialogConstants.CANCEL_LABEL, false);
    }

    @Override
    protected void okPressed() {
        String bName = this.comboName.getText();
        if ("Save".equals(this.optionStr)) {
            if (!this.protectedNames.contains(bName)) {
                performComboSave(bName);
            } else {
                statusHandler.handle(Priority.SIGNIFICANT, "Combinations "
                        + bName + " is protected or an invalid name. ");
            }
        } else if ("Delete".equals(this.optionStr)) {
            if (!this.protectedNames.contains(bName)
                    && this.names.contains(bName)) {
                performComboDelete(bName);
            } else {
                statusHandler.handle(Priority.SIGNIFICANT, "Combinations "
                        + bName + " is protected or an invalid name. ");
            }
        }
        super.okPressed();
    }

    /**
     * @param bName
     */
    private void performComboDelete(String name) {
        String actName = CombinationsFileUtil.nameToFN(this.mapNames, name);
        try {
            CombinationsFileUtil.deleteComboData(actName);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, "Error deleting combo file",
                    e);
        }
    }

    /**
     * @param bName
     */
    private void performComboSave(String name) {
        String actName = CombinationsFileUtil.nameToFN(this.mapNames, name);
        try {
            CombinationsFileUtil.saveComboData(actName, this.comboDict);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM, "Error saving combo file",
                    e);
        }
    }

    private void setSelectedBox() {
        this.comboName.setText(this.comboListBox.getSelection()[0]);
    }

}
