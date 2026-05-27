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

import java.util.HashSet;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.dataplugin.gfe.sample.SampleId;
import com.raytheon.uf.common.localization.ILocalizationFile;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.viz.gfe.core.ISampleSetManager;
import com.raytheon.viz.gfe.core.internal.SampleSetManager;
import com.raytheon.viz.gfe.ui.AccessMgr;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * Dialog for selecting sample sets to save or delete
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 07, 2008            Eric Babin  Initial Creation
 * Oct 24, 2012 1287       rferrel     Code clean up for non-blocking dialog.
 * Sep 15, 2014 3592       randerso    Re-implemented to match A1
 * Nov 19, 2014 5129       dgilling    Support SampleSetManager changes.
 * Feb 05, 2016 5242       dgilling    Remove calls to deprecated Localization APIs.
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

public class SaveDeleteSampleSetDialog extends CaveJFACEDialog {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SaveDeleteSampleSetDialog.class);

    private ISampleSetManager sampleSetMgr;

    private String optionStr;

    private SortedSet<String> sets;

    private Set<String> protectedSets;

    private List sampleSetList;

    private Text identifierField;

    /**
     * Constructor
     * 
     * @param sampleSetMgr
     *            SampleSetManager to use
     * @param parent
     *            parent shell for dialog
     * @param optionStr
     *            "Save" or "Delete"
     */
    public SaveDeleteSampleSetDialog(ISampleSetManager sampleSetMgr,
            Shell parent, String optionStr) {
        super(parent);

        this.sampleSetMgr = sampleSetMgr;
        this.optionStr = optionStr;
        this.sets = new TreeSet<String>();
        this.protectedSets = new HashSet<String>();

        SampleId[] sampleIDs = this.sampleSetMgr.getInventory();

        if (this.optionStr.equals("Save")) {
            for (SampleId id : sampleIDs) {
                if (!id.isProtected()) {
                    this.sets.add(id.getName());
                } else {
                    this.protectedSets.add(id.getName());
                }
            }
        }
        // delete mode
        else {
            for (SampleId id : sampleIDs) {
                if (!id.isProtected()
                        && id.getAccess().equals(LocalizationLevel.USER)) {
                    this.sets.add(id.getName());
                } else {
                    this.protectedSets.add(id.getName());
                }
            }
        }
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        Composite comp = (Composite) super.createDialogArea(parent);

        Group group = new Group(comp, SWT.NONE);
        group.setLayout(new GridLayout());
        GridData layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);
        group.setLayoutData(layoutData);
        group.setLayout(new GridLayout(1, false));
        group.setText("Sample Set Name");

        sampleSetList = new List(group, SWT.BORDER | SWT.V_SCROLL
                | SWT.H_SCROLL | SWT.SINGLE);
        layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);
        sampleSetList.setLayoutData(layoutData);
        if (!sets.isEmpty()) {
            sampleSetList.setItems(sets.toArray(new String[sets.size()]));
            sampleSetList.select(0);
            sampleSetList.deselectAll();
        }

        sampleSetList.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                identifierField.setText(sampleSetList.getItem(sampleSetList
                        .getSelectionIndex()));
            }
        });

        Label label = new Label(group, SWT.NONE);
        layoutData = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
        label.setLayoutData(layoutData);
        label.setText("Identifier");

        if (this.optionStr.equals("Save")) {
            identifierField = new Text(group, SWT.BORDER);
        } else {
            identifierField = new Text(group, SWT.BORDER | SWT.READ_ONLY);
        }
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        identifierField.setLayoutData(layoutData);

        return group;
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

        if (this.optionStr.equals("Save")) {
            shell.setText("Save Sample Set");
        } else {
            shell.setText("Delete Sample Set");
        }
    }

    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        super.createButton(parent, IDialogConstants.OK_ID, this.optionStr,
                false);
        super.createButton(parent, IDialogConstants.CANCEL_ID, "Cancel", false);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.Dialog#okPressed()
     */
    @Override
    protected void okPressed() {
        String name = this.identifierField.getText();
        if (this.optionStr.equals("Save")) {
            saveSample(name);
        } else {
            deleteSample(name);
        }

        super.okPressed();
    }

    private void saveSample(String name) {
        SampleId id = new SampleId(name);
        if (id.isValid() && !this.protectedSets.contains(name)) {
            // LogStream.logUse("Save", name);
            this.sampleSetMgr.saveActiveSampleSet(id);
        } else {
            String message = "Sample Set " + name + " is protected "
                    + "or an invalid name. ";
            statusHandler.handle(Priority.SIGNIFICANT, message);
        }
    }

    private void deleteSample(String name) {
        SampleId id = new SampleId(name);
        if ((id.isValid()) && (!protectedSets.contains(name))
                && (sets.contains(name))) {
            IPathManager pm = PathManagerFactory.getPathManager();
            LocalizationContext ctx = pm.getContext(
                    LocalizationType.COMMON_STATIC, LocalizationLevel.USER);
            ILocalizationFile lf = pm.getLocalizationFile(ctx,
                    SampleSetManager.SAMPLE_SETS_DIR + id.getName() + ".xml");
            boolean verify = AccessMgr.verifyDelete(lf.getPath(), lf
                    .getContext().getLocalizationType(), false);
            if (!verify) {
                return;
            }

            // LogStream.logUse("Delete", name);
            sampleSetMgr.deleteSampleSet(id);
        } else {
            String message = "Sample Set " + name + " is protected "
                    + "or an invalid name. ";
            statusHandler.handle(Priority.SIGNIFICANT, message);
        }

    }
}
