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

package com.raytheon.viz.hydro.pointdatacontrol;

import java.text.ParseException;
import java.util.ArrayList;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydro.pointdatacontrol.data.PointDataPreset;
import com.raytheon.viz.hydro.pointdatacontrol.db.PDCDataManager;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the Save dialog for Point Data Control dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 29 NOV 2007  373        lvenable    Initial creation
 * 07 FEB 2013  1578       rferrel     Changes for non-blocking dialog.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class PDC_SaveDlg extends CaveSWTDialog {
    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(PDC_SaveDlg.class);

    /** Title for Warning message dialog. */
    private final String PRESET_ERROR_TITLE = "Invalid Value";

    /** Message when invalid new ID. */
    private final String PRESET_ID_ERROR = "Invalid Preset Identifier.";

    /** Message when invalid new description. */
    private final String PRESET_DESC_ERROR = "Invalid Preset Description.";

    /** Message when invalid new rank. */
    private final String PRESET_RANK_ERROR = "Invalid Preset Rank.";

    /** Title for Error dialog when save fails. */
    private final String ERROR_TITLE = "Error Occurred";

    /** Error message when save fails. */
    private final String SAVE_ERROR_MESSAGE = "An error occurred during the save function.";

    /**
     * Current information unique ID.
     */
    private Text curUniqueIdTF;

    /**
     * Current information description.
     */
    private Text curDescTF;

    /**
     * Current information rank.
     */
    private Text curRankTF;

    /**
     * New information unique ID.
     */
    private Text newUniqueIdTF;

    /**
     * New information description.
     */
    private Text newDescTF;

    /**
     * New information rank.
     */
    private Text newRankTF;

    /**
     * Index of the selection in the preset list of the parent dialog.
     */
    private int selection;

    /**
     * Reference back to parent.
     */
    private PointDataControlDlg parentDialog;

    /**
     * the Point Data Control Data Manager.
     */
    private PDCDataManager dataManager = PDCDataManager.getInstance();

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public PDC_SaveDlg(Shell parent, int selection,
            PointDataControlDlg parentDialog) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("Save Preset Options");

        this.selection = selection;
        this.parentDialog = parentDialog;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#constructShellLayout()
     */
    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
        return mainLayout;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#initializeComponents(org
     * .eclipse.swt.widgets.Shell)
     */
    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);
        createCurrentInfoGroup();
        createNewInfoGroup();
        createBottomButtons();
        populateTextFields();
    }

    /**
     * Create the current information group and controls.
     */
    private void createCurrentInfoGroup() {
        Group currentInfoGroup = new Group(shell, SWT.NONE);
        currentInfoGroup.setText(" Current Information ");
        GridLayout groupLayout = new GridLayout(6, false);
        currentInfoGroup.setLayout(groupLayout);

        Label curUniqueIdLbl = new Label(currentInfoGroup, SWT.NONE);
        curUniqueIdLbl.setText("Unique Id:");

        curUniqueIdTF = new Text(currentInfoGroup, SWT.BORDER);
        curUniqueIdTF.setEditable(false);

        Label curDescLbl = new Label(currentInfoGroup, SWT.NONE);
        curDescLbl.setText("   Description:");

        GridData gd = new GridData(200, SWT.DEFAULT);
        curDescTF = new Text(currentInfoGroup, SWT.BORDER);
        curDescTF.setEditable(false);
        curDescTF.setLayoutData(gd);

        Label curRankLbl = new Label(currentInfoGroup, SWT.NONE);
        curRankLbl.setText("   Rank:");

        gd = new GridData(50, SWT.DEFAULT);
        curRankTF = new Text(currentInfoGroup, SWT.BORDER);
        curRankTF.setEditable(false);
        curRankTF.setLayoutData(gd);
    }

    /**
     * Create the new information group and controls.
     */
    private void createNewInfoGroup() {
        Group newInfoGroup = new Group(shell, SWT.NONE);
        newInfoGroup.setText(" New Information ");
        GridLayout groupLayout = new GridLayout(6, false);
        newInfoGroup.setLayout(groupLayout);

        Label newUniqueIdLbl = new Label(newInfoGroup, SWT.NONE);
        newUniqueIdLbl.setText("Unique Id:");

        newUniqueIdTF = new Text(newInfoGroup, SWT.BORDER);
        newUniqueIdTF.setEditable(true);
        newUniqueIdTF.setTextLimit(8);

        Label newDescLbl = new Label(newInfoGroup, SWT.NONE);
        newDescLbl.setText("   Description:");

        GridData gd = new GridData(200, SWT.DEFAULT);
        newDescTF = new Text(newInfoGroup, SWT.BORDER);
        newDescTF.setLayoutData(gd);

        Label newRankLbl = new Label(newInfoGroup, SWT.NONE);
        newRankLbl.setText("   Rank:");

        gd = new GridData(50, SWT.DEFAULT);
        newRankTF = new Text(newInfoGroup, SWT.BORDER);
        newRankTF.setLayoutData(gd);
    }

    /**
     * Create the bottom OK and Cancel buttons.
     */
    private void createBottomButtons() {
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Composite buttonArea = new Composite(shell, SWT.NONE);
        buttonArea.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
        buttonArea.setLayout(new GridLayout(2, false));
        buttonArea.setLayoutData(gd);

        gd = new GridData(80, SWT.DEFAULT);
        Button okBtn = new Button(buttonArea, SWT.PUSH);
        okBtn.setLayoutData(gd);
        okBtn.setText("OK");
        okBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                boolean status = saveData();
                if (status) {
                    close();
                }
            }
        });

        gd = new GridData(80, SWT.DEFAULT);
        Button cancelBtn = new Button(buttonArea, SWT.PUSH);
        cancelBtn.setLayoutData(gd);
        cancelBtn.setText("Cancel");
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                setReturnValue(false);
                close();
            }
        });
    }

    /**
     * Populate the text fields with the currently selected preset values.
     */
    private void populateTextFields() {
        try {
            ArrayList<PointDataPreset> presetList = (ArrayList<PointDataPreset>) dataManager
                    .getPresets();
            PointDataPreset pdp = null;

            if (selection >= 0) {
                pdp = presetList.get(selection);
            }

            if (pdp != null) {
                curUniqueIdTF.setText(pdp.getPresetId());
                newUniqueIdTF.setText(pdp.getPresetId());
                curDescTF.setText(pdp.getDescription());
                newDescTF.setText(pdp.getDescription());
                curRankTF.setText(String.valueOf(pdp.getPresetRank()));
                newRankTF.setText(String.valueOf(pdp.getPresetRank()));
            } else {
                curUniqueIdTF.setText("");
                newUniqueIdTF.setText("");
                curDescTF.setText("");
                newDescTF.setText("");
                curRankTF.setText("");
                newRankTF.setText("");
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Problem populating text fields: ", e);
        }
    }

    /**
     * Save the data.
     * 
     * @return true if successful, false otherwise
     */
    private boolean saveData() {
        PointDataPreset presetNode = new PointDataPreset();
        boolean matchFound = false;

        StringBuilder sb = new StringBuilder();
        String prefix = "";

        /* Check for valid values */
        String editedPresetId = newUniqueIdTF.getText().trim();
        if ((editedPresetId == null) || (editedPresetId.length() == 0)) {
            sb.append(PRESET_ID_ERROR);
            prefix = "\n";
        }

        String editedDesc = newDescTF.getText().trim();
        if ((editedDesc == null) || (editedDesc.length() == 0)) {
            sb.append(prefix).append(PRESET_DESC_ERROR);
            prefix = "\n";
        }

        int rank = -1;
        try {
            rank = Integer.parseInt(newRankTF.getText().trim());
        } catch (NumberFormatException nfe) {
            sb.append(prefix).append(PRESET_RANK_ERROR);
        } catch (Exception e) {
            sb.append(prefix).append(PRESET_RANK_ERROR);
        }

        if (sb.length() > 0) {
            MessageDialog.openWarning(shell, PRESET_ERROR_TITLE, sb.toString());
            return false;
        }

        /* Values are valid, continue */
        boolean success = true;

        try {
            parentDialog.setTimeFields();
            String presetString = PointDataControlPresets
                    .buildPresetStringFromOptions();

            if (presetString != null) {
                presetNode.setPresetId(editedPresetId);
                presetNode.setDescription(editedDesc);
                presetNode.setPresetString(presetString);
                presetNode.setPresetRank(rank);

                /*
                 * Loop over the linked list of PointDataPresets structures.
                 * Compare each preset id with the one the user has edited. If
                 * there is a match than do an update. Otherwise, perform an
                 * insert.
                 */
                ArrayList<PointDataPreset> presetList = parentDialog.presets;

                if (presetList != null) {
                    for (PointDataPreset pre : presetList) {
                        if (pre.getPresetId().equalsIgnoreCase(editedPresetId)) {
                            matchFound = true;
                            break;
                        }
                    }
                }
            }

            if (matchFound) {
                dataManager.updatePreset(presetNode);
            } else {
                dataManager.insertPreset(presetNode);
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM, "Save Error: ", e);
            success = false;
            MessageDialog.openError(shell, ERROR_TITLE, SAVE_ERROR_MESSAGE);
        } catch (ParseException e) {
            statusHandler.handle(Priority.PROBLEM, "Save Error: ", e);
            success = false;
            MessageDialog.openError(shell, ERROR_TITLE, SAVE_ERROR_MESSAGE);
        }

        parentDialog.populatePresetData(editedPresetId);

        return success;
    }
}
