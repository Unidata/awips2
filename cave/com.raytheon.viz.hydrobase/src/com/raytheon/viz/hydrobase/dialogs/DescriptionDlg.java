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
package com.raytheon.viz.hydrobase.dialogs;

import java.util.ArrayList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.data.DescriptionData;
import com.raytheon.viz.hydrocommon.data.LocationAreaData;
import com.raytheon.viz.hydrocommon.data.ProximityData;
import com.raytheon.viz.hydrocommon.datamanager.DescriptionDataManager;
import com.raytheon.viz.hydrocommon.datamanager.LocationAreaManager;
import com.raytheon.viz.hydrocommon.datamanager.ProximityDataManager;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the description dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Sep 4, 2008				lvenable	Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class DescriptionDlg extends CaveSWTDialog {

    /**
     * Control font.
     */
    private Font controlFont;

    /**
     * Proximity combo box.
     */
    private Combo proximityCbo;

    /**
     * Stream bed text control.
     */
    private Text streamBedTF;

    /**
     * Divert text control.
     */
    private Text divertTF;

    /**
     * Remarks text control.
     */
    private Text remarksTF;

    /**
     * Freezing text control.
     */
    private Text freezingTF;

    /**
     * Reach text control.
     */
    private Text reachTF;

    /**
     * Regulation text control.
     */
    private Text regulationTF;

    /**
     * TOPO text control.
     */
    private Text topoTF;

    /**
     * Affected area text control.
     */
    private Text affectedAreaTF;

    /**
     * Delete button.
     */
    private Button deleteBtn;

    /**
     * Save button.
     */
    private Button saveBtn;

    /**
     * Affected area delete button.
     */
    private Button affectAreaDeleteBtn;

    /**
     * Affected area save button.
     */
    private Button affectAreaSaveBtn;

    private String locationId;
    
    private String currentRemarkText=null;
    private String currentFreezeText=null;
    private String currentReachText=null;
    private String currentRegText=null;
    private String currentTopoText=null;
    private String currentAreaText=null;

    // private boolean hasCurrentData = false;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param titleInfo
     *            Dialog title information.
     */
    public DescriptionDlg(Shell parent, String titleInfo, String lid) {
        super(parent);
        setText("Description" + titleInfo);

        locationId = lid;
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        return mainLayout;
    }

    @Override
    protected void disposed() {
        controlFont.dispose();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);
        controlFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);

        createMainControls();
        addSeparator();
        createBottomButton();
        populateProximityCombo();
        populateControls();
    }

    /**
     * Create the main controls.
     */
    private void createMainControls() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite controlComp = new Composite(shell, SWT.NONE);
        controlComp.setLayout(new GridLayout(2, false));
        controlComp.setLayoutData(gd);

        int textControlWidth = 750;
        int buttonWidth = 100;

        gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        Label proximityLbl = new Label(controlComp, SWT.RIGHT);
        proximityLbl.setText("Proximity: ");
        proximityLbl.setLayoutData(gd);

        // TODO : it looks like this combo box gets filled with data
        // at run time...
        proximityCbo = new Combo(controlComp, SWT.DROP_DOWN | SWT.READ_ONLY);

        gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        Label streamBedLbl = new Label(controlComp, SWT.RIGHT);
        streamBedLbl.setText("Stream Bed: ");
        streamBedLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.widthHint = textControlWidth;
        streamBedTF = new Text(controlComp, SWT.BORDER);
        streamBedTF.setFont(controlFont);
        streamBedTF.setLayoutData(gd);
        streamBedTF.setTextLimit(60);

        gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        Label divertLbl = new Label(controlComp, SWT.RIGHT);
        divertLbl.setText("Divert: ");
        divertLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.widthHint = textControlWidth;
        divertTF = new Text(controlComp, SWT.BORDER);
        divertTF.setFont(controlFont);
        divertTF.setLayoutData(gd);
        divertTF.setTextLimit(60);

        gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        Label remarksLbl = new Label(controlComp, SWT.RIGHT);
        remarksLbl.setText("Remarks: ");
        remarksLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.widthHint = textControlWidth;
        gd.heightHint = 70;
        remarksTF = new Text(controlComp, SWT.BORDER | SWT.MULTI | SWT.WRAP
                | SWT.V_SCROLL);
        remarksTF.setFont(controlFont);
        remarksTF.setLayoutData(gd);
        currentRemarkText=remarksTF.getText();
        ModifyListener listener = new ModifyListener() {
        	public void modifyText(ModifyEvent e) {
        		if (remarksTF.getText().length()>255){
        			remarksTF.setText(currentRemarkText);
        			shell.getDisplay().beep();
        		}
        		else
        			currentRemarkText=remarksTF.getText();
        	}
        };

        remarksTF.addModifyListener(listener);

        gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        Label freezingLbl = new Label(controlComp, SWT.RIGHT);
        freezingLbl.setText("Freezing: ");
        freezingLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.widthHint = textControlWidth;
        gd.heightHint = 70;
        freezingTF = new Text(controlComp, SWT.BORDER | SWT.MULTI | SWT.WRAP
                | SWT.V_SCROLL);
        freezingTF.setFont(controlFont);
        freezingTF.setLayoutData(gd);
        currentFreezeText=freezingTF.getText();
        ModifyListener listenerF = new ModifyListener() {
        	public void modifyText(ModifyEvent e) {
        		if (freezingTF.getText().length()>160){
        			freezingTF.setText(currentFreezeText);
        			shell.getDisplay().beep();
        		}
        		else
        			currentFreezeText=freezingTF.getText();
        	}
        };
        freezingTF.addModifyListener(listenerF);

        gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        Label reachLbl = new Label(controlComp, SWT.RIGHT);
        reachLbl.setText("Reach: ");
        reachLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.widthHint = textControlWidth;
        gd.heightHint = 50;
        reachTF = new Text(controlComp, SWT.BORDER | SWT.MULTI | SWT.WRAP
                | SWT.V_SCROLL);
        reachTF.setFont(controlFont);
        reachTF.setLayoutData(gd);
        currentReachText=reachTF.getText();
        ModifyListener listenerR = new ModifyListener() {
        	public void modifyText(ModifyEvent e) {
        		if (reachTF.getText().length()>80){
        			reachTF.setText(currentReachText);
        			shell.getDisplay().beep();
        		}
        		else
        			currentReachText=reachTF.getText();
        	}
        };
        reachTF.addModifyListener(listenerR);

        gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        Label regulationLbl = new Label(controlComp, SWT.RIGHT);
        regulationLbl.setText("Regulation: ");
        regulationLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.widthHint = textControlWidth;
        gd.heightHint = 70;
        regulationTF = new Text(controlComp, SWT.BORDER | SWT.MULTI | SWT.WRAP
                | SWT.V_SCROLL);
        regulationTF.setFont(controlFont);
        regulationTF.setLayoutData(gd);
        currentRegText=regulationTF.getText();
        ModifyListener listenerReg = new ModifyListener() {
        	public void modifyText(ModifyEvent e) {
        		if (regulationTF.getText().length()>230){
        			regulationTF.setText(currentRegText);
        			shell.getDisplay().beep();
        		}
        		else
        			currentRegText=regulationTF.getText();
        	}
        };
        regulationTF.addModifyListener(listenerReg);

        gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        Label topoLbl = new Label(controlComp, SWT.RIGHT);
        topoLbl.setText("Topography: ");
        topoLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.widthHint = textControlWidth;
        gd.heightHint = 70;
        topoTF = new Text(controlComp, SWT.BORDER | SWT.MULTI | SWT.WRAP
                | SWT.V_SCROLL);
        topoTF.setFont(controlFont);
        topoTF.setLayoutData(gd);
        currentTopoText=topoTF.getText();
        ModifyListener listenerT = new ModifyListener() {
        	public void modifyText(ModifyEvent e) {
        		if (topoTF.getText().length()>230){
        			topoTF.setText(currentTopoText);
        			shell.getDisplay().beep();
        		}
        		else
        			currentTopoText=topoTF.getText();
        	}
        };
        topoTF.addModifyListener(listenerT);

        // ---------------------------------------------
        // Add the Delete and Save buttons
        // ---------------------------------------------
        Composite buttonComp = new Composite(controlComp, SWT.NONE);
        buttonComp.setLayout(new GridLayout(2, true));
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        buttonComp.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        deleteBtn = new Button(buttonComp, SWT.PUSH);
        deleteBtn.setText("Delete");
        deleteBtn.setLayoutData(gd);
        deleteBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                deleteDescription();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        saveBtn = new Button(buttonComp, SWT.PUSH);
        saveBtn.setText("Save");
        saveBtn.setLayoutData(gd);
        saveBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                saveDescription();
            }
        });

        gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.horizontalSpan = 2;
        Label sepLbl = new Label(controlComp, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);

        // ------------------------------------------------------
        // Add the Affected controls and Delete & Save buttons
        // ------------------------------------------------------
        gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        Label affectedLbl = new Label(controlComp, SWT.RIGHT);
        affectedLbl.setText("Affected Area: ");
        affectedLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.widthHint = textControlWidth;
        gd.heightHint = 70;
        affectedAreaTF = new Text(controlComp, SWT.BORDER | SWT.MULTI
                | SWT.WRAP | SWT.V_SCROLL);
        affectedAreaTF.setFont(controlFont);
        affectedAreaTF.setLayoutData(gd);
        currentAreaText=affectedAreaTF.getText();
        ModifyListener listenerA = new ModifyListener() {
        	public void modifyText(ModifyEvent e) {
        		if (affectedAreaTF.getText().length()>80){
        			affectedAreaTF.setText(currentAreaText);
        			shell.getDisplay().beep();
        		}
        		else
        			currentAreaText=affectedAreaTF.getText();
        	}
        };
        affectedAreaTF.addModifyListener(listenerA);


        // ---------------------------------------------
        // Add the Delete and Save buttons
        // ---------------------------------------------
        Composite affectedButtonComp = new Composite(controlComp, SWT.NONE);
        affectedButtonComp.setLayout(new GridLayout(2, true));
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        affectedButtonComp.setLayoutData(gd);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        affectAreaDeleteBtn = new Button(affectedButtonComp, SWT.PUSH);
        affectAreaDeleteBtn.setText("Delete");
        affectAreaDeleteBtn.setLayoutData(gd);
        affectAreaDeleteBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                deleteAffectedarea();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        affectAreaSaveBtn = new Button(affectedButtonComp, SWT.PUSH);
        affectAreaSaveBtn.setText("Save");
        affectAreaSaveBtn.setLayoutData(gd);
        affectAreaSaveBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                saveAffectedArea();
            }
        });
    }

    /**
     * create the buttons at the bottom of the dialog.
     */
    private void createBottomButton() {
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(1, false));
        buttonComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 80;
        Button exitBtn = new Button(buttonComp, SWT.PUSH);
        exitBtn.setText("Exit");
        exitBtn.setLayoutData(gd);
        exitBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });
    }

    /**
     * Add a horizontal separator to the main display.
     */
    private void addSeparator() {
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        Label sepLbl = new Label(shell, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);
    }

    /**
     * Populate the proximity combo control with data from the database.
     */
    private void populateProximityCombo() {
        try {
            ArrayList<ProximityData> proximityData = ProximityDataManager
                    .getInstance().getProximityData();

            for (ProximityData data : proximityData) {
                proximityCbo.add(data.getProximity());
            }

        } catch (VizException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    /**
     * Populate the description and location area controls.
     */
    private void populateControls() {
        /*
         * Populate the description controls.
         */
        try {
            ArrayList<DescriptionData> descriptData = DescriptionDataManager
                    .getInstance().getDescriptionData(locationId);

            for (DescriptionData descriptionData : descriptData) {
                // Selected the correct proximity in the proximity combo box.
                proximityCbo.select(proximityCbo.indexOf(descriptionData
                        .getProximity()));

                streamBedTF.setText(descriptionData.getStreamBed());
                divertTF.setText(descriptionData.getDivert());
                remarksTF.setText(descriptionData.getRemark());
                freezingTF.setText(descriptionData.getIce());
                reachTF.setText(descriptionData.getReach());
                regulationTF.setText(descriptionData.getRegulation());
                topoTF.setText(descriptionData.getTopo());
            }

            if (proximityCbo.getSelectionIndex() == -1) {
                proximityCbo.select(0);
            }

            ArrayList<LocationAreaData> locData = LocationAreaManager
                    .getInstance().getLocationAreaData(locationId);

            for (LocationAreaData locAreaData : locData) {
                affectedAreaTF.setText(locAreaData.getArea());
            }
        } catch (VizException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    /**
     * Delete an existing description.
     */
    private void deleteDescription() {
        MessageBox mb = new MessageBox(shell, SWT.ICON_QUESTION | SWT.OK
                | SWT.CANCEL);
        mb.setText("Delete");
        mb.setMessage("Do you want to delete the selected description?");
        int result = mb.open();

        if (result == SWT.CANCEL) {
            return;
        }

        try {
            DescriptionDataManager.getInstance().deleteDescription(locationId);
        } catch (VizException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();

            MessageBox errorMb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            errorMb.setText("Error");
            errorMb.setMessage("Error deleting the selected description.");
            errorMb.open();

            return;
        }

        streamBedTF.setText("");
        divertTF.setText("");
        remarksTF.setText("");
        freezingTF.setText("");
        reachTF.setText("");
        regulationTF.setText("");
        topoTF.setText("");
    }

    /**
     * Save a new/existing description.
     */
    private void saveDescription() {
        try {
            DescriptionData descriptionData = getDataFromControls();

            boolean recordExists = DescriptionDataManager.getInstance()
                    .recordExists(locationId);

            if (recordExists == true) {
                DescriptionDataManager.getInstance().updateDescriptionData(
                        descriptionData);
            } else {
                DescriptionDataManager.getInstance().insertDescriptionData(
                        descriptionData);
            }
        } catch (VizException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    /**
     * Delete an existing location area.
     */
    private void deleteAffectedarea() {
        MessageBox mb = new MessageBox(shell, SWT.ICON_QUESTION | SWT.OK
                | SWT.CANCEL);
        mb.setText("Delete");
        mb.setMessage("Do you want to delete the affected area?");
        int result = mb.open();

        if (result == SWT.CANCEL) {
            return;
        }

        try {
            LocationAreaManager.getInstance().deleteRecord(locationId);
        } catch (VizException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();

            MessageBox errorMb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            errorMb.setText("Error");
            errorMb.setMessage("Error deleting the selected description.");
            errorMb.open();

            return;
        }

        affectedAreaTF.setText("");
    }

    /**
     * Save a new/existing location area.
     */
    private void saveAffectedArea() {
        try {
            LocationAreaData locAreaData = new LocationAreaData();
            locAreaData.setLid(locationId);
            locAreaData.setArea(affectedAreaTF.getText());

            boolean recordExists = LocationAreaManager.getInstance()
                    .recordExists(locationId);

            if (recordExists == true) {
                LocationAreaManager.getInstance().updateLocationAreaData(
                        locAreaData);
            } else {
                LocationAreaManager.getInstance().insertLocationAreaData(
                        locAreaData);
            }
        } catch (VizException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    /**
     * Get the description data from the controls.
     * 
     * @return Description data.
     */
    private DescriptionData getDataFromControls() {
        DescriptionData descriptionData = new DescriptionData();
        descriptionData.setLid(locationId);
        descriptionData.setStreamBed(streamBedTF.getText());
        descriptionData.setDivert(divertTF.getText());
        descriptionData.setRemark(remarksTF.getText());
        descriptionData.setIce(freezingTF.getText());
        descriptionData.setReach(reachTF.getText());
        descriptionData.setRegulation(regulationTF.getText());
        descriptionData.setTopo(topoTF.getText());
        descriptionData.setProximity(proximityCbo.getItem(proximityCbo
                .getSelectionIndex()));

        return descriptionData;
    }
}
