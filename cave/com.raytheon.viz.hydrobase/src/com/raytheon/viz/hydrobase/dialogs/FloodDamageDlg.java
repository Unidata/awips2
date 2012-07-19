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

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.data.FloodData;
import com.raytheon.viz.hydrocommon.datamanager.FloodDataManager;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This class displays the Flood Damage dialog.
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
public class FloodDamageDlg extends CaveSWTDialog {

    /**
     * Control font.
     */
    private Font controlFont;

    /**
     * Stage list control.
     */
    private List stageList;

    /**
     * Stage text control.
     */
    private Text stageTF;

    /**
     * Display text control.
     */
    private Text displayTF;

    /**
     * Damage text control.
     */
    private Text damageTF;

    /**
     * text from the remark text box
     */
    private String currentDamageText=null;

    /**
     * Lid for the dialog.
     */
    private String lid;

    /**
     * Collection of stages for the current location
     */
    private ArrayList<FloodData> floodData;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param titleInfo
     *            Dialog title information.
     */
    public FloodDamageDlg(Shell parent, String titleInfo, String lid) {
        super(parent);
        setText("Flood Damage" + titleInfo);

        this.lid = lid;
    }

    @Override
    protected Layout constructShellLayout() {
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
        // Initialize all of the controls and layouts
        initializeComponents();

        // Load data for current lid
        getFloodDamageData();
    }

    /**
     * Initialize the components on the display.
     */
    private void initializeComponents() {
        controlFont = new Font(shell.getDisplay(), "Monospace", 10, SWT.NORMAL);

        createTopListControls();

        createStatementGroup();

        createBottomButtons();
    }

    /**
     * Create the top list controls.
     */
    private void createTopListControls() {
        Composite listComp = new Composite(shell, SWT.NONE);
        listComp.setLayout(new GridLayout(1, false));

        GridData gd = new GridData();
        gd.horizontalIndent = 4;
        Label listLbl = new Label(listComp, SWT.NONE);
        listLbl.setText(getListLabelText());
        listLbl.setFont(controlFont);
        listLbl.setLayoutData(gd);

        // --------------------------------------
        // Create the Stage list control
        // --------------------------------------
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 800;
        gd.heightHint = 125;
        stageList = new List(listComp, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL);
        stageList.setLayoutData(gd);
        stageList.setFont(controlFont);
        stageList.addSelectionListener(new SelectionListener() {
            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
            }

            @Override
            public void widgetSelected(SelectionEvent e) {
                getDamageStatement();
            }
        });
    }

    /**
     * Create the Statement group and controls.
     */
    private void createStatementGroup() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group statmentGroup = new Group(shell, SWT.NONE);
        statmentGroup.setLayout(new GridLayout(2, false));
        statmentGroup.setLayoutData(gd);
        statmentGroup.setText(" Statement ");

        gd = new GridData(100, SWT.DEFAULT);
        Label stageLbl = new Label(statmentGroup, SWT.RIGHT);
        stageLbl.setText("Stage: ");
        stageLbl.setLayoutData(gd);

        gd = new GridData(120, SWT.DEFAULT);
        stageTF = new Text(statmentGroup, SWT.BORDER);
        stageTF.setLayoutData(gd);

        gd = new GridData(100, SWT.DEFAULT);
        Label displayLbl = new Label(statmentGroup, SWT.RIGHT);
        displayLbl.setText("Display: ");
        displayLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        displayTF = new Text(statmentGroup, SWT.BORDER);
        displayTF.setLayoutData(gd);
        displayTF.setTextLimit(60);

        gd = new GridData(SWT.DEFAULT, SWT.TOP, false, true);
        gd.widthHint = 100;
        Label damageLbl = new Label(statmentGroup, SWT.RIGHT);
        damageLbl.setText("Damage: ");
        damageLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.heightHint = 175;
        damageTF = new Text(statmentGroup, SWT.BORDER | SWT.MULTI | SWT.WRAP);
        damageTF.setLayoutData(gd);
        damageTF.setFont(controlFont);
        currentDamageText=damageTF.getText();
        ModifyListener listener = new ModifyListener() {
        	public void modifyText(ModifyEvent e) {
        		if (damageTF.getText().length()>510){
        			damageTF.setText(currentDamageText);
        			shell.getDisplay().beep();
        		}
        		else
        			currentDamageText=damageTF.getText();
        	}
        };

        damageTF.addModifyListener(listener);

    }

    /**
     * Create the buttons at the bottom of the dialog.
     */
    private void createBottomButtons() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(5, true));
        buttonComp.setLayoutData(gd);

        int buttonWidth = 90;

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button okBtn = new Button(buttonComp, SWT.PUSH);
        okBtn.setText("OK");
        okBtn.setLayoutData(gd);
        okBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button applyBtn = new Button(buttonComp, SWT.PUSH);
        applyBtn.setText("Apply");
        applyBtn.setLayoutData(gd);
        applyBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                saveRecord();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button newBtn = new Button(buttonComp, SWT.PUSH);
        newBtn.setText("New");
        newBtn.setLayoutData(gd);
        newBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                clearStatement();
            }
        });

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = buttonWidth;
        Button deleteBtn = new Button(buttonComp, SWT.PUSH);
        deleteBtn.setText("Delete");
        deleteBtn.setLayoutData(gd);
        deleteBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                deleteRecord();
            }
        });
    }

    /**
     * Get the label text for the stage list control label.
     * 
     * @return Label text.
     */
    private String getListLabelText() {
        String format = "    %S         %S";

        String labelStr = String.format(format, "Stage", "Display Statement");

        return labelStr;
    }

    /**
     * Loads the Stage/Display statement data for a location
     */
    private void getFloodDamageData() {
        if (floodData != null) {
            floodData.clear();
        }

        try {
            floodData = FloodDataManager.getInstance().getFloodData(lid);
        } catch (VizException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        updateFloodDamageData();
    }

    private void updateFloodDamageData() {
        // Clear current Stages
        stageList.removeAll();

        for (FloodData currData : floodData) {
            stageList.add(currData.toString());
        }
    }

    private void getDamageStatement() {
        updateDamageDisplay(getCurrentlySelectedStage());
    }

    private void updateDamageDisplay(FloodData data) {
        stageTF.setText(Double.toString(data.getStage()));
        displayTF.setText(data.getDisplayStatement());
        damageTF.setText(data.getDamage());
    }

    private FloodData getCurrentlySelectedStage() {
        return floodData.get(stageList.getSelectionIndex());
    }

    private void deleteRecord() {
        if (MessageDialog.openConfirm(null, "Delete Confirmation",
                "Do you wish to delete this entry?")) {
            try {
                FloodDataManager.getInstance().deleteRecord(
                        getCurrentlySelectedStage());
            } catch (VizException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }

            getFloodDamageData();
        }

        clearStatement();
    }

    private void clearStatement() {
        stageTF.setText("");
        damageTF.setText("");
        displayTF.setText("");
    }

    private void saveRecord() {
        try {
            FloodDataManager.getInstance().putFloodCategoryData(lid,
                    Double.parseDouble(stageTF.getText()), damageTF.getText(),
                    displayTF.getText());
        } catch (NumberFormatException e) {
            MessageDialog.openConfirm(null, "Invalid Stage value",
                    "Please enter a valid numeric value for Stage");
        } catch (VizException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

        getFloodDamageData();
    }
}
