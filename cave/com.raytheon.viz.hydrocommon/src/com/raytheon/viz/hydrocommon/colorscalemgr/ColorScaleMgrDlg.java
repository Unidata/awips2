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
package com.raytheon.viz.hydrocommon.colorscalemgr;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.Set;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.Activator;
import com.raytheon.viz.hydrocommon.constants.StatusConstants;
import com.raytheon.viz.hydrocommon.data.ColorValueData;
import com.raytheon.viz.hydrocommon.datamanager.HydroDBDataManager;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Color Scale Manager dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 29 NOV 2007  373        lvenable    Initial creation
 * 23 Feb 2011  5400       lbousaidi   fixed issues in color/value bar
 * 11 Mar 2013  15065      lbousaidi   fixed issue with both color legend 
 *                         disappearing after save	
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class ColorScaleMgrDlg extends CaveSWTDialog {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(ColorScaleMgrDlg.class);
    private static final String OFFICE = "Office";

    private static final String DEFAULT = "Default";

    private static final String OFFICE_DEFAULT = "default";

    private static final String USER = "User";

    /**
     * User's name.
     */
    private String userName;

    /**
     * Current color in use.
     */
    private Color currentColor;

    /**
     * Label composite.
     */
    private Composite labelComp;

    /**
     * Used label composite.
     */
    private Composite usedLabelComp;

    /**
     * Updated Color/Value array of color and value labels.
     */
    private ArrayList<ColorValueLabels> colorValLblArray;

    /**
     * Used Color/Value array of color and value labels.
     */
    private ArrayList<ColorValueLabels> usedColorValLblArray;

    /**
     * Source combo box.
     */
    private Combo sourceCbo;

    /**
     * User ID combo box.
     */
    private Combo userIdCbo;

    /**
     * Data type combo box.
     */
    private Combo dataTypeCbo;

    /**
     * Duration combo box.
     */
    private Combo durationCbo;

    /**
     * User ID label.
     */
    private Label userIdLabel;

    /**
     * User ID filler label (space holder).
     */
    private Label userIdFillerLabel;

    /**
     * Composite containing combo boxes.
     */
    private Composite comboComp;

    /**
     * Left arrow used to cycle through the data types.
     */
    private Button leftArrowBtn;

    /**
     * Right arrow used to cycle through the data types.
     */
    private Button rightArrowBtn;

    /**
     * Button to delete as user
     */
    private Button deleteAsUserBtn;

    /**
     * Button to delete as office
     */
    private Button deleteAsOfficeBtn;

    /**
     * Label displaying the current color.
     */
    private Label colorLbl;

    /**
     * Text control displaying the selected scale value.
     */
    private Text valueTF;

    /**
     * Instance of the EditColorData class.
     */
    private EditColorData editColorData;

    /**
     * Used color set group container.
     */
    private Group usedColorSetGroup;

    /**
     * Save data type combo box.
     */
    private Combo saveDataTypeCbo;

    /**
     * colorManager for dialog to use, does database stuff for dialog
     */
    private ColorManager colorManager;

    /**
     * current selected index of sourceCbo, used to select previous source when
     * no data found for newly selected one
     */
    private int sourceIndex = 0;

    /**
     * the tab folder for the dialog
     */
    TabFolder tabFolder;

    /**
     * browse TabItem
     */
    private TabItem browseTab;

    /**
     * browseDataTypeCbo
     */
    private Combo browseDataTypeCbo;

    /**
     * browseDurationDbo
     */
    private Combo browseDurationCbo;

    private ArrayList<ColorValueLabels> browseColorValLblArray;

    private Composite browseLabelComp;

    private Integer selectedDurationInSeconds = 0;

    private Integer selectedBrowseDurationInSeconds = 0;

    private String sourceColor = null;

    private Button saveAsUserBtn, saveAsOfficeBtn;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     * @param userName
     *            User's name.
     */
    public ColorScaleMgrDlg(Shell parent, String userName) {
        super(parent, SWT.DIALOG_TRIM | SWT.RESIZE);

        this.userName = userName;
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 3;
        mainLayout.marginWidth = 3;
        mainLayout.verticalSpacing = 10;
        mainLayout.marginTop = 5;
        return mainLayout;
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);
        currentColor = new Color(getDisplay(), 0, 0, 0);

        createDefaultData();

        createTabFolder();
        createCloseButton();
        updateButtons();
    }

    /**
     * Create the tab folder container.
     */
    private void createTabFolder() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        tabFolder = new TabFolder(shell, SWT.NONE);
        tabFolder.setLayoutData(gd);

        TabItem editTab = new TabItem(tabFolder, SWT.NONE);
        editTab.setText("Edit Color Sets");
        editTab.setControl(createEditColorComp(tabFolder));

        browseTab = new TabItem(tabFolder, SWT.NONE);
        browseTab.setText("Browse Color Sets");
        tabFolder.addSelectionListener(new SelectionAdapter() {
            Composite browseControl;

            @Override
            public void widgetSelected(SelectionEvent event) {
                if (tabFolder.getSelectionIndex() == 1) {
                    browseControl = createBrowseColorSetComp(tabFolder);
                    browseTab.setControl(browseControl);
                    browseTab.getParent().layout();
                } else {
                    if ((browseControl != null)
                            && (browseControl.isDisposed() == false)) {
                        browseControl.dispose();
                    }
                }
            }
        });

    }

    /**
     * Create the composite that contains the edit color controls.
     * 
     * @param parentComp
     *            Parent composite.
     * @return Composite containing the edit color controls.
     */
    private Composite createEditColorComp(Composite parentComp) {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite mainEditComp = new Composite(parentComp, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        gl.marginHeight = 3;
        gl.marginWidth = 3;
        gl.verticalSpacing = 0;
        gl.marginTop = 0;
        mainEditComp.setLayout(gl);
        mainEditComp.setLayoutData(gd);

        createEditColorTopControls(mainEditComp);

        // Create the editable color/value bar
        createEditColorDisplay(mainEditComp);

        createEditColorControls(mainEditComp);

        createUsedColorSetGroup(mainEditComp);

        createDbControlsGroup(mainEditComp);

        return mainEditComp;
    }

    /**
     * Create the composite that contains the browse color controls.
     * 
     * @param parentComp
     *            Parent composite.
     * @return Composite containing the browse color controls.
     */
    private Composite createBrowseColorSetComp(Composite parentComp) {

        // gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        // Label messageLbl = new Label(mainBrowseComp, SWT.NONE);
        // messageLbl.setText("Browse controls not implemented yet...");
        // messageLbl.setLayoutData(gd);

        // TODO, get help from Lee on laying out data

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite topControlComp = new Composite(parentComp, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        topControlComp.setLayout(gl);
        topControlComp.setLayoutData(gd);

        Composite controlComp = new Composite(topControlComp, SWT.NONE);
        gl = new GridLayout(8, false);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        controlComp.setLayout(gl);
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        controlComp.setLayoutData(gd);

        int fillerWidth = 65;

        Label sourceLbl = new Label(controlComp, SWT.NONE);
        String source = sourceCbo.getText();
        if (source.equals(USER)) {
            source = userIdCbo.getText();
        }
        sourceLbl.setText("Source: " + source);

        Label fillerLbl = new Label(controlComp, SWT.NONE);
        gd = new GridData(fillerWidth, SWT.DEFAULT);
        fillerLbl.setLayoutData(gd);

        Label dataTypeLbl = new Label(controlComp, SWT.CENTER);
        dataTypeLbl.setText("Data Type:");

        browseDataTypeCbo = new Combo(controlComp, SWT.DROP_DOWN
                | SWT.READ_ONLY);

        populateBrowseDataTypeCombo();
        browseDataTypeCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                changeBrowseDataType();
            }
        });

        fillerLbl = new Label(controlComp, SWT.NONE);
        gd = new GridData(fillerWidth, SWT.DEFAULT);
        fillerLbl.setLayoutData(gd);

        Label durationLbl = new Label(controlComp, SWT.NONE);
        durationLbl.setText("Duration:");

        browseDurationCbo = new Combo(controlComp, SWT.DROP_DOWN
                | SWT.READ_ONLY);
        browseDurationCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                setBrowseSelectedDuration(browseDurationCbo.getText());
                updateBrowseColorValueLabelBar();
            }
        });
        updateBrowseDurationCombo();

        Label hrsLbl = new Label(controlComp, SWT.NONE);
        hrsLbl.setText("Hrs");

        // -----------------------------------------------
        // Create the navigation arrow buttons container
        // -----------------------------------------------
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Composite arraowBtnComp = new Composite(topControlComp, SWT.BOTTOM);
        gl = new GridLayout(2, false);
        gl.verticalSpacing = 100;
        arraowBtnComp.setLayout(gl);
        arraowBtnComp.setLayoutData(gd);

        gd = new GridData(50, SWT.DEFAULT);
        Button leftArrowBtn = new Button(arraowBtnComp, SWT.ARROW | SWT.LEFT);
        leftArrowBtn.setLayoutData(gd);
        leftArrowBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                int index = browseDataTypeCbo.getSelectionIndex();

                if (index == 0) {
                    index = browseDataTypeCbo.getItemCount() - 1;
                } else {
                    --index;
                }

                browseDataTypeCbo.select(index);
                changeBrowseDataType();
            }
        });

        gd = new GridData(50, SWT.DEFAULT);
        Button rightArrowBtn = new Button(arraowBtnComp, SWT.ARROW | SWT.RIGHT);
        rightArrowBtn.setLayoutData(gd);
        rightArrowBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                int index = browseDataTypeCbo.getSelectionIndex();

                if (index == (browseDataTypeCbo.getItemCount() - 1)) {
                    index = 0;
                } else {
                    ++index;
                }

                browseDataTypeCbo.select(index);
                changeBrowseDataType();
            }
        });

        createBrowseEditColorDisplay(topControlComp);

        controlComp.layout();
        shell.pack();

        return topControlComp;
    }

    /**
     * Create the controls at the top of the color editor.
     * 
     * @param parentComp
     *            Parent composite.
     */
    private void createEditColorTopControls(Composite parentComp) {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite topControlComp = new Composite(parentComp, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        topControlComp.setLayout(gl);
        topControlComp.setLayoutData(gd);

        // ------------------------------------------
        // Create the combo box and label container
        // ------------------------------------------
        comboComp = new Composite(topControlComp, SWT.NONE);
        gl = new GridLayout(12, false);
        comboComp.setLayout(gl);
        comboComp.setLayoutData(gd);

        int fillerWidth = 20;

        // Create the source label and combo box
        Label sourceLbl = new Label(comboComp, SWT.NONE);
        sourceLbl.setText("Source: ");

        gd = new GridData(100, SWT.DEFAULT);
        sourceCbo = new Combo(comboComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        populateSourceCombo();
        sourceCbo.setLayoutData(gd);
        sourceCbo.select(0);
        sourceCbo.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent event) {
                String source = sourceCbo.getText();

                if (!source.equals(USER)) {
                    if (source.equals(OFFICE)) {
                        source = OFFICE_DEFAULT;
                    }

                    boolean rval = updateDataType(source);
                    if (rval) {
                        userIdLabel.setVisible(false);
                        ((GridData) userIdLabel.getLayoutData()).exclude = true;

                        userIdCbo.setVisible(false);
                        ((GridData) userIdCbo.getLayoutData()).exclude = true;

                        userIdFillerLabel.setVisible(false);
                        ((GridData) userIdFillerLabel.getLayoutData()).exclude = true;
                        sourceIndex = sourceCbo.getSelectionIndex();
                    } else {
                        MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR
                                | SWT.OK);
                        mb.setText("Error");
                        mb.setMessage("No Office data found");
                        mb.open();
                        sourceCbo.select(sourceIndex);
                    }
                } else {

                    boolean rval = populateUserIdCombo();

                    if (rval) {
                        userIdCbo.select(0);
                        updateDataType(userIdCbo.getText());

                        userIdLabel.setVisible(true);
                        ((GridData) userIdLabel.getLayoutData()).exclude = false;

                        userIdCbo.setVisible(true);
                        ((GridData) userIdCbo.getLayoutData()).exclude = false;

                        userIdFillerLabel.setVisible(true);
                        ((GridData) userIdFillerLabel.getLayoutData()).exclude = false;
                        sourceIndex = sourceCbo.getSelectionIndex();
                    } else {
                        MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR
                                | SWT.OK);
                        mb.setText("Error");
                        mb.setMessage("No User data found");
                        mb.open();
                        sourceCbo.select(sourceIndex);
                    }
                }

                // shell.layout();
                comboComp.layout();
                shell.pack();
                updateUsedColorSetGroupText();
                updateButtons();
            }
        });

        gd = new GridData(fillerWidth, SWT.DEFAULT);
        Label filler1 = new Label(comboComp, SWT.NONE);
        filler1.setLayoutData(gd);

        gd = new GridData();
        gd.exclude = true;
        userIdLabel = new Label(comboComp, SWT.NONE);
        userIdLabel.setText("User ID: ");
        userIdLabel.setLayoutData(gd);
        userIdLabel.setVisible(false);

        gd = new GridData();
        gd.exclude = true;
        userIdCbo = new Combo(comboComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        userIdCbo.add("-----");
        userIdCbo.select(0);
        userIdCbo.setVisible(false);
        userIdCbo.setLayoutData(gd);
        userIdCbo.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent event) {
                updateDataType(userIdCbo.getText());
                updateButtons();
            }
        });

        gd = new GridData(fillerWidth, SWT.DEFAULT);
        gd.exclude = true;
        userIdFillerLabel = new Label(comboComp, SWT.NONE);
        userIdFillerLabel.setVisible(false);
        userIdFillerLabel.setLayoutData(gd);

        // Create the data type label and combo box
        Label dataTypeLbl = new Label(comboComp, SWT.NONE);
        dataTypeLbl.setText("Data Type: ");

        dataTypeCbo = new Combo(comboComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        populateDataTypeCombo();
        dataTypeCbo.select(0);
        dataTypeCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                changeDataType();
            }
        });

        gd = new GridData(fillerWidth, SWT.DEFAULT);
        Label filler3 = new Label(comboComp, SWT.NONE);
        filler3.setLayoutData(gd);

        // Create the duration label and combo box
        Label durationLbl = new Label(comboComp, SWT.NONE);
        durationLbl.setText("Duration: ");

        gd = new GridData(110, SWT.DEFAULT);
        durationCbo = new Combo(comboComp, SWT.DROP_DOWN);
        durationCbo.setLayoutData(gd);
        updateDurationCombo();
        durationCbo.addModifyListener(new ModifyListener() {
            @Override
            public void modifyText(ModifyEvent e) {
                boolean good = true;
                try {
                    Integer.parseInt(durationCbo.getText());
                } catch (Throwable t) {
                    good = false;
                }
                saveAsOfficeBtn.setEnabled(good);
                saveAsUserBtn.setEnabled(good);
            }
        });
        durationCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                changeDuration();
            }
        });

        Label hoursLbl = new Label(comboComp, SWT.NONE);
        hoursLbl.setText("Hrs");

        // -----------------------------------------------
        // Create the navigation arrow buttons container
        // -----------------------------------------------
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Composite arraowBtnComp = new Composite(topControlComp, SWT.NONE);
        gl = new GridLayout(2, false);
        arraowBtnComp.setLayout(gl);
        arraowBtnComp.setLayoutData(gd);

        gd = new GridData(50, SWT.DEFAULT);
        leftArrowBtn = new Button(arraowBtnComp, SWT.ARROW | SWT.LEFT);
        leftArrowBtn.setLayoutData(gd);
        leftArrowBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                int index = dataTypeCbo.getSelectionIndex();

                if (index == 0) {
                    index = dataTypeCbo.getItemCount() - 1;
                } else {
                    --index;
                }

                dataTypeCbo.select(index);
                changeDataType();
            }
        });

        gd = new GridData(50, SWT.DEFAULT);
        rightArrowBtn = new Button(arraowBtnComp, SWT.ARROW | SWT.RIGHT);
        rightArrowBtn.setLayoutData(gd);
        rightArrowBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                int index = dataTypeCbo.getSelectionIndex();

                if (index == (dataTypeCbo.getItemCount() - 1)) {
                    index = 0;
                } else {
                    ++index;
                }

                dataTypeCbo.select(index);
                changeDataType();
            }
        });
    }

    /**
     * Create the controls to change/add/update the color/value data.
     * 
     * @param parentComp
     *            Parent composite.
     */
    private void createEditColorControls(Composite parentComp) {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite editControlsComp = new Composite(parentComp, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        gl.marginHeight = 3;
        gl.marginWidth = 3;
        gl.verticalSpacing = 0;
        gl.marginTop = 0;
        editControlsComp.setLayout(gl);
        editControlsComp.setLayoutData(gd);

        // -------------------------------------
        // Create edit color container
        // -------------------------------------
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Composite editColorCenterComp = new Composite(editControlsComp,
                SWT.NONE);
        gl = new GridLayout(1, false);
        editColorCenterComp.setLayout(gl);
        editColorCenterComp.setLayoutData(gd);

        gd = new GridData(250, SWT.DEFAULT);
        Composite editColorComp = new Composite(editColorCenterComp, SWT.NONE);
        gl = new GridLayout(3, false);
        editColorComp.setLayout(gl);
        editColorComp.setLayoutData(gd);

        Label colorTextLbl = new Label(editColorComp, SWT.NONE);
        colorTextLbl.setText("Color: ");

        gd = new GridData(80, 35);
        colorLbl = new Label(editColorComp, SWT.BORDER);
        colorLbl.setLayoutData(gd);
        colorLbl.setBackground(currentColor);

        Button changeBtn = new Button(editColorComp, SWT.PUSH);
        changeBtn.setText("Change...");
        changeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
            	if (valueTF.getText() == null || valueTF.getText().equals("")) {
                    MessageBox mb = new MessageBox(shell, SWT.ICON_WARNING
                            | SWT.OK);
                    mb.setText("Choose a value");
                    mb.setMessage("Please enter a value for the color.");
                    mb.open();

                    return;
            	}
                String source = getSource();
                changeColor(currentColor.getRGB(), source);
                updateColorValueLabelBar();
            }
        });

        // -------------------------------------
        // Create value container
        // -------------------------------------
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Composite valueCenterComp = new Composite(editControlsComp, SWT.NONE);
        gl = new GridLayout(1, false);
        valueCenterComp.setLayout(gl);
        valueCenterComp.setLayoutData(gd);

        gd = new GridData(250, SWT.DEFAULT);
        Composite valueComp = new Composite(valueCenterComp, SWT.NONE);
        gl = new GridLayout(2, false);
        valueComp.setLayout(gl);
        valueComp.setLayoutData(gd);

        Label valueLbl = new Label(valueComp, SWT.NONE);
        valueLbl.setText("Value: ");

        gd = new GridData(70, SWT.DEFAULT);
        valueTF = new Text(valueComp, SWT.BORDER);
        valueTF.setLayoutData(gd);

        // -------------------------------------
        // Create value container
        // -------------------------------------
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Composite btnComp = new Composite(editControlsComp, SWT.NONE);
        gl = new GridLayout(4, false);
        btnComp.setLayout(gl);
        btnComp.setLayoutData(gd);

        int buttonWidth = 100;

        gd = new GridData(buttonWidth, SWT.DEFAULT);
        Button addBtn = new Button(btnComp, SWT.PUSH);
        addBtn.setText("Add/Update");
        addBtn.setToolTipText("Add/Update Color-Value Pair");
        addBtn.setLayoutData(gd);
        addBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {

                String source = getSource();
                updateColor(source);
                updateUsedColorValueLabelBar();
                updateUsedColorSetGroupText();
            }
        });

        gd = new GridData(buttonWidth, SWT.DEFAULT);
        Button undoBtn = new Button(btnComp, SWT.PUSH);
        undoBtn.setText("Undo");
        undoBtn.setToolTipText("Undo unsaved changes");
        undoBtn.setLayoutData(gd);
        undoBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                editColorData.resetColorValueData();
                updateColorValueLabelBar();
            }
        });

        gd = new GridData(buttonWidth, SWT.DEFAULT);
        Button deleteBtn = new Button(btnComp, SWT.PUSH);
        deleteBtn.setText("Delete");
        deleteBtn.setToolTipText("Delete Color-Value Pair");
        deleteBtn.setLayoutData(gd);
        deleteBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (valueTF.getText().compareTo(ColorScaleData.MISSING) == 0) {
                    MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR
                            | SWT.OK);
                    mb.setText("Warning");
                    mb.setMessage("Cannot remove 'missing' (MSG) from the list.");
                    mb.open();

                    return;
                } else if (valueTF.getText().compareTo(
                        ColorScaleData.LESS_THAN_MIN) == 0) {
                    MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR
                            | SWT.OK);
                    mb.setText("Warning");
                    mb.setMessage("Cannot remove 'less than min' (< Min) from the list.");
                    mb.open();

                    return;
                } else {
                    double dblValue;

                    try {
                        String source = getSource();

                        dblValue = Double.parseDouble(valueTF.getText());

                        dblValue = dblValue * 100.0;

                        dblValue = Math.round(dblValue) / 100.0;

                        editColorData.deleteColorValue(
                                source,
                                selectedDurationInSeconds + "_"
                                        + dataTypeCbo.getText(), dblValue);
                        updateColorValueLabelBar();
                    } catch (Exception ex) {
                        MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR
                                | SWT.OK);
                        mb.setText("Error");
                        mb.setMessage("Not a valid entry:\n\n"
                                + valueTF.getText());
                        mb.open();

                        return;
                    }
                }
            }
        });

    }

    /**
     * Update the color label on the display
     * 
     * @param source
     */
    public void updateColor(String source) {

        if (valueTF.getText().compareTo(ColorScaleData.MISSING) == 0) {
            // Update the missing color
            editColorData.addUpdateMissingColor(source,
                    selectedDurationInSeconds + "_" + dataTypeCbo.getText(),
                    colorLbl.getBackground().getRGB());
            updateColorValueLabelBar();
        } else if (valueTF.getText().compareTo(ColorScaleData.LESS_THAN_MIN) == 0) {
            // Update the less than color
            editColorData.addUpdateLessThanColor(source,
                    selectedDurationInSeconds + "_" + dataTypeCbo.getText(),
                    colorLbl.getBackground().getRGB());
            updateColorValueLabelBar();
        } else {
            double dblValue;

            try {
                dblValue = Double.parseDouble(valueTF.getText());

                dblValue = dblValue * 100.0;

                dblValue = Math.round(dblValue) / 100.0;

                editColorData
                        .updateColorValue(source, selectedDurationInSeconds
                                + "_" + dataTypeCbo.getText(), colorLbl
                                .getBackground().getRGB(), dblValue);

                updateColorValueLabelBar();
            } catch (Exception ex) {
                ex.printStackTrace();
                MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
                mb.setText("Error");
                mb.setMessage("Not a valid entry:\n\n" + valueTF.getText());
                mb.open();

                return;
            }
        }
    }

    /**
     * Create the used color set controls.
     * 
     * @param parentComp
     *            Parent composite.
     */
    private void createUsedColorSetGroup(Composite parentComp) {
        GridData mainGridData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        usedColorSetGroup = new Group(parentComp, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        usedColorSetGroup.setLayout(gl);
        usedColorSetGroup.setLayoutData(mainGridData);

        updateUsedColorSetGroupText();

        createUsedColorDisplay(usedColorSetGroup);

    }

    /**
     * 
     * @param parentComp
     *            Parent composite.
     */
    private void createDbControlsGroup(Composite parentComp) {
        GridData mainGridData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Group dbControlGroup = new Group(parentComp, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        dbControlGroup.setLayout(gl);
        dbControlGroup.setLayoutData(mainGridData);
        dbControlGroup.setText("Color-Value Set Database Controls");

        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Composite comboComp = new Composite(dbControlGroup, SWT.NONE);
        gl = new GridLayout(2, false);
        comboComp.setLayout(gl);
        comboComp.setLayoutData(gd);

        Label saveDataTypeLbl = new Label(comboComp, SWT.NONE);
        saveDataTypeLbl.setText("Save for datatype: ");

        saveDataTypeCbo = new Combo(comboComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        populateSaveDataTypeCombo();
        saveDataTypeCbo.select(0);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Composite buttonComp = new Composite(dbControlGroup, SWT.NONE);
        gl = new GridLayout(4, false);
        buttonComp.setLayout(gl);
        buttonComp.setLayoutData(gd);

        gd = new GridData(120, SWT.DEFAULT);
        saveAsUserBtn = new Button(buttonComp, SWT.PUSH);
        saveAsUserBtn.setText("Save as:\n" + userName);
        saveAsUserBtn.setLayoutData(gd);
        saveAsUserBtn.addSelectionListener(new SelectionListener() {

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
                widgetSelected(e);
            }

            @Override
            public void widgetSelected(SelectionEvent e) {
                boolean rval = MessageDialog
                        .openConfirm(shell, "Save Confirmation",
                                "Are you sure you want to save the displayed color set as your user set? ");

                if (rval) {
                    saveData(userName);
                    ;
                }

            }

        });

        gd = new GridData(120, SWT.DEFAULT);
        saveAsOfficeBtn = new Button(buttonComp, SWT.PUSH);
        saveAsOfficeBtn.setText("Save as:\nOffice");
        saveAsOfficeBtn.setLayoutData(gd);
        saveAsOfficeBtn.addSelectionListener(new SelectionListener() {

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
                widgetSelected(e);
            }

            @Override
            public void widgetSelected(SelectionEvent e) {
                boolean rval = MessageDialog
                        .openConfirm(shell, "Save Confirmation",
                                "Are you sure you want to save the displayed color set as your office set? ");

                if (rval) {
                    saveData(OFFICE_DEFAULT);
                }

            }

        });

        gd = new GridData(120, SWT.DEFAULT);
        deleteAsUserBtn = new Button(buttonComp, SWT.PUSH);
        deleteAsUserBtn.setText("Delete as:\n" + userName);
        deleteAsUserBtn.setLayoutData(gd);
        deleteAsUserBtn.addSelectionListener(new SelectionListener() {

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
                widgetSelected(e);
            }

            @Override
            public void widgetSelected(SelectionEvent e) {
                boolean rval = MessageDialog.openConfirm(
                        shell,
                        "Confirm",
                        "Are you sure you want to delete "
                                + dataTypeCbo.getText() + "?");
                if (rval) {
                    deleteData(userName);
                }
            }

        });

        gd = new GridData(120, SWT.DEFAULT);
        deleteAsOfficeBtn = new Button(buttonComp, SWT.PUSH);
        deleteAsOfficeBtn.setText("Delete as:\nOffice");
        deleteAsOfficeBtn.setLayoutData(gd);
        deleteAsOfficeBtn.addSelectionListener(new SelectionListener() {

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
                widgetSelected(e);
            }

            @Override
            public void widgetSelected(SelectionEvent e) {
                boolean rval = MessageDialog.openConfirm(
                        shell,
                        "Confirm",
                        "Are you sure you want to delete "
                                + dataTypeCbo.getText() + " for Office?");
                if (rval) {
                    deleteData(OFFICE_DEFAULT);
                }
            }

        });
    }

    /**
     * Create the edit color display. This displays the color and its associated
     * value.
     * 
     * @param parentComp
     *            Parent composite.
     */
    private void createEditColorDisplay(Composite parentComp) {
        String source = getSource();

        ArrayList<ColorScaleData> updatedColorSet = editColorData
                .getColorScaleDataArray(source, selectedDurationInSeconds + "_"
                        + dataTypeCbo.getText());

        int numCols = updatedColorSet.size();
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        labelComp = new Composite(parentComp, SWT.NONE);
        GridLayout labelGl = new GridLayout(numCols, true);
        labelGl.horizontalSpacing = 0;
        labelGl.verticalSpacing = 0;
        labelComp.setLayout(labelGl);
        labelComp.setLayoutData(gd);

        colorValLblArray = new ArrayList<ColorValueLabels>();

        for (int i = 0; i < numCols; i++) {
            colorValLblArray.add(new ColorValueLabels(i));
        }

        for (int i = 0; i < numCols; i++) {
            createColorLabel(labelComp, updatedColorSet.get(i).color,
                    colorValLblArray.get(i));
        }

        for (int i = 0; i < numCols; i++) {
            createValueLabel(labelComp, updatedColorSet.get(i).value,
                    colorValLblArray.get(i));
        }
    }

    /**
     * Create the edit color display for browse tab. This displays the color and
     * its associated value.
     * 
     * @param parentComp
     *            Parent composite.
     */
    private void createBrowseEditColorDisplay(Composite parentComp) {
        String source = getSource();

        ArrayList<ColorScaleData> updatedColorSet = editColorData
                .getColorScaleDataArray(source, selectedBrowseDurationInSeconds
                        + "_" + browseDataTypeCbo.getText());

        int numCols = updatedColorSet.size();
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        browseLabelComp = new Composite(parentComp, SWT.NONE);
        GridLayout labelGl = new GridLayout(numCols, true);
        labelGl.horizontalSpacing = 0;
        labelGl.verticalSpacing = 0;
        browseLabelComp.setLayout(labelGl);
        browseLabelComp.setLayoutData(gd);

        browseColorValLblArray = new ArrayList<ColorValueLabels>();

        for (int i = 0; i < numCols; i++) {
            browseColorValLblArray.add(new ColorValueLabels(i));
        }

        for (int i = 0; i < numCols; i++) {
            createBrowseColorLabel(browseLabelComp,
                    updatedColorSet.get(i).color, browseColorValLblArray.get(i));
        }

        for (int i = 0; i < numCols; i++) {
            createBrowseValueLabel(browseLabelComp,
                    updatedColorSet.get(i).value, browseColorValLblArray.get(i));
        }
    }

    /**
     * Create the color label that will display the scale color.
     * 
     * @param parent
     *            Parent composite.
     * @param rgb
     *            RGB color.
     * @param data
     *            Data class containing the color/value labels.
     */
    private void createColorLabel(Composite parent, RGB rgb,
            ColorValueLabels data) {
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        Label c1 = new Label(parent, SWT.BORDER);
        c1.setLayoutData(gd);

        c1.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseDown(MouseEvent e) {
                Label lbl = (Label) e.getSource();
                updateEditControlsValueSelected(e);
                updateEditControlsColorSelected(e);
                String source = getSource();
                changeColor(lbl.getBackground().getRGB(), source);
            }
        });

        data.setColorLabel(c1);
        data.changeLabelColor(rgb);
    }

    /**
     * Create the color label that will display the scale color.
     * 
     * @param parent
     *            Parent composite.
     * @param rgb
     *            RGB color.
     * @param data
     *            Data class containing the color/value labels.
     */
    private void createBrowseColorLabel(Composite parent, RGB rgb,
            ColorValueLabels data) {
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        Label c1 = new Label(parent, SWT.BORDER);
        c1.setLayoutData(gd);

        data.setColorLabel(c1);
        data.changeLabelColor(rgb);
    }

    /**
     * Create the label containing the scale value label.
     * 
     * @param parent
     *            Parent composite.
     * @param text
     *            Text displayed in the label.
     * @param data
     *            Data class containing the color/value labels.
     */
    private void createValueLabel(Composite parent, String text,
            ColorValueLabels data) {
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        Label lbl = new Label(parent, SWT.BORDER | SWT.CENTER);
        lbl.setText(text);
        lbl.setLayoutData(gd);
        lbl.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseDown(MouseEvent e) {
                updateEditControlsValueSelected(e);
            }
        });

        data.setValueLbl(lbl);
    }

    /**
     * Create the label containing the scale value label.
     * 
     * @param parent
     *            Parent composite.
     * @param text
     *            Text displayed in the label.
     * @param data
     *            Data class containing the color/value labels.
     */
    private void createBrowseValueLabel(Composite parent, String text,
            ColorValueLabels data) {
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        Label lbl = new Label(parent, SWT.BORDER | SWT.CENTER);
        lbl.setText(text);
        lbl.setLayoutData(gd);

        data.setValueLbl(lbl);
    }

    /**
     * Create the Used Color Set display. This displays the used color and its
     * associated value.
     * 
     * @param parentComp
     *            Parent composite.
     */
    private void createUsedColorDisplay(Composite parentComp) {
        ArrayList<ColorScaleData> usedColorSet = editColorData
                .getUsedColorScaleDataArray(sourceCbo.getText(),
                        selectedDurationInSeconds + "_" + dataTypeCbo.getText());

        int numCols = usedColorSet.size();
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        usedLabelComp = new Composite(parentComp, SWT.NONE);
        GridLayout labelGl = new GridLayout(numCols, true);
        labelGl.horizontalSpacing = 0;
        labelGl.verticalSpacing = 0;
        usedLabelComp.setLayout(labelGl);
        usedLabelComp.setLayoutData(gd);

        usedColorValLblArray = new ArrayList<ColorValueLabels>();

        for (int i = 0; i < numCols; i++) {
            usedColorValLblArray.add(new ColorValueLabels(i));
        }

        for (int i = 0; i < numCols; i++) {
            createUsedColorLabel(usedLabelComp, usedColorSet.get(i).color,
                    usedColorValLblArray.get(i));
        }

        for (int i = 0; i < numCols; i++) {
            createUsedValueLabel(usedLabelComp, usedColorSet.get(i).value,
                    usedColorValLblArray.get(i));
        }
    }

    /**
     * Create the label for the used scale color.
     * 
     * @param parent
     *            Parent composite.
     * @param rgb
     *            RGB color.
     * @param data
     *            Data class containing the color/value labels.
     */
    private void createUsedColorLabel(Composite parent, RGB rgb,
            ColorValueLabels data) {
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        Label c1 = new Label(parent, SWT.BORDER);
        c1.setLayoutData(gd);
        data.setColorLabel(c1);
        data.changeLabelColor(rgb);
    }

    /**
     * Create the label for the used scale value.
     * 
     * @param parent
     *            Parent composite.
     * @param text
     *            Scale value.
     * @param data
     *            Data class containing the color/value labels.
     */
    private void createUsedValueLabel(Composite parent, String text,
            ColorValueLabels data) {
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        Label lbl = new Label(parent, SWT.BORDER | SWT.CENTER);
        lbl.setText(text);
        lbl.setLayoutData(gd);
        data.setValueLbl(lbl);
    }

    /**
     * This method is called when a user selects a value label. The edit
     * controls are updated to reflect the selected color and scale value.
     * 
     * @param e
     *            Mouse event.
     */
    private void updateEditControlsValueSelected(MouseEvent e) {
        Label lbl = (Label) e.getSource();

        for (int i = 0; i < colorValLblArray.size(); i++) {
            if (colorValLblArray.get(i).valueLblIsEqual(lbl)) {
                if (currentColor != null) {
                    currentColor.dispose();
                }

                currentColor = new Color(getDisplay(), colorValLblArray.get(i)
                        .getRgbColor());
                colorLbl.setBackground(currentColor);

                valueTF.setText(colorValLblArray.get(i).getValueText());

                break;
            }
        }
    }

    /**
     * This method is called when a user selects a color label. A color dialog
     * pops up first allowing the user to change the selected color. The edit
     * controls are updated to reflect the selected color and scale value.
     * 
     * @param e
     *            Mouse event.
     */
    private void updateEditControlsColorSelected(MouseEvent e) {
        Label lbl = (Label) e.getSource();

        for (int i = 0; i < colorValLblArray.size(); i++) {
            if (colorValLblArray.get(i).colorLblIsEqual(lbl)) {
                valueTF.setText(colorValLblArray.get(i).getValueText());
                break;
            }
        }
    }

    /**
     * Update the user color set group text (text in the frame) to reflect the
     * data type change.
     */
    private void updateUsedColorSetGroupText() {
        StringBuffer strBuf = new StringBuffer("Used Color Set - Source: ");
        String source = getSource();
        strBuf.append(source).append("     Data-Type: ")
                .append(dataTypeCbo.getText()).append("     Duration: ")
                .append(durationCbo.getText());

        usedColorSetGroup.setText(strBuf.toString());
    }

    /**
     * Update the save data type combo box to reflect the data selected in the
     * data type combo box.
     */
    private void updateSaveDataTypeCombo() {
        int index = saveDataTypeCbo.indexOf(dataTypeCbo.getText());

        saveDataTypeCbo.select(index);
    }

    /**
     * get all durations for selected datatype and populate combo box
     */
    private void updateDurationCombo() {
        durationCbo.removeAll();

        // durationCbo.add(colorManager.getDuration(dataTypeCbo.getText()));
        String sourceKey = getSource();
        if (sourceKey.equals(DEFAULT)) {
            durationCbo.add("0");
        } else {
            // HERE is the NULL Pointer
        	String dataType = colorManager.getDataTypeName(dataTypeCbo.getText());
            Set<String> durations = editColorData.getColorDataTypeSets(
                    sourceKey).getDurations(colorManager.getDescription(dataType));
            Iterator<String> i = durations.iterator();
            while (i.hasNext()) {
                addDuration(i.next());
            }
        }
        durationCbo.select(0);
        setSelectedDuration(durationCbo.getItem(0));
    }

    /**
     * Same as updateDurationCombo but for browse tab
     */
    private void updateBrowseDurationCombo() {
        String source = getSource();
        ArrayList<String> durations;
        if (source.equals(DEFAULT)) {
            durations = new ArrayList<String>();
            durations.add("0");
        } else {
            durations = colorManager.getDurations(getSource(),
                    colorManager.getDataTypeName(browseDataTypeCbo.getText()));
        }
        browseDurationCbo.removeAll();
        for (String duration : durations) {
            addBrowseDuration(duration);
        }
        browseDurationCbo.select(0);
        setBrowseSelectedDuration(browseDurationCbo.getText());
    }

    /**
     * Update the color/value labels on the display. Updating occurs after
     * adding/updating/deleting color/value pairs or when the data type changes.
     */
    private void updateColorValueLabelBar() {
        for (int i = 0; i < colorValLblArray.size(); i++) {
            colorValLblArray.get(i).disposeLabels();
        }

        colorValLblArray.clear();

        String source = getSource();

        ArrayList<ColorScaleData> updatedColorSet = editColorData
                .getColorScaleDataArray(source, selectedDurationInSeconds + "_"
                        + dataTypeCbo.getText());
        // ArrayList<ColorScaleData> updatedColorSet = editColorData
        // .getColorScaleDataArray(source, durationCbo.getText() + "_"
        // + dataTypeCbo.getText());
       
        if (updatedColorSet.size()==0) {          
        	 updatedColorSet = editColorData
        	    .getColorScaleDataArray(source, 0 + "_"
                        + dataTypeCbo.getText());
        }
        int numCols = updatedColorSet.size();

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout labelGl = new GridLayout(numCols, true);
        labelGl.horizontalSpacing = 0;
        labelGl.verticalSpacing = 0;
        labelComp.setLayout(labelGl);
        labelComp.setLayoutData(gd);

        for (int i = 0; i < numCols; i++) {
            colorValLblArray.add(new ColorValueLabels(i));
        }

        for (int i = 0; i < numCols; i++) {
            createColorLabel(labelComp, updatedColorSet.get(i).color,
                    colorValLblArray.get(i));
        }

        for (int i = 0; i < numCols; i++) {
            createValueLabel(labelComp, updatedColorSet.get(i).value,
                    colorValLblArray.get(i));
        }

        labelComp.layout();
    }

    /**
     * same as updateColorValueLabelBar but for Browse Color Sets tab
     */
    private void updateBrowseColorValueLabelBar() {
        for (int i = 0; i < browseColorValLblArray.size(); i++) {
            browseColorValLblArray.get(i).disposeLabels();
        }

        browseColorValLblArray.clear();

        String source = getSource();

        ArrayList<ColorScaleData> updatedColorSet = editColorData
                .getColorScaleDataArray(source, selectedBrowseDurationInSeconds
                        + "_" + browseDataTypeCbo.getText());
        if (updatedColorSet == null) {
            updatedColorSet = new ArrayList<ColorScaleData>();
        }
        int numCols = updatedColorSet.size();

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout labelGl = new GridLayout(numCols, true);
        labelGl.horizontalSpacing = 0;
        labelGl.verticalSpacing = 0;
        browseLabelComp.setLayout(labelGl);
        browseLabelComp.setLayoutData(gd);

        for (int i = 0; i < numCols; i++) {
            browseColorValLblArray.add(new ColorValueLabels(i));
        }

        for (int i = 0; i < numCols; i++) {
            createBrowseColorLabel(browseLabelComp,
                    updatedColorSet.get(i).color, browseColorValLblArray.get(i));
        }

        for (int i = 0; i < numCols; i++) {
            createBrowseValueLabel(browseLabelComp,
                    updatedColorSet.get(i).value, browseColorValLblArray.get(i));
        }

        browseLabelComp.layout();
    }

    /**
     * Update the used color/value labels on the display. Updating occurs after
     * adding/updating/deleting color/value pairs or when the data type changes.
     */
    private void updateUsedColorValueLabelBar() {
        for (int i = 0; i < usedColorValLblArray.size(); i++) {
            usedColorValLblArray.get(i).disposeLabels();
        }

        usedColorValLblArray.clear();

        String source = getSource();

        // ArrayList<ColorScaleData> updatedColorSet = editColorData
        // .getUsedColorScaleDataArray(source, durationCbo.getText() + "_"
        // + dataTypeCbo.getText());
        ArrayList<ColorScaleData> updatedColorSet = editColorData
                .getUsedColorScaleDataArray(source, selectedDurationInSeconds
                        + "_" + dataTypeCbo.getText());
        
        //use default color
        if (updatedColorSet.size() == 0 ) {       	
        	 updatedColorSet = editColorData
                  .getUsedColorScaleDataArray(source, 0
                       + "_" + dataTypeCbo.getText());
        }
        int numCols = updatedColorSet.size();

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout labelGl = new GridLayout(numCols, true);
        labelGl.horizontalSpacing = 0;
        labelGl.verticalSpacing = 0;
        usedLabelComp.setLayout(labelGl);
        usedLabelComp.setLayoutData(gd);

        for (int i = 0; i < numCols; i++) {
            usedColorValLblArray.add(new ColorValueLabels(i));
        }

        for (int i = 0; i < numCols; i++) {
            createUsedColorLabel(usedLabelComp, updatedColorSet.get(i).color,
                    usedColorValLblArray.get(i));
        }

        for (int i = 0; i < numCols; i++) {
            createUsedValueLabel(usedLabelComp, updatedColorSet.get(i).value,
                    usedColorValLblArray.get(i));
        }

        usedLabelComp.layout();
    }

    /**
     * get new color from user
     * 
     * @param rgbColor
     *            current color
     */
    private void changeColor(RGB rgbColor, String source) {
        // Create the color dialog
        ColorChooserDlg colorDlg = new ColorChooserDlg(shell);

        colorDlg.setSelected(DbRGBColors.getIndexOf(rgbColor));

        String val = colorDlg.open();
        RGB rgb = null;
        if (val != null) {
            rgb = RGBColors.getRGBColor(val);
        }

        if (rgb == null) {
            if (currentColor != null) {
                currentColor.dispose();
            }
            currentColor = new Color(getDisplay(), rgbColor);

            colorLbl.setBackground(currentColor);
            return;
        }

        if (currentColor != null) {
            currentColor.dispose();
        }

        currentColor = new Color(getDisplay(), rgb);

        colorLbl.setBackground(currentColor);
        updateColorValueLabelBar();
        updateColor(source);
    }

    /**
     * Populate the source combo box.
     */
    private void populateSourceCombo() {
        // Set<String> keys = editColorData.getSourceKeys();
        //
        // for (Iterator<String> iterator = keys.iterator();
        // iterator.hasNext();) {
        // sourceCbo.add(iterator.next());
        // }
        sourceCbo.add(DEFAULT);
        sourceCbo.add(USER);
        sourceCbo.add(OFFICE);
    }

    /**
     * Populate the data type combo box.
     */
    private void populateDataTypeCombo() {
        String source = getSource();
        ColorDataTypeSets sourceKeys = editColorData
                .getColorDataTypeSets(source);

        if (sourceKeys == null) {
            return;
        }

        Set<String> keys = sourceKeys.getDataTypes();

        for (Iterator<String> iterator = keys.iterator(); iterator.hasNext();) {
            dataTypeCbo.add(iterator.next());
        }
    }

    /**
     * same as populateDataTypeCombo but for browse tab
     */
    private void populateBrowseDataTypeCombo() {
        String[] items = dataTypeCbo.getItems();
        for (int i = 0; i < items.length; ++i) {
            browseDataTypeCbo.add(items[i]);
        }
        browseDataTypeCbo.select(0);
    }

    /**
     * NOTE: This method needs to combine all of the data type from all of the
     * sources into the combo box.
     * 
     * Populate the save data type combo box.
     */
    private void populateSaveDataTypeCombo() {
        String source = DEFAULT;

        ColorDataTypeSets sourceKeys = editColorData
                .getColorDataTypeSets(source);

        if (sourceKeys == null) {
            return;
        }

        Set<String> keys = sourceKeys.getDataTypes();

        for (Iterator<String> iterator = keys.iterator(); iterator.hasNext();) {
            saveDataTypeCbo.add(iterator.next());
        }
    }

    /**
     * populates the userId Combo box
     * 
     * @return false if no users in database, true otherwise
     */
    private boolean populateUserIdCombo() {
        userIdCbo.removeAll();
        ArrayList<String> userIds = colorManager.getUsers();
        if (userIds.size() == 0) {
            return false;
        } else {
            for (String user : userIds) {
                userIdCbo.add(user);
            }
        }
        userIdCbo.select(0);
        if (editColorData.getColorDataTypeSets(userIdCbo.getText()) == null) {
            createColorData(userIdCbo.getText());
        }

        return true;
    }

    /**
     * gets the color data out of the database and populates dialog
     * 
     * @param user
     *            user to get color data for
     * @return true if colordata is available false if none to be found
     */
    private boolean createColorData(String user) {
        ColorDataTypeSets dataTypeSets = new ColorDataTypeSets();
        // get all datatypes this user has saved data for
        ArrayList<String> userDataTypes = colorManager.getDataTypes(user);

        if (userDataTypes.size() == 0) {
            return false;
        }

        String userId = user;
        String applicationName = colorManager.getApplicationName();

        HydroDBDataManager manager = HydroDBDataManager.getInstance();

        // for each datatype in database...
        for (String dataType : userDataTypes) {
            // get all durations for this datatype and this user
            ArrayList<String> durations = colorManager.getDurations(userId,
                    dataType);

            // for each duration for datatype
            for (String duration : durations) {

                ColorValueData cvd = new ColorValueData();
                cvd.setUserId(userId);
                cvd.setApplicationName(applicationName);
                cvd.setColorUseName(dataType);
                cvd.setDuration(duration);

                try {
                    // actually get the data from database
                    ArrayList<ColorValueData> data = manager.getData(cvd);
                    // sort data by double value because data is stored as
                    // String
                    // see ColorValueData class for compareTo function
                    Collections.sort(data);
                    ColorScaleSets colorScaleSets = new ColorScaleSets();
                    ArrayList<ColorScaleData> origList = new ArrayList<ColorScaleData>();

                    for (ColorValueData colorValue : data) {
                        ColorScaleData csd = new ColorScaleData();
                        if (colorValue.getThresholdValue().startsWith("-8888")) {
                            csd.lessThanMinScaleData(RGBColors
                                    .getRGBColor(colorValue.getColorName()));
                        } else if (colorValue.getThresholdValue().startsWith(
                                "-9999")) {
                            csd.missingScaleData(RGBColors
                                    .getRGBColor(colorValue.getColorName()));
                        } else {
                            csd.setColor(RGBColors.getRGBColor(colorValue
                                    .getColorName()));
                            csd.setValueLbl(Double.parseDouble(colorValue
                                    .getThresholdValue()));
                        }
                        origList.add(csd);
                    }
                    ArrayList<ColorScaleData> usedList = new ArrayList<ColorScaleData>();
                    usedList.addAll(origList);

                    colorScaleSets.setOriginalArray(origList);
                    colorScaleSets.setUsedArray(usedList);

                    // Right now last data will be only data to show up
                    // Need to incorporate duration into key for dataTypeSets
                    dataTypeSets.addDataTypeColorSets(
                            duration + "_" + colorManager.getDescription(dataType), colorScaleSets);
                } catch (VizException e) {
                    e.printStackTrace();
                }
            }
        }

        // Add source
        editColorData.addSource(user, dataTypeSets);
        return true;
    }

    /**
     * Create the close button located at the bottom of the dialog.
     */
    private void createCloseButton() {
        Composite centeredComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        centeredComp.setLayout(gl);
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        centeredComp.setLayoutData(gd);

        gd = new GridData(90, SWT.DEFAULT);
        Button closeBtn = new Button(centeredComp, SWT.NONE);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });
    }

    /**
     * Creates the default color data
     */
    private void createDefaultData() {
        ArrayList<String> defaultDataTypes = colorManager.getDefaultDataTypes();

        editColorData = new EditColorData();

        ColorDataTypeSets dataTypeSets = new ColorDataTypeSets();

        for (int i = 0; i < defaultDataTypes.size(); i++) {
            ColorScaleSets colorScaleSets = new ColorScaleSets();
            ArrayList<ColorScaleData> origList = colorManager
                    .getDefaultColorScaleData(defaultDataTypes.get(i));

            colorScaleSets.setOriginalArray(origList);

            dataTypeSets.addDataTypeColorSets("0_" + defaultDataTypes.get(i),
                    colorScaleSets);

        }

        editColorData.addSource(DEFAULT, dataTypeSets);

    }

    /**
     * update the datatypes for given source
     * 
     * @param source
     *            source to update datatypes for
     * @return false if no datatypes found true if successfully found datatypes
     */
    private boolean updateDataType(String source) {
        ColorDataTypeSets dataTypeSets = editColorData
                .getColorDataTypeSets(source);

        if (dataTypeSets == null) {
            boolean rval = createColorData(source);
            if (!rval) {
                return false;
            }
            dataTypeSets = editColorData.getColorDataTypeSets(source);
        }

        dataTypeCbo.removeAll();
        Set<String> dataTypes = dataTypeSets.getDataTypes();
        if (dataTypes.size() == 0) {
            return false;
        }
        Iterator<String> i = dataTypes.iterator();
        while (i.hasNext()) {
            String dt = i.next();
            //add a check in case there is a typo in dataType the it will be null
            if (!dt.contains("null")) {            	            	
               dataTypeCbo.add(colorManager.getDescription(dt));
            }            
        }

        if (dataTypeCbo.getItemCount() == 0) {
            dataTypeCbo.add("-----");
        }

        dataTypeCbo.select(0);
        changeDataType();
        return true;
    }

    /**
     * save the data to the database
     * 
     * @param user
     *            user to save current data as
     */
    private void saveData(String user) {
        setSelectedDuration(durationCbo.getText());

        HydroDBDataManager manager = HydroDBDataManager.getInstance();

        String userId = user;
        String applicationName = colorManager.getApplicationName();
        String colorUseName = colorManager.getDataTypeName(saveDataTypeCbo
                .getText());
        String duration = selectedDurationInSeconds.toString();

        ColorValueData cvd = new ColorValueData();
        for (ColorValueLabels cvls : colorValLblArray) {
            String threshold = cvls.getValueText();
            String colorName = cvls.getColorName();
            String thresholdUnit = "E";
            if (ColorScaleData.MISSING.equals(threshold)) {
                threshold = "-9999";
            } else if (ColorScaleData.LESS_THAN_MIN.equals(threshold)) {
                threshold = "-8888";
            }
            cvd.setApplicationName(applicationName);
            cvd.setUserId(userId);
            cvd.setColorName(colorName);
            cvd.setColorUseName(colorUseName);
            cvd.setDuration(duration);
            cvd.setThresholdUnit(thresholdUnit);
            cvd.setThresholdValue(threshold);

            try {
                manager.putData(cvd);
            } catch (VizException e1) {
                e1.printStackTrace();
            }
        }

        for (ColorValueLabels cvls : usedColorValLblArray) {
            boolean found = false;
            for (int i = 0; (i < colorValLblArray.size()) && !found; ++i) {
                String val = colorValLblArray.get(i).getValueText();
                if (val.equals(cvls.getValueText())) {
                    found = true;
                }
            }
            if (!found) {
                cvd.setApplicationName(applicationName);
                cvd.setUserId(userId);
                cvd.setColorName(cvls.getColorName());
                cvd.setColorUseName(colorUseName);
                cvd.setDuration(duration);
                cvd.setThresholdUnit("E");
                cvd.setThresholdValue(cvls.getValueText());
                try {
                    manager.deleteRecord(cvd);
                } catch (VizException e1) {
                    e1.printStackTrace();
                }
            }
        }

        if (sourceCbo.getText().equals(DEFAULT)) {
            createDefaultData();
        } else {
            createColorData(user);
        }
       
        updateDurationCombo(); 
        updateColorValueLabelBar();   
        
        setReturnValue(true);  
    }

    /**
     * dataType has changed, update dialog
     */
    private void changeDataType() {
        updateDurationCombo();
        updateColorValueLabelBar();
        updateUsedColorValueLabelBar();
        updateUsedColorSetGroupText();
        updateSaveDataTypeCombo();
    }

    private void changeDuration() {
        try {
            setSelectedDuration(durationCbo.getText());
            updateColorValueLabelBar();
            updateUsedColorValueLabelBar();
            updateUsedColorSetGroupText();
            updateSaveDataTypeCombo();
            updateUsedColorSetGroupText();
        } catch (Throwable t) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error parsing duration: " + durationCbo.getText(), t);
        }
    }

    /**
     * same as changeDataType but for browse tab
     */
    private void changeBrowseDataType() {
        updateBrowseDurationCombo();
        updateBrowseColorValueLabelBar();
    }

    /**
     * update delete buttons based on source/user
     */
    private void updateButtons() {
        if (sourceCbo.getText().equals(OFFICE)) {
            deleteAsOfficeBtn.setEnabled(true);
            deleteAsUserBtn.setEnabled(false);
        } else if (sourceCbo.getText().equals(USER)) {
            deleteAsOfficeBtn.setEnabled(false);
            if (userIdCbo.getText().equals(userName)) {
                deleteAsUserBtn.setEnabled(true);
            } else {
                deleteAsUserBtn.setEnabled(false);
            }
        } else {
            deleteAsUserBtn.setEnabled(false);
            deleteAsOfficeBtn.setEnabled(false);
        }

    }

    private void deleteData(String source) {
        HydroDBDataManager manager = HydroDBDataManager.getInstance();

        // 0. Collect data to delete (user, dataType, duration
        String dataType = dataTypeCbo.getText();
        String duration = selectedDurationInSeconds.toString();
        String dataTypeKey = duration + "_" + dataType;
        ArrayList<ColorScaleData> data = editColorData
                .getUsedColorScaleDataArray(source, duration + "_" + dataType);
        ColorValueData cvd = new ColorValueData();
        cvd.setApplicationName(colorManager.getApplicationName());
        cvd.setColorUseName(colorManager.getDataTypeName(dataType));
        cvd.setUserId(source);
        cvd.setDuration(duration);

        // 1. Delete each record from database
        for (ColorScaleData csd : data) {
            cvd.setThresholdValue(csd.getDoubleVal().toString());
            try {
                manager.deleteRecord(cvd);
            } catch (VizException e) {
                e.printStackTrace();
            }
        }

        // 2. Update editColorData
        // if (cvd.getThresholdValue().equals("-9999.0")) {
        // editColorData.deleteColorValue(source, dataTypeKey, -9999.0);
        // } else if (cvd.getThresholdValue().equals("-8888.0")) {
        // editColorData.deleteColorValue(source, dataTypeKey, -8888.0);
        // } else {
        // editColorData.deleteColorValue(source, dataTypeKey, Double
        // .parseDouble(cvd.getThresholdValue()));
        // }
        boolean dataLeft = true;
        if (source.equals(DEFAULT)) {
            createDefaultData();
        } else {
            dataLeft = createColorData(source);
        }

        // ColorDataTypeSets cdts = editColorData.getColorDataTypeSets(source);
        // cdts.addDataTypeColorSets(duration + "_" + dataType, null);
        // editColorData.addSource(source, cdts);

        // 3. Update dialog
        boolean rval = false;

        if (dataLeft) {
            rval = updateDataType(source);
        } else {
            createDefaultData();
        }
        if (!rval) {
            // No more data for source, show Default
            sourceIndex = 0;
            sourceCbo.select(0);
            updateDataType(DEFAULT);
            userIdLabel.setVisible(false);
            ((GridData) userIdLabel.getLayoutData()).exclude = true;

            userIdCbo.setVisible(false);
            ((GridData) userIdCbo.getLayoutData()).exclude = true;

            userIdFillerLabel.setVisible(false);
            ((GridData) userIdFillerLabel.getLayoutData()).exclude = true;

            comboComp.layout();
            shell.pack();
        }
    }

    /**
     * Return the source of the data (user/office/default
     * 
     * @return source of data
     */
    public String getSource() {
        sourceColor = sourceCbo.getText();

        if (sourceColor.equals(OFFICE)) {
            sourceColor = OFFICE_DEFAULT;
        } else if (sourceColor.equals(USER)) {
            sourceColor = userIdCbo.getItem(userIdCbo.getSelectionIndex());
        }

        return sourceColor;
    }

    /**
     * Shouldn't need this... should always be focused
     */
    public void bringToFront() {
        shell.setVisible(true);
        shell.forceFocus();
    }

    /**
     * Set the title of the dialog
     * 
     * @param title
     */
    public void setTitle(String title) {
        setText(title);
    }

    /**
     * Set the ColorManager for the dialog to use
     * 
     * @param cm
     */
    public void setColorManager(ColorManager cm) {
        colorManager = cm;
    }

    private void addDuration(String durInSeconds) {
        try {
            durationCbo
                    .add(String.valueOf(Integer.parseInt(durInSeconds) / 3600));
        } catch (Throwable t) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error parsing duration: " + durInSeconds, t);
        }
    }

    private void addBrowseDuration(String durInSeconds) {
        try {
            browseDurationCbo
                    .add(String.valueOf(Integer.parseInt(durInSeconds) / 3600));
        } catch (Throwable t) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error parsing duration: " + durInSeconds, t);
        }
    }

    private void setSelectedDuration(String duration) {
        selectedDurationInSeconds = Integer.parseInt(duration) * 3600;
    }

    private void setBrowseSelectedDuration(String duration) {
        selectedBrowseDurationInSeconds = Integer.parseInt(duration) * 3600;
    }
}
