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
package com.raytheon.viz.ghg.monitor;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.ghg.monitor.data.GhgConfigData;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.dialogs.colordialog.ColorWheelComp;
import com.raytheon.viz.ui.dialogs.colordialog.IColorWheelChange;

/**
 * This class displays the GHG Color Dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 25 MAR 2008  N/A        lvenable    Initial creation
 * 28 NOV 2012  1353       rferrel     Changes for non-blocking dialog.
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 * 
 */
public class GhgColorDlg extends CaveSWTDialog implements IColorWheelChange {

    final private String FOREGROUND_KEY = "Foreground_key";

    final private String BACKGROUND_KEY = "Background_key";

    /**
     * Alert level 1 radio button.
     */
    private Button alertLvl1Rdo;

    /**
     * Alert level 2 radio button.
     */
    private Button alertLvl2Rdo;

    /**
     * Expired alert radio button.
     */
    private Button expiredAlertRdo;

    /**
     * Map sections radio button.
     */
    private Button mapSectionsRdo;

    /**
     * Regular entries radio button.
     */
    private Button regEntriesRdo;

    /**
     * Monitor selections radio button.
     */
    private Button monitorSelectionsRdo;

    /**
     * Test product radio button.
     */
    private Button testProductRdo;

    /**
     * Currently selected radio button.
     */
    private Button selectedRdo;

    /**
     * Label displaying the selected foreground and background colors.
     */
    private Label colorLabel;

    /**
     * Label background color.
     */
    private Color labelBackGroundColor;

    /**
     * Label foreground color.
     */
    private Color labelForeGroundColor;

    /**
     * Large font for the color label.
     */
    private Font largeFont;

    /**
     * "Text color" label for the color wheel.
     */
    private final String TEXT_COLOR = "Text Color";

    /**
     * "Background color" label for the color wheel.
     */
    private final String BACKGROUND_COLOR = "Background Color";

    /**
     * Instance of the GHG configuration data.
     */
    private GhgConfigData ghgConfigData;

    /**
     * Color wheel composite for the foreground (text) color.
     */
    private ColorWheelComp foregroundColorWheel;

    /**
     * Color wheel composite for the background color.
     */
    private ColorWheelComp backGroundColorWheel;

    /**
     * Constructor.
     * 
     * @param parent
     *            Parent Shell.
     */
    public GhgColorDlg(Shell parent) {
        super(parent, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("GHG Color Dialog");
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
        labelBackGroundColor.dispose();
        labelForeGroundColor.dispose();
        largeFont.dispose();
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(Boolean.FALSE);

        // Get the GHG configuration data.
        ghgConfigData = GhgConfigData.getInstance();

        // Setup the font and colors.
        largeFont = new Font(getDisplay(), "Monospace", 16, SWT.BOLD);
        labelBackGroundColor = new Color(getDisplay(), ghgConfigData
                .getAlertLvl1Colors().getBackgroundRgb());
        labelForeGroundColor = new Color(getDisplay(), ghgConfigData
                .getAlertLvl1Colors().getForegroundRgb());

        // Create the main composite.
        Composite mainComposite = new Composite(shell, SWT.NONE);
        mainComposite.setLayout(new GridLayout(2, false));

        // Create the color scheme controls.
        createColorSchemeControls(mainComposite);

        // Create the color wheels.
        createColorChangeControls(mainComposite);

        // Create the buttons at the bottom of the display.
        createBottomButtons();
    }

    /**
     * Create the color scheme controls.
     * 
     * @param parentComp
     *            Parent composite.
     */
    private void createColorSchemeControls(Composite parentComp) {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Group colorSchemeGroup = new Group(parentComp, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        colorSchemeGroup.setLayout(gl);
        colorSchemeGroup.setLayoutData(gd);
        colorSchemeGroup.setText(" Color Scheme ");

        // Listener to update color based on the radio button selection.
        SelectionListener listener = new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                Button button = (Button) event.widget;
                if (button.getSelection()) {
                    RGB rgb = (RGB) button.getData(FOREGROUND_KEY);
                    foregroundColorWheel.setColor(rgb);
                    rgb = (RGB) button.getData(BACKGROUND_KEY);
                    backGroundColorWheel.setColor(rgb);
                    selectedRdo = button;
                }
            }
        };

        alertLvl1Rdo = new Button(colorSchemeGroup, SWT.RADIO);
        alertLvl1Rdo.setText("Alert Level 1");
        alertLvl1Rdo.setSelection(true);
        selectedRdo = alertLvl1Rdo;
        alertLvl1Rdo.setData(FOREGROUND_KEY, ghgConfigData.getAlertLvl1Colors()
                .getForegroundRgb());
        alertLvl1Rdo.setData(BACKGROUND_KEY, ghgConfigData.getAlertLvl1Colors()
                .getBackgroundRgb());
        alertLvl1Rdo.addSelectionListener(listener);

        alertLvl2Rdo = new Button(colorSchemeGroup, SWT.RADIO);
        alertLvl2Rdo.setText("Alert Level 2");
        alertLvl2Rdo.setData(FOREGROUND_KEY, ghgConfigData.getAlertLvl2Colors()
                .getForegroundRgb());
        alertLvl2Rdo.setData(BACKGROUND_KEY, ghgConfigData.getAlertLvl2Colors()
                .getBackgroundRgb());
        alertLvl2Rdo.addSelectionListener(listener);

        expiredAlertRdo = new Button(colorSchemeGroup, SWT.RADIO);
        expiredAlertRdo.setText("Expired Alert");
        expiredAlertRdo.setData(FOREGROUND_KEY, ghgConfigData
                .getExpiredAlertColors().getForegroundRgb());
        expiredAlertRdo.setData(BACKGROUND_KEY, ghgConfigData
                .getExpiredAlertColors().getBackgroundRgb());
        expiredAlertRdo.addSelectionListener(listener);

        mapSectionsRdo = new Button(colorSchemeGroup, SWT.RADIO);
        mapSectionsRdo.setText("Map Selections");
        mapSectionsRdo.setData(FOREGROUND_KEY, ghgConfigData
                .getMapSelectionsColors().getForegroundRgb());
        mapSectionsRdo.setData(BACKGROUND_KEY, ghgConfigData
                .getMapSelectionsColors().getBackgroundRgb());
        mapSectionsRdo.addSelectionListener(listener);

        regEntriesRdo = new Button(colorSchemeGroup, SWT.RADIO);
        regEntriesRdo.setText("Regular Entries");
        regEntriesRdo.setData(FOREGROUND_KEY, ghgConfigData
                .getRegularEntriesColors().getForegroundRgb());
        regEntriesRdo.setData(BACKGROUND_KEY, ghgConfigData
                .getRegularEntriesColors().getBackgroundRgb());
        regEntriesRdo.addSelectionListener(listener);

        monitorSelectionsRdo = new Button(colorSchemeGroup, SWT.RADIO);
        monitorSelectionsRdo.setText("Monitor Selections");
        monitorSelectionsRdo.setData(FOREGROUND_KEY, ghgConfigData
                .getMonitorSelectionsColors().getForegroundRgb());
        monitorSelectionsRdo.setData(BACKGROUND_KEY, ghgConfigData
                .getMonitorSelectionsColors().getBackgroundRgb());
        monitorSelectionsRdo.addSelectionListener(listener);

        testProductRdo = new Button(colorSchemeGroup, SWT.RADIO);
        testProductRdo.setText("Test Product");
        testProductRdo.setData(FOREGROUND_KEY, ghgConfigData
                .getTestProductsColors().getForegroundRgb());
        testProductRdo.setData(BACKGROUND_KEY, ghgConfigData
                .getTestProductsColors().getBackgroundRgb());
        testProductRdo.addSelectionListener(listener);
    }

    /**
     * Create the color wheels to change the text foreground and background
     * colors.
     * 
     * @param parentComp
     *            Parent composite.
     */
    private void createColorChangeControls(Composite parentComp) {
        Composite colorComp = new Composite(parentComp, SWT.NONE);
        colorComp.setLayout(new GridLayout(1, false));

        foregroundColorWheel = new ColorWheelComp(colorComp, this, TEXT_COLOR,
                true);
        foregroundColorWheel.showRgbSliders(false);
        foregroundColorWheel.setColor(labelForeGroundColor.getRGB());

        backGroundColorWheel = new ColorWheelComp(colorComp, this,
                BACKGROUND_COLOR, true);
        backGroundColorWheel.showRgbSliders(false);
        backGroundColorWheel.setColor(labelBackGroundColor.getRGB());

        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        colorLabel = new Label(colorComp, SWT.CENTER);
        colorLabel.setText("Sample Text");
        colorLabel.setFont(largeFont);
        colorLabel.setBackground(labelBackGroundColor);
        colorLabel.setForeground(labelForeGroundColor);
        colorLabel.setLayoutData(gd);
    }

    /**
     * Create the bottom controls buttons.
     */
    private void createBottomButtons() {
        Composite buttonArea = new Composite(shell, SWT.NONE);
        buttonArea.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
        buttonArea.setLayout(new GridLayout(1, false));

        // The intent is for this composite to be centered
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Composite buttons = new Composite(buttonArea, SWT.NONE);
        buttons.setLayoutData(gd);
        buttons.setLayout(new GridLayout(2, true));

        gd = new GridData(120, SWT.DEFAULT);
        Button applyColorsBtn = new Button(buttons, SWT.PUSH);
        applyColorsBtn.setText("Apply Colors");
        applyColorsBtn.setLayoutData(gd);
        applyColorsBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                setReturnValue(true);
                saveButtonColor();
                close();
            }
        });

        gd = new GridData(120, SWT.DEFAULT);
        Button cancelBtn = new Button(buttons, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                setReturnValue(false);
                close();
            }
        });
    }

    /**
     * Change the color of the label foreground or background color based on the
     * selection in the color wheels.
     */
    public void colorChange(RGB rgb, String colorWheelTitle) {
        if (colorWheelTitle.compareTo(TEXT_COLOR) == 0) {
            labelForeGroundColor.dispose();
            labelForeGroundColor = new Color(getDisplay(), rgb);
            colorLabel.setForeground(labelForeGroundColor);
            selectedRdo.setData(FOREGROUND_KEY, rgb);
        } else {
            labelBackGroundColor.dispose();
            labelBackGroundColor = new Color(getDisplay(), rgb);
            colorLabel.setBackground(labelBackGroundColor);
            selectedRdo.setData(BACKGROUND_KEY, rgb);
        }
    }

    /**
     * Color changes are being applied save them back to the GhgConfigData.
     */
    private void saveButtonColor() {
        RGB rgb = null;

        rgb = (RGB) alertLvl1Rdo.getData(FOREGROUND_KEY);
        ghgConfigData.getAlertLvl1Colors().setForegroundRgb(rgb);
        rgb = (RGB) alertLvl1Rdo.getData(BACKGROUND_KEY);
        ghgConfigData.getAlertLvl1Colors().setBackgroundRgb(rgb);

        rgb = (RGB) alertLvl2Rdo.getData(FOREGROUND_KEY);
        ghgConfigData.getAlertLvl2Colors().setForegroundRgb(rgb);
        rgb = (RGB) alertLvl2Rdo.getData(BACKGROUND_KEY);
        ghgConfigData.getAlertLvl2Colors().setBackgroundRgb(rgb);

        rgb = (RGB) expiredAlertRdo.getData(FOREGROUND_KEY);
        ghgConfigData.getExpiredAlertColors().setForegroundRgb(rgb);
        rgb = (RGB) expiredAlertRdo.getData(BACKGROUND_KEY);
        ghgConfigData.getExpiredAlertColors().setBackgroundRgb(rgb);

        rgb = (RGB) mapSectionsRdo.getData(FOREGROUND_KEY);
        ghgConfigData.getMapSelectionsColors().setForegroundRgb(rgb);
        rgb = (RGB) mapSectionsRdo.getData(BACKGROUND_KEY);
        ghgConfigData.getMapSelectionsColors().setBackgroundRgb(rgb);

        rgb = (RGB) regEntriesRdo.getData(FOREGROUND_KEY);
        ghgConfigData.getRegularEntriesColors().setForegroundRgb(rgb);
        rgb = (RGB) regEntriesRdo.getData(BACKGROUND_KEY);
        ghgConfigData.getRegularEntriesColors().setBackgroundRgb(rgb);

        rgb = (RGB) monitorSelectionsRdo.getData(FOREGROUND_KEY);
        ghgConfigData.getMonitorSelectionsColors().setForegroundRgb(rgb);
        rgb = (RGB) monitorSelectionsRdo.getData(FOREGROUND_KEY);
        ghgConfigData.getMonitorSelectionsColors().setBackgroundRgb(rgb);

        rgb = (RGB) testProductRdo.getData(FOREGROUND_KEY);
        ghgConfigData.getTestProductsColors().setForegroundRgb(rgb);
        rgb = (RGB) testProductRdo.getData(BACKGROUND_KEY);
        ghgConfigData.getTestProductsColors().setBackgroundRgb(rgb);
    }
}