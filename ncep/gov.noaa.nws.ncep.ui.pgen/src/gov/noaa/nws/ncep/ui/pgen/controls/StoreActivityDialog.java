/*
 * gov.noaa.nws.ncep.ui.pgen.controls.StoreActivityeDialog
 * 
 * 27 March 2013
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.controls;

import gov.noaa.nws.ncep.common.dataplugin.pgen.ActivityInfo;
import gov.noaa.nws.ncep.ui.pgen.PgenSession;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.elements.Product;
import gov.noaa.nws.ncep.ui.pgen.rsc.PgenResource;
import gov.noaa.nws.ncep.ui.pgen.store.PgenStorageException;
import gov.noaa.nws.ncep.ui.pgen.store.StorageUtils;

import java.io.File;
import java.util.Calendar;
import java.util.TimeZone;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.DateTime;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.viz.core.mode.CAVEMode;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * Create a dialog to Store PGEN products to EDEX.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	-----------------------------------
 * 03/13		#977		S. Gilbert	Initial creation
 * 01/14        #1105       J. Wu       Pre-fill for each activity info.
 * 
 * </pre>
 * 
 * @author
 * @version 1
 */
public class StoreActivityDialog extends CaveJFACEDialog {

    private String title = null;

    private Shell shell;

    private static final int SAVE_ID = IDialogConstants.CLIENT_ID + 3841;

    private static final String SAVE_LABEL = "Save";

    private static final int CANCEL_ID = IDialogConstants.CLIENT_ID + 3842;

    private static final String CANCEL_LABEL = "Cancel";

    private Button cancelBtn = null;

    private Button saveBtn = null;

    private Text infoText = null;

    private Text nameText = null;

    private Text typeText = null;

    private Text subtypeText = null;

    private Text siteText = null;

    private Text deskText = null;

    private Text forecasterText = null;

    private Combo modeCombo;

    private Text statusText;

    private DateTime validDate;

    private DateTime validTime;

    private Button autoSaveOffBtn;

    private Button autoSaveOnBtn;

    private PgenResource rsc;

    private Product activity;

    // private Text messageText;

    /*
     * Constructor
     */
    public StoreActivityDialog(Shell parShell, String btnName)
            throws VizException {

        super(parShell);
        setStoreMode(btnName);
        rsc = PgenSession.getInstance().getPgenResource();
        activity = rsc.getActiveProduct();

    }

    /*
     * Set up the file mode.
     */
    private void setStoreMode(String btnName) {

        if (btnName.equals("Open")) {
            title = "Open a PGEN Activity file";
        } else if (btnName.equals("Save") || btnName.equals("Save All")) {
            title = "Save the PGEN Activity";
        } else if (btnName.equals("Save As")) {
            title = "Save the PGEN Activity as";
        }

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
        this.setShellStyle(SWT.RESIZE | SWT.PRIMARY_MODAL);
        super.configureShell(shell);

        this.shell = shell;
        if (title != null) {
            shell.setText(title);
        }
    }

    /**
     * (non-Javadoc) Create all of the widgets on the Dialog
     * 
     * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
     */
    @Override
    public Control createDialogArea(Composite parent) {

        Composite dlgAreaForm = (Composite) super.createDialogArea(parent);
        GridLayout mainLayout = new GridLayout(1, true);
        mainLayout.marginHeight = 3;
        mainLayout.marginWidth = 3;
        dlgAreaForm.setLayout(mainLayout);

        /*
         * Initialize all of the Storm Information Section
         */
        Composite g1 = new Composite(dlgAreaForm, SWT.NONE);
        g1.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
        createActivityInfoArea(g1);

        /*
         * Initialize all of the Reference Time Section
         */
        Composite g2 = new Composite(dlgAreaForm, SWT.NONE);
        g2.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
        createRefTimeArea(g2);

        /*
         * Initialize all of the Reference Time Section
         */
        Composite g3 = new Composite(dlgAreaForm, SWT.NONE);
        g3.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
        createAutoSaveArea(g3);

        /*
         * Initialize the Message Area
         */
        /*
         * Group g4 = new Group(dlgAreaForm, SWT.NONE); g4.setLayoutData(new
         * GridData(GridData.FILL_HORIZONTAL)); createMessageArea(g4);
         */

        setDialogFields();
        return dlgAreaForm;
    }

    private void createActivityInfoArea(Composite g1) {

        g1.setLayout(new GridLayout(2, false));
        GridData gdata = new GridData(SWT.FILL, SWT.CENTER, true, false);

        Label infoLabel = new Label(g1, SWT.NONE);
        infoLabel.setText("Activity Label*:");

        infoText = new Text(g1, SWT.NONE);
        infoText.setLayoutData(gdata);
        infoText.setToolTipText("Input a file name - required.");

        Label nameLabel = new Label(g1, SWT.NONE);
        nameLabel.setText("Activity Name*:");

        nameText = new Text(g1, SWT.NONE);
        nameText.setLayoutData(gdata);
        nameText.setToolTipText("Alias for this activity, just like your first name while activity "
                + "type/subtype is the last name. Leave it as is");

        Label typeLabel = new Label(g1, SWT.NONE);
        typeLabel.setText("Activity Type*:");
        typeLabel.setEnabled(false);

        typeText = new Text(g1, SWT.NONE);
        typeText.setLayoutData(gdata);
        typeText.setEditable(false);
        typeText.setToolTipText("Activity type as defined. Leave it as is");

        Label subtypeLabel = new Label(g1, SWT.NONE);
        subtypeLabel.setText("Activity Subtype*:");
        subtypeLabel.setEnabled(false);

        subtypeText = new Text(g1, SWT.NONE);
        subtypeText.setLayoutData(gdata);
        subtypeText.setEditable(false);
        subtypeText
                .setToolTipText("Activity subtype as defined. Leave it as is");

        Label siteLabel = new Label(g1, SWT.NONE);
        siteLabel.setText("Site*:");
        siteLabel.setEnabled(false);

        siteText = new Text(g1, SWT.NONE);
        siteText.setLayoutData(gdata);
        siteText.setEditable(false);
        siteText.setToolTipText("Site defined in localization. Leave it as is");

        Label deskLabel = new Label(g1, SWT.NONE);
        deskLabel.setText("Desk:");
        deskLabel.setEnabled(false);

        deskText = new Text(g1, SWT.NONE);
        deskText.setLayoutData(gdata);
        deskText.setEditable(false);
        deskText.setToolTipText("Desk defined in localization or set when starting CAVE. Leave it as is");

        Label forecasterLabel = new Label(g1, SWT.NONE);
        forecasterLabel.setText("Forecaster:");

        forecasterText = new Text(g1, SWT.NONE);
        forecasterText.setLayoutData(gdata);
        forecasterText
                .setToolTipText("Forecaster's name, default is your user name, not required");

        Label modeLabel = new Label(g1, SWT.NONE);
        modeLabel.setText("Operating Mode:");

        modeCombo = new Combo(g1, SWT.DROP_DOWN | SWT.READ_ONLY);

        for (CAVEMode cm : CAVEMode.values()) {
            modeCombo.add(cm.name());
        }

        modeCombo.select(0);
        modeCombo
                .setToolTipText("CAVE mode set at starting, or pick one here.");

        Label statusLabel = new Label(g1, SWT.NONE);
        statusLabel.setText("Activity Status:");

        statusText = new Text(g1, SWT.NONE);
        statusText.setLayoutData(gdata);
        statusText.setToolTipText("Activity status, not in use yet.");

    }

    private void createRefTimeArea(Composite g2) {

        g2.setLayout(new GridLayout(4, false));

        Label refTimeLabel = new Label(g2, SWT.NONE);
        refTimeLabel.setText("Ref Time*:");

        validDate = new DateTime(g2, SWT.BORDER | SWT.DATE);

        validDate
                .setToolTipText("Activity's reference date, changing it and saving the "
                        + "activity will save the current activity as a new entry in PGEN DB.");

        validTime = new DateTime(g2, SWT.BORDER | SWT.TIME | SWT.SHORT);

        validTime
                .setToolTipText("Activity's reference time, changing it and saving the "
                        + "activity will save the current activity as a new entry in PGEN DB.");

        Label utcLabel = new Label(g2, SWT.NONE);
        utcLabel.setText("UTC");

    }

    private void createAutoSaveArea(Composite g3) {
        g3.setLayout(new GridLayout(3, false));

        Label autoSaveLbl = new Label(g3, SWT.NONE);
        autoSaveLbl.setText("Auto Save:");

        autoSaveOffBtn = new Button(g3, SWT.RADIO);
        autoSaveOffBtn.setText("Off");
        autoSaveOffBtn.setSelection(true);

        autoSaveOnBtn = new Button(g3, SWT.RADIO);
        autoSaveOnBtn.setText("On");
        autoSaveOnBtn.setSelection(false);

    }

    /*
     * private void createMessageArea(Composite g4) { g4.setLayout(new
     * GridLayout(1, true)); GridData gdata = new GridData(SWT.FILL, SWT.CENTER,
     * true, false);
     * 
     * messageText = new Text(g4, SWT.MULTI | SWT.READ_ONLY | SWT.H_SCROLL |
     * SWT.V_SCROLL); messageText.setLayoutData(gdata); }
     */
    /**
     * Save/Cancel button for "Save" a product file.
     */
    @Override
    protected void createButtonsForButtonBar(Composite parent) {

        saveBtn = createButton(parent, SAVE_ID, SAVE_LABEL, true);

        cancelBtn = createButton(parent, CANCEL_ID, CANCEL_LABEL, true);

    }

    @Override
    protected void buttonPressed(int buttonId) {

        if (buttonId == SAVE_ID) {
            storeProducts();
        } else if (buttonId == CANCEL_ID) {
            close();
        }
    }

    private void setDialogFields() {

        if (activity.getOutputFile() != null) {
            infoText.setText(activity.getOutputFile());
        } else {
            String filename = PgenSession.getInstance().getPgenResource()
                    .buildFileName(activity);
            filename = filename
                    .substring(filename.lastIndexOf(File.separator) + 1);
            // filename = filename.replace(".xml", ""); // remove ending ".xml"

            infoText.setText(filename);
        }

        if (activity.getName() != null)
            nameText.setText(activity.getName());

        /*
         * Activity type/subtype is stored in Product as "type(subtype)", so we
         * need to split it up here.
         */
        String type = activity.getType();
        if (type != null) {
            int loc1 = type.indexOf("(");
            if (loc1 > 0) {
                typeText.setText(type.substring(0, loc1));
                String subtype = type.substring(loc1 + 1).replace(")", "");
                if (subtype.length() > 0 && !subtype.equalsIgnoreCase("NONE"))
                    subtypeText.setText(subtype);
            } else {
                typeText.setText(type);
            }
        }

        siteText.setText(PgenUtil.getCurrentOffice());

        // get the desk info.
        String desk = LocalizationManager.getContextName(LocalizationLevel
                .valueOf("DESK"));

        if (desk != null && !desk.equalsIgnoreCase("none")) {
            deskText.setText(desk);
        }

        forecasterText.setText(System.getProperty("user.name"));

        // Select the cave mode.
        String mode = CAVEMode.getMode().name();
        int index = 0;
        int ii = 0;
        for (String md : modeCombo.getItems()) {
            if (md.equals(mode)) {
                index = ii;
                break;
            }
            ii++;
        }
        modeCombo.select(index);

        statusText.setText("Unknown");

        Calendar datetime = activity.getTime().getStartTime();
        if (datetime != null) {
            validDate.setYear(datetime.get(Calendar.YEAR));
            validDate.setMonth(datetime.get(Calendar.MONTH));
            validDate.setDay(datetime.get(Calendar.DAY_OF_MONTH));
            validTime.setHours(datetime.get(Calendar.HOUR_OF_DAY));
            validTime.setMinutes(datetime.get(Calendar.MINUTE));
        }

    }

    /**
     * Store the products to EDEX.
     */
    private void storeProducts() {

        String activityLabel = infoText.getText();

        if (activityLabel == null || activityLabel.isEmpty()) {
            MessageDialog confirmDlg = new MessageDialog(
                    PlatformUI.getWorkbench().getActiveWorkbenchWindow()
                            .getShell(),
                    "Need More Information",
                    null,
                    "Activity Info field is required.\nPlease enter an appropriate string and then try saving!",
                    MessageDialog.WARNING, new String[] { "OK" }, 0);

            confirmDlg.open();
            infoText.forceFocus();
            return;

        }

        rsc.setAutosave(autoSaveOnBtn.getSelection());
        rsc.setAutoSaveFilename(activityLabel);

        // PgenSession.getInstance().getPgenPalette().setActiveIcon("Select");

        // activity.setInputFile(activityLabel);
        // activity.setOutputFile(activityLabel);

        ActivityInfo info = getActivityInfo();
        activity.setInputFile(info.getActivityLabel());
        activity.setOutputFile(info.getActivityLabel());
        activity.setCenter(info.getSite());
        activity.setForecaster(info.getForecaster());

        /*
         * messageText.setText("Sending Activity...");
         * messageText.setBackground(shell.getDisplay().getSystemColor(
         * SWT.COLOR_WIDGET_BACKGROUND)); messageText.redraw();
         */
        try {
            // StorageUtils.storeProduct(info, activity, true);
            String uri = StorageUtils.storeProduct(info, activity, true);
            // System.out.println("Activity saved at dataURI: " + uri);
        } catch (PgenStorageException e) {
            e.printStackTrace();
            /*
             * messageText.setText(e.getMessage());
             * messageText.setBackground(shell.getDisplay().getSystemColor(
             * SWT.COLOR_RED)); if (e.getMessage().contains("\n")) {
             * messageText.setSize(SWT.DEFAULT, 5 *
             * messageText.getLineHeight()); messageText.getShell().pack();
             * messageText.getShell().layout(); }
             */
            return;
        }

        close();
        PgenFileNameDisplay.getInstance().setFileName(activityLabel);
        PgenUtil.setSelectingMode();
        rsc.getResourceData().setNeedsSaving(false);
    }

    private ActivityInfo getActivityInfo() {

        ActivityInfo info = new ActivityInfo();
        String lbl = infoText.getText();
        if (!lbl.endsWith(".xml")) {
            lbl += ".xml";
        }
        info.setActivityLabel(lbl);
        info.setActivityName(nameText.getText());
        info.setActivityType(typeText.getText());
        info.setActivitySubtype(subtypeText.getText());
        info.setSite(siteText.getText());
        info.setDesk(deskText.getText());
        info.setForecaster(forecasterText.getText());
        info.setMode(modeCombo.getText());
        info.setStatus(statusText.getText());

        Calendar refTime = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        refTime.set(validDate.getYear(), validDate.getMonth(),
                validDate.getDay(), validTime.getHours(),
                validTime.getMinutes(), 0);
        refTime.set(Calendar.MILLISECOND, 0);

        info.setRefTime(refTime);

        return info;
    }
}