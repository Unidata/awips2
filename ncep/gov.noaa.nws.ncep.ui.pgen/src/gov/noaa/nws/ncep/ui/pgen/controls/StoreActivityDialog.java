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

import java.util.Calendar;
import java.util.TimeZone;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.DateTime;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.viz.core.exception.VizException;
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

    private Text modeText;

    private Text statusText;

    private DateTime validDate;

    private DateTime validTime;

    private Button autoSaveOffBtn;

    private Button autoSaveOnBtn;

    private PgenResource rsc;

    private Product activity;

    private Text messageText;

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
            title = "Open a PGEN Product file";
        } else if (btnName.equals("Save") || btnName.equals("Save All")) {
            title = "Save the PGEN Product";
        } else if (btnName.equals("Save As")) {
            title = "Save the PGEN Product as";
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
        Group g4 = new Group(dlgAreaForm, SWT.NONE);
        g4.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
        createMessageArea(g4);

        setDialogFields();
        return dlgAreaForm;
    }

    private void createActivityInfoArea(Composite g1) {

        g1.setLayout(new GridLayout(2, false));
        GridData gdata = new GridData(SWT.FILL, SWT.CENTER, true, false);

        Label infoLabel = new Label(g1, SWT.NONE);
        infoLabel.setText("Activity Info:");

        infoText = new Text(g1, SWT.NONE);
        infoText.setLayoutData(gdata);

        Label nameLabel = new Label(g1, SWT.NONE);
        nameLabel.setText("Activity Name:");

        nameText = new Text(g1, SWT.NONE);
        nameText.setLayoutData(gdata);

        Label typeLabel = new Label(g1, SWT.NONE);
        typeLabel.setText("Activity Type:");

        typeText = new Text(g1, SWT.NONE);
        typeText.setLayoutData(gdata);

        Label subtypeLabel = new Label(g1, SWT.NONE);
        subtypeLabel.setText("Activity Subtype:");

        subtypeText = new Text(g1, SWT.NONE);
        subtypeText.setLayoutData(gdata);

        Label siteLabel = new Label(g1, SWT.NONE);
        siteLabel.setText("Site:");

        siteText = new Text(g1, SWT.NONE);
        siteText.setLayoutData(gdata);

        Label deskLabel = new Label(g1, SWT.NONE);
        deskLabel.setText("Desk:");

        deskText = new Text(g1, SWT.NONE);
        deskText.setLayoutData(gdata);

        Label forecasterLabel = new Label(g1, SWT.NONE);
        forecasterLabel.setText("Forecaster:");

        forecasterText = new Text(g1, SWT.NONE);
        forecasterText.setLayoutData(gdata);

        Label modeLabel = new Label(g1, SWT.NONE);
        modeLabel.setText("Operating Mode:");

        modeText = new Text(g1, SWT.NONE);
        modeText.setLayoutData(gdata);

        Label statusLabel = new Label(g1, SWT.NONE);
        statusLabel.setText("Operating Mode:");

        statusText = new Text(g1, SWT.NONE);
        statusText.setLayoutData(gdata);

    }

    private void createRefTimeArea(Composite g2) {

        g2.setLayout(new GridLayout(4, false));

        Label refTimeLabel = new Label(g2, SWT.NONE);
        refTimeLabel.setText("Ref Time:");

        validDate = new DateTime(g2, SWT.BORDER | SWT.DATE);
        validTime = new DateTime(g2, SWT.BORDER | SWT.TIME | SWT.SHORT);

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

    private void createMessageArea(Composite g4) {
        g4.setLayout(new GridLayout(1, true));
        GridData gdata = new GridData(SWT.FILL, SWT.CENTER, true, false);

        messageText = new Text(g4, SWT.MULTI | SWT.READ_ONLY | SWT.H_SCROLL
                | SWT.V_SCROLL);
        messageText.setLayoutData(gdata);
    }

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

        if (activity.getOutputFile() != null)
            infoText.setText(activity.getOutputFile());
        if (activity.getName() != null)
            nameText.setText(activity.getName());
        if (activity.getType() != null)
            typeText.setText(activity.getType());
        if (activity.getCenter() != null)
            siteText.setText(activity.getCenter());
        if (activity.getForecaster() != null)
            forecasterText.setText(activity.getForecaster());
        modeText.setText(CAVEMode.getMode().name());
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

        activity.setInputFile(activityLabel);
        activity.setOutputFile(activityLabel);

        ActivityInfo info = getActivityInfo();
        messageText.setText("Sending Activity...");
        messageText.setBackground(shell.getDisplay().getSystemColor(
                SWT.COLOR_WIDGET_BACKGROUND));
        messageText.redraw();

        try {
            StorageUtils.storeProduct(info, activity, true);
        } catch (PgenStorageException e) {
            e.printStackTrace();
            messageText.setText(e.getMessage());
            messageText.setBackground(shell.getDisplay().getSystemColor(
                    SWT.COLOR_RED));
            if (e.getMessage().contains("\n")) {
                messageText.setSize(SWT.DEFAULT,
                        5 * messageText.getLineHeight());
                messageText.getShell().pack();
                messageText.getShell().layout();
            }
            return;
        }

        close();
        PgenFileNameDisplay.getInstance().setFileName(activityLabel);
        PgenUtil.setSelectingMode();
        rsc.getResourceData().setNeedsSaving(false);
    }

    private ActivityInfo getActivityInfo() {

        ActivityInfo info = new ActivityInfo();
        info.setActivityLabel(infoText.getText());
        info.setActivityName(nameText.getText());
        info.setActivityType(typeText.getText());
        info.setActivitySubtype(subtypeText.getText());
        info.setSite(siteText.getText());
        info.setDesk(deskText.getText());
        info.setForecaster(forecasterText.getText());
        info.setMode(modeText.getText());
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
