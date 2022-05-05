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

import java.io.File;
import java.io.IOException;
import java.text.DecimalFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TimeZone;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.resource.ImageRegistry;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.printing.PrintDialog;
import org.eclipse.swt.printing.Printer;
import org.eclipse.swt.printing.PrinterData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;
import org.eclipse.ui.plugin.AbstractUIPlugin;

import com.raytheon.uf.common.activetable.ActiveTableMode;
import com.raytheon.uf.common.activetable.ActiveTableRecord;
import com.raytheon.uf.common.activetable.GetActiveTableRequest;
import com.raytheon.uf.common.activetable.GetActiveTableResponse;
import com.raytheon.uf.common.activetable.VTECChange;
import com.raytheon.uf.common.activetable.VTECTableChangeNotification;
import com.raytheon.uf.common.dataplugin.gfe.textproduct.DraftProduct;
import com.raytheon.uf.common.dataplugin.gfe.textproduct.ProductDefinition;
import com.raytheon.uf.common.dataplugin.text.db.MixedCaseProductSupport;
import com.raytheon.uf.common.jms.notification.INotificationObserver;
import com.raytheon.uf.common.jms.notification.NotificationException;
import com.raytheon.uf.common.jms.notification.NotificationMessage;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.notification.jobs.NotificationManagerJob;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.uf.viz.spellchecker.dialogs.SpellCheckDlg;
import com.raytheon.viz.core.mode.CAVEMode;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.GFEPreference;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.dialogs.formatterlauncher.ConfigData.ProductStateEnum;
import com.raytheon.viz.gfe.product.ProductFileUtil;
import com.raytheon.viz.gfe.product.TextDBUtil;
import com.raytheon.viz.ui.dialogs.ICloseCallback;
import com.raytheon.viz.ui.simulatedtime.SimulatedTimeOperations;

/**
 * Composite containing the product editor controls.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#      Engineer  Description
 * ------------- ------------ --------- ----------------------------------------
 * Apr 18, 2008  ###          lvenable  Initial creation
 * Jan 19, 2010  4085         ryu       Save and load draft
 * Feb 19, 2010  4132         ryu       Product correction.
 * Feb 23, 2010  3463         rtran     CallToActions dialogs
 * Mar 03, 2010  4765         MW Fegan  Use common spell checker.
 * Mar 22, 2010  2053         wkwock    Fix issue time in text
 * Mar 24, 2010  4255         wkwock    Fix expire time in text
 * Aug 02, 2010  2187         cjeanbap  Update variable/method signature to be
 *                                      consistent.
 * Aug 20, 2010  4687         cjeanbap  Fixed NullPointerException.
 * Aug 27, 2010  6730         jnjanga   Port FixVTEC functionality from A1.
 * Sep 21, 2010  5817         jnjanga   Fix attribution phrase append from
 *                                      issuance to issuance.
 * Oct 27, 2010  5817         jnjanga   Fix N.P.E caused when call
 *                                      getVTECActionCodes on non VTEC products.
 * Aug 31, 2012  15178        mli       Add autoWrite and autoStore capability
 * Aug 31, 2012  15037        mli       Handle bad characters in text formatter
 *                                      definition
 * Nov 07, 2012  1298         rferrel   Changes for non-blocking
 *                                      CallToActionsDlg. Changes for
 *                                      non-blocking FindReplaceDlg. Changes for
 *                                      non-blocking StoreTransmitDlg. Changes
 *                                      for non-blocking WrapLengthDialog.
 * Feb 08, 2013  12851        jzeng     Add menuToAddTo in create*Menu Create
 *                                      createEditorPopupMenu() Add
 *                                      mouselistener in createTextControl() for
 *                                      StyledText
 * Feb 28, 2013  15889        ryu       Removed detachAttributionPhrase and
 *                                      getVTECActionCodes
 * Feb 12, 2013  1597         randerso  Code cleanup. Fixed possible widget
 *                                      disposed errors on shut down.
 * May 08, 2013  1842         dgilling  Add alternate setProductText(), fix
 *                                      warnings.
 * Sep 03, 2013  16534        ryu       Refactor; sneak in a change for Ron (RM
 *                                      #1597).
 * Jan 06, 2014  2649         dgilling  Pass flag to StoreTransmitDlg to only
 *                                      update VTEC lines on products that
 *                                      aren't being corrected.
 * Feb 05, 2014  17022        ryu       Modified loadDraft() to fix merging of
 *                                      WMO heading and AWIPS ID.
 * Mar 25, 2014  2884         randerso  Added xxxid to check for disabling
 *                                      editor
 * May 12, 2014  16195        zhao      Modified widgetSelected() for "Auto
 *                                      Wrap" option widget
 * Oct 20, 2014  3685         randerso  Made conversion to upper case
 *                                      conditional on product id
 * Dec 01, 2014  624          zhao      Modified saveFile()
 * Dec 16, 2014  14946        ryu       Modified updateIssueExpireTimes() so
 *                                      issuance time is displayed for the local
 *                                      time zones for each segment.
 * Jan 28, 2015  4018         randerso  Code cleanup.
 * Feb 04, 2014  17039        ryu       Removed menu item related to the
 *                                      HighlighFramingCodes feature.
 * Apr 20, 2015  4027         randerso  Renamed ProductStateEnum with an initial
 *                                      capital Expunged Calendar from
 *                                      ActiveTableRecord
 * Jul 02, 2015  13753        lshi      Update times for products in Product
 *                                      Editor
 * Jul 22, 2015  13753        lshi      Keeps issue time unchanged
 * Jul 28, 2015  4263         dgilling  Support changes to TextProductManager.
 * Aug 06, 2015  13753        lshi      removed updateTime flag, undo the change
 *                                      of updateIssueExpireTimes, etc.
 * Aug 10, 2015  4721         randerso  Changed getNNNid() to use the productID
 *                                      field (not textdbPil)
 * Aug 31, 2015  4749         njensen   Changed setCloseCallback to
 *                                      addCloseCallback
 * Sep 15, 2015  4858         dgilling  Disable store/transmit in DRT mode.
 * Oct 14, 2015  4959         dgilling  Support new function signature for
 *                                      wordWrap.
 * Oct 26, 2015  18244        lshi      fixed NullPointerException (pds,
 *                                      updateIssueExpireTimes)
 * Dec 14, 2015  18367        ryu       Disable finalization of ETN when product
 *                                      is stored to text database.
 * Dec 16, 2015  18410        lshi      For corrected products, both WMO time
 *                                      and MND time should match the current
 *                                      time
 * Jan 21, 2016  18505        lshi      Resent product should have same WMO,
 *                                      MND, and segment times as original
 *                                      product.
 * Feb 05, 2016  5242         dgilling  Remove calls to deprecated Localization
 *                                      APIs.
 * Feb 10, 2016  5337         dgilling  Prevent CAN products past VTEC end time
 *                                      from being transmitted.
 * Feb 24, 2016  5411         randerso  Leave issue times in mixed case.
 * Mar 01, 2016  14775        ryu       Initialize product definition for
 *                                      product correction; modified saveFile()
 *                                      and getDir().
 * Jun 13, 2016  5685         dgilling  Use product definition to initialize
 *                                      word wrap value.
 * Jun 17, 2016  18940/18428  arickert  Do not unlock text on first line of
 *                                      Product Editor
 * Jun 28, 2016  5578         bsteffen  Prevent getTimeZones from calling python
 *                                      while synced with the UI thread.
 * Oct 25, 2016  19446        ryu       Replace call to updateExpireTime() in
 *                                      setPurgeTime() to fix CAVE freeze issue
 *                                      when a ValuesDialog is open.
 * Nov 02, 2016  5979         njensen   Cast to Number where applicable
 * Feb 24, 2017  19715        ryu       Fix issue of expiration time being
 *                                      changed on product correction.
 * Jan 23, 2018  7153         randerso  Changes to allow new GFE config file to
 *                                      be selected when perspective is
 *                                      re-opened.
 * Feb 02, 2018  7089         dgilling  Allow larger product purge times.
 * Feb 02, 2018  6702         randerso  Fixed printing products to paginate
 *                                      correctly
 * Feb 07, 2018  7073         randerso  Remove "brain" from productDefinition
 *                                      when reviving editor
 * Mar 24, 2020  8049         randerso  Remove READ_ONLY from Spinner controls
 *
 * </pre>
 *
 * @author lvenable
 *
 */
public class ProductEditorComp extends Composite
        implements INotificationObserver {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ProductEditorComp.class);

    /**
     * Parent composite.
     */
    private final Composite parent;

    /**
     * Toolbar used to mimic a menu bar.
     */
    private ToolBar toolbar;

    /**
     * Pop-up Menu
     */
    private Menu popupMenu;

    /**
     * File menu.
     */
    private Menu fileMenu;

    /**
     * Edit menu.
     */
    private Menu editMenu;

    /**
     * Options menu.
     */
    private Menu optionsMenu;

    /**
     * Call to Actions menu.
     */
    private Menu callToActionsMenu;

    /**
     * File toolbar item.
     */
    private ToolItem fileTI;

    /**
     * Edit toolbar item.
     */
    private ToolItem editTI;

    /**
     * Options toolbar item.
     */
    private ToolItem optionsTI;

    /**
     * Call to Actions toolbar item.
     */
    private ToolItem callToActionsTI;

    /**
     * Styled text editor.
     */
    private StyledText textEditorST;

    private StyledTextComp textComp;

    /**
     * Disabled transmission image.
     */
    private Image transDisabledImg;

    /**
     * Live transmission image.
     */
    private Image transLiveImg;

    /**
     * Check image.
     */
    private Image checkImg;

    /**
     * Transmit menu item.
     */
    private MenuItem transmitMI;

    /**
     * Transmit button.
     */
    private Button transmitBtn;

    /**
     * Product type combo button.
     */
    private Combo prodTypeCbo;

    /**
     * Date & time label.
     */
    private Label dateTimeLbl;

    /**
     * Hours spinner.
     */
    private Spinner hoursSpnr;

    /**
     * Date & time formatter.
     */
    private final SimpleDateFormat expireLabelFmt = new SimpleDateFormat(
            "HH:mm'Z' dd-MMM-yy");

    /**
     * Expiration date.
     */
    private Date expireDate;

    private ImageRegistry registry;

    private ProductDefinition productDefinition;

    private final String productName;

    private final boolean editorCorrectionMode;

    private boolean testVTEC;

    private String wmoId;

    private String pil;

    private String productId;

    private String textdbPil;

    private String fullStationId;

    private String autoSendAddress;

    private static final String longLocalFmtStr = "hmm a z EEE MMM d yyyy";

    private static final TimeZone gmtTimeZone = TimeZone.getTimeZone("GMT");

    private final int initialYear = year(
            SimulatedTime.getSystemTime().getTime());

    private boolean HazardCTA = false;

    private CallToActionsDlg ctaDialog;

    private ProductDataStruct prodDataStruct;

    private TimeZone localTimeZone;

    private enum Action {
        STORE, TRANSMIT, AUTOSTORE
    }

    /**
     * Product transmission callback to report the state of transmitting a
     * product.
     */
    private final ITransmissionState transmissionCB;

    private final SimpleDateFormat purgeTimeFmt = new SimpleDateFormat(
            "ddHHmm");

    private final SimpleDateFormat vtecTimeFmt = new SimpleDateFormat(
            "yyMMdd'T'HHmm'Z'");

    private final Pattern vtecRE = Pattern
            .compile("/[OTEX]\\.([A-Z]{3})\\.([A-Z]{4})\\.([A-Z]{2})\\."
                    + "([WAYSOFN])\\.([0-9]{4})\\.([0-9]{6})T([0-9]{4})Z-"
                    + "([0-9]{6})T([0-9]{4})Z/");

    private final Map<String, Map<String, Integer>> newYearETNs = new HashMap<>();

    /**
     * Job to update times in product
     */
    private ChangeTimesJob timeUpdater;

    private final Listener visibilityListener;

    /**
     * Enumeration of product types.
     *
     * @author lvenable
     */
    @SuppressWarnings("javadoc")
    public enum productTypeEnum {
        rou, res, AAA, AAB, AAC, AAD, AAE, RRA, RRB, RRC, CCA, CCB, CCC;
    }

    protected enum PTypeCategory {
        REG, PE, COR;
    }

    private static final String[] REG_PTypes = { "rou", "AAA", "AAB", "AAC",
            "AAD", "AAE", "RRA", "RRB", "RRC" };

    private static final String[] PE_PTypes = { "res", "RRA", "RRB", "RRC" };

    private static final String[] COR_PTypes = { "CCA", "CCB", "CCC" };

    private static final String ZERO_VTEC = "000000T0000Z";

    private List<ActiveTableRecord> activeVtecRecords = null;

    /**
     * Selected product type.
     */
    private productTypeEnum selectedType;

    private boolean dead;

    // buttons to be disabled when brained
    private List<Button> buttons;

    // menu items to be disabled when brained
    private List<MenuItem> menuItems;

    private int wrapColumn;

    private boolean wrapMode;

    private String prdDir;

    private String prodEditorDirectory;

    /**
     * Zones don't change so save the time zone for each segment to save trips
     * to python when time is updated.
     */
    private final Map<List<String>, Collection<String>> zonesToTimeZones = new HashMap<>();

    private final DataManager dm;

    /**
     * Constructor
     *
     * @param parent
     *            Parent composite.
     * @param productDefinition
     * @param productName
     * @param editorCorrectionMode
     * @param transmissionCB
     * @param dataMgr
     */
    public ProductEditorComp(Composite parent,
            ProductDefinition productDefinition, String productName,
            boolean editorCorrectionMode, ITransmissionState transmissionCB,
            DataManager dataMgr) {
        super(parent, SWT.BORDER);

        this.parent = parent;
        if (productDefinition != null) {
            this.productDefinition = productDefinition;
        } else {
            this.productDefinition = new ProductDefinition();
        }
        this.productName = productName;
        this.editorCorrectionMode = editorCorrectionMode;
        this.transmissionCB = transmissionCB;
        this.dm = dataMgr;

        prdDir = GFEPreference.getString("GFESUITE_PRDDIR",
                System.getProperty("java.io.tmpdir"));

        prodEditorDirectory = GFEPreference.getString("ProductEditorDirectory",
                prdDir);

        if (!editorCorrectionMode) {
            wmoId = getDefString("wmoID");
            pil = getDefString("pil");
            productId = getDefString("awipsWANPil");
            textdbPil = getDefString("textdbPil");
            fullStationId = getDefString("fullStationID");
            autoSendAddress = getDefString("autoSendAddress");
            if (autoSendAddress == null) {
                autoSendAddress = "000";
            }
        }

        testVTEC = GFEPreference.getBoolean("TestVTECDecode");
        if (CAVEMode.getMode().equals(CAVEMode.PRACTICE)) {
            testVTEC = true;
        }

        init();

        visibilityListener = new Listener() {
            @Override
            public void handleEvent(Event e) {
                switch (e.type) {
                case SWT.Hide:
                    timeUpdater.cancel();
                    break;
                case SWT.Show:
                    if ((!dead) && ((getProductText() != null)
                            || !getProductText().isEmpty())) {
                        timeUpdater.schedule();
                    }
                    break;
                }
            }
        };
        getShell().addListener(SWT.Hide, visibilityListener);
        getShell().addListener(SWT.Show, visibilityListener);
        addListener(SWT.Hide, visibilityListener);
        addListener(SWT.Show, visibilityListener);

        addDisposeListener(new DisposeListener() {

            @Override
            public void widgetDisposed(DisposeEvent e) {
                timeUpdater.cancel();
                getShell().removeListener(SWT.Hide, visibilityListener);
                getShell().removeListener(SWT.Show, visibilityListener);
                removeListener(SWT.Hide, visibilityListener);
                removeListener(SWT.Show, visibilityListener);

                NotificationManagerJob.removeObserver("edex.alerts.vtec",
                        ProductEditorComp.this);
                if (registry != null) {
                    registry.dispose();
                }
            }

        });

        NotificationManagerJob.addObserver("edex.alerts.vtec", this);
    }

    /**
     * Initialize the composite.
     */
    private void init() {
        expireLabelFmt.setTimeZone(gmtTimeZone);
        purgeTimeFmt.setTimeZone(gmtTimeZone);
        vtecTimeFmt.setTimeZone(gmtTimeZone);
        localTimeZone = getLocalTimeZone();

        transDisabledImg = getImageRegistry().get("transmitDisabled");
        transLiveImg = getImageRegistry().get("transmitLive");
        checkImg = getImageRegistry().get("checkmark");
        menuItems = new ArrayList<>();

        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        GridLayout gl = new GridLayout(1, false);
        gl.marginHeight = 1;
        gl.marginWidth = 1;
        setLayout(gl);
        setLayoutData(gd);

        /*
         * NOTE: How we determine the wrapColumn value is different than A1. In
         * A1, the lineLength was only used by the product formatter code to
         * determine the initial line length of each line of the product. The
         * gfeConfig preference ProductOutputDialog_wrapSize determined the word
         * wrap value used by the Product Editor. These could be independent of
         * each other so that the editor wrapped using a different value than
         * was used to generate the initial product.
         */
        Object pdefWrapValue = productDefinition.get("lineLength");
        if (pdefWrapValue != null) {
            wrapColumn = ((Number) pdefWrapValue).intValue();
        } else {
            wrapColumn = GFEPreference.getInt("ProductOutputDialog_wrapSize",
                    66);
        }

        wrapMode = GFEPreference.getBoolean("ProductOutputDialog_wrapMode",
                true);

        if (this.pil != null) {
            String[] wrapPils = GFEPreference
                    .getStringArray("ProductOutputDialog_wrapPils");
            if (Arrays.asList(wrapPils).contains(pil.substring(0, 3))) {
                wrapMode = true;
            }

            String[] nonWrapPils = GFEPreference
                    .getStringArray("ProductOutputDialog_nonWrapPils");
            if (Arrays.asList(nonWrapPils).contains(pil.substring(0, 3))) {
                wrapMode = false;
            }
        }

        initializeComponents();

        this.pack();

        // initialize "type" combo box
        setPTypeCategory(PTypeCategory.REG);
    }

    /**
     * Initialize the controls on the composite.
     */
    private void initializeComponents() {
        createToolbar();

        createTextControl();

        createBottomControls();

        setLiveTransmission();
    }

    /**
     * Create the toolbar.
     */
    private void createToolbar() {
        toolbar = new ToolBar(this, SWT.NONE);

        fileMenu = new Menu(parent.getShell(), SWT.POP_UP);
        createFileMenu(fileMenu);
        editMenu = new Menu(parent.getShell(), SWT.POP_UP);
        createEditMenu(editMenu);
        optionsMenu = new Menu(parent.getShell(), SWT.POP_UP);
        createOptionsMenu(optionsMenu);
        callToActionsMenu = new Menu(parent.getShell(), SWT.POP_UP);
        createCallToActionsMenu(callToActionsMenu);

        fileTI = new ToolItem(toolbar, SWT.DROP_DOWN);
        fileTI.setText("File");
        fileTI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                Rectangle rect = fileTI.getBounds();
                Point pt = new Point(rect.x, rect.y + rect.height);
                pt = toolbar.toDisplay(pt);
                fileMenu.setLocation(pt.x, pt.y);
                fileMenu.setVisible(true);
            }
        });

        editTI = new ToolItem(toolbar, SWT.DROP_DOWN);
        editTI.setText("Edit");
        editTI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                Rectangle rect = editTI.getBounds();
                Point pt = new Point(rect.x, rect.y + rect.height);
                pt = toolbar.toDisplay(pt);
                editMenu.setLocation(pt.x, pt.y);
                editMenu.setVisible(true);
            }
        });

        optionsTI = new ToolItem(toolbar, SWT.DROP_DOWN);
        optionsTI.setText("Options");
        optionsTI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                Rectangle rect = optionsTI.getBounds();
                Point pt = new Point(rect.x, rect.y + rect.height);
                pt = toolbar.toDisplay(pt);
                optionsMenu.setLocation(pt.x, pt.y);
                optionsMenu.setVisible(true);
            }
        });

        callToActionsTI = new ToolItem(toolbar, SWT.DROP_DOWN);
        callToActionsTI.setText("CallToActions");
        callToActionsTI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                Rectangle rect = callToActionsTI.getBounds();
                Point pt = new Point(rect.x, rect.y + rect.height);
                pt = toolbar.toDisplay(pt);
                callToActionsMenu.setLocation(pt.x, pt.y);
                callToActionsMenu.setVisible(true);
            }
        });
    }

    /**
     * Create the file menu.
     */
    private void createFileMenu(Menu menuToAddTo) {

        MenuItem saveFileMI = new MenuItem(menuToAddTo, SWT.PUSH);
        saveFileMI.setText("Save File...");
        saveFileMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                saveFile();
            }
        });

        MenuItem storeMI = new MenuItem(menuToAddTo, SWT.PUSH);
        storeMI.setText("Store...");
        storeMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                storeTransmit(Action.STORE);
            }
        });
        menuItems.add(storeMI);

        // we can't color the background of the menu item so
        // we use an image like the tab folder.
        transmitMI = new MenuItem(menuToAddTo, SWT.PUSH);
        transmitMI.setText("Transmit...");
        transmitMI.setImage(transLiveImg);
        transmitMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                storeTransmit(Action.TRANSMIT);
            }
        });
        menuItems.add(transmitMI);

        // Menu Separator
        new MenuItem(menuToAddTo, SWT.SEPARATOR);

        MenuItem printMI = new MenuItem(menuToAddTo, SWT.PUSH);
        printMI.setText("Print");
        printMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                PrintDialog print = new PrintDialog(getShell(), SWT.NONE);
                print.setText("Print");
                PrinterData data = print.open();
                if (!(data == null)) {
                    printProductText(data);
                }

            }
        });

        // Menu Separator
        new MenuItem(menuToAddTo, SWT.SEPARATOR);

        if (editorCorrectionMode) {
            MenuItem loadDraftMI = new MenuItem(menuToAddTo, SWT.PUSH);
            loadDraftMI.setText("Open File...");
            loadDraftMI.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    openFile();
                }
            });

            MenuItem saveDraftMI = new MenuItem(menuToAddTo, SWT.PUSH);
            saveDraftMI.setText("Load Product / Make Correction...");
            saveDraftMI.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    loadPrevious();
                }
            });
        } else {
            MenuItem loadDraftMI = new MenuItem(menuToAddTo, SWT.PUSH);
            loadDraftMI.setText("Load Draft");
            loadDraftMI.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    loadDraft();
                }
            });
            menuItems.add(loadDraftMI);

            MenuItem saveDraftMI = new MenuItem(menuToAddTo, SWT.PUSH);
            saveDraftMI.setText("Save Draft");
            saveDraftMI.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    saveDraft();
                }
            });
            menuItems.add(saveDraftMI);
        }
    }

    /**
     * Create the edit menu.
     */
    private void createEditMenu(Menu menuToAddTo) {

        MenuItem undoMI = new MenuItem(menuToAddTo, SWT.PUSH);
        undoMI.setText("Undo");
        undoMI.setEnabled(false);

        MenuItem redoMI = new MenuItem(menuToAddTo, SWT.PUSH);
        redoMI.setText("Redo");
        redoMI.setEnabled(false);

        // Menu Separator
        new MenuItem(menuToAddTo, SWT.SEPARATOR);

        MenuItem cutMI = new MenuItem(menuToAddTo, SWT.PUSH);
        cutMI.setText("Cut");
        cutMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                textComp.getTextEditorST().cut();
            }
        });

        MenuItem copyMI = new MenuItem(menuToAddTo, SWT.PUSH);
        copyMI.setText("Copy");
        copyMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                textComp.getTextEditorST().copy();
            }
        });

        MenuItem pasteMI = new MenuItem(menuToAddTo, SWT.PUSH);
        pasteMI.setText("Paste");
        pasteMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                textComp.getTextEditorST().paste();
            }
        });

        // Menu Separator
        new MenuItem(menuToAddTo, SWT.SEPARATOR);

        MenuItem findMI = new MenuItem(menuToAddTo, SWT.PUSH);
        findMI.setText("Find...");
        findMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                displayFindReplaceDialog(false);
            }
        });

        MenuItem replaceMI = new MenuItem(menuToAddTo, SWT.PUSH);
        replaceMI.setText("Replace...");
        replaceMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                displayFindReplaceDialog(true);
            }
        });

        MenuItem spellCheckMI = new MenuItem(menuToAddTo, SWT.PUSH);
        spellCheckMI.setText("Spell Check...");
        spellCheckMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                SpellCheckDlg spellCheckDlg = new SpellCheckDlg(
                        parent.getShell(), getTextEditorST(),
                        MixedCaseProductSupport.isMixedCase(getNNNid()));
                spellCheckDlg.open();
            }
        });

        // Menu Separator
        new MenuItem(menuToAddTo, SWT.SEPARATOR);

        MenuItem wrapSelectedMI = new MenuItem(menuToAddTo, SWT.PUSH);
        wrapSelectedMI.setText("Wrap Selected");
        wrapSelectedMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                doWrapSelection();
            }
        });
    }

    /**
     * Create the options menu.
     */
    private void createOptionsMenu(Menu menuToAddTo) {
        MenuItem autoWrapMI = new MenuItem(menuToAddTo, SWT.CHECK);
        autoWrapMI.setText("Auto Wrap");
        autoWrapMI.setSelection(wrapMode);
        autoWrapMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                wrapMode = !wrapMode;
                textComp.setAutoWrapMode(wrapMode);
            }
        });

        MenuItem wrapLengthMI = new MenuItem(menuToAddTo, SWT.PUSH);
        wrapLengthMI.setText("Wrap Length...");
        wrapLengthMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                // The dialog being opened is modal to the parent dialog. This
                // will prevent the launching of another dialog until the modal
                // dialog is closed.
                final WrapLengthDialog wrapLengthDialog = new WrapLengthDialog(
                        getShell());
                wrapLengthDialog.setWrapLength(wrapColumn);
                wrapLengthDialog.setBlockOnOpen(false);
                wrapLengthDialog.addCloseCallback(new ICloseCallback() {

                    @Override
                    public void dialogClosed(Object returnValue) {
                        if (returnValue instanceof Integer) {
                            int result = (Integer) returnValue;
                            if (result == Window.OK) {
                                wrapColumn = wrapLengthDialog.getWrapLength();
                                textComp.setWrapColumn(wrapColumn);
                            }
                        }
                    }
                });
                wrapLengthDialog.open();
            }
        });
    }

    /**
     * Create the call to actions menu.
     */
    private void createCallToActionsMenu(Menu menuToAddTo) {
        MenuItem hazardMI = new MenuItem(menuToAddTo, SWT.PUSH);
        hazardMI.setText("Hazard...");
        hazardMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                displayCallToActionsDialog(1);
            }
        });

        MenuItem productMI = new MenuItem(menuToAddTo, SWT.PUSH);
        productMI.setText("Product...");
        productMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                displayCallToActionsDialog(2);
            }
        });

        MenuItem genericMI = new MenuItem(menuToAddTo, SWT.PUSH);
        genericMI.setText("Generic...");
        genericMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                displayCallToActionsDialog(3);
            }
        });
    }

    /**
     * Create the editor text control.
     */
    private void createTextControl() {

        textComp = new StyledTextComp(this, wrapColumn, wrapMode);

        createEditorPopupMenu();

        textComp.getTextEditorST().addMouseListener(new MouseAdapter() {
            @Override
            public void mouseDown(MouseEvent e) {
                if (e.button == 3) {
                    popupMenu.setVisible(true);
                }
            }
        });
    }

    /**
     * Create the controls at the bottom of the composite.
     */
    private void createBottomControls() {
        Composite bottomComp = new Composite(this, SWT.NONE);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        bottomComp.setLayout(new GridLayout(11, false));
        bottomComp.setLayoutData(gd);

        buttons = new ArrayList<>();

        if (!editorCorrectionMode) {
            gd = new GridData(110, SWT.DEFAULT);
            Button saveDraftBtn = new Button(bottomComp, SWT.PUSH);
            saveDraftBtn.setText("Save Draft");
            saveDraftBtn.setLayoutData(gd);
            saveDraftBtn.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    saveDraft();
                }
            });
            buttons.add(saveDraftBtn);
        }

        transmitBtn = new Button(bottomComp, SWT.PUSH);
        transmitBtn.setText("Transmit...");
        transmitBtn.setImage(transLiveImg);
        transmitBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                storeTransmit(Action.TRANSMIT);
            }
        });
        buttons.add(transmitBtn);

        Button checkBtn = new Button(bottomComp, SWT.PUSH);
        checkBtn.setImage(checkImg);
        checkBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                checkText();
            }
        });

        Label sepLbl = new Label(bottomComp, SWT.SEPARATOR | SWT.VERTICAL);
        gd = new GridData(SWT.DEFAULT, SWT.FILL, false, true);
        gd.heightHint = 1;
        sepLbl.setLayoutData(gd);

        Label typeLbl = new Label(bottomComp, SWT.NONE);
        typeLbl.setText("Type:");

        prodTypeCbo = new Combo(bottomComp, SWT.DROP_DOWN | SWT.READ_ONLY);
        prodTypeCbo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                typeSelected(prodTypeCbo.getText());
            }
        });

        Label sepLbl2 = new Label(bottomComp, SWT.SEPARATOR | SWT.VERTICAL);
        gd = new GridData(SWT.DEFAULT, SWT.FILL, false, true);
        gd.heightHint = 1;
        sepLbl2.setLayoutData(gd);

        Label prodExpiresLbl = new Label(bottomComp, SWT.NONE);
        prodExpiresLbl.setText("Product expires in:");

        hoursSpnr = new Spinner(bottomComp, SWT.BORDER);
        hoursSpnr.setValues(TimeUtil.HOURS_PER_HALF_DAY * 100, 100,
                TimeUtil.HOURS_PER_WEEK * 100, 2, 25, 25);
        hoursSpnr.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                updateExpireTimeFromTimer();
            }
        });

        Label atLbl = new Label(bottomComp, SWT.NONE);
        atLbl.setText(" At:");

        // TODO: should expireDate just be new Date() or
        // SimulatedTime.getSystemTime().getTime()
        Calendar origExpireCal = Calendar.getInstance();
        expireDate = origExpireCal.getTime();

        dateTimeLbl = new Label(bottomComp, SWT.NONE);
        dateTimeLbl.setText(expireLabelFmt.format(expireDate));

        timeUpdater = new ChangeTimesJob("update product times");
    }

    private void updateExpireTimeFromTimer() {
        /*
         * Calling into python on the UI thread can result in a deadlock if the
         * python thread is using the UI thread to open a dialog. To get around
         * this get(and cache) the time zones on this thread amd then actually
         * update the times on the UIThread which will use the cached time
         * zones.
         */
        ProductDataStruct pds = textComp.getProductDataStruct();
        if (pds == null) {
            return;
        }
        String officeTimeZone = dm.getParmManager().compositeGridLocation()
                .getTimeZone();
        for (SegmentData segment : pds.getSegmentsArray()) {
            if (segment.getSementMap().containsKey("nwstime")) {
                getTimeZones(decodeUGCs(segment), officeTimeZone);
            }
        }

        VizApp.runSync(new Runnable() {
            @Override
            public void run() {
                updateExpireTime();
            }
        });
    }

    /**
     * Set the transmission controls to reflect live or disabled transmission.
     */
    public void setLiveTransmission() {
        CAVEMode mode = CAVEMode.getMode();
        if (mode.equals(CAVEMode.OPERATIONAL)) {
            transmitBtn.setImage(transLiveImg);
            transmitMI.setImage(transLiveImg);
        } else {
            transmitBtn.setImage(transDisabledImg);
            transmitMI.setImage(transDisabledImg);
        }
    }

    /**
     * Store or Transmit text product.
     *
     * @param action
     *            STORE: show the Store dialog TRANSMITT: shows the Transmit
     *            dialog. AUTOSTORE: implement autoStore
     */
    private void storeTransmit(Action action) {
        if (!SimulatedTimeOperations.isTransmitAllowed()) {
            SimulatedTimeOperations.displayFeatureLevelWarning(getShell(),
                    "Text Product Store/Transmit");
            brain();
            return;
        }

        ProductDataStruct pds = textComp.getProductDataStruct();
        if (pds == null) {
            String msg = "There is no product to transmit.\n\nAction cancelled.";
            MessageBox mb = new MessageBox(getShell(),
                    SWT.OK | SWT.ICON_WARNING);
            mb.setText("Error");
            mb.setMessage(msg);
            mb.open();
            return;
        }

        if (pds.getCiMap() == null) {
            String msg = "The current product does not have a ci block.\n\nAction cancelled.";
            MessageBox mb = new MessageBox(getShell(),
                    SWT.OK | SWT.ICON_WARNING);
            mb.setText("Error");
            mb.setMessage(msg);
            mb.open();
            return;
        }

        if (fixText()) {

            // autoStore
            if (action == Action.AUTOSTORE) {
                if (testVTEC) {
                    devStore(textdbPil.substring(3));
                } else {
                    TextDBUtil.storeProduct(textdbPil, getProductText(),
                            testVTEC);
                }

                return;
            }

            // Store/transmit...
            boolean showStore = (action == Action.STORE) ? true : false;
            String pid;
            if (showStore) {
                pid = guessTDBPil();
            } else {
                if (productId != null) {
                    pid = productId;
                } else {
                    pid = "kkkknnnxxx";
                }
            }

            // The dialog being opened is modal to the parent dialog. This will
            // prevent the launching of another dialog until the modal dialog is
            // closed.
            StoreTransmitDlg storeDlg = new StoreTransmitDlg(parent.getShell(),
                    showStore, this, transmissionCB, pid,
                    !textComp.isCorMode() && (action == Action.TRANSMIT));
            storeDlg.open();
        }
    }

    private boolean fixText() {
        textComp.startUpdate();
        // mmaron #7285: make sure there is \n in the end - to allow
        // Transmit
        if (!textComp.getTextEditorST().getText().endsWith("\n")) {
            textComp.getTextEditorST()
                    .setText(textComp.getTextEditorST().getText() + "\n");
        }
        if (!MixedCaseProductSupport.isMixedCase(getNNNid())) {
            textComp.upper();
        }
        textComp.endUpdate();

        if (!frameCheck(false)) {
            return false;
        }

        boolean retVal = true;
        if (!textComp.isCorMode()) {
            retVal = changeTimes();
        }

        textComp.updatePType(selectedType.name());
        return retVal;
    }

    private boolean frameCheck(boolean silent) {
        String text = textComp.getProductText();
        if ((text.indexOf("|*") > -1) || (text.indexOf("*|") > -1)) {
            if (!silent) {
                String msg = "The product still has 1 or more framing codes or contains a partial "
                        + "framing code tag.\n\nAction cancelled.";
                MessageBox mb = new MessageBox(getShell(),
                        SWT.OK | SWT.ICON_WARNING);
                mb.setText("Framing Codes");
                mb.setMessage(msg);
                mb.open();
            } else {
                statusHandler.handle(Priority.PROBLEM,
                        "Product still has framing codes.");
            }
            return false;
        }
        return true;
    }

    private boolean changeTimes() {
        if (selectedType == productTypeEnum.res) {
            return false;
        }
        Calendar GMT = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        GMT.setTime(SimulatedTime.getSystemTime().getTime());
        GMT.set(Calendar.SECOND, 0);
        Date tt = GMT.getTime();

        tweakVTEC(tt);
        updateIssueExpireTimes(tt);
        return true;
    }

    /**
     * get the zones contained in this segment Decode the UGC header line and
     * return the UGCs
     */
    private List<String> decodeUGCs(SegmentData segData) {
        Map<String, TextIndexPoints> segMap = segData.getSementMap();
        TextIndexPoints tipUgc = segMap.get("uhdr");
        String uhdr = tipUgc.getText();
        uhdr = uhdr.replaceAll("\n", "");
        String[] uhdrTokens = uhdr.split("-");
        int validTokens = uhdrTokens.length - 1;
        String[] UGCs = new String[validTokens];
        System.arraycopy(uhdrTokens, 0, UGCs, 0, validTokens);

        List<String> zones = new ArrayList<>();
        String alpha = "";
        for (String ugc : UGCs) {
            int index = ugc.indexOf('>');
            if (index > -1) {
                String first = ugc.substring(0, index);
                String last = ugc.substring(index + 1);
                if (first.length() == 6) {
                    alpha = first.substring(0, 3);
                    first = first.substring(3);
                }
                int firstInt = Integer.parseInt(first);
                int lastInt = Integer.parseInt(last);
                for (int num = firstInt; num <= lastInt; num++) {
                    zones.add(alpha + String.format("%03d", num));
                }
            } else {
                if (ugc.length() == 6) {
                    alpha = ugc.substring(0, 3);
                    zones.add(ugc);
                } else {
                    zones.add(alpha + ugc);
                }
            }
        }

        return zones;
    }

    /**
     * get the list of VTEC lines for this segment
     */
    private List<String> getVTEClines(SegmentData segData) {
        prodDataStruct = textComp.getProductDataStruct();
        Map<String, TextIndexPoints> segMap = segData.getSementMap();
        TextIndexPoints tipVtec = segMap.get("vtec");
        int lineCount = tipVtec.getEndIndex().x - tipVtec.getStartIndex().x;
        List<String> vtecList = new ArrayList<>(lineCount);
        for (int i = 0; i < lineCount; i++) {
            String vtec = prodDataStruct.getProductTextArray()[i
                    + tipVtec.getStartIndex().x];
            vtecList.add(vtec);
        }
        return vtecList;
    }

    /**
     * Retrieve the latest active VTEC for our site from server. Filter and keep
     * only the ones for this pil.
     */
    public void retrieveActiveVTEC() {
        GetActiveTableRequest req = new GetActiveTableRequest();
        req.setSiteID(fullStationId);
        CAVEMode mode = CAVEMode.getMode();
        if (mode.equals(CAVEMode.PRACTICE)) {
            req.setMode(ActiveTableMode.PRACTICE);
        } else {
            req.setMode(ActiveTableMode.OPERATIONAL);
        }

        List<ActiveTableRecord> records = null;
        try {
            GetActiveTableResponse resp = (GetActiveTableResponse) ThriftClient
                    .sendRequest(req);
            records = resp.getActiveTable();
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error sending active table request to http server ", e);
        }

        if ((records == null) || records.isEmpty()) {
            activeVtecRecords = null;
        } else {
            if (pil != null) {
                String shortPil = pil.substring(0, 3);
                activeVtecRecords = new ArrayList<>();
                for (ActiveTableRecord r : records) {
                    if (r.getPil().equalsIgnoreCase(shortPil)) {
                        activeVtecRecords.add(r);
                    }
                }
            } else {
                activeVtecRecords = records;
            }
        }
    }

    /**
     * get the active VTEC with matching attributes
     */
    private List<ActiveTableRecord> getMatchingActiveVTEC(List<String> zones,
            String officeId, String phen, String sig, String etn) {

        List<ActiveTableRecord> matches = new ArrayList<>();
        if (activeVtecRecords == null) {
            return null;
        }
        for (ActiveTableRecord r : activeVtecRecords) {
            if (r.getPhen().equalsIgnoreCase(phen)
                    && r.getSig().equalsIgnoreCase(sig)
                    && r.getOfficeid().equalsIgnoreCase(officeId)
                    && r.getEtn().equalsIgnoreCase(etn)) {
                for (String zone : zones) {
                    if (r.getUgcZone().equalsIgnoreCase(zone)) {
                        matches.add(r);
                        break;
                    }
                }
            }
        }
        return matches;
    }

    private String fixVTEC(List<String> zones, List<String> vtecs,
            Date transmissionTime) throws VizException {

        List<String> rval = new ArrayList<>();
        DecimalFormat formatter = new DecimalFormat("0000");
        for (String vtec : vtecs) {
            if (vtec.indexOf('-') < 0) {
                // ignore HVTEC
                rval.add(vtec);
                continue;
            }

            String vline = vtec.split("/", 3)[1];
            String[] vtecTokens = vline.split("\\.");
            String action = vtecTokens[1];
            int timesTokenIdx = vtecTokens.length - 1;
            String timesToken = vtecTokens[timesTokenIdx];
            String vtecStartStr = timesToken.substring(0, 12);
            String vtecEndStr = timesToken.substring(13);
            Date vtecStart = null;
            Date vtecEnd = null;
            try {
                vtecStart = decodeVTECTime(vtecStartStr);
                vtecEnd = decodeVTECTime(vtecEndStr);
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "error parsing vtec time range " + timesToken, e);
            }

            /*
             * Correct the starting time to not start before now NEW, EXB
             * actions: ensure start time not in past UPG, EXP, CAN actions:
             * when tt>=st: switch to zeros (active event) CON actions: when
             * tt>=st: switch to zeros (active event) EXT action: ensure start
             * time not in past if event hasn't gone into effect; otherwise
             * switch start time to zeros or warn user to reformat product
             */
            String oid, phen, sig, etn;
            List<ActiveTableRecord> activeRecs;
            if (vtecStart != null) {
                if (("NEW".equals(action) || "EXB".equals(action))
                        && vtecStart.before(transmissionTime)) {
                    vtecStartStr = vtecTimeFmt.format(transmissionTime);

                    // set to transmission time
                    vtecStart = transmissionTime;
                } else if (("UPG".equals(action) || "EXP".equals(action)
                        || "CAN".equals(action) || "CON".equals(action))
                        && (transmissionTime.getTime() >= vtecStart
                                .getTime())) {
                    vtecStartStr = ZERO_VTEC;
                    vtecStart = null;
                } else if ("EXT".equals(action)) {
                    oid = vtecTokens[2];
                    phen = vtecTokens[3];
                    sig = vtecTokens[4];
                    etn = vtecTokens[5];
                    activeRecs = getMatchingActiveVTEC(zones, oid, phen, sig,
                            etn);
                    String eventStr = "." + phen + "." + sig + "." + etn;
                    if ((activeRecs == null) || activeRecs.isEmpty()) {
                        statusHandler.handle(Priority.PROBLEM,
                                "No active records found for " + vtec);
                    } else {
                        // check if the segment is still valid
                        int started = 0;
                        for (ActiveTableRecord atr : activeRecs) {
                            if (atr.getStartTime().getTime() <= transmissionTime
                                    .getTime()) {
                                started++;
                            }
                        }

                        // segment invalid due to the event going into
                        // effect in part of the segment area
                        if ((started > 0) && (started < activeRecs.size())) {
                            final String msg = "Event " + eventStr
                                    + " has gone into effect in part"
                                    + " of its current segment's geographical area."
                                    + " The product must be regenerated.";
                            throw new VizException(msg);
                        } else if (started == activeRecs.size()) {
                            // all active VTEC started
                            // if the current start time is in the future,
                            // product needs to be regenerated
                            if (vtecStart.after(transmissionTime)) {
                                final String msg = "Event " + eventStr
                                        + " has gone into effect but the product "
                                        + "has it starting in the future. "
                                        + "The product must be regenerated.";
                                throw new VizException(msg);
                            }
                            // check if actions are the same
                            List<String> newActions = new ArrayList<>();
                            for (ActiveTableRecord r : activeRecs) {
                                // need to change the action to CON if
                                // the end time did not change
                                if (r.getEndTime().getTime() == vtecEnd
                                        .getTime()) {
                                    if (!newActions.contains("CON")) {
                                        newActions.add("CON");
                                    }
                                } else {
                                    if (!newActions.contains("EXT")) {
                                        newActions.add("EXT");
                                    }
                                }
                            }

                            // invalid segmentation
                            if (newActions.size() > 1) {
                                final String msg = "Event " + eventStr
                                        + " has gone into effect. The"
                                        + " current segment is invalid"
                                        + " since both EXT and CON "
                                        + "actions are required. Product "
                                        + "must be regenerated.";
                                throw new VizException(msg);
                            }

                            // len(newActions) == 1
                            // set the action
                            vtecTokens[1] = newActions.get(0);

                            // zero the start time.
                            vtecStartStr = ZERO_VTEC;
                            vtecStart = null;
                        } else if (started == 0) {
                            // VTEC has not gone into effect
                            // push back start time - set to transmission time
                            if (transmissionTime.after(vtecStart)) {
                                vtecStartStr = vtecTimeFmt
                                        .format(transmissionTime);
                                vtecStart = transmissionTime;
                            }
                        }
                    }
                }
            }
            // force ETN to 1 if this is a new year (diff year from orig tt)
            // NEWs may need to have new ETN assigned if transtime next
            // year and starting time in the next year.
            if ("NEW".equals(action) && (year(transmissionTime) != initialYear)
                    && (year(vtecStart) > initialYear)) {
                String phensig = vtecTokens[3] + vtecTokens[4];
                Map<String, Integer> times = newYearETNs.get(phensig);
                if (times == null) {
                    times = new HashMap<>();
                }
                boolean match = false;
                final TimeRange tr1 = new TimeRange(vtecStart, vtecEnd);
                for (Entry<String, Integer> entry : times.entrySet()) {
                    final TimeRange tr2 = new TimeRange(entry.getKey());
                    if (tr1.overlaps(tr2)) {
                        vtecTokens[5] = formatter
                                .format(entry.getValue().longValue());
                        match = true;
                        break;
                    }
                }
                if (!match) {
                    // find max assigned for NEWs for phensig
                    int maxETN = 0;
                    for (Integer t : times.values()) {
                        maxETN = Math.max(maxETN, t);
                    }
                    // assign the next one
                    Integer nextETN = new Integer(maxETN + 1);
                    times.put(tr1.toString(), nextETN);
                    vtecTokens[5] = formatter.format(nextETN.longValue());
                    newYearETNs.put(phensig, times);
                }
            }

            // Check start and ending time for end later than start
            if ((vtecStart != null) && (vtecEnd != null)
                    && (vtecStart.getTime() >= vtecEnd.getTime())) {
                setTabColorFunc(ProductStateEnum.New);
                String msg = "VTEC ending time is before "
                        + "starting time. Product is invalid and must"
                        + " be regenerated.";
                throw new VizException(msg);
            }

            /*
             * Give 30 minutes of slack for EXP action code, check the ending
             * time and transmission time
             */
            if (("EXP".equals(action)) && (vtecEnd != null)) {
                vtecEnd.setTime(
                        vtecEnd.getTime() + (30 * TimeUtil.MILLIS_PER_MINUTE));
            }

            if ((vtecEnd != null)
                    && (vtecEnd.getTime() <= transmissionTime.getTime())) {
                setTabColorFunc(ProductStateEnum.New);
                String msg = "VTEC ends before current time."
                        + " Product is invalid and must be regenerated.";
                throw new VizException(msg);
            }

            vtecTokens[timesTokenIdx] = vtecStartStr + "-" + vtecEndStr;
            vline = "/" + String.join(".", vtecTokens) + "/";
            rval.add(vline);
        }
        return String.join("\n", rval);
    }

    /**
     * Decodes the start and end times of VTEC, return null if all zeros.
     *
     * @param vt
     *            The VTEC date string in "yyMMdd'T'HHmm'Z'" format
     * @return Date object that corresponds to the specified VTEC time or null
     *         if the VTEC is all zeros.
     * @throws ParseException
     *             if the VTEC string does not match the VTEC "yyMMdd'T'HHmm'Z'"
     *             format.
     */
    private Date decodeVTECTime(String vt) throws ParseException {
        if (!vt.equals(ZERO_VTEC)) {
            return vtecTimeFmt.parse(vt);
        }

        return null;
    }

    /*
     * helper inner class to handle VTEC time ranges
     */
    class TimeRange {
        private Date startTime, endTime;

        private String st, et;

        TimeRange(Date startTime, Date endTime) {
            this.startTime = startTime;
            this.endTime = endTime;
            st = (startTime == null) ? ZERO_VTEC
                    : vtecTimeFmt.format(startTime);
            et = (endTime == null) ? ZERO_VTEC : vtecTimeFmt.format(endTime);
        }

        TimeRange(String tr) {
            try {
                String[] times = tr.split("-");
                st = times[0];
                et = times[1];
                startTime = vtecTimeFmt.parse(st);
                endTime = vtecTimeFmt.parse(et);
            } catch (ParseException | IndexOutOfBoundsException e) {
                statusHandler.error(
                        String.format("Error parsing time range: \"%s\"", tr),
                        e);
            }
        }

        // time overlaps, if tr1 overlaps tr2 (adjacent is not an overlap) def
        public boolean overlaps(TimeRange tr) {
            if (tr.contains(startTime) || this.contains(tr.getStartTime())) {
                return true;
            }
            return false;
        }

        // time contains, if time range (tr) contains time (t), return 1 def
        public boolean contains(Date t) {
            return (t.getTime() >= startTime.getTime()) && t.before(endTime);
        }

        public Date getStartTime() {
            return startTime;
        }

        public Date getEndTime() {
            return endTime;
        }

        @Override
        public String toString() {
            return st + "-" + et;
        }
    }

    private static int year(Date tt) {
        SimpleDateFormat simpleDateformat = new SimpleDateFormat("yyyy");
        if (tt == null) {
            tt = SimulatedTime.getSystemTime().getTime();
        }
        return Integer.valueOf(simpleDateformat.format(tt));
    }

    private void tweakVTEC(Date transmissionTime) {
        List<String> vtecs = null;
        String newVtecs = null;
        List<String> zones = null;

        textComp.startUpdate();

        prodDataStruct = textComp.getProductDataStruct();
        if (prodDataStruct == null) {
            textComp.endUpdate();
            return;
        }
        List<SegmentData> segs = prodDataStruct.getSegmentsArray();
        if ((segs == null) || (segs.isEmpty())) {
            textComp.endUpdate();
            return;
        }

        for (SegmentData segData : segs) {
            Map<String, TextIndexPoints> segMap = segData.getSementMap();
            TextIndexPoints tipVtec = segMap.get("vtec");
            if (tipVtec == null) {
                break;
            }

            try {
                zones = decodeUGCs(segData);
                vtecs = getVTEClines(segData);
                newVtecs = fixVTEC(zones, vtecs, transmissionTime);
                if ((newVtecs != null) && !newVtecs.isEmpty()) {
                    textComp.replaceText(tipVtec, newVtecs);
                }
            } catch (VizException e) {
                // disable transmission of product
                brain();
                statusHandler.error(e.getLocalizedMessage(), e);
            }
        }
        textComp.endUpdate();
    }

    private String guessTDBPil() {
        if (textdbPil != null) {
            return textdbPil;
        }

        return "cccnnnxxx";
    }

    private void parseIDs() {
        if (!editorCorrectionMode) {
            return;
        }

        ProductDataStruct pds = textComp.getProductDataStruct();
        wmoId = pds.getWmoId();
        fullStationId = pds.getFullStationId();
        pil = pds.getPil();
        if ((wmoId == null) || (fullStationId == null) || (pil == null)) {
            statusHandler.handle(Priority.SIGNIFICANT,
                    "Failed to parse wmoID, fullStationID, pil from loaded product");
            return;
        }
        productId = fullStationId + pil;
        textdbPil = productId.substring(1);
    }

    private void typeSelected(String val) {
        selectedType = productTypeEnum.valueOf(val);

        String txt;
        boolean updateTimes = true;
        if (val.charAt(0) == 'A') {
            txt = "UPDATED";

        } else if (val.charAt(0) == 'R') {
            txt = "DELAYED";
        } else if (val.charAt(0) == 'C') {
            txt = "CORRECTED";
        } else if ("res".equals(val)) {
            txt = "RESENT";
            updateTimes = false;
        } else if ("rou".equals(val)) {
            txt = "";
            updateTimes = false;
        } else {
            return;
        }

        if (updateTimes) {
            updateExpireTimeFromTimer();
        }

        textComp.startUpdate();
        textComp.patchMND(txt, true);
        textComp.updatePType(val);
        textComp.endUpdate();
    }

    protected void setPTypeCategory(PTypeCategory category) {
        String[] types;
        switch (category) {
        case PE:
            types = PE_PTypes;
            break;
        case COR:
            types = COR_PTypes;
            break;
        default:
            types = REG_PTypes;
        }
        prodTypeCbo.removeAll();
        for (String type : types) {
            prodTypeCbo.add(type);
        }

        if (selectedType != null) {
            for (int i = 0; i < types.length; i++) {
                if (types[i].equals(selectedType.name())) {
                    prodTypeCbo.select(i);
                    return;
                }
            }
        }

        prodTypeCbo.select(0);
        typeSelected(types[0]);
    }

    private void checkText() {
        if (dead) {
            setStatusText('R',
                    "Product already transmitted or stored. No checks made.");
            return;
        }

        boolean status1 = false;
        if (textComp.isCorMode()) {
            status1 = frameCheck(true);
            if (status1) {
                setStatusText('R', "Product Scanned. Times not updated");
                return;
            }
        }

        textComp.startUpdate();
        try {
            if (!MixedCaseProductSupport.isMixedCase(getNNNid())) {
                textComp.upper();
            }
            status1 = frameCheck(true);
            boolean status2 = changeTimes();
            if (status1 && status2) {
                setStatusText('R', "Product Scanned and Times Updated");
            }
        } finally {
            textComp.endUpdate();
        }
    }

    /**
     * @return
     */
    private TimeZone getLocalTimeZone() {
        TimeZone timeZone = TimeZone.getTimeZone(
                dm.getParmManager().compositeGridLocation().getTimeZone());
        return timeZone;
    }

    private void setPurgeTime() {

        Float offset = null;
        if (!editorCorrectionMode) {
            Object obj = productDefinition.get("purgeTime");
            if (obj != null) {
                offset = Float.valueOf(obj.toString());
            }
        }

        if (offset == null) {
            offset = extractPurgeTime();
        }

        if (offset == null) {
            // default
            offset = 12.F;
        }

        hoursSpnr.setSelection((int) (offset * 100));

        if (selectedType == productTypeEnum.res) {
            return;
        }

        // run updateExpireTime() in a separate thread
        timeUpdater.schedule();
    }

    private Float extractPurgeTime() {
        ProductDataStruct pds = textComp.getProductDataStruct();
        if (pds == null) {
            return null;
        }

        if (pds.getPIT() == null) {
            return null;
        }
        String pit = pds.getPIT().getText().trim();
        Date pitTime = timeFromDDHHMM(pit);
        if (pitTime == null) {
            return null;
        }

        List<String> purgeTimeStrs = new ArrayList<>();
        List<SegmentData> segArray = pds.getSegmentsArray();
        for (SegmentData sd : segArray) {
            Map<String, TextIndexPoints> segMap = sd.getSementMap();
            if (segMap.containsKey("purgeT")) {
                String pt = segMap.get("purgeT").getText();
                if (pt != null) {
                    purgeTimeStrs.add(pt);
                }
            }
        }

        if (purgeTimeStrs.isEmpty()) {
            return null;
        }

        // remove duplicates
        Set<String> timeStrings = new HashSet<>(purgeTimeStrs);

        // get largest value
        long maxPurgeTime = 0L;
        for (String timeStr : timeStrings) {
            Date purgeTime = timeFromDDHHMM(timeStr);
            if (purgeTime.getTime() > maxPurgeTime) {
                maxPurgeTime = purgeTime.getTime();
            }
        }

        // seconds
        long offset = (maxPurgeTime - pitTime.getTime())
                / TimeUtil.MILLIS_PER_SECOND;

        // Round up to nearest 15 minutes
        long extra = offset % 900;
        if (extra != 0L) {
            offset += 900L - extra;
        }

        // convert to hours and check bounds
        Float purgeOffset = offset / ((float) TimeUtil.SECONDS_PER_HOUR);
        purgeOffset = Math.min(purgeOffset, 24F);
        purgeOffset = Math.max(purgeOffset, 1F);

        return purgeOffset;
    }

    /**
     * Returns a Date from an encoded YYMMDD and hhmm string. Function name is a
     * misnomer, but kept from porting AWIPS1 equivalent function.
     *
     * @param day
     *            The "calendar day" of the time in Java's "yyMMdd" format.
     * @param time
     *            The "clock time" of the the time in Java's "HHmm" format.
     * @return A Date in GMT format based on the given day and time strings.
     * @throws ParseException
     *             If either of the given strings does not match the expected
     *             format this exception will be thrown.
     */
    private Date timeFromYYYYMMDD_HHMM(String day, String time)
            throws ParseException {
        if ("000000".equals(day) && "0000".equals(time)) {
            return new Date(0);
        }

        String timeString = day + time;
        SimpleDateFormat formatter = new SimpleDateFormat("yyMMddHHmm");
        formatter.setTimeZone(gmtTimeZone);
        return formatter.parse(timeString);
    }

    /**
     * Convert time string in DDHHMM format to a Date.
     *
     * @param dtgString
     *            time string in DDHHMM format
     * @return time converted from input string
     */
    private Date timeFromDDHHMM(String dtgString) {
        int day, hour, min;
        try {
            day = Integer.valueOf(dtgString.substring(0, 2));
            hour = Integer.valueOf(dtgString.substring(2, 4));
            min = Integer.valueOf(dtgString.substring(4, 6));
        } catch (Exception e) {
            String msg = "Time string \"" + dtgString
                    + "\" not in DDHHMM format.";
            statusHandler.error(msg, e);
            return null;
        }

        Calendar gmCal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        gmCal.setTime(SimulatedTime.getSystemTime().getTime());
        int curDay = gmCal.get(Calendar.DAY_OF_MONTH);
        gmCal.set(Calendar.DAY_OF_MONTH, day);
        if ((curDay - day) > 15) {
            gmCal.add(Calendar.MONTH, 1);
        } else if ((curDay - day) < -15) {
            gmCal.add(Calendar.MONTH, -1);
        }
        gmCal.set(Calendar.HOUR_OF_DAY, hour);
        gmCal.set(Calendar.MINUTE, min);
        gmCal.set(Calendar.SECOND, 0);

        return gmCal.getTime();
    }

    private void updateExpireTime() {
        if (textComp.isDisposed() || hoursSpnr.isDisposed()
                || dateTimeLbl.isDisposed()) {
            // we're shutting down just return
            return;
        }

        int sel = hoursSpnr.getSelection();
        int hours = sel / 100;
        int minuteInc = (sel % 100) / 25;

        // compute purgeOffset in minutes
        int purgeOffset = (hours * TimeUtil.MINUTES_PER_HOUR)
                + (minuteInc * 15);

        Date now = SimulatedTime.getSystemTime().getTime();
        Calendar cal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        cal.setTime(now);
        cal.add(Calendar.MINUTE, purgeOffset);
        int min = cal.get(Calendar.MINUTE);
        if ((min % 15) >= 1) {
            cal.set(Calendar.MINUTE, ((min / 15) + 1) * 15);
            cal.set(Calendar.SECOND, 0);
        }
        this.expireDate = cal.getTime();
        dateTimeLbl.setText(expireLabelFmt.format(expireDate));

        if (!dead) {
            changeTimes();
        }
    }

    /**
     * update issuance and expiration times in the text
     */
    private void updateIssueExpireTimes(Date now) {
        // Up front: an apology for this code you are about to read...
        // This code is going to be horribly slow 4 times during the day--both
        // transitions from 959 to 1000 and 1259 to 100 local time. To ensure
        // that the product doesn't get mangled when replacing the "nwstime"
        // strings I have added startUpdate() and endUpdate() calls at any point
        // I know this time string has been replaced. If the lengths of the
        // before and after strings are different, a reParse() will be made,
        // else it will continue on.

        if (textComp != null) {
            try {
                textComp.startUpdate();
                ProductDataStruct pds = textComp.getProductDataStruct();
                if (pds != null) {
                    // update WMO time
                    // if (!textComp.isCorMode()) { ## uncomment this if want to
                    // keep WMO time original
                    TextIndexPoints pit = pds.getPIT();
                    if (pit != null) {
                        String time = purgeTimeFmt.format(now);
                        textComp.replaceText(pit, time);
                    }
                    // }

                    // Update MND time
                    TextIndexPoints tip = pds.getMndMap().get("nwstime");
                    if (tip != null) {
                        SimpleDateFormat fmt = new SimpleDateFormat(
                                longLocalFmtStr);
                        fmt.setTimeZone(localTimeZone);
                        String issueTime = fmt.format(now);
                        textComp.replaceText(tip, issueTime);
                    }
                }
            } finally {
                textComp.endUpdate();
            }

            // The working assumption here is that any time replacement we will
            // perform will not alter the number of segments in the products. So
            // the number of iterations is predetermined, but we will force
            // StyledTextComp to re-evaluate whether a reParse() is needed and
            // ask it for the segment information each time through the loop (in
            // case we're at one of the 4 transition points).

            // Update segments' time
            try {
                ProductDataStruct pds = textComp.getProductDataStruct();

                if (pds != null) {
                    String officeTimeZone = dm.getParmManager()
                            .compositeGridLocation().getTimeZone();
                    int numSegments = pds.getSegmentsArray().size();
                    SimpleDateFormat fmt = new SimpleDateFormat(
                            longLocalFmtStr);
                    fmt.setTimeZone(localTimeZone);
                    String officeIssueTime = fmt.format(now);

                    for (int i = 0; i < numSegments; i++) {
                        textComp.startUpdate();
                        Map<String, TextIndexPoints> segMap = pds
                                .getSegmentsArray().get(i).getSementMap();

                        TextIndexPoints tip = null;
                        // don't change expiration time when product is
                        // corrected
                        if (!editorCorrectionMode) {
                            tip = segMap.get("purgeT");
                            if (tip != null) {
                                TextIndexPoints vtecTip = segMap.get("vtec");
                                String vtecStr = null;
                                if (vtecTip != null) {
                                    vtecStr = vtecTip.getText();
                                }
                                Date newExpireTime = getExpireTime(now,
                                        expireDate, vtecStr);
                                String expireTimeStr = purgeTimeFmt
                                        .format(newExpireTime);
                                textComp.replaceText(tip, expireTimeStr);
                            }
                        }

                        tip = segMap.get("nwstime");
                        if (tip != null) {
                            /*
                             * we make this replacement last since purge time
                             * and vtecs are fixed length and this is variable
                             * length, which ensures we only need to reParse()
                             * once per segment
                             */
                            List<String> zones = decodeUGCs(
                                    pds.getSegmentsArray().get(i));
                            Collection<String> timeZones = getTimeZones(zones,
                                    officeTimeZone);

                            StringBuilder sb = new StringBuilder();
                            for (String tz : timeZones) {
                                String issueTime;
                                if (tz.equals(officeTimeZone)) {
                                    issueTime = officeIssueTime;
                                } else {
                                    fmt.setTimeZone(TimeZone.getTimeZone(tz));
                                    issueTime = fmt.format(now);
                                }
                                if (sb.length() > 0) {
                                    sb.append(" /");
                                    sb.append(issueTime);
                                    sb.append("/");
                                } else {
                                    sb.append(issueTime);
                                }
                            }

                            textComp.replaceText(tip, sb.toString());
                        }
                        textComp.endUpdate();
                    }

                    // force a single reParse(), since we've updated a bunch
                    // of text
                    textComp.reParse();
                }
            } finally {
                textComp.endUpdate();
            }
        }
    }

    private Collection<String> getTimeZones(List<String> zones,
            String officeTimeZone) {
        Collection<String> result = zonesToTimeZones.get(zones);
        if (result == null) {
            result = dm.getTextProductMgr().getTimeZones(zones, officeTimeZone);
            zonesToTimeZones.put(zones, result);
        }
        return result;
    }

    /**
     * Given the issuance time, expiration time (desired), and the VTEC codes,
     * returns the appropriate expiration time. Expiration time is the earliest
     * of the specified expiration time, 1 hr if a CAN code is detected, or the
     * ending time of ongoing events (CON, EXT, EXB, NEW).
     *
     * @param issTime
     *            issue time
     * @param expTime
     *            expire time
     * @param vtecStr
     *            vtec string
     *
     * @return expire time
     */
    public Date getExpireTime(Date issTime, Date expTime, String vtecStr) {
        long roundMinutes = 15;
        Date endTime = null;
        Date expireTime = expTime;
        Calendar issCal = Calendar.getInstance();
        issCal.setTime(issTime);
        issCal.add(Calendar.HOUR_OF_DAY, 1);
        Date issTimePlusHour = issCal.getTime();

        if (vtecStr != null) {
            // break up the VTEC strings, decode ending time
            String vtecStrs[] = vtecStr.split("\n");
            boolean canExpFound = false;
            boolean activeFound = false;

            // later end time of all active events
            Date laterActive = null;

            Date zeroDate = new Date(0);

            for (String str : vtecStrs) {
                Matcher matcher = vtecRE.matcher(str);

                if (matcher.find()) {
                    String action = matcher.group(1);
                    if ("CAN".equals(action) || "EXP".equals(action)) {
                        canExpFound = true;
                    } else {
                        try {
                            activeFound = true;
                            endTime = timeFromYYYYMMDD_HHMM(matcher.group(8),
                                    matcher.group(9));
                            if (!endTime.equals(zeroDate)) {
                                if (laterActive != null) {
                                    laterActive = (laterActive.after(endTime))
                                            ? laterActive
                                            : endTime;
                                } else {
                                    laterActive = endTime;
                                }
                            }
                        } catch (ParseException e) {
                            statusHandler.error("Error parsing time", e);
                        }
                    }
                }
            }

            if (laterActive != null) {
                expireTime = (laterActive.before(expireTime)) ? laterActive
                        : expireTime;
            } else if (canExpFound && !activeFound) {
                expireTime = (expireTime.before(issTimePlusHour)) ? expireTime
                        // 1hr from now
                        : issTimePlusHour;
            }
        }

        // ensure expireTime is not before issueTime, and is at least 1 hour
        if (expireTime.before(issTimePlusHour)) {
            expireTime = issTimePlusHour;
        }

        // round to next "roundMinutes"
        long roundSec = roundMinutes * TimeUtil.SECONDS_PER_MINUTE;

        // converting to seconds
        long expireTimeSec = expireTime.getTime() / TimeUtil.MILLIS_PER_SECOND;
        long delta = expireTimeSec % roundSec;
        long baseTime = (expireTimeSec / roundSec) * roundSec
                * TimeUtil.MILLIS_PER_SECOND;
        if ((delta / TimeUtil.SECONDS_PER_MINUTE) >= 1) {
            expireTime.setTime(
                    baseTime + (roundSec * TimeUtil.MILLIS_PER_SECOND));
        } else {
            // within 1 minute, don't add next increment
            expireTime.setTime(baseTime);
        }

        return expireTime;
    }

    /**
     * Load a previous product into the editor.
     */
    private void loadPrevious() {
        String initialValue;

        if (!testVTEC) {
            initialValue = "cccnnnxxx";
        } else {
            initialValue = "nnnxxx";
        }

        InputDialog dlg = new InputDialog(getShell(), "Read AWIPS TextDB",
                "AWIPS Product ID", initialValue, null);
        if (dlg.open() != Window.OK) {
            return;
        }
        String pid = dlg.getValue();
        if (!testVTEC) {
            if ((pid.length() != 8) && (pid.length() != 9)) {
                MessageDialog.openError(getShell(), "Bad text database pil",
                        "TextDB Pil must be 8 or 9 characters in length.");
                return;
            }
        } else {
            if ((pid.length() < 4) || (pid.length() > 6)) {
                MessageDialog.openError(getShell(), "Bad text database pil",
                        "TextDB Pil must be 4, 5 or 6 characters in length.");
                return;
            }
        }

        pid = pid.toUpperCase();
        textdbPil = pid;

        if (!testVTEC) {
            CAVEMode mode = CAVEMode.getMode();
            boolean operationalMode = (CAVEMode.OPERATIONAL.equals(mode)
                    || CAVEMode.TEST.equals(mode) ? true : false);

            String product = TextDBUtil.retrieveProduct(pid, operationalMode);
            if ((product != null) && !product.isEmpty()) {
                // add back the new line stripped off by text decoder
                setProductText(product + "\n", false);
            }
        } else {
            devLoad(pid);
        }
        parseIDs();
        revive();

        // Enter res mode
        setPTypeCategory(PTypeCategory.PE);
        textComp.setCorMode(true);

        setPurgeTime();
    }

    /**
     * Open a file.
     */
    private void openFile() {
        String fname = getDir();
        FileDialog dialog = new FileDialog(parent.getShell(), SWT.OPEN);
        dialog.setFilterPath(fname);
        fname = dialog.open();
        if (fname != null) {
            File f = new File(fname);
            try {
                String fileText = ProductFileUtil.readFile(f);
                setProductText(fileText);
            } catch (IOException e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }
    }

    /**
     * Save the current text in the product editor to a file.
     */
    private void saveFile() {

        String fname = getDir();

        FileDialog fd = new FileDialog(parent.getShell(), SWT.SAVE);
        fd.setText("Save As");
        fd.setFilterPath(fname);
        fd.setFileName(guessFilename());
        fname = fd.open();

        if (fname != null) {
            productDefinition.put("outputFile", fname);
            try {
                ProductFileUtil.writeFile(textComp.getProductText(),
                        new File(fname));
            } catch (IOException e) {
                statusHandler.debug("Save Failed", e);
                MessageBox mb = new MessageBox(parent.getShell(),
                        SWT.RETRY | SWT.CANCEL);
                mb.setText("Save Failed");
                mb.setMessage(e.getLocalizedMessage() + "\n Try again?");
                if (mb.open() == SWT.RETRY) {
                    saveFile();
                }
            }
        }
    }

    /**
     * handle autoWrite, autoStore
     */
    public void doAutoStuff() {
        int autoWrite = 0;
        Object autoWrite_obj = productDefinition.get("autoWrite");
        if (autoWrite_obj != null) {
            autoWrite = ((Number) autoWrite_obj).intValue();
        }

        int autoStore = 0;
        Object autoStore_obj = productDefinition.get("autoStore");
        if (autoStore_obj != null) {
            autoStore = ((Number) autoStore_obj).intValue();
        }

        if (autoWrite == 1) {
            autoWrite();
        }

        if (autoStore == 1) {
            storeTransmit(Action.AUTOSTORE);
        }
    }

    /*
     * autoWrite
     */
    private void autoWrite() {
        String fname = null;
        if (productDefinition.get("outputFile") != null) {
            fname = getDefString("outputFile");
            if (fname.isEmpty()) {
                return;
            }
        } else {
            return;
        }

        fname = fixfname(fname);

        try {
            ProductFileUtil.writeFile(getProductText(), new File(fname));
        } catch (IOException e) {
            statusHandler.debug("Formatter AutoWrite failed: " + this.pil, e);
            MessageBox mb = new MessageBox(parent.getShell(),
                    SWT.OK | SWT.ICON_WARNING);
            mb.setText("Error");
            mb.setMessage("Formatter AutoWrite failed: " + this.pil);
            mb.open();
        }
    }

    /*
     * Replace {prddir} with siteConfig.GFESUITE_PRDDIR if applicable.
     */
    private String fixfname(String fname) {
        if (fname.contains("{prddir}")) {
            fname = fname.replace("{prddir}", prdDir);
        }

        return fname;
    }

    private String guessFilename() {
        if (productDefinition.get("outputFile") != null) {
            String basename = new File(getDefString("outputFile")).getName();
            return basename;
        }

        return guessTDBPil();
    }

    /**
     * Get the directory.
     *
     * @return The directory
     */
    private String getDir() {
        String fname = null;

        if (testVTEC) {
            fname = prdDir + "/PRACTICE";
        } else {
            if (productDefinition.get("outputFile") != null) {
                fname = getDefString("outputFile");
                if (fname != null) {
                    fname = fixfname(fname);
                    if (!(new File(fname)).isDirectory()) {
                        fname = new File(fname).getParent();
                    }
                }
            }

            if (fname == null) {
                fname = prodEditorDirectory;
            }
        }

        return fname;
    }

    /**
     * Store the product to a local file.
     *
     * Used for testVTEC products instead of storing to the TextDB
     *
     * @param pil
     */
    public void devStore(String pil) {
        try {
            ProductFileUtil.writeFile(getProductText(),
                    ProductFileUtil.getProductFile(pil, true));
        } catch (Exception ex) {
            statusHandler.handle(Priority.PROBLEM,
                    "Failed to save product " + pil + " to file.", ex);
        }
    }

    private void devLoad(String pil) {
        LocalizationFile file = ProductFileUtil.getLastProductFile(pil, true);
        if (file == null) {
            statusHandler.handle(Priority.EVENTA,
                    "Previous product for " + pil + " not found.");
            return;
        }

        String product = null;
        try {
            product = ProductFileUtil.readFile(file);
        } catch (IOException | LocalizationException ex) {
            statusHandler.handle(Priority.PROBLEM,
                    "Failed to load product " + pil, ex);
            return;
        }

        if (product != null) {
            // Start from a clean slate by resetting the editor text to empty
            setProductText("", false);

            // Add the text from the file to the editor window
            setProductText(product, false);
        }
    }

    /**
     * Save the product text as a draft.
     */
    public void saveDraft() {
        try {
            DraftProduct draft = new DraftProduct(productDefinition,
                    getProductText());
            draft.save(ProductFileUtil.getDraftFile(productId));
            setStatusText('R', productId + " draft saved.");
        } catch (Exception ex) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error storing draft for " + productId, ex);
        }
    }

    /**
     * Load saved draft product into the editor.
     */
    public void loadDraft() {
        try {
            LocalizationFile lf = ProductFileUtil.getDraftFile(productId);

            if (!lf.exists()) {
                setStatusText('S', "No previous draft for " + productId);
                return;
            }

            DraftProduct draft = DraftProduct.load(lf);
            productDefinition = draft.getProductDefinition();

            clearProductText();
            setProductText(draft.getProductText());
            setTabColorFunc(ProductStateEnum.Finished);
            setStatusText('R', productId + " draft loaded.");

            if (productDefinition.get("brain") != null) {
                brain();
                String msg = "Your saved draft was loaded, but the draft is invalid "
                        + "and cannot be transmitted.\nThe draft is invalid since a "
                        + "product containing VTEC matching the pil was received "
                        + "after the draft was originally saved.\nThis causes VTEC to "
                        + "be suspect.  You must re-run the formatter.";
                setStatusText('U', msg);
                setTabColorFunc(ProductStateEnum.New);
            } else {
                revive();
                retrieveActiveVTEC();
            }
        } catch (Exception ex) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error loading draft for " + productId, ex);
            return;
        }
    }

    private void displayCallToActionsDialog(int callToActionType) {
        // Allow only one of the 3 types of dialogs to be displayed.
        if ((ctaDialog != null) && (ctaDialog.getShell() != null)
                && !ctaDialog.isDisposed()) {
            ctaDialog.bringToTop();
            return;
        }

        if (callToActionType == 1) {
            CTAHazCB(callToActionType);
        }
        if (callToActionType == 2) {
            String[] Sig = new String[1];
            String pil = getDefString("pil");
            if (pil != null) {
                Sig[0] = pil.substring(0, 3);
                ctaDialog = new CallToActionsDlg(parent.getShell(),
                        callToActionType, Sig, this);
                ctaDialog.setBlockOnOpen(false);
                ctaDialog.open();
            }
        }
        if (callToActionType == 3) {
            String[] Sig = new String[1];
            ctaDialog = new CallToActionsDlg(parent.getShell(),
                    callToActionType, Sig, this);
            ctaDialog.setBlockOnOpen(false);
            ctaDialog.open();
        }
    }

    /**
     * Display the Find or Find & Replace dialog.
     *
     * @param findAndReplace
     *            If true show the Find & Replace dialog, false shows the Find
     *            dialog.
     */
    private void displayFindReplaceDialog(boolean findAndReplace) {
        // The dialog being opened is modal to the parent dialog. This will
        // prevent the launching of another dialog until the modal dialog is
        // closed.
        FindReplaceDlg findAndReplaceDlg = new FindReplaceDlg(parent.getShell(),
                findAndReplace, textComp);
        findAndReplaceDlg.open();
    }

    private void CTAHazCB(int callToActionType) {

        textEditorST = textComp.getTextEditorST();
        int offset = textEditorST.getCaretOffset();

        prodDataStruct = textComp.getProductDataStruct();
        List<SegmentData> segArray = prodDataStruct.getSegmentsArray();
        TextIndexPoints segTip;

        int[] offset_header = new int[50];
        int h = 0;
        for (SegmentData segmentData : segArray) {
            segTip = segmentData.getSegmentDataIndexPoints("header");

            if (segTip != null) {
                int startLine = segTip.getStartIndex().x;
                offset_header[h] = textEditorST.getOffsetAtLine(startLine);
                h++;
            }
        }

        if (offset < offset_header[0]) {
            String msg1 = "...Insertion point not in segment...\n\n ";
            MessageBox mb1 = new MessageBox(getShell(),
                    SWT.OK | SWT.ICON_WARNING);
            mb1.setText("CTA");
            mb1.setMessage(msg1);
            mb1.open();
            return;
        }

        if (offset >= (textEditorST.getCharCount() - 1)) {
            String msg1 = "...Insertion point out of segment...\n\n ";
            MessageBox mb1 = new MessageBox(getShell(),
                    SWT.OK | SWT.ICON_WARNING);
            mb1.setText("CTA");
            mb1.setMessage(msg1);
            mb1.open();
            return;
        }

        List<SegmentData> segs = prodDataStruct.getSegmentsArray();
        if ((segs == null) || (segs.isEmpty())) {
            return;
        }
        Pattern codePattern = Pattern.compile("\\.([A-Z]{3})\\.");
        for (SegmentData segData : segs) {
            Map<String, TextIndexPoints> segMap = segData.getSementMap();
            TextIndexPoints tipUgc = segMap.get("ugc");
            int start = prodDataStruct.positionToOffset(tipUgc.getStartIndex());
            int end = prodDataStruct.positionToOffset(tipUgc.getEndIndex());

            if ((offset <= start) || (offset >= end)) {
                continue;
            }

            TextIndexPoints tipVtec = segMap.get("vtec");
            if (tipVtec == null) {
                break;
            }

            start = prodDataStruct.positionToOffset(tipVtec.getStartIndex());
            end = prodDataStruct.positionToOffset(tipVtec.getEndIndex());

            int lineCount = tipVtec.getEndIndex().x - tipVtec.getStartIndex().x;
            String[] newVtec = new String[lineCount];
            String[] Sig = new String[lineCount];
            for (int i = 0; i < lineCount; i++) {
                String vtec = prodDataStruct.getProductTextArray()[i
                        + tipVtec.getStartIndex().x];
                if (vtec.indexOf('-') < 0) {
                    newVtec[i] = vtec;
                } else {
                    Matcher matcher = codePattern.matcher(vtec);
                    if (matcher.find()) {
                        String code = matcher.group();

                        if (".NEW.".equals(code) || ".CON.".equals(code)
                                || ".EXT.".equals(code) || ".EXB.".equals(code)
                                || ".EXA.".equals(code)
                                || ".COR.".equals(code)) {
                            newVtec[i] = vtec;

                            String phenSig = vtec.substring(12, 16);
                            Sig[i] = phenSig;
                        }
                    }
                }
            }

            HazardCTA = true;
            ctaDialog = new CallToActionsDlg(parent.getShell(),
                    callToActionType, Sig, this);
            ctaDialog.setBlockOnOpen(false);
            ctaDialog.addCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    HazardCTA = false;
                    ctaDialog = null;
                }
            });
            ctaDialog.open();
            // Found segment the cursor is in no need to check more segments.
            break;
        }
    }

    /**
     * Insert text
     *
     * @param txt
     */
    public void insertText(String txt) {

        textEditorST = getTextEditorST();
        prodDataStruct = textComp.getProductDataStruct();

        int offset = textEditorST.getCaretOffset();
        if (offset >= (textEditorST.getCharCount() - 1)) {
            return;
        }

        StyleRange sr = textEditorST.getStyleRangeAtOffset(offset);

        if (sr != null) {
            // If the cursor is in a locked area then return;
            if (sr.foreground == textComp.getLockColor()) {
                String msg = "The insertion point is in locked area then try "
                        + "to select another.\n\nAction cancelled.";
                MessageBox mb = new MessageBox(getShell(),
                        SWT.OK | SWT.ICON_WARNING);
                mb.setText("CTA");
                mb.setMessage(msg);
                mb.open();
                return;
            }
        }

        List<SegmentData> segArray = prodDataStruct.getSegmentsArray();
        TextIndexPoints segTip;

        int[] offset_header = new int[50];
        int h = 0;
        for (SegmentData segmentData : segArray) {
            segTip = segmentData.getSegmentDataIndexPoints("header");

            if (segTip != null) {
                int startLine = segTip.getStartIndex().x;
                offset_header[h] = textEditorST.getOffsetAtLine(startLine);
                h++;
            }
        }

        if (HazardCTA && (offset < offset_header[0])) {
            String msg1 = "...Insertion point not in segment...\n\n ";
            MessageBox mb1 = new MessageBox(getShell(),
                    SWT.OK | SWT.ICON_WARNING);
            mb1.setText("CTA");
            mb1.setMessage(msg1);
            mb1.open();
            return;
        }

        this.textEditorST.insert(txt);
        this.textEditorST.setCaretOffset(
                this.textEditorST.getCaretOffset() + txt.length());
    }

    /**
     * @return the ImageRegistry
     */
    public ImageRegistry getImageRegistry() {
        if (registry == null) {
            registry = new ImageRegistry();
            registry.put("transmitDisabled",
                    AbstractUIPlugin.imageDescriptorFromPlugin(
                            Activator.PLUGIN_ID, "icons/transmitDisabled.gif"));
            registry.put("transmitLive",
                    AbstractUIPlugin.imageDescriptorFromPlugin(
                            Activator.PLUGIN_ID, "icons/transmitLive.gif"));
            registry.put("checkmark",
                    AbstractUIPlugin.imageDescriptorFromPlugin(
                            Activator.PLUGIN_ID, "icons/checkmark.gif"));
            registry.put("yieldsign",
                    AbstractUIPlugin.imageDescriptorFromPlugin(
                            Activator.PLUGIN_ID, "icons/yieldsign.gif"));
            registry.put("stopsign", AbstractUIPlugin.imageDescriptorFromPlugin(
                    Activator.PLUGIN_ID, "icons/stopsign.gif"));
        }

        return registry;
    }

    /**
     * @return the StyledText of the editor
     */
    public StyledText getTextEditorST() {
        return textComp.getTextEditorST();
    }

    /**
     * Set the product text and revive the editor
     *
     * @param text
     */
    public void setProductText(String text) {
        setProductText(text, true);
    }

    /**
     * Set the product text
     *
     * @param text
     * @param reviveEditor
     *            true if editor should be revived
     */
    public void setProductText(String text, boolean reviveEditor) {
        textComp.setProductText(text);
        if (reviveEditor) {
            revive();
            setPurgeTime();
        }
    }

    /**
     * @return the product text
     */
    public String getProductText() {
        return textComp.getProductText();
    }

    /**
     * @return the productId
     */
    public String getProductId() {
        return productId;
    }

    /**
     * @return the NNN ID of the product
     */
    public String getNNNid() {
        return productId.substring(4, 7);
    }

    /**
     * @return the productName
     */
    public String getProductName() {
        return productName;
    }

    /**
     * @return the product type
     */
    public String getProductType() {
        return prodTypeCbo.getItem(prodTypeCbo.getSelectionIndex());
    }

    /**
     * @return the autoSendAddress
     */
    public String getAutoSendAddress() {
        return autoSendAddress;
    }

    /**
     * @return true if using test VTEC
     */
    public boolean isTestVTEC() {
        return testVTEC;
    }

    @Override
    public void notificationArrived(NotificationMessage[] messages) {
        for (NotificationMessage msg : messages) {
            try {
                Object payload = msg.getMessagePayload();
                if (payload instanceof VTECTableChangeNotification) {
                    final VTECTableChangeNotification notification = (VTECTableChangeNotification) payload;
                    VizApp.runSync(new Runnable() {
                        @Override
                        public void run() {
                            processTableMsg(notification);
                        }
                    });
                }
            } catch (NotificationException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error getting notification message", e);
            }
        }
    }

    private void processTableMsg(VTECTableChangeNotification msg) {
        // LogStream.logEvent('processTableMsg: ', msg);
        if (productDefinition == null) {
            return;
        }

        if (!CAVEMode.getMode().equals(CAVEMode.PRACTICE)
                && msg.getMode().equals(ActiveTableMode.PRACTICE)) {
            return;
        }
        if (CAVEMode.getMode().equals(CAVEMode.PRACTICE)
                && !msg.getMode().equals(ActiveTableMode.PRACTICE)) {
            return;
        }
        String brained = null;
        boolean allFound = false;
        String sid = getDefString("fullStationID");
        String pilxxx = getDefString("pil");
        String pil = null;
        if (pilxxx != null) {
            pil = pilxxx.substring(0, 3);
            List<String> pils = VTECTableChangeNotification.DisableTable
                    .get(pil);

            // append xxxId to pil for matching
            if (pils != null) {
                String xxxId = pilxxx.substring(3, pilxxx.length());
                for (int i = 0; i < pils.size(); i++) {
                    pils.set(i, pils.get(i) + xxxId);
                }
            }

            for (VTECChange m : msg.getChanges()) {
                if ("*ALL".equals(m.getSite()) || "*ALL*".equals(m.getPil())) {
                    allFound = true;
                }
                String msgPilxxx = m.getPil() + m.getXxxid();
                if (m.getSite().equals(sid)) {
                    if ((pils == null) && msgPilxxx.equals(pilxxx)) {
                        if (brain()) {
                            brained = m.getPil();
                        }
                        break;
                    } else if (pils != null) {
                        if (pils.contains(m.getPil())) {
                            if (brain()) {
                                brained = m.getPil();
                            }
                            break;
                        }
                    }
                }
            }
        }

        // inform user of product arrival
        for (VTECChange m : msg.getChanges()) {
            if (m.getSite().equals(sid)) {
                String msg1 = "Received Product: " + m.getPil() + " for " + sid;
                setStatusText('R', msg1);
            }
        }

        // send out warning to due braining and conflicts
        if (brained != null) {
            String msg1 = "A " + brained + " was just sent by your office.\n"
                    + "This affects the VTEC coding for your " + pil
                    + " product currently in the Formatter Launcher and thus the "
                    + pil + " product is now invalid.\n"
                    + "Transmitting has been disabled for the " + pil + " tab. "
                    + "You must re-run the " + pil + " formatter. ";
            setStatusText('U', msg1);

            // set tab back to gray
            setTabColorFunc(ProductStateEnum.New);
            // special *ALL* case
        } else if (allFound) {
            brain();

            // set tab back to gray
            setTabColorFunc(ProductStateEnum.New);
        }
    }

    /**
     * Disable (brain) the editor
     *
     * @return true if editor was enabled, false if already disabled
     */
    public boolean brain() {
        // brains the editor (dims the buttons), returns 0 if already dead,
        // 1 if state has changed to dead.
        if (dead) {
            return false;
        }
        dead = true;
        activeVtecRecords = null;
        for (Button b : buttons) {
            b.setEnabled(false);
        }
        for (MenuItem i : menuItems) {
            i.setEnabled(false);
        }
        timeUpdater.cancel();
        return true;
    }

    /**
     * Revive the editor
     */
    public void revive() {
        timeUpdater.schedule();
        if (!dead) {
            return;
        }
        dead = false;
        productDefinition.getDefinition().remove("brain");
        for (Button b : buttons) {
            b.setEnabled(true);
        }
        for (MenuItem i : menuItems) {
            i.setEnabled(true);
        }
    }

    /**
     * @param c
     * @param msg1
     */
    private void setStatusText(char importance, String status) {
        Priority priority;

        switch (importance) {
        case 'U':
            // Urgent
            priority = Priority.CRITICAL;
            break;

        case 'S':
            // Significant
            priority = Priority.SIGNIFICANT;
            break;

        case 'A':
            // Alert
            priority = Priority.PROBLEM;
            break;

        case 'R':
            // Routine
        default:
            priority = Priority.EVENTA;
        }

        statusHandler.handle(priority, status);
    }

    /**
     * @param string
     */
    private void setTabColorFunc(ProductStateEnum state) {
        transmissionCB.setTransmissionState(state);
    }

    /**
     * Word-wrap the text selected by the user.
     *
     */
    private void doWrapSelection() {
        StyledText styledText = textComp.getTextEditorST();
        Point selectionRange = styledText.getSelectionRange();
        if (selectionRange.y == 0) {
            String msg1 = "No text is selected\n\n ";
            MessageBox mb1 = new MessageBox(getShell(),
                    SWT.OK | SWT.ICON_WARNING);
            mb1.setText("CTA");
            mb1.setMessage(msg1);
            mb1.open();
            return;
        }

        // Look for locked text in the selection
        if (textComp.rangeHasLockedText(selectionRange.x, selectionRange.y)) {
            String msg2 = "Selection contains locked text\n\n ";
            MessageBox mb2 = new MessageBox(getShell(),
                    SWT.OK | SWT.ICON_WARNING);
            mb2.setText("CTA");
            mb2.setMessage(msg2);
            mb2.open();
            return;
        }

        // Word-wrap the whole selection.
        int curLine = styledText.getLineAtOffset(selectionRange.x);
        int lastSelIdx = (selectionRange.x + selectionRange.y) - 1;
        int lastLine = styledText.getLineAtOffset(lastSelIdx);
        int[] indices = null;
        while ((curLine <= lastLine) && (curLine < styledText.getLineCount())) {
            int lineOff = styledText.getOffsetAtLine(curLine);
            // word wrap a block, and find out how the text length changed.
            indices = textComp.wordWrap(lineOff, wrapColumn);
            int firstIdx = indices[0];
            int lastIdx = indices[1];
            int newLen = indices[2];
            int oldLen = (1 + lastIdx) - firstIdx;
            int diff = newLen - oldLen;
            // adjust our endpoint for the change in length
            lastSelIdx += diff;
            lastLine = styledText.getLineAtOffset(lastSelIdx);
            // newLen doesn't include \n, so it can be 0. Don't allow
            // firstIdx+newLen-1 to be < firstIdx, or loop becomes infinite.
            int lastWrapIdx = Math.max(firstIdx, (firstIdx + newLen) - 1);
            // move down to the next unwrapped line
            curLine = styledText.getLineAtOffset(lastWrapIdx) + 1;
        }
    }

    /**
     * @return the textComp
     */
    public StyledTextComp getTextComp() {
        return textComp;
    }

    /**
     * @return true if editor is disabled
     */
    public boolean isEditorDisabled() {
        return dead;
    }

    protected void clearProductText() {
        textComp.setProductText("");
        timeUpdater.cancel();
    }

    private class ChangeTimesJob extends Job {

        /**
         * @param name
         */
        public ChangeTimesJob(String name) {
            super(name);
            setSystem(true);
        }

        @Override
        protected IStatus run(IProgressMonitor monitor) {
            if (monitor.isCanceled()) {
                return Status.CANCEL_STATUS;
            }

            updateExpireTimeFromTimer();

            // reschedule job to run at the top of the next minute
            Calendar cal = Calendar.getInstance();
            cal.setTime(SimulatedTime.getSystemTime().getTime());
            int secondsTilNextMinute = TimeUtil.SECONDS_PER_MINUTE
                    - cal.get(Calendar.SECOND);
            schedule(secondsTilNextMinute * TimeUtil.MILLIS_PER_SECOND);
            return Status.OK_STATUS;
        }

    }

    /*
     * Handle bad characters in text formatter definition.
     */
    private String getDefString(String key) {
        String str = null;

        Object obj = productDefinition.get(key);
        if ((obj != null) && (obj instanceof Collection)) {
            Collection<?> collection = (Collection<?>) obj;
            str = (String) (collection.toArray())[0];
        } else {
            str = (String) productDefinition.get(key);
        }

        return str;
    }

    /*
     * Add Pop-up GUI for File, Edit, Options, and CallToActions at the location
     * of mouse, when right click the mouse
     */
    private void createEditorPopupMenu() {
        popupMenu = new Menu(textComp);

        MenuItem fileMI = new MenuItem(popupMenu, SWT.CASCADE);
        fileMI.setText("File");
        Menu fileSubMenu = new Menu(popupMenu);
        fileMI.setMenu(fileSubMenu);
        createFileMenu(fileSubMenu);

        MenuItem editMI = new MenuItem(popupMenu, SWT.CASCADE);
        editMI.setText("Edit");
        Menu editSubMenu = new Menu(popupMenu);
        editMI.setMenu(editSubMenu);
        createEditMenu(editSubMenu);

        MenuItem optionsMI = new MenuItem(popupMenu, SWT.CASCADE);
        optionsMI.setText("Options");
        Menu optionsSubMenu = new Menu(popupMenu);
        optionsMI.setMenu(optionsSubMenu);
        createOptionsMenu(optionsSubMenu);

        MenuItem callToActionsMI = new MenuItem(popupMenu, SWT.CASCADE);
        callToActionsMI.setText("CallToActions");
        Menu callToActionsSubMenu = new Menu(popupMenu);
        callToActionsMI.setMenu(callToActionsSubMenu);
        createCallToActionsMenu(callToActionsSubMenu);
    }

    private void printProductText(PrinterData printerData) {
        Printer printer = new Printer(printerData);
        printer.startJob(prodDataStruct.getPil());

        Rectangle clientArea = printer.getClientArea();
        Rectangle trim = printer.computeTrim(0, 0, 0, 0);
        Point dpi = printer.getDPI();

        int leftMargin = dpi.x + trim.x;
        int topMargin = (dpi.y / 2) + trim.y;
        int bottomMargin = clientArea.height - dpi.y / 2 + trim.y + trim.height;

        String productText = textComp.getProductText();
        String[] lines = productText.split(
                Pattern.quote(textComp.getTextEditorST().getLineDelimiter()));

        GC gc = new GC(printer);
        // gc.setFont(font);
        int lineHeight = gc.getFontMetrics().getHeight();

        printer.startPage();
        int y = topMargin;
        for (int line = 0; line < lines.length; line++) {
            gc.drawText(lines[line], leftMargin, y);
            y += lineHeight;

            if (y + lineHeight >= bottomMargin) {
                printer.endPage();
                if (line + 1 < lines.length) {
                    y = topMargin;
                    printer.startPage();
                }
            }
        }

        // font.dispose();
        gc.dispose();
        printer.endJob();
        printer.dispose();
    }

}
