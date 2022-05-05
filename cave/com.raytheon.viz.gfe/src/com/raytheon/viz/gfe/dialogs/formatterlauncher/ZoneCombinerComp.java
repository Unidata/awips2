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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.RejectedExecutionException;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ArmEvent;
import org.eclipse.swt.events.ArmListener;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.MenuEvent;
import org.eclipse.swt.events.MenuListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridLocation;
import com.raytheon.uf.common.dataplugin.gfe.exception.GfeException;
import com.raytheon.uf.common.dataplugin.gfe.textproduct.CombinationsFileUtil;
import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.LocalizationUtil;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.protectedfiles.ProtectedFileLookup;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.gfe.GFEPreference;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.DataManagerUIFactory;
import com.raytheon.viz.gfe.textformatter.TextProductManager;
import com.raytheon.viz.gfe.ui.zoneselector.ZoneSelector;

/**
 * Composite containing the Zone Combiner controls.
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Apr 21, 2008  ###      lvenable  Initial creation
 * Jul 07, 2011  9293     rferrel   Hook to allow checking of includeAllZones
 * Nov 07, 2012  1298     rferrel   Changes for non-blocking
 *                                  ClearZoneGroupsDialog. Changes for
 *                                  non-blocking SaveDeleteComboDlg. Changes for
 *                                  non-blocking ShuffleZoneGroupsDialog.
 *                                  Changes for non-blocking ZoneColorEditorDlg.
 * Mar 14, 2013  1794     djohnson  Consolidate common FilenameFilter
 *                                  implementations.
 * Sep 05, 2013  2329     randerso  Removed obsolete methods, added
 *                                  ApplyZoneCombo method
 * Oct 17, 2013  2481     randerso  Fixed regression which cause configured
 *                                  level combinations files to not be found.
 *                                  Removed message when combinations file not
 *                                  found to match A1.
 * Dec 03, 2013  2591     dgilling  Ensure all change states to the combo file
 *                                  are handled.
 * Jan 07, 2014  2662     randerso  Disabled zone combiner if no maps are
 *                                  selected
 * Feb 04, 2014  2591     randerso  Forced reload of combinations file after
 *                                  save to ensure file is updated prior to
 *                                  running formatter Changed to use
 *                                  CombinationsFileChangedNotification instead
 *                                  of FileUpdatedMessage so we can ignore our
 *                                  own changes Moved retrieval of combinations
 *                                  file to CombinationsFileUtil.init
 * Oct 07, 2015  4695     dgilling  Move loading of combinations file off UI
 *                                  thread.
 * Apr 25, 2016  5605     randerso  Switched back to writing combinations file
 *                                  using Localization
 * Oct 03, 2016  19293    randerso  Moved CombinationsFileUtil to common
 * Nov 16, 2016  6007     randerso  Fix issue where combinations file may not be
 *                                  found if created in another GFE session
 * Aug 07, 2017  6379     njensen   Use ProtectedFileLookup
 * Jan 24, 2018  7153     randerso  Changes to allow new GFE config file to be
 *                                  selected when perspective is re-opened.
 * Feb 20, 2018  7045     randerso  Fixed invalid thread access when combination
 *                                  file is updated.
 *
 * </pre>
 *
 * @author lvenable
 *
 */
public class ZoneCombinerComp extends Composite implements IZoneCombiner {

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(ZoneCombinerComp.class);

    /**
     * Parent composite.
     */
    private final Composite parent;

    /**
     * Tool bar that mimics a menu bar.
     */
    private ToolBar toolbar;

    /**
     * Combo Sets menu.
     */
    private Menu comboSetsMenu;

    /**
     * Combo Actions menu.
     */
    private Menu comboActionsMenu;

    /**
     * Combo Options menu.
     */
    private Menu comboOptionsMenu;

    /**
     * Zoom menu.
     */
    private Menu zoomMenu;

    /**
     * Combo Sets tool item that contains the Combo Sets menu.
     */
    private ToolItem comboSetsTI;

    /**
     * Combo Actions tool item that contains the Combo Actions menu.
     */
    private ToolItem comboActionsTI;

    /**
     * Combo Options tool item that contains the Combo Options menu.
     */
    private ToolItem comboOptionsTI;

    /**
     * Zoom tool item that contains the zoom menu.
     */
    private ToolItem zoomTI;

    /**
     * Zone combiner apply button.
     */
    private Button applyZoneComboBtn;

    /**
     * Product combinations label.
     */
    private Label productCombinationsLbl;

    /**
     * Product name.
     */
    private final String productName;

    /**
     * Load sub menu. This gets generated when the program runs.
     */
    private Menu loadSubMenu;

    /**
     * A list of saved combination names.
     */
    private ZoneSelector zoneSelector;

    private final IProductTab callBack;

    protected TextProductManager textProductMgr;

    protected DataManager dataManager;

    protected String product;

    protected boolean mapRequired;

    private List<RGB> colorMap = new ArrayList<>();

    private final String COLOR_MAP_FILE = FileUtil.join("gfe", "combinations",
            "Combinations_ColorMap");

    private static final String theSaved = "";

    private Composite mapCompCtrl;

    private boolean labelZones = false;

    private boolean labelZoneGroups = true;

    protected Object initialCenterLat = null;

    protected Object initialCenterLon = null;

    protected Object initialZoom = null;

    private final LocalizationFile combinationsFile;

    private boolean includeAllZones = false;

    private List<String> mapNames;

    private ILocalizationFileObserver combinationsChangeListener;

    private final ExecutorService asyncExecutor;

    private void initPreferences() {
        labelZones = GFEPreference.getBoolean("ZoneCombiner_LabelZones", false);
        labelZoneGroups = GFEPreference.getBoolean("ZoneCombiner_LabelGroups",
                true);
    }

    /**
     * Constructor.
     *
     * @param parent
     *            Parent composite.
     * @param callBack
     * @param productName
     *            Product name.
     * @param textProductMgr
     * @param dataManager
     */
    public ZoneCombinerComp(Composite parent, IProductTab callBack,
            String productName, TextProductManager textProductMgr,
            DataManager dataManager) {
        super(parent, SWT.NONE);

        this.parent = parent;

        this.productName = productName;

        this.callBack = callBack;

        this.textProductMgr = textProductMgr;

        this.dataManager = dataManager;

        mapRequired = this.textProductMgr.mapRequired(productName);
        this.mapNames = getMapNames(productName);
        if (mapNames.isEmpty()) {
            mapRequired = false;
        }

        this.asyncExecutor = Executors.newCachedThreadPool();

        initPreferences();
        init();

        String combinationsName = textProductMgr
                .getCombinationsFileName(productName);

        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext baseCtx = pathMgr.getContext(
                LocalizationType.CAVE_STATIC, LocalizationLevel.BASE);
        combinationsFile = pathMgr.getLocalizationFile(baseCtx,
                LocalizationUtil.join(
                        CombinationsFileUtil.COMBINATIONS_DIR_PATH,
                        combinationsName + ".py"));

        this.combinationsChangeListener = new ILocalizationFileObserver() {

            @Override
            public void fileUpdated(FileUpdatedMessage message) {
                comboFileChanged(message);

            }
        };

        this.addDisposeListener(new DisposeListener() {

            @Override
            public void widgetDisposed(DisposeEvent e) {
                combinationsFile
                        .removeFileUpdatedObserver(combinationsChangeListener);
                ZoneCombinerComp.this.asyncExecutor.shutdown();
            }
        });

        combinationsFile
                .addFileUpdatedObserver(this.combinationsChangeListener);

        /*
         * load the combinations file to ensure the latest version is pulled
         * from the server even if the zone combiner is not displayed
         */
        try {
            CombinationsFileUtil.init(combinationsName);
        } catch (GfeException e) {
            statusHandler.error(
                    "Error retrieving combinations file: " + combinationsName,
                    e);
        }
    }

    private List<String> getMapNames(String productName) {
        Object obj = this.textProductMgr.getMapNameForCombinations(productName);
        List<String> mapNames = new ArrayList<>();
        if (obj instanceof String) {
            String s = (String) obj;
            if (!s.isEmpty()) {
                mapNames.add(s);
            }
        } else if (obj instanceof List<?>) {
            @SuppressWarnings("unchecked")
            List<String> list = (List<String>) obj;
            mapNames.addAll(list);
        }

        return mapNames;
    }

    /**
     * Initialize the composite.
     */
    private void init() {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        GridLayout gl = new GridLayout(1, false);
        gl.marginHeight = 1;
        gl.marginWidth = 1;
        setLayout(gl);
        setLayoutData(gd);

        // loadMapConfig();
        initializeComponents();

        this.pack();
    }

    /**
     * Initialize the controls on the composite.
     */
    private void initializeComponents() {

        createToolbar();

        createMapArea(theSaved);

        createBottomControls();

        applyButtonState(false);
    }

    /**
     * Create the tool bar that will act like a menu bar.
     */
    private void createToolbar() {
        toolbar = new ToolBar(this, SWT.NONE);

        createComboSetsMenu();
        createComboActionsMenu();
        createComboOptionsMenu();
        createZoomMenu();

        comboSetsTI = new ToolItem(toolbar, SWT.DROP_DOWN);
        comboSetsTI.setText("Combo Sets");
        if (!mapRequired) {
            comboSetsTI.setEnabled(false);
        }
        comboSetsTI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                Rectangle rect = comboSetsTI.getBounds();
                Point pt = new Point(rect.x, rect.y + rect.height);
                pt = toolbar.toDisplay(pt);
                comboSetsMenu.setLocation(pt.x, pt.y);
                comboSetsMenu.setVisible(true);
            }
        });

        comboActionsTI = new ToolItem(toolbar, SWT.DROP_DOWN);
        comboActionsTI.setText("Combo Actions");
        if (!mapRequired) {
            comboActionsTI.setEnabled(false);
        }
        comboActionsTI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                Rectangle rect = comboActionsTI.getBounds();
                Point pt = new Point(rect.x, rect.y + rect.height);
                pt = toolbar.toDisplay(pt);
                comboActionsMenu.setLocation(pt.x, pt.y);
                comboActionsMenu.setVisible(true);
            }
        });

        comboOptionsTI = new ToolItem(toolbar, SWT.DROP_DOWN);
        comboOptionsTI.setText("Combo Options");
        if (!mapRequired) {
            comboOptionsTI.setEnabled(false);
        }
        comboOptionsTI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                Rectangle rect = comboOptionsTI.getBounds();
                Point pt = new Point(rect.x, rect.y + rect.height);
                pt = toolbar.toDisplay(pt);
                comboOptionsMenu.setLocation(pt.x, pt.y);
                comboOptionsMenu.setVisible(true);
            }
        });

        zoomTI = new ToolItem(toolbar, SWT.DROP_DOWN);
        zoomTI.setText("Zoom");
        if (!mapRequired) {
            zoomTI.setEnabled(false);
        }
        zoomTI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                Rectangle rect = zoomTI.getBounds();
                Point pt = new Point(rect.x, rect.y + rect.height);
                pt = toolbar.toDisplay(pt);
                zoomMenu.setLocation(pt.x, pt.y);
                zoomMenu.setVisible(true);
            }
        });
    }

    /**
     * Create the Combo Sets menu.
     */
    private void createComboSetsMenu() {
        comboSetsMenu = new Menu(parent.getShell(), SWT.POP_UP);

        MenuItem saveComboMI = new MenuItem(comboSetsMenu, SWT.PUSH);
        saveComboMI.setText("Save Combo...");
        saveComboMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                // The dialog being opened is modal to the parent dialog. This
                // will
                // prevent the launching of another dialog until the modal
                // dialog is
                // closed.
                SaveDeleteComboDlg saveCombDlg = new SaveDeleteComboDlg(
                        parent.getShell(), mapNames, "Save",
                        zoneSelector.getCombos());
                saveCombDlg.setBlockOnOpen(false);
                saveCombDlg.open();
            }
        });

        // ------------------------------------------------------
        // Create the Load Combo menu
        // ------------------------------------------------------
        MenuItem loadComboMI = new MenuItem(comboSetsMenu, SWT.CASCADE);
        loadComboMI.setText("Load Combo");

        loadSubMenu = new Menu(parent.getShell(), SWT.DROP_DOWN);
        loadComboMI.setMenu(loadSubMenu);

        loadComboMI.addArmListener(new ArmListener() {

            @Override
            public void widgetArmed(ArmEvent e) {
                updateLoadMenu();
            }
        });

        // Create the Delete Combo menu item
        MenuItem deleteComboMI = new MenuItem(comboSetsMenu, SWT.PUSH);
        deleteComboMI.setText("Delete Combo...");
        deleteComboMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                // The dialog being opened is modal to the parent dialog. This
                // will
                // prevent the launching of another dialog until the modal
                // dialog is
                // closed.
                SaveDeleteComboDlg deleteCombDlg = new SaveDeleteComboDlg(
                        parent.getShell(), mapNames, "Delete", null);
                deleteCombDlg.setBlockOnOpen(false);
                deleteCombDlg.open();
            }
        });
    }

    /**
     * Create the Combo Actions menu.
     */
    private void createComboActionsMenu() {
        comboActionsMenu = new Menu(parent.getShell(), SWT.POP_UP);

        MenuItem clearMI = new MenuItem(comboActionsMenu, SWT.PUSH);
        clearMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                zoneSelector.updateCombos(new HashMap<String, Integer>());
                applyButtonState(false);
            }
        });
        clearMI.setText("Clear");

        MenuItem revertMI = new MenuItem(comboActionsMenu, SWT.PUSH);
        revertMI.setText("Revert");
        revertMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                Map<String, Integer> comboDict = loadCombinationsFile(
                        getCombinationsFileName());
                zoneSelector.updateCombos(comboDict);
                applyButtonState(false);
            }
        });

        MenuItem clearExamineMI = new MenuItem(comboActionsMenu, SWT.PUSH);
        clearExamineMI.setText("Clear/Examine Groups...");
        clearExamineMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                openClearDialog();
            }
        });

        MenuItem shuffleGroupsMI = new MenuItem(comboActionsMenu, SWT.PUSH);
        shuffleGroupsMI.setText("Shuffle Groups...");
        shuffleGroupsMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                openShuffleDialog();
            }
        });
    }

    /**
     * @return the zone groupings
     */
    public List<List<String>> getZoneGroupings() {
        if (zoneSelector != null) {
            return zoneSelector.getZoneGroupings();
        } else {
            return new ArrayList<>();
        }
    }

    /**
     * Force the zoneSelector to be refreshed
     */
    public void refresh() {
        zoneSelector.refresh();
    }

    private void openClearDialog() {
        // The dialog being opened is modal to the parent dialog. This will
        // prevent the launching of another dialog until the modal dialog is
        // closed.
        ClearZoneGroupsDialog examClearDlg = new ClearZoneGroupsDialog(
                parent.getShell(), zoneSelector, getCombinationsFileName());
        examClearDlg.setBlockOnOpen(false);
        examClearDlg.open();
    }

    private void openShuffleDialog() {
        // The dialog being opened is modal to the parent dialog. This will
        // prevent the launching of another dialog until the modal dialog is
        // closed.
        ShuffleZoneGroupsDialog shuffleDlg = new ShuffleZoneGroupsDialog(
                parent.getShell(), this.zoneSelector,
                getCombinationsFileName());
        shuffleDlg.setBlockOnOpen(false);
        shuffleDlg.open();
    }

    /**
     * Set includeAllZones on the zoneSelector
     */
    public void setIncludeAllZones() {
        zoneSelector.setIncludeAllZones(isIncludeAllZones());
    }

    /**
     * @return true if includeAllZones is set
     */
    public boolean isIncludeAllZones() {
        return includeAllZones;
    }

    /**
     * Create the Combo Options menu.
     */
    private void createComboOptionsMenu() {
        comboOptionsMenu = new Menu(parent.getShell(), SWT.POP_UP);

        MenuItem includeAllZonesMI = new MenuItem(comboOptionsMenu, SWT.CHECK);
        includeAllZonesMI.setText("Include All Zones");
        includeAllZonesMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                includeAllZones = ((MenuItem) event.getSource()).getSelection();
                setIncludeAllZones();
            }
        });
        final MenuItem labelZonesMI = new MenuItem(comboOptionsMenu, SWT.CHECK);
        labelZonesMI.setText("Label Zones");
        labelZonesMI.setSelection(labelZones);
        labelZonesMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                zoneSelector.setLabelZones(labelZonesMI.getSelection());
            }
        });
        final MenuItem labelZoneGroupsMI = new MenuItem(comboOptionsMenu,
                SWT.CHECK);
        labelZoneGroupsMI.setText("Label Zone Groups");
        labelZoneGroupsMI.setSelection(labelZoneGroups);
        labelZoneGroupsMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                zoneSelector
                        .setLabelZoneGroups(labelZoneGroupsMI.getSelection());
            }
        });
        MenuItem zoneColorsMI = new MenuItem(comboOptionsMenu, SWT.PUSH);
        zoneColorsMI.setText("Zone Colors...");
        zoneColorsMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                displayColorEditorDialog();
                // setColorArrayListToFile(rgbArray);
            }
        });
    }

    /**
     * Create the Zoom menu.
     */
    private void createZoomMenu() {
        zoomMenu = new Menu(parent.getShell(), SWT.POP_UP);
        zoomMenu.addMenuListener(new MenuListener() {

            @Override
            public void menuShown(MenuEvent e) {
                double zoomLevel = 1.0
                        / ZoneCombinerComp.this.zoneSelector.getZoomLevel();
                for (MenuItem item : zoomMenu.getItems()) {
                    item.setSelection(Math
                            .abs(zoomLevel - (Double) item.getData()) < 0.1);
                }
            }

            @Override
            public void menuHidden(MenuEvent e) {
                // do nothing
            }
        });

        MenuItem noZoomMI = new MenuItem(zoomMenu, SWT.RADIO);
        noZoomMI.setText("No Zoom");
        noZoomMI.setData(new Double(1.0));

        MenuItem x1_5MI = new MenuItem(zoomMenu, SWT.RADIO);
        x1_5MI.setData(new Double(1.5));
        x1_5MI.setText("x1.5");

        MenuItem x2MI = new MenuItem(zoomMenu, SWT.RADIO);
        x2MI.setData(new Double(2.0));
        x2MI.setText("x2");

        MenuItem x3MI = new MenuItem(zoomMenu, SWT.RADIO);
        x3MI.setData(new Double(3.0));
        x3MI.setText("x3");

        MenuItem x4MI = new MenuItem(zoomMenu, SWT.RADIO);
        x4MI.setData(new Double(4.0));
        x4MI.setText("x4");

        MenuItem x6MI = new MenuItem(zoomMenu, SWT.RADIO);
        x6MI.setData(new Double(6));
        x6MI.setText("x6");

        MenuItem x8MI = new MenuItem(zoomMenu, SWT.RADIO);
        x8MI.setData(new Double(8.0));
        x8MI.setText("x8");

        SelectionListener zoomListener = new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if (((MenuItem) e.widget).getSelection()) {
                    zoneSelector
                            .setZoomLevel(1.0 / (Double) e.widget.getData());
                }
            }
        };

        noZoomMI.addSelectionListener(zoomListener);
        x1_5MI.addSelectionListener(zoomListener);
        x2MI.addSelectionListener(zoomListener);
        x3MI.addSelectionListener(zoomListener);
        x4MI.addSelectionListener(zoomListener);
        x6MI.addSelectionListener(zoomListener);
        x8MI.addSelectionListener(zoomListener);
    }

    /**
     * Update the load menu with any additions or deletions.
     */
    private void updateLoadMenu() {
        if (loadSubMenu.getItemCount() > 0) {
            for (int i = loadSubMenu.getItemCount() - 1; i >= 0; --i) {
                loadSubMenu.getItem(i).dispose();
            }
        }

        LocalizationFile[] lfs = CombinationsFileUtil.getSavedCombos();
        List<String> names = new ArrayList<>();
        for (LocalizationFile lf : lfs) {
            String id = CombinationsFileUtil.fileToId(lf);
            String name = CombinationsFileUtil.fnToName(this.mapNames, id);
            if (name.isEmpty()) {
                continue;
            }
            if (!ProtectedFileLookup.isProtected(lf)) {
                names.add(name);
            }
        }

        if (names.size() >= 0) {
            Collections.sort(names);
            for (String name : names) {
                MenuItem comboMI = new MenuItem(loadSubMenu, SWT.PUSH);
                comboMI.setText(name);
                comboMI.addSelectionListener(new SelectionAdapter() {
                    @Override
                    public void widgetSelected(SelectionEvent event) {
                        String text = ((MenuItem) event.getSource()).getText();
                        loadSavedCombination(text);
                    }
                });
            }
        }
    }

    /**
     * Create the map area.
     *
     */
    private void createMapArea(String theFile) {
        if (mapCompCtrl != null) {
            mapCompCtrl.setVisible(false);
            mapCompCtrl.dispose();
        }
        mapCompCtrl = new Composite(this, SWT.NONE);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        GridLayout gl = new GridLayout(1, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        mapCompCtrl.setLayout(gl);
        mapCompCtrl.setLayoutData(gd);
        gd = new GridData(SWT.CENTER, SWT.CENTER, true, true);
        if (mapRequired) {
            try {
                initializeShapeComponent(mapCompCtrl, theFile);
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
                        e);
            }
        }
    }

    /**
     * Create the buttons at the bottom of the display.
     */
    private void createBottomControls() {
        Composite controlComp = new Composite(this, SWT.NONE);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        GridLayout gl = new GridLayout(2, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        controlComp.setLayout(gl);
        controlComp.setLayoutData(gd);

        applyZoneComboBtn = new Button(controlComp, SWT.PUSH);
        applyZoneComboBtn.setText("Apply Zone Combo");
        if (!mapRequired) {
            applyZoneComboBtn.setEnabled(false);
        }
        applyZoneComboBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                applyZoneCombo();
            }
        });
        Label label = new Label(controlComp, SWT.CENTER);
        GridData gda = new GridData();
        gda.horizontalAlignment = GridData.FILL;
        gda.grabExcessHorizontalSpace = true;
        label.setLayoutData(gda);
        if ("NONE".equals(getCombinationsFileName())) {
            label.setText("<NONE>");
        } else {
            label.setText(productName + " (" + getCombinationsFileName() + ")");
        }
        label.setAlignment(SWT.CENTER);
    }

    /**
     * Save zone combo
     */
    public void applyZoneCombo() {
        if (!buttonState()) {
            return;
        }

        try {
            String comboName = getCombinationsFileName();
            CombinationsFileUtil.generateAutoCombinationsFile(
                    zoneSelector.getZoneGroupings(), comboName);

            // reload here forces file to be retrieved from server before
            // allowing formatter to run
            loadCombinationsFile(comboName);

            applyButtonState(false);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Unable to save " + getCombinationsFileName(), e);
        }
    }

    /**
     * Display the Color Editor dialog.
     */
    private void displayColorEditorDialog() {
        // The dialog being opened is modal to the parent dialog. This will
        // prevent the launching of another dialog until the modal dialog is
        // closed.
        ZoneColorEditorDlg zoneColorDlg = new ZoneColorEditorDlg(
                parent.getShell(), colorMap);
        zoneColorDlg.open();
    }

    private void initializeShapeComponent(Composite controlComp, String theFile)
            throws TransformException, FactoryException, VizException {

        colorMap = getColorsFromFile();

        String comboName = theFile;
        if ((comboName == null) || comboName.isEmpty()) {
            comboName = getCombinationsFileName();
        }
        Map<String, Integer> comboDict = loadCombinationsFile(comboName);

        boolean singleComboOnly = false;
        Object obj = textProductMgr.getDefinitionValue(productName,
                "singleComboOnly");
        if (obj != null) {
            if (obj instanceof Integer) {
                singleComboOnly = ((Integer) obj) != 0;
            } else if (obj instanceof Boolean) {
                singleComboOnly = (Boolean) obj;
            }
        }
        @SuppressWarnings("unchecked")
        List<String> limitZones = (List<String>) textProductMgr
                .getProductDefinition(productName).get("subDomainUGCs");

        // First thing, give the zone resource a bounding geometry.
        GridLocation gloc = DataManagerUIFactory.getCurrentInstance()
                .getParmManager().compositeGridLocation();

        zoneSelector = new ZoneSelector(controlComp, gloc, this);

        zoneSelector.setLimitZones(limitZones);
        zoneSelector.setLimitToOneGroup(singleComboOnly);

        zoneSelector.setLabelZoneGroups(labelZoneGroups);
        zoneSelector.setLabelZones(labelZones);
        zoneSelector.setIncludeAllZones(includeAllZones);
        zoneSelector.setMap(mapNames, comboDict, colorMap);

        if (!mapRequired) {
            controlComp.setEnabled(false);
            controlComp.setVisible(false);
        }
    }

    /**
     * @return the productCombinationsLbl
     */
    public Label getProductCombinationsLbl() {
        return productCombinationsLbl;
    }

    /**
     * @param productCombinationsLbl
     *            the productCombinationsLbl to set
     */
    public void setProductCombinationsLbl(Label productCombinationsLbl) {
        this.productCombinationsLbl = productCombinationsLbl;
    }

    /**
     * @param productMgr
     *            the productMgr to set
     */
    public void setTextProductManager(TextProductManager productMgr) {
        textProductMgr = productMgr;
    }

    /**
     * Returns the combinations file name
     *
     * @return the combinations file name
     */
    public String getCombinationsFileName() {
        String file = textProductMgr.getCombinationsFileName(productName);
        return file;
    }

    /**
     * Returns the localization for the save and delete functions. This is a
     * wrapper around getLocalization(String, level).
     *
     * @param local
     *            The name of the local file
     * @return the USER-level File.
     */
    public File getLocalization(String local) {
        File result = getLocalization(local, LocalizationLevel.USER);
        return result;
    }

    /**
     * Return the local file for the given name and level. Neither parameter is
     * allowed to be null.
     *
     * @param local
     *            The name of the local file
     * @param level
     *            The localization level from which to get the file.
     * @return the specified File.
     */
    public File getLocalization(String local, LocalizationLevel level) {
        if (local == null) {
            throw new IllegalArgumentException("Local file name is null");
        }
        if (level == null) {
            throw new IllegalArgumentException("Localization level is null");
        }
        IPathManager pm = PathManagerFactory.getPathManager();

        LocalizationContext localization = pm
                .getContext(LocalizationType.CAVE_STATIC, level);
        File localFile = pm.getFile(localization, FileUtil
                .join(CombinationsFileUtil.COMBINATIONS_DIR_PATH, local));
        return localFile;
    }

    /**
     * Load a saved combination file
     *
     * @param nameToLoad
     *            name of the file to load
     */
    protected void loadSavedCombination(String nameToLoad) {
        String actName = CombinationsFileUtil.nameToFN(this.mapNames,
                nameToLoad);
        Map<String, Integer> comboDict = null;
        try {
            comboDict = CombinationsFileUtil.loadComboData(actName);
        } catch (Exception e) {
            statusHandler.handle(Priority.SIGNIFICANT,
                    "Error loading combo file", e);
            comboDict = new HashMap<>();
        }
        zoneSelector.updateCombos(comboDict);
    }

    private Map<String, Integer> loadCombinationsFile(final String comboName) {
        List<List<String>> combolist = Collections.emptyList();
        try {
            Callable<List<List<String>>> loadTask = new Callable<List<List<String>>>() {

                @Override
                public List<List<String>> call() throws Exception {
                    return CombinationsFileUtil.init(comboName);
                }
            };

            Future<List<List<String>>> taskResult = asyncExecutor
                    .submit(loadTask);
            combolist = taskResult.get();
        } catch (ExecutionException e) {
            statusHandler.handle(Priority.SIGNIFICANT,
                    "Could not load combinations file " + comboName, e);
            return Collections.emptyMap();
        } catch (InterruptedException | RejectedExecutionException e) {
            /*
             * We should only ever fall into this block if the Composite is
             * disposing and the ExecutorService has been shutdown. Probably not
             * worth logging.
             */
            return Collections.emptyMap();
        }

        Map<String, Integer> dict = new HashMap<>();
        // reformat combinations into combo dictionary
        int group = 1;
        for (List<String> zonelist : combolist) {
            for (String z : zonelist) {
                dict.put(z, group);
            }
            group += 1;
        }

        return dict;
    }

    /**
     * load the color map file
     */
    private List<RGB> getColorsFromFile() {
        List<RGB> colors = new ArrayList<>();

        IPathManager pm = PathManagerFactory.getPathManager();
        File file = pm.getStaticFile(COLOR_MAP_FILE);
        try (BufferedReader in = new BufferedReader(new FileReader(file))) {
            String line = null;
            while ((line = in.readLine()) != null) {
                colors.add(RGBColors.getRGBColor(line));
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error reading file: " + file.getAbsolutePath(), e);
        }
        return colors;
    }

    private void comboFileChanged(FileUpdatedMessage message) {
        String comboName = LocalizationUtil.extractName(message.getFileName())
                .replace(".py", "");
        statusHandler
                .info("Received CombinationsFileChangedNotification for combinations file: "
                        + comboName);

        Map<String, Integer> comboDict = loadCombinationsFile(comboName);
        if (this.zoneSelector != null) {
            this.zoneSelector.updateCombos(comboDict);
        }
        applyButtonState(false);
    }

    @Override
    public void setStatusText(String significance, String message) {
        VizApp.runAsync(new Runnable() {
            @Override
            public void run() {
                callBack.updateStatus(significance, message);
            }
        });
    }

    @Override
    public void applyButtonState(final boolean enabled) {
        if ((this.applyZoneComboBtn != null)
                && !this.applyZoneComboBtn.isDisposed()) {
            VizApp.runAsync(new Runnable() {
                @Override
                public void run() {
                    ZoneCombinerComp.this.applyZoneComboBtn.setEnabled(enabled);
                }
            });
        }
    }

    private boolean buttonState() {
        final boolean[] state = { false };
        if ((this.applyZoneComboBtn != null)
                && !this.applyZoneComboBtn.isDisposed()) {
            VizApp.runSync(new Runnable() {
                @Override
                public void run() {
                    state[0] = ZoneCombinerComp.this.applyZoneComboBtn
                            .isEnabled();
                }
            });
        }

        return state[0];
    }
}
