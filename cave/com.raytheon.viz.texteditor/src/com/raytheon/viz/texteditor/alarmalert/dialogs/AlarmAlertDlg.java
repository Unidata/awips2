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
package com.raytheon.viz.texteditor.alarmalert.dialogs;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.ShellAdapter;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.dataplugin.text.alarms.AlarmAlertProduct;
import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.core.mode.CAVEMode;
import com.raytheon.viz.texteditor.alarmalert.dialogs.AlarmAlertSaveLoadDlg.SaveOrLoad;
import com.raytheon.viz.texteditor.alarmalert.util.AAPACombined;
import com.raytheon.viz.texteditor.alarmalert.util.AlarmAlertFunctions;
import com.raytheon.viz.texteditor.alarmalert.util.AlarmAlertLists;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.ui.dialogs.ICloseCallback;
import com.raytheon.viz.ui.dialogs.ModeListener;

/**
 * Create the alarm dialog for the alarm/alert functionality
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 9, 2009            mnash     Initial creation
 * Oct 31,2011  8510       rferrel     Cleaned up code made more robust
 * Sep 20,2011  1196       rferrel     Change dialogs so they do not block.
 * Mar 05,2013  15173   mgamazaychikov Set the initial location and dimension of 
 * 									   dialog as it is in A1.
 * Jun 23, 2014 #3161      lvenable    Added SWT dialog trim to the dialogs for thin client.
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

public class AlarmAlertDlg extends CaveSWTDialog {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AlarmAlertDlg.class);

    private Font font;

    private Composite shellComp;

    private List aaList;

    private List paList;

    private NewAlarmDlg proximityAlarmDlg = null;

    private static int siteAdminNumberAA = 0;

    private static int siteAdminNumberPA = 0;

    private static final String UNEDITABLE = "-- items above are uneditable --";

    private Button changeEntry;

    private Button deleteEntry;

    private Button saveAndCloseButton;

    private Button saveButton;

    private boolean newChanges = false;

    private Label label;

    private java.util.List<AlarmAlertProduct> aapList = new ArrayList<AlarmAlertProduct>();

    private java.util.List<AlarmAlertProduct> papList = new ArrayList<AlarmAlertProduct>();

    private static File currentFile;

    private static final int HEIGHT_HINT = 150;

    private static final int WIDTH_HINT = 500;

    private static Point shellLocation = null;

    private ILocalizationFileObserver listener = new ILocalizationFileObserver() {
        @Override
        public void fileUpdated(FileUpdatedMessage message) {
            VizApp.runAsync(new Runnable() {

                @Override
                public void run() {
                    if (!isDisposed()) {
                        populateLists();
                    }
                }
            });
        }
    };

    /**
     * @param parentShell
     */
    protected AlarmAlertDlg(Shell parentShell) {
        super(parentShell, SWT.DIALOG_TRIM | SWT.RESIZE,
                CAVE.PERSPECTIVE_INDEPENDENT | CAVE.DO_NOT_BLOCK);
        setText("(init) Alarm/Alert and Proximity Alarm Products");
    }

    @Override
    protected void disposed() {
        font.dispose();
    }

    // Opens the dialog without ever displaying it, doing all the initialization
    // necessary to load the alarm list and get it functioning.
    public void openInvisible() {
        Shell parent = getParent();

        shell = new Shell(parent, getStyle());

        shell.setText(getText());

        if (doesNotHaveAttribute(CAVE.MODE_INDEPENDENT)) {
            new ModeListener(shell);
        }

        // Create the main layout for the shell.
        shell.setLayout(constructShellLayout());
        shell.setLayoutData(constructShellLayoutData());
        shell.setVisible(false);

        // Initialize all of the controls and layouts
        initializeComponents(shell);

        // pack and open the dialog
        if (doesNotHaveAttribute(CAVE.NO_PACK)) {
            shell.pack();
            shell.setVisible(false);
        }

        shell.addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                disposed();
            }
        });

        preOpened();

        opened();

        setLocation();
    }

    /**
     * Sets the shell location.
     */
    private void setLocation() {
        int shellSizeX = getShell().getSize().x;
        int shellSizeY = getShell().getSize().y;
        Rectangle displayArea = shell.getDisplay().getClientArea();
        int locationX = displayArea.width - shellSizeX;
        int locationY = displayArea.y + shellSizeY;
        shellLocation = new Point(locationX, locationY);
        shell.setLocation(locationX, locationY);
        return;
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(false);

        // make a composite that covers the entire shell
        shellComp = new Composite(shell, SWT.NONE);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        shellComp.setLayout(constructShellLayout());
        shellComp.setLayoutData(gd);

        font = new Font(shell.getDisplay(), "Helvetica", 10, SWT.BOLD);

        // Initialize all of the controls and layouts
        initializeComponents();

        // Set the shell location
        if (shellLocation != null) {
            shell.setLocation(shellLocation);
        }

        shell.addShellListener(new ShellAdapter() {
            public void shellClosed(ShellEvent event) {
                forcedClose();
            }
        });
    }

    private void initializeComponents() {
        createMenus();
        createButtons();
        createAAList();
        createPAList();
        createAcknowledgeButtons();
        populateLists();
    }

    private void createMenus() {
        Menu menuBar = new Menu(shell, SWT.BAR);
        createFileMenus(menuBar);
        shell.setMenuBar(menuBar);
    }

    private void createFileMenus(Menu menuBar) {
        // -------------------------------------
        // Create all the items in the file menu
        // -------------------------------------
        MenuItem fileMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        fileMenuItem.setText("File");

        // Create the File menu item with a File "dropdown" menu
        Menu fileMenu = new Menu(menuBar);
        fileMenuItem.setMenu(fileMenu);

        MenuItem saveMenuItem = new MenuItem(fileMenu, SWT.NONE);
        saveMenuItem.setText("Save");
        saveMenuItem.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                save();
                changeSaveState(false);
            }
        });

        MenuItem saveCloseMenuItem = new MenuItem(fileMenu, SWT.NONE);
        saveCloseMenuItem.setText("Save && Close");
        saveCloseMenuItem.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                save();
                changeSaveState(false);
                closeDisplay();
            }
        });

        MenuItem closeMenuItem = new MenuItem(fileMenu, SWT.NONE);
        closeMenuItem.setText("Close");
        closeMenuItem.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                closeDisplay();
            }
        });

        @SuppressWarnings("unused")
        MenuItem separator = new MenuItem(fileMenu, SWT.SEPARATOR);

        MenuItem saveAsMenuItem = new MenuItem(fileMenu, SWT.NONE);
        saveAsMenuItem.setText("Save As...");
        saveAsMenuItem.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                // open dialog
                AlarmAlertSaveLoadDlg dialog = new AlarmAlertSaveLoadDlg(shell,
                        SaveOrLoad.SAVE);
                dialog.setCloseCallback(new ICloseCallback() {

                    @Override
                    public void dialogClosed(Object returnValue) {
                        doSaveAs(returnValue.toString());
                    }
                });
                dialog.open();
            }
        });

        MenuItem loadMenuItem = new MenuItem(fileMenu, SWT.NONE);
        loadMenuItem.setText("Load...");
        loadMenuItem.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                // open dialog
                AlarmAlertSaveLoadDlg dialog = new AlarmAlertSaveLoadDlg(shell,
                        SaveOrLoad.LOAD);
                dialog.setCloseCallback(new ICloseCallback() {

                    @Override
                    public void dialogClosed(Object returnValue) {
                        doLoad(returnValue.toString());
                    }
                });
                dialog.open();
            }
        });
    }

    private void doSaveAs(String fname) {
        fname = validateFileName(fname);
        // AlarmAlertFunctions.validateName(fname);

        if (fname != null && !fname.isEmpty()) {
            setShellText(fname);
            setLastFile(fname);
            // save alert products
            LocalizationFile lFile = AlarmAlertFunctions.getFile(
                    AlarmAlertFunctions.initUserLocalization(), fname);
            if (lFile != null) {
                AlarmAlertFunctions.saveAlarms(
                        aapList.subList(siteAdminNumberAA - 1, aapList.size()),
                        papList.subList(siteAdminNumberPA - 1, papList.size()),
                        lFile);
            } else {
                statusHandler.handle(Priority.SIGNIFICANT,
                        "Null localization file", new VizException(
                                "Null localization file"));
            }
        }
    }

    private void doLoad(String fname) {
        fname = validateFileName(fname);
        // AlarmAlertFunctions.validateName(fname);
        if (fname != null && !fname.isEmpty()) {

            // load alert products
            LocalizationFile lFile = AlarmAlertFunctions.getFile(
                    AlarmAlertFunctions.initUserLocalization(), fname);
            if (lFile != null) {
                File file = lFile.getFile();
                setShellText(fname);
                setLastFile(fname);
                currentFile = file;
                AAPACombined combined = null;

                try {
                    combined = AlarmAlertFunctions.loadFile(file);
                } catch (FileNotFoundException e) {
                    combined = AlarmAlertFunctions.createDefaultAAPACombined();
                }
                if (combined != null) {
                    populateLists(combined.getAaList(), combined.getPaList());
                }
            } else {
                statusHandler.handle(Priority.SIGNIFICANT,
                        "Null localization file", new VizException(
                                "Null localization file"));
            }
        }
    }

    /**
     * Create the buttons right below the menu
     */
    private void createButtons() {
        Composite entryButtons = new Composite(shellComp, SWT.NONE);
        GridLayout buttonLayout = new GridLayout(4, true);
        entryButtons.setLayout(buttonLayout);
        Button newAAEntry = new Button(entryButtons, SWT.PUSH);
        newAAEntry.setText("New AA Entry");

        newAAEntry.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                if (proximityAlarmDlg != null
                        && !proximityAlarmDlg.getShell().isDisposed()) {
                    proximityAlarmDlg.setDialogFocus();
                    return;
                }
                proximityAlarmDlg = new NewAlarmDlg(shell, "ALARM",
                        new AlarmAlertProduct(isOperationalMode()));
                proximityAlarmDlg.setCloseCallback(new ICloseCallback() {

                    @Override
                    public void dialogClosed(Object returnValue) {
                        AlarmAlertProduct prod = (AlarmAlertProduct) returnValue;
                        if (proximityAlarmDlg.haveOkEvent()) {
                            aapList.add(prod);
                            String string = "";
                            if (prod.isAlarm()) {
                                string += " (Alarm)";
                            }
                            if (!"".equals(prod.getSearchString())) {
                                string += " containing \""
                                        + prod.getSearchString() + "\"";
                            }
                            aaList.add(prod.getProductId() + string);
                            changeSaveState(true);
                        }
                    }
                });
                proximityAlarmDlg.open();
            }
        });

        Button newPAEntry = new Button(entryButtons, SWT.PUSH);
        newPAEntry.setText("New PA Entry");
        newPAEntry.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                if (proximityAlarmDlg != null
                        && !proximityAlarmDlg.getShell().isDisposed()) {
                    proximityAlarmDlg.setDialogFocus();
                    return;
                }

                proximityAlarmDlg = new NewAlarmDlg(shell, "PROXIMITY",
                        new AlarmAlertProduct(isOperationalMode()));
                proximityAlarmDlg.setCloseCallback(new ICloseCallback() {

                    @Override
                    public void dialogClosed(Object returnValue) {
                        AlarmAlertProduct prod = (AlarmAlertProduct) returnValue;
                        if (proximityAlarmDlg.haveOkEvent()) {
                            prod.setOperationalMode(isOperationalMode());
                            papList.add(prod);
                            paList.add(prod.getProductId() + " "
                                    + prod.getAlarmType() + " "
                                    + prod.getActionCmd() + " "
                                    + AlarmAlertFunctions.buildDistance(prod));
                            changeSaveState(true);
                        }
                    }
                });

                proximityAlarmDlg.open();
            }
        });

        changeEntry = new Button(entryButtons, SWT.PUSH);
        changeEntry.setText("Change Entry");
        changeEntry.setEnabled(false);
        changeEntry.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                // Ignore if dialog already being displayed.
                if (proximityAlarmDlg != null
                        && !proximityAlarmDlg.getShell().isDisposed()) {
                    proximityAlarmDlg.setDialogFocus();
                    return;
                }
                if (aaList.getSelectionIndex() != -1) {
                    // User might change selection while dialog is displayed.
                    final int index = aaList.getSelectionIndex();
                    proximityAlarmDlg = new NewAlarmDlg(shell, "ALARM", aapList
                            .get(index - 1));
                    proximityAlarmDlg.setCloseCallback(new ICloseCallback() {

                        @Override
                        public void dialogClosed(Object returnValue) {
                            AlarmAlertProduct prod = (AlarmAlertProduct) returnValue;
                            prod = aapList.set(index - 1, prod);
                            if (proximityAlarmDlg.haveOkEvent()) {
                                String string = "";
                                if (prod.isAlarm()) {
                                    string += " (Alarm)";
                                }
                                if (!"".equals(prod.getSearchString())) {
                                    string += " \"" + prod.getSearchString()
                                            + "\"";
                                }
                                aaList.setItem(index, prod.getProductId()
                                        + string);
                                paList.deselectAll();
                                aaList.select(index);
                                aaList.showSelection();
                                changeSaveState(true);
                            }
                        }
                    });
                    proximityAlarmDlg.open();
                } else if (paList.getSelectionIndex() != -1) {
                    // User might change selection while dialog is displayed.
                    final int index = paList.getSelectionIndex();
                    proximityAlarmDlg = new NewAlarmDlg(shell, "PROXIMITY",
                            papList.get(index - 1));
                    proximityAlarmDlg.setCloseCallback(new ICloseCallback() {

                        @Override
                        public void dialogClosed(Object returnValue) {
                            AlarmAlertProduct prod = (AlarmAlertProduct) returnValue;
                            prod = papList.set(index - 1, prod);
                            if (proximityAlarmDlg.haveOkEvent()) {
                                paList.setItem(
                                        paList.getSelectionIndex(),
                                        prod.getProductId()
                                                + " "
                                                + prod.getAlarmType()
                                                + " "
                                                + prod.getActionCmd()
                                                + " "
                                                + AlarmAlertFunctions
                                                        .buildDistance(prod));
                                aaList.deselectAll();
                                paList.select(index);
                                paList.showSelection();
                                changeSaveState(true);
                            }
                        }
                    });
                    proximityAlarmDlg.open();
                }
            }
        });

        deleteEntry = new Button(entryButtons, SWT.PUSH);
        deleteEntry.setText("Delete Entry");
        deleteEntry.setEnabled(false);
        deleteEntry.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                if (paList.getSelectionIndex() != -1) {
                    papList.remove(paList.getSelectionIndex() - 1);
                    paList.remove(paList.getSelectionIndices());
                } else if (aaList.getSelectionIndex() != -1) {
                    aapList.remove(aaList.getSelectionIndex() - 1);
                    aaList.remove(aaList.getSelectionIndices());
                }
                switchEntryButtons(false);
                changeSaveState(true);
            }
        });
    }

    /**
     * Creates the alarm alert list in the dialog
     */
    private void createAAList() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label alarmAlertLabel = new Label(shellComp, SWT.CENTER);
        alarmAlertLabel.setFont(font);
        alarmAlertLabel.setText("Alarm/Alert Products");
        alarmAlertLabel.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.widthHint = WIDTH_HINT;
        gd.heightHint = HEIGHT_HINT;
        aaList = new List(shellComp, SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL);
        aaList.setLayoutData(gd);
        aaList.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                int selected = aaList.getSelectionIndex();
                if (selected <= siteAdminNumberAA - 1) {
                    label.setText("Uneditable Entry (Added by Site Manager)");
                    aaList.deselect(selected);
                    switchEntryButtons(false);
                    paList.deselectAll();
                } else {
                    label.setText("");
                    switchEntryButtons(true);
                    paList.deselectAll();
                }
            }
        });
    }

    /**
     * Creates the Proximity Alarm list in the dialog
     */
    private void createPAList() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label proximityAlarmLabel = new Label(shellComp, SWT.CENTER);
        proximityAlarmLabel.setFont(font);
        proximityAlarmLabel
                .setText("Proximity Alarm Products\n(ProductID, Alarm/Alert, Action Name, Area Definition)");
        proximityAlarmLabel.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.widthHint = WIDTH_HINT;
        gd.heightHint = HEIGHT_HINT;
        paList = new List(shellComp, SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL);
        paList.setLayoutData(gd);
        paList.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                int selected = paList.getSelectionIndex();
                if (selected <= siteAdminNumberPA - 1) {
                    label.setText("Uneditable Entry (Added by Site Manager)");
                    paList.deselect(selected);
                    switchEntryButtons(false);
                    aaList.deselectAll();
                } else {
                    label.setText("");
                    switchEntryButtons(true);
                    aaList.deselectAll();
                }

            }
        });
    }

    /**
     * Creates the Save, Save & Close, and Close buttons in the dialog
     */
    private void createAcknowledgeButtons() {
        Composite ackButtons = new Composite(shellComp, SWT.NONE);
        GridLayout ackButtonLayout = new GridLayout(3, false);
        ackButtons.setLayout(ackButtonLayout);
        ackButtons.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true,
                false));

        GridData gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
        saveButton = new Button(ackButtons, SWT.PUSH);
        saveButton.setText("Save");
        saveButton.setLayoutData(gd);

        gd = new GridData(SWT.DEFAULT, SWT.DEFAULT, false, false);
        saveAndCloseButton = new Button(ackButtons, SWT.PUSH);
        saveAndCloseButton.setText("Save && Close");
        saveAndCloseButton.setLayoutData(gd);

        GridData gd2 = new GridData(SWT.RIGHT, SWT.DEFAULT, true, false);
        gd2.horizontalAlignment = GridData.END;
        Button closeButton = new Button(ackButtons, SWT.PUSH);
        closeButton.setText("Close");
        closeButton.setLayoutData(gd2);

        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        label = new Label(shellComp, SWT.BORDER);
        label.setText("");
        label.setLayoutData(gd);

        saveButton.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                save();
                changeSaveState(false);
            }
        });

        saveAndCloseButton.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                save();
                closeDisplay();
            }
        });

        closeButton.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                closeDisplay();
            }
        });
    }

    private void save() {
        changeSaveState(false);

        AlarmAlertLists lists = AlarmAlertLists.getInstance();
        lists.getFilteredProducts().clear();
        lists.getFilteredProducts().addAll(aapList);
        lists.getFilteredProducts().addAll(papList);

        LocalizationFile lFile = AlarmAlertFunctions.getFile(
                AlarmAlertFunctions.initUserLocalization(),
                currentFile.getName());

        AlarmAlertFunctions.saveAlarms(
                aapList.subList(siteAdminNumberAA - 1, aapList.size()),
                papList.subList(siteAdminNumberPA - 1, papList.size()), lFile);

    }

    private void populateLists() {
        populateLists(null, null);
    }

    /**
     * load local alerts from AAPACombined instance
     * 
     * @param local
     */
    private void populateFromCombined(AAPACombined local) {
        // set shell text
        setShellText(currentFile.getName());

        // clear gui lists
        aaList.removeAll();
        paList.removeAll();

        // clear current alarms
        java.util.List<AlarmAlertProduct> filteredAlarms = AlarmAlertLists
                .getInstance().getFilteredProducts();
        filteredAlarms.clear();

        AAPACombined site = AlarmAlertFunctions.loadSiteAlarms(listener);

        // populate the Alert/Alarm list
        try {
            // clear current list
            aapList.clear();
            // add site alarms
            aapList.addAll(site.getAaList());
            siteAdminNumberAA = aapList.size() + 1;
            StringBuilder sb = new StringBuilder();
            // add site alarms to gui list
            for (AlarmAlertProduct prod : aapList) {
                sb.setLength(0);
                sb.append(prod.getProductId());
                if (prod.isAlarm()) {
                    sb.append(" (Alarm)");
                }
                if (!"".equals(prod.getSearchString())) {
                    sb.append(" \"").append(prod.getSearchString())
                            .append("\"");
                }
                aaList.add(sb.toString());
            }
            // add separator
            aaList.add(UNEDITABLE);

            // add user alarms and add user alarms to gui list
            for (AlarmAlertProduct prod : local.getAaList()) {
                aapList.add(prod);
                sb.setLength(0);
                sb.append(prod.getProductId());
                if (prod.isAlarm()) {
                    sb.append(" (Alarm)");
                }
                if (!"".equals(prod.getSearchString())) {
                    sb.append(" containing '").append(prod.getSearchString())
                            .append("'");
                }
                aaList.add(sb.toString());
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.EVENTB, "Could not retrieve alarms",
                    e);
        }

        // populate the Proximity Alarm list
        try {
            // load site alarms
            papList.clear();
            papList.addAll(site.getPaList());
            siteAdminNumberPA = papList.size() + 1;
            for (AlarmAlertProduct prod : papList) {
                paList.add(prod.getProductId() + " " + prod.getAlarmType()
                        + " " + prod.getActionCmd() + " "
                        + AlarmAlertFunctions.buildDistance(prod));
            }
            // add separator
            paList.add(UNEDITABLE);

            // add user alarms and load into gui list
            for (AlarmAlertProduct prod : local.getPaList()) {
                papList.add(prod);
                // if (!"".equals(papList.get(i).getProductId())) {
                paList.add(prod.getProductId() + " " + prod.getAlarmType()
                        + " " + prod.getActionCmd() + " "
                        + AlarmAlertFunctions.buildDistance(prod));
                // }
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.EVENTB, "Could not retrieve alarms",
                    e);
            populateLists();
        }

        // add to AlarmAlertLists instance
        filteredAlarms.addAll(aapList);
        filteredAlarms.addAll(papList);
    }

    /**
     * 
     */
    private void populateLists(java.util.List<AlarmAlertProduct> aapLocal,
            java.util.List<AlarmAlertProduct> papLocal) {

        // if both arguments are null load the default file
        AAPACombined localCombined;
        if (aapLocal == null && papLocal == null) {
            // load last list if it exists
            localCombined = loadLastList();
        } else {
            localCombined = new AAPACombined();
            localCombined.setAaList(aapLocal);
            localCombined.setPaList(papLocal);
        }
        populateFromCombined(localCombined);

    }

    /**
     * Changes the Change Entry and Delete Entry button to whatever it needs to
     * be based on what was selected or clicked
     * 
     * @param offOn
     */
    private void switchEntryButtons(boolean offOn) {
        changeEntry.setEnabled(offOn);
        deleteEntry.setEnabled(offOn);
    }

    /**
     * Change save state, false if save is not needed, true if there are changes
     * to save
     * 
     * @param newValue
     */
    public void changeSaveState(boolean newValue) {
        newChanges = newValue;
    }

    private void forcedClose() {
        if (newChanges) {
            MessageDialog dlg = new MessageDialog(shell, "Save Changes?", null,
                    "There are unsaved changes,\nSave or Discard?",
                    MessageDialog.QUESTION, new String[] { "Save", "Discard" },
                    1);
            int ans = dlg.open();
            if (ans == 0) {
                // if save
                save();
                changeSaveState(false);
            }
        }
        shell.dispose();
    }

    public void closeDisplay() {
        if (newChanges) {
            MessageDialog dlg = new MessageDialog(shell, "Save Changes?", null,
                    "There are unsaved changes,\nSave or Discard?",
                    MessageDialog.QUESTION, new String[] { "Save", "Discard",
                            "Cancel" }, 2);
            int ans = dlg.open();
            if (ans == 0) {
                // if save
                save();
                changeSaveState(false);
            } else if (ans == 2 || ans == -1) {
                // if cancel do not dispose
                return;
            }
        }
        shell.dispose();
    }

    public void setDialogFocus() {
        shell.setActive();
    }

    /*
     * public boolean compareLists(java.util.List<AlarmAlertProduct> prev,
     * java.util.List<AlarmAlertProduct> curr) { boolean isTrue = true; for (int
     * i = 0; i < curr.size(); i++) { if (!prev.isEmpty() && prev.get(i) != null
     * && !prev.get(i).equals(curr.get(i))) { isTrue = false; break; } } return
     * isTrue; }
     */

    private boolean isOperationalMode() {
        CAVEMode mode = CAVEMode.getMode();
        return (CAVEMode.OPERATIONAL.equals(mode) || CAVEMode.TEST.equals(mode) ? true
                : false);
    }

    private AAPACombined loadLastList() {
        LocalizationFile lFile = AlarmAlertFunctions.getCurrentFileFromConfig();
        // lFile might be null, need to use the default in that case
        if (lFile == null) {
            setLastFile("default.xml");
            return loadLastList();
        }

        // if lFile is not null get the File from the LocalizationFile
        File file = lFile.getFile();
        AlarmAlertDlg.currentFile = file;
        // if currentFile is null use default.xml
        // else if currentFile does not exists and is not default.xml use
        // default.xml
        // else load normally ( non-existant file will be created and return and
        // empty lists of alarms )
        if (AlarmAlertDlg.currentFile == null) {
            setLastFile("default.xml");
            return loadLastList();
        } else if (!AlarmAlertDlg.currentFile.exists()
                && !AlarmAlertDlg.currentFile.getName().equals("default.xml")) {
            setLastFile("default.xml");
            return loadLastList();
        } else {
            // setShellText(currentFile.getName());
            return AlarmAlertFunctions.loadUserAlarmsFromConfig();
        }
    }

    private static void setLastFile(String filename) {
        AlarmAlertFunctions.setLastFileInConfig(filename);
    }

    private static String validateFileName(String fname) {
        if (!fname.isEmpty() && !fname.endsWith(".xml")) {
            return fname + ".xml";
        }
        return fname;
    }

    private void setShellText(String fname) {
        shell.setText("( " + fname
                + " ) Alarm/Alert and Proximity Alarm Products");
    }
}
