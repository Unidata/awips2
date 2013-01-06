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
package com.raytheon.viz.gfe.dialogs;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

import jep.JepException;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MenuAdapter;
import org.eclipse.swt.events.MenuEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.GFERecord.GridType;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData.CoordinateType;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceData.RefType;
import com.raytheon.uf.common.dataplugin.gfe.reference.ReferenceID;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.viz.gfe.Activator;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.IParmManager;
import com.raytheon.viz.gfe.core.IReferenceSetManager;
import com.raytheon.viz.gfe.core.IReferenceSetManager.RefSetMode;
import com.raytheon.viz.gfe.core.msgs.IDisplayedParmListChangedListener;
import com.raytheon.viz.gfe.core.msgs.IEditAreaGroupInvChangedListener;
import com.raytheon.viz.gfe.core.msgs.IReferenceSetChangedListener;
import com.raytheon.viz.gfe.core.msgs.IReferenceSetIDChangedListener;
import com.raytheon.viz.gfe.core.msgs.IReferenceSetInvChangedListener;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.wxvalue.DiscreteWxValue;
import com.raytheon.viz.gfe.core.wxvalue.VectorWxValue;
import com.raytheon.viz.gfe.core.wxvalue.WeatherWxValue;
import com.raytheon.viz.gfe.core.wxvalue.WxValue;
import com.raytheon.viz.gfe.query.QueryFactory;
import com.raytheon.viz.gfe.query.QueryScript;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;
import com.raytheon.viz.ui.dialogs.ICloseCallback;
import com.raytheon.viz.ui.widgets.ToggleSelectList;
import com.vividsolutions.jts.geom.Envelope;
import com.vividsolutions.jts.geom.MultiPolygon;

/**
 * The edit area and query dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 11, 2008		       Eric Babin  Initial Creation
 * Jul 15, 2008            njensen     Hooked into backend/fixes
 * Oct 24, 2012 1287       rferrel     Code clean up for non-blocking dialog.
 * Oct 24, 2012 1287       rferrel     Changes for non-blocking SaveDeleteRefDialog.
 * Oct 24, 2012 1287       rferrel     Changes for non-blocking SaveDeleteEditAreaGroupDialog.
 * Oct 31, 2012 1298       rferrel     Changes for non-blocking MaskDialog.
 *                                      Changes for non-blocking WeatherDialog.
 *                                      Changes for non-blocking DiscreteDialog.
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

public class DefineRefSetDialog extends CaveJFACEDialog implements
        IReferenceSetChangedListener, IReferenceSetIDChangedListener,
        IReferenceSetInvChangedListener, IDisplayedParmListChangedListener,
        IEditAreaGroupInvChangedListener {
    private final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DefineRefSetDialog.class);

    private final int NUM_ITEMS = 13;

    private final int CLEAR_QUERY_ID = IDialogConstants.CLIENT_ID + 1;

    private final int RECALL_QUERY_ID = IDialogConstants.CLIENT_ID + 2;

    private final int UNDO_EDIT_AREA_ID = IDialogConstants.CLIENT_ID + 3;

    private final int CONVERT_TO_LOCATION_ID = IDialogConstants.CLIENT_ID + 4;

    private Composite top;

    private ToggleSelectList groupList;

    private List editAreasList;

    private List weatherElementsList;

    private Text queryField;

    private Text activeDisplay;

    private Menu menuBar;

    private Menu saveMenu, pickupMenu, wxdisMenu, createMaskMenu;

    private MenuItem saveMenuHeader, pickupMenuHeader, wxdisMenuHeader,
            createMaskMenuHeader, saveEditArea, deleteEditArea,
            saveEditAreaGroup, deleteEditAreaGroup;

    private Button undoButton;

    private Button recallQueryButton;

    private Button convertButton;

    private final String refOps[][] = { { "|", "Union" },
            { "&&", "Intersect" }, { "(", "" }, { ")", "" } };

    private final String parmOps[][] = { { "<", "Less than" },
            { ">", "Greater than" }, { "<=", "Less or equal" },
            { ">=", "Greater or equal" }, { "==", "Equal to" },
            { "!=", "Not equal to" },
            { "mask", "mask(Wx/Dis, query, isreg=0)" } };

    private final String labels[][] = { { "7", "" }, { "8", "" }, { "9", "" },
            { "4", "" }, { "5", "" }, { "6", "" }, { "1", "" }, { "2", "" },
            { "3", "" }, { "-", "" }, { "0", "" }, { ".", "" },
            { "BS", "BackSpace" }, { "", "" }, { "SP", "Space" }, };

    private IReferenceSetManager refSetMgr;

    private IParmManager parmManager;

    private DataManager dataManager;

    private String[] initialGroups;

    /**
     * Modal dialog from the menu so only one can be open at a time.
     */
    private CaveJFACEDialog menuModalDlg;
    private SaveDeleteEditAreaGroupDialog deleteGroupDlg;

    private SaveDeleteEditAreaGroupDialog saveGroupDlg;

    public DefineRefSetDialog(Shell parent, DataManager dataManager) {
        super(parent);
        this.setShellStyle(SWT.DIALOG_TRIM | SWT.MODELESS);
        this.dataManager = dataManager;
        this.refSetMgr = this.dataManager.getRefManager();
        this.parmManager = this.dataManager.getParmManager();

        this.initialGroups = Activator.getDefault().getPreferenceStore()
                .getStringArray("EditAreaGroups");
        if (this.initialGroups == null || this.initialGroups.length == 0) {
            this.initialGroups = new String[] { "Misc" };
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
        super.configureShell(shell);

        shell.setText("Edit Area and Query");
    }

    @Override
    protected Control createContents(Composite parent) {
        Control contents = super.createContents(parent);

        Point size = getInitialSize();
        getShell().setSize(size);
        getShell().setLocation(getInitialLocation(size));

        refreshRefsets();
        activeChanged();

        this.refSetMgr.addReferenceSetChangedListener(this);
        this.refSetMgr.addReferenceSetIDChangedListener(this);
        this.refSetMgr.addReferenceSetInvChangedListener(this);
        this.refSetMgr.addEditAreaGroupInvChangedListener(this);
        this.parmManager.addDisplayedParmListChangedListener(this);

        return contents;
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        this.top = (Composite) super.createDialogArea(parent);
        GridLayout layout = new GridLayout(6, false);
        this.top.setLayout(layout);

        initializeComponents();

        return this.top;
    }

    private void initializeComponents() {
        createMenus();
        createGroupList();
        createEditAreaList();
        createRefOpsButtons();
        createWeatherElements();
        createParmOpsButtons();
        createNumberButtons();
        createQueryComp();

        // initialize
        if (this.refSetMgr.getActiveRefSet().isQuery()) {
            this.queryField
                    .setText(this.refSetMgr.getActiveRefSet().getQuery());
        }
        this.activeDisplay.setText(getActiveRefDesc(this.refSetMgr
                .getActiveRefSet()));

        for (String group : initialGroups) {
            groupList.select(groupList.indexOf(group));
        }

        top.pack();
    }

    private void createQueryComp() {
        Composite comp = new Composite(this.top, SWT.NONE);
        GridData layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);
        layoutData.horizontalSpan = 5;
        comp.setLayout(new GridLayout(2, false));
        comp.setLayoutData(layoutData);

        Label query = new Label(comp, SWT.NONE);
        query.setText("Query");
        layoutData = new GridData(SWT.RIGHT, SWT.CENTER, false, false);
        query.setLayoutData(layoutData);

        this.queryField = new Text(comp, SWT.BORDER);
        layoutData = new GridData(SWT.FILL, SWT.TOP, true, false);
        this.queryField.setLayoutData(layoutData);

        Label activeArea = new Label(comp, SWT.NONE);
        layoutData = new GridData(SWT.RIGHT, SWT.CENTER, false, false);
        activeArea.setText("Active Edit Area");
        activeArea.setLayoutData(layoutData);

        this.activeDisplay = new Text(comp, SWT.BORDER | SWT.READ_ONLY);
        layoutData = new GridData(SWT.FILL, SWT.TOP, true, false);
        this.activeDisplay.setLayoutData(layoutData);

        comp.layout();
    }

    private void createRefOpsButtons() {
        Composite comp = new Composite(this.top, SWT.NONE);
        GridData layoutData = new GridData(SWT.DEFAULT, SWT.DEFAULT, false,
                false);
        comp.setLayout(new GridLayout());
        comp.setLayoutData(layoutData);

        layoutData = new GridData();
        for (int i = 0; i < this.refOps.length; i++) {
            layoutData = new GridData(30, 30);
            layoutData.horizontalAlignment = SWT.CENTER;
            final Button b = new Button(comp, SWT.PUSH);
            b.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    addToQueryField(((Button) event.widget).getText()
                            .replaceFirst("&&", "&") + " ");
                }
            });
            b.setLayoutData(layoutData);
            b.setText(this.refOps[i][0]);
            b.setToolTipText(this.refOps[i][1]);
        }
    }

    private void createGroupList() {
        Group groupFrame = new Group(this.top, SWT.BORDER);
        groupFrame.setLayout(new GridLayout());
        GridData layoutData = new GridData(GridData.FILL_BOTH);
        groupFrame.setLayoutData(layoutData);
        groupFrame.setText("Group Name(s)");

        this.groupList = new ToggleSelectList(groupFrame, SWT.V_SCROLL
                | SWT.MULTI | SWT.BORDER);
        this.groupList.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                refreshRefsets();
            }
        });
        Rectangle rect = groupList.computeTrim(0, 0,
                this.convertWidthInCharsToPixels(24), groupList.getItemHeight()
                        * NUM_ITEMS);
        layoutData = new GridData(GridData.FILL_BOTH);
        layoutData.minimumWidth = rect.width;
        layoutData.heightHint = rect.height;
        this.groupList.setLayoutData(layoutData);

        java.util.List<String> groupNames = refSetMgr.getGroupInventory();
        groupList.setItems(groupNames.toArray(new String[groupNames.size()]));
        groupList.add("Misc");
    }

    private void createEditAreaList() {
        Group groupFrame = new Group(this.top, SWT.BORDER);
        GridData layoutData = new GridData(GridData.FILL_BOTH);
        groupFrame.setLayout(new GridLayout());
        groupFrame.setLayoutData(layoutData);
        layoutData = new GridData(GridData.FILL_HORIZONTAL);
        layoutData.horizontalAlignment = SWT.CENTER;
        groupFrame.setText("Edit Areas");

        this.editAreasList = new List(groupFrame, SWT.V_SCROLL | SWT.SINGLE
                | SWT.BORDER);
        this.editAreasList.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent arg0) {
                if (editAreasList.getSelectionIndex() != -1) {
                    addToQueryField(editAreasList.getSelection()[0] + " ");
                }

            }
        });
        Rectangle rect = editAreasList.computeTrim(0, 0,
                this.convertWidthInCharsToPixels(24),
                editAreasList.getItemHeight() * NUM_ITEMS);
        layoutData = new GridData(GridData.FILL_BOTH);
        layoutData.minimumWidth = rect.width;
        layoutData.heightHint = rect.height;
        this.editAreasList.setLayoutData(layoutData);
    }

    private void createWeatherElements() {
        Group groupFrame = new Group(this.top, SWT.BORDER);
        groupFrame.setLayout(new GridLayout());
        GridData layoutData = new GridData(GridData.FILL_BOTH);
        groupFrame.setLayoutData(layoutData);
        groupFrame.setText("Weather Elements");

        this.weatherElementsList = new List(groupFrame, SWT.BORDER | SWT.SINGLE
                | SWT.V_SCROLL);

        refreshParms();
        this.weatherElementsList.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                if (weatherElementsList.getSelectionIndex() != -1) {
                    addToQueryField(weatherElementsList.getSelection()[0] + " ");
                }
            }
        });
        Rectangle rect = weatherElementsList.computeTrim(0, 0,
                this.convertWidthInCharsToPixels(24),
                weatherElementsList.getItemHeight() * NUM_ITEMS);
        layoutData = new GridData(GridData.FILL_BOTH);
        layoutData.minimumWidth = rect.width;
        layoutData.heightHint = rect.height;
        this.weatherElementsList.setLayoutData(layoutData);
    }

    /**
     * 
     */
    private void createParmOpsButtons() {
        Composite comp = new Composite(this.top, SWT.NONE);
        GridData layoutData = new GridData(SWT.DEFAULT, SWT.DEFAULT, false,
                false);
        comp.setLayout(new GridLayout());
        comp.setLayoutData(layoutData);

        layoutData = new GridData();
        for (int i = 0; i < this.parmOps.length; i++) {
            layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
            Button b = new Button(comp, SWT.PUSH);
            b.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent event) {
                    addToQueryField(((Button) event.widget).getText()
                            .replaceFirst("&&", "&") + " ");
                }
            });
            b.setLayoutData(layoutData);
            b.setText(this.parmOps[i][0]);
            b.setToolTipText(this.parmOps[i][1]);
        }
    }

    public void createNumberButtons() {
        Composite comp = new Composite(this.top, SWT.NONE);
        GridData layoutData = new GridData(SWT.DEFAULT, SWT.DEFAULT, false,
                false);
        comp.setLayout(new GridLayout(3, false));
        comp.setLayoutData(layoutData);

        for (int i = 0; i < this.labels.length; i++) {
            layoutData = new GridData(30, 30);
            if (this.labels[i][0].equalsIgnoreCase("")) {
                new Label(comp, SWT.NONE);
            } else {
                final Button b1 = new Button(comp, SWT.PUSH);
                b1.addSelectionListener(new SelectionAdapter() {
                    @Override
                    public void widgetSelected(SelectionEvent arg0) {
                        addToQueryField(b1.getText());
                    }
                });

                b1.setText(this.labels[i][0]);
                b1.setToolTipText(this.labels[i][1]);
                b1.setLayoutData(layoutData);
            }
        }
    }

    private void createMenus() {

        this.menuBar = new Menu(getShell(), SWT.BAR);

        this.saveMenuHeader = new MenuItem(this.menuBar, SWT.CASCADE);
        this.saveMenuHeader.setText("&Save/Delete");

        this.saveMenu = new Menu(getShell(), SWT.DROP_DOWN);
        this.saveMenuHeader.setMenu(this.saveMenu);

        this.saveEditArea = new MenuItem(this.saveMenu, SWT.DROP_DOWN);
        this.saveEditArea.setText("Save Edit Area...");

        this.saveEditArea.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent arg0) {
                saveAreaCB();
            }
        });

        this.deleteEditArea = new MenuItem(this.saveMenu, SWT.DROP_DOWN);
        this.deleteEditArea.setText("Delete Edit Area...");
        this.deleteEditArea.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent arg0) {
                deleteAreaCB();
            }
        });

        this.saveEditAreaGroup = new MenuItem(this.saveMenu, SWT.DROP_DOWN);
        this.saveEditAreaGroup.setText("Save Edit Area Group...");
        this.saveEditAreaGroup.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent arg0) {
                saveGroupCB();
            }
        });

        this.deleteEditAreaGroup = new MenuItem(this.saveMenu, SWT.DROP_DOWN);
        this.deleteEditAreaGroup.setText("Delete Edit Area Group...");
        this.deleteEditAreaGroup.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                deleteGroupCB();
            }
        });
        this.pickupMenuHeader = new MenuItem(this.menuBar, SWT.CASCADE);
        this.pickupMenuHeader.setText("&PickupValue");

        this.pickupMenu = new Menu(getShell(), SWT.DROP_DOWN);
        this.pickupMenuHeader.setMenu(this.pickupMenu);
        this.pickupMenu.addMenuListener(new MenuAdapter() {

            @Override
            public void menuShown(MenuEvent e) {
                pickPost();
            }
        });

        this.wxdisMenuHeader = new MenuItem(this.menuBar, SWT.CASCADE);
        this.wxdisMenuHeader.setText("&Wx/Dis Values");

        this.wxdisMenu = new Menu(getShell(), SWT.DROP_DOWN);
        this.wxdisMenuHeader.setMenu(this.wxdisMenu);
        this.wxdisMenu.addMenuListener(new MenuAdapter() {

            @Override
            public void menuShown(MenuEvent e) {
                wxPost();
            }
        });

        this.createMaskMenuHeader = new MenuItem(this.menuBar, SWT.CASCADE);
        this.createMaskMenuHeader.setText("&Create Mask");

        this.createMaskMenu = new Menu(getShell(), SWT.DROP_DOWN);
        this.createMaskMenuHeader.setMenu(this.createMaskMenu);
        this.createMaskMenu.addMenuListener(new MenuAdapter() {

            @Override
            public void menuShown(MenuEvent e) {
                maskPost();
            }
        });

        super.getShell().setMenuBar(this.menuBar);
    }

    private void buildWxDisMenu(Menu menu, SelectionAdapter cb, boolean filterWx) {
        for (MenuItem item : menu.getItems()) {
            item.dispose();
        }

        Parm[] parms = this.parmManager.getDisplayedParms();

        java.util.List<Parm> filteredParms = new ArrayList<Parm>();
        for (Parm parm : parms) {
            if (!filterWx
                    || parm.getGridInfo().getGridType()
                            .equals(GridType.WEATHER)
                    || parm.getGridInfo().getGridType()
                            .equals(GridType.DISCRETE)) {
                filteredParms.add(parm);
            }
        }

        Collections.sort(filteredParms, new Comparator<Parm>() {
            @Override
            public int compare(Parm o1, Parm o2) {
                return o1.expressionName().compareTo(o2.expressionName());
            }
        });

        for (Parm parm : filteredParms) {
            MenuItem item = new MenuItem(menu, SWT.CASCADE);
            item.setText(parm.expressionName());
            item.setData(parm);
            item.addSelectionListener(cb);
        }
    }

    protected void maskCB(Parm parm) {
        if (menuModalDlg == null || menuModalDlg.getShell() == null
                || menuModalDlg.isDisposed()) {
            menuModalDlg = new MaskDialog(this.getShell(), parm);
            menuModalDlg.setBlockOnOpen(false);
            menuModalDlg.setCloseCallback(new ICloseCallback() {

                @Override
                public void dialogClosed(Object returnValue) {
                    if (returnValue instanceof Integer) {
                        int status = (Integer) returnValue;
                        if (status != IDialogConstants.CANCEL_ID) {
                            MaskDialog mask = (MaskDialog) menuModalDlg;
                            addToQueryField(mask.mask());
                        }
                    }
                    menuModalDlg = null;
                }
            });
            menuModalDlg.open();
        } else {
            menuModalDlg.bringToTop();
        }
    }

    /**
     * 
     */
    protected void maskPost() {
        SelectionAdapter maskAdapter = new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                maskCB((Parm) event.widget.getData());
            }
        };

        buildWxDisMenu(this.createMaskMenu, maskAdapter, true);
    }

    /**
     * 
     */
    protected void wxPost() {
        SelectionAdapter wxAdapter = new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                wxCB((Parm) event.widget.getData());
            }
        };

        buildWxDisMenu(this.wxdisMenu, wxAdapter, true);
    }

    protected void wxCB(Parm parm) {
        if (parm.getGridInfo().getGridType().equals(GridType.WEATHER)) {
            if (menuModalDlg == null || menuModalDlg.getShell() == null
                    || menuModalDlg.isDisposed()) {
                menuModalDlg = new WeatherDialog(this.getShell(), parm);
                menuModalDlg.setBlockOnOpen(false);
                menuModalDlg.setCloseCallback(new ICloseCallback() {

                    @Override
                    public void dialogClosed(Object returnValue) {
                        if (returnValue instanceof Integer) {
                            int status = (Integer) returnValue;
                            if (status != IDialogConstants.CANCEL_ID) {
                                WeatherDialog d = (WeatherDialog) menuModalDlg;
                                addToQueryField('"' + ((WeatherWxValue) d
                                        .getWxValue()).getWeatherKey()
                                        .toString() + '"');
                            }
                        }
                        menuModalDlg = null;
                    }
                });
                menuModalDlg.open();
            } else {
                menuModalDlg.bringToTop();
            }
        } else {
            if (menuModalDlg == null || menuModalDlg.getShell() == null
                    || menuModalDlg.isDisposed()) {
                menuModalDlg = new DiscreteDialog(this.getShell(), parm);
                menuModalDlg.setBlockOnOpen(false);
                menuModalDlg.setCloseCallback(new ICloseCallback() {

                    @Override
                    public void dialogClosed(Object returnValue) {
                        if (returnValue instanceof Integer) {
                            int status = (Integer) returnValue;
                            if (status != IDialogConstants.CANCEL_ID) {
                                DiscreteDialog d = (DiscreteDialog) menuModalDlg;
                                addToQueryField('"' + ((DiscreteWxValue) d
                                        .getDiscreteValue()).getDiscreteKey()
                                        .toString() + '"');
                            }
                        }
                        menuModalDlg = null;
                    }
                });
                menuModalDlg.open();
            } else {
                menuModalDlg.bringToTop();
            }
        }
    }

    /**
     * 
     */
    protected void pickPost() {
        SelectionAdapter pickupAdapter = new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent event) {
                pickCB((Parm) event.widget.getData());
            }
        };
        buildWxDisMenu(this.pickupMenu, pickupAdapter, false);
    }

    protected void pickCB(Parm parm) {
        WxValue puv = parm.getParmState().getPickUpValue();
        String s;
        if (puv instanceof WeatherWxValue) {
            s = ('"' + ((WeatherWxValue) puv).getWeatherKey().toString() + '"');
        } else if (puv instanceof DiscreteWxValue) {
            s = ('"' + ((DiscreteWxValue) puv).getDiscreteKey().toString() + '"');
        } else if (puv instanceof VectorWxValue) {
            String fmt = "%." + parm.getGridInfo().getPrecision() + "f";
            s = String.format(fmt, ((VectorWxValue) puv).getMag());
        } else {
            s = puv.toString();
        }
        addToQueryField(s);
    }

    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        GridLayout layout = (GridLayout) parent.getLayout();
        layout.makeColumnsEqualWidth = false;
        GridData data = (GridData) parent.getLayoutData();
        data.horizontalAlignment = SWT.FILL;
        data.grabExcessHorizontalSpace = true;

        createButton(parent, CLEAR_QUERY_ID, "Clear Query", false);

        this.recallQueryButton = createButton(parent, RECALL_QUERY_ID,
                "Recall Query", false);
        this.recallQueryButton.setEnabled(this.refSetMgr.getHistoryStack()
                .size() > 0);

        this.undoButton = super.createButton(parent, UNDO_EDIT_AREA_ID,
                "Undo Edit Area", false);

        this.convertButton = super.createButton(parent, CONVERT_TO_LOCATION_ID,
                "Convert To Location", false);

        Button spacer = super.createButton(parent, 0, "    ", false);
        spacer.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
        spacer.setEnabled(false);
        spacer.setVisible(false);

        createButton(parent, IDialogConstants.OK_ID, "Submit", false);
        createButton(parent, IDialogConstants.CANCEL_ID, "Cancel", false);

        parent.pack();
    }

    @Override
    protected void buttonPressed(int buttonId) {

        switch (buttonId) {
        case CLEAR_QUERY_ID:
            clear();
            break;

        case RECALL_QUERY_ID:
            showRecallQueryList();
            break;

        case UNDO_EDIT_AREA_ID:
            undo();
            break;

        case CONVERT_TO_LOCATION_ID:
            convert();
            break;

        case IDialogConstants.OK_ID:
            submit();
            break;

        case IDialogConstants.CANCEL_ID:
            cancelPressed();
            break;

        default:
            statusHandler.handle(Priority.PROBLEM,
                    "Unknown button pressed. ID=" + buttonId);
        }
    }

    private void showRecallQueryList() {
        Menu menu = new Menu(getParentShell(), SWT.POP_UP);
        java.util.List<String> historyStack = this.refSetMgr.getHistoryStack();
        if (historyStack.size() != 0) {
            for (String query : historyStack) {
                final MenuItem item = new MenuItem(menu, SWT.PUSH);
                item.addSelectionListener(new SelectionAdapter() {
                    @Override
                    public void widgetSelected(SelectionEvent e) {
                        DefineRefSetDialog.this.queryField.setText(item
                                .getText());
                    }
                });
                item.setText(query);
            }
            menu.setVisible(true);
        }
    }

    @Override
    public boolean close() {
        // clean up the listeners...
        this.refSetMgr.removeEditAreaGroupInvChangedListener(this);
        this.refSetMgr.removeReferenceSetChangedListener(this);
        this.refSetMgr.removeReferenceSetIDChangedListener(this);
        this.refSetMgr.removeReferenceSetInvChangedListener(this);
        this.parmManager.removeDisplayedParmListChangedListener(this);

        return super.close();
    }

    /**
     * 
     */
    private void clear() {
        // LogStream.logUse('Clear Query')
        // Clear the query entry
        this.queryField.setText("");
    }

    private void undo() {
        // LogStream.logUse("Undo Edit Area")
        // Undo Edit Area
        this.refSetMgr.undoRefSet();
    }

    private void convert() {
        // LogStream.logUse("Convert To Location")
        // Convert the activeRefSet from a query to polygons
        ReferenceData refData = this.refSetMgr.getActiveRefSet();
        if (refData.isQuery()) {
            ReferenceData newRef = new ReferenceData(refData.getGloc(),
                    refData.getId(), refData.getGrid());
            this.refSetMgr.setActiveRefSet(newRef);
            // this.activeEditArea.setText(getActiveRefDesc(newRef));
        }
    }

    private void submit() {
        try {
            String s = this.queryField.getText().trim();
            QueryScript script = QueryFactory.getCachedScript(DataManager
                    .getCurrentInstance());
            HashMap<String, Object> argMap = new HashMap<String, Object>();
            argMap.put("expression", s);
            ReferenceData newRef = (ReferenceData) script.execute("evaluate",
                    argMap);

            this.activeDisplay.setText(s);
            this.refSetMgr.incomingRefSet(newRef, RefSetMode.USE_CURRENT);
            addToHistory(s);
            this.queryField.setText("");
        } catch (JepException e) {
            String errorMsg = e.getMessage();
            statusHandler.handle(Priority.PROBLEM,
                    "Syntax error in query string '" + errorMsg + "'", e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.viz.gfe.core.msgs.IReferenceSetChangedListener#
     * referenceSetChanged(com.raytheon.edex.plugin.gfe.reference.ReferenceData,
     * java.util.ArrayList)
     */
    @Override
    public void referenceSetChanged(ReferenceData refSet,
            ArrayList<Envelope> domains) {
        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                activeChanged();
            }
        });
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.viz.gfe.core.msgs.IReferenceSetIDChangedListener#
     * referenceSetIDChanged(com.raytheon.edex.plugin.gfe.reference.ReferenceID)
     */
    @Override
    public void referenceSetIDChanged(ReferenceID refID) {
        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                activeChanged();
            }
        });
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.viz.gfe.core.msgs.IReferenceSetInvChangedListener#
     * referenceSetInvChanged(java.util.List, java.util.List, java.util.List,
     * java.util.List)
     */
    @Override
    public void referenceSetInvChanged(java.util.List<ReferenceID> inventory,
            java.util.List<ReferenceID> additions,
            java.util.List<ReferenceID> deletions,
            java.util.List<ReferenceID> changes) {

        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                refreshRefsets();
            }
        });
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.viz.gfe.core.msgs.IEditAreaGroupInvChangedListener#
     * editAreaGroupInvChanged()
     */
    @Override
    public void editAreaGroupInvChanged() {
        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                refreshRefsets();
            }
        });
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.viz.gfe.core.msgs.IDisplayedParmListChangedListener#
     * displayedParmListChanged(com.raytheon.viz.gfe.core.parm.Parm[],
     * com.raytheon.viz.gfe.core.parm.Parm[],
     * com.raytheon.viz.gfe.core.parm.Parm[])
     */
    @Override
    public void displayedParmListChanged(Parm[] parms, Parm[] deletions,
            Parm[] additions) {
        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                refreshParms();
            }
        });
    }

    private void activeChanged() {
        // Update display when active refset changes
        ReferenceData refData = refSetMgr.getActiveRefSet();

        // Fix active refset display
        if (!activeDisplay.isDisposed()) {
            activeDisplay.setText(getActiveRefDesc(refData));
        }

        // Enable/Disable Undo button
        if (!undoButton.isDisposed()) {
            undoButton.setEnabled(!refData.refType().equals(RefType.NONE));
        }

        // Enable/Disable Convert to Location button
        if (!convertButton.isDisposed()) {
            convertButton.setEnabled(refData.isQuery());
        }
    }

    private String getActiveRefDesc(ReferenceData refData) {
        String s = "";

        if (refData.isQuery()) {
            s = refData.getId().getName() + ": {" + refData.getQuery() + "}";
        } else {
            MultiPolygon polygons = refData.getPolygons(CoordinateType.GRID);
            if (polygons == null || polygons.isEmpty()) {
                s = "Empty";
            } else {
                s = refData.getId().getName() + ": Polygons";
            }
        }
        return s;
    }

    protected String[] getAreaNames(String[] groupList) {
        Set<String> areaList = new HashSet<String>();
        for (String groupName : groupList) {
            areaList.addAll(refSetMgr.getGroupData(groupName));
        }

        String[] areas = areaList.toArray(new String[areaList.size()]);
        Arrays.sort(areas);
        return areas;
    }

    private String[] getParms() {
        Parm[] parms = this.parmManager.getDisplayedParms();
        ArrayList<String> we = new ArrayList<String>();

        we.add("Topo");
        for (Parm parm : parms) {
            String exprName;
            if (parm.getParmID().getDbId().getModelName()
                    .equalsIgnoreCase("Fcst")) {
                exprName = parm.getParmID().compositeNameUI();
            } else {
                exprName = parm.getParmID().getParmId().replaceAll(":", "_");
            }
            if (exprName.indexOf("Topo") != -1) {
                exprName = "Topo";
            }
            if (!we.contains(exprName)) {
                we.add(exprName);
            }
        }

        String[] weArray = we.toArray(new String[we.size()]);
        Arrays.sort(weArray);
        return weArray;
    }

    private void refreshRefsets() {
        if (groupList.isDisposed() || editAreasList.isDisposed()) {
            return;
        }

        String[] groups = groupList.getSelection();

        // Refresh the Group and Areas lists
        java.util.List<String> availGroups = refSetMgr.getGroupInventory();
        availGroups.add("Misc");
        groupList.setItems(availGroups.toArray(new String[availGroups.size()]));

        // update selection
        groupList.deselectAll();
        for (String group : groups) {
            int index = groupList.indexOf(group);
            if (index >= 0) {
                groupList.select(index);
            }
        }

        groups = groupList.getSelection();
        String[] areaNames = getAreaNames(groups);
        editAreasList.setItems(areaNames);
    }

    private void refreshParms() {
        this.weatherElementsList.removeAll();
        this.weatherElementsList.setItems(getParms());
    }

    private void saveAreaCB() {

        if (menuModalDlg == null || menuModalDlg.getShell() == null
                || menuModalDlg.isDisposed()) {
            menuModalDlg = new SaveDeleteRefDialog(getShell(), this.refSetMgr,
                    "Save");
            menuModalDlg.setBlockOnOpen(false);
            menuModalDlg.open();
        } else {
            menuModalDlg.bringToTop();
        }
    }

    private void deleteAreaCB() {

        if (menuModalDlg == null || menuModalDlg.getShell() == null
                || menuModalDlg.isDisposed()) {
            menuModalDlg = new SaveDeleteRefDialog(this.getShell(), refSetMgr,
                    "Delete");
            menuModalDlg.setBlockOnOpen(false);
            menuModalDlg.open();
        } else {
            menuModalDlg.bringToTop();
        }
    }

    private void saveGroupCB() {
        if (menuModalDlg == null || menuModalDlg.getShell() == null
                || menuModalDlg.isDisposed()) {
            menuModalDlg = new SaveDeleteEditAreaGroupDialog(getShell(),
                    this.refSetMgr, "Save");
            menuModalDlg.setBlockOnOpen(false);
            menuModalDlg.open();
        } else {
            menuModalDlg.bringToTop();
        }
    }

    private void deleteGroupCB() {

        if (menuModalDlg == null || menuModalDlg.getShell() == null
                || menuModalDlg.isDisposed()) {
            menuModalDlg = new SaveDeleteEditAreaGroupDialog(getShell(),
                    this.refSetMgr, "Delete");
            menuModalDlg.setBlockOnOpen(false);
            menuModalDlg.open();
        } else {
            menuModalDlg.bringToTop();
        }
    }

    private void addToQueryField(String s) {
        // get selection range
        Point sel = this.queryField.getSelection();

        // get text before and after selection
        String oldText = this.queryField.getText();
        String preText = oldText.substring(0, sel.x);
        String postText = oldText.substring(sel.y);

        // special case for BS and SP
        if (s.equals("BS")) {
            if (preText.length() > 0) {
                preText = preText.substring(0, preText.length() - 1);
            }
        } else if (s.equals("SP")) {
            preText += " ";
        }
        // otherwise just replace selection with new text
        else {
            preText += s;
        }
        this.queryField.setText(preText + postText);

        // position the caret at the end of the new text
        this.queryField.setFocus();
        this.queryField.setSelection(preText.length());
    }

    private void addToHistory(String s) {
        this.refSetMgr.pushHistoryStack(s);
        this.recallQueryButton.setEnabled(true);
    }

}
