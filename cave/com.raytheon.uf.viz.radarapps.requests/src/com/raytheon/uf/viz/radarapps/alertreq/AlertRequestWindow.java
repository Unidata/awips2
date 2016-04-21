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
package com.raytheon.uf.viz.radarapps.alertreq;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.databinding.observable.list.WritableList;
import org.eclipse.jface.databinding.viewers.ObservableListContentProvider;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.CheckboxCellEditor;
import org.eclipse.jface.viewers.ColumnViewer;
import org.eclipse.jface.viewers.ComboBoxViewerCellEditor;
import org.eclipse.jface.viewers.EditingSupport;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.rcm.alertreq.AlertRequestDefinition;
import com.raytheon.rcm.alertreq.AlertRequestDefinition.Threshold;
import com.raytheon.rcm.config.RadarConfig;
import com.raytheon.rcm.config.RadarType;
import com.raytheon.rcm.config.RcmUtil;
import com.raytheon.rcm.message.AlertAdaptationParameters;
import com.raytheon.rcm.message.AlertAdaptationParameters.ParameterSpec;
import com.raytheon.rcm.message.MessageFormatException;
import com.raytheon.rcm.mqsrvr.ReplyObj.ConfigReply;
import com.raytheon.rcm.mqsrvr.ReplyObj.ROStatus;
import com.raytheon.rcm.mqsrvr.ReplyObj.StatusMessagesReply;
import com.raytheon.rcm.mqsrvr.ReqObj;
import com.raytheon.rcm.mqsrvr.ReqObj.GetRadarStatusMessages;
import com.raytheon.rcm.mqsrvr.ReqObj.SendAlertRequest;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.radarapps.alertreq.impl.AlertCategory;
import com.raytheon.uf.viz.radarapps.alertreq.impl.AlertRequestDoc;
import com.raytheon.uf.viz.radarapps.alertreq.impl.AlertRequestEditing;
import com.raytheon.uf.viz.radarapps.alertreq.impl.AlertRequestEditing.Sel;
import com.raytheon.uf.viz.radarapps.core.RadarApps;
import com.raytheon.uf.viz.radarapps.products.ui.ExtProductsUI;
import com.raytheon.viz.ui.EditorUtil;

/**
 * A window for alert requests.
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 *  Date         Ticket#    Engineer    Description
 *  ------------ ---------- ----------- --------------------------
 *  2015-06-10   4498       nabowle     Rename Util->RcmUtil
 * </pre>
 * 
 */
public class AlertRequestWindow extends Window {
    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(AlertRequestWindow.class);

    ExtProductsUI rpgSelector;

    AlertRequestDefinition[] alertRequests = new AlertRequestDefinition[2];

    int currentAlertAreaIndex = 0;

    HashMap<Integer, ParameterSpec> aapInfo = new HashMap<Integer, ParameterSpec>();

    private Button aa1Button;

    private Button aa2Button;

    TableViewer elementViewer;

    private WritableList getObservableList() {
        AlertRequestDoc ard = getAlertRequest();
        if (ard != null)
            return ard.getThresholds();
        else
            return new WritableList();
    }

    private AlertRequestDoc currentAlertRequestDoc;

    /*
     * Cannot use the controls to determine the current radar/area during and
     * event handler for those controls -- The value has already changed.
     */
    private String _currentRadarID;

    private int _currentAreaIndex;

    private AlertRequestDoc getAlertRequest() {
        return currentAlertRequestDoc;
    }

    public AlertRequestWindow(Shell parentShell) {
        super(parentShell);
        setShellStyle(getShellStyle()
                & ~(SWT.APPLICATION_MODAL | SWT.PRIMARY_MODAL | SWT.RESIZE));
        setBlockOnOpen(false);
    }

    @Override
    protected void handleShellCloseEvent() {
        onExit();
    }

    @Override
    protected void configureShell(Shell newShell) {
        super.configureShell(newShell);
        newShell.setText("Alert Request");
    }

    @Override
    protected Control createContents(Composite parent) {
        Composite c = new Composite(parent, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        c.setLayout(gl);

        Composite row;
        RowLayout rl;
        Button b;

        row = new Composite(c, SWT.NONE);
        rl = new RowLayout(SWT.HORIZONTAL);
        rl.center = true;
        row.setLayout(rl);
        fillRow(row);
        b = createButton(row, "Add", SWT.PUSH);
        b.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                onAdd();
            }
        });
        b = createButton(row, "Remove", SWT.PUSH);
        b.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                onRemove();
            }
        });
        rpgSelector = new ExtProductsUI(row);
        rpgSelector.getSelectionProvider().addSelectionChangedListener(
                new ISelectionChangedListener() {
                    public void selectionChanged(SelectionChangedEvent event) {
                        onRpgSelected();
                    }
                });
        rpgSelector.addFilter(new ViewerFilter() {
            public boolean select(Viewer viewer, Object parentElement,
                    Object element) {
                if (element instanceof RadarConfig) {
                    RadarConfig rc = (RadarConfig) element;
                    return rc.isDedicated()
                            && RcmUtil.getRadarType(rc) == RadarType.WSR;
                } else
                    return false;
            }
        });
        Label l = new Label(row, SWT.LEFT);
        l.setText("Clear");
        final Button clearButton = new Button(row, SWT.ARROW | SWT.DOWN);
        final Menu m = new Menu(getShell(), SWT.POP_UP);
        MenuItem mi;
        mi = new MenuItem(m, SWT.PUSH);
        mi.setText("Products");
        mi.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                onClearProducts();
            }
        });
        mi = new MenuItem(m, SWT.PUSH);
        mi.setText("Alert Area");
        mi.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                onClearAlertArea();
            }
        });
        mi = new MenuItem(m, SWT.PUSH);
        mi.setText("Global Clear");
        mi.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                onGlobalClear();
            }
        });
        clearButton.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                Point sz = clearButton.getSize();
                Point p = clearButton.toDisplay(0, sz.y);
                m.setLocation(p);
                m.setVisible(true);
            }
        });

        fillRow(new Label(c, SWT.SEPARATOR | SWT.HORIZONTAL));

        row = new Composite(c, SWT.NONE);
        fillRow(row);
        rl = new RowLayout(SWT.HORIZONTAL);
        rl.spacing = 40;
        row.setLayout(rl);
        aa1Button = createButton(row, "Alert Area #1", SWT.RADIO);
        aa2Button = createButton(row, "Alert Area #2", SWT.RADIO);
        SelectionAdapter sa = new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                onAreaIndexSelected();
            }
        };
        aa1Button.setSelection(true);
        aa1Button.addSelectionListener(sa);
        aa2Button.addSelectionListener(sa);

        TableViewer tv = new TableViewer(c, SWT.SINGLE | SWT.H_SCROLL
                | SWT.V_SCROLL | SWT.BORDER | SWT.FULL_SELECTION);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 430; // TODO: ..
        gd.heightHint = 200;
        tv.getControl().setLayoutData(gd);

        ElementLabelProvider elementLabelProvider = new ElementLabelProvider();

        for (int i = 0; i < ElementLabelProvider.N_PROPERTIES; ++i) {
            TableViewerColumn column = new TableViewerColumn(tv, SWT.NONE);
            column.getColumn().setText(columnTitles[i]);
            column.getColumn().setWidth(columnWidths[i] * 10);
            column.getColumn().setResizable(true);
            column.setEditingSupport(elementLabelProvider.getEditorSupport(tv,
                    i));
        }
        tv.setLabelProvider(elementLabelProvider);
        tv.setContentProvider(new ObservableListContentProvider());
        tv.setInput(getObservableList());
        tv.getTable().setHeaderVisible(true);
        tv.getTable().setLinesVisible(true);
        elementViewer = tv;

        row = new Composite(c, SWT.NONE);
        fillRow(row);
        row.setLayout(new RowLayout(SWT.HORIZONTAL));
        b = createButton(row, "Send Request", SWT.PUSH);
        b.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                onSendRequest();
            }
        });
        b = createButton(row, "Revert", SWT.PUSH);
        b.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                onRevert();
            }
        });
        b = createButton(row, "Load/Edit Area", SWT.PUSH);
        b.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                onLoadArea();
            }
        });
        b = createButton(row, "Exit", SWT.PUSH);
        b.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                onExit();
            }
        });

        getShell().getDisplay().asyncExec(new Runnable() {
            public void run() {
                rpgSelector.selectDefaultRpg();
            }
        });

        return c;
    }

    private Button createButton(Composite parent, String string, int flags) {
        Button button = new Button(parent, flags);
        button.setText(string);
        return button;
    }

    private void fillRow(Control c) {
        c.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, false));
    }

    private void onExit() {
        AlertRequestEditing ae = AlertRequestEditing.getInstance();
        Map<Sel, AlertRequestDoc> docs = ae.getDocuments();

        boolean anyModified = false;
        for (AlertRequestDoc doc : docs.values()) {
            if (doc.isModified()) {
                anyModified = true;
                break;
            }
        }

        if (anyModified) {
            String message = "There are modified alert requests for some radars. "
                    + "Do you want to send them before exiting?\n\n"
                    + "\t\"Cancel\" to return to the editor.\n"
                    + "\t\"Exit\" to exit, losing any modifications.\n"
                    + "\t\"Send\" to send modified requests and exit.";
            String[] labels = { "Cancel", "Exit", "Send Requests" };
            MessageDialog dlg = new MessageDialog(getShell(), "Confirm Exit",
                    (Image) null, message, MessageDialog.WARNING, labels, 2);
            boolean send = false;
            switch (dlg.open()) {
            case 0: // Cancel
                return;
            case 1: // Exit
                // Revert all modified below.
                break;
            case 2: // Send Requests
                send = true;
                break;
            }

            for (Map.Entry<Sel, AlertRequestDoc> sel : docs.entrySet()) {
                AlertRequestDoc doc = sel.getValue();
                if (doc.isModified()) {
                    if (send) {
                        SendAlertRequest r = new SendAlertRequest();
                        r.radarID = sel.getKey().getRadarID();
                        r.areaIndex = sel.getKey().getAreaIndex();
                        r.alertRequest = sel.getValue()
                                .toAlertRequestDefinition();

                        // If error, return without closing the window
                        if (RadarApps.getRcmSystem().sendCheckedAndHandled(r,
                                getShell()) == null)
                            return;
                    }
                    ae.clearDocument(sel.getKey().getRadarID(), sel.getKey()
                            .getAreaIndex());
                }
            }

        }

        // Remove any alert area editors (from the main panel.)
        for (int i = 1; i <= 2; ++i) {
            AlertAreaLayer aal = findAlertAreaLayer(i);
            if (aal != null) {
                aal.unload();
                aal.disposeInternal();
            }
        }

        close();
    }

    private void onRpgSelected() {
        commit();

        aapInfo.clear();

        String radarID = rpgSelector.getSelectedRPG();
        GetRadarStatusMessages req = new GetRadarStatusMessages();
        req.radarID = radarID;
        StatusMessagesReply reply = (StatusMessagesReply) RadarApps
                .getRcmSystem().sendCheckedAndHandled(req, getShell());
        if (reply != null) {
            ROStatus status = reply.status.iterator().next();
            byte[] msg = status.currentAAP != null ? status.currentAAP
                    : status.lastAAP;
            if (msg != null)
                try {
                    ParameterSpec[] psArray = AlertAdaptationParameters
                            .decode(msg);
                    for (ParameterSpec ps : psArray)
                        aapInfo.put(ps.category, ps);
                } catch (MessageFormatException e) {
                    // nothing
                }
        }

        // TODO: warn if no AAP info

        loadDoc();
        /*
         * If it is already loaded, make the opposite alert area layer display
         * the same RPG.
         */
        doLoadArea(getAreaIndex() == 1 ? 2 : 1, false);
    }

    private void onAreaIndexSelected() {
        commit();
        loadDoc();
    }

    private void loadDoc() {
        _currentRadarID = rpgSelector.getSelectedRPG();
        _currentAreaIndex = getAreaIndex();
        currentAlertRequestDoc = AlertRequestEditing.getInstance().getDocument(
                _currentRadarID, _currentAreaIndex, getShell());
        elementViewer.setInput(getObservableList());
        doLoadArea(getAreaIndex(), true);
    }

    private void commit() {
        // Remove lines with category of "(unset)"
        List<Threshold> l = getObservableList();
        for (int i = 0; i < l.size(); ++i) {
            Threshold t = l.get(i);
            if (!isValidCategory(t.category)) {
                l.remove(i);
                --i;
            }
        }
    }

    protected void onRemove() {
        IStructuredSelection sel = (IStructuredSelection) elementViewer
                .getSelection();
        if (sel.size() > 0)
            getObservableList().removeAll(sel.toList());
    }

    protected void onAdd() {
        Threshold e = new Threshold();
        getObservableList().add(e);
        elementViewer.setSelection(new StructuredSelection(e));
        elementViewer.editElement(e, ElementLabelProvider.CATEGORY);
    }

    private void onClearProducts() {
        if (confirm("Confirm Clear Products", String.format(
                "Clear the list of threshold for area %d of %s?",
                getAreaIndex(), rpgSelector.getSelectedRPG().toUpperCase()))) {
            getObservableList().clear();
        }
    }

    private void onClearAlertArea() {
        if (confirm("Confirm Clear Area", String.format(
                "Clear the alert area for area %d of %s?", getAreaIndex(),
                rpgSelector.getSelectedRPG().toUpperCase()))) {
            AlertRequestDoc doc = getAlertRequest();
            doc.getBoxBits().clear();
            doc.handleBoxBitsChanged();
        }
    }

    private void onGlobalClear() {
        String msg = "Do you want to clear all category/threshold and alert area "
                + "settings for all radars?  You will not be able to undo "
                + "or revert to the current settings.";
        if (confirm("Confirm Global Clear", msg)) {
            ConfigReply cr = (ConfigReply) RadarApps.getRcmSystem()
                    .sendCheckedAndHandled(new ReqObj.GetRadarConfig(),
                            getShell());
            if (cr != null) {
                for (RadarConfig rc : cr.config) {
                    /*
                     * TODO: Should clear non-dedicated too, but
                     * AlertRequestManager.sendAlertRequest would return an
                     * error.
                     */
                    if (rc.isDedicated()) {
                        for (int aai = 1; aai <= 2; ++aai) {
                            AlertRequestEditing.getInstance().clearDocument(
                                    rc.getRadarID(), aai);
                            SendAlertRequest req = new SendAlertRequest();
                            req.radarID = rc.getRadarID();
                            req.areaIndex = aai;
                            req.alertRequest = null;
                            RadarApps.getRcmSystem().sendCheckedAndHandled(req,
                                    getShell());
                        }
                    }
                }
            }

        }
    }

    private AlertAreaLayer findAlertAreaLayer(int areaIndex) {
        IDisplayPaneContainer container = EditorUtil.getActiveVizContainer();
        if (container != null) {
            List<AlertAreaLayer> layers = container.getActiveDisplayPane()
                    .getDescriptor().getResourceList()
                    .getResourcesByTypeAsType(AlertAreaLayer.class);
            for (AlertAreaLayer layer : layers) {
                if (layer.getResourceData().getAreaIndex() == areaIndex) {
                    return layer;
                }
            }
        }

        return null;
    }

    private void doLoadArea(int areaIndex, boolean create) {
        AlertAreaLayer aal = findAlertAreaLayer(areaIndex);
        if (aal == null && create) {
            IDisplayPaneContainer container = EditorUtil
                    .getActiveVizContainer();
            if (container != null) {
                IDescriptor desc = container.getActiveDisplayPane()
                        .getDescriptor();
                AlertAreaResourceData rd = new AlertAreaResourceData(areaIndex);
                try {
                    LoadProperties loadProps = new LoadProperties();
                    if (areaIndex == 2) {
                        ColorableCapability cc = new ColorableCapability();
                        cc.setColor(new RGB(255, 127, 80));
                        loadProps.getCapabilities().addCapability(cc);
                    }
                    aal = rd.construct(loadProps, desc);
                    desc.getResourceList().add(aal);
                } catch (VizException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            "Error loading alert area layer", e);
                    return;
                }
            }
        }

        if (aal != null) {
            AlertAreaLayer other = findAlertAreaLayer(areaIndex == 1 ? 2 : 1);
            if (other != null)
                other.setEditable(false);
            aal.setEditable(true);
            aal.setRPG(rpgSelector.getSelectedRPG());
            aal.setAlertRequest(AlertRequestEditing.getInstance().getDocument(
                    rpgSelector.getSelectedRPG(), areaIndex, getShell()));
        }
    }

    private void onLoadArea() {
        doLoadArea(getAreaIndex(), true);
    }

    private void onRevert() {
        String radarID = rpgSelector.getSelectedRPG();
        int areaIndex = getAreaIndex();
        if (confirm("Confirm Revert", String.format(
                "Revert the previously sent alert request for area %d of %s?",
                areaIndex, radarID.toUpperCase()))) {
            doRevert();
        }
    }

    private void doRevert() {
        String radarID = rpgSelector.getSelectedRPG();
        int areaIndex = getAreaIndex();

        AlertRequestEditing.getInstance().clearDocument(radarID, areaIndex);
        loadDoc();
    }

    private boolean confirm(String title, String prompt) {
        MessageBox mb = new MessageBox(getShell(), SWT.ICON_QUESTION | SWT.YES
                | SWT.NO | SWT.PRIMARY_MODAL);
        mb.setText(title);
        mb.setMessage(prompt);
        return mb.open() == SWT.YES;
    }

    private int getAreaIndex() {
        return aa2Button.getSelection() ? 2 : 1;
    }

    private void onSendRequest() {
        commit();

        String radarID = rpgSelector.getSelectedRPG();

        if (radarID == null || getAlertRequest() == null) {
            MessageBox mb = new MessageBox(getShell(), SWT.ICON_INFORMATION
                    | SWT.OK);
            mb.setText("Send Request");
            mb.setMessage("Please select a RPG.");
            mb.open();
            return;
        }

        SendAlertRequest r = new SendAlertRequest();
        r.radarID = radarID;
        r.areaIndex = getAreaIndex();
        r.alertRequest = getAlertRequest().toAlertRequestDefinition();

        if (RadarApps.getRcmSystem().sendCheckedAndHandled(r, getShell()) != null) {
            /*
             * If the request was successful, we should be able to reload from
             * RadarServer.
             */
            doRevert();
        }
    }

    private boolean isValidCategory(int category) {
        return category > 0;
    }

    private static final String[] columnTitles = { "Category", "Threshold",
            "Request Product" };

    private static final int[] columnWidths = { 21, 12, 5 };

    private class ElementLabelProvider implements ITableLabelProvider {

        public static final int CATEGORY = 0;

        public static final int THRESHOLD = 1;

        public static final int REQUEST_PRODUCT = 2;

        public static final int N_PROPERTIES = 3;

        @Override
        public Image getColumnImage(Object element, int columnIndex) {
            return null;
        }

        String getCategoryLabel(int category) {
            if (category != 0) {
                AlertCategory ac = AlertCategory.getAlertCategory(category);
                if (ac != null)
                    return ac.getName();
                else
                    return Integer.toString(category);
            } else
                return "(unset)";
        }

        String getThresholdLabel(int category, int threshold) {
            if (isValidCategory(category) && aapInfo != null) {
                AlertCategory ac = AlertCategory.getAlertCategory(category);
                ParameterSpec ps = aapInfo.get(ac.id);
                if (ps != null) {
                    try {
                        return String.format("(#%d) %d %s", threshold,
                                ps.thresholds[threshold - 1], ac.units);
                    } catch (ArrayIndexOutOfBoundsException e) {
                        // ignore
                    }
                }
            }
            return String.format("(Threshold #%d)", threshold);
        }

        @Override
        public String getColumnText(Object element, int columnIndex) {
            Threshold threshold = (Threshold) element;
            switch (columnIndex) {
            case CATEGORY:
                return getCategoryLabel(threshold.category);
            case THRESHOLD:
                if (isValidCategory(threshold.category)
                        && threshold.thresholdIndex != null)
                    return getThresholdLabel(threshold.category,
                            threshold.thresholdIndex);
                return "(not set)";
            case REQUEST_PRODUCT:
                return threshold.requestProduct ? "yes" : "no";
            }
            return null;
        }

        @Override
        public void addListener(ILabelProviderListener listener) {
            // nothing
        }

        @Override
        public void dispose() {
            // nothing
        }

        @Override
        public boolean isLabelProperty(Object element, String property) {
            return true;
        }

        @Override
        public void removeListener(ILabelProviderListener listener) {
            // nothing
        }

        public EditingSupport getEditorSupport(ColumnViewer viewer,
                int columnIndex) {
            return new ElementEditSupport(viewer, columnIndex);
        }

        protected class ElementEditSupport extends EditingSupport {

            int columnIndex;

            CellEditor editor;

            AlertCategory currentAC;

            ParameterSpec currentPS;

            public ElementEditSupport(ColumnViewer viewer, int columnIndex) {
                super(viewer);
                this.columnIndex = columnIndex;

                switch (columnIndex) {
                case REQUEST_PRODUCT:
                    editor = new CheckboxCellEditor(
                            (Composite) viewer.getControl(), SWT.CHECK);
                    break;
                case CATEGORY:
                case THRESHOLD:
                    ComboBoxViewerCellEditor ve = new ComboBoxViewerCellEditor(
                            (Composite) viewer.getControl(), SWT.READ_ONLY);
                    editor = ve;

                    if (columnIndex == CATEGORY) {
                        ve.getViewer().setLabelProvider(new LabelProvider() {
                            public String getText(Object element) {
                                if (element instanceof AlertCategory)
                                    return ((AlertCategory) element).getName();
                                else
                                    return getCategoryLabel((Integer) element);
                            }
                        });
                    } else {
                        ve.getViewer().setLabelProvider(new LabelProvider() {
                            @Override
                            public String getText(Object element) {
                                return getThresholdLabel(
                                        currentAC != null ? currentAC.id : 0,
                                        ((Integer) element));
                            }
                        });
                    }
                    break;
                }
            }

            @Override
            protected boolean canEdit(Object element) {
                return true;
            }

            @Override
            protected CellEditor getCellEditor(Object element) {
                // TODO
                Threshold threshold = (Threshold) element;
                ComboBoxViewerCellEditor ve;
                switch (columnIndex) {
                case REQUEST_PRODUCT:
                    return editor;
                case CATEGORY:
                    /*
                     * Prepare a list of categories without elements that have
                     * already been used.
                     */
                    ve = (ComboBoxViewerCellEditor) editor;
                    ArrayList<AlertCategory> lizt = new ArrayList<AlertCategory>();
                    lizt.addAll(Arrays.asList(AlertCategory
                            .getAlertCategories()));
                    for (Threshold t : (List<Threshold>) getObservableList()) {
                        if (t != element)
                            lizt.remove(AlertCategory
                                    .getAlertCategory(t.category));
                    }
                    ve.getViewer().setContentProvider(
                            new ArrayContentProvider());
                    ve.getViewer().setInput(lizt.toArray());
                    return editor;
                case THRESHOLD:
                    ve = (ComboBoxViewerCellEditor) editor;
                    currentAC = AlertCategory
                            .getAlertCategory(threshold.category);
                    currentPS = aapInfo.get(threshold.category);
                    ve.getViewer().setContentProvider(
                            new ArrayContentProvider());
                    int nLevels;
                    if (currentPS != null)
                        nLevels = currentPS.thresholds.length;
                    else if (currentAC != null)
                        nLevels = currentAC.nThresholdCodes;
                    else
                        nLevels = 6;
                    // Doesn't work with array of primitive type
                    Integer[] x = new Integer[nLevels];
                    for (int i = 0; i < x.length; ++i)
                        x[i] = i + 1;
                    ve.getViewer().setInput(x);
                    return editor;
                }
                return null;
            }

            @Override
            protected Object getValue(Object element) {
                Threshold threshold = (Threshold) element;
                switch (columnIndex) {
                case REQUEST_PRODUCT:
                    return threshold.requestProduct;
                case CATEGORY:
                    // TODO: Doesn't handle the case of an unknown category...
                    return AlertCategory.getAlertCategory(threshold.category);
                case THRESHOLD:
                    /*
                     * Must be an Integer or it will not match to any element of
                     * the list created in getCellEditor.
                     */
                    if (threshold.thresholdIndex != null)
                        return new Integer(threshold.thresholdIndex);
                    else
                        return new Integer(1);
                }
                return null;
            }

            @Override
            protected void setValue(Object element, Object value) {
                Threshold threshold = (Threshold) element;
                switch (columnIndex) {
                case REQUEST_PRODUCT:
                    threshold.requestProduct = (Boolean) value;
                    break;
                case CATEGORY:
                    int curCat = threshold.category;
                    int newCat;
                    if (value instanceof AlertCategory)
                        newCat = ((AlertCategory) value).id;
                    else if (value instanceof Number)
                        newCat = ((Number) value).intValue();
                    else
                        return;
                    threshold.category = newCat;
                    if (curCat != newCat)
                        threshold.thresholdIndex = 1;
                    break;
                case THRESHOLD:
                    if (value != null && value instanceof Number) {
                        threshold.thresholdIndex = ((Number) value)
                                .shortValue();
                    }
                    break;
                }
                int i = getObservableList().indexOf(threshold);
                if (i != -1)
                    getObservableList().set(i, threshold);
            }

        }
    }

}
