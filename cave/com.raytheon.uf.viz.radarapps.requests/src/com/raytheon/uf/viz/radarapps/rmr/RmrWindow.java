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
package com.raytheon.uf.viz.radarapps.rmr;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import org.eclipse.core.databinding.observable.ChangeEvent;
import org.eclipse.core.databinding.observable.IChangeListener;
import org.eclipse.core.databinding.observable.list.WritableList;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.ITreeSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.StructuredViewer;
import org.eclipse.jface.viewers.TreePath;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.rcm.config.RadarConfig;
import com.raytheon.rcm.event.NotificationEvent;
import com.raytheon.rcm.event.RadarEventAdapter;
import com.raytheon.rcm.mqsrvr.ReplyObj.ConfigReply;
import com.raytheon.rcm.mqsrvr.ReplyObj.RmrReply;
import com.raytheon.rcm.mqsrvr.ReqObj.ActivateRMR;
import com.raytheon.rcm.mqsrvr.ReqObj.CancelRMR;
import com.raytheon.rcm.mqsrvr.ReqObj.GetActiveRMRs;
import com.raytheon.rcm.mqsrvr.ReqObj.GetRadarConfig;
import com.raytheon.rcm.products.ProductInfo;
import com.raytheon.rcm.products.RadarProduct;
import com.raytheon.rcm.products.RadarProduct.Param;
import com.raytheon.rcm.request.Request;
import com.raytheon.rcm.rmr.ActiveRequest;
import com.raytheon.rcm.rmr.MultipleRequest;
import com.raytheon.rcm.rmr.MultipleRequest.Element;
import com.raytheon.rcm.rmr.RmrEvent;
import com.raytheon.rcm.rmr.RmrXml;
import com.raytheon.rcm.rmr.RmrXml.Requests;
import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.viz.radarapps.core.RadarApps;

public class RmrWindow extends Dialog {

    private TreeViewer availableRequestsViewer;

    private TreeViewer activeRequestsViewer;

    private WritableList availableList = new WritableList();

    private WritableList activeList = new WritableList();

    private Button addProductsButton;

    private Button editButton;

    private Button copyButton;

    private Button deleteButton;

    private Button submitButton;

    private Button cancelButton;

    private ArrayList<RadarConfig> radars = new ArrayList<RadarConfig>();

    public RmrWindow(Shell parentShell) {
        super(parentShell);
        setBlockOnOpen(false);
        setShellStyle(getShellStyle() & ~SWT.APPLICATION_MODAL);
    }

    @Override
    protected void configureShell(Shell newShell) {
        super.configureShell(newShell);
        newShell.setText("Radar Multiple Request (RMR)");
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        Composite c = (Composite) super.createDialogArea(parent);
        GridLayout gl = (GridLayout) c.getLayout();
        gl.numColumns = 1;

        RowLayout rl;

        createSection(c, "Existing Requests");

        Composite client;
        client = new Composite(c, SWT.WRAP);
        client.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, true));
        GridLayout gl2 = new GridLayout();
        gl2.numColumns = 2;
        gl2.marginWidth = 2;
        gl2.marginHeight = 2;
        client.setLayout(gl2);
        // Tree t = toolkit.createTree(client, SWT.NULL);
        availableRequestsViewer = new TreeViewer(client, SWT.H_SCROLL
                | SWT.V_SCROLL | SWT.BORDER | SWT.SINGLE);
        GridData gd = new GridData(GridData.FILL_BOTH);
        gd.widthHint = convertWidthInCharsToPixels(58);
        gd.heightHint = convertHeightInCharsToPixels(20);
        availableRequestsViewer.getControl().setLayoutData(gd);
        Composite bb = new Composite(client, SWT.NONE);
        rl = new RowLayout(SWT.VERTICAL);
        rl.pack = false;
        bb.setLayout(rl);
        Button b;

        b = createButton(bb, "New Request", SWT.PUSH);
        b.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                onNewRequest();
            }
        });
        copyButton = b = createButton(bb, "Copy", SWT.PUSH);
        b.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                onCopyElement();
            }
        });
        addProductsButton = b = createButton(bb, "Add Prod", SWT.PUSH);
        b.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                onAddProducts();
            }
        });
        editButton = b = createButton(bb, "Edit", SWT.PUSH);
        b.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                onEdit();
            }
        });
        deleteButton = b = createButton(bb, "Delete", SWT.PUSH);
        b.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                onDelete();
            }
        });
        new Label(bb, SWT.LEFT);
        submitButton = b = createButton(bb, "Submit", SWT.PUSH);
        b.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                onSubmit();
            }
        });

        availableRequestsViewer
                .addSelectionChangedListener(new ISelectionChangedListener() {
                    public void selectionChanged(SelectionChangedEvent event) {
                        onAvailableViewerSelectionChanged();
                    }
                });

        availableRequestsViewer
                .setContentProvider(new RmrTreeContentProvider());
        availableRequestsViewer.setLabelProvider(new RmrLabelProvider());
        availableRequestsViewer.setInput(availableList);

        createSection(c, "Active Requests");

        client = new Composite(c, SWT.WRAP);
        client.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, true));
        gl2 = new GridLayout();
        gl2.numColumns = 2;
        gl2.marginWidth = 2;
        gl2.marginHeight = 2;
        client.setLayout(gl2);
        activeRequestsViewer = new TreeViewer(client);
        gd = new GridData(GridData.FILL_BOTH);
        gd.widthHint = convertWidthInCharsToPixels(40);
        gd.heightHint = convertHeightInCharsToPixels(5);
        activeRequestsViewer.getControl().setLayoutData(gd);
        activeRequestsViewer
                .addSelectionChangedListener(new ISelectionChangedListener() {
                    public void selectionChanged(SelectionChangedEvent event) {
                        onActiveViewerSelectionChanged();
                    }
                });
        bb = new Composite(client, SWT.NONE);
        rl = new RowLayout(SWT.VERTICAL);
        rl.pack = false;
        bb.setLayout(rl);
        cancelButton = b = createButton(bb, "Cancel", SWT.PUSH);
        b.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                onCancel();
            }
        });
        activeRequestsViewer.setContentProvider(new RmrTreeContentProvider());
        activeRequestsViewer.setLabelProvider(new RmrLabelProvider());
        activeRequestsViewer.setInput(activeList);

        onAvailableViewerSelectionChanged();
        onActiveViewerSelectionChanged();

        final RadarEventAdapter rel = new RadarEventAdapter() {
            @Override
            public void handleNotificationEvent(NotificationEvent event) {
                if (event instanceof RmrEvent) {
                    getShell().getDisplay().asyncExec(new Runnable() {
                        public void run() {
                            getActiveRequests();
                        }
                    });
                }
            }
        };

        getShell().addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                RadarApps.getRcmSystem().getClient().removeEventListener(rel);
            }
        });

        RadarApps.getRcmSystem().getClient().addEventListener(rel);

        getShell().getDisplay().asyncExec(new Runnable() {
            public void run() {
                getRadarConfig();
                load();
                getActiveRequests();
            }
        });

        return c;
    }

    private Control createSection(Composite parent, String title) {
        Composite r;
        Label l;
        GridLayout gl;

        r = new Composite(parent, SWT.NONE);
        gl = new GridLayout(2, false);
        gl.marginWidth = gl.marginHeight = 0;
        r.setLayout(gl);
        l = new Label(r, SWT.LEFT);
        l.setText(title);
        l.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false));
        l = new Label(r, SWT.SEPARATOR | SWT.HORIZONTAL);
        l.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

        r.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
        return r;
    }

    private Button createButton(Composite parent, String string, int flags) {
        Button button = new Button(parent, flags);
        button.setText(string);
        return button;
    }

    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        createButton(parent, IDialogConstants.CLOSE_ID,
                IDialogConstants.CLOSE_LABEL, false);
    }

    @Override
    protected void buttonPressed(int buttonId) {
        if (buttonId == IDialogConstants.CLOSE_ID) {
            setReturnCode(Window.OK);
            close();
        }
    }

    protected void onEdit() {
        ITreeSelection sel = (ITreeSelection) availableRequestsViewer
                .getSelection();
        if (sel.size() > 0) {
            TreePath path = sel.getPaths()[0];
            Object item = path.getLastSegment();
            if (item instanceof MultipleRequest) {
                editRequest((MultipleRequest) item);
            } else if (item instanceof Element) {
                editElement((Element) item);
            } else if (item instanceof String) {
                editElement((Element) path
                        .getSegment(path.getSegmentCount() - 2));
            }
        }
    }

    private boolean editRequest(MultipleRequest mr) {
        RequestDialog dialog = new RequestDialog(getShell(), mr, availableList);
        if (dialog.open() == Window.OK) {
            onElementChanged(mr);
            return true;
        } else {
            return false;
        }
    }

    private boolean editElement(Element element) {
        ElementDialog dialog = new ElementDialog(this, element);
        if (dialog.open() == Window.OK) {
            onElementChanged(element);
            return true;
        } else {
            return false;
        }
    }

    protected void onAvailableViewerSelectionChanged() {
        boolean enbAddProds = false;
        boolean enbEdit = false;
        boolean enbDelete = false;
        boolean enbCopy = false;
        boolean enbSubmit = false;

        if (getSelectedMR() != null) {
            enbAddProds = true;
            enbSubmit = canSubmitMR(getSelectedMR());
        } else {
            enbAddProds = enbSubmit = false;
        }

        ITreeSelection sel = (ITreeSelection) availableRequestsViewer
                .getSelection();
        if (sel.size() > 0) {
            Object item = sel.getPaths()[0].getLastSegment();
            enbEdit = enbDelete = true;
            enbCopy = item instanceof MultipleRequest
                    || item instanceof Element;
        }
        addProductsButton.setEnabled(enbAddProds);
        editButton.setEnabled(enbEdit);
        deleteButton.setEnabled(enbDelete);
        copyButton.setEnabled(enbCopy);
        submitButton.setEnabled(enbSubmit);
    }

    private boolean canSubmitMR(MultipleRequest selectedMR) {
        if (selectedMR.getElements().length < 1) {
            return false;
        }
        for (Element elm : selectedMR.getElements()) {
            if (elm.getRadarIDs().length < 1) {
                return false;
            }
        }
        return true;
    }

    protected void onActiveViewerSelectionChanged() {
        cancelButton.setEnabled(getSelectedMR(activeRequestsViewer) != null);
    }

    protected void onCancel() {
        MultipleRequest mr = getSelectedMR(activeRequestsViewer);
        if (mr != null && mr.getName() != null && mr.getName().length() > 0) {
            CancelRMR ro = new CancelRMR();
            ro.requestName = mr.getName();
            RadarApps.getRcmSystem().sendCheckedAndHandled(ro, getShell());
        }
    }

    protected void onSubmit() {
        MultipleRequest mr = getSelectedMR();
        if (mr != null && mr.getName() != null && mr.getName().length() > 0
                && mr.getElements().length > 0) {
            ActivateRMR ro = new ActivateRMR();
            ro.multipleRequest = mr;
            RadarApps.getRcmSystem().sendCheckedAndHandled(ro, getShell());
        }
    }

    protected boolean confirmDelete(String description) {
        MessageBox mb = new MessageBox(getShell(), SWT.ICON_QUESTION | SWT.YES
                | SWT.NO);
        mb.setText("Confirm Delete");
        mb.setMessage(String.format("Are you sure you want to delete %s?",
                description));
        return mb.open() == SWT.YES;
    }

    protected void onDelete() {
        TreeViewer treeViewer = this.availableRequestsViewer;
        ITreeSelection sel = (ITreeSelection) treeViewer.getSelection();
        if (sel.size() > 0) {
            TreePath path = sel.getPaths()[0];
            Object last = path.getLastSegment();

            if (last instanceof String) {
                MultipleRequest.Element elm = (MultipleRequest.Element) path
                        .getSegment(path.getSegmentCount() - 2);
                ArrayList<String> x = new ArrayList<String>(Arrays.asList(elm
                        .getRadarIDs()));
                x.remove(last);
                elm.setRadarIDs(x.toArray(new String[x.size()]));
                onElementChanged(elm);
            } else if (last instanceof MultipleRequest.Element) {
                MultipleRequest mr = (MultipleRequest) path.getSegment(path
                        .getSegmentCount() - 2);
                ArrayList<MultipleRequest.Element> x = new ArrayList<MultipleRequest.Element>(
                        Arrays.asList(mr.getElements()));
                if (!confirmDelete("the selected product request")) {
                    return;
                }
                // defend against possible future impl of
                // MultipleRequest.Element.equals...
                int i = 0;
                for (MultipleRequest.Element elm : x) {
                    if (elm == last) {
                        x.remove(i);
                        break;
                    }
                    ++i;
                }
                mr.setElements(x.toArray(new MultipleRequest.Element[x.size()]));
                onElementChanged(mr);
            } else if (last instanceof MultipleRequest) {
                if (!confirmDelete(String.format("the request \"%s\"",
                        ((MultipleRequest) last).getName()))) {
                    return;
                }
                // defend against possible future impl of
                // MultipleRequest.equals...
                int i = 0;
                for (MultipleRequest mr : (List<MultipleRequest>) availableList) {
                    if (mr == last) {
                        availableList.remove(i);
                        break;
                    }
                    ++i;
                }
                // already notified
            }
        }
    }

    private void appendElement(MultipleRequest mr, Element elem) {
        Element[] newElems = new Element[mr.getElements().length + 1];
        System.arraycopy(mr.getElements(), 0, newElems, 0,
                mr.getElements().length);
        newElems[newElems.length - 1] = elem;
        mr.setElements(newElems);
    }

    protected void onAddProducts() {
        MultipleRequest mr = getSelectedMR();
        if (mr != null) {
            while (true) {
                MultipleRequest.Element ne = new MultipleRequest.Element();
                ne.setRequest(getDefaultRequest());
                if (editElement(ne)) {
                    appendElement(mr, ne);
                    onElementChanged(mr);
                    availableRequestsViewer.expandToLevel(mr, 2);
                    availableRequestsViewer.reveal(ne);
                    availableRequestsViewer
                            .setSelection(new StructuredSelection(ne));
                } else {
                    break;
                }
            }
        }
    }

    private Request getDefaultRequest() {
        Request r = new Request();
        r.productCode = 19;
        r.selectCurrent();
        r.count = 1;
        r.interval = 1;
        return r;
    }

    protected void onNewRequest() {
        MultipleRequest mr = new MultipleRequest();
        mr.setName("");
        mr.setDuration(4 * 60 * 60); // Four hours
        mr.setInterval(6 * 60); // Six minutes
        if (editRequest(mr)) {
            availableList.add(mr);
            availableRequestsViewer.setSelection(new StructuredSelection(mr),
                    true);

            onAddProducts();
        }
    }

    protected void onCopyElement() {
        ITreeSelection sel = (ITreeSelection) availableRequestsViewer
                .getSelection();
        if (sel.size() > 0) {
            TreePath path = sel.getPaths()[0];
            Object item = path.getLastSegment();
            if (item instanceof MultipleRequest) {
                MultipleRequest copy = ((MultipleRequest) item).duplicate();
                copy.setName("Copy of " + copy.getName());
                availableList.add(copy);
                availableRequestsViewer.setSelection(new StructuredSelection(
                        copy), true);
            } else if (item instanceof Element) {
                MultipleRequest mr = getSelectedMR();
                if (mr != null) {
                    Element copy = ((Element) item).duplicate();
                    appendElement(mr, copy);
                    onElementChanged(mr);
                    availableRequestsViewer.reveal(copy);
                    availableRequestsViewer
                            .setSelection(new StructuredSelection(copy));
                }
            }
        }
    }

    private MultipleRequest getSelectedMR() {
        return getSelectedMR(availableRequestsViewer);
    }

    private MultipleRequest getSelectedMR(TreeViewer viewer) {
        ITreeSelection sel = (ITreeSelection) viewer.getSelection();
        if (sel.size() > 0) {
            TreePath path = sel.getPaths()[0];
            for (int i = 0; i < path.getSegmentCount(); ++i) {
                Object obj = path.getSegment(i);
                if (obj instanceof MultipleRequest) {
                    return (MultipleRequest) obj;
                } else if (obj instanceof ActiveRequest) {
                    return ((ActiveRequest) obj).getMultipleRequest();
                }
            }
        }
        return null;
    }

    private class RmrTreeContentProvider implements ITreeContentProvider,
            IChangeListener {

        private WritableList input;

        private StructuredViewer viewer;

        @Override
        public Object[] getChildren(Object parentElement) {
            if (parentElement instanceof MultipleRequest) {
                // ugh, this is a loser... no writeback...
                MultipleRequest mr = (MultipleRequest) parentElement;
                return mr.getElements();
            } else if (parentElement instanceof ActiveRequest) {
                MultipleRequest mr = ((ActiveRequest) parentElement)
                        .getMultipleRequest();
                return mr.getElements();
            } else if (parentElement instanceof MultipleRequest.Element) {
                MultipleRequest.Element elem = (MultipleRequest.Element) parentElement;
                return elem.getRadarIDs();
            } else {
                System.out.println("Should not be asked for children of "
                        + parentElement);
                return null;
            }
        }

        @Override
        public Object getParent(Object element) {
            if (element instanceof MultipleRequest) {
                return input;
            } else {
                return null;
            }
        }

        @Override
        public boolean hasChildren(Object element) {
            if (element instanceof MultipleRequest) {
                return ((MultipleRequest) element).getElements().length > 0;
            } else if (element instanceof ActiveRequest) {
                return ((ActiveRequest) element).getMultipleRequest()
                        .getElements().length > 0;
            } else if (element instanceof MultipleRequest.Element) {
                return ((MultipleRequest.Element) element).getRadarIDs().length > 0;
            } else {
                return false;
            }
        }

        @Override
        public Object[] getElements(Object inputElement) {
            if (inputElement instanceof WritableList) {
                return ((WritableList) inputElement).toArray();
            } else {
                System.out.println("Shouldn't be asked for elements of "
                        + inputElement);
                return new Object[0];
            }
        }

        @Override
        public void dispose() {
            // nothing
        }

        @Override
        public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
            if (oldInput != null) {
                ((WritableList) oldInput).removeChangeListener(this);
            }
            if (newInput != null) {
                ((WritableList) newInput).addChangeListener(this);
            }
            input = (WritableList) newInput;
            this.viewer = (StructuredViewer) viewer;
        }

        @Override
        public void handleChange(ChangeEvent event) {
            if (viewer != null) {
                viewer.refresh(input);
            }
        }

    }

    private class RmrLabelProvider extends LabelProvider {

        @Override
        public String getText(Object element) {
            if (element instanceof MultipleRequest) {
                // TODO: checks on name != null
                return ((MultipleRequest) element).getName();
            } else if (element instanceof ActiveRequest) {
                ActiveRequest ar = (ActiveRequest) element;
                MultipleRequest mr = ar.getMultipleRequest();
                String s = mr.getName();
                /*
                 * As in AWIPS 1, one-shot requests do actually dislay in this
                 * list, but there is a check in case they will ben shown in the
                 * future.
                 */
                if (!mr.isSingle() && mr.getInterval() != 0) {
                    int interval = mr.getInterval();
                    int readableInterval;
                    String unit;
                    if ((interval % 3600) == 0) {
                        unit = "hour";
                        readableInterval = interval / 3600;
                    } else {
                        unit = "minute";
                        readableInterval = interval / 60;
                    }
                    String intvs = readableInterval != 1 ? String.format(
                            "%d %ss", readableInterval, unit) : unit;
                    s = s
                            + String.format(" (%d left, every %s)",
                                    ar.getRemainingTime() / interval, intvs);
                }
                return s;
            } else if (element instanceof MultipleRequest.Element) {
                return getRequestLabel(((MultipleRequest.Element) element)
                        .getRequest());
            } else {
                return super.getText(element);
            }
        }

    }

    // TODO: unduplicate this from .rps and put it in .core or maybe lib..
    protected String getRequestLabel(Request req) {
        RadarProduct rp = ProductInfo.getInstance().getPoductForCode(
                req.productCode);
        StringBuilder sb = new StringBuilder();
        if (rp != null) {
            if (rp.name != null) {
                sb.append(rp.name);
                /*
                 * if (rp.mnemonic != null)
                 * sb.append(" (").append(rp.mnemonic).append(')');
                 */
            }

            if (rp.levels != null) {
                sb.append(", levels ").append(rp.levels);
            }
            if (rp.resolution != null) {
                sb.append(", resol ").append(rp.resolution);
            }
            if (rp.params.contains(Param.BASELINE)
                    || rp.params.contains(Param.CFC_BITMAP)) {
                // TODO:...
                sb.append(", (TODO: ...)");
            }
            // rp.layer will be part of the product name...

            // The SRM exception follows AWIPS-1; not sure if it really correct.
            if (rp.params.contains(Param.STORM_SPEED_DIR)
                    && (rp.mnemonic == null || !rp.mnemonic.equals("SRM"))) {
                sb.append(", (TODO: ...)");
            }

            if (rp.params.contains(Param.TIME_SPAN)) {
                // TODO: is the other supposed to be -1 (and ignored.....?)
                if (req.getTimeSpan() != -1) {
                    sb.append(", ").append(req.getTimeSpan())
                            .append(" hr span");
                }
                if (req.getEndHour() != -1) {
                    sb.append(", End hr ").append(req.getEndHour());
                } else {
                    sb.append(", Latest");
                }
            }

            if (rp.params.contains(Param.ELEVATION)) {
                sb.append(", ");
                float angle = req.getElevationAngle() / 10.0f;
                switch (req.getElevationSelection()) {
                case Request.SPECIFIC_ELEVATION:
                    sb.append(String.format("elev %.1f%s", angle,
                    /*
                     * TODO: need isTdwr from context
                     * listEditor.isTdwrVcp(listEditor.getVcp()) ? ", Cuts One"
                     * :
                     */""));
                    break;
                case Request.N_ELEVATIONS:
                    sb.append("elev lowest " + req.getElevationAngle());
                    break;
                case Request.LOWER_ELEVATIONS:
                    sb.append("elev <= " + angle);
                    break;
                case Request.ALL_ELEVATIONS:
                    if (angle == 0f) {
                        sb.append("elev all");
                    } else {
                        sb.append(String.format("elev %.1f, Cuts All", angle));
                    }
                    break;
                }
            }
        } else {
            sb.append(String.format("Product #%d", req.productCode));
        }
        return sb.toString();
        // return element.toString();
    }

    private File requestsFile;

    private LocalizationFile requestsLocnFile;

    /**
     * Get the local file used to store the list of available requests.
     * 
     * Note: This must be be called while the shell is open.
     */
    private File getRequestsFile() {
        if (requestsFile == null) {
            // Testing code
            String s = System
                    .getProperty("com.raytheon.uf.viz.radarapps.rmr.use.fake.file");
            if (s != null) {
                s = System.getProperty("user.home");
                if (s == null) {
                    s = "/tmp";
                }
                return new File(s, "rmr.xml");
            }

            IPathManager pathMgr = PathManagerFactory.getPathManager();
            LocalizationContext context = pathMgr.getContext(
                    LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
            // LocalizationManager.getInstance().getCurrentSite()
            requestsLocnFile = pathMgr.getLocalizationFile(context, "radar"
                    + File.separator + "rmr" + File.separator
                    + "rmrAvailableRequests.xml");
            final ILocalizationFileObserver fobs = new ILocalizationFileObserver() {
                public void fileUpdated(FileUpdatedMessage message) {
                    Shell s = getShell();
                    if (s != null && !s.isDisposed()) {
                        s.getDisplay().asyncExec(new Runnable() {
                            public void run() {
                                reload();
                            }
                        });
                    }
                }
            };

            getShell().addDisposeListener(new DisposeListener() {
                public void widgetDisposed(DisposeEvent e) {
                    requestsLocnFile.removeFileUpdatedObserver(fobs);
                }
            });
            requestsLocnFile.addFileUpdatedObserver(fobs);

        }
        /*
         * Must call requestsLocnFile.getFile() before reading to update the
         * contents of the local copy.
         */
        if (requestsLocnFile != null) {
            requestsFile = requestsLocnFile.getFile();
        }
        return requestsFile;
    }

    protected void reload() {
        // TODO: save selection or merge...
        load();
    }

    private void load() {
        File f = getRequestsFile();
        Requests newRequests;

        if (f == null) {
            return;
        }

        try {
            FileInputStream ins = new FileInputStream(f);
            Unmarshaller u = RmrXml.getUnmarshaller();
            newRequests = (Requests) u.unmarshal(ins);
        } catch (FileNotFoundException e) {
            return;
        } catch (Exception e) {
            MessageBox mb = new MessageBox(getShell(), SWT.ICON_ERROR | SWT.OK);
            mb.setText("Error");
            mb.setMessage(String.format("Could not read requests from %s: %s",
                    f, e));
            mb.open();
            return;
        }

        if (newRequests != null) {
            fixRequests(newRequests.list);
            availableList.clear();
            availableList.addAll(newRequests.list);
        }
    }

    private void store() {
        File f = getRequestsFile();

        if (f == null) {
            return;
        }

        try {
            File dir = f.getParentFile();
            if (!dir.exists() && !dir.mkdirs()) {
                throw new IOException("Could not create directory: " + f);
            }
            File tmp = File.createTempFile("rmrt", null, dir);
            Marshaller m = RmrXml.getMarshaller();
            Requests requests = new Requests();
            requests.list.addAll(availableList);
            m.marshal(requests, tmp);

            File bak = new File(dir, f.getName() + ".bak");
            bak.delete();
            f.renameTo(bak);
            if (!tmp.renameTo(f)) {
                throw new IOException(String.format(
                        "Could not rename %s to %s", bak, f));
            }

            if (requestsLocnFile != null) {
                requestsLocnFile.save();
            }
        } catch (Exception e) {
            if (e instanceof RuntimeException) {
                e.printStackTrace(System.err);
            }
            /*
             * String err = "Error saving to server"; Status status = new
             * Status(Status.ERROR, Activator.PLUGIN_ID, 0, err, e);
             * ErrorDialog.openError( / *Display.getCurrent().getActiveShell()*
             * / getShell(), "ERROR", err, status);
             */
            MessageBox mb = new MessageBox(getShell(), SWT.ICON_ERROR | SWT.OK);
            mb.setText("Error");
            mb.setMessage(String.format("Could not write requests: %s", e));
            mb.open();
            return;
        }
    }

    private void getRadarConfig() {
        GetRadarConfig ro = new GetRadarConfig();
        ConfigReply reply = (ConfigReply) RadarApps.getRcmSystem()
                .sendCheckedAndHandled(ro, getShell());
        if (reply != null) {
            radars.clear();
            radars.addAll(reply.config);
        }
    }

    private void getActiveRequests() {
        GetActiveRMRs ro = new GetActiveRMRs();
        RmrReply reply = (RmrReply) RadarApps.getRcmSystem()
                .sendCheckedAndHandled(ro, getShell());
        if (reply != null) {
            activeList.clear();
            activeList.addAll(reply.list);
        }
    }

    public void onElementChanged(Object obj) {
        availableRequestsViewer.refresh(obj, true);
        onAvailableViewerSelectionChanged();
    }

    // This is supposed to be package access only.
    List<RadarConfig> getRadars() {
        return radars;
    }

    @Override
    public boolean close() {
        store();
        return super.close();
    }

    /*
     * Fix up requests from older versions which may have had the request
     * interval set to zero.
     */
    private void fixRequests(List<MultipleRequest> list) {
        for (MultipleRequest mr : list) {
            for (Element elem : mr.getElements()) {
                elem.getRequest().interval = (short) Math.max(1,
                        elem.getRequest().interval);
            }
        }
    }

}
