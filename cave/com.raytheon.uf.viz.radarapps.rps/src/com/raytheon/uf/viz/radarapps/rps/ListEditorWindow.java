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
package com.raytheon.uf.viz.radarapps.rps;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import javax.xml.bind.JAXBException;

import org.eclipse.core.databinding.observable.ChangeEvent;
import org.eclipse.core.databinding.observable.IChangeListener;
import org.eclipse.core.databinding.observable.list.WritableList;
import org.eclipse.jface.databinding.viewers.ObservableListContentProvider;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.events.ShellAdapter;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Monitor;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.dialogs.ListDialog;

import com.raytheon.rcm.config.LinkResource;
import com.raytheon.rcm.config.RadarType;
import com.raytheon.rcm.config.Util;
import com.raytheon.rcm.config.awips1.Awips1RpsListUtil;
import com.raytheon.rcm.config.awips1.Awips1RpsListUtil.Selector;
import com.raytheon.rcm.message.GSM;
import com.raytheon.rcm.mqsrvr.ReplyObj;
import com.raytheon.rcm.mqsrvr.ReplyObj.ConfigReply;
import com.raytheon.rcm.mqsrvr.ReplyObj.RpsListReply;
import com.raytheon.rcm.mqsrvr.ReqObj;
import com.raytheon.rcm.mqsrvr.ReqObj.GetRpsList;
import com.raytheon.rcm.mqsrvr.ReqObj.SendRpsList;
import com.raytheon.rcm.products.ElevationInfo;
import com.raytheon.rcm.products.ElevationInfo.VCPInfo;
import com.raytheon.rcm.products.ProductInfo;
import com.raytheon.rcm.products.RadarProduct;
import com.raytheon.rcm.products.RadarProduct.Param;
import com.raytheon.rcm.request.Request;
import com.raytheon.rcm.request.RpsList;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.radarapps.client.RcmClient;
import com.raytheon.uf.viz.radarapps.client.RcmWaiter;
import com.raytheon.uf.viz.radarapps.core.RadarApps;
import com.raytheon.uf.viz.radarapps.products.ui.RadarProductUI;

/**
 * RPS List Editor window
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2009-04-22   #2286      D. Friedman Initial checkin
 * ...
 * 2013-01-31   DR 15458   D. Friedman Send RPS list so that it will be
 *                                     accepted for any VCP.
 * </pre>
 * 
 */
public class ListEditorWindow {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ListEditorWindow.class);

    Shell shell;

    ListEditor listEditor;

    ListViewer listViewer;

    Label numberOfProductsLabel;

    IChangeListener listChangedListener;

    public ListEditorWindow(ListEditor listEditor) {
        shell = new Shell(SWT.SHELL_TRIM);
        shell.setText("RPS List Editor");

        shell.addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                onDispose();
            }
        });
        shell.addShellListener(new ShellAdapter() {
            @Override
            public void shellClosed(ShellEvent e) {
                onClose(e);
            }
        });

        this.listEditor = listEditor;

        Menu mb = new Menu(shell, SWT.BAR);
        MenuItem mi = new MenuItem(mb, SWT.CASCADE);
        mi.setText("File");
        Menu m = new Menu(shell, SWT.DROP_DOWN);
        mi.setMenu(m);

        mi = new MenuItem(m, SWT.PUSH);
        mi.setText("New...");
        mi.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                onNewList();
            }
        });

        mi = new MenuItem(m, SWT.PUSH);
        mi.setText("Open...");
        mi.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                onOpenList();
            }
        });

        mi = new MenuItem(m, SWT.PUSH);
        mi.setText("Save");
        mi.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                doSave();
            }
        });

        mi = new MenuItem(m, SWT.PUSH);
        mi.setText("Save As...");
        mi.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                doSaveAs();
            }
        });

        mi = new MenuItem(m, SWT.PUSH);
        mi.setText("Exit");
        mi.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                shell.close();
            }
        });

        mi = new MenuItem(mb, SWT.CASCADE);
        mi.setText("List");
        m = new Menu(shell, SWT.DROP_DOWN);
        mi.setMenu(m);

        SelectionListener saAdd = new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                onAdd();
            }
        };
        SelectionListener saEdit = new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                onEdit();
            }
        };
        SelectionListener saRemove = new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                onRemove();
            }
        };

        SelectionListener saSend = new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                onSendList();
            }
        };

        mi = new MenuItem(m, SWT.PUSH);
        mi.setText("Add Product...");
        mi.addSelectionListener(saAdd);

        mi = new MenuItem(m, SWT.PUSH);
        mi.setText("Edit Product...");
        mi.addSelectionListener(saEdit);

        mi = new MenuItem(m, SWT.PUSH);
        mi.setText("Remove Product(s)");
        mi.addSelectionListener(saRemove);

        mi = new MenuItem(m, SWT.SEPARATOR);

        mi = new MenuItem(m, SWT.PUSH);
        mi.setText("Send List...");
        mi.addSelectionListener(saSend);

        // removed send and store list
        // mi = new MenuItem(m, SWT.PUSH);
        // mi.setText("Send and Store List...");
        // mi.addSelectionListener(new SelectionAdapter() {
        // public void widgetSelected(SelectionEvent e) {
        // onStoreList();
        // }
        // });

        mi = new MenuItem(mb, SWT.CASCADE);
        mi.setText("View");
        m = new Menu(shell, SWT.DROP_DOWN);
        mi.setMenu(m);

        mi = new MenuItem(m, SWT.PUSH);
        mi.setText("Current List...");
        mi.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                onViewCurrentList(-1);
            }
        });

        Collection<VCPInfo> vcpInfo = ElevationInfo.getInstance().getVcpInfo();
        for (int pass = 0; pass < 2; ++pass) {

            mi = new MenuItem(m, SWT.SEPARATOR);
            mi = new MenuItem(m, SWT.PUSH);
            mi.setText(String.format("%s Lists", pass == 0 ? "Clear Air"
                    : "Storm Mode"));

            for (VCPInfo vi : vcpInfo) {
                final int vcp = vi.vcp;
                if ((pass == 0 && vi.opMode == GSM.OP_MODE_CLEAR_AIR)
                        || (pass == 1 && vi.opMode == GSM.OP_MODE_STORM)) {
                    mi = new MenuItem(m, SWT.PUSH);
                    mi.setText(String.format("    VCP%d (%s)", vcp,
                            listEditor.isTdwrVcp(vcp) ? "TDWR" : "WSR-88D"));
                    mi.addSelectionListener(new SelectionAdapter() {
                        @Override
                        public void widgetSelected(SelectionEvent e) {
                            onViewCurrentList(vcp);
                        }
                    });

                }
            }
        }

        shell.setLayout(new GridLayout(1, true));

        Composite bar = new Composite(shell, SWT.NONE);
        RowLayout rl = new RowLayout(SWT.HORIZONTAL);
        rl.pack = false;
        bar.setLayout(rl);
        bar.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));

        Button b = new Button(bar, SWT.PUSH);
        b.setText("Add");
        b.addSelectionListener(saAdd);

        b = new Button(bar, SWT.PUSH);
        b.setText("Edit");
        b.addSelectionListener(saEdit);

        b = new Button(bar, SWT.PUSH);
        b.setText("Remove");
        b.addSelectionListener(saRemove);

        b = new Button(bar, SWT.PUSH);
        b.setText("Send");
        b.addSelectionListener(saSend);

        // shell.setLayout(new FillLayout());

        ListViewer lv = new ListViewer(shell);
        lv.setContentProvider(new ObservableListContentProvider());
        lv.setLabelProvider(new LabelProvider() {
            @Override
            public String getText(Object element) {
                return getRequestLabel((Request) element);
            }
        });
        lv.setInput(listEditor.getRequestList());
        lv.addDoubleClickListener(new IDoubleClickListener() {
            @Override
            public void doubleClick(DoubleClickEvent event) {
                onEdit();
            }
        });
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 380; // TODO:
        gd.heightHint = 400; // TODO:
        lv.getControl().setLayoutData(gd);
        listViewer = lv;

        bar = new Group(shell, SWT.SHADOW_IN);
        bar.setLayout(new FillLayout());
        numberOfProductsLabel = new Label(bar, SWT.LEFT);
        bar.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));

        listChangedListener = new IChangeListener() {
            @Override
            public void handleChange(ChangeEvent event) {
                updateNumberOfProducts();
            }
        };
        listEditor.getRequestList().addChangeListener(listChangedListener);
        updateNumberOfProducts();

        shell.setMenuBar(mb);
        shell.pack();

        // TODO: Should just inherit from JFace Window
        try {
            Monitor mon = shell.getDisplay().getPrimaryMonitor();
            Point p = shell.getSize();

            shell.setLocation(mon.getBounds().x + (mon.getBounds().width - p.x)
                    / 2, mon.getBounds().y + (mon.getBounds().height - p.y) / 2);
        } catch (Exception ex) {
        }
    }

    private void onDispose() {
        listEditor.getRequestList().removeChangeListener(listChangedListener);
    }

    protected void updateNumberOfProducts() {
        if (numberOfProductsLabel != null
                && !numberOfProductsLabel.isDisposed()) {
            numberOfProductsLabel.setText(getRequestCounts(
                    listEditor.getRpsList(), listEditor.getRadarID())
                    .getDescription());
        }
    }

    private int getOpModeForVcp(int vcp) {
        Collection<VCPInfo> vcpInfo = ElevationInfo.getInstance().getVcpInfo();
        for (VCPInfo vi : vcpInfo) {
            if (vi.vcp == vcp)
                return vi.opMode;
        }
        return -1;
    }

    protected void onViewCurrentList(int vcp) {
        if (!checkUnsaved())
            return;

        RadarType type = null;
        if (vcp != -1)
            type = listEditor.isTdwrVcp(vcp) ? RadarType.TDWR : RadarType.WSR;
        String radarID = RadarApps.chooseRpg(getShell(), true, type, false);
        if (radarID == null)
            return;

        GetRpsList req = new GetRpsList();
        req.radarID = radarID;
        if (vcp != -1) {
            req.vcp = vcp;
            req.opMode = getOpModeForVcp(vcp);
        }

        String error = null;
        RpsList newList = null;

        String PREFIX = "Could not retrieve the RPS list: ";
        try {
            // TODO: needs to be more foregiving...
            RcmWaiter waiter = new RcmWaiter(req, getShell());
            // ReplyObj reply = client.sendRequest(req, 1500);
            ReplyObj reply = waiter.send();
            if (reply == null)
                return;
            if (reply.error == null) {
                newList = ((RpsListReply) reply).rpsList;
                if (newList == null) {
                    if (vcp == -1)
                        error = "There is no current RPS list.  Most likely there is no connection to the RPG.";
                    else
                        error = "The specified list does not exist.  Most likely there is no connection to the RPG.";
                }
            } else
                error = PREFIX + reply.error;
        } catch (IOException e) {
            error = PREFIX + e.toString();
        }

        if (error != null) {
            showError(error);
            return;
        }
        listEditor.setRadarID(radarID);
        replaceRpsList(newList);
        listEditor.setPath(new File(String.format("%s.%s.VCP%d", radarID
                .toUpperCase(),
                newList.getOpMode() == GSM.OP_MODE_CLEAR_AIR ? "clear-air"
                        : "storm", newList.getVcp())));
        listEditor.setUntitled(true);
        listEditor.setDirty(false);
    }

    protected void onClose(ShellEvent e) {
        e.doit = false;
        if (!checkUnsaved())
            return;
        // TODO: reset if just going to make invisible...
        listEditor.dispose();
    }

    private RadarType getRadarTypeRestriction() {
        return listEditor.isTdwrVcp(listEditor.getVcp()) ? RadarType.TDWR
                : RadarType.WSR;
    }

    protected void onSendList() {
        if (!sendCheck())
            return;

        String rpg = RadarApps.chooseRpg(getShell(), true,
                getRadarTypeRestriction(), true);
        if (rpg == null)
            return;
        if (!checkListLength(Arrays.asList(rpg),
                new int[] { listEditor.getVcp() }))
            return;

        RcmClient client = RadarApps.getRcmSystem().getClient();
        SendRpsList msg = new SendRpsList();
        msg.radarIDs = Arrays.asList(rpg);
        msg.requests = Arrays.asList(listEditor.getRpsList().getRequests());
        /* Specify that the RadarServer should accept this list no matter 
         * what VCP the RPG is currently using.
         */
        msg.vcp = RpsList.UNSPECIFIED_VCP;
        String error = null;
        try {
            error = client.sendRequest(msg, 2000).error;
        } catch (IOException e) {
            error = e.toString();
        }

        if (error != null) {
            statusHandler.handle(Priority.ERROR, "Error sending VCP" + msg.vcp
                    + " RPS List to " + rpg, new Throwable(error));
        } else {
            statusHandler.handle(Priority.INFO, "VCP" + msg.vcp
                    + " RPS list sent successfully for " + rpg + " ("
                    + msg.requests.size() + " products)");
        }
        return;
    }

    protected void onAdd() {
        RpsProductDialog d = getProductDialog();
        d.getProductUI().setDefaultRequest();
        if (d.open() == Window.OK) {
            Request req = d.getProductUI().getRequest();

            // Ensure the request is RPS-style
            req.count = Request.CONTINUOUS;
            req.selectCurrent();

            listEditor.getRequestList().add(req);
            listEditor.setDirty(true);
        }
    }

    private RpsProductDialog getProductDialog() {
        RpsProductDialog d = new RpsProductDialog(getShell());
        RadarProductUI ui = d.getProductUI();
        ui.setRadarType(getRadarTypeRestriction());
        ui.setRadarID(listEditor.getRadarID());
        ui.setVcp(listEditor.getVcp());
        return d;
    }

    protected void onEdit() {
        IStructuredSelection sel = (IStructuredSelection) listViewer
                .getSelection();
        if (sel.size() != 1)
            return;
        RpsProductDialog d = getProductDialog();
        Request req = (Request) sel.getFirstElement();
        d.getProductUI().setRequest(req);
        if (d.open() == Window.OK) {
            WritableList l = listEditor.getRequestList();
            for (int i = 0; i < l.size(); ++i) {
                if (l.get(i) == req) {
                    Request newReq = d.getProductUI().getRequest();

                    // Ensure the request is RPS-style
                    newReq.count = Request.CONTINUOUS;
                    newReq.selectCurrent();

                    l.set(i, newReq);
                    listEditor.setDirty(true);
                    break;
                }
            }
        }
    }

    protected void onRemove() {
        IStructuredSelection sel = (IStructuredSelection) listViewer
                .getSelection();
        if (sel.size() > 0) {
            listEditor.getRequestList().removeAll(sel.toList());
            listEditor.setDirty(true);
        }
    }

    // See AWIPS-1 D-2D/src/applications/radar/common/prod-mgmt.tcl :
    // format_product
    protected String getRequestLabel(Request req) {

        RadarProduct rp = ProductInfo.getInstance().getPoductForCode(
                req.productCode);
        Collection<RadarProduct> variants = ProductInfo.getInstance().select(
                new ProductInfo.Selector(null, rp.mnemonic, null, null));
        StringBuilder sb = new StringBuilder();
        if (rp != null) {
            if (rp.name != null)
                sb.append(rp.name);
            /*
             * if (rp.mnemonic != null)
             * sb.append(" (").append(rp.mnemonic).append(')');
             */

            if (rp.levels != null && variants.size() > 1)
                sb.append(", levels ").append(rp.levels);
            if (rp.resolution != null && variants.size() > 1)
                sb.append(", resol ").append(rp.resolution);
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
                if (req.getTimeSpan() != -1)
                    sb.append(", ").append(req.getTimeSpan())
                            .append(" hr span");
                if (req.getEndHour() != -1)
                    sb.append(", End hr ").append(req.getEndHour());
                else
                    sb.append(", Latest");
            }

            if (rp.params.contains(Param.TIME_SPAN_MINUTES)) {
                int thour = req.getTimeSpan() / 60;
                int tmin = req.getTimeSpan() - thour * 60;
                sb.append(String.format(", Time span %d:%02d", thour, tmin));
                if (req.getEndHour() != -1) {
                    int ehour = req.getEndHour() / 60;
                    int emin = req.getEndHour() - ehour * 60;
                    sb.append(String
                            .format(", End time %02d:%02d", ehour, emin));
                } else {
                    sb.append(", Latest");
                }
            }

            if (rp.params.contains(Param.ELEVATION)) {
                sb.append(", ");
                float angle = req.getElevationAngle() / 10.0f;
                switch (req.getElevationSelection()) {
                case Request.SPECIFIC_ELEVATION:
                    sb.append(String.format("elev %.1f%s", angle, listEditor
                            .isTdwrVcp(listEditor.getVcp()) ? ", Cuts One" : ""));
                    break;
                case Request.N_ELEVATIONS:
                    sb.append("elev lowest " + req.getElevationAngle());
                    break;
                case Request.LOWER_ELEVATIONS:
                    sb.append("elev <= " + angle);
                    break;
                case Request.ALL_ELEVATIONS:
                    if (angle == 0f)
                        sb.append("elev all");
                    else
                        sb.append(String.format("elev %.1f, Cuts All", angle));
                    break;
                }
            }
        } else {
            sb.append(String.format("Product #%d", req.productCode));
        }
        return sb.toString();
        // return element.toString();
    }

    private VCPInfo chooseVcp(String message) {
        Collection<VCPInfo> vcps = ElevationInfo.getInstance().getVcpInfo();
        ListDialog ld = new ListDialog(getShell());
        ld.setMessage(message);
        ld.setLabelProvider(new LabelProvider());
        ld.setContentProvider(new ArrayContentProvider());
        ld.setInput(vcps.toArray());

        int result = ld.open();
        if (result == Window.OK)
            return (VCPInfo) ld.getResult()[0];
        else
            return null;
    }

    private void onNewList() {
        if (!checkUnsaved())
            return;

        VCPInfo vcpInfo = chooseVcp("Please select a VCP.");
        if (vcpInfo == null)
            return;
        String radarID = null;
        if (listEditor.isTdwrVcp(vcpInfo.vcp)) {
            radarID = RadarApps.chooseRpg(getShell(), true, RadarType.TDWR,
                    false);
            if (radarID == null)
                return;
        }
        listEditor.newList(radarID, vcpInfo.vcp);
    }

    private boolean checkUnsaved() {
        if (listEditor.isDirty()) {
            // AWIPS-1 had no "Cancel" option
            MessageBox mb = new MessageBox(getShell(), SWT.ICON_QUESTION
                    | SWT.YES | SWT.NO | SWT.CANCEL);
            mb.setText("Modified List");
            mb.setMessage(String.format("Save the RPS List \"%s\"", listEditor
                    .getPath().getName()));

            switch (mb.open()) {
            case SWT.YES:
                return doSave();
            case SWT.CANCEL:
                return false;
            case SWT.NO:
                // nothing;
            }
        }
        return true;
    }

    private boolean doSave() {
        if (listEditor.isUntitled())
            return doSaveAs();
        else
            return listEditor.saveList();
    }

    private boolean doSaveAs() {
        FileDialog fd = new FileDialog(getShell(), SWT.SAVE);
        PathManager pm = (PathManager) PathManagerFactory.getPathManager();
        LocalizationContext context = pm.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
        String fileLocation = pm.getFile(context, File.separator + "rcm")
                .getAbsolutePath();
        File file = pm.getFile(context, "rcm");
        if (!file.exists()) {
            file.mkdir();
        }
        fd.setFilterPath(fileLocation);
        String fn = fd.open();
        if (fn == null)
            return false;
        File f = new File(fn);
        Selector sel = Awips1RpsListUtil.parseName(f.getName());
        if (sel == null || sel.vcp != listEditor.getVcp()) {
            // Split off everything before the . in the filename and add the VCP
            // and extension
            fn = f.getParent() + "/" + f.getName().split("\\.")[0] + ".VCP"
                    + listEditor.getVcp() + ".rps";
            f = new File(fn);
        }

        listEditor.setPath(new File(fn));
        listEditor.setUntitled(false);
        return listEditor.saveList();
    }

    private void onStoreList() {
        if (!sendCheck())
            return;

        StoreDialog sd = new StoreDialog(getShell(), listEditor);
        if (sd.open() == Window.OK) {
            if (!checkListLength(sd.getRadarIDs(), sd.getVcps()))
                return;

            RcmClient client = RadarApps.getRcmSystem().getClient();
            for (int vcp : sd.getVcps()) {
                SendRpsList msg = new SendRpsList();
                msg.radarIDs = sd.getRadarIDs();
                if (msg.radarIDs.size() == 0)
                    return;
                msg.requests = Arrays.asList(listEditor.getRpsList()
                        .getRequests());
                msg.vcp = vcp;
                msg.store = true;

                String error = null;
                try {
                    ReplyObj ro = client.sendRequest(msg, 2000);
                    error = ro.error;
                } catch (IOException e) {
                    error = e.toString();
                }
                if (error != null) {
                    MessageBox mb = new MessageBox(getShell(), SWT.ICON_ERROR
                            | SWT.OK);
                    mb.setText("Error");
                    mb.setMessage(error);
                    mb.open();
                } else
                    listEditor.setDirty(false);
            }
        }
    }

    private boolean sendCheck() {
        if (listEditor.getRequestList().size() >= 1)
            return true;
        else {
            MessageBox mb = new MessageBox(getShell(), SWT.ICON_ERROR | SWT.OK);
            mb.setText("Send List");
            mb.setMessage("You cannot send an empty RPS list to the RPGs.");
            mb.open();
            return false;
        }
    }

    private int getMaxRpsListSize(String radarID) {
        int result = -1;
        ConfigReply reply = (ConfigReply) RadarApps.getRcmSystem()
                .sendCheckedAndHandled(
                        ReqObj.getRadarConfig(listEditor.getRadarID()), shell);
        if (reply != null && reply.config.size() > 0) {
            /*
             * We do not know which link is being used, but the max length will
             * be the same for all links in practice.
             */
            for (LinkResource lr : reply.config.iterator().next()
                    .getLinkResources()) {
                result = Math.max(lr.getMaxRpsListSize(), result);
            }
        }
        return result;
    }

    private boolean checkListLength(List<String> radarIDs, int[] vcps) {
        RpsList rpsList = listEditor.getRpsList();

        for (String rpg : radarIDs) {
            int maxRpsListSize = getMaxRpsListSize(rpg);
            for (int vcp : vcps) {
                rpsList.setVcp(vcp);
                RequestCounts counts = getRequestCounts(rpsList, rpg);

                if (maxRpsListSize < 0 || counts.getTotalRequestCount() < 0
                        || counts.getTotalRequestCount() <= maxRpsListSize) {
                    // nothing
                } else {
                    String error = null;
                    error = String.format("The RPS list which has %s in "
                            + "VCP %d cannot be sent to %s which accepts a "
                            + "maximum of %d requests.  Adjust the RPS list "
                            + "and/or the set of selected RPGs.",
                            counts.getDescription(), vcp, rpg, maxRpsListSize);
                    MessageBox mb = new MessageBox(getShell(), SWT.ICON_ERROR
                            | SWT.OK);
                    mb.setText("Error");
                    mb.setMessage(error);
                    mb.open();
                    return false;
                }
            }
        }
        return true;
    }

    private void onOpenList() {
        FileDialog fd = new FileDialog(getShell(), SWT.OPEN);
        fd.setFilterPath("/data/fxa/rps-lists"); // TODO:
        String fn = fd.open();
        if (fn == null)
            return;

        File f = new File(fn);

        RpsList newList = null;
        Exception exc = null;
        int opMode = -1;
        int vcp = RpsList.UNSPECIFIED_VCP;
        Selector sel = null;
        try {
            sel = Awips1RpsListUtil.parseName(f.getName());
            FileInputStream fis = new FileInputStream(f);
            ByteBuffer buf;
            try {
                FileChannel fc = fis.getChannel();
                buf = ByteBuffer.allocate((int) fc.size());
                fc.read(buf);
            } finally {
                fis.close();
            }

            if (sel != null) {
                opMode = sel.opMode;
                vcp = sel.vcp;
            }

            newList = Util.parseRpsListData(buf.array(), opMode, vcp);

        } catch (IOException e) {
            exc = e;
        } catch (JAXBException e) {
            exc = e;
        } catch (RuntimeException e) {
            exc = e;
        }
        if (exc != null) {
            showError(String.format("Could not read '%s': %s", fn, exc));
            return;
        }

        if (newList.getVcp() == RpsList.UNSPECIFIED_VCP) {
            VCPInfo vcpInfo = chooseVcp("Could not determine VCP from the file.  Please select a VCP.");
            if (vcpInfo == null)
                return;
            newList.setVcp(vcpInfo.vcp);
        }

        // Ensure all entries are RPS-style
        for (Request req : newList.getRequests()) {
            req.count = Request.CONTINUOUS;
            req.selectCurrent();
        }

        replaceRpsList(newList);
        if (sel != null)
            listEditor.setRadarID(sel.radar);
        else
            listEditor.setRadarID(null);
        listEditor.setPath(f);
        listEditor.setUntitled(false);
        listEditor.setDirty(false);
    }

    void showError(String error) {
        MessageBox mb = new MessageBox(getShell(), SWT.ICON_ERROR | SWT.OK);
        mb.setText("Error");
        mb.setMessage(error);
        mb.open();
    }

    private void replaceRpsList(RpsList newList) {
        replaceRpsList(newList.getVcp(), Arrays.asList(newList.getRequests()));
    }

    private void replaceRpsList(int vcp, List<Request> list) {
        listEditor.setVcp(vcp);
        listEditor.getRequestList().clear();
        listEditor.getRequestList().addAll(list);
    }

    private Shell getShell() {
        return shell;
    }

    public void open() {
        shell.open();
    }

    public RequestCounts getRequestCounts(RpsList rpsList, String radarID) {
        int entryCount;
        int baseRequestCount;
        int withMiniVolumeCount;

        entryCount = rpsList.getRequests().length;
        baseRequestCount = rpsList.getRequestCount(radarID,
                listEditor.getTypeRestriction());
        withMiniVolumeCount = baseRequestCount;
        if (rpsList.getVcp() == 80 && withMiniVolumeCount >= 0)
            withMiniVolumeCount += rpsList
                    .getAdditionalMiniVolumeProductCount();

        return new RequestCounts(entryCount, baseRequestCount,
                withMiniVolumeCount);
    }

    private static class RequestCounts {
        private int entryCount;

        private int baseRequestCount;

        private int withMiniVolumeCount;

        public RequestCounts(int entryCount, int baseRequestCount,
                int withMiniVolumeCount) {
            this.entryCount = entryCount;
            this.baseRequestCount = baseRequestCount;
            this.withMiniVolumeCount = withMiniVolumeCount;
        }

        public String getDescription() {
            StringBuilder sb = new StringBuilder();
            sb.append(String.format("%d entries", entryCount));
            if (entryCount != baseRequestCount
                    || entryCount != withMiniVolumeCount) {
                if (baseRequestCount >= 0) {
                    String mvMessage = "";
                    if (baseRequestCount != withMiniVolumeCount)
                        mvMessage = String.format(
                                ", %d with mini-volume additions",
                                withMiniVolumeCount);
                    sb.append(String.format(" (%d requests%s)",
                            baseRequestCount, mvMessage));
                } else
                    sb.append(" (cannot determine the number of requests)");
            }
            return sb.toString();
        }

        public int getTotalRequestCount() {
            return withMiniVolumeCount;
        }
    }

}
