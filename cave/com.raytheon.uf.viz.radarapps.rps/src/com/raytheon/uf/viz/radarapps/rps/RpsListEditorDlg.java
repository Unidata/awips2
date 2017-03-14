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
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

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
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.ShellAdapter;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.graphics.GC;
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
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.dialogs.ListDialog;

import com.raytheon.rcm.config.LinkResource;
import com.raytheon.rcm.config.RadarType;
import com.raytheon.rcm.config.RcmUtil;
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
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.radarapps.client.RcmClient;
import com.raytheon.uf.viz.radarapps.client.RcmWaiter;
import com.raytheon.uf.viz.radarapps.core.RadarApps;
import com.raytheon.uf.viz.radarapps.products.ui.RadarProductUI;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

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
 * 2015-06-10   4498       nabowle     Rename Util->RcmUtil
 * 2016-01-20   5271       bkowal      Code cleanup.
 * 2016-03-28   5511       dgilling    Renamed from ListEditorWindow, 
 *                                     rewritten based on CaveSWTDialog.
 * </pre>
 * 
 */
public class RpsListEditorDlg extends CaveSWTDialog {

    private static final String VIEW_ERROR_MSG_FORMAT = "Could not retrieve the RPS list: %s";

    private final IUFStatusHandler statusHandler = UFStatus
            .getHandler(getClass());

    private final RpsListRequestContainer requestContainer;

    private IChangeListener listChangedListener;

    private ListViewer list;

    private Label numberOfProductsLabel;

    protected RpsListEditorDlg(Shell parent, RpsListRequestContainer requestContainer) {
        super(parent, SWT.DIALOG_TRIM | SWT.MAX | SWT.MIN | SWT.RESIZE,
                CAVE.INDEPENDENT_SHELL);

        this.requestContainer = requestContainer;
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setText(getWindowTitle());
        shell.setLayout(new GridLayout(1, true));
        shell.addShellListener(new ShellAdapter() {
            @Override
            public void shellClosed(ShellEvent e) {
                onClose(e);
            }
        });

        Menu mb = constructMenu(shell);
        shell.setMenuBar(mb);

        constructButtonBar(shell);

        list = constructListControl(shell);

        constructStatusBarControl(shell);
    }

    private ListViewer constructListControl(Shell shell) {
        ListViewer lv = new ListViewer(shell);
        lv.setContentProvider(new ObservableListContentProvider());
        lv.setLabelProvider(new LabelProvider() {
            @Override
            public String getText(Object element) {
                return getRequestLabel((Request) element);
            }
        });
        lv.setInput(requestContainer.getRequestList());
        lv.addDoubleClickListener(new IDoubleClickListener() {
            @Override
            public void doubleClick(DoubleClickEvent event) {
                onEdit();
            }
        });

        GC gc = new GC(lv.getControl());
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = gc.getFontMetrics().getAverageCharWidth() * 65;
        gd.heightHint = lv.getList().getItemHeight() * 20;
        gc.dispose();
        lv.getControl().setLayoutData(gd);

        return lv;
    }

    private Menu constructMenu(Shell shell) {
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
                getShell().close();
            }
        });

        mi = new MenuItem(mb, SWT.CASCADE);
        mi.setText("List");
        m = new Menu(shell, SWT.DROP_DOWN);
        mi.setMenu(m);

        mi = new MenuItem(m, SWT.PUSH);
        mi.setText("Add Product...");
        mi.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                onAdd();
            }
        });

        mi = new MenuItem(m, SWT.PUSH);
        mi.setText("Edit Product...");
        mi.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                onEdit();
            }
        });

        mi = new MenuItem(m, SWT.PUSH);
        mi.setText("Remove Product(s)");
        mi.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                onRemove();
            }
        });

        mi = new MenuItem(m, SWT.SEPARATOR);

        mi = new MenuItem(m, SWT.PUSH);
        mi.setText("Send List...");
        mi.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                onSendList();
            }
        });

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
                            requestContainer.isTdwrVcp(vcp) ? "TDWR"
                                    : "WSR-88D"));
                    mi.addSelectionListener(new SelectionAdapter() {
                        @Override
                        public void widgetSelected(SelectionEvent e) {
                            onViewCurrentList(vcp);
                        }
                    });

                }
            }
        }

        return mb;
    }

    private void constructButtonBar(Shell shell) {
        Composite buttonBar = new Composite(shell, SWT.NONE);
        buttonBar.setLayout(new RowLayout(SWT.HORIZONTAL));
        buttonBar.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));

        Button b = new Button(buttonBar, SWT.PUSH);
        b.setText("Add");
        b.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                onAdd();
            }
        });

        b = new Button(buttonBar, SWT.PUSH);
        b.setText("Edit");
        b.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                onEdit();
            }
        });

        b = new Button(buttonBar, SWT.PUSH);
        b.setText("Remove");
        b.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                onRemove();
            }
        });

        b = new Button(buttonBar, SWT.PUSH);
        b.setText("Send");
        b.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                onSendList();
            }
        });
    }

    private void constructStatusBarControl(Shell shell) {
        Group statusBar = new Group(shell, SWT.SHADOW_IN);
        statusBar.setLayout(new FillLayout());
        statusBar.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false));

        numberOfProductsLabel = new Label(statusBar, SWT.LEFT);
        numberOfProductsLabel.setText(getStatusBarText());
        listChangedListener = new IChangeListener() {

            @Override
            public void handleChange(ChangeEvent event) {
                if ((numberOfProductsLabel != null)
                        && (!numberOfProductsLabel.isDisposed())) {
                    numberOfProductsLabel.setText(getStatusBarText());
                }
            }
        };
        requestContainer.getRequestList()
                .addChangeListener(listChangedListener);
    }

    @Override
    protected void disposed() {
        requestContainer.getRequestList().removeChangeListener(
                listChangedListener);
        super.disposed();
    }

    private String getWindowTitle() {
        StringBuilder sb = new StringBuilder();
        sb.append("RPS List Editor: ");
        if (requestContainer.isDirty()) {
            sb.append("* ");
        }
        if ((requestContainer.getPath() != null)) {
            sb.append(requestContainer.getPath().getName());
        }

        return sb.toString();
    }

    private String getStatusBarText() {
        return getRequestCounts(requestContainer.getRpsList(),
                requestContainer.getRadarID()).getDescription();
    }

    private int getOpModeForVcp(int vcp) {
        Collection<VCPInfo> vcpInfo = ElevationInfo.getInstance().getVcpInfo();
        for (VCPInfo vi : vcpInfo) {
            if (vi.vcp == vcp) {
                return vi.opMode;
            }
        }
        return -1;
    }

    private void onViewCurrentList(int vcp) {
        if (isUnsaved()) {
            return;
        }

        RadarType type = null;
        if (vcp != -1) {
            type = requestContainer.isTdwrVcp(vcp) ? RadarType.TDWR
                    : RadarType.WSR;
        }
        String radarID = RadarApps.chooseRpg(getShell(), true, type, false);
        if (radarID == null) {
            return;
        }

        GetRpsList req = new GetRpsList();
        req.radarID = radarID;
        if (vcp != -1) {
            req.vcp = vcp;
            req.opMode = getOpModeForVcp(vcp);
        }

        String error = null;
        RpsList newList = null;
        try {
            // TODO: needs to be more forgiving...
            RcmWaiter waiter = new RcmWaiter(req, getShell());
            ReplyObj reply = waiter.send();
            if (reply == null) {
                return;
            }

            if (reply.error == null) {
                newList = ((RpsListReply) reply).rpsList;
                if (newList == null) {
                    if (vcp == -1) {
                        error = String
                                .format(VIEW_ERROR_MSG_FORMAT,
                                        "There is no current RPS list.  Most likely there is no connection to the RPG.");
                    } else {
                        error = String
                                .format(VIEW_ERROR_MSG_FORMAT,
                                        "The specified list does not exist.  Most likely there is no connection to the RPG.");
                    }
                }
            } else {
                error = String.format(VIEW_ERROR_MSG_FORMAT, reply.error);
            }
        } catch (IOException e) {
            String errorMsg = String.format(VIEW_ERROR_MSG_FORMAT,
                    e.getLocalizedMessage());
            statusHandler.error(errorMsg, e);
            return;
        }

        if (error != null) {
            statusHandler.error(error);
            showError(error);
            return;
        }

        String opMode = (newList.getOpMode() == GSM.OP_MODE_CLEAR_AIR) ? "clear-air"
                : "storm";
        String fileName = String.format("%s.%s.VCP%d", radarID.toUpperCase(),
                opMode, newList.getVcp());
        requestContainer
                .replaceList(newList, radarID, new File(fileName), true);

        setText(getWindowTitle());
    }

    private void onClose(ShellEvent e) {
        e.doit = !isUnsaved();
        return;
    }

    private RadarType getRadarTypeRestriction() {
        return requestContainer.isTdwrVcp(requestContainer.getVcp()) ? RadarType.TDWR
                : RadarType.WSR;
    }

    private void onSendList() {
        if (!shouldSend()) {
            return;
        }

        String rpg = RadarApps.chooseRpg(getShell(), true,
                getRadarTypeRestriction(), true);
        if (rpg == null) {
            return;
        }
        if (!checkListLength(Arrays.asList(rpg),
                new int[] { requestContainer.getVcp() })) {
            return;
        }

        RcmClient client = RadarApps.getRcmSystem().getClient();
        SendRpsList msg = new SendRpsList();
        msg.radarIDs = Arrays.asList(rpg);
        msg.requests = Arrays.asList(requestContainer.getRpsList()
                .getRequests());
        /*
         * Specify that the RadarServer should accept this list no matter what
         * VCP the RPG is currently using.
         */
        msg.vcp = RpsList.UNSPECIFIED_VCP;
        try {
            String error = client.sendRequest(msg, 2000).error;

            if (error != null) {
                statusHandler.error("Error sending VCP" + msg.vcp
                        + " RPS List to " + rpg + ": " + error);
            } else {
                statusHandler.info("VCP" + msg.vcp
                        + " RPS list sent successfully for " + rpg + " ("
                        + msg.requests.size() + " products)");
            }
        } catch (IOException e) {
            statusHandler.error("Error sending VCP" + msg.vcp + " RPS List to "
                    + rpg, e);
        }

        return;
    }

    private void onAdd() {
        RpsProductDialog d = getProductDialog();
        d.getProductUI().setDefaultRequest();
        if (d.open() == Window.OK) {
            Request req = d.getProductUI().getRequest();

            // Ensure the request is RPS-style
            req.count = Request.CONTINUOUS;
            req.selectCurrent();

            requestContainer.add(req);
        }

        setText(getWindowTitle());
    }

    private RpsProductDialog getProductDialog() {
        RpsProductDialog d = new RpsProductDialog(getShell());
        RadarProductUI ui = d.getProductUI();
        ui.setRadarType(getRadarTypeRestriction());
        ui.setRadarID(requestContainer.getRadarID());
        ui.setVcp(requestContainer.getVcp());
        return d;
    }

    private void onEdit() {
        IStructuredSelection sel = (IStructuredSelection) list.getSelection();
        if (sel.size() != 1) {
            return;
        }

        RpsProductDialog d = getProductDialog();
        Request req = (Request) sel.getFirstElement();
        d.getProductUI().setRequest(req);
        if (d.open() == Window.OK) {
            WritableList l = requestContainer.getRequestList();
            for (int i = 0; i < l.size(); ++i) {
                if (l.get(i) == req) {
                    Request newReq = d.getProductUI().getRequest();

                    // Ensure the request is RPS-style
                    newReq.count = Request.CONTINUOUS;
                    newReq.selectCurrent();

                    requestContainer.setItem(i, newReq);
                    break;
                }
            }
        }

        setText(getWindowTitle());
    }

    private void onRemove() {
        IStructuredSelection sel = (IStructuredSelection) list.getSelection();
        if (sel.size() > 0) {
            requestContainer.removeAll(sel.toList());
        }

        setText(getWindowTitle());
    }

    /*
     * See AWIPS-1 D-2D/src/applications/radar/common/prod-mgmt.tcl :
     * format_product
     */
    private String getRequestLabel(Request req) {
        RadarProduct rp = ProductInfo.getInstance().getPoductForCode(
                req.productCode);
        StringBuilder sb = new StringBuilder();
        if (rp != null) {
            Collection<RadarProduct> variants = ProductInfo.getInstance()
                .select(new ProductInfo.Selector(null, rp.mnemonic, null,
                        null));
            if (rp.name != null) 
                sb.append(rp.name);

            if (rp.levels != null && variants.size() > 1) {
                sb.append(", levels ").append(rp.levels);
            }
            if (rp.resolution != null && variants.size() > 1) {
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
                    sb.append(String.format("elev %.1f%s", angle,
                            requestContainer.isTdwrVcp(requestContainer
                                    .getVcp()) ? ", Cuts One" : ""));
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
    }

    private VCPInfo chooseVcp(String message) {
        Collection<VCPInfo> vcps = ElevationInfo.getInstance().getVcpInfo();
        ListDialog ld = new ListDialog(getShell());
        ld.setMessage(message);
        ld.setLabelProvider(new LabelProvider());
        ld.setContentProvider(new ArrayContentProvider());
        ld.setInput(vcps.toArray());

        int result = ld.open();
        if (result == Window.OK) {
            return (VCPInfo) ld.getResult()[0];
        } else {
            return null;
        }
    }

    private void onNewList() {
        if (isUnsaved()) {
            return;
        }

        VCPInfo vcpInfo = chooseVcp("Please select a VCP.");
        if (vcpInfo == null) {
            return;
        }
        String radarID = null;
        if (requestContainer.isTdwrVcp(vcpInfo.vcp)) {
            radarID = RadarApps.chooseRpg(getShell(), true, RadarType.TDWR,
                    false);
            if (radarID == null) {
                return;
            }
        }
        requestContainer.newList(radarID, vcpInfo.vcp);

        setText(getWindowTitle());
    }

    private boolean isUnsaved() {
        if (requestContainer.isDirty()) {
            // AWIPS-1 had no "Cancel" option
            MessageBox mb = new MessageBox(getShell(), SWT.ICON_QUESTION
                    | SWT.YES | SWT.NO | SWT.CANCEL);
            mb.setText("Modified List");
            mb.setMessage(String.format("Save the RPS List \"%s\"",
                    requestContainer.getPath().getName()));

            switch (mb.open()) {
            case SWT.YES:
                return !doSave();
            case SWT.CANCEL:
                return true;
            case SWT.NO:
                // nothing;
            }
        }

        return false;
    }

    private boolean doSave() {
        if (requestContainer.isUntitled()) {
            return doSaveAs();
        } else {
            try {
                return requestContainer.saveList();
            } catch (IOException e) {
                String msg = String.format("Could not write list to '%s': %s",
                        requestContainer.getPath(), e.getLocalizedMessage());
                statusHandler.error(msg, e);
                showError(msg);
            }
        }

        setText(getWindowTitle());

        return false;
    }

    private boolean doSaveAs() {
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext context = pm.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.SITE);
        File defaultDir = pm.getFile(context, "rcm");
        try {
            Files.createDirectories(defaultDir.toPath());
        } catch (IOException e) {
            String msg = String.format("Could not create directory '%s': %s",
                    defaultDir, e.getLocalizedMessage());
            statusHandler.error(msg, e);
            showError(msg);
            return false;
        }

        FileDialog fd = new FileDialog(getShell(), SWT.SAVE);
        fd.setFilterPath(defaultDir.getAbsolutePath());
        fd.setFilterExtensions(new String[] { "*.rps" });
        String fn = fd.open();
        if (fn == null) {
            return false;
        }
        Path userFilePath = Paths.get(fn);

        Selector sel = Awips1RpsListUtil.parseName(userFilePath.getFileName()
                .toString());
        if (sel == null || sel.vcp != requestContainer.getVcp()) {
            /*
             * Split off everything before the . in the filename and add the VCP
             * and extension
             */
            String properFileName = String.format("%s.VCP%d.rps", userFilePath
                    .getFileName().toString().split("\\.")[0],
                    requestContainer.getVcp());
            userFilePath = userFilePath.resolveSibling(properFileName);
        }
        boolean success = false;
        try {
            success = requestContainer.saveList(userFilePath.toFile());
            setText(getWindowTitle());
        } catch (IOException e) {
            String msg = String.format("Could not write list to '%s': %s",
                    userFilePath, e.getLocalizedMessage());
            statusHandler.error(msg, e);
            showError(msg);
        }
        return success;
    }

    private boolean shouldSend() {
        if (requestContainer.getRequestList().size() >= 1) {
            return true;
        } else {
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
                        ReqObj.getRadarConfig(requestContainer.getRadarID()),
                        shell);
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
        RpsList rpsList = requestContainer.getRpsList();

        for (String rpg : radarIDs) {
            int maxRpsListSize = getMaxRpsListSize(rpg);
            for (int vcp : vcps) {
                rpsList.setVcp(vcp);
                RequestCounts counts = getRequestCounts(rpsList, rpg);

                if ((maxRpsListSize >= 0)
                        && (counts.getTotalRequestCount() >= 0)
                        && (counts.getTotalRequestCount() > maxRpsListSize)) {
                    String error = String
                            .format("The RPS list which has %s in "
                                    + "VCP %d cannot be sent to %s which accepts a "
                                    + "maximum of %d requests.  Adjust the RPS list "
                                    + "and/or the set of selected RPGs.",
                                    counts.getDescription(), vcp, rpg,
                                    maxRpsListSize);
                    statusHandler.error(error);
                    showError(error);
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
        if (fn == null) {
            return;
        }

        RpsList newList = null;
        int opMode = -1;
        int vcp = RpsList.UNSPECIFIED_VCP;
        Selector sel = null;
        Path filePath = Paths.get(fn);
        try {
            sel = Awips1RpsListUtil
                    .parseName(filePath.getFileName().toString());
            byte[] fileContents = Files.readAllBytes(filePath);

            if (sel != null) {
                opMode = sel.opMode;
                vcp = sel.vcp;
            }

            newList = RcmUtil.parseRpsListData(fileContents, opMode, vcp);
        } catch (Exception e) {
            String errorMsg = String.format("Could not read '%s': %s", fn,
                    e.getLocalizedMessage());
            statusHandler.error(errorMsg, e);
            showError(errorMsg);
            return;
        }

        if (newList.getVcp() == RpsList.UNSPECIFIED_VCP) {
            VCPInfo vcpInfo = chooseVcp("Could not determine VCP from the file.  Please select a VCP.");
            if (vcpInfo == null) {
                return;
            }
            newList.setVcp(vcpInfo.vcp);
        }

        // Ensure all entries are RPS-style
        for (Request req : newList.getRequests()) {
            req.count = Request.CONTINUOUS;
            req.selectCurrent();
        }

        String radarID = (sel != null) ? sel.radar : null;
        requestContainer.replaceList(newList, radarID, filePath.toFile());

        setText(getWindowTitle());
    }

    private void showError(String error) {
        MessageBox mb = new MessageBox(getShell(), SWT.ICON_ERROR | SWT.OK);
        mb.setText("Error");
        mb.setMessage(error);
        mb.open();
    }

    private RequestCounts getRequestCounts(RpsList rpsList, String radarID) {
        int entryCount = rpsList.getRequests().length;
        int baseRequestCount = rpsList.getRequestCount(radarID,
                requestContainer.getTypeRestriction());
        int withMiniVolumeCount = baseRequestCount;
        if ((rpsList.getVcp() == 80) && (withMiniVolumeCount >= 0)) {
            withMiniVolumeCount += rpsList
                    .getAdditionalMiniVolumeProductCount();
        }

        return new RequestCounts(entryCount, baseRequestCount,
                withMiniVolumeCount);
    }

    private static class RequestCounts {

        private final int entryCount;

        private final int baseRequestCount;

        private final int withMiniVolumeCount;

        public RequestCounts(int entryCount, int baseRequestCount,
                int withMiniVolumeCount) {
            this.entryCount = entryCount;
            this.baseRequestCount = baseRequestCount;
            this.withMiniVolumeCount = withMiniVolumeCount;
        }

        public String getDescription() {
            StringBuilder sb = new StringBuilder();
            sb.append(entryCount).append(" entries");
            if (entryCount != baseRequestCount
                    || entryCount != withMiniVolumeCount) {
                if (baseRequestCount >= 0) {
                    sb.append(" (").append(baseRequestCount)
                            .append(" requests)");

                    if (baseRequestCount != withMiniVolumeCount) {
                        sb.append(", ").append(withMiniVolumeCount)
                                .append(" with mini-volume additions");
                    }

                } else {
                    sb.append(" (cannot determine the number of requests)");
                }
            }
            return sb.toString();
        }

        public int getTotalRequestCount() {
            return withMiniVolumeCount;
        }
    }
}
