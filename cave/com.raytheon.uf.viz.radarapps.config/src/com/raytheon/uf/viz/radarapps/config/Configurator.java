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
package com.raytheon.uf.viz.radarapps.config;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.databinding.observable.list.WritableList;
import org.eclipse.jface.databinding.viewers.ObservableListContentProvider;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITableColorProvider;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.rcm.config.LinkResource;
import com.raytheon.rcm.config.RadarConfig;
import com.raytheon.rcm.event.ConfigEvent;
import com.raytheon.rcm.event.NotificationEvent;
import com.raytheon.rcm.event.RadarEvent;
import com.raytheon.rcm.event.RadarEventListener;
import com.raytheon.rcm.message.GSM;
import com.raytheon.rcm.message.Message;
import com.raytheon.rcm.message.MessageFormatException;
import com.raytheon.rcm.mqsrvr.ReplyObj;
import com.raytheon.rcm.mqsrvr.ReqObj;
import com.raytheon.rcm.mqsrvr.ReplyObj.GlobalConfigReply;
import com.raytheon.rcm.mqsrvr.ReplyObj.ROStatus;
import com.raytheon.rcm.mqsrvr.ReqObj.SetGlobalConfig;
import com.raytheon.rcm.mqsrvr.ReqObj.SetRadarConfig;
import com.raytheon.uf.viz.radarapps.client.RcmClientExceptionListener;
import com.raytheon.uf.viz.radarapps.client.RcmSystem;
import com.raytheon.uf.viz.radarapps.core.RadarApps;

public class Configurator extends Dialog implements RadarEventListener,
        RcmClientExceptionListener {

    private Label statusLabel;

    private TableViewer statusTable;

    private Image dots[] = new Image[4];

    private WritableList statusList = new WritableList();

    private Button enableDisableButton;

    private Button repurposeButton;

    private Button enableCollectionButton;

    private Button configureRadarButton;

    private enum Separator {
        DEDICATED("-- Dedicated Radars --"), DIAL("-- Dial Radars --"), NONE_DEFINED(
                "No RPGs defined");
        public final String label;

        private Separator(String label) {
            this.label = label;
        }
    }

    private static final int DOT_EMPTY = 0;

    private static final int DOT_GREEN = 1;

    private static final int DOT_YELLOW = 2;

    private static final int DOT_RED = 3;

    private class Bit {
        int connEvent = -1;

        RadarConfig config;

        ROStatus status;

        int vcp; // cached from GSM
    }

    public Configurator() {
        super((Shell) null);
        setBlockOnOpen(false);
        setShellStyle(getShellStyle() & ~SWT.APPLICATION_MODAL);
    }

    public void show() {
        open();
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        getShell().addDisposeListener(new DisposeListener() {
            public void widgetDisposed(DisposeEvent e) {
                onDispose();
            }
        });

        RadarApps.getRcmSystem().getClient().addEventListener(this);
        RadarApps.getRcmSystem().getClient().addRcmClientListener(this);

        ImageData id = new ImageData(Configurator.class
                .getResourceAsStream("white.png"));
        dots[DOT_EMPTY] = new Image(parent.getDisplay(), id);
        id = new ImageData(Configurator.class.getResourceAsStream("green.png"));
        dots[DOT_GREEN] = new Image(parent.getDisplay(), id);
        id = new ImageData(Configurator.class.getResourceAsStream("yellow.png"));
        dots[DOT_YELLOW] = new Image(parent.getDisplay(), id);
        id = new ImageData(Configurator.class.getResourceAsStream("red.png"));
        dots[DOT_RED] = new Image(parent.getDisplay(), id);

        Composite c = (Composite) super.createDialogArea(parent);
        GridLayout gl = (GridLayout) c.getLayout();
        gl.numColumns = 2;

        GridData gd;
        Composite r;

        Label l;
        Button b;

        statusLabel = new Label(c, SWT.LEFT | SWT.WRAP);
        statusLabel
                .setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, true));

        b = new Button(c, SWT.PUSH);
        b.setText("Global Settings...");
        b.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                onGlobalConfig();
            }
        });
        gd = new GridData(SWT.RIGHT, SWT.CENTER, false, false);
        b.setLayoutData(gd);

        l = new Label(c, SWT.LEFT);
        l.setText("RPGs");
        gd = new GridData();
        gd.verticalIndent = convertHeightInCharsToPixels(1);
        l.setLayoutData(gd);

        l = new Label(c, SWT.LEFT); // pad out the cell

        statusTable = new TableViewer(c, SWT.H_SCROLL | SWT.V_SCROLL
                | SWT.BORDER);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = convertWidthInCharsToPixels(30);
        statusTable.getControl().setLayoutData(gd);
        statusTable.setContentProvider(new ObservableListContentProvider());
        statusTable.setLabelProvider(new StatusLabelProvider());
        statusTable
                .addSelectionChangedListener(new ISelectionChangedListener() {
                    public void selectionChanged(SelectionChangedEvent event) {
                        onSelectionChanged();
                    }
                });
        statusTable.setInput(statusList);

        r = new Composite(c, SWT.NONE);
        RowLayout rl = new RowLayout(SWT.VERTICAL);
        rl.pack = false;
        r.setLayout(rl);
        b = new Button(r, SWT.PUSH);
        b.setText("Enable / Disable");
        b.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                onEnableDisable();
            }
        });
        enableDisableButton = b;

        b = new Button(r, SWT.PUSH);
        b.setText("Enable Transmission of\nProducts for Distribution");
        b.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                onEnableCollection();
            }
        });
        enableCollectionButton = b;

        b = new Button(r, SWT.PUSH);
        b.setText("Repurpose for \nBackup Transmission");
        b.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                onRepurpose();
            }
        });
        repurposeButton = b;

        b = new Button(r, SWT.PUSH);
        b.setText("RPG Settings...");
        b.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                onConfigureRadar();
            }
        });
        configureRadarButton = b;

        /*
         * b = new Button(r, SWT.PUSH); b.setText("Add/Remove RPGs");
         * b.setEnabled(false);
         */

        getShell().getDisplay().asyncExec(new Runnable() {
            public void run() {
                onSelectionChanged();
                fillList();
            }
        });

        return c;
    }

    protected void onConfigureRadar() {
        Bit bit = getSelectedBit();
        if (bit == null)
            return;

        RadarConfig rc = bit.config.duplicate();
        RadarConfigDialog d = new RadarConfigDialog(getShell(), rc);
        if (d.open() == Window.OK) {
            SetRadarConfig req = new SetRadarConfig();
            req.radarID = rc.getRadarID();
            req.config = rc;
            RadarApps.getRcmSystem().sendCheckedAndHandled(req, getShell());
        }
    }

    protected void onDispose() {
        for (Image img : dots)
            if (img != null)
                img.dispose();
        RadarApps.getRcmSystem().getClient().removeEventListener(this);
        RadarApps.getRcmSystem().getClient().removeRcmClientListener(this);
    }

    private Bit getSelectedBit() {
        Object obj = ((IStructuredSelection) statusTable.getSelection())
                .getFirstElement();

        if (obj instanceof Bit)
            return (Bit) obj;
        else
            return null;
    }

    private enum Repurposing {
        TO_BACKUP, TO_ASSOCIATED
    }

    Repurposing canReupurpose(RadarConfig rc) {
        boolean ok = false;
        if (rc.getLinkResources() != null)
            for (LinkResource lr : rc.getLinkResources()) {
                if (lr.isDedicated() != rc.isDedicated()) {
                    ok = true;
                    break;
                }
            }
        if (ok)
            return rc.isDedicated() ? Repurposing.TO_ASSOCIATED
                    : Repurposing.TO_BACKUP;
        else
            return null;
    }

    protected void onSelectionChanged() {
        Bit bit = getSelectedBit();

        if (bit != null) {
            enableDisableButton.setText(bit.config.isEnabled() ? "Disable"
                    : "Enable");
            enableDisableButton.setEnabled(true);

            if (bit.config.isDedicated()) {
                enableCollectionButton.setEnabled(true);
                String s = " Transmission of\nProducts for Distribution";
                enableCollectionButton.setText((bit.config
                        .isCollectionEnabled() ? "Disable" : "Enable")
                        + s);
            } else
                enableCollectionButton.setEnabled(false);

            Repurposing rep = canReupurpose(bit.config);
            if (rep != null) {
                repurposeButton.setEnabled(true);
                repurposeButton
                        .setText(rep == Repurposing.TO_ASSOCIATED ? "Repurpose for\nDial Access"
                                : "Repurpose for \nBackup Transmission");
            } else
                repurposeButton.setEnabled(false);

            configureRadarButton.setEnabled(true);
        } else {
            enableDisableButton.setEnabled(false);
            enableCollectionButton.setEnabled(false);
            repurposeButton.setEnabled(false);
            configureRadarButton.setEnabled(false);
        }
    }

    protected void onEnableCollection() {
        Bit bit = getSelectedBit();
        if (bit != null) {
            RadarConfig rc = bit.config.duplicate();
            rc.setCollectionEnabled(!rc.isCollectionEnabled());
            SetRadarConfig req = new SetRadarConfig();
            req.radarID = rc.getRadarID();
            req.config = rc;
            RadarApps.getRcmSystem().sendCheckedAndHandled(req, getShell());
        }
    }

    protected void onRepurpose() {
        Bit bit = getSelectedBit();
        if (bit != null) {
            RadarConfig rc = bit.config.duplicate();

            Repurposing rep = canReupurpose(rc);
            if (rep != null) {
                if (rep == Repurposing.TO_ASSOCIATED) {
                    rc.setDedicated(false);
                } else {
                    rc.setDedicated(true);
                    rc.setCollectionEnabled(true);
                }

                SetRadarConfig req = new SetRadarConfig();
                req.radarID = rc.getRadarID(); // TODO: hey this redundant...
                req.config = rc;
                RadarApps.getRcmSystem().sendCheckedAndHandled(req, getShell());
            }
        }
    }

    protected void onEnableDisable() {
        Bit bit = getSelectedBit();
        if (bit != null) {
            RadarConfig rc = bit.config.duplicate();
            rc.setEnabled(!rc.isEnabled());
            SetRadarConfig req = new SetRadarConfig();
            req.radarID = rc.getRadarID();
            req.config = rc;
            RadarApps.getRcmSystem().sendCheckedAndHandled(req, getShell());
        }
    }

    protected void fillList() {
        RcmSystem rcmSystem = RadarApps.getRcmSystem();
        Map<String, ROStatus> status = new HashMap<String, ROStatus>();
        ReplyObj.ConfigReply cfgReply;
        ReplyObj ro;

        ro = rcmSystem.sendCheckedAndHandled(new ReqObj.GetRadarConfig(),
                getShell());
        if (ro == null)
            return;
        cfgReply = (ReplyObj.ConfigReply) ro;

        ro = rcmSystem.sendCheckedAndHandled(
                new ReqObj.GetRadarStatusMessages(), getShell());
        if (ro != null) {
            for (ROStatus s : ((ReplyObj.StatusMessagesReply) ro).status) {
                status.put(s.radarID, s);
            }
        }

        ArrayList<Bit> ded = new ArrayList<Bit>();
        ArrayList<Bit> dial = new ArrayList<Bit>();
        for (RadarConfig rc : cfgReply.config) {
            Bit bit = new Bit();
            bit.config = rc;
            bit.status = status.get(rc.getRadarID());
            bit.vcp = -1;
            if (bit.status != null) {
                byte[] gsmData = null;
                if (bit.status.currentGSM != null)
                    gsmData = bit.status.currentGSM;
                else if (bit.status.lastGSM != null)
                    gsmData = bit.status.lastGSM; // and gray out..
                if (gsmData != null) {
                    try {
                        GSM gsm = GSM.decode(gsmData);
                        bit.vcp = gsm.vcp;
                    } catch (MessageFormatException e) {
                        // nothing;
                    }
                }
            }
            if (rc.isDedicated())
                ded.add(bit);
            else
                dial.add(bit);
        }
        statusList.clear();
        if (ded.size() > 0) {
            statusList.add(Separator.DEDICATED);
            statusList.addAll(ded);
        }
        if (dial.size() > 0) {
            statusList.add(Separator.DIAL);
            statusList.addAll(dial);
        }
        if (statusList.size() == 0)
            statusList.add(Separator.NONE_DEFINED);
        // getShell().pack();
    }

    protected void onGlobalConfig() {
        GlobalConfigReply ro = (GlobalConfigReply) RadarApps
                .getRcmSystem()
                .sendCheckedAndHandled(new ReqObj.GetGlobalConfig(), getShell());
        if (ro == null)
            return;
        GlobalConfigDialog gcd = new GlobalConfigDialog(this.getShell(),
                ro.global);
        if (gcd.open() == Window.OK) {
            SetGlobalConfig r = new SetGlobalConfig();
            r.global = ro.global;
            RadarApps.getRcmSystem().sendCheckedAndHandled(r, getShell());
        }
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

        super.buttonPressed(buttonId);
    }

    @Override
    protected void configureShell(Shell newShell) {
        super.configureShell(newShell);
        newShell.setText("RadarServer Configuration");
    }

    private class StatusLabelProvider extends LabelProvider implements
            ITableLabelProvider, ITableColorProvider {

        @Override
        public Image getColumnImage(Object element, int columnIndex) {
            if (element instanceof Bit) {
                Bit bit = (Bit) element;
                if (columnIndex == 0) {
                    if (bit.connEvent == RadarEvent.CONNECTION_UP
                            || (bit.connEvent == -1 && bit.status != null && bit.status.currentGSM != null))
                        return dots[DOT_GREEN];
                    else if (bit.connEvent == RadarEvent.CONNECTION_ATTEMPT_STARTED)
                        return dots[DOT_YELLOW];
                    else if (bit.config.isDedicated() && bit.config.isEnabled())
                        return dots[DOT_RED];
                    else
                        return dots[DOT_EMPTY];
                }
            }
            return null;
        }

        @Override
        public String getColumnText(Object element, int columnIndex) {
            if (element instanceof Bit) {
                Bit bit = (Bit) element;
                if (columnIndex == 0) {
                    String s = bit.config.getRadarID().toUpperCase();
                    if (!bit.config.isEnabled())
                        s = s + " (disabled)";
                    return s;
                }
            } else if (element instanceof Separator)
                return ((Separator) element).label;
            return "";
        }

        @Override
        public Color getBackground(Object element, int columnIndex) {
            return statusTable.getControl().getBackground();
        }

        @Override
        public Color getForeground(Object element, int columnIndex) {
            if (element instanceof Separator)
                return statusTable.getControl().getDisplay().getSystemColor(
                        SWT.COLOR_DARK_GRAY);
            else
                return statusTable.getControl().getForeground();
        }

    }

    @Override
    public void handleConfigEvent(final ConfigEvent event) {
        Shell s = getShell();
        if (s != null && !s.isDisposed()) {
            s.getDisplay().asyncExec(new Runnable() {
                public void run() {
                    handleConfigEvent2(event);
                }
            });
        }
    }

    @Override
    public void handleRadarEvent(final RadarEvent event) {
        Shell s = getShell();
        if (s != null && !s.isDisposed()) {
            s.getDisplay().asyncExec(new Runnable() {
                public void run() {
                    handleRadarEvent2(event);
                }
            });
        }
    }

    protected void handleConfigEvent2(ConfigEvent event) {
        ConfigEvent e = (ConfigEvent) event;

        if (e.getOldConfig() != null && e.getNewConfig() != null) {
            String radarID = e.getRadarID();
            int i = 0;
            Separator sep = null;
            for (Object obj : statusList) {
                if (obj instanceof Separator)
                    sep = (Separator) obj;
                else if (obj instanceof Bit) {
                    Bit bit = (Bit) obj;
                    if (bit.config.getRadarID().equals(radarID)) {
                        bit.config = e.getNewConfig();
                        if ((bit.config.isDedicated() && sep != Separator.DEDICATED)
                                || (!bit.config.isDedicated() && sep != Separator.DIAL)) {
                            break;
                        }
                        statusList.set(i, bit);
                        onSelectionChanged();// TODO: needed?
                        return;
                    }
                }
                ++i;
            }
        }

        fillList();
    }

    protected void handleRadarEvent2(RadarEvent event) {
        RadarEvent e = (RadarEvent) event;
        // System.out.format("RadarEvent %d %s\n", e.getType(), e.getRadarID());
        String radarID = e.getRadarID();
        int i = 0;
        for (Object obj : statusList) {
            if (obj instanceof Bit) {
                Bit bit = (Bit) obj;
                if (bit.config.getRadarID().equals(radarID)) {
                    switch (e.getType()) {
                    case RadarEvent.CONNECTION_DOWN:
                    case RadarEvent.CONNECTION_ATTEMPT_FAILED:
                    case RadarEvent.CONNECTION_ATTEMPT_STARTED:
                    case RadarEvent.CONNECTION_UP:
                    	if (bit.status != null)
                    		bit.status.currentGSM = null;
                        bit.connEvent = e.getType();
                        break;
                    case RadarEvent.MESSAGE_RECEIVED:
                        try {
                            if (Message.messageCodeOf(e.getMessageData()) == Message.GSM)
                            	if (bit.status != null)
                            		bit.status.currentGSM = e.getMessageData();
                        } catch (RuntimeException exc) {
                            // nothing;
                        }
                        break;
                    }
                    // System.out.println("setting...");
                    statusList.set(i, bit);
                    break;
                }
            }
            ++i;
        }
    }

    @Override
    public void onRcmClientReady() {
        getShell().getDisplay().asyncExec(new Runnable() {
            public void run() {
                fillList();
            }
        });
    }

    @Override
    public void onRcmException(Exception exception) {
        // TODO: or as long as RcmSystem has event listeners it should try to
        // reconnect...
    }

	@Override
	public void handleNotificationEvent(NotificationEvent event) {
		// nothing
	}
}
