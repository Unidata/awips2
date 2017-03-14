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
package com.raytheon.uf.viz.radarapps.ccm;

import java.util.Arrays;
import org.apache.commons.lang.ArrayUtils;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.rcm.ccmreq.CcmRequestDefinition;
import com.raytheon.rcm.config.RadarConfig;
import com.raytheon.rcm.config.RadarType;
import com.raytheon.rcm.config.RcmUtil;
import com.raytheon.rcm.message.CPM;
import com.raytheon.rcm.message.GSM;
import com.raytheon.rcm.message.MessageFormatException;
import com.raytheon.rcm.mqsrvr.ReplyObj.ROStatus;
import com.raytheon.rcm.mqsrvr.ReplyObj.StatusMessagesReply;
import com.raytheon.rcm.mqsrvr.ReqObj.GetRadarStatusMessages;
import com.raytheon.rcm.mqsrvr.ReqObj.SendCcmRequest;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.radarapps.core.RadarApps;
import com.raytheon.uf.viz.radarapps.products.ui.ExtProductsUI;

import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * A window for command control message requests.
 *
 * <pre>
 *
 *  SOFTWARE HISTORY
 *  Date         Ticket#    Engineer    Description
 *  ------------ ---------- ----------- --------------------------
 *  2016-05-10   18795      jdynina     Initial creation
 * </pre>
 *
 */

public class CcmRequestWindow extends CaveSWTDialog {

    private static final transient IUFStatusHandler statusHandler = UFStatus.getHandler(CcmRequestWindow.class);

    private ExtProductsUI rpgSelector;

    private Combo vcpCombo, avsetCombo, sailsCombo;

    private Button sendButton, volumeScanRestart;

    private CPM cpm;

    private GSM gsm;

    private String[] textVcps;

    private CcmRequestDefinition opts = new CcmRequestDefinition();

    private final String[] vcpSupplementalStr = {
            "AVSET", "SAILS", "site-specific-vcp", "RxRN", "CBT"
    };

    public CcmRequestWindow(Shell parent) {
        super(parent);
        setText("VCP Change Request");
    }

    @Override
    protected void initializeComponents(Shell shell) {
        Composite c = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        c.setLayout(gl);

        Composite row;
        RowLayout rl;

        row = new Composite(c, SWT.NONE);
        rl = new RowLayout(SWT.HORIZONTAL);
        rl.center = true;
        row.setLayout(rl);
        fillRow(row);

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
                } else {
                    return false;
                }
            }
        });

        fillRow(new Label(c, SWT.SEPARATOR | SWT.HORIZONTAL));

        row = new Composite(c, SWT.NONE);
        Label l1 = new Label(row, SWT.LEFT);
        l1.setText("Select VCP:       ");
        vcpCombo = new Combo(row, SWT.READ_ONLY);
        setupRow(row, l1, vcpCombo);
        vcpCombo.add("xxxxx");
        vcpCombo.select(0);
        Point sz = vcpCombo.computeSize(SWT.DEFAULT, SWT.DEFAULT);
        sz.y = SWT.DEFAULT;
        vcpCombo.removeAll();

        vcpCombo.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                onVcpSelect();
            }
        });

        row = new Composite(c, SWT.NONE);
        Label l2 = new Label(row, SWT.LEFT);
        l2.setText("AVSET On/Off:       ");
        avsetCombo = new Combo(row, SWT.READ_ONLY);
        setupRow(row, l2, avsetCombo);
        avsetCombo.add("xxxxx");
        avsetCombo.select(0);
        sz = vcpCombo.computeSize(SWT.DEFAULT, SWT.DEFAULT);
        sz.y = SWT.DEFAULT;
        avsetCombo.removeAll();

        avsetCombo.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                onAvsetSelect();
            }
        });

        row = new Composite(c, SWT.NONE);
        Label l3 = new Label(row, SWT.LEFT);
        l3.setText("Number of SAILS Cuts:       ");
        sailsCombo = new Combo(row, SWT.READ_ONLY);
        setupRow(row, l3, sailsCombo);
        sailsCombo.add("xxxxx");
        sailsCombo.select(0);
        sz = vcpCombo.computeSize(SWT.DEFAULT, SWT.DEFAULT);
        sz.y = SWT.DEFAULT;
        sailsCombo.removeAll();

        sailsCombo.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                onSailsSelect();
            }
        });

        row = new Composite(c, SWT.NONE);
        fillRow(row);
        row.setLayout(new RowLayout(SWT.HORIZONTAL));

        row = new Composite(c, SWT.NONE);
        fillRow(row);
        row.setLayout(new RowLayout(SWT.HORIZONTAL));
        volumeScanRestart = createButton(row, "Force Volume Scan Restart", SWT.CHECK);

        volumeScanRestart.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                setVolumeScanRestart();
            }
        });

        fillRow(new Label(c, SWT.SEPARATOR | SWT.HORIZONTAL));

        row = new Composite(c, SWT.NONE);
        fillRow(row);
        row.setLayout(new RowLayout(SWT.HORIZONTAL));
        sendButton = createButton(row, "Send Request", SWT.PUSH);
        sendButton.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                onSendRequest();
            }
        });

        Button exitButton = createButton(row, "Exit", SWT.PUSH);
        exitButton.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent e) {
                close();
            }
        });

        getShell().getDisplay().asyncExec(new Runnable() {
            public void run() {
                rpgSelector.selectDefaultRpg();
            }
        });
    }

    private void setupRow(Control c) {
        GridData gd = new GridData(SWT.RIGHT, SWT.CENTER, true, false);
        c.setLayoutData(gd);
    }

    private void setupRow(Composite row, Control left, Control right) {
        GridData gd;

        setupRow(row);
        row.setLayout(new GridLayout(2, false));
        if (left != null) {
            gd = new GridData(SWT.LEFT, SWT.CENTER, false, false);
            left.setLayoutData(gd);
        }
        if (right != null) {
            gd = new GridData(SWT.RIGHT, SWT.CENTER, true, false);
            right.setLayoutData(gd);
        }
    }

    private Button createButton(Composite parent, String string, int flags) {
        Button button = new Button(parent, flags);
        button.setText(string);
        return button;
    }

    private void fillRow(Control c) {
        c.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, false));
    }

    private void onRpgSelected() {
        String message = "";

        String radarID = rpgSelector.getSelectedRPG();
        GetRadarStatusMessages req = new GetRadarStatusMessages();
        req.radarID = radarID;

        StatusMessagesReply reply = (StatusMessagesReply) RadarApps
                .getRcmSystem().sendCheckedAndHandled(req, getShell());
        if (reply != null) {
            ROStatus status = reply.status.iterator().next();

            byte[] msg = status.currentCPM != null ? status.currentCPM
                    : status.lastCPM;
            if (msg != null) {
                try {
                    cpm = CPM.decode(msg);
                } catch (MessageFormatException e) {
                    // nothing
                }
            } else {
                message = "VCP Change Request Parameter Message has not been received. "
                        + "You may not be running RPG Build 18, or your site may "
                        + "not be authorized to modify VCP/SAILS/AVSET.\n\n"
                        + "Please contact your adiminstrator for assistance.";
                String[] labels = { "OK" };
                MessageDialog dlg = new MessageDialog(getShell(), "Acknowledge",
                        (Image) null, message, MessageDialog.WARNING, labels, 2);
                switch (dlg.open()) {
                case 0:
                    vcpCombo.removeAll();
                    avsetCombo.removeAll();
                    sailsCombo.removeAll();
                    sendButton.setEnabled(false);
                    break;
                }

                return;
            }

            msg = status.currentGSM != null ? status.currentGSM
                    : status.lastGSM;
            if (msg != null) {
                try {
                    gsm = GSM.decode(msg);
                } catch (MessageFormatException e) {
                    // nothing
                }
            }
        }
        setSelections();
        sendButton.setEnabled(true);
    }

    private String formatBits(short bits, String[] strings) {
        StringBuilder result = new StringBuilder();
        for (int i = 0; i < 16; i++) {
            if ((bits & (1 << i)) != 0) {
                if (result.length() > 0) {
                    result.append(',');
                }
                String s = null;
                if (i < strings.length) {
                    s = strings[i];
                }
                if (s == null) {
                    s = "unk" + Integer.toString(15 - i);
                }
                result.append(s);
            }
        }
        return result.toString();
    }

    private void setSelections() {
        vcpCombo.removeAll();
        avsetCombo.removeAll();
        sailsCombo.removeAll();

        int[] vcps = (int[])ArrayUtils.addAll(cpm.clearAirVcps, cpm.precipVcps);
        Arrays.sort(vcps);
        textVcps = Arrays.toString(vcps).split("[\\[\\]]")[1].split(", ");

        opts.setRestartVcp(false); // volume scan restart flag

        for (int i = 0; i < textVcps.length; i++) {
            vcpCombo.add(textVcps[i]);
        }

        vcpCombo.select(0);
        if (Arrays.asList(textVcps).contains(Integer.toString(gsm.vcp))) {
            vcpCombo.select(Arrays.asList(textVcps).indexOf(Integer.toString(gsm.vcp)));
            opts.setVcp(gsm.vcp);
        } else {
            opts.setVcp(vcps[0]);
        }

        String vcpSuppl = formatBits((short) gsm.vcpSupplemental, vcpSupplementalStr);

        avsetCombo.add("On");
        avsetCombo.add("Off");
        avsetCombo.select(0);
        if (!vcpSuppl.contains("AVSET")) {
            avsetCombo.select(1);
        }

        opts.setAvsetEnabled(0);  // no avset flag change

        for (int i = 0; i < 4; i++) {
            sailsCombo.add(Integer.toString(i));
        }

        sailsCombo.select(0);
        if (vcpSuppl.contains("SAILS")) {
            String textSails[] = {"0", "1", "2", "3"};
            sailsCombo.select(Arrays.asList(textSails).indexOf(Integer.toString(cpm.maxSailsCuts)));
        }

        opts.setSailsCount(-1);  // no sails flag change
    }

    private void onSendRequest() {
        String radarID = rpgSelector.getSelectedRPG();

        if (radarID == null) {
            MessageBox mb = new MessageBox(getShell(), SWT.ICON_INFORMATION
                    | SWT.OK);
            mb.setText("Send Request");
            mb.setMessage("Please select a RPG.");
            mb.open();
            return;
        }

        SendCcmRequest r = new SendCcmRequest();
        r.radarID = radarID;
        r.ccmRequest = opts;
        RadarApps.getRcmSystem().sendCheckedAndHandled(r, getShell());

        int vcpIndex = vcpCombo.getSelectionIndex();
        String vcp = vcpCombo.getItem(vcpIndex);

        int avsetIndex = avsetCombo.getSelectionIndex();
        String avset = avsetCombo.getItem(avsetIndex);

        int sailsIndex = sailsCombo.getSelectionIndex();
        String sails = sailsCombo.getItem(sailsIndex);

        statusHandler.handle(Priority.EVENTA,
                String.format("VCP Config Change: %s: Vcp=%s; Avset=%s; Sails=%s; "
                        + "Volume Scan Restart=%s",
                        radarID, vcp, avset, sails,
                        volumeScanRestart.getSelection()));
    }

    private void onVcpSelect() {
        int vcpIndex = vcpCombo.getSelectionIndex();
        String vcp = vcpCombo.getItem(vcpIndex);

        opts.setVcp(Integer.parseInt(vcp));
    }

    private void onAvsetSelect() {
        int avsetIndex = avsetCombo.getSelectionIndex();
        String avset = avsetCombo.getItem(avsetIndex);

        if (avset.equals("Off")) {
            opts.setAvsetEnabled(4);
        } else {
            opts.setAvsetEnabled(2);
        }
    }

    private void onSailsSelect() {
        int sailsIndex = sailsCombo.getSelectionIndex();
        String sails = sailsCombo.getItem(sailsIndex);

        opts.setSailsCount(Integer.parseInt(sails));
    }

    private void setVolumeScanRestart() {
        if (volumeScanRestart.getSelection()) {
            opts.setRestartVcp(true);
        } else {
            opts.setRestartVcp(false);
        }
    }
}
