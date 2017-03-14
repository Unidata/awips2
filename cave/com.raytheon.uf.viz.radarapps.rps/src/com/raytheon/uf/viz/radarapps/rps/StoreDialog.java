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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.rcm.config.RadarConfig;
import com.raytheon.rcm.config.RadarType;
import com.raytheon.rcm.config.RcmUtil;
import com.raytheon.rcm.mqsrvr.ReplyObj.ConfigReply;
import com.raytheon.rcm.mqsrvr.ReqObj;
import com.raytheon.rcm.products.ElevationInfo;
import com.raytheon.rcm.products.ElevationInfo.VCPInfo;
import com.raytheon.uf.viz.radarapps.core.RadarApps;

/**
 * RPS Store Dialog
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2015-06-10   4498       nabowle     Rename Util->RcmUtil
 * </pre>
 *
 */
public class StoreDialog extends Dialog {

    RpsListRequestContainer listEditor;

    ArrayList<String> selectedRadarIDs = new ArrayList<String>();

    int[] selectedVcps = new int[0];

    TableViewer rpgViewer;

    TableViewer vcpViewer;

    public StoreDialog(Shell parentShell, RpsListRequestContainer listEditor) {
        super(parentShell);
        this.listEditor = listEditor;
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        Composite c = (Composite) super.createDialogArea(parent);
        GridLayout gl = (GridLayout) c.getLayout();
        gl.numColumns = 2;
        GridData gd;
        Label l;

        l = new Label(c, SWT.LEFT | SWT.WRAP);
        l.setText("Select RPGs and VCPs for which the list will saved.  If a radar's current VCP is one of those specified, the list will become active immediately.");
        gd = new GridData();
        gd.widthHint = convertWidthInCharsToPixels(60);
        gd.horizontalSpan = 2;
        l.setLayoutData(gd);

        // l = new Label(c, SWT.LEFT); // dummy

        l = new Label(c, SWT.LEFT);
        l.setText("RPGs");

        l = new Label(c, SWT.LEFT);
        l.setText("VCPs");

        RadarType typeRestriction = listEditor.getTypeRestriction();

        // Duplicates code form RadarApps
        ArrayList<String> rpgs = new ArrayList<String>();
        {
            // TODO: Shell not visible yet, so using parent shell...
            ConfigReply reply = (ConfigReply) RadarApps.getRcmSystem()
                    .sendCheckedAndHandled(new ReqObj.GetRadarConfig(),
                            getParentShell());
            if (reply == null)
                return null;

            for (RadarConfig rc : reply.config) {
                if (!rc.isDedicated())
                    continue;
                if (typeRestriction != null
                        && RcmUtil.getRadarType(rc) != typeRestriction)
                    continue;
                rpgs.add(rc.getRadarID());
            }
        }

        TableViewer fTableViewer = new TableViewer(c, SWT.MULTI | SWT.H_SCROLL
                | SWT.V_SCROLL | SWT.BORDER);
        fTableViewer.setContentProvider(new ArrayContentProvider());
        fTableViewer.setLabelProvider(new LabelProvider() {
            public String getText(Object element) {
                return super.getText(element).toUpperCase();
            }
        });
        fTableViewer.setInput(rpgs.toArray());
        fTableViewer.addSelectionChangedListener(new ISelectionChangedListener() {
                    public void selectionChanged(SelectionChangedEvent event) {
                        validate();
                    }
        });
        if (listEditor.getRadarID() != null)
            fTableViewer.setSelection(new StructuredSelection(Arrays
                    .asList(listEditor.getRadarID())));
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = convertWidthInCharsToPixels(10);
        fTableViewer.getTable().setLayoutData(gd);
        rpgViewer = fTableViewer;

        ArrayList<VCPInfo> vcps = new ArrayList<VCPInfo>();
        {
            Collection<VCPInfo> allVcps = ElevationInfo.getInstance()
                    .getVcpInfo();
            for (VCPInfo vi : allVcps) {
                if (typeRestriction != null
                        && (typeRestriction == RadarType.TDWR) != listEditor
                                .isTdwrVcp(vi.vcp))
                    continue;
                vcps.add(vi);
            }
        }
        fTableViewer = new TableViewer(c, SWT.MULTI | SWT.H_SCROLL
                | SWT.V_SCROLL | SWT.BORDER);
        fTableViewer.setContentProvider(new ArrayContentProvider());
        fTableViewer.setLabelProvider(new LabelProvider());
        fTableViewer.addSelectionChangedListener(new ISelectionChangedListener() {
                    public void selectionChanged(SelectionChangedEvent event) {
                        validate();
                    }
        });
        fTableViewer.setInput(vcps);
        vcpViewer = fTableViewer;

        if (listEditor.getVcp() != -1) {
            for (VCPInfo vi : ElevationInfo.getInstance().getVcpInfo()) {
                if (vi.vcp == listEditor.getVcp()) {
                    fTableViewer.setSelection(new StructuredSelection(Arrays
                            .asList(vi)));
                    break;
                }
            }
        }
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = convertWidthInCharsToPixels(8);
        fTableViewer.getTable().setLayoutData(gd);

        return c;
    }

    public int[] getVcps() {
        return selectedVcps;
    }

    public List<String> getRadarIDs() {
        return selectedRadarIDs;
    }

    @Override
    protected void configureShell(Shell newShell) {
        super.configureShell(newShell);
        newShell.setText("Send and Store RPS List");
    }

    protected void validate() {
        boolean valid = rpgViewer != null && vcpViewer != null
                && ((IStructuredSelection) rpgViewer.getSelection()).size() > 0
                && ((IStructuredSelection) vcpViewer.getSelection()).size() > 0;
        Button b = getButton(IDialogConstants.OK_ID);
        if (b != null)
            b.setEnabled(valid);
    }

    @Override
    public void create() {
        super.create();
        validate();
    }

    @Override
    protected void okPressed() {
        IStructuredSelection sel;

        sel = (IStructuredSelection) rpgViewer.getSelection();
        selectedRadarIDs = new ArrayList<String>(sel.toList());

        sel = (IStructuredSelection) vcpViewer.getSelection();

        selectedVcps = new int[sel.size()];
        int i = 0;
        for (VCPInfo vi : (List<VCPInfo>) sel.toList())
            selectedVcps[i++] = vi.vcp;

        super.okPressed();
    }


}

