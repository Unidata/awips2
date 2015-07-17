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

import java.util.HashSet;

import org.eclipse.core.databinding.observable.list.WritableList;
import org.eclipse.jface.databinding.viewers.ObservableListContentProvider;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.StructuredViewer;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.rcm.config.RadarConfig;
import com.raytheon.rcm.config.RadarType;
import com.raytheon.rcm.config.RcmUtil;
import com.raytheon.rcm.products.Usage;
import com.raytheon.rcm.rmr.MultipleRequest.Element;
import com.raytheon.uf.viz.radarapps.products.ui.RadarProductUI;

/**
 * Element dialog.
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
public class ElementDialog extends Dialog {

    private RmrWindow rmr;

    private Element input;

    private Combo radarTypeCombo;

    private StructuredViewer dialViewer;

    private TableViewer dedViewer;

    private RadarProductUI pui = new RadarProductUI() {
        // TODO: this should be a listener interface
        @Override
        protected void onRequestChanged() {
            super.onRequestChanged();
            validate();
        }
    };

    public ElementDialog(RmrWindow rmr, Element element) {
        super(rmr);
        if (rmr == null || element == null)
            throw new IllegalArgumentException("Arguments must not be null.");
        setShellStyle((getShellStyle() & ~SWT.APPLICATION_MODAL)
                | SWT.PRIMARY_MODAL);
        this.rmr = rmr;
        this.input = element;
        pui.setUsage(Usage.RMR);
    }

    @Override
    protected void configureShell(Shell newShell) {
        super.configureShell(newShell);
        newShell.setText("Edit Product");
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        Composite c = (Composite) super.createDialogArea(parent);
        ((GridLayout) c.getLayout()).numColumns = 2;

        GridLayout gl/* = new GridLayout(2, false) */;
        GridData gd;
        Label l;

        Composite r = new Composite(c, SWT.NONE);
        gd = new GridData();
        gd.horizontalSpan = 2;
        r.setLayoutData(gd);

        RowLayout rl = new RowLayout(SWT.HORIZONTAL);
        rl.center = true;
        r.setLayout(rl);

        l = new Label(r, SWT.LEFT);
        l.setText("Radar Type: ");

        radarTypeCombo = new Combo(r, SWT.READ_ONLY);
        radarTypeCombo.add("WSR-88D");
        radarTypeCombo.add("TDWR");
        radarTypeCombo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                onSetRadarType();
            }
        });

        l = new Label(c, SWT.LEFT);
        l.setText("Dial RPGs");
        l = new Label(c, SWT.LEFT);
        l.setText("Dedicated RPGs");

        dialViewer = new TableViewer(c, SWT.MULTI | SWT.BORDER);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 100;
        gd.heightHint = 60;
        dialViewer.getControl().setLayoutData(gd);

        dedViewer = new TableViewer(c, SWT.MULTI | SWT.BORDER);
        gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.widthHint = 100;
        gd.heightHint = 60;
        dedViewer.getControl().setLayoutData(gd);

        Control c2 = pui.createProductRequestUI(c);
        gd = new GridData(SWT.FILL, SWT.FILL, true, false);
        gd.horizontalSpan = 2;
        c2.setLayoutData(gd);

        refresh();

        return c;
    }

    protected void onSetRadarType() {
        RadarType radarType = radarTypeCombo.getSelectionIndex() == 0 ? RadarType.WSR
                : RadarType.TDWR;

        pui.setRadarType(radarType);

        WritableList dialList = new WritableList();
        WritableList dedList = new WritableList();
        for (RadarConfig rc : rmr.getRadars()) {
            if (RcmUtil.getRadarType(rc) == radarType) {
                if (rc.isDedicated())
                    dedList.add(rc.getRadarID());
                else
                    dialList.add(rc.getRadarID());
            }
        }
        dialViewer.setContentProvider(new ObservableListContentProvider());
        dialViewer.setLabelProvider(new LabelProvider());
        dialViewer.setInput(dialList);
        dedViewer.setContentProvider(new ObservableListContentProvider());
        dedViewer.setLabelProvider(new LabelProvider());
        dedViewer.setInput(dedList);
        if (dedViewer.getTable().getItemCount() > 0) {
            dedViewer.getTable().setSelection(0);
        }
    }

    public void refresh() {
        RadarType rt = RadarType.WSR;

        if (input.getRadarIDs().length > 0) {
            String anID = input.getRadarIDs()[0];
            for (RadarConfig rc : rmr.getRadars()) {
                if (rc.getRadarID().equalsIgnoreCase(anID)) {
                    rt = RcmUtil.getRadarType(rc);
                    break;
                }
            }
        }

        if (rt == RadarType.WSR)
            radarTypeCombo.select(0);
        else
            radarTypeCombo.select(1);

        onSetRadarType();

        pui.setRequest(input.getRequest());
    }

    @Override
    protected void buttonPressed(int buttonId) {
        if (buttonId == IDialogConstants.OK_ID) {
            if (checkValid(true))
                super.buttonPressed(buttonId);
        } else
            super.buttonPressed(buttonId);
    }

    private void validate() {
        Button b = getButton(IDialogConstants.OK_ID);
        if (b != null)
            b.setEnabled(checkValid(false));
    }

    private boolean checkValid(boolean commit) {
        if (!pui.isRequestValid())
            return false;

        if (commit) {
            input.setRequest(pui.getRequest());
            HashSet<String> radarIDs = new HashSet<String>();
            radarIDs.addAll(((IStructuredSelection) dialViewer.getSelection())
                    .toList());
            radarIDs.addAll(((IStructuredSelection) dedViewer.getSelection())
                    .toList());
            input.setRadarIDs(radarIDs.toArray(new String[radarIDs.size()]));
        }

        return true;
    }
}
