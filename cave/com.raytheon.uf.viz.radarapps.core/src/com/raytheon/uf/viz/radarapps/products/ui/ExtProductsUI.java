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
package com.raytheon.uf.viz.radarapps.products.ui;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.eclipse.core.databinding.observable.list.WritableList;
import org.eclipse.jface.databinding.viewers.ObservableListContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.RowData;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;

import com.raytheon.rcm.config.RadarConfig;
import com.raytheon.rcm.event.ConfigEvent;
import com.raytheon.rcm.event.NotificationEvent;
import com.raytheon.rcm.event.RadarEvent;
import com.raytheon.rcm.event.RadarEventListener;
import com.raytheon.rcm.mqsrvr.ReplyObj.ConfigReply;
import com.raytheon.rcm.mqsrvr.ReqObj;
import com.raytheon.uf.common.dataplugin.radar.util.RadarsInUseUtil;
import com.raytheon.uf.viz.core.localization.LocalizationConstants;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.uf.viz.radarapps.client.RcmClientExceptionListener;
import com.raytheon.uf.viz.radarapps.client.RcmSystem;
import com.raytheon.uf.viz.radarapps.core.RadarApps;

/**
 * Creates an RPG selector widget.
 * <p>
 * Will probably be merged into RadarProductUI. Used for the OTR GUI. TODO: On
 * config updates, what happens in terms of selection events? TODO: Does not
 * handle status *changes*... Client would need to decide how to handle.
 */
public class ExtProductsUI extends Composite implements RadarEventListener,
        RcmClientExceptionListener {
    private ComboViewer rpgViewer;

    private WritableList rpgList = new WritableList();

    public ExtProductsUI(Composite parent) {
        super(parent, SWT.NONE);
        createRpgSelectorUI();
    }

    public void onDispose() {
        RadarApps.getRcmSystem().getClient().removeEventListener(this);
        RadarApps.getRcmSystem().getClient().removeRcmClientListener(this);
    }

    public String getSelectedRPG() {
        try {
            RadarConfig rc = (RadarConfig) ((IStructuredSelection) rpgViewer
                    .getSelection()).getFirstElement();
            return rc.getRadarID();
        } catch (Exception e) {
            return null;
        }
    }

    public ISelectionProvider getSelectionProvider() {
        return rpgViewer;
    }

    public void addFilter(ViewerFilter filter) {
        rpgViewer.addFilter(filter);
    }

    public void selectDefaultRpg() {
        // Go through the viewer instead of the list to honor filtering.
        int i = 0;
        do {
            Object o = rpgViewer.getElementAt(i++);
            if (o instanceof RadarConfig) {
                rpgViewer.setSelection(new StructuredSelection(o));
                return;
            } else if (o == null)
                break;
        } while (true);
    }

    private void createRpgSelectorUI() {
        Composite p = this;
        RowLayout rl = new RowLayout(SWT.HORIZONTAL);
        rl.center = true;
        p.setLayout(rl);

        Label l = new Label(p, SWT.LEFT);
        l.setText("RPG: ");
        l.setAlignment(SWT.CENTER);

        Combo cb = new Combo(p, SWT.READ_ONLY);
        cb.add("+KXXX");
        cb.select(0);
        Point sz = cb.computeSize(SWT.DEFAULT, SWT.DEFAULT);
        sz.y = SWT.DEFAULT;
        cb.removeAll();
        cb.setLayoutData(new RowData(sz));

        rpgViewer = new ComboViewer(cb);
        rpgViewer.setContentProvider(new ObservableListContentProvider());
        rpgViewer.setLabelProvider(new LabelProvider() {
            @Override
            public String getText(Object element) {
                if (element instanceof RadarConfig) {
                    RadarConfig rc = (RadarConfig) element;
                    return rc.getRadarID().toUpperCase();
                } else
                    return super.getText(element);
            }
        });
        rpgViewer.setInput(rpgList);

        addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                onDispose();
            }
        });

        fillList();

        RadarApps.getRcmSystem().getClient().addEventListener(this);
        RadarApps.getRcmSystem().getClient().addRcmClientListener(this);
    }

    public void realizeStatus() {
    }

    @Override
    public void handleConfigEvent(final ConfigEvent event) {
        if (event.getRadarID() != null && !isDisposed())
            getDisplay().asyncExec(new Runnable() {
                @Override
                public void run() {
                    fillList();
                }
            });
    }

    private void fillList() {
        if (isDisposed())
            return;

        String currentSelection = getSelectedRPG();
        RadarConfig newSelection = null;
        RcmSystem rcmSystem = RadarApps.getRcmSystem();
        ConfigReply cfgReply;

        cfgReply = (ConfigReply) rcmSystem.sendCheckedAndHandled(
                new ReqObj.GetRadarConfig(), getShell());
        if (cfgReply == null)
            return;

        ArrayList<Object> ded = new ArrayList<Object>();
        ArrayList<Object> dial = new ArrayList<Object>();
        String site = LocalizationManager.getInstance().getLocalizationStore()
                .getString(LocalizationConstants.P_LOCALIZATION_SITE_NAME);
        List<String> sites = RadarsInUseUtil.getSite(site,
                RadarsInUseUtil.LOCAL_CONSTANT);
        sites.addAll(RadarsInUseUtil.getSite(site,
                RadarsInUseUtil.DIAL_CONSTANT));
        for (RadarConfig rc : cfgReply.config) {
            if (rc.getRadarID().equals(currentSelection))
                newSelection = rc;
            if (rc.isDedicated() && sites.contains(rc.getRadarID()))
                ded.add(rc);
            else if (sites.contains(rc.getRadarID()))
                dial.add(rc);
        }

        // sort the sites
        List<RadarConfig> temp = new ArrayList<RadarConfig>();
        for (int i = 0; i < ded.size(); i++) {
            temp.add((RadarConfig) ded.get(i));
        }
        Collections.sort(temp);
        ded.clear();
        for (int i = 0; i < temp.size(); i++) {
            ded.add(temp.get(i));
        }
        if (ded.size() > 0 && dial.size() > 0)
            ded.add("--------");

        temp.clear();
        for (int i = 0; i < dial.size(); i++) {
            temp.add((RadarConfig) dial.get(i));
        }
        Collections.sort(temp);
        dial.clear();
        for (int i = 0; i < temp.size(); i++) {
            dial.add(temp.get(i));
        }

        ded.addAll(dial);

        rpgList.clear();
        rpgList.addAll(ded);

        if (newSelection != null)
            rpgViewer.setSelection(new StructuredSelection(newSelection));
    }

    @Override
    public void handleRadarEvent(RadarEvent event) {
        // nothing
    }

    @Override
    public void handleNotificationEvent(NotificationEvent event) {
        // nothing
    }

    @Override
    public void onRcmClientReady() {
        if (!isDisposed())
            getDisplay().asyncExec(new Runnable() {
                @Override
                public void run() {
                    fillList();
                }
            });
    }

    @Override
    public void onRcmException(Exception exception) {
        // nothing
    }

}