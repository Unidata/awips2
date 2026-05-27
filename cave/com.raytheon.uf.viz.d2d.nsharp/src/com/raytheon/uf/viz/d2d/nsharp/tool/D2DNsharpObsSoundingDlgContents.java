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
package com.raytheon.uf.viz.d2d.nsharp.tool;

import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingProfile;
import gov.noaa.nws.ncep.ui.nsharp.NsharpConstants;
import gov.noaa.nws.ncep.ui.nsharp.display.map.AbstractNsharpMapResource;
import gov.noaa.nws.ncep.ui.nsharp.view.AbstractObsSoundingDlgContents;

import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Listener;

/**
 *
 * The class for D2D NSHARP Observed Sounding Dialog Contents
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Mar 20, 2020  73172    smanoj   Initial creation
 * Jun 25, 2020  80230    smanoj   Fixing some errors and enhancements.
 * 
 * </pre>
 *
 * @author smanoj
 */
public class D2DNsharpObsSoundingDlgContents
        extends AbstractObsSoundingDlgContents {

    private Button loadBtn;

    public D2DNsharpObsSoundingDlgContents(Composite parent) {
        super(parent);
        ldDia = D2DNsharpLoadDialog.getAccess();
        newFont = ldDia.getNewFont();
    }

    public void createObsDialogContents() {
        topGp = new Group(parent, SWT.SHADOW_ETCHED_IN);
        topGp.setLayout(new GridLayout(2, false));

        ldDia.createSndTypeList(topGp);

        btnGp = new Group(topGp, SWT.SHADOW_ETCHED_IN);
        btnGp.setText("File Type");
        btnGp.setFont(newFont);
        uairBtn = new Button(btnGp, SWT.RADIO | SWT.BORDER);
        uairBtn.setText(FILE_UAIR);
        uairBtn.setEnabled(true);
        uairBtn.setBounds(btnGp.getBounds().x + NsharpConstants.btnGapX,
                btnGp.getBounds().y + NsharpConstants.labelGap,
                NsharpConstants.btnWidth, NsharpConstants.btnHeight);
        uairBtn.setFont(newFont);
        uairBtn.addListener(SWT.MouseUp, new Listener() {
            public void handleEvent(Event event) {
                currentSndType = NcSoundingProfile.ObsSndType.NCUAIR;
                createTimeList(currentSndType);
            }
        });

        bufruaBtn = new Button(btnGp, SWT.RADIO | SWT.BORDER);
        bufruaBtn.setText(FILE_BUFRUA);
        bufruaBtn.setEnabled(true);
        bufruaBtn.setBounds(btnGp.getBounds().x + NsharpConstants.btnGapX,
                uairBtn.getBounds().y + uairBtn.getBounds().height
                        + NsharpConstants.btnGapY,
                NsharpConstants.btnWidth, NsharpConstants.btnHeight);
        bufruaBtn.setFont(newFont);
        bufruaBtn.addListener(SWT.MouseUp, new Listener() {
            public void handleEvent(Event event) {
                currentSndType = NcSoundingProfile.ObsSndType.BUFRUA;
                createTimeList(currentSndType);
            }
        });

        loadBtn = new Button(parent, SWT.PUSH);
        loadBtn.setText("Load ");
        loadBtn.setFont(newFont);
        loadBtn.setEnabled(true);
        loadBtn.addListener(SWT.MouseUp, new Listener() {
            @Override
            public void handleEvent(Event event) {
                AbstractNsharpMapResource nsharpMapResource = ldDia.getMapRsc()
                        .getOrCreateNsharpMapResource();
                nsharpMapResource.setPoints(null);
                handleSndTimeSelection(currentSndType);
                nsharpMapResource.setPoints(stnPoints);
                ldDia.getMapRsc().bringMapEditorToTop();
                ldDia.close();
            }
        });
    }

    public void cleanup() {
        if (bufruaBtn != null) {
            bufruaBtn.removeListener(SWT.MouseUp,
                    bufruaBtn.getListeners(SWT.MouseUp)[0]);
            bufruaBtn.dispose();
            bufruaBtn = null;
        }
        if (uairBtn != null) {
            uairBtn.removeListener(SWT.MouseUp,
                    uairBtn.getListeners(SWT.MouseUp)[0]);
            uairBtn.dispose();
            uairBtn = null;
        }
        if (loadBtn != null) {
            loadBtn.removeListener(SWT.MouseUp,
                    loadBtn.getListeners(SWT.MouseUp)[0]);
            loadBtn.dispose();
            loadBtn = null;
        }
        if (btnGp != null) {
            btnGp.dispose();
            btnGp = null;
        }

        D2DNsharpLoadDialog ldDia = D2DNsharpLoadDialog
                .getInstance(parent.getShell());
        ldDia.cleanSndTypeList();
        ldDia.getMapRsc().setPoints(null);

        if (topGp != null) {
            topGp.dispose();
            topGp = null;
        }
        if (!this.selectedTimeList.isEmpty()) {
            this.selectedTimeList.clear();
        }
    }

    @Override
    protected void setSndTimeList(List<String> timeList) {
        this.selectedTimeList = timeList;
    }
}