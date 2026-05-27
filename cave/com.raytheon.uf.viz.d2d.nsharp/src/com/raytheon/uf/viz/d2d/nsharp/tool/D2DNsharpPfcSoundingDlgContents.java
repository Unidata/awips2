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

import gov.noaa.nws.ncep.viz.soundingrequest.NcSoundingQuery;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingProfile;
import gov.noaa.nws.ncep.edex.common.sounding.NcSoundingTimeLines;
import gov.noaa.nws.ncep.ui.nsharp.NsharpConfigManager;
import gov.noaa.nws.ncep.ui.nsharp.NsharpConfigStore;
import gov.noaa.nws.ncep.ui.nsharp.NsharpConstants;
import gov.noaa.nws.ncep.ui.nsharp.NsharpGraphProperty;
import gov.noaa.nws.ncep.ui.nsharp.display.map.AbstractNsharpMapResource;
import gov.noaa.nws.ncep.ui.nsharp.view.AbstractPfcSoundingDlgContents;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Listener;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 *
 * The class for D2D NSHARP Point Forecast Sounding Dialog Contents
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Mar 20, 2020  73172    smanoj   Initial creation
 * Jun 25, 2020  80230    smanoj   Fixing some errors and enhancements.
 * Jan 27, 2021  87346    smanoj   Added ARW and RAP to PFC sounding Types.
 *
 * </pre>
 *
 * @author smanoj
 */
public class D2DNsharpPfcSoundingDlgContents
        extends AbstractPfcSoundingDlgContents {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(AbstractPfcSoundingDlgContents.class);

    private Button loadBtn;

    private List<String> availablefileList = new ArrayList<String>();

    private org.eclipse.swt.widgets.List pfcTypeNameList = null;

    public D2DNsharpPfcSoundingDlgContents(Composite parent) {
        super(parent);
        ldDia = D2DNsharpLoadDialog.getAccess();
        newFont = ldDia.getNewFont();
    }

    private void createPFCTypeList() {

        List<String> pfccfgList = null;

        NsharpConfigManager configMgr = NsharpConfigManager.getInstance();
        NsharpConfigStore configStore = configMgr
                .retrieveNsharpConfigStoreFromFs();
        NsharpGraphProperty graphConfigProperty = configStore
                .getGraphProperty();
        pfccfgList = graphConfigProperty.getPfcTypeList();
        if ((pfccfgList != null) && (pfccfgList.size() > 0)) {
            for (int i = 0; i < pfccfgList.size(); i++) {
                pfcTypeNameList.add(pfccfgList.get(i));
            }
        }

    }

    public void createPfcDialogContents() {
        topGp = new Group(parent, SWT.SHADOW_ETCHED_IN);
        topGp.setLayout(new GridLayout(2, false));

        currentSndType = null;
        ldDia.createSndTypeList(topGp);

        fileTypeGp = new Group(topGp, SWT.SHADOW_ETCHED_IN);
        fileTypeGp.setText("File Type");
        fileTypeGp.setFont(newFont);

        pfcTypeNameList = new org.eclipse.swt.widgets.List(fileTypeGp,
                SWT.BORDER | SWT.V_SCROLL);

        pfcTypeNameList.setBounds(fileTypeGp.getBounds().x,
                fileTypeGp.getBounds().y + NsharpConstants.labelGap,
                NsharpConstants.filelistWidth, NsharpConstants.listHeight);

        pfcTypeNameList.setFont(newFont);
        createPFCTypeList();

        // create a selection listener to handle user's selection on list
        pfcTypeNameList.addListener(SWT.Selection, new Listener() {
            @Override
            public void handleEvent(Event e) {
                if (pfcTypeNameList.getSelectionCount() > 0) {
                    currentSndType = null;
                    String selectedType = pfcTypeNameList.getSelection()[0];

                    switch (selectedType) {
                    case "NAM":
                        currentSndType = NcSoundingProfile.PfcSndType.NAMSND;
                        break;
                    case "GFS":
                        currentSndType = NcSoundingProfile.PfcSndType.GFSSND;
                        break;
                    case "ARW":
                        currentSndType = NcSoundingProfile.PfcSndType.ARWSND;
                        break;
                    case "RAP":
                        currentSndType = NcSoundingProfile.PfcSndType.RAPSND;
                        break;
                    default:
                        currentSndType = null;
                        return;
                    }

                    createPFCAvailableFileList();
                    ldDia.setActivePfcSndType(currentSndType);
                }
            }
        });

        loadBtn = new Button(parent, SWT.PUSH);
        loadBtn.setText("Load ");
        loadBtn.setFont(newFont);
        loadBtn.setEnabled(true);

        loadBtn.addListener(SWT.MouseUp, new Listener() {
            @Override
            public void handleEvent(Event event) {
                // Check File Type is selected
                if ((currentSndType == null)) {
                    statusHandler.handle(Priority.WARN,
                            "File Type not Available/Selected to load.");
                    return;
                }
                ldDia.startWaitCursor();
                AbstractNsharpMapResource nsharpMapResource = ldDia.getMapRsc()
                        .getOrCreateNsharpMapResource();
                nsharpMapResource.setPoints(null);
                createTimeList(currentSndType, availablefileList);
                handleSndTimeSelection(currentSndType);
                nsharpMapResource.setPoints(stnPoints);
                ldDia.getMapRsc().bringMapEditorToTop();
                ldDia.stopWaitCursor();
                ldDia.close();
            }
        });

    }

    public void cleanup() {

        ldDia = D2DNsharpLoadDialog.getAccess();
        ldDia.cleanSndTypeList();
        ldDia.getMapRsc().setPoints(null);

        if (topGp != null) {
            topGp.dispose();
            topGp = null;
        }
        if (loadBtn != null) {
            loadBtn.removeListener(SWT.MouseUp,
                    loadBtn.getListeners(SWT.MouseUp)[0]);
            loadBtn.dispose();
            loadBtn = null;
        }
        if (fileTypeGp != null) {
            fileTypeGp.dispose();
            fileTypeGp = null;
        }
        if (!this.selectedTimeList.isEmpty()) {
            this.selectedTimeList.clear();
        }
    }

    private void createPFCAvailableFileList() {
        // query using NcSoundingQuery class to query
        NcSoundingTimeLines timeLines = NcSoundingQuery
                .soundingTimeLineQuery(currentSndType.toString());
        if (timeLines != null && timeLines.getTimeLines() != null) {
            ldDia.startWaitCursor();
            for (Object timeLine : timeLines.getTimeLines()) {
                Date reftime = (Date) timeLine;
                if (reftime != null) {
                    Calendar cal = Calendar
                            .getInstance(TimeZone.getTimeZone("GMT"));
                    cal.setTimeInMillis(reftime.getTime());
                    String gmtTimeStr = String.format("%1$tY-%1$tm-%1$td %1$tH",
                            cal);
                    if (!availablefileList.contains(gmtTimeStr)) {
                        availablefileList.add(gmtTimeStr);
                    }
                }
            }
            ldDia.stopWaitCursor();
        } else {
            statusHandler.handle(Priority.INFO,
                    "SQL: query return null for " + currentSndType);
        }
    }

    @Override
    protected void setSndTimeList(List<String> timeList) {
        this.selectedTimeList = timeList;
    }
}