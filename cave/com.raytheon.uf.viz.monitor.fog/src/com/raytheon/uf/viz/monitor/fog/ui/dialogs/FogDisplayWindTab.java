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
package com.raytheon.uf.viz.monitor.fog.ui.dialogs;

import java.util.ArrayList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.TabFolder;

import com.raytheon.uf.common.monitor.data.MonitorConfigConstants.FogDisplay;
import com.raytheon.uf.common.monitor.data.ObConst.DataUsageKey;
import com.raytheon.uf.common.monitor.xml.AreaXML;
import com.raytheon.uf.common.monitor.xml.ThresholdsXML;
import com.raytheon.uf.viz.monitor.fog.threshold.FogDisplayWindData;
import com.raytheon.uf.viz.monitor.fog.threshold.FogThresholdMgr;
import com.raytheon.uf.viz.monitor.ui.dialogs.TabItemComp;

/**
 * Fog Display Wind Table.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 26, 2015 5115       skorolev    Corrected imports.
 * 
 * </pre>
 * 
 * @author skorolev
 * @version 1.0
 */
public class FogDisplayWindTab extends TabItemComp implements
        IUpdateDisplayWind {
    /**
     * Dialog used for editing the FOG display wind data.
     */
    private FogDisplayWindEditDlg fogWindEditDlg;

    private ArrayList<String> areaIDArray;

    private ArrayList<FogDisplayWindData> fogDataArray;

    public FogDisplayWindTab(TabFolder parent, DataUsageKey duKey) {
        super(parent, duKey);
    }

    @Override
    protected void createListHeader(Composite parentComp) {
        Composite lblComp = new Composite(parentComp, SWT.NONE);
        GridLayout gl = new GridLayout(5, false);
        gl.horizontalSpacing = 0;
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        lblComp.setLayout(gl);

        /*
         * Create filler label.
         */
        GridData gd = new GridData(73, SWT.DEFAULT);
        Label fillerLbl = new Label(lblComp, SWT.CENTER);
        fillerLbl.setLayoutData(gd);

        /*
         * Wind
         */
        Composite windComp = createGroupComposite(lblComp, 5, null);
        createLabelComp(windComp, "Wind", "Speed(kt)", false);
        createLabelComp(windComp, "Peak", "Wind(kt)", false);
        createLabelComp(windComp, "Gust", "Speed(kt)", false);
        createLabelComp(windComp, "Wind", "Dir(deg)\n(from)", false);
        createLabelComp(windComp, "Wind", "Dir(deg)\n(to)", false);
    }

    /*
     * TODO: redo...
     */
    @Override
    protected void populateList() {
        if (fogDataArray == null) {
            createDataArray();
        }

        boolean update = false;
        if (dataList.getItemCount() > 0) {
            update = true;
        }

        areaIDArray = new ArrayList<String>();

        String currentAreaID;

        StringBuilder sb = null;
        FogDisplayWindData fdwd = null;

        for (int i = 0; i < fogDataArray.size(); i++) {
            sb = new StringBuilder();

            fdwd = fogDataArray.get(i);

            currentAreaID = fdwd.getAreaID();
            areaIDArray.add(currentAreaID);

            sb.append(String.format(areaIdFmt, currentAreaID));

            /*
             * Wind Speed
             */
            appendIntData(sb, fdwd.getWindWindSpeedR(),
                    fdwd.getWindWindSpeedY());

            /*
             * Peak Wind
             */
            appendIntData(sb, fdwd.getWindPeakR(), fdwd.getWindPeakY());

            /*
             * Gust Speed
             */
            appendIntData(sb, fdwd.getWindGustR(), fdwd.getWindGustY());

            /*
             * Wind Direction: From
             */
            appendIntData(sb, fdwd.getWindDirFromY(), fdwd.getWindDirFromR());

            /*
             * Wind Direction: To
             */
            appendIntData(sb, fdwd.getWindDirToR(), fdwd.getWindDirToY());

            /*
             * Add the line of data to the list.
             */
            sb.append(" ");

            if (update == true) {
                dataList.setItem(i, sb.toString());
            } else {
                dataList.add(sb.toString());
            }
        }

        packListControls();
    }

    private void createDataArray() {
        fogDataArray = new ArrayList<FogDisplayWindData>();

        FogThresholdMgr ftm = FogThresholdMgr.getInstance();

        String xmlKey;
        String areaID;

        ThresholdsXML threshXML = ftm.getThresholdsXmlData(duKey);

        ArrayList<AreaXML> areasArray = threshXML.getAreas();

        for (AreaXML area : areasArray) {
            areaID = area.getAreaId();
            FogDisplayWindData fdwd = new FogDisplayWindData();

            fdwd.setAreaID(areaID);

            /*
             * Wind Speed
             */
            xmlKey = FogDisplay.FOG_DISP_WIND_WIND_SPEED.getXmlKey();
            fdwd.setWindWindSpeedR(ftm.getThresholdValue(duKey, threshKeyR,
                    areaID, xmlKey));
            fdwd.setWindWindSpeedY(ftm.getThresholdValue(duKey, threshKeyY,
                    areaID, xmlKey));

            /*
             * Peak Wind
             */
            xmlKey = FogDisplay.FOG_DISP_WIND_PEAK_WIND.getXmlKey();
            fdwd.setWindPeakR(ftm.getThresholdValue(duKey, threshKeyR, areaID,
                    xmlKey));
            fdwd.setWindPeakY(ftm.getThresholdValue(duKey, threshKeyY, areaID,
                    xmlKey));

            /*
             * Gust Speed
             */
            xmlKey = FogDisplay.FOG_DISP_WIND_GUST_SPEED.getXmlKey();
            fdwd.setWindGustR(ftm.getThresholdValue(duKey, threshKeyR, areaID,
                    xmlKey));
            fdwd.setWindGustY(ftm.getThresholdValue(duKey, threshKeyY, areaID,
                    xmlKey));

            /*
             * Wind Direction: From
             */
            xmlKey = FogDisplay.FOG_DISP_WIND_DIR_FROM.getXmlKey();
            fdwd.setWindDirFromR(ftm.getThresholdValue(duKey, threshKeyR,
                    areaID, xmlKey));
            fdwd.setWindDirFromY(ftm.getThresholdValue(duKey, threshKeyY,
                    areaID, xmlKey));

            /*
             * Wind Direction: To
             */
            xmlKey = FogDisplay.FOG_DISP_WIND_DIR_TO.getXmlKey();
            fdwd.setWindDirToR(ftm.getThresholdValue(duKey, threshKeyR, areaID,
                    xmlKey));
            fdwd.setWindDirToY(ftm.getThresholdValue(duKey, threshKeyY, areaID,
                    xmlKey));

            fogDataArray.add(fdwd);
        }
    }

    private FogDisplayWindData getDataAtFirstSelection() {
        int index = dataList.getSelectionIndex();

        return fogDataArray.get(index);
    }

    private void updateFogDataArray(FogDisplayWindData fdwd) {
        int[] dataListIndexes = dataList.getSelectionIndices();
        int currentIndex = 0;

        for (int i = 0; i < dataListIndexes.length; i++) {
            currentIndex = dataListIndexes[i];

            fogDataArray.get(currentIndex).updateData(fdwd);
        }
    }

    @Override
    public void commitDataToXML() {
        FogThresholdMgr ftm = FogThresholdMgr.getInstance();

        String xmlKey;
        String areaID;

        for (FogDisplayWindData fdwd : fogDataArray) {
            areaID = fdwd.getAreaID();

            /*
             * Wind Speed
             */
            xmlKey = FogDisplay.FOG_DISP_WIND_WIND_SPEED.getXmlKey();
            ftm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    fdwd.getWindWindSpeedR());
            ftm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    fdwd.getWindWindSpeedY());

            /*
             * Peak Wind
             */
            xmlKey = FogDisplay.FOG_DISP_WIND_PEAK_WIND.getXmlKey();
            ftm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    fdwd.getWindPeakR());
            ftm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    fdwd.getWindPeakY());

            /*
             * Gust Speed
             */
            xmlKey = FogDisplay.FOG_DISP_WIND_GUST_SPEED.getXmlKey();
            ftm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    fdwd.getWindGustR());
            ftm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    fdwd.getWindGustY());

            /*
             * Wind Direction: From
             */
            xmlKey = FogDisplay.FOG_DISP_WIND_DIR_FROM.getXmlKey();
            ftm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    fdwd.getWindDirFromR());
            ftm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    fdwd.getWindDirFromY());

            /*
             * Wind Direction: To
             */
            xmlKey = FogDisplay.FOG_DISP_WIND_DIR_TO.getXmlKey();
            ftm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    fdwd.getWindDirToR());
            ftm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    fdwd.getWindDirToY());
        }
    }

    @Override
    public void reloadData() {
        dataList.removeAll();
        fogDataArray.clear();
        fogDataArray = null;

        populateList();
    }

    @Override
    protected void editDataAction() {
        FogDisplayWindData fdwd = getDataAtFirstSelection();

        if (fogWindEditDlg == null) {
            fogWindEditDlg = new FogDisplayWindEditDlg(getParent().getShell(),
                    fdwd, this);
            fogWindEditDlg.open();
            fogWindEditDlg = null;
        }
    }

    @Override
    public void updateThresholdData(FogDisplayWindData fdwd) {
        updateFogDataArray(fdwd);
        populateList();
    }
}
