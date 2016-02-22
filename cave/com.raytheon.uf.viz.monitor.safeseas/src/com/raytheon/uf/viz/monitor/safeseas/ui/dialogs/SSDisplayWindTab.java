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
package com.raytheon.uf.viz.monitor.safeseas.ui.dialogs;

import java.util.ArrayList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.TabFolder;

import com.raytheon.uf.common.monitor.data.MonitorConfigConstants.SafeSeasDisplay;
import com.raytheon.uf.common.monitor.data.ObConst.DataUsageKey;
import com.raytheon.uf.common.monitor.xml.AreaXML;
import com.raytheon.uf.common.monitor.xml.ThresholdsXML;
import com.raytheon.uf.viz.monitor.safeseas.threshold.SSDisplayWindData;
import com.raytheon.uf.viz.monitor.safeseas.threshold.SSThresholdMgr;
import com.raytheon.uf.viz.monitor.ui.dialogs.TabItemComp;

/**
 * SAFESEAS Display Wind Table.
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
public class SSDisplayWindTab extends TabItemComp implements IUpdateDisplayWind {
    private SSDisplayWindEditDlg windEditDlg;

    private ArrayList<String> areaIDArray;

    private ArrayList<SSDisplayWindData> ssDataArray;

    public SSDisplayWindTab(TabFolder parent, DataUsageKey duKey) {
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

    @Override
    protected void populateList() {
        if (ssDataArray == null) {
            createDataArray();
        }

        boolean update = false;
        if (dataList.getItemCount() > 0) {
            update = true;
        }

        areaIDArray = new ArrayList<String>();

        String currentAreaID;

        StringBuilder sb = null;
        SSDisplayWindData ssdwd = null;

        for (int i = 0; i < ssDataArray.size(); i++) {
            sb = new StringBuilder();

            ssdwd = ssDataArray.get(i);

            currentAreaID = ssdwd.getAreaID();
            areaIDArray.add(currentAreaID);

            sb.append(String.format(areaIdFmt, currentAreaID));

            /*
             * Wind Speed
             */
            appendIntData(sb, ssdwd.getWindSpeedR(), ssdwd.getWindSpeedY());

            /*
             * Peak Wind
             */
            appendIntData(sb, ssdwd.getPeakWindR(), ssdwd.getPeakWindY());

            /*
             * Gust Speed
             */
            appendIntData(sb, ssdwd.getGustSpeedR(), ssdwd.getGustSpeedY());

            /*
             * Wind Direction From
             */
            appendIntData(sb, ssdwd.getWindDirFromY(), ssdwd.getWindDirFromR());

            /*
             * Wind Direction To
             */
            appendIntData(sb, ssdwd.getWindDirToR(), ssdwd.getWindDirToY());

            /*
             * Append a space and add the data line to the list.
             */
            if (update == true) {
                dataList.setItem(i, sb.toString());
            } else {
                dataList.add(sb.toString());
            }
        }

        packListControls();
    }

    private void createDataArray() {
        ssDataArray = new ArrayList<SSDisplayWindData>();

        SSThresholdMgr sstm = SSThresholdMgr.getInstance();

        String xmlKey;
        String areaID;

        ThresholdsXML threshXML = sstm.getThresholdsXmlData(duKey);

        ArrayList<AreaXML> areasArray = threshXML.getAreas();

        for (AreaXML area : areasArray) {
            areaID = area.getAreaId();
            SSDisplayWindData ssdwd = new SSDisplayWindData();

            ssdwd.setAreaID(areaID);

            /*
             * Wind Speed
             */
            xmlKey = SafeSeasDisplay.SS_DISP_WIND_WIND_SPEED.getXmlKey();
            ssdwd.setWindSpeedR(sstm.getThresholdValue(duKey, threshKeyR,
                    areaID, xmlKey));
            ssdwd.setWindSpeedY(sstm.getThresholdValue(duKey, threshKeyY,
                    areaID, xmlKey));

            /*
             * Peak Wind
             */
            xmlKey = SafeSeasDisplay.SS_DISP_WIND_PEAK_WIND.getXmlKey();
            ssdwd.setPeakWindR(sstm.getThresholdValue(duKey, threshKeyR,
                    areaID, xmlKey));
            ssdwd.setPeakWindY(sstm.getThresholdValue(duKey, threshKeyY,
                    areaID, xmlKey));

            /*
             * Gust Speed
             */
            xmlKey = SafeSeasDisplay.SS_DISP_WIND_GUST_SPEED.getXmlKey();
            ssdwd.setGustSpeedR(sstm.getThresholdValue(duKey, threshKeyR,
                    areaID, xmlKey));
            ssdwd.setGustSpeedY(sstm.getThresholdValue(duKey, threshKeyY,
                    areaID, xmlKey));

            /*
             * Wind Direction From
             */
            xmlKey = SafeSeasDisplay.SS_DISP_WIND_DIR_FROM.getXmlKey();
            ssdwd.setWindDirFromR(sstm.getThresholdValue(duKey, threshKeyR,
                    areaID, xmlKey));
            ssdwd.setWindDirFromY(sstm.getThresholdValue(duKey, threshKeyY,
                    areaID, xmlKey));

            /*
             * Wind Direction To
             */
            xmlKey = SafeSeasDisplay.SS_DISP_WIND_DIR_TO.getXmlKey();
            ssdwd.setWindDirToR(sstm.getThresholdValue(duKey, threshKeyR,
                    areaID, xmlKey));
            ssdwd.setWindDirToY(sstm.getThresholdValue(duKey, threshKeyY,
                    areaID, xmlKey));

            /*
             * Add data to array.
             */
            ssDataArray.add(ssdwd);
        }
    }

    private SSDisplayWindData getDataAtFirstSelection() {

        int index = dataList.getSelectionIndex();

        return ssDataArray.get(index);

    }

    private void updateDataArray(SSDisplayWindData ssdwd) {
        int[] dataListIndexes = dataList.getSelectionIndices();
        int currentIndex = 0;

        for (int i = 0; i < dataListIndexes.length; i++) {
            currentIndex = dataListIndexes[i];

            ssDataArray.get(currentIndex).updateData(ssdwd);
        }
    }

    @Override
    public void commitDataToXML() {
        SSThresholdMgr sstm = SSThresholdMgr.getInstance();

        String xmlKey;
        String areaID;

        for (SSDisplayWindData ssdwd : ssDataArray) {
            areaID = ssdwd.getAreaID();

            /*
             * Wind Speed
             */
            xmlKey = SafeSeasDisplay.SS_DISP_WIND_WIND_SPEED.getXmlKey();
            sstm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    ssdwd.getWindSpeedR());
            sstm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    ssdwd.getWindSpeedY());

            /*
             * Peak Wind
             */
            xmlKey = SafeSeasDisplay.SS_DISP_WIND_PEAK_WIND.getXmlKey();
            sstm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    ssdwd.getPeakWindR());
            sstm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    ssdwd.getPeakWindY());

            /*
             * Gust Speed
             */
            xmlKey = SafeSeasDisplay.SS_DISP_WIND_GUST_SPEED.getXmlKey();
            sstm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    ssdwd.getGustSpeedR());
            sstm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    ssdwd.getGustSpeedY());

            /*
             * Wind Direction From
             */
            xmlKey = SafeSeasDisplay.SS_DISP_WIND_DIR_FROM.getXmlKey();
            sstm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    ssdwd.getWindDirFromR());
            sstm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    ssdwd.getWindDirFromY());

            /*
             * Wind Direction To
             */
            xmlKey = SafeSeasDisplay.SS_DISP_WIND_DIR_TO.getXmlKey();
            sstm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    ssdwd.getWindDirToR());
            sstm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    ssdwd.getWindDirToY());
        }
    }

    @Override
    public void reloadData() {
        dataList.removeAll();
        ssDataArray.clear();
        ssDataArray = null;

        populateList();
    }

    @Override
    protected void editDataAction() {
        SSDisplayWindData ssdwd = getDataAtFirstSelection();

        if (windEditDlg == null) {
            windEditDlg = new SSDisplayWindEditDlg(getParent().getShell(),
                    ssdwd, this);
            windEditDlg.open();
            windEditDlg = null;
        }
    }

    @Override
    public void updateThresholdData(SSDisplayWindData ssdwd) {
        updateDataArray(ssdwd);
        populateList();
    }
}
