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
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.TabFolder;

import com.raytheon.uf.common.monitor.data.ObConst.DataUsageKey;
import com.raytheon.uf.viz.monitor.data.RangesUtil;
import com.raytheon.uf.viz.monitor.fog.threshold.FogDisplayMeteoData;
import com.raytheon.uf.viz.monitor.fog.threshold.FogThresholdMgr;
import com.raytheon.uf.viz.monitor.ui.dialogs.TabItemComp;
import com.raytheon.uf.viz.monitor.util.MonitorConfigConstants.FogDisplay;
import com.raytheon.uf.viz.monitor.xml.AreaXML;
import com.raytheon.uf.viz.monitor.xml.ThresholdsXML;

/**
 * Fog Display Meteo Table.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 21, 2014 3086       skorolev    Cleaned code
 * 
 * </pre>
 * 
 * @author
 * @version 1.0
 */
public class FogDisplayMeteoTab extends TabItemComp implements
        IUpdateDisplayMeteo {
    /** Dialog used for editing the display meteo data. */
    private FogDisplayMeteoEditDlg fogMeteoEditDlg;

    /** List of zones */
    private List<String> areaIDArray;

    /** Fog Display Meteo Data. */
    private List<FogDisplayMeteoData> fogDataArray;

    /**
     * Constructor
     * 
     * @param parent
     * @param duKey
     *            threshold usage data key
     */
    public FogDisplayMeteoTab(TabFolder parent, DataUsageKey duKey) {
        super(parent, duKey);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.monitor.ui.dialogs.TabItemComp#createListHeader(org
     * .eclipse.swt.widgets.Composite)
     */
    @Override
    protected void createListHeader(Composite parentComp) {
        Composite lblComp = new Composite(parentComp, SWT.NONE);
        GridLayout gl = new GridLayout(5, false);
        gl.horizontalSpacing = 0;
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        lblComp.setLayout(gl);

        /* Create filler label. */
        GridData gd = new GridData(68, SWT.DEFAULT);
        Label fillerLbl = new Label(lblComp, SWT.CENTER);
        fillerLbl.setLayoutData(gd);

        /* Meteo */
        Composite meteoComp = createGroupComposite(lblComp, 6, null);
        createLabelComp(meteoComp, "Vis(mi)", "", true);
        createLabelComp(meteoComp, "Ceiling", "(100ft)", false);
        createLabelComp(meteoComp, "Temp(F)", "", false);
        createLabelComp(meteoComp, "Dewpt(F)", "", false);
        createLabelComp(meteoComp, "T-Td(F)", "", false);
        createLabelComp(meteoComp, "Rel Hum(%)", "", false);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.ui.dialogs.TabItemComp#populateList()
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

        RangesUtil rangeUtil = RangesUtil.getInstance();

        areaIDArray = new ArrayList<String>();

        String tmpVisStr;
        String currentAreaID;

        double visVal = 0.0;

        StringBuilder sb = null;
        FogDisplayMeteoData fdmd = null;

        for (int i = 0; i < fogDataArray.size(); i++) {
            sb = new StringBuilder();

            fdmd = fogDataArray.get(i);

            currentAreaID = fdmd.getAreaID();
            areaIDArray.add(currentAreaID);

            sb.append(String.format(areaIdFmt, currentAreaID));

            /* Visibility */
            visVal = fdmd.getMeteoVisR();
            tmpVisStr = rangeUtil.getVisString((int) visVal);
            sb.append(String.format(dataFmt, tmpVisStr));

            visVal = fdmd.getMeteoVisY();
            tmpVisStr = rangeUtil.getVisString((int) visVal);
            sb.append(String.format(dataFmt, tmpVisStr));

            /* Ceiling */
            appendIntData(sb, fdmd.getMeteoCeilingR(), fdmd.getMeteoCeilingY());

            /* Temperature */
            appendIntData(sb, fdmd.getMeteoTempR(), fdmd.getMeteoTempY());

            /* Dewpoint */
            appendIntData(sb, fdmd.getMeteoDewpointR(),
                    fdmd.getMeteoDewpointY());

            /* T-Td */
            appendIntData(sb, fdmd.getMeteoTtdR(), fdmd.getMeteoTtdY());

            /* Relative Humidity */
            appendIntData(sb, fdmd.getMeteoRelHumR(), fdmd.getMeteoRelHumY());

            /* Append a space and add the data line to the list. */
            sb.append(" ");

            if (update == true) {
                dataList.setItem(i, sb.toString());
            } else {
                dataList.add(sb.toString());
            }
        }
        packListControls();
    }

    /**
     * Creates Data Array.
     */
    private void createDataArray() {
        fogDataArray = new ArrayList<FogDisplayMeteoData>();

        FogThresholdMgr ftm = FogThresholdMgr.getInstance();

        String xmlKey;
        String areaID;

        ThresholdsXML threshXML = ftm.getThresholdsXmlData(duKey);

        List<AreaXML> areasArray = threshXML.getAreas();

        for (AreaXML area : areasArray) {
            areaID = area.getAreaId();
            FogDisplayMeteoData fdmd = new FogDisplayMeteoData();

            fdmd.setAreaID(areaID);

            /* Visibility */
            xmlKey = FogDisplay.FOG_DISP_METEO_VIS.getXmlKey();
            fdmd.setMeteoVisR(ftm.getThresholdValue(duKey, threshKeyR, areaID,
                    xmlKey));
            fdmd.setMeteoVisY(ftm.getThresholdValue(duKey, threshKeyY, areaID,
                    xmlKey));

            /* Ceiling */
            xmlKey = FogDisplay.FOG_DISP_METEO_CEILING.getXmlKey();
            fdmd.setMeteoCeilingR(ftm.getThresholdValue(duKey, threshKeyR,
                    areaID, xmlKey));
            fdmd.setMeteoCeilingY(ftm.getThresholdValue(duKey, threshKeyY,
                    areaID, xmlKey));

            /* Temperature */
            xmlKey = FogDisplay.FOG_DISP_METEO_TEMP.getXmlKey();
            fdmd.setMeteoTempR(ftm.getThresholdValue(duKey, threshKeyR, areaID,
                    xmlKey));
            fdmd.setMeteoTempY(ftm.getThresholdValue(duKey, threshKeyY, areaID,
                    xmlKey));

            /* Dewpoint */
            xmlKey = FogDisplay.FOG_DISP_METEO_DEWPT.getXmlKey();
            fdmd.setMeteoDewpointR(ftm.getThresholdValue(duKey, threshKeyR,
                    areaID, xmlKey));
            fdmd.setMeteoDewpointY(ftm.getThresholdValue(duKey, threshKeyY,
                    areaID, xmlKey));

            /* T-Td */
            xmlKey = FogDisplay.FOG_DISP_METEO_T_TD.getXmlKey();
            fdmd.setMeteoTtdR(ftm.getThresholdValue(duKey, threshKeyR, areaID,
                    xmlKey));
            fdmd.setMeteoTtdY(ftm.getThresholdValue(duKey, threshKeyY, areaID,
                    xmlKey));

            /* Relative Humidity */
            xmlKey = FogDisplay.FOG_DISP_METEO_REL_HUMIDITY.getXmlKey();
            fdmd.setMeteoRelHumR(ftm.getThresholdValue(duKey, threshKeyR,
                    areaID, xmlKey));
            fdmd.setMeteoRelHumY(ftm.getThresholdValue(duKey, threshKeyY,
                    areaID, xmlKey));

            /* Add data to array. */
            fogDataArray.add(fdmd);
        }
    }

    /**
     * Gets Data At First Selection.
     * 
     * @return selected data
     */
    private FogDisplayMeteoData getDataAtFirstSelection() {
        int index = dataList.getSelectionIndex();
        return fogDataArray.get(index);
    }

    /**
     * Updates Data Array.
     * 
     * @param fdmd
     *            Display Meteo Data
     */
    private void updateDataArray(FogDisplayMeteoData fdmd) {
        int[] dataListIndexes = dataList.getSelectionIndices();
        int currentIndex = 0;

        for (int i = 0; i < dataListIndexes.length; i++) {
            currentIndex = dataListIndexes[i];
            fogDataArray.get(currentIndex).updateData(fdmd);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.ui.dialogs.TabItemComp#commitDataToXML()
     */
    @Override
    public void commitDataToXML() {
        FogThresholdMgr ftm = FogThresholdMgr.getInstance();

        String xmlKey;
        String areaID;

        for (FogDisplayMeteoData fdmd : fogDataArray) {
            areaID = fdmd.getAreaID();

            /* Visibility */
            xmlKey = FogDisplay.FOG_DISP_METEO_VIS.getXmlKey();
            ftm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    fdmd.getMeteoVisR());
            ftm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    fdmd.getMeteoVisY());

            /* Ceiling */
            xmlKey = FogDisplay.FOG_DISP_METEO_CEILING.getXmlKey();
            ftm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    fdmd.getMeteoCeilingR());
            ftm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    fdmd.getMeteoCeilingY());

            /* Temperature */
            xmlKey = FogDisplay.FOG_DISP_METEO_TEMP.getXmlKey();
            ftm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    fdmd.getMeteoTempR());
            ftm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    fdmd.getMeteoTempY());

            /* Dewpoint */
            xmlKey = FogDisplay.FOG_DISP_METEO_DEWPT.getXmlKey();
            ftm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    fdmd.getMeteoDewpointR());
            ftm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    fdmd.getMeteoDewpointY());

            /* T-Td */
            xmlKey = FogDisplay.FOG_DISP_METEO_T_TD.getXmlKey();
            ftm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    fdmd.getMeteoTtdR());
            ftm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    fdmd.getMeteoTtdY());

            /* Relative Humidity */
            xmlKey = FogDisplay.FOG_DISP_METEO_REL_HUMIDITY.getXmlKey();
            ftm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    fdmd.getMeteoRelHumR());
            ftm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    fdmd.getMeteoRelHumY());
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.ui.dialogs.TabItemComp#reloadData()
     */
    @Override
    public void reloadData() {
        dataList.removeAll();
        fogDataArray.clear();
        fogDataArray = null;
        populateList();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.ui.dialogs.TabItemComp#editDataAction()
     */
    @Override
    protected void editDataAction() {
        FogDisplayMeteoData fdmd = getDataAtFirstSelection();

        if (fogMeteoEditDlg == null) {
            fogMeteoEditDlg = new FogDisplayMeteoEditDlg(
                    getParent().getShell(), fdmd, this);
            fogMeteoEditDlg.open();
            fogMeteoEditDlg = null;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.fog.ui.dialogs.IUpdateDisplayMeteo#
     * updateThresholdData
     * (com.raytheon.uf.viz.monitor.fog.threshold.FogDisplayMeteoData)
     */
    @Override
    public void updateThresholdData(FogDisplayMeteoData fdmd) {
        updateDataArray(fdmd);
        populateList();
    }
}
