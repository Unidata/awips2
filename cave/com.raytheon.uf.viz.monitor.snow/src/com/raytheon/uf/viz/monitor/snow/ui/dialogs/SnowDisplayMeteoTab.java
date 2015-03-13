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
package com.raytheon.uf.viz.monitor.snow.ui.dialogs;

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
import com.raytheon.uf.viz.monitor.snow.threshold.SnowDisplayMeteoData;
import com.raytheon.uf.viz.monitor.snow.threshold.SnowThresholdMgr;
import com.raytheon.uf.viz.monitor.ui.dialogs.TabItemComp;
import com.raytheon.uf.viz.monitor.util.MonitorConfigConstants.SnowDisplay;
import com.raytheon.uf.viz.monitor.xml.AreaXML;
import com.raytheon.uf.viz.monitor.xml.ThresholdsXML;

/**
 * Snow Display Meteo Table.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 21, 2014 3086       skorolev    Cleaned code.
 * 
 * </pre>
 * 
 * @author
 * @version 1.0
 */
public class SnowDisplayMeteoTab extends TabItemComp implements
        IUpdateDisplayMeteo {

    /** Dialog used for editing the display meteo data. */
    private SnowDisplayMeteoEditDlg meteoEditDlg;

    /** List of zones */
    private List<String> areaIDArray;

    /** Snow Display Meteo Data. */
    private List<SnowDisplayMeteoData> snowDataArray;

    /**
     * Constructor
     * 
     * @param parent
     * @param duKey
     *            threshold usage data key
     */
    public SnowDisplayMeteoTab(TabFolder parent, DataUsageKey duKey) {
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
        GridData gd = new GridData(71, SWT.DEFAULT);
        Label fillerLbl = new Label(lblComp, SWT.CENTER);
        fillerLbl.setLayoutData(gd);

        /* Meteo */
        Composite meteoComp = createGroupComposite(lblComp, 10, null);
        createLabelComp(meteoComp, "Temp(F)", "", false);
        createLabelComp(meteoComp, "Dewpt(F)", "", false);
        createLabelComp(meteoComp, "Vis(mi)", "", true);
        createLabelComp(meteoComp, "SLP(mb)", "", false);
        createLabelComp(meteoComp, "Hourly", "Prcp(in)", false);
        createLabelComp(meteoComp, "Wind", "Chill(F)", false);
        createLabelComp(meteoComp, "Frost", "Bite\nTime(min)", false);
        createLabelComp(meteoComp, "Snow", "Depth(in)", false);
        createLabelComp(meteoComp, "SNINCR", "Hr(in)", false);
        createLabelComp(meteoComp, "SNINCR", "Tot(in)", false);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.ui.dialogs.TabItemComp#populateList()
     */
    @Override
    protected void populateList() {
        if (snowDataArray == null) {
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
        SnowDisplayMeteoData sdmd = null;

        for (int i = 0; i < snowDataArray.size(); i++) {
            sb = new StringBuilder();

            sdmd = snowDataArray.get(i);

            currentAreaID = sdmd.getAreaID();
            areaIDArray.add(currentAreaID);

            sb.append(String.format(areaIdFmt, currentAreaID));

            /* Temperature */
            appendIntData(sb, sdmd.getTempR(), sdmd.getTempY());

            /* Dew point */
            appendIntData(sb, sdmd.getDewpointR(), sdmd.getDewpointY());

            /* Visibility */
            visVal = sdmd.getVisR();
            tmpVisStr = rangeUtil.getVisString((int) visVal);
            sb.append(String.format(dataFmt, tmpVisStr));

            visVal = sdmd.getVisY();
            tmpVisStr = rangeUtil.getVisString((int) visVal);
            sb.append(String.format(dataFmt, tmpVisStr));

            /* SLP */
            appendIntData(sb, sdmd.getSlpR(), sdmd.getSlpY());

            /* Hourly Precipitation */
            appendDecimalData(sb, sdmd.getHrPrecipR(), sdmd.getHrPrecipY());

            /* Wind Chill */
            appendIntData(sb, sdmd.getWindChillR(), sdmd.getWindChillY());

            /* Frost Bite */
            appendIntData(sb, sdmd.getFrostBiteR(), sdmd.getFrostBiteY());

            /* Snow Depth */
            appendIntData(sb, sdmd.getSnowDepthR(), sdmd.getSnowDepthY());

            /* SNINCR Hourly */
            appendIntData(sb, sdmd.getSnincrHrlyR(), sdmd.getSnincrHrlyY());

            /* SNINCR Total */
            appendIntData(sb, sdmd.getSnincrTotR(), sdmd.getSnincrTotY());

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
        snowDataArray = new ArrayList<SnowDisplayMeteoData>();

        SnowThresholdMgr stm = SnowThresholdMgr.getInstance();

        String xmlKey;
        String areaID;

        ThresholdsXML threshXML = stm.getThresholdsXmlData(duKey);

        List<AreaXML> areasArray = threshXML.getAreas();

        for (AreaXML area : areasArray) {
            areaID = area.getAreaId();
            SnowDisplayMeteoData sdmd = new SnowDisplayMeteoData();

            sdmd.setAreaID(areaID);

            /* Tempature */
            xmlKey = SnowDisplay.SNOW_DISP_METEO_TEMP.getXmlKey();
            sdmd.setTempR(stm.getThresholdValue(duKey, threshKeyR, areaID,
                    xmlKey));
            sdmd.setTempY(stm.getThresholdValue(duKey, threshKeyY, areaID,
                    xmlKey));

            /* Dew point */
            xmlKey = SnowDisplay.SNOW_DISP_METEO_DEWPT.getXmlKey();
            sdmd.setDewpointR(stm.getThresholdValue(duKey, threshKeyR, areaID,
                    xmlKey));
            sdmd.setDewpointY(stm.getThresholdValue(duKey, threshKeyY, areaID,
                    xmlKey));

            /* Visibility */
            xmlKey = SnowDisplay.SNOW_DISP_METEO_VIS.getXmlKey();
            sdmd.setVisR(stm.getThresholdValue(duKey, threshKeyR, areaID,
                    xmlKey));
            sdmd.setVisY(stm.getThresholdValue(duKey, threshKeyY, areaID,
                    xmlKey));

            /* SLP */
            xmlKey = SnowDisplay.SNOW_DISP_METEO_SLP.getXmlKey();
            sdmd.setSlpR(stm.getThresholdValue(duKey, threshKeyR, areaID,
                    xmlKey));
            sdmd.setSlpY(stm.getThresholdValue(duKey, threshKeyY, areaID,
                    xmlKey));

            /* Hourly Precipitation */
            xmlKey = SnowDisplay.SNOW_DISP_METEO_HOURLY_PRECIP.getXmlKey();
            sdmd.setHrPrecipR(stm.getThresholdValue(duKey, threshKeyR, areaID,
                    xmlKey));
            sdmd.setHrPrecipY(stm.getThresholdValue(duKey, threshKeyY, areaID,
                    xmlKey));

            /* Wind Chill */
            xmlKey = SnowDisplay.SNOW_DISP_METEO_WIND_CHILL.getXmlKey();
            sdmd.setWindChillR(stm.getThresholdValue(duKey, threshKeyR, areaID,
                    xmlKey));
            sdmd.setWindChillY(stm.getThresholdValue(duKey, threshKeyY, areaID,
                    xmlKey));

            /* Frost Bite */
            xmlKey = SnowDisplay.SNOW_DISP_METEO_FROSTBITE.getXmlKey();
            sdmd.setFrostBiteR(stm.getThresholdValue(duKey, threshKeyR, areaID,
                    xmlKey));
            sdmd.setFrostBiteY(stm.getThresholdValue(duKey, threshKeyY, areaID,
                    xmlKey));

            /* Snow Depth */
            xmlKey = SnowDisplay.SNOW_DISP_METEO_SNOW_DEPTH.getXmlKey();
            sdmd.setSnowDepthR(stm.getThresholdValue(duKey, threshKeyR, areaID,
                    xmlKey));
            sdmd.setSnowDepthY(stm.getThresholdValue(duKey, threshKeyY, areaID,
                    xmlKey));

            /* SNINCR Hourly */
            xmlKey = SnowDisplay.SNOW_DISP_METEO_SNINCR_HOURLY.getXmlKey();
            sdmd.setSnincrHrlyR(stm.getThresholdValue(duKey, threshKeyR,
                    areaID, xmlKey));
            sdmd.setSnincrHrlyY(stm.getThresholdValue(duKey, threshKeyY,
                    areaID, xmlKey));

            /* SNINCR Total */
            xmlKey = SnowDisplay.SNOW_DISP_METEO_SNINCR_TOTAL.getXmlKey();
            sdmd.setSnincrTotR(stm.getThresholdValue(duKey, threshKeyR, areaID,
                    xmlKey));
            sdmd.setSnincrTotY(stm.getThresholdValue(duKey, threshKeyY, areaID,
                    xmlKey));

            /* Add data to array. */
            snowDataArray.add(sdmd);
        }
    }

    /**
     * Gets Data At First Selection.
     * 
     * @return selected data
     */
    private SnowDisplayMeteoData getDataAtFirstSelection() {
        int index = dataList.getSelectionIndex();
        return snowDataArray.get(index);
    }

    /**
     * Updates Data Array.
     * 
     * @param sdmd
     *            Display Meteo Data
     */
    private void updateDataArray(SnowDisplayMeteoData sdmd) {
        int[] dataListIndexes = dataList.getSelectionIndices();
        int currentIndex = 0;

        for (int i = 0; i < dataListIndexes.length; i++) {
            currentIndex = dataListIndexes[i];

            snowDataArray.get(currentIndex).updateData(sdmd);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.ui.dialogs.TabItemComp#commitDataToXML()
     */
    @Override
    public void commitDataToXML() {
        SnowThresholdMgr stm = SnowThresholdMgr.getInstance();

        String xmlKey;
        String areaID;

        for (SnowDisplayMeteoData sdmd : snowDataArray) {
            areaID = sdmd.getAreaID();

            /* Temperature */
            xmlKey = SnowDisplay.SNOW_DISP_METEO_TEMP.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    sdmd.getTempR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    sdmd.getTempY());

            /* Dew point */
            xmlKey = SnowDisplay.SNOW_DISP_METEO_DEWPT.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    sdmd.getDewpointR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    sdmd.getDewpointY());

            /* Visibility */
            xmlKey = SnowDisplay.SNOW_DISP_METEO_VIS.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    sdmd.getVisR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    sdmd.getVisY());

            /* SLP */
            xmlKey = SnowDisplay.SNOW_DISP_METEO_SLP.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    sdmd.getSlpR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    sdmd.getSlpY());

            /* Hourly Precipitation */
            xmlKey = SnowDisplay.SNOW_DISP_METEO_HOURLY_PRECIP.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    sdmd.getHrPrecipR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    sdmd.getHrPrecipY());

            /* Wind Chill */
            xmlKey = SnowDisplay.SNOW_DISP_METEO_WIND_CHILL.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    sdmd.getWindChillR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    sdmd.getWindChillY());

            /* Frost Bite */
            xmlKey = SnowDisplay.SNOW_DISP_METEO_FROSTBITE.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    sdmd.getFrostBiteR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    sdmd.getFrostBiteY());

            /* Snow Depth */
            xmlKey = SnowDisplay.SNOW_DISP_METEO_SNOW_DEPTH.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    sdmd.getSnowDepthR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    sdmd.getSnowDepthY());

            /* SNINCR Hourly */
            xmlKey = SnowDisplay.SNOW_DISP_METEO_SNINCR_HOURLY.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    sdmd.getSnincrHrlyR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    sdmd.getSnincrHrlyY());

            /* SNINCR Total */
            xmlKey = SnowDisplay.SNOW_DISP_METEO_SNINCR_TOTAL.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey,
                    sdmd.getSnincrTotR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey,
                    sdmd.getSnincrTotY());
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
        snowDataArray.clear();
        snowDataArray = null;
        populateList();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.ui.dialogs.TabItemComp#editDataAction()
     */
    @Override
    protected void editDataAction() {
        SnowDisplayMeteoData sdmd = getDataAtFirstSelection();

        if (meteoEditDlg == null) {
            meteoEditDlg = new SnowDisplayMeteoEditDlg(getParent().getShell(),
                    sdmd, this);
            meteoEditDlg.open();
            meteoEditDlg = null;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.monitor.snow.ui.dialogs.IUpdateDisplayMeteo#
     * updateThresholdData
     * (com.raytheon.uf.viz.monitor.snow.threshold.SnowDisplayMeteoData)
     */
    @Override
    public void updateThresholdData(SnowDisplayMeteoData sdmd) {
        updateDataArray(sdmd);
        populateList();
    }
}
