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

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.TabFolder;

import com.raytheon.uf.common.monitor.data.ObConst.DataUsageKey;
import com.raytheon.uf.viz.monitor.data.RangesUtil;
import com.raytheon.uf.viz.monitor.snow.threshold.SnowMonitorMeteoData;
import com.raytheon.uf.viz.monitor.snow.threshold.SnowThresholdMgr;
import com.raytheon.uf.viz.monitor.ui.dialogs.TabItemComp;
import com.raytheon.uf.viz.monitor.util.MonitorConfigConstants.SnowMonitor;
import com.raytheon.uf.viz.monitor.xml.AreaXML;
import com.raytheon.uf.viz.monitor.xml.ThresholdsXML;

public class SnowMonitorMeteoTab extends TabItemComp implements IUpdateMonitorMeteo
{
    private SnowMonitorMeteoEditDlg meteoEditDlg;
    
    private ArrayList<String> areaIDArray;
    
    private ArrayList<SnowMonitorMeteoData> snowDataArray;
    
    public SnowMonitorMeteoTab(TabFolder parent, DataUsageKey duKey)
    {
        super(parent, duKey);
    }

    @Override    
    protected void createListHeader(Composite parentComp)
    {
        Composite lblComp = new Composite(parentComp, SWT.NONE);
        GridLayout gl = new GridLayout(5, false);
        gl.horizontalSpacing = 0;   
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        lblComp.setLayout(gl);
        
        /*
         * Create filler label.
         */
        GridData gd = new GridData(74, SWT.DEFAULT);
        Label fillerLbl = new Label(lblComp, SWT.CENTER);
        fillerLbl.setLayoutData(gd);
        
        /*
         * Monitor Meteo
         */
        Composite meteoComp = createGroupComposite(lblComp, 7, null);
        createLabelComp(meteoComp, "Wind", "Speed(kt)", false);
        createLabelComp(meteoComp, "Peak", "Wind(kt)", false);    
        createLabelComp(meteoComp, "Gust", "Speed(kt)", false);            
        createLabelComp(meteoComp, "Temp(F)", "", false);
        createLabelComp(meteoComp, "Wind", "Chill(F)", false); 
        createLabelComp(meteoComp, "Vis(mi)", "", true);
        createLabelComp(meteoComp, "Snow", "Depth(in)", false);
    }    
    
    @Override
    protected void populateList()
    {   
        if (snowDataArray == null)
        {
            createDataArray();
        }
        
        boolean update = false;
        if (dataList.getItemCount() > 0)
        {
            update = true;
        }
        
        RangesUtil rangeUtil = RangesUtil.getInstance();
        
        areaIDArray = new ArrayList<String>();
        
        String tmpVisStr;
        String currentAreaID;
        
        double visVal = 0.0;
        
        StringBuilder sb = null;       
        SnowMonitorMeteoData smmd = null;
        
        for (int i = 0; i < snowDataArray.size(); i++)
        {
            sb = new StringBuilder();
            
            smmd = snowDataArray.get(i);
            
            currentAreaID = smmd.getAreaID();
            areaIDArray.add(currentAreaID);
            
            sb.append(String.format(areaIdFmt, currentAreaID));
            
            /*
             * Wind Speed
             */
            appendIntData(sb, smmd.getWindSpeedR(), smmd.getWindSpeedY());
            
            /*
             * Peak Wind
             */
            appendIntData(sb, smmd.getPeakWindR(), smmd.getPeakWindY());
            
            /*
             * Gust Speed
             */
            appendIntData(sb, smmd.getGustSpeedR(), smmd.getGustSpeedY());
            
            /*
             * Temperature
             */
            appendIntData(sb, smmd.getTempR(), smmd.getTempY());
            
            /*
             * Wind Chill
             */
            appendIntData(sb, smmd.getWindChillR(), smmd.getWindChillY());
            
            /*
             * Visibility
             */            
            visVal = smmd.getVisR();            
            tmpVisStr = rangeUtil.getVisString((int)visVal);
            sb.append(String.format(dataFmt, tmpVisStr));
            
            visVal = smmd.getVisY();           
            tmpVisStr = rangeUtil.getVisString((int)visVal);
            sb.append(String.format(dataFmt, tmpVisStr));
            
            /*
             * Snow Depth
             */
            appendIntData(sb, smmd.getSnowDepthR(), smmd.getSnowDepthY());
            
            /*
             * Append a space and add the data line to the list.
             */
            sb.append(" ");
            
            if (update == true)
            {
                dataList.setItem(i, sb.toString());
            }
            else
            {
                dataList.add(sb.toString());
            }            
        }
        
        packListControls();
    }
    
    private void createDataArray()
    {
        snowDataArray = new ArrayList<SnowMonitorMeteoData>();
        
        SnowThresholdMgr stm = SnowThresholdMgr.getInstance();   
        
        String xmlKey;
        String areaID;       
        
        ThresholdsXML threshXML = stm.getThresholdsXmlData(duKey);
        
        ArrayList<AreaXML> areasArray = threshXML.getAreas();
        
        for (AreaXML area : areasArray)
        {
            areaID = area.getAreaId();
            SnowMonitorMeteoData smmd = new SnowMonitorMeteoData();
            
            smmd.setAreaID(areaID);
            
            /*
             * Wind Speed
             */
            xmlKey = SnowMonitor.SNOW_MON_METEO_WIND_SPEED.getXmlKey();
            smmd.setWindSpeedR(stm.getThresholdValue(duKey, threshKeyR, areaID, xmlKey));
            smmd.setWindSpeedY(stm.getThresholdValue(duKey, threshKeyY, areaID, xmlKey));
            
            /*
             * Peak Wind
             */
            xmlKey = SnowMonitor.SNOW_MON_METEO_PEAK_WIND.getXmlKey();
            smmd.setPeakWindR(stm.getThresholdValue(duKey, threshKeyR, areaID, xmlKey));
            smmd.setPeakWindY(stm.getThresholdValue(duKey, threshKeyY, areaID, xmlKey));
            
            /*
             * Gust Speed
             */
            xmlKey = SnowMonitor.SNOW_MON_METEO_GUST_SPEED.getXmlKey();
            smmd.setGustSpeedR(stm.getThresholdValue(duKey, threshKeyR, areaID, xmlKey));
            smmd.setGustSpeedY(stm.getThresholdValue(duKey, threshKeyY, areaID, xmlKey));
            
            /*
             * Temperature
             */
            xmlKey = SnowMonitor.SNOW_MON_METEO_TEMP.getXmlKey();
            smmd.setTempR(stm.getThresholdValue(duKey, threshKeyR, areaID, xmlKey));
            smmd.setTempY(stm.getThresholdValue(duKey, threshKeyY, areaID, xmlKey));
            
            /*
             * Wind Chill
             */
            xmlKey = SnowMonitor.SNOW_MON_METEO_WIND_CHILL.getXmlKey();
            smmd.setWindChillR(stm.getThresholdValue(duKey, threshKeyR, areaID, xmlKey));
            smmd.setWindChillY(stm.getThresholdValue(duKey, threshKeyY, areaID, xmlKey));

            /*
             * Visibility
             */
            xmlKey = SnowMonitor.SNOW_MON_METEO_VIS.getXmlKey();
            smmd.setVisR(stm.getThresholdValue(duKey, threshKeyR, areaID, xmlKey));
            smmd.setVisY(stm.getThresholdValue(duKey, threshKeyY, areaID, xmlKey));

            /*
             * Snow Depth
             */
            xmlKey = SnowMonitor.SNOW_MON_METEO_SNOW_DEPTH.getXmlKey();
            smmd.setSnowDepthR(stm.getThresholdValue(duKey, threshKeyR, areaID, xmlKey));
            smmd.setSnowDepthY(stm.getThresholdValue(duKey, threshKeyY, areaID, xmlKey));
            
            /*
             * Add data to array.
             */
            snowDataArray.add(smmd);
        }
    }
    
    private SnowMonitorMeteoData getDataAtFirstSelection()
    {        
        int index = dataList.getSelectionIndex();
        
        return snowDataArray.get(index);        
    }
    
    private void updateDataArray(SnowMonitorMeteoData smmd)
    {        
        int[] dataListIndexes = dataList.getSelectionIndices();        
        int currentIndex = 0;
        
        for (int i = 0; i < dataListIndexes.length; i++)
        {
            currentIndex = dataListIndexes[i];
            
            snowDataArray.get(currentIndex).updateData(smmd);
        }        
    }
    
    @Override
    public void commitDataToXML()
    {
        SnowThresholdMgr stm = SnowThresholdMgr.getInstance();
        
        String xmlKey;
        String areaID;
        
        for (SnowMonitorMeteoData smmd : snowDataArray)
        {
            areaID = smmd.getAreaID();
            
            /*
             * Wind Speed
             */
            xmlKey = SnowMonitor.SNOW_MON_METEO_WIND_SPEED.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey, smmd.getWindSpeedR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey, smmd.getWindSpeedY());
            
            /*
             * Peak Wind
             */
            xmlKey = SnowMonitor.SNOW_MON_METEO_PEAK_WIND.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey, smmd.getPeakWindR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey, smmd.getPeakWindY());
            
            /*
             * Gust Speed
             */
            xmlKey = SnowMonitor.SNOW_MON_METEO_GUST_SPEED.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey, smmd.getGustSpeedR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey, smmd.getGustSpeedY());
            
            /*
             * Temperature
             */
            xmlKey = SnowMonitor.SNOW_MON_METEO_TEMP.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey, smmd.getTempR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey, smmd.getTempY());
            
            /*
             * Wind Chill
             */
            xmlKey = SnowMonitor.SNOW_MON_METEO_WIND_CHILL.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey, smmd.getWindChillR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey, smmd.getWindChillY());
            
            /*
             * Visibility
             */
            xmlKey = SnowMonitor.SNOW_MON_METEO_VIS.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey, smmd.getVisR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey, smmd.getVisY());
            
            /*
             * Snow Depth
             */
            xmlKey = SnowMonitor.SNOW_MON_METEO_SNOW_DEPTH.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey, smmd.getSnowDepthR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey, smmd.getSnowDepthY());
        }
    }

    @Override
    public void reloadData()
    {
        dataList.removeAll();
        snowDataArray.clear();
        snowDataArray = null;
        
        populateList();        
    }

    @Override
    protected void editDataAction()
    {
        SnowMonitorMeteoData smmd = getDataAtFirstSelection();
        
        if (meteoEditDlg == null)
        {
            meteoEditDlg = new SnowMonitorMeteoEditDlg(getParent().getShell(), smmd, this);
            meteoEditDlg.open();
            meteoEditDlg = null;
        }
    }
   
    @Override
    public void updateThresholdData(SnowMonitorMeteoData smmd)
    {
        updateDataArray(smmd);
        populateList();
    }
}
