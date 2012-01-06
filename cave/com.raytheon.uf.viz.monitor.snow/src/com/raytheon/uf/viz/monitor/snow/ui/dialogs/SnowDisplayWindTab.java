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
import com.raytheon.uf.viz.monitor.snow.threshold.SnowDisplayWindData;
import com.raytheon.uf.viz.monitor.snow.threshold.SnowThresholdMgr;
import com.raytheon.uf.viz.monitor.ui.dialogs.TabItemComp;
import com.raytheon.uf.viz.monitor.util.MonitorConfigConstants.SnowDisplay;
import com.raytheon.uf.viz.monitor.xml.AreaXML;
import com.raytheon.uf.viz.monitor.xml.ThresholdsXML;

public class SnowDisplayWindTab extends TabItemComp implements IUpdateDisplayWind
{
    private SnowDisplayWindEditDlg windEditDlg;
    
    private ArrayList<String> areaIDArray;
    
    private ArrayList<SnowDisplayWindData> snowDataArray;
    
    public SnowDisplayWindTab(TabFolder parent, DataUsageKey duKey)
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
        
        areaIDArray = new ArrayList<String>();
        
        String currentAreaID;
        
        StringBuilder sb = null;       
        SnowDisplayWindData sdwd = null;
        
        for (int i = 0; i < snowDataArray.size(); i++)
        {
            sb = new StringBuilder();
            
            sdwd = snowDataArray.get(i);
            
            currentAreaID = sdwd.getAreaID();
            areaIDArray.add(currentAreaID);
            
            sb.append(String.format(areaIdFmt, currentAreaID));
            
            /*
             * Wind Speed
             */            
            appendIntData(sb, sdwd.getWindWindSpeedR(), sdwd.getWindWindSpeedY());
            
            /*
             * Peak Wind
             */            
            appendIntData(sb, sdwd.getWindPeakR(), sdwd.getWindPeakY());
            
            /*
             * Gust Speed
             */            
            appendIntData(sb, sdwd.getWindGustR(), sdwd.getWindGustY());
            
            /*
             * Wind Direction From
             */            
            appendIntData(sb,  sdwd.getWindDirFromY(),sdwd.getWindDirFromR());
            
            /*
             * Wind Direction To
             */            
            appendIntData(sb, sdwd.getWindDirToR(), sdwd.getWindDirToY());
            
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
        snowDataArray = new ArrayList<SnowDisplayWindData>();
        
        SnowThresholdMgr stm = SnowThresholdMgr.getInstance();   
        
        String xmlKey;
        String areaID;       
        
        ThresholdsXML threshXML = stm.getThresholdsXmlData(duKey);
        
        ArrayList<AreaXML> areasArray = threshXML.getAreas();
        
        for (AreaXML area : areasArray)
        {
            areaID = area.getAreaId();
            SnowDisplayWindData sdwd = new SnowDisplayWindData();
            
            sdwd.setAreaID(areaID);

            /*
             * Wind Speed
             */
            xmlKey = SnowDisplay.SNOW_DISP_WIND_WIND_SPEED.getXmlKey();
            sdwd.setWindWindSpeedR(stm.getThresholdValue(duKey, threshKeyR, areaID, xmlKey));
            sdwd.setWindWindSpeedY(stm.getThresholdValue(duKey, threshKeyY, areaID, xmlKey));

            /*
             * Peak Wind
             */
            xmlKey = SnowDisplay.SNOW_DISP_WIND_PEAK_WIND.getXmlKey();
            sdwd.setWindPeakR(stm.getThresholdValue(duKey, threshKeyR, areaID, xmlKey));
            sdwd.setWindPeakY(stm.getThresholdValue(duKey, threshKeyY, areaID, xmlKey));
            
            /*
             * Gust Speed
             */
            xmlKey = SnowDisplay.SNOW_DISP_WIND_GUST_SPEED.getXmlKey();
            sdwd.setWindGustR(stm.getThresholdValue(duKey, threshKeyR, areaID, xmlKey));
            sdwd.setWindGustY(stm.getThresholdValue(duKey, threshKeyY, areaID, xmlKey));
            
            /*
             * Wind Direction From
             */
            xmlKey = SnowDisplay.SNOW_DISP_WIND_DIR_FROM.getXmlKey();
            sdwd.setWindDirFromR(stm.getThresholdValue(duKey, threshKeyR, areaID, xmlKey));
            sdwd.setWindDirFromY(stm.getThresholdValue(duKey, threshKeyY, areaID, xmlKey));
            
            /*
             * Wind Direction From
             */
            xmlKey = SnowDisplay.SNOW_DISP_WIND_DIR_TO.getXmlKey();
            sdwd.setWindDirToR(stm.getThresholdValue(duKey, threshKeyR, areaID, xmlKey));
            sdwd.setWindDirToY(stm.getThresholdValue(duKey, threshKeyY, areaID, xmlKey));
            
            /*
             * Add data to array.
             */
            snowDataArray.add(sdwd);
        }
    }
    
    private SnowDisplayWindData getDataAtFirstSelection()
    {        
        int index = dataList.getSelectionIndex();
        
        return snowDataArray.get(index);        
    }
    
    private void updateDataArray(SnowDisplayWindData sdwd)
    {        
        int[] dataListIndexes = dataList.getSelectionIndices();        
        int currentIndex = 0;
        
        for (int i = 0; i < dataListIndexes.length; i++)
        {
            currentIndex = dataListIndexes[i];
            
            snowDataArray.get(currentIndex).updateData(sdwd);
        }        
    }
    
    @Override
    public void commitDataToXML()
    {
        SnowThresholdMgr stm = SnowThresholdMgr.getInstance();
        
        String xmlKey;
        String areaID;
        
        for (SnowDisplayWindData sdwd : snowDataArray)
        {
            areaID = sdwd.getAreaID();
            
            /*
             * Wind Speed
             */
            xmlKey = SnowDisplay.SNOW_DISP_WIND_WIND_SPEED.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey, sdwd.getWindWindSpeedR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey, sdwd.getWindWindSpeedY());
            
            /*
             * Peak Wind
             */
            xmlKey = SnowDisplay.SNOW_DISP_WIND_PEAK_WIND.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey, sdwd.getWindPeakR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey, sdwd.getWindPeakY());
            
            /*
             * Gust Speed
             */
            xmlKey = SnowDisplay.SNOW_DISP_WIND_GUST_SPEED.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey, sdwd.getWindGustR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey, sdwd.getWindGustY());
            
            /*
             * Wind Direction From
             */
            xmlKey = SnowDisplay.SNOW_DISP_WIND_DIR_FROM.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey, sdwd.getWindDirFromR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey, sdwd.getWindDirFromY());
            
            /*
             * Wind Direction To
             */
            xmlKey = SnowDisplay.SNOW_DISP_WIND_DIR_TO.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey, sdwd.getWindDirToR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey, sdwd.getWindDirToY());
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
        SnowDisplayWindData sdwd = getDataAtFirstSelection();
        
        if (windEditDlg == null)
        {
            windEditDlg = new SnowDisplayWindEditDlg(getParent().getShell(), sdwd, this);
            windEditDlg.open();
            windEditDlg = null;
        } 
    }    

    @Override
    public void updateThresholdData(SnowDisplayWindData sdwd)
    {
        updateDataArray(sdwd);
        populateList();
    }
}

