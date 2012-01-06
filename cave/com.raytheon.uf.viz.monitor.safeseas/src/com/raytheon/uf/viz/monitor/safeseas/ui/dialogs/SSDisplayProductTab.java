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

import com.raytheon.uf.common.monitor.data.ObConst.DataUsageKey;
import com.raytheon.uf.viz.monitor.safeseas.threshold.SSDisplayProductData;
import com.raytheon.uf.viz.monitor.safeseas.threshold.SSThresholdMgr;
import com.raytheon.uf.viz.monitor.ui.dialogs.TabItemComp;
import com.raytheon.uf.viz.monitor.util.MonitorConfigConstants.SafeSeasDisplay;
import com.raytheon.uf.viz.monitor.xml.AreaXML;
import com.raytheon.uf.viz.monitor.xml.ThresholdsXML;

public class SSDisplayProductTab extends TabItemComp implements IUpdateDisplayProduct
{
    SSDisplayProductEditDlg productEditDlg;
    
    private ArrayList<String> areaIDArray;
    
    private ArrayList<SSDisplayProductData> ssDataArray;
    
    public SSDisplayProductTab(TabFolder parent, DataUsageKey duKey)
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
        GridData gd = new GridData(75, SWT.DEFAULT);
        Label fillerLbl = new Label(lblComp, SWT.CENTER);
        fillerLbl.setLayoutData(gd);
        
        /*
         * Small AirCraft Warning
         */
        Composite smallAircraftComp = createGroupComposite(lblComp, 4, "Small Craft Advisory");        
        createLabelComp(smallAircraftComp, "Wind", "Speed(kt)", false);
        createLabelComp(smallAircraftComp, "Gust", "Speed(kt)", false);
        createLabelComp(smallAircraftComp, "Peak", "Wind(kt)", false);    
        createLabelComp(smallAircraftComp, "Wave", "Height(ft)", false);
        
        /*
         * Gale Warning
         */
        Composite galeWarningComp = createGroupComposite(lblComp, 3, "Gale Warning");        
        createLabelComp(galeWarningComp, "Wind", "Speed(kt)", false);
        createLabelComp(galeWarningComp, "Gust", "Speed(kt)", false);
        createLabelComp(galeWarningComp, "Peak", "Wind(kt)", false); 
        
        /*
         * Storm Warning
         */
        Composite stormWarningComp = createGroupComposite(lblComp, 3, "Storm Warning");        
        createLabelComp(stormWarningComp, "Wind", "Speed(kt)", false);
        createLabelComp(stormWarningComp, "Gust", "Speed(kt)", false);
        createLabelComp(stormWarningComp, "Peak", "Wind(kt)", false); 
        
        /*
         * HFWW (Hurricane Force Wind Warning)
         */
        Composite hfwwComp = createGroupComposite(lblComp, 3, "HFWW");        
        createLabelComp(hfwwComp, "Wind", "Speed(kt)", false);
        createLabelComp(hfwwComp, "Gust", "Speed(kt)", false);
        createLabelComp(hfwwComp, "Peak", "Wind(kt)", false); 
    }    
    
    @Override
    protected void populateList()
    {   
        if (ssDataArray == null)
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
        SSDisplayProductData ssdpd = null;
        
        for (int i = 0; i < ssDataArray.size(); i++)
        {
            sb = new StringBuilder();
            
            ssdpd = ssDataArray.get(i);
            
            currentAreaID = ssdpd.getAreaID();
            areaIDArray.add(currentAreaID);
            
            sb.append(String.format(areaIdFmt, currentAreaID));
            
            /*
             * Small Craft Advisory
             */
            appendIntData(sb, ssdpd.getScaWindSpeedR(), ssdpd.getScaWindSpeedY());
            appendIntData(sb, ssdpd.getScaGustSpeedR(), ssdpd.getScaGustSpeedY());
            appendIntData(sb, ssdpd.getScaPeakWindR(), ssdpd.getScaPeakWindY());
            appendIntData(sb, ssdpd.getScaWaveHgtR(), ssdpd.getScaWaveHgtY());
            
            /*
             * Gale Warning
             */
            appendIntData(sb, ssdpd.getGaleWindSpeedR(), ssdpd.getGaleWindSpeedY());
            appendIntData(sb, ssdpd.getGaleGustSpeedR(), ssdpd.getGaleGustSpeedY());
            appendIntData(sb, ssdpd.getGalePeakWindR(), ssdpd.getGalePeakWindY());
            
            /*
             * Storm Warning
             */
            appendIntData(sb, ssdpd.getStormWrnWindSpeedR(), ssdpd.getStormWrnWindSpeedY());
            appendIntData(sb, ssdpd.getStormWrnGustSpeedR(), ssdpd.getStormWrnGustSpeedY());
            appendIntData(sb, ssdpd.getStormWrnPeakWindR(), ssdpd.getStormWrnPeakWindY());
            
            /*
             * HFWW
             */
            appendIntData(sb, ssdpd.getHfwwWindSpeedR(), ssdpd.getHfwwWindSpeedY());
            appendIntData(sb, ssdpd.getHfwwGustSpeedR(), ssdpd.getHfwwGustSpeedY());
            appendIntData(sb, ssdpd.getHfwwPeakWindR(), ssdpd.getHfwwPeakWindY());
            
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
        ssDataArray = new ArrayList<SSDisplayProductData>();
        
        SSThresholdMgr sstm = SSThresholdMgr.getInstance();   
        
        String xmlKey;
        String areaID;       
        
        ThresholdsXML threshXML = sstm.getThresholdsXmlData(duKey);
        
        ArrayList<AreaXML> areasArray = threshXML.getAreas();
        
        for (AreaXML area : areasArray)
        {
            areaID = area.getAreaId();
            SSDisplayProductData ssdpd = new SSDisplayProductData();
            
            ssdpd.setAreaID(areaID);

            /*
             * Small Craft Advisory
             */
            xmlKey = SafeSeasDisplay.SS_DISP_PROD_SCA_WIND_SPEED.getXmlKey();
            ssdpd.setScaWindSpeedR(sstm.getThresholdValue(duKey, threshKeyR, areaID, xmlKey));
            ssdpd.setScaWindSpeedY(sstm.getThresholdValue(duKey, threshKeyY, areaID, xmlKey));
            
            xmlKey = SafeSeasDisplay.SS_DISP_PROD_SCA_GUST_SPEED.getXmlKey();
            ssdpd.setScaGustSpeedR(sstm.getThresholdValue(duKey, threshKeyR, areaID, xmlKey));
            ssdpd.setScaGustSpeedY(sstm.getThresholdValue(duKey, threshKeyY, areaID, xmlKey));
            
            xmlKey = SafeSeasDisplay.SS_DISP_PROD_SCA_PEAK_WIND.getXmlKey();
            ssdpd.setScaPeakWindR(sstm.getThresholdValue(duKey, threshKeyR, areaID, xmlKey));
            ssdpd.setScaPeakWindY(sstm.getThresholdValue(duKey, threshKeyY, areaID, xmlKey));
            
            xmlKey = SafeSeasDisplay.SS_DISP_PROD_SCA_WAVE_HT.getXmlKey();
            ssdpd.setScaWaveHgtR(sstm.getThresholdValue(duKey, threshKeyR, areaID, xmlKey));
            ssdpd.setScaWaveHgtY(sstm.getThresholdValue(duKey, threshKeyY, areaID, xmlKey));
            
            /*
             * Gale Warning
             */
            xmlKey = SafeSeasDisplay.SS_DISP_PROD_GALE_WIND_SPEED.getXmlKey();
            ssdpd.setGaleWindSpeedR(sstm.getThresholdValue(duKey, threshKeyR, areaID, xmlKey));
            ssdpd.setGaleWindSpeedY(sstm.getThresholdValue(duKey, threshKeyY, areaID, xmlKey));
            
            xmlKey = SafeSeasDisplay.SS_DISP_PROD_GALE_GUST_SPEED.getXmlKey();
            ssdpd.setGaleGustSpeedR(sstm.getThresholdValue(duKey, threshKeyR, areaID, xmlKey));
            ssdpd.setGaleGustSpeedY(sstm.getThresholdValue(duKey, threshKeyY, areaID, xmlKey));
            
            xmlKey = SafeSeasDisplay.SS_DISP_PROD_GALE_PEAK_WIND.getXmlKey();
            ssdpd.setGalePeakWindR(sstm.getThresholdValue(duKey, threshKeyR, areaID, xmlKey));
            ssdpd.setGalePeakWindY(sstm.getThresholdValue(duKey, threshKeyY, areaID, xmlKey));
            
            /*
             * Storm Warning
             */
            xmlKey = SafeSeasDisplay.SS_DISP_PROD_STORM_WIND_SPEED.getXmlKey();
            ssdpd.setStormWrnWindSpeedR(sstm.getThresholdValue(duKey, threshKeyR, areaID, xmlKey));
            ssdpd.setStormWrnWindSpeedY(sstm.getThresholdValue(duKey, threshKeyY, areaID, xmlKey));
            
            xmlKey = SafeSeasDisplay.SS_DISP_PROD_STORM_GUST_SPEED.getXmlKey();
            ssdpd.setStormWrnGustSpeedR(sstm.getThresholdValue(duKey, threshKeyR, areaID, xmlKey));
            ssdpd.setStormWrnGustSpeedY(sstm.getThresholdValue(duKey, threshKeyY, areaID, xmlKey));
            
            xmlKey = SafeSeasDisplay.SS_DISP_PROD_STORM_PEAK_WIND.getXmlKey();
            ssdpd.setStormWrnPeakWindR(sstm.getThresholdValue(duKey, threshKeyR, areaID, xmlKey));
            ssdpd.setStormWrnPeakWindY(sstm.getThresholdValue(duKey, threshKeyY, areaID, xmlKey));
            
            /*
             * HFWW
             */
            xmlKey = SafeSeasDisplay.SS_DISP_PROD_HFWW_WIND_SPEED.getXmlKey();
            ssdpd.setHfwwWindSpeedR(sstm.getThresholdValue(duKey, threshKeyR, areaID, xmlKey));
            ssdpd.setHfwwWindSpeedY(sstm.getThresholdValue(duKey, threshKeyY, areaID, xmlKey));
            
            xmlKey = SafeSeasDisplay.SS_DISP_PROD_HFWW_GUST_SPEED.getXmlKey();
            ssdpd.setHfwwGustSpeedR(sstm.getThresholdValue(duKey, threshKeyR, areaID, xmlKey));
            ssdpd.setHfwwGustSpeedY(sstm.getThresholdValue(duKey, threshKeyY, areaID, xmlKey));
            
            xmlKey = SafeSeasDisplay.SS_DISP_PROD_HFWW_PEAK_WIND.getXmlKey();
            ssdpd.setHfwwPeakWindR(sstm.getThresholdValue(duKey, threshKeyR, areaID, xmlKey));
            ssdpd.setHfwwPeakWindY(sstm.getThresholdValue(duKey, threshKeyY, areaID, xmlKey));     
            
            /*
             * Add data to the array.
             */
            ssDataArray.add(ssdpd);
        }
    }
    
    private SSDisplayProductData getDataAtFirstSelection()
    {        
        int index = dataList.getSelectionIndex();
        
        return ssDataArray.get(index);        
    }
    
    private void updateDataArray(SSDisplayProductData ssdpd)
    {        
        int[] dataListIndexes = dataList.getSelectionIndices();        
        int currentIndex = 0;
        
        for (int i = 0; i < dataListIndexes.length; i++)
        {
            currentIndex = dataListIndexes[i];
            
            ssDataArray.get(currentIndex).updateData(ssdpd);
        }        
    }   
    
    @Override
    public void commitDataToXML()
    {
        SSThresholdMgr stm = SSThresholdMgr.getInstance();
        
        String xmlKey;
        String areaID;
        
        for (SSDisplayProductData ssdpd : ssDataArray)
        {
            areaID = ssdpd.getAreaID();
            
            /*
             * Small Craft Advisory
             */
            xmlKey = SafeSeasDisplay.SS_DISP_PROD_SCA_WIND_SPEED.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey, ssdpd.getScaWindSpeedR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey, ssdpd.getScaWindSpeedY());
            
            xmlKey = SafeSeasDisplay.SS_DISP_PROD_SCA_GUST_SPEED.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey, ssdpd.getScaGustSpeedR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey, ssdpd.getScaGustSpeedY());
            
            xmlKey = SafeSeasDisplay.SS_DISP_PROD_SCA_PEAK_WIND.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey, ssdpd.getScaPeakWindR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey, ssdpd.getScaPeakWindY());
            
            xmlKey = SafeSeasDisplay.SS_DISP_PROD_SCA_WAVE_HT.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey, ssdpd.getScaWaveHgtR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey, ssdpd.getScaWaveHgtY());
            
            /*
             * Gale Warning
             */
            xmlKey = SafeSeasDisplay.SS_DISP_PROD_GALE_WIND_SPEED.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey, ssdpd.getGaleWindSpeedR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey, ssdpd.getGaleWindSpeedY());
            
            xmlKey = SafeSeasDisplay.SS_DISP_PROD_GALE_GUST_SPEED.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey, ssdpd.getGaleGustSpeedR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey, ssdpd.getGaleGustSpeedY());
            
            xmlKey = SafeSeasDisplay.SS_DISP_PROD_GALE_PEAK_WIND.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey, ssdpd.getGalePeakWindR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey, ssdpd.getGalePeakWindY());
            
            /*
             * Storm Warning
             */
            xmlKey = SafeSeasDisplay.SS_DISP_PROD_STORM_WIND_SPEED.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey, ssdpd.getStormWrnWindSpeedR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey, ssdpd.getStormWrnWindSpeedY());
            
            xmlKey = SafeSeasDisplay.SS_DISP_PROD_STORM_GUST_SPEED.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey, ssdpd.getStormWrnGustSpeedR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey, ssdpd.getStormWrnGustSpeedY());
            
            xmlKey = SafeSeasDisplay.SS_DISP_PROD_STORM_PEAK_WIND.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey, ssdpd.getStormWrnPeakWindR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey, ssdpd.getStormWrnPeakWindY());
            
            /*
             * HFWW
             */
            xmlKey = SafeSeasDisplay.SS_DISP_PROD_HFWW_WIND_SPEED.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey, ssdpd.getHfwwWindSpeedR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey, ssdpd.getHfwwWindSpeedY());
            
            xmlKey = SafeSeasDisplay.SS_DISP_PROD_HFWW_GUST_SPEED.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey, ssdpd.getHfwwGustSpeedR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey, ssdpd.getHfwwGustSpeedY());
            
            xmlKey = SafeSeasDisplay.SS_DISP_PROD_HFWW_PEAK_WIND.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey, ssdpd.getHfwwPeakWindR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey, ssdpd.getHfwwPeakWindY());
        }         
    }   
    
    @Override
    public void reloadData()
    {
        dataList.removeAll();
        ssDataArray.clear();
        ssDataArray = null;
        
        populateList();
    }

    @Override
    protected void editDataAction()
    {
        SSDisplayProductData sspdp = getDataAtFirstSelection();
        
        if (productEditDlg == null)
        {
            productEditDlg = new SSDisplayProductEditDlg(getParent().getShell(), sspdp, this);
            productEditDlg.open();
            productEditDlg = null;
        }
    }

    @Override
    public void updateThresholdData(SSDisplayProductData sspdp)
    {
        updateDataArray(sspdp);
        populateList();
    }

    
}
