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
import com.raytheon.uf.viz.monitor.snow.threshold.SnowDisplayProductData;
import com.raytheon.uf.viz.monitor.snow.threshold.SnowThresholdMgr;
import com.raytheon.uf.viz.monitor.ui.dialogs.TabItemComp;
import com.raytheon.uf.viz.monitor.util.MonitorConfigConstants.SnowDisplay;
import com.raytheon.uf.viz.monitor.xml.AreaXML;
import com.raytheon.uf.viz.monitor.xml.ThresholdsXML;

public class SnowDisplayProductTab extends TabItemComp implements IUpdateDisplayProduct
{
    private SnowDisplayProductEditDlg productEditDlg;
    
    private ArrayList<String> areaIDArray;
    
    private ArrayList<SnowDisplayProductData> snowDataArray;
    
    public SnowDisplayProductTab(TabFolder parent, DataUsageKey duKey)
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
        GridData gd = new GridData(68, SWT.DEFAULT);
        Label fillerLbl = new Label(lblComp, SWT.CENTER);
        fillerLbl.setLayoutData(gd);
        
        /*
         * Blizzard Warning
         */
        Composite blizzardComp = createGroupComposite(lblComp, 4, "Blizzard Warning");
        createLabelComp(blizzardComp, "Vis(mi)", "", true);
        createLabelComp(blizzardComp, "Wind", "Speed(kt)", false);
        createLabelComp(blizzardComp, "Gust", "Speed(kt)", false);
        createLabelComp(blizzardComp, "Peak", "Wind(kt)", false);        
        
        /*
         * Freezing Precip
         */
        Composite freezePrecipComp = createGroupComposite(lblComp, 2, "Freezing Precip");
        createLabelComp(freezePrecipComp, "Temp(F)", "", false);
        createLabelComp(freezePrecipComp, "Hourly", "Prcp(in)", false);
        
        /*
         * Heavy Snow Warning
         */
        Composite heavySnowWarnComp = createGroupComposite(lblComp, 3, "Heavy Snow Warning");
        createLabelComp(heavySnowWarnComp, "SNINCR", "Hr(in)", false);
        createLabelComp(heavySnowWarnComp, "SNINCR", "Tot(in)", false);
        createLabelComp(heavySnowWarnComp, "Snow", "Depth(in)", false);
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
        SnowDisplayProductData sdpd = null;
        
        for (int i = 0; i < snowDataArray.size(); i++)
        {
            sb = new StringBuilder();
            
            sdpd = snowDataArray.get(i);
            
            currentAreaID = sdpd.getAreaID();
            areaIDArray.add(currentAreaID);
            
            sb.append(String.format(areaIdFmt, currentAreaID));
            
            /*
             * Blizzard Warning
             */            
            visVal = sdpd.getBlizWrnVisR();            
            tmpVisStr = rangeUtil.getVisString((int)visVal);
            sb.append(String.format(dataFmt, tmpVisStr));
            
            visVal = sdpd.getBlizWrnVisY();           
            tmpVisStr = rangeUtil.getVisString((int)visVal);
            sb.append(String.format(dataFmt, tmpVisStr));
            
            appendIntData(sb, sdpd.getBlizWrnWindSpdR(), sdpd.getBlizWrnWindSpdY());
            appendIntData(sb, sdpd.getBlizWrnGustSpdR(), sdpd.getBlizWrnGustSpdY());
            appendIntData(sb, sdpd.getBlizWrnPeakWindR(), sdpd.getBlizWrnPeakWindY());
            
            /*
             * Freezing Precip
             */
            appendIntData(sb, sdpd.getFrzPrecipTempR(), sdpd.getFrzPrecipTempY());
            appendDecimalData(sb, sdpd.getFrzPrecipHrlyPrcpR(), sdpd.getFrzPrecipHrlyPrcpY());
            
            /*
             * Heavy Snow Warning
             */ 
            appendIntData(sb, sdpd.getHvySnowSnincrHrR(), sdpd.getHvySnowSnincrHrY());
            appendIntData(sb, sdpd.getHvySnowSnincrTotR(), sdpd.getHvySnowSnincrTotY());
            appendIntData(sb, sdpd.getHvySnowDepthR(), sdpd.getHvySnowDepthY());
            
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
        snowDataArray = new ArrayList<SnowDisplayProductData>();
        
        SnowThresholdMgr stm = SnowThresholdMgr.getInstance();   
        
        String xmlKey;
        String areaID;       
        
        ThresholdsXML threshXML = stm.getThresholdsXmlData(duKey);
        
        ArrayList<AreaXML> areasArray = threshXML.getAreas();
        
        for (AreaXML area : areasArray)
        {
            areaID = area.getAreaId();
            SnowDisplayProductData sdpd = new SnowDisplayProductData();
            
            sdpd.setAreaID(areaID);

            /*
             * Blizzard Warning
             */
            xmlKey = SnowDisplay.SNOW_DISP_PROD_BLIZZ_VIS.getXmlKey();
            sdpd.setBlizWrnVisR(stm.getThresholdValue(duKey, threshKeyR, areaID, xmlKey));
            sdpd.setBlizWrnVisY(stm.getThresholdValue(duKey, threshKeyY, areaID, xmlKey));
            
            xmlKey = SnowDisplay.SNOW_DISP_PROD_BLIZZ_WIND_SPEED.getXmlKey();
            sdpd.setBlizWrnWindSpdR(stm.getThresholdValue(duKey, threshKeyR, areaID, xmlKey));
            sdpd.setBlizWrnWindSpdY(stm.getThresholdValue(duKey, threshKeyY, areaID, xmlKey));
            
            xmlKey = SnowDisplay.SNOW_DISP_PROD_BLIZZ_GUST_SPEED.getXmlKey();
            sdpd.setBlizWrnGustSpdR(stm.getThresholdValue(duKey, threshKeyR, areaID, xmlKey));
            sdpd.setBlizWrnGustSpdY(stm.getThresholdValue(duKey, threshKeyY, areaID, xmlKey));
            
            xmlKey = SnowDisplay.SNOW_DISP_PROD_BLIZZ_PEAK_WIND.getXmlKey();
            sdpd.setBlizWrnPeakWindR(stm.getThresholdValue(duKey, threshKeyR, areaID, xmlKey));
            sdpd.setBlizWrnPeakWindY(stm.getThresholdValue(duKey, threshKeyY, areaID, xmlKey));

            /*
             * Freezing Precip
             */
            xmlKey = SnowDisplay.SNOW_DISP_PROD_FRZ_TEMP.getXmlKey();
            sdpd.setFrzPrecipTempR(stm.getThresholdValue(duKey, threshKeyR, areaID, xmlKey));
            sdpd.setFrzPrecipTempY(stm.getThresholdValue(duKey, threshKeyY, areaID, xmlKey));
            
            xmlKey = SnowDisplay.SNOW_DISP_PROD_FRZ_HOURLY_PRECIP.getXmlKey();
            sdpd.setFrzPrecipHrlyPrcpR(stm.getThresholdValue(duKey, threshKeyR, areaID, xmlKey));
            sdpd.setFrzPrecipHrlyPrcpY(stm.getThresholdValue(duKey, threshKeyY, areaID, xmlKey));
            
            /*
             * Heavy Snow Warning
             */
            xmlKey = SnowDisplay.SNOW_DISP_PROD_HSW_SNINCR_HOURLY.getXmlKey();
            sdpd.setHvySnowSnincrHrR(stm.getThresholdValue(duKey, threshKeyR, areaID, xmlKey));
            sdpd.setHvySnowSnincrHrY(stm.getThresholdValue(duKey, threshKeyY, areaID, xmlKey));
            
            xmlKey = SnowDisplay.SNOW_DISP_PROD_HSW_SNINCR_TOTAL.getXmlKey();
            sdpd.setHvySnowSnincrTotR(stm.getThresholdValue(duKey, threshKeyR, areaID, xmlKey));
            sdpd.setHvySnowSnincrTotY(stm.getThresholdValue(duKey, threshKeyY, areaID, xmlKey));
            
            xmlKey = SnowDisplay.SNOW_DISP_PROD_HSW_SNOW_DEPTH.getXmlKey();
            sdpd.setHvySnowDepthR(stm.getThresholdValue(duKey, threshKeyR, areaID, xmlKey));
            sdpd.setHvySnowDepthY(stm.getThresholdValue(duKey, threshKeyY, areaID, xmlKey));
            
            /*
             * Add data to array.
             */
            snowDataArray.add(sdpd);
        }
    }
    
    private SnowDisplayProductData getDataAtFirstSelection()
    {        
        int index = dataList.getSelectionIndex();
        
        return snowDataArray.get(index);        
    }
    
    private void updateDataArray(SnowDisplayProductData sdpd)
    {        
        int[] dataListIndexes = dataList.getSelectionIndices();        
        int currentIndex = 0;
        
        for (int i = 0; i < dataListIndexes.length; i++)
        {
            currentIndex = dataListIndexes[i];
            
            snowDataArray.get(currentIndex).updateData(sdpd);
        }        
    }
    
    @Override
    public void commitDataToXML()
    {
        SnowThresholdMgr stm = SnowThresholdMgr.getInstance();
        
        String xmlKey;
        String areaID;
        
        for (SnowDisplayProductData sdpd : snowDataArray)
        {
            areaID = sdpd.getAreaID();
            
            /*
             * Blizzard Warning
             */
            xmlKey = SnowDisplay.SNOW_DISP_PROD_BLIZZ_VIS.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey, sdpd.getBlizWrnVisR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey, sdpd.getBlizWrnVisY());
            
            xmlKey = SnowDisplay.SNOW_DISP_PROD_BLIZZ_WIND_SPEED.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey, sdpd.getBlizWrnWindSpdR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey, sdpd.getBlizWrnWindSpdY());
            
            xmlKey = SnowDisplay.SNOW_DISP_PROD_BLIZZ_GUST_SPEED.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey, sdpd.getBlizWrnGustSpdR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey, sdpd.getBlizWrnGustSpdY());
            
            xmlKey = SnowDisplay.SNOW_DISP_PROD_BLIZZ_PEAK_WIND.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey, sdpd.getBlizWrnPeakWindR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey, sdpd.getBlizWrnPeakWindY());

            /*
             * Freezing Precip
             */
            xmlKey = SnowDisplay.SNOW_DISP_PROD_FRZ_TEMP.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey, sdpd.getFrzPrecipTempR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey, sdpd.getFrzPrecipTempY());
            
            xmlKey = SnowDisplay.SNOW_DISP_PROD_FRZ_HOURLY_PRECIP.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey, sdpd.getFrzPrecipHrlyPrcpR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey, sdpd.getFrzPrecipHrlyPrcpY());
            
            /*
             * Heavy Snow Warning
             */
            xmlKey = SnowDisplay.SNOW_DISP_PROD_HSW_SNINCR_HOURLY.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey, sdpd.getHvySnowSnincrHrR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey, sdpd.getHvySnowSnincrHrY());
            
            xmlKey = SnowDisplay.SNOW_DISP_PROD_HSW_SNINCR_TOTAL.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey, sdpd.getHvySnowSnincrTotR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey, sdpd.getHvySnowSnincrTotY());
            
            xmlKey = SnowDisplay.SNOW_DISP_PROD_HSW_SNOW_DEPTH.getXmlKey();
            stm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey, sdpd.getHvySnowDepthR());
            stm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey, sdpd.getHvySnowDepthY());
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
        SnowDisplayProductData sdpd = getDataAtFirstSelection();
        
        if (productEditDlg == null)
        {
            productEditDlg = new SnowDisplayProductEditDlg(getParent().getShell(), sdpd, this);
            productEditDlg.open();
            productEditDlg = null;
        }
    }   

    @Override
    public void updateThresholdData(SnowDisplayProductData sdpd)
    {
        updateDataArray(sdpd);
        populateList();        
    }
}
