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
import com.raytheon.uf.viz.monitor.safeseas.threshold.SSDispMonSwellData;
import com.raytheon.uf.viz.monitor.safeseas.threshold.SSThresholdMgr;
import com.raytheon.uf.viz.monitor.ui.dialogs.TabItemComp;
import com.raytheon.uf.viz.monitor.util.MonitorConfigConstants;
import com.raytheon.uf.viz.monitor.util.MonitorConfigConstants.SafeSeasDisplay;
import com.raytheon.uf.viz.monitor.xml.AreaXML;
import com.raytheon.uf.viz.monitor.xml.ThresholdsXML;

public class SSDisplaySwellTab extends TabItemComp implements IUpdateDisplayMonitorSwell
{
    private SSDispMonSwellEditDlg displaySwellEditDlg;
    
    private ArrayList<String> areaIDArray;
    
    private ArrayList<SSDispMonSwellData> ssDataArray;
    
    public SSDisplaySwellTab(TabFolder parent, DataUsageKey duKey)
    {
        super(parent, duKey, true);
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
         * Primary Swell
         */
        Composite priSwellComp = createGroupComposite(lblComp, 4, "Primary Swell");
        createLabelComp(priSwellComp, "Height(ft)", "", false);
        createLabelComp(priSwellComp, "Periods(s)", "", false);
        createLabelComp(priSwellComp, "Dir(deg)", "(from)", false);
        createLabelComp(priSwellComp, "Dir(deg)", "(to)", false);        
        
        /*
         * Secondary Swell
         */
        Composite secSwellComp = createGroupComposite(lblComp, 4, "Secondary Swell");
        createLabelComp(secSwellComp, "Height(ft)", "", false);
        createLabelComp(secSwellComp, "Periods(s)", "", false);
        createLabelComp(secSwellComp, "Dir(deg)", "(from)", false);
        createLabelComp(secSwellComp, "Dir(deg)", "(to)", false);
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
        SSDispMonSwellData sssd = null;
        
        for (int i = 0; i < ssDataArray.size(); i++)
        {
            sb = new StringBuilder();
            
            sssd = ssDataArray.get(i);
            
            currentAreaID = sssd.getAreaID();
            areaIDArray.add(currentAreaID);
            
            sb.append(String.format(areaIdFmt, currentAreaID));           
            
            /*
             * Primary Swell
             */
            appendIntData(sb, sssd.getPriSwellHeightR(), sssd.getPriSwellHeightY());
            
            double higherThreshold = Math.max(sssd.getPriSwellPeriodR(), sssd.getPriSwellPeriodY());
            double lowerThreshold = Math.min(sssd.getPriSwellPeriodR(), sssd.getPriSwellPeriodY()); 
            if ( rankSwellPeriodHigh ) {
            	sssd.setRankSwellPeriodHigh(true);
            	sssd.setPriSwellPeriodR(higherThreshold);
            	sssd.setPriSwellPeriodY(lowerThreshold);
            } else {
            	sssd.setRankSwellPeriodHigh(false);
            	sssd.setPriSwellPeriodR(lowerThreshold);
            	sssd.setPriSwellPeriodY(higherThreshold);
            }
            appendIntData(sb, sssd.getPriSwellPeriodR(), sssd.getPriSwellPeriodY());
            
            appendIntData(sb, sssd.getPriSwellDirFromY(), sssd.getPriSwellDirFromR());
            appendIntData(sb, sssd.getPriSwellDirToR(), sssd.getPriSwellDirToY());
            
            /*
             * Secondary Swell
             */
            appendIntData(sb, sssd.getSecSwellHeightR(), sssd.getSecSwellHeightY());
            
            higherThreshold = Math.max(sssd.getSecSwellPeriodR(), sssd.getSecSwellPeriodY());
            lowerThreshold = Math.min(sssd.getSecSwellPeriodR(), sssd.getSecSwellPeriodY()); 
            if ( rankSwellPeriodHigh ) {
            	//sssd.setRankSwellPeriodHigh(true);
            	sssd.setSecSwellPeriodR(higherThreshold);
            	sssd.setSecSwellPeriodY(lowerThreshold);
            } else {
            	//sssd.setRankSwellPeriodHigh(false);
            	sssd.setSecSwellPeriodR(lowerThreshold);
            	sssd.setSecSwellPeriodY(higherThreshold);
            }
            appendIntData(sb, sssd.getSecSwellPeriodR(), sssd.getSecSwellPeriodY());
            appendIntData(sb, sssd.getSecSwellDirFromY(), sssd.getSecSwellDirFromR());
            appendIntData(sb, sssd.getSecSwellDirToR(), sssd.getSecSwellDirToY());
            
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
        ssDataArray = new ArrayList<SSDispMonSwellData>();
        
        SSThresholdMgr sstm = SSThresholdMgr.getInstance();   
        
        String xmlKey;
        String areaID;       
        
        ThresholdsXML threshXML = sstm.getThresholdsXmlData(duKey);
        
        ArrayList<AreaXML> areasArray = threshXML.getAreas();
        
        for (AreaXML area : areasArray)
        {
            areaID = area.getAreaId();
            SSDispMonSwellData sssd = new SSDispMonSwellData();
            
            sssd.setAreaID(areaID);

            /*
             * Primary Swell
             */
            xmlKey = SafeSeasDisplay.SS_DISP_SWELL_PRIM_HT.getXmlKey();
            sssd.setPriSwellHeightR(sstm.getThresholdValue(duKey, threshKeyR, areaID, xmlKey));
            sssd.setPriSwellHeightY(sstm.getThresholdValue(duKey, threshKeyY, areaID, xmlKey)); 
            
            xmlKey = SafeSeasDisplay.SS_DISP_SWELL_PRIM_PD.getXmlKey();
            sssd.setPriSwellPeriodR(sstm.getThresholdValue(duKey, threshKeyR, areaID, xmlKey));
            sssd.setPriSwellPeriodY(sstm.getThresholdValue(duKey, threshKeyY, areaID, xmlKey));
            
            xmlKey = SafeSeasDisplay.SS_DISP_SWELL_PRIM_DIR_FROM.getXmlKey();
            sssd.setPriSwellDirFromR(sstm.getThresholdValue(duKey, threshKeyR, areaID, xmlKey));
            sssd.setPriSwellDirFromY(sstm.getThresholdValue(duKey, threshKeyY, areaID, xmlKey));
            
            xmlKey = SafeSeasDisplay.SS_DISP_SWELL_PRIM_DIR_TO.getXmlKey();
            sssd.setPriSwellDirToR(sstm.getThresholdValue(duKey, threshKeyR, areaID, xmlKey));
            sssd.setPriSwellDirToY(sstm.getThresholdValue(duKey, threshKeyY, areaID, xmlKey));
            
            /*
             * Secondary Swell
             */
            xmlKey = SafeSeasDisplay.SS_DISP_SWELL_SEC_HT.getXmlKey();
            sssd.setSecSwellHeightR(sstm.getThresholdValue(duKey, threshKeyR, areaID, xmlKey));
            sssd.setSecSwellHeightY(sstm.getThresholdValue(duKey, threshKeyY, areaID, xmlKey)); 
            
            xmlKey = SafeSeasDisplay.SS_DISP_SWELL_SEC_PD.getXmlKey();
            sssd.setSecSwellPeriodR(sstm.getThresholdValue(duKey, threshKeyR, areaID, xmlKey));
            sssd.setSecSwellPeriodY(sstm.getThresholdValue(duKey, threshKeyY, areaID, xmlKey));
            
            xmlKey = SafeSeasDisplay.SS_DISP_SWELL_SEC_DIR_FROM.getXmlKey();
            sssd.setSecSwellDirFromR(sstm.getThresholdValue(duKey, threshKeyR, areaID, xmlKey));
            sssd.setSecSwellDirFromY(sstm.getThresholdValue(duKey, threshKeyY, areaID, xmlKey));
            
            xmlKey = SafeSeasDisplay.SS_DISP_SWELL_SEC_DIR_TO.getXmlKey();
            sssd.setSecSwellDirToR(sstm.getThresholdValue(duKey, threshKeyR, areaID, xmlKey));
            sssd.setSecSwellDirToY(sstm.getThresholdValue(duKey, threshKeyY, areaID, xmlKey));
            
            /*
             * Add data to array.
             */
            ssDataArray.add(sssd);
        }
    }
    
    private SSDispMonSwellData getDataAtFirstSelection()
    {        
        int index = dataList.getSelectionIndex();
        
        return ssDataArray.get(index);        
    }    
    
    private void updateDataArray(SSDispMonSwellData sssd)
    {        
        int[] dataListIndexes = dataList.getSelectionIndices();        
        int currentIndex = 0;
        
        for (int i = 0; i < dataListIndexes.length; i++)
        {
            currentIndex = dataListIndexes[i];
            
            ssDataArray.get(currentIndex).updateData(sssd);
        }        
    }
    
    @Override
    public void commitDataToXML()
    {
        SSThresholdMgr sstm = SSThresholdMgr.getInstance();
        
        String xmlKey;
        String areaID;
        
        for (SSDispMonSwellData sssd : ssDataArray)
        {
        	
        	MonitorConfigConstants.setRankSwellPeriodHigh(rankSwellPeriodHigh);
        	
            areaID = sssd.getAreaID();
            
            /*
             * Primary Swell
             */
            xmlKey = SafeSeasDisplay.SS_DISP_SWELL_PRIM_HT.getXmlKey();
            sstm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey, sssd.getPriSwellHeightR());
            sstm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey, sssd.getPriSwellHeightY());
            
            xmlKey = SafeSeasDisplay.SS_DISP_SWELL_PRIM_PD.getXmlKey();
            sstm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey, sssd.getPriSwellPeriodR());
            sstm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey, sssd.getPriSwellPeriodY());
            
            xmlKey = SafeSeasDisplay.SS_DISP_SWELL_PRIM_DIR_FROM.getXmlKey();
            sstm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey, sssd.getPriSwellDirFromR());
            sstm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey, sssd.getPriSwellDirFromY());
            
            xmlKey = SafeSeasDisplay.SS_DISP_SWELL_PRIM_DIR_TO.getXmlKey();
            sstm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey, sssd.getPriSwellDirToR());
            sstm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey, sssd.getPriSwellDirToY());
            
            /*
             * Secondary Swell
             */
            xmlKey = SafeSeasDisplay.SS_DISP_SWELL_SEC_HT.getXmlKey();
            sstm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey, sssd.getSecSwellHeightR());
            sstm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey, sssd.getSecSwellHeightY());
            
            xmlKey = SafeSeasDisplay.SS_DISP_SWELL_SEC_PD.getXmlKey();
            sstm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey, sssd.getSecSwellPeriodR());
            sstm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey, sssd.getSecSwellPeriodY());
            
            xmlKey = SafeSeasDisplay.SS_DISP_SWELL_SEC_DIR_FROM.getXmlKey();
            sstm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey, sssd.getSecSwellDirFromR());
            sstm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey, sssd.getSecSwellDirFromY());
            
            xmlKey = SafeSeasDisplay.SS_DISP_SWELL_SEC_DIR_TO.getXmlKey();
            sstm.setThresholdValue(duKey, threshKeyR, areaID, xmlKey, sssd.getSecSwellDirToR());
            sstm.setThresholdValue(duKey, threshKeyY, areaID, xmlKey, sssd.getSecSwellDirToY());
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
        SSDispMonSwellData sssd = getDataAtFirstSelection();
        
        if (displaySwellEditDlg == null)
        {
            displaySwellEditDlg = new SSDispMonSwellEditDlg(getParent().getShell(), sssd, this, true);
            displaySwellEditDlg.open();
            displaySwellEditDlg = null;
        }

    }

    @Override
    public void updateThresholdData(SSDispMonSwellData sssd)
    {
        updateDataArray(sssd);
        populateList();
        
    }
}
