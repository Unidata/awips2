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
package com.raytheon.uf.viz.monitor.snow.test;

import java.util.Date;

import com.raytheon.uf.common.monitor.data.CommonConfig;
import com.raytheon.uf.viz.monitor.config.CommonTableConfig;
import com.raytheon.uf.viz.monitor.config.CommonTableConfig.CellType;
import com.raytheon.uf.viz.monitor.data.TableCellData;
import com.raytheon.uf.viz.monitor.data.TableData;
import com.raytheon.uf.viz.monitor.data.TableRowData;

public class DataGenerator
{
    private CommonTableConfig ctc;
    
    public DataGenerator()
    {
        ctc = CommonTableConfig.getInstance();
    }
    
    public TableData generateData(CommonConfig.AppName appName)
    {
        TableData tData = new TableData(appName);

        System.out.println("Creating data for: " + appName.name());
        
        if (appName == CommonConfig.AppName.SAFESEAS)
        {
            for (int i = 0; i < 2; i++)
            {
                safeseasRow1(i,tData);
                safeseasRow1(i,tData);
            }            
        }
        else if (appName == CommonConfig.AppName.SNOW)
        {
            for (int i = 0; i < 2; i++)
            {
                snowRow1(i,tData);
                snowRow2(i,tData);
            }
        }
        else if (appName == CommonConfig.AppName.FOG)
        {
            for (int i = 0; i < 2000; i++)
            {
                fogRow1(i,tData);
                fogRow2(i,tData);
                fogRow3(i,tData);
                fogRow4(i,tData);
            }  
        }
        
        return tData;
    }
    
    private void fogRow1(int num, TableData tData)
    {
        TableRowData trd = new TableRowData(ctc.getTableColumnKeys(CommonConfig.AppName.FOG).length); 
        
        trd.setTableCellData(0, new TableCellData("NJC00" + num, "Hover Text", CellType.AreaId, false));            
        trd.setTableCellData(1, new TableCellData("3", CellType.G, true));                
        trd.setTableCellData(2, new TableCellData(0, CellType.NotMonitored, true));                
        trd.setTableCellData(3, new TableCellData("4", CellType.R, true));             
        trd.setTableCellData(4, new TableCellData(0, CellType.NotMonitored, true));               
        trd.setTableCellData(5, new TableCellData(4, CellType.G, true));               
        trd.setTableCellData(6, new TableCellData(0, CellType.NotMonitored, true));            
        trd.setTableCellData(7, new TableCellData(0, CellType.NotMonitored, true));              
        trd.setTableCellData(8, new TableCellData(70, CellType.G, true));             
        trd.setTableCellData(9, new TableCellData(66, CellType.G, true));               
        trd.setTableCellData(10, new TableCellData(4, CellType.R, true));              
        trd.setTableCellData(11, new TableCellData(88, CellType.R, true));             
        trd.setTableCellData(12, new TableCellData("", CellType.R, true));
        
        tData.addReplaceDataRow(trd);
    }
    
    private void fogRow2(int num, TableData tData)
    {
        TableRowData trd = new TableRowData(ctc.getTableColumnKeys(CommonConfig.AppName.FOG).length);
               
        trd.setTableCellData(0, new TableCellData("ANZ45" + num, "Hover Text", CellType.AreaId, false));               
        trd.setTableCellData(1, new TableCellData("1 1/2", CellType.R, true));                
        trd.setTableCellData(2, new TableCellData(0, CellType.NotMonitored, true));          
        trd.setTableCellData(3, new TableCellData("50", CellType.Y, true));                
        trd.setTableCellData(4, new TableCellData(0, CellType.NotMonitored, true));              
        trd.setTableCellData(5, new TableCellData(80, CellType.R, true));              
        trd.setTableCellData(6, new TableCellData(0, CellType.NotMonitored, true));             
        trd.setTableCellData(7, new TableCellData(0, CellType.NotMonitored, true));              
        trd.setTableCellData(8, new TableCellData(84, CellType.Y, true));              
        trd.setTableCellData(9, new TableCellData(63, CellType.G, true));              
        trd.setTableCellData(10, new TableCellData(6, CellType.Y, true));              
        trd.setTableCellData(11, new TableCellData(48, CellType.G, true));      
        trd.setTableCellData(12, new TableCellData("", CellType.NotDetermined, true));
        
        tData.addReplaceDataRow(trd);
    }
    
    private void fogRow3(int num, TableData tData)
    {
        TableRowData trd = new TableRowData(ctc.getTableColumnKeys(CommonConfig.AppName.FOG).length);
               
        trd.setTableCellData(0, new TableCellData("VAC84" + num, "Hover Text", CellType.AreaId, false));              
        trd.setTableCellData(1, new TableCellData("2 1/2", CellType.Y, true));               
        trd.setTableCellData(2, new TableCellData(0, CellType.NotMonitored, true));                
        trd.setTableCellData(3, new TableCellData("CLR", CellType.G, true));                
        trd.setTableCellData(4, new TableCellData(0, CellType.NotMonitored, true));               
        trd.setTableCellData(5, new TableCellData(45, CellType.Y, true));               
        trd.setTableCellData(6, new TableCellData(0, CellType.NotMonitored, true));                
        trd.setTableCellData(7, new TableCellData(0, CellType.NotMonitored, true));             
        trd.setTableCellData(8, new TableCellData(95, CellType.R, true));              
        trd.setTableCellData(9, new TableCellData(75, CellType.Y, true));               
        trd.setTableCellData(10, new TableCellData(15, CellType.G, true));               
        trd.setTableCellData(11, new TableCellData(66, CellType.Y, true));              
        trd.setTableCellData(12, new TableCellData("", CellType.G, true));
        
        tData.addReplaceDataRow(trd);
    }
    
    private void fogRow4(int num, TableData tData)
    {
        TableRowData trd = new TableRowData(ctc.getTableColumnKeys(CommonConfig.AppName.FOG).length);
               
        trd.setTableCellData(0, new TableCellData("VEC84" + num, "Hover Text", CellType.AreaId, false));              
        trd.setTableCellData(1, new TableCellData("1 7/8", CellType.Y, true));                
        trd.setTableCellData(2, new TableCellData(0, CellType.NotMonitored, true));                
        trd.setTableCellData(3, new TableCellData("CLR", CellType.G, true));                
        trd.setTableCellData(4, new TableCellData(0, CellType.NotMonitored, true));              
        trd.setTableCellData(5, new TableCellData(46, CellType.Y, true));                
        trd.setTableCellData(6, new TableCellData(0, CellType.NotMonitored, true));                
        trd.setTableCellData(7, new TableCellData(0, CellType.NotMonitored, true));              
        trd.setTableCellData(8, new TableCellData(93, CellType.R, true));               
        trd.setTableCellData(9, new TableCellData(76, CellType.Y, true));              
        trd.setTableCellData(10, new TableCellData(14, CellType.G, true));              
        trd.setTableCellData(11, new TableCellData(60, CellType.Y, true));              
        trd.setTableCellData(12, new TableCellData("", CellType.G, true));
        
        tData.addReplaceDataRow(trd);
    }
    
    private void snowRow1(int num, TableData tData)
    {
        TableRowData trd = new TableRowData(ctc.getTableColumnKeys(CommonConfig.AppName.SNOW).length);
             
        trd.setTableCellData(0, new TableCellData("PAC00" + num, "Hover Text", CellType.AreaId, false));               
        trd.setTableCellData(1, new TableCellData("", CellType.G, true));            
        trd.setTableCellData(2, new TableCellData("", CellType.Y, true));               
        trd.setTableCellData(3, new TableCellData(0, CellType.NotAvailable, true));              
        trd.setTableCellData(4, new TableCellData(0, CellType.NotMonitored, true));              
        trd.setTableCellData(5, new TableCellData(0, CellType.NotMonitored, true));              
        trd.setTableCellData(6, new TableCellData(11, CellType.G, true));                
        trd.setTableCellData(7, new TableCellData(0, CellType.NotAvailable, true));             
        trd.setTableCellData(8, new TableCellData(16, CellType.G, true));              
        trd.setTableCellData(9, new TableCellData(36, CellType.Y, true));              
        trd.setTableCellData(10, new TableCellData(28, CellType.R, true));              
        trd.setTableCellData(11, new TableCellData("9", CellType.G, true));               
        trd.setTableCellData(12, new TableCellData(1015, CellType.G, true));        
        trd.setTableCellData(13, new TableCellData(27, CellType.G, true));             
        trd.setTableCellData(14, new TableCellData(0, CellType.NotAvailable, true));              
        trd.setTableCellData(15, new TableCellData(0.025, CellType.Y, true));              
        trd.setTableCellData(16, new TableCellData(0, CellType.NotAvailable, true));              
        trd.setTableCellData(17, new TableCellData(0, CellType.NotAvailable, true));               
        trd.setTableCellData(18, new TableCellData(0, CellType.NotAvailable, true));
        
        tData.addReplaceDataRow(trd);
    }
    
    private void snowRow2(int num, TableData tData)
    {
        TableRowData trd = new TableRowData(ctc.getTableColumnKeys(CommonConfig.AppName.SNOW).length);
             
        trd.setTableCellData(0, new TableCellData("ANZ00" + num, "Hover Text", CellType.AreaId, false));               
        trd.setTableCellData(1, new TableCellData("", CellType.Y, true));            
        trd.setTableCellData(2, new TableCellData("", CellType.R, true));               
        trd.setTableCellData(3, new TableCellData(0, CellType.NotAvailable, true));              
        trd.setTableCellData(4, new TableCellData(0, CellType.NotMonitored, true));              
        trd.setTableCellData(5, new TableCellData(0, CellType.NotMonitored, true));              
        trd.setTableCellData(6, new TableCellData(45, CellType.Y, true));                
        trd.setTableCellData(7, new TableCellData(24, CellType.G, true));             
        trd.setTableCellData(8, new TableCellData(25, CellType.Y, true));              
        trd.setTableCellData(9, new TableCellData(28, CellType.R, true));              
        trd.setTableCellData(10, new TableCellData(45, CellType.G, true));              
        trd.setTableCellData(11, new TableCellData("2 1/2", CellType.G, true));               
        trd.setTableCellData(12, new TableCellData(900, CellType.G, true));        
        trd.setTableCellData(13, new TableCellData(16, CellType.Y, true));             
        trd.setTableCellData(14, new TableCellData(25, CellType.R, true));              
        trd.setTableCellData(15, new TableCellData(1.02566, CellType.R, true));              
        trd.setTableCellData(16, new TableCellData(0, CellType.NotAvailable, true));              
        trd.setTableCellData(17, new TableCellData(0, CellType.NotAvailable, true));               
        trd.setTableCellData(18, new TableCellData(0, CellType.NotAvailable, true));
        
        tData.addReplaceDataRow(trd);
    }
    
    private void safeseasRow1(int num, TableData tData)
    {
        TableRowData trd = new TableRowData(ctc.getTableColumnKeys(CommonConfig.AppName.SAFESEAS).length);
             
        trd.setTableCellData(0, new TableCellData("ANZ00" + num, "Hover Text", CellType.AreaId, false));               
        trd.setTableCellData(1, new TableCellData("", CellType.R, true));            
        trd.setTableCellData(2, new TableCellData("", CellType.G, true));               
        trd.setTableCellData(3, new TableCellData("", CellType.G, true));              
        trd.setTableCellData(4, new TableCellData("", CellType.G, true));              
        trd.setTableCellData(5, new TableCellData(0, CellType.NotAvailable, true));              
        trd.setTableCellData(6, new TableCellData(15, CellType.Y, true));                
        trd.setTableCellData(7, new TableCellData(0, CellType.NotAvailable, true));             
        trd.setTableCellData(8, new TableCellData(19, CellType.R, true));              
        trd.setTableCellData(9, new TableCellData("8", CellType.G, true));              
        trd.setTableCellData(10, new TableCellData(61, CellType.G, true));              
        trd.setTableCellData(11, new TableCellData(43, CellType.G, true));        
        trd.setTableCellData(12, new TableCellData(0, CellType.NotAvailable, true));        
        trd.setTableCellData(13, new TableCellData(0, CellType.NotAvailable, true));             
        trd.setTableCellData(14, new TableCellData(0, CellType.NotAvailable, true));              
        trd.setTableCellData(15, new TableCellData(0, CellType.NotAvailable, true));              
        trd.setTableCellData(16, new TableCellData(0, CellType.NotAvailable, true));              
        trd.setTableCellData(17, new TableCellData(0, CellType.NotAvailable, true));         
        trd.setTableCellData(18, new TableCellData(0, CellType.NotMonitored, true));
        trd.setTableCellData(19, new TableCellData(0, CellType.NotAvailable, true));
        trd.setTableCellData(20, new TableCellData(0, CellType.NotAvailable, true));
        trd.setTableCellData(21, new TableCellData(0, CellType.NotMonitored, true));
        trd.setTableCellData(22, new TableCellData("", CellType.G, true));
        
        tData.addReplaceDataRow(trd);
    }
    
    //*******************************************************************************************
    //*******************************************************************************************
    //*******************************************************************************************
    //*******************************************************************************************
    //*******************************************************************************************
    
    public TableData generateObsHistData(CommonConfig.AppName appName, CommonTableConfig.ObsHistType obsType)
    {
        TableData tData = new TableData(appName);

        System.out.println("Creating data for: " + appName.name() + " and " + obsType.name());
        
        if (appName == CommonConfig.AppName.SAFESEAS)
        {
            // Create Safeseas Data        
        }
        else if (appName == CommonConfig.AppName.SNOW)
        {
            if (obsType == CommonTableConfig.ObsHistType.METAR)
            {
                obsSnowMetarRow1(0, tData);
            }
        }
        else if (appName == CommonConfig.AppName.FOG)
        {
            // Create Fog Data
        }
        
        return tData;
    }
    
    private void obsSnowMetarRow1(int num, TableData tData)
    {
//        TableRowData trd = new TableRowData(ctc.getZoneStnColumnKeys(CommonConfig.AppName.SNOW).length);
        
        int numCols = ctc.getObsHistColumnKeys(CommonConfig.AppName.SNOW,
                CommonTableConfig.ObsHistType.METAR).length;
        TableRowData trd = new TableRowData(numCols);
             
        trd.setTableCellData(0, new TableCellData(new Date(), CellType.ObsHist));               
        trd.setTableCellData(1, new TableCellData(300, CellType.ObsHist, false));            
        trd.setTableCellData(2, new TableCellData(7, CellType.ObsHist, false));               
        trd.setTableCellData(3, new TableCellData(20, CellType.ObsHist, false));              
        trd.setTableCellData(4, new TableCellData(1023, CellType.ObsHist, false));              
        trd.setTableCellData(5, new TableCellData(-.02, CellType.ObsHist, false));              
        
        tData.addReplaceDataRow(trd);
    }
}
