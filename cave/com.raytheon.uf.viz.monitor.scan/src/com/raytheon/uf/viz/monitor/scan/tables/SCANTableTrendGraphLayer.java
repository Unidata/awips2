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
package com.raytheon.uf.viz.monitor.scan.tables;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Set;

import org.eclipse.swt.widgets.Composite;

import com.raytheon.uf.viz.monitor.scan.commondialogs.IRequestTrendGraphData;
import com.raytheon.uf.viz.monitor.scan.commondialogs.ITrendGraphUpdate;
import com.raytheon.uf.viz.monitor.scan.commondialogs.ITrendSetsGraphUpdate;
import com.raytheon.uf.viz.monitor.scan.commondialogs.TrendGraphDlg;
import com.raytheon.uf.viz.monitor.scan.commondialogs.TrendSetsGraphDlg;

/**
 * This class is a layer between the SCANTable class and (currently) the
 * SCANCellTableComp and SCANDmdTableComp classes.  The purpose of this
 * layer is to allow managing the Trend Graph and Trend Sets dialogs that
 * can be displayed from the CELL and DMD tables.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 28, 2010            lvenable     Initial creation
 *
 * </pre>
 *
 * @author lvenable
 * @version 1.0	
 */

public abstract class SCANTableTrendGraphLayer extends SCANTable
                       implements ITrendGraphUpdate, ITrendSetsGraphUpdate
{
    private HashMap<TrendGraphDlg, String> trendMap;
    private HashMap<TrendSetsGraphDlg, String> trendSetsMap;
    
    private IRequestTrendGraphData requestDataCallback;
    
    private Integer vcp=0;
    
    public SCANTableTrendGraphLayer(Composite parent, SCANTableData tableData,
            ITableAction tableActionCB, IRequestTrendGraphData requestDataCallback, String site)
    {
        super(parent, tableData, tableActionCB, site);
        
        trendMap = new HashMap<TrendGraphDlg, String>();
        trendSetsMap = new HashMap<TrendSetsGraphDlg, String>();
        this.requestDataCallback = requestDataCallback;
    }
    
    protected void displayTrendGraphDialog(String ident, String attrName)
    {
        String identAttrKey = ident + attrName;
        
        if (trendMap.containsValue(identAttrKey) == true)
        {
            Set<TrendGraphDlg> keys = trendMap.keySet();
            
            for (TrendGraphDlg keyDlg : keys)
            {
                if (identAttrKey.compareTo(trendMap.get(keyDlg)) == 0)
                {
                    if (keyDlg.dialogIsDisposed() == false)
                    {
                        keyDlg.displayDialog();
                        return;
                    }
                    else
                    {
                        trendMap.remove(keyDlg);
                    }
                }
            }                       
        }
        
        if (tableData.getTableName().name().equalsIgnoreCase("CELL")) {
            vcp=tableData.getTableRows().get(0).getVcp();//0 should be fine
        }
        TrendGraphDlg tgd = new TrendGraphDlg(getParent().getShell(), scanTable, ident,
                attrName, this, requestDataCallback, getIdentList(),vcp);
        tgd.open();
        
        trendMap.put(tgd, identAttrKey);
    }
    
    protected void displayTrendSetsGraphDialog(String ident)
    {
        String trendSetName = tableActionCB.getTrendSetName();
        if (trendSetName == null)
        {
            return;
        }
        
        String identTrendSetKey = ident + trendSetName;
        
        if (trendSetsMap.containsValue(identTrendSetKey) == true)
        {
            Set<TrendSetsGraphDlg> keys = trendSetsMap.keySet();
            
            for (TrendSetsGraphDlg keyDlg : keys)
            {
                if (identTrendSetKey.compareTo(trendSetsMap.get(keyDlg)) == 0)
                {
                    if (keyDlg.dialogIsDisposed() == false)
                    {
                        keyDlg.displayDialog();
                        return;
                    }
                    else
                    {
                        trendSetsMap.remove(keyDlg);
                    }
                }
            }           
        }

        if (tableData.getTableName().name().equalsIgnoreCase("CELL")) {
            vcp=tableData.getTableRows().get(0).getVcp();//0 should be fine
        }
        TrendSetsGraphDlg tsgd = new TrendSetsGraphDlg(getParent().getShell(), scanTable, ident,
                trendSetName, this, requestDataCallback, getIdentList(),vcp);
        tsgd.open();
        trendSetsMap.put(tsgd, identTrendSetKey);
    }
    
    public void displayTrendSetGraphFromMap(String ident)
    {
        displayTrendSetsGraphDialog(ident);
    }
    
    public void updateAllTrendGraphs()
    {
        // Need to keep a list to avoid a ConcurrentModificationException
        List<TrendSetsGraphDlg> disposeList = new ArrayList<TrendSetsGraphDlg>();
        for (TrendSetsGraphDlg tsgDlg : trendSetsMap.keySet()) {
            boolean toDispose = tsgDlg.updateTrendSetsGraph();
            if (toDispose) {
                disposeList.add(tsgDlg);
            }
        }

        for (TrendSetsGraphDlg dlg : disposeList) {
            trendSetsMap.remove(dlg);
            dlg.close();
        }

        for (TrendSetsGraphDlg tsgDlg : trendSetsMap.keySet()) {
            tsgDlg.updateTrendSetsGraph();
        }

        List<TrendGraphDlg> graphDisposeList = new ArrayList<TrendGraphDlg>();
        for (TrendGraphDlg tgDlg : trendMap.keySet()) {
            boolean toDispose = tgDlg.updateTrendGraph();
            if (toDispose) {
                graphDisposeList.add(tgDlg);
            }
        }

        for (TrendGraphDlg dlg : graphDisposeList) {
            trendMap.remove(dlg);
            dlg.close();
        }

        for (TrendGraphDlg tgDlg : trendMap.keySet()) {
            tgDlg.updateTrendGraph();
        }
   }
    
    public void redrawAllTrendGraphs()
    {
        for (TrendSetsGraphDlg tsgDlg : trendSetsMap.keySet())
        {
            tsgDlg.redrawTrendGraph();
        }
        
        for (TrendGraphDlg tgDlg : trendMap.keySet())
        {
            tgDlg.redrawTrendGraph();
        }
    }
    
    @Override
    public void trendGraphChanged(String newIdentName, String newAttrName, TrendGraphDlg tgd)
    {
        trendMap.put(tgd, newIdentName + newAttrName);              
    }

    @Override
    public void trendGraphClosing(TrendGraphDlg tgd)
    {
            trendMap.remove(tgd);
    }

    @Override
    public void trendSetGraphChanged(String newIdentName, String newTrendSetName, TrendSetsGraphDlg tsgd)
    {
        trendSetsMap.put(tsgd, newIdentName + newTrendSetName);
    }

    @Override
    public void trendSetGraphClosing(TrendSetsGraphDlg tsgd)
    {  
        trendSetsMap.remove(tsgd);
    }

    public void setVcp(Integer vcp) {
        this.vcp = vcp;
    }

    public Integer getVcp() {
        return vcp;
    }
}
