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
package com.raytheon.uf.viz.monitor.ffmp.ui.dialogs;

import java.util.ArrayList;
import java.util.Iterator;

public class ConfigSummaryData
{    
    private String layer = "";
    private boolean linkToFrame = false;
    private boolean worstCase = false;
    private boolean zoomMaintainLayer = false;
    private boolean zoomOnlyBasins = false;
    private ArrayList<String> includedCWAs = new ArrayList<String>();
    private String d2dClickAction = "";
    private String d2dDisplayType = "";
    private boolean autoRefresh = false;
    
    public ConfigSummaryData(String layer, boolean linkToFrame, boolean worstCase,
            boolean zoomMaintainLayer, boolean zoomOnlyBasins, ArrayList<String> includedCWAs,
            String clickAction, String displayType, boolean autoRefresh)
    {
        this.layer = layer;
        this.linkToFrame = linkToFrame;
        this.worstCase = worstCase;
        this.zoomMaintainLayer = zoomMaintainLayer;
        this.zoomOnlyBasins = zoomOnlyBasins;
        this.includedCWAs = includedCWAs;
        this.d2dClickAction = clickAction;
        this.d2dDisplayType = displayType;
        this.autoRefresh = autoRefresh;
    }

    public String getLayer()
    {
        return layer;
    }

    public String getLinkToFrame()
    {
        if (linkToFrame == true)
        {
            return "y";
        }
        return "n";
    }

    public String getWorstCase()
    {
        if (worstCase == true)
        {
            return "y";
        }
        return "n";
    }

    public String getZoomMaintainLayer()
    {
        if (zoomMaintainLayer == true)
        {
            return "y";
        }
        return "n";
    }

    public String getZoomOnlyBasins()
    {
        if (zoomOnlyBasins == true)
        {
            return "y";
        }
        return "n";
    }
    
    public String getIncludedCWAs()
    {
        StringBuilder sb = new StringBuilder();
        
        Iterator<String> iter = includedCWAs.iterator();
        
        while (iter.hasNext() == true)
        {
            sb.append(iter.next());
            
            if (iter.hasNext() == true)
            {
                sb.append(", ");
            }
        }            
        
        return sb.toString();
    }

    public String getD2dClickAction()
    {
        return d2dClickAction;
    }

    public String getD2dDisplayType()
    {
        return d2dDisplayType;
    }

    public String getAutoRefresh()
    {
        if (autoRefresh == true)
        {
            return "y";
        }
        return "n";
    }
    
    public String[] getDisplayData()
    {
        String[] dataArray = new String[9];
        
        dataArray[0] = getLayer();
        dataArray[1] = getLinkToFrame();
        dataArray[2] = getWorstCase();
        dataArray[3] = getZoomMaintainLayer();
        dataArray[4] = getZoomOnlyBasins();
        dataArray[5] = getIncludedCWAs();
        dataArray[6] = getD2dClickAction();
        dataArray[7] = getD2dDisplayType();
        dataArray[8] = getAutoRefresh();
        
        return dataArray;
    }
}
