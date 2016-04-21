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

public class BasinTrendCommon
{
    public BasinTrendCommon()
    {        
    }
    
    public enum PlotItems
    {
        RATE("rate"), QPE("qpe"), QPF("qpf"), QPFSCAN("QPFSCAN"), GUID("guid"),
        RFCFFG("RFCFFG"), VGB("vgb");
        
        private String itemName;
        
        PlotItems(String name)
        {
            itemName = name;
        }
        
        public String getItemName()
        {
            return itemName;
        }
    }
    
    public enum Underlays
    {
        RATE("rate"), QPE("qpe"), RATIO("ratio"), DIFF("diff");
        
        private String itemName;
        
        Underlays(String name)
        {
            itemName = name;
        }
        
        public String getUnderlayName()
        {
            return itemName;
        }
    }
    
    public enum TimeDuration
    {
        ALL("All hr.", 24, 16), HR_1("1 hr.", 1, 5), HR_3("3 hr.", 3, 9), HR_6("6 hr.", 6, 12),
        HR_12("12 hr.", 12, 14), HR_24("24 hr.", 24, 16);
        
        private String timeDurName;
        private int hours;
        private int yCoordHours;
        
        TimeDuration(String name, int hrs, int yCoordHrs)
        {
            timeDurName = name;
            hours = hrs;
            yCoordHours = yCoordHrs;
        }
        
        public String getTimeDurName()
        {
            return timeDurName;
        }
        
        public int getHours()
        {
            return hours;
        }
        
        public int getYCoordHours()
        {
            return yCoordHours;
        }
    }
}
