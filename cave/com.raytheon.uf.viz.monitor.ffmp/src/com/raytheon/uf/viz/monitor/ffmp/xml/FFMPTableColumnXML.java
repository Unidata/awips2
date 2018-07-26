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
package com.raytheon.uf.viz.monitor.ffmp.xml;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

/**
 * Table column xml object.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Oct 24, 2013  2491     bsteffen    Remove ISerializableObject
 * 
 * </pre>
 * 
 */
@XmlAccessorType(XmlAccessType.NONE)
public class FFMPTableColumnXML
{
    @XmlElement(name = "ColumnName")
    private String columnName;
    
    @XmlElement(name = "ReverseFilter")
    private boolean reverseFilter;
    
    @XmlElement(name = "Filter")
    private double filter;
    
    @XmlElement(name = "Low")
    private double low;
    
    @XmlElement(name = "Mid")
    private double mid;
    
    @XmlElement(name = "Upper")
    private double upper;
    
    @XmlElement(name = "Sort")
    private String sort;
    
    @XmlElement(name = "ColorCell")
    private boolean colorCell;
    
    @XmlElement(name = "DisplayedInTable")
    private boolean displayedInTable;
    
    @XmlElement(name = "Units")
    private String units;
    
    @XmlElement(name = "BasinTrendPlotColor")
    private String basinTrendPlotColor;
    
    public FFMPTableColumnXML()
    {
        
    }

    public String getColumnName() {
        return columnName;
    }

    public void setColumnName(String columnName) {
        this.columnName = columnName;
    }

    public boolean getReverseFilter() {
        return reverseFilter;
    }

    public void setReverseFilter(boolean reverseFilter) {
        this.reverseFilter = reverseFilter;
    }

    public double getFilter() {
        return filter;
    }

    public void setFilter(double filter) {
        this.filter = filter;
    }

    public double getLow() {
        return low;
    }

    public void setLow(double low) {
        this.low = low;
    }

    public double getMid() {
        return mid;
    }

    public void setMid(double mid) {
        this.mid = mid;
    }

    public double getUpper() {
        return upper;
    }

    public void setUpper(double upper) {
        this.upper = upper;
    }

    public String getSort() {
        return sort;
    }

    public void setSort(String sort) {
        this.sort = sort;
    }

    public boolean getColorCell() {
        return colorCell;
    }

    public void setColorCell(boolean colorCell) {
        this.colorCell = colorCell;
    }

    public boolean getDisplayedInTable() {
        return displayedInTable;
    }

    public void setDisplayedInTable(boolean displayedInTable) {
        this.displayedInTable = displayedInTable;
    }

    public String getUnits() {
        return units;
    }

    public void setUnits(String units) {
        this.units = units;
    }

    public String getBasinTrendPlotColor() {
        return basinTrendPlotColor;
    }

    public void setBasinTrendPlotColor(String basinTrendPlotColor) {
        this.basinTrendPlotColor = basinTrendPlotColor;
    }
}
