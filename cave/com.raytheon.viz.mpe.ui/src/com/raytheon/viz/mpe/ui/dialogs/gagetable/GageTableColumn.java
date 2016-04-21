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
package com.raytheon.viz.mpe.ui.dialogs.gagetable;

/**
 * Represents a column in the Gage Table Dialog.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 16, 2009 2476       mpduff      Initial creation
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0	
 */

public class GageTableColumn {
    /**
     * Is this a data column?
     */
    private boolean dataColumn = true;
    
    /**
     * Is the column sorted ascending?
     */
    private boolean ascending = true;
    
    /**
     * Column name.
     */
    private String name;
    
    /**
     * Column prefix.
     */
    private String prefix = null;
    
    /**
     * Column width.
     */
    private int width = 65;
    
    /**
     * Column sort order.
     */
    private int sortOrder = -999;
    
    /**
     * Column tool tip text.
     */
    private String toolTipText = null;
    
    /**
     * Column product descriptor.
     */
    private GageTableProductDescriptor productDescriptor = null;
    
    public GageTableColumn(GageTableProductDescriptor productDescriptor) {
        this.productDescriptor = productDescriptor;
    }
    
    /**
     * @return the dataColumn
     */
    public boolean isDataColumn() {
        return dataColumn;
    }
    /**
     * @param dataColumn the dataColumn to set
     */
    public void setDataColumn(boolean dataColumn) {
        this.dataColumn = dataColumn;
    }
    /**
     * @return the ascending
     */
    public boolean isAscending() {
        return ascending;
    }
    /**
     * @param ascending the ascending to set
     */
    public void setAscending(boolean ascending) {
        this.ascending = ascending;
    }
    /**
     * @return the name
     */
    public String getName() {        
        return name;
    }
    /**
     * @return the prefix
     */
    public String getPrefix() {
        return prefix;
    }
    /**
     * @param prefix the prefix to set
     */
    public void setPrefix(String prefix) {
        this.prefix = prefix;
    }
    /**
     * @return the width
     */
    public int getWidth() {
        return width;
    }
    /**
     * @param width the width to set
     */
    public void setWidth(int width) {
        this.width = width;
    }
    /**
     * @return the sortOrder
     */
    public int getSortOrder() {
        return sortOrder;
    }
    /**
     * @param sortOrder the sortOrder to set
     */
    public void setSortOrder(int sortOrder) {
        this.sortOrder = sortOrder;
    }
    /**
     * @return the toolTipText
     */
    public String getToolTipText() {
        return toolTipText;
    }
    /**
     * @param toolTipText the toolTipText to set
     */
    public void setToolTipText(String toolTipText) {
        this.toolTipText = toolTipText;
    }
    /**
     * @return the productDescriptor
     */
    public GageTableProductDescriptor getProductDescriptor() {
        return productDescriptor;
    }
    /**
     * @param productDescriptor the productDescriptor to set
     */
    public void setProductDescriptor(GageTableProductDescriptor productDescriptor) {
        this.productDescriptor = productDescriptor;
        name = productDescriptor.getProductName();
    }
    
    /**
     * Set the name.
     * @param name
     *      The name.
     */
    public void setName(String name) {
        this.name = name;
    }
}
