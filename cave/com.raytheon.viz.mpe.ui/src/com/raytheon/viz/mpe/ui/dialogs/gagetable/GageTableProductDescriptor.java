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
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 1, 2009  2476       mpduff     Initial creation
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0	
 */

public class GageTableProductDescriptor implements Comparable<GageTableProductDescriptor> {
    private String productName = null;
    private String productFilenamePrefix = null;
    private String productDescription = null;
    private String productPathToken = null;
    private String[] dependencies = null;
    private boolean sort = false;
    private boolean sortAscending = true;
    
    public GageTableProductDescriptor(String productName,
            String productFilenamePrefix, String productDescription,
            String productPathToken, String[] dependencies) {
        this.productName = productName;
        this.productFilenamePrefix = productFilenamePrefix;
        this.productDescription = productDescription;
        this.productPathToken = productPathToken;
        this.dependencies = dependencies;
    }

    /**
     * @return the productName
     */
    public String getProductName() {
        return productName;
    }

    /**
     * @param productName the productName to set
     */
    public void setProductName(String productName) {
        this.productName = productName;
    }

    /**
     * @return the productFilenamePrefix
     */
    public String getProductFilenamePrefix() {
        return productFilenamePrefix;
    }

    /**
     * @param productFilenamePrefix the productFilenamePrefix to set
     */
    public void setProductFilenamePrefix(String productFilenamePrefix) {
        this.productFilenamePrefix = productFilenamePrefix;
    }

    /**
     * @return the productDescription
     */
    public String getProductDescription() {
        return productDescription;
    }

    /**
     * @param productDescription the productDescription to set
     */
    public void setProductDescription(String productDescription) {
        this.productDescription = productDescription;
    }

    /**
     * @return the productPathToken
     */
    public String getProductPathToken() {
        return productPathToken;
    }

    /**
     * @param productPathToken the productPathToken to set
     */
    public void setProductPathToken(String productPathToken) {
        this.productPathToken = productPathToken;
    }

    /**
     * @return the dependencies
     */
    public String[] getDependencies() {
        return dependencies;
    }

    /**
     * @param dependencies the dependencies to set
     */
    public void setDependencies(String[] dependencies) {
        this.dependencies = dependencies;
    }

    @Override
    public int compareTo(GageTableProductDescriptor o) {
        String thisProductName = getProductName();
        String thatProductName = o.getProductName();

        return thisProductName.compareTo(thatProductName);
    }

    /**
     * @return the sort
     */
    public boolean isSort() {
        return sort;
    }

    /**
     * @param sort the sort to set
     */
    public void setSort(boolean sort) {
        this.sort = sort;
    }

    /**
     * @return the sortAscending
     */
    public boolean isSortAscending() {
        return sortAscending;
    }

    /**
     * @param sortAscending the sortAscending to set
     */
    public void setSortAscending(boolean sortAscending) {
        this.sortAscending = sortAscending;
    }

    
}
