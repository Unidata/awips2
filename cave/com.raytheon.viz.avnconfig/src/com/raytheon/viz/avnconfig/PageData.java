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
package com.raytheon.viz.avnconfig;

import java.util.ArrayList;

/**
 * A class containing 'page' data where the page is the tab on a tab folder
 * and the data contained in each folder. 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 22 MAY 2008  1119       lvenable    Initial creation 
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 *
 */
public class PageData {
    /**
     * Data source (or page name).
     */
    private String dataSource;

    /**
     * Array of method data.
     */
    private ArrayList<MethodData> methodArray;

    /**
     * Constructor.
     */
    public PageData() {
        init();
    }

    /**
     * Constructor.
     * @param dataSource Data source (page name).
     */
    public PageData(String dataSource) {
        this.dataSource = dataSource;
        init();
    }

    /**
     * Constructor.
     * @param dataSource Data source (page name).
     * @param methodArray Array of MethodData.
     */
    public PageData(String dataSource, ArrayList<MethodData> methodArray) {
        this.dataSource = dataSource;
        this.methodArray = methodArray;
    }

    /**
     * Initialize method.
     */
    private void init() {
        methodArray = new ArrayList<MethodData>();
    }

    /**
     * Add MethodData to the MethodData array.
     * @param methodData MethodData object.
     */
    public void addMethodData(MethodData methodData) {
        methodArray.add(methodData);
    }

    /**
     * Get the data source name.
     * @return The data source name.
     */
    public String getDataSource() {
        return dataSource;
    }

    /**
     * Set the data source name.
     * @param dataSource The data source name.
     */
    public void setDataSource(String dataSource) {
        this.dataSource = dataSource;
    }

    /**
     * Get the array of MethodData.
     * @return The array of MethodData.
     */
    public ArrayList<MethodData> getMethodArray() {
        return methodArray;
    }

    /**
     * Set the array of MethodData.
     * @return The array of MethodData.
     */
    public void setMethodArray(ArrayList<MethodData> methodArray) {
        this.methodArray = methodArray;
    }
}