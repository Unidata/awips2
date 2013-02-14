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
package com.raytheon.uf.viz.datadelivery.common.ui;

import org.eclipse.swt.SWT;

import com.raytheon.uf.viz.datadelivery.utils.DataDeliveryUtils.TABLE_TYPE;

/**
 * Configuration for setting up the SWT table.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 14, 2012            lvenable     Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */

public class TableCompConfig {

    private int tableStyle = SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL | SWT.SINGLE;

    private boolean headerVisible = true;

    private boolean linesVisible = true;

    private int tableHeight = 150;

    private TABLE_TYPE tableType = null;

    public TableCompConfig(TABLE_TYPE tableType) {
        this.tableType = tableType;
    }

    public int getTableStyle() {
        return tableStyle;
    }

    public void setTableStyle(int tableStyle) {
        this.tableStyle = tableStyle;
    }

    public boolean isHeaderVisible() {
        return headerVisible;
    }

    public void setHeaderVisible(boolean headerVisible) {
        this.headerVisible = headerVisible;
    }

    public boolean isLinesVisible() {
        return linesVisible;
    }

    public void setLinesVisible(boolean linesVisible) {
        this.linesVisible = linesVisible;
    }

    public int getTableHeight() {
        return tableHeight;
    }

    public void setTableHeight(int tableHeight) {
        this.tableHeight = tableHeight;
    }

    public TABLE_TYPE getTableType() {
        return tableType;
    }

}
