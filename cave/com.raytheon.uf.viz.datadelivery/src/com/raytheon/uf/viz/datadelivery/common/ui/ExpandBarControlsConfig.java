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

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 22, 2012            lvenable     Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */

public class ExpandBarControlsConfig {

    private boolean collapseAll = false;

    private boolean expandAll = false;

    private boolean expandSelected = false;

    private boolean disable = false;

    private boolean previewSelected = false;

    private boolean clearAll = false;

    /**
     * Constructor.
     */
    public ExpandBarControlsConfig() {
    }

    /**
     * Constructor.
     * 
     * @param collapseAll
     * @param expandAll
     * @param expandSelected
     * @param disable
     * @param previewSelected
     * @param clearAll
     */
    public ExpandBarControlsConfig(boolean collapseAll, boolean expandAll, boolean expandSelected, boolean disable,
            boolean previewSelected, boolean clearAll) {
        super();
        this.collapseAll = collapseAll;
        this.expandAll = expandAll;
        this.expandSelected = expandSelected;
        this.disable = disable;
        this.previewSelected = previewSelected;
        this.clearAll = clearAll;
    }

    public boolean getCollapseAll() {
        return collapseAll;
    }

    public void setCollapseAll(boolean collapseAll) {
        this.collapseAll = collapseAll;
    }

    public boolean getExpandAll() {
        return expandAll;
    }

    public void setExpandAll(boolean expandAll) {
        this.expandAll = expandAll;
    }

    public boolean getExpandSelected() {
        return expandSelected;
    }

    public void setExpandSelected(boolean expandSelected) {
        this.expandSelected = expandSelected;
    }

    public boolean getDisable() {
        return disable;
    }

    public void setDisable(boolean disable) {
        this.disable = disable;
    }

    public boolean getPreviewSelected() {
        return previewSelected;
    }

    public void setPreviewSelected(boolean previewSelected) {
        this.previewSelected = previewSelected;
    }

    public boolean getClearAll() {
        return clearAll;
    }

    public void setClearAll(boolean clearAll) {
        this.clearAll = clearAll;
    }

}
