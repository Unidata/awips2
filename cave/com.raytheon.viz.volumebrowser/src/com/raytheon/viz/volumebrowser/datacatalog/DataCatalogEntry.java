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
package com.raytheon.viz.volumebrowser.datacatalog;

import com.raytheon.viz.volumebrowser.vbui.SelectedData;
import com.raytheon.viz.volumebrowser.vbui.VolumeBrowserAction;
import com.raytheon.viz.volumebrowser.vbui.VolumeBrowserDialogSettings;

/**
 * Simple implementation of the data catalog entry interface that provides
 * common functionality among various data catalog entries.
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 6, 2009  2987       jelkins     Initial creation
 * 
 * </pre>
 * 
 * @author jelkins
 * @version 1.0
 */

public class DataCatalogEntry implements IDataCatalogEntry {

    protected SelectedData selectedData;

    private VolumeBrowserDialogSettings dialogSettings;

    public DataCatalogEntry(SelectedData selectedData) {
        this.selectedData = selectedData;
    }

    /**
     * Copy Constructor
     * 
     * @param other
     *            object object to copy
     */
    public DataCatalogEntry(DataCatalogEntry other) {
        this.selectedData = other.selectedData;
        this.dialogSettings = new VolumeBrowserDialogSettings(
                other.dialogSettings);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.volumebrowser.datacatalog.IDataCatalogEntry#getSelectedData
     * ()
     */
    @Override
    public SelectedData getSelectedData() {
        return selectedData;
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.viz.volumebrowser.datacatalog.IDataCatalogEntry#
     * getDialogSettings()
     */
    @Override
    public VolumeBrowserDialogSettings getDialogSettings() {
        if (dialogSettings == null) {
            dialogSettings = VolumeBrowserAction.getVolumeBrowserDlg()
                    .getDialogSettings();
        }
        return dialogSettings;
    }

}
