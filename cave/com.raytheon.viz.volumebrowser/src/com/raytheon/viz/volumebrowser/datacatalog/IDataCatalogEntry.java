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
import com.raytheon.viz.volumebrowser.vbui.VolumeBrowserDialogSettings;

/**
 * DataCatalogEntries are meant to only be simple storage containers. Most of
 * the heavy lifting logic should be performed by the Catalog and not the Entry.
 * 
 * @author jelkins
 * 
 */
public interface IDataCatalogEntry {

    /**
     * 
     * @return the SelectedData from which this catalog entry was formed
     */
    SelectedData getSelectedData();

    /**
     * 
     * @return the dialog settings of the volume browser that more specifically
     *         defines how to load the catalog entry, unless
     *         freezeDialogSettings has been called the dialog settings will
     *         reflect changes in the UI.
     */
    VolumeBrowserDialogSettings getDialogSettings();

}
