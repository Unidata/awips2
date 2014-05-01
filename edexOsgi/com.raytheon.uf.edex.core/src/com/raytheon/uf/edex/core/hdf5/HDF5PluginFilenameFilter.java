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

package com.raytheon.uf.edex.core.hdf5;

import java.io.File;
import java.io.FilenameFilter;

/**
 * Filename filter implementation to filter a list of files in order to find a
 * plugin-specific HDF5 file in the HDF5 repository
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 10/8/2008    1532        bphillip    Initial checkin   
 * 2/6/2009     1990        bphillip    Modified filter for changed purge behavior
 * 
 * </pre>
 * 
 */
public class HDF5PluginFilenameFilter implements FilenameFilter {

    /** The plugin name to filter on */
    private String pluginName;

    /**
     * Creates a new plugin file name filter.
     * 
     * @param pluginName
     *            The plugin name to filter on
     */
    public HDF5PluginFilenameFilter(String pluginName) {
        this.pluginName = pluginName;
    }

    @Override
    public boolean accept(File dir, String name) {
        if (name.endsWith(".h5")) {
            return true;
        } else {
            return false;
        }
    }

}
