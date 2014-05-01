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
package com.raytheon.uf.common.dataplugin.persist;

import java.util.List;

/**
 * Interface to allow plugin to define their own custom strategy for storing
 * HDF5 data. It is highly advised to use DefaultPathProvider if this data is to
 * be purged using the purge rule system currently in place.
 * <p>
 * If plugins use an implementation of this class other than the
 * DefaultPathProvider, the purge mechanism in the data access object will more
 * than likely have to be overriden to account for the modified data locations.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20080606           1174 jkorman     Initial Implementation
 * 20081216                chammack    Added filename capability
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public interface IHDFFilePathProvider {

    /**
     * Constructs the directory path to where the HDF5 files are stored for this
     * plugin. This method should not return the path to a specific HDF5 file,
     * but only the directory containing the files.
     * <p>
     * It is not advised to override the behavior implemented in the
     * DefaultPathProvider. If this behavior is overridden, the purge process
     * will not be able to purge the data for this plugin unless a custom purge
     * routine is specified.
     * 
     * @param pluginName
     *            The name of the plugin to which this provider belongs
     * 
     * @param persistable
     *            The specific IPersistable object to be stored. This is
     *            provided as a possible aid to the construction of the path.
     *            The use of this object is not required.
     */
    String getHDFPath(String pluginName, IPersistable persistable);

    /**
     * Generates the name of the HDF5 file based on the provided plugin name and
     * object. This method should not return the full path to the file, but only
     * the file name itself.
     * 
     * @param pluginName
     *            The name of the plugin to which this provider belongs
     * @param persistable
     *            The specific IPersistable object to be stored or retrieved.
     * @return The name of the file to store the provided object or retrieve the
     *         HDF5 data associated with this object
     */
    String getHDFFileName(String pluginName, IPersistable persistable);

    /**
     * Gets the key names used for constructing the HDF5 file path for this
     * plugin. These keys are contained in the pathKeys.xml file in the
     * common_static localization directory. A plugin may not specify path keys.
     * In this case, an empty list is returned.
     * 
     * @param pluginName
     *            The plugin name to get the path keys for
     * @return The list of path keys used by this plugin. An empty list is
     *         returned if none are specified
     */
    List<String> getKeyNames(String pluginName);

}
