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

package com.raytheon.edex.db.dao;

import java.io.File;
import java.io.FileNotFoundException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.persist.DefaultPathProvider;
import com.raytheon.uf.common.dataplugin.persist.IHDFFilePathProvider;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.dataplugin.persist.PersistablePluginDataObject;
import com.raytheon.uf.common.datastorage.DataStoreFactory;
import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.Request;
import com.raytheon.uf.common.datastorage.StorageException;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.edex.core.props.EnvProperties;
import com.raytheon.uf.edex.core.props.PropertiesFactory;

/**
 * Data access object for saving and retrieving data from the HDF5 repository.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 7/24/07      353         bphillip    Initial Check in    
 * 20070914            379  jkorman     Changed to use IPersistable populateDataStore
 *                                      and getPersistenceTime methods.
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class HDF5Dao {

    public static final String HDF5DIR = "HDF5DIR";

    public static final IHDFFilePathProvider DEFAULT_PATH_PROVIDER = DefaultPathProvider
            .getInstance();

    /** The logger */
    protected Log logger = LogFactory.getLog(getClass());

    private String hdf5Dir = null;

    private IHDFFilePathProvider pathProvider = null;

    /**
     * Construct an instance of the HDFDao using system default values.
     */
    public HDF5Dao() {
        EnvProperties properties = PropertiesFactory.getInstance()
                .getEnvProperties();
        if (properties != null) {
            hdf5Dir = properties.getEnvValue(HDF5DIR);
        }
        pathProvider = DEFAULT_PATH_PROVIDER;
    }

    /**
     * Construct an instance of the HDFDao using user supplied properties and
     * path provider.
     * 
     * @param properties
     *            An environment properties instance that must contain an entry
     *            from the property HDF5DIR.
     */
    public HDF5Dao(EnvProperties properties) {
        if (properties != null) {
            hdf5Dir = properties.getEnvValue(HDF5DIR);
        }
        pathProvider = DEFAULT_PATH_PROVIDER;
    }

    /**
     * Construct an instance of the HDFDao using user supplied properties and
     * path provider.
     * 
     * @param properties
     *            An environment properties instance that must contain an entry
     *            from the property HDF5DIR.
     * @param pathProvider
     *            The path provider to use that creates a path to a specific HDF
     *            repository. This path provider must not provide the name of
     *            the repository, and the path must be relative to the base
     *            directory given in the properties HDF5DIR property.
     */
    public HDF5Dao(EnvProperties properties, IHDFFilePathProvider pathProvider) {
        this(properties);
        this.pathProvider = pathProvider;
    }

    /**
     * Retrieves data from the HDF5 repository given a corresponding record
     * populated with metadata
     * 
     * @param obj
     *            The record containing a data URI
     * @return The data corresponding to the provided record
     */
    public IDataRecord retrieveFromHDF5(PluginDataObject obj)
            throws FileNotFoundException {
        IDataRecord record = null;

        if (obj instanceof IPersistable) {
            IPersistable persistable = (IPersistable) obj;

            String persistDir = hdf5Dir
                    + pathProvider.getHDFPath(obj.getPluginName(), persistable)
                    + File.separator;
            String archive = pathProvider.getHDFFileName(obj.getPluginName(),
                    persistable);

            File persistFile = new File(persistDir, archive);
            /* connect to the data store and retrieve the data */
            IDataStore dataStore = DataStoreFactory.getDataStore(persistFile);
            try {
                record = dataStore.retrieve(obj.getDataURI(), "Data",
                        Request.ALL);
            } catch (StorageException e) {
                logger.error("Unable to retrieve file " + obj.getDataURI(), e);
            }
        }

        return record;

    }

    /**
     * Retrieves a data group from the HDF5 repository given a corresponding
     * record populated with metadata
     * 
     * @param obj
     *            The record containing a data URI
     * @return The data corresponding to the provided record
     */
    public IDataRecord[] retrieveGroupFromHDF5(PluginDataObject obj)
            throws FileNotFoundException {
        IDataRecord[] records = null;

        if (obj instanceof PersistablePluginDataObject) {
            IPersistable pRecord = (IPersistable) obj;

            String persistDir = hdf5Dir
                    + pathProvider.getHDFPath(obj.getPluginName(), pRecord)
                    + File.separator;
            String archive = pathProvider.getHDFFileName(obj.getPluginName(),
                    pRecord);

            File persistFile = new File(persistDir, archive);
            /* connect to the data store and retrieve the data */
            IDataStore dataStore = DataStoreFactory.getDataStore(persistFile);
            try {
                records = dataStore.retrieve(obj.getDataURI());
            } catch (StorageException e) {
                logger.error("Unable to retrieve file " + obj.getDataURI(), e);
            }
        }
        return records;

    }

    /**
     * @return the pathProvider
     */
    public IHDFFilePathProvider getPathProvider() {
        return pathProvider;
    }

    /**
     * @param pathProvider
     *            the pathProvider to set
     */
    public void setPathProvider(IHDFFilePathProvider pathProvider) {
        this.pathProvider = pathProvider;
    }

    /**
     * @return the hdf5Dir
     */
    public String getHdf5Dir() {
        return hdf5Dir;
    }

    /**
     * @param hdf5Dir
     *            the hdf5Dir to set
     */
    public void setHdf5Dir(String hdf5Dir) {
        this.hdf5Dir = hdf5Dir;
    }
}
