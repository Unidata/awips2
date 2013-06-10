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

package com.raytheon.uf.common.datastorage;

import java.io.FileNotFoundException;
import java.util.Map;

import com.raytheon.uf.common.datastorage.StorageProperties.Compression;
import com.raytheon.uf.common.datastorage.records.IDataRecord;
import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * Defines the interface for operating against a hierarchical datastore
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 9, 2007             chammack    Initial Creation.
 * Apr 1, 2008             chammack    Added delete API
 * Aug 3, 2009             chammack    Modified to support Request
 * Sep 27, 2010      5091  njensen     Added deleteFiles(String)
 * Feb 12, 2013     #1608  randerso    Added explicit methods for deleting groups and datasets
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
public interface IDataStore extends ISerializableObject {

    public static enum HDF5_ITEM {
        DATASET, GROUP
    }

    public static enum StoreOp {
        STORE_ONLY, REPLACE, APPEND, OVERWRITE
    };

    /**
     * Add a datarecord with optional properties.
     * 
     * NOTE: Record is not written to disk until store method is called.
     * 
     * @param dataset
     *            the data to add to the write
     * @param properties
     *            the storage characteristics of the data (optional)
     * @throws StorageException
     */
    public abstract void addDataRecord(IDataRecord dataset,
            StorageProperties properties) throws StorageException;

    /**
     * Add a datarecord
     * 
     * NOTE: Record is not written to disk until store method is called.
     * 
     * @param dataset
     */
    public abstract void addDataRecord(IDataRecord dataset)
            throws StorageException;

    /**
     * Store all data records
     * 
     * Stores all data added using the addDataRecord methods.
     * 
     * @throws StorageException
     */
    public abstract StorageStatus store() throws StorageException;

    /**
     * Delete one or more datasets. If all datasets have been deleted from a
     * file, the file will be deleted also.
     * 
     * @param datasets
     *            the full path to the dataset(s) to be deleted
     * @throws StorageException
     *             if deletion fails
     * @throws FileNotFoundException
     */
    public abstract void deleteDatasets(String... datasets)
            throws StorageException, FileNotFoundException;

    /**
     * Delete one or more groups and all subgroups/datasets they contain. If all
     * datasets have been deleted from a file, the file will be deleted also.
     * 
     * @param groups
     *            the full path to the group(s) to be deleted
     * @throws StorageException
     *             if deletion fails
     * @throws FileNotFoundException
     */
    public abstract void deleteGroups(String... groups)
            throws StorageException, FileNotFoundException;

    /**
     * Store all data records to a given data group, or replace it the group
     * already exists. Works similarly to store, except for the ability to
     * replace data.
     * 
     * @param storeOp
     *            store operation
     * @throws StorageException
     */
    public abstract StorageStatus store(StoreOp storeOp)
            throws StorageException;

    /**
     * Convenience method for retrieve.
     * 
     * Retrieves all data (except interpolated tilesets) at a given group.
     * 
     * @param group
     *            the group of data to retrieve
     * @return the data records
     * @throws StorageException
     * @throws FileNotFoundException
     */
    public abstract IDataRecord[] retrieve(String group)
            throws StorageException, FileNotFoundException;

    /**
     * Convenience method for retrieve
     * 
     * Retrieves all data at a given group, with the option to retrieve
     * interpolated tilesets.
     * 
     * @param group
     *            the group of data to retrieve
     * @param includeInterpolated
     *            a flag indicating whether interpolated tilesets should be
     *            retrieved
     * @return the data records
     * @throws StorageException
     * @throws FileNotFoundException
     */
    public abstract IDataRecord[] retrieve(String group,
            boolean includeInterpolated) throws StorageException,
            FileNotFoundException;

    /**
     * Retrieve a single dataset with optional subsetting
     * 
     * @param group
     *            the data group name
     * @param dataset
     *            the dataset name
     * @param request
     *            the request type to perform
     * @return the data record
     * @throws StorageException
     * @throws FileNotFoundException
     */
    public abstract IDataRecord retrieve(String group, String dataset,
            Request request) throws StorageException, FileNotFoundException;

    /**
     * Retrieve multiple datasets from a single file
     * 
     * 
     * @param datasetGroupPath
     *            the full path to a dataset.
     * @param request
     *            the request type to perform
     * @return a set of datarecords
     * @throws StorageException
     * @throws FileNotFoundException
     */
    public IDataRecord[] retrieveDatasets(String[] datasetGroupPath,
            Request request) throws StorageException, FileNotFoundException;

    /**
     * Retrieve multiple groups from a single file, retrieves all datasets from
     * each group.
     * 
     * NOTE: The request is applied to every group
     * 
     * @param groups
     *            the group names
     * @param request
     *            The request type to perform
     * @return the data records
     * @throws StorageException
     * @throws FileNotFoundException
     */
    public abstract IDataRecord[] retrieveGroups(String[] groups,
            Request request) throws StorageException, FileNotFoundException;

    /**
     * List all the datasets available inside a group
     * 
     * @param group
     *            the group
     * @return a list of datasets available
     * @throws StorageException
     * @throws FileNotFoundException
     * 
     */
    public String[] getDatasets(String group) throws StorageException,
            FileNotFoundException;

    public static class LinkLocation {
        /** Optional: Used for when the link is in another file */
        public String fileName;

        /** Required: The point to where the link should be made */
        public String linkTarget;
    }

    /**
     * Create links from a point in the current file to another point in the
     * same file, or a point in another file.
     * 
     * @param links
     *            the links to create
     */
    public void createLinks(Map<String, LinkLocation> links)
            throws StorageException, FileNotFoundException;

    /**
     * Deletes the provided list of dates from the provided dataStorePath. The
     * directory from which to delete the hdf5 files is created by appending the
     * dataStorePath to the base HDF5 directory path. All files named according
     * to the provided list of dates is deleted
     * 
     * @param dataStorePath
     *            The path extension to append to the base HDF5 directory path
     * @param datesToDelete
     *            The dates to delete
     * @throws StorageException
     *             If errors occur while deleting data from the HDF5 file
     * @throws FileNotFoundException
     *             If the HDF5 file does not exist
     */
    public void deleteFiles(String[] datesToDelete) throws StorageException,
            FileNotFoundException;

    /**
     * Creates an empty dataset with the specified dimensions, type, and fill
     * value
     * 
     * @param rec
     *            an empty record containing the attributes of the dataset
     * @return status of the creation
     */
    public void createDataset(IDataRecord rec) throws StorageException,
            FileNotFoundException;

    /**
     * Recursively repacks all files of a certain directory. Presumes that the
     * IDataStore instance is tied to a directory, not a specific file.
     * 
     * @param compression
     *            the type of compression to repack with
     */
    public void repack(Compression compression) throws StorageException;

    /**
     * Recursively copies all files of a certain directory. If compression is
     * specified the file will be repacked to the specified compression.
     * Presumes that the IDataStore instance is tied to a directory, not a
     * specific file.
     * 
     * @param outputDir
     *            the output directory to put the copied files
     * @param compression
     *            If specified will repack the output file with a given
     *            compression
     * @param timestampCheck
     *            if not null, the attribute to check on the file for a
     *            timestamp of the last time this particular action was run.
     *            e.g. "lastRepacked" or "lastArchived". if set, this attribute
     *            will be set on the file when the request is made, and then
     *            future requests for the same file will check this attribute
     *            and if the file has not been modified since last run, the file
     *            will be skipped.
     * @param minMillisSinceLastChange
     *            if greater than 0, the last modified time on the file cannot
     *            be within minMillisSinceLastChange from current time. This is
     *            used to not repack files that have changed within a recent
     *            threshold.
     * @param maxMillisSinceLastChange
     *            if greater than 0, the last modified time on the file must be
     *            within maxMillisSinceLastChange from current time. This is
     *            used to ignore files that have not changed within a recent
     *            threshold.
     */
    public void copy(String outputDir, Compression compression,
            String timestampCheck, int minMillisSinceLastChange,
            int maxMillisSinceLastChange) throws StorageException;
}