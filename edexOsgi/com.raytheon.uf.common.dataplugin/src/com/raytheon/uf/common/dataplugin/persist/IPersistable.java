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

import java.util.Date;

/**
 * Plugin data records which are stored in the data store must implement this
 * interface.
 * <p>
 * This interface allows the developer to configure how a record is to be stored
 * in the data store.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 				353			bphillip	Initial creation	
 * 20070914            379  jkorman     Added populateDataStore() and
 *                                      getPersistenceTime() methods.
 * 20071113     377         randerso    Modified to use interface IDataStore
 *                                      instead of concrete class HDF5DataStore
 *                                      Removed createHDF5Record method in favor 
 *                                      of the more general populateDataStore method.
 * 02/06/09     1990        bphillip    Moved populateDataStore method to plugin specific dao implementations
 * 04/08/13     1293        bkowal      Removed references to hdffileid.
 * </pre>
 * 
 */
public interface IPersistable {

    /**
     * Get the time to be used for the persistence time for this object.
     * 
     * @return The persistence time to be used.
     */
    public Date getPersistenceTime();

    /**
     * Set the time to be used for the persistence time for this object.
     * 
     * @param persistTime
     *            The persistence time to be used.
     */
    public void setPersistenceTime(Date persistTime);

    /**
     * Gets the HDF5 file path provider used by this object
     * 
     * @return The path provider
     */
    public IHDFFilePathProvider getHDFPathProvider();
}
