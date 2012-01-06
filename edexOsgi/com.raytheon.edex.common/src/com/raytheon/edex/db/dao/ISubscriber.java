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

import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.edex.database.DataAccessLayerException;

/**
 * Defines the Data Access Layer (DAL) interface for subscription management.
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 03May2007    208         MW Fegan    Initial creation.
 * 
 * </pre>
 *
 * @author mfegan
 * @version 1
 */

public interface ISubscriber {
    /* constants used for subscription manipulation */
    public static final int SUBSCRIBE_MODE_SAVE = 0;
    public static final int SUBSCRIBE_MODE_DELETE = 1;
    public static final int SUBSCRIBE_MODE_UPDATE = 2;

    /**
     * Retrieves the {@link com.raytheon.edex.subscription.Subscription Subscription 
     * object} matching the specified data URI.
     * 
     * @param dataURI the data URI to match
     * 
     * @return the Subscription Object
     * 
     * @throws DataAccessLayerException in the event an error occurred.
     */
    public Object getSubscription(String dataURI)
    throws DataAccessLayerException ;
    /**
     * Returns a list of available subscription keys.
     * 
     * @return the list of subscription keys.
     * 
     * @throws DataAccessLayerException in the event of an error
     */
    public Object[] getSubscriptions()
    throws DataAccessLayerException;
    /**
     * Provides a single entry point for managing subscription in the DAL.
     * 
     * @param record the subscription to manage
     * @param mode the management mode - must be one of SUBSCRIBE_MODE_SAVE,
     *             SUBSCRIBE_MODE_DELETE, and SUBSCRIBE_MODE_UPDATE.
     *
     * @throws DataAccessLayerException thrown in one of two cases.
     *         1) if the {@code record} is null, or 2) if the {@code mode}
     *         is invalid.
     */
    public void manageSubscriptions(PersistableDataObject record, int mode)
    throws DataAccessLayerException;
}
