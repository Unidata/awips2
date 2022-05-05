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
package com.raytheon.edex.plugin.gfe.server.database;

import java.util.Date;
import java.util.SortedSet;

import com.raytheon.edex.plugin.gfe.config.IFPServerConfig;
import com.raytheon.uf.common.dataplugin.gfe.exception.GfeException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.database.DataAccessLayerException;

/**
 * An abstract base class for virtual grid databases (wrappers).
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Aug 26, 2011           dgilling  Initial creation
 * May 04, 2012  574      dgilling  Port getSiteID() method.
 * May 02, 2013  1969     randerso  Removed unnecessary updateDbs method
 * Sep 12, 2016  5861     randerso  Change IFPServerConfig.getSiteID() to return
 *                                  a single value instead of a list containing
 *                                  only one value.
 *
 * </pre>
 *
 * @author dgilling
 */

public abstract class VGridDatabase extends GridDatabase {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(VGridDatabase.class);

    protected IFPServerConfig config;

    protected VGridDatabase(IFPServerConfig config) {
        this.config = config;
    }

    protected static String getSiteID(final IFPServerConfig config) {
        return config.getSiteID();
    }

    public abstract SortedSet<Date> getValidTimes()
            throws GfeException, DataAccessLayerException;
}
