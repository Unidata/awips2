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
import com.raytheon.uf.edex.database.DataAccessLayerException;

/**
 * An abstract base class for virtual grid databases (wrappers).
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 26, 2011            dgilling     Initial creation
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public abstract class VGridDatabase extends GridDatabase {

    protected IFPServerConfig config;

    protected VGridDatabase(IFPServerConfig config) {
        this.config = config;
    }

    public abstract SortedSet<Date> getValidTimes() throws GfeException,
            DataAccessLayerException;
}
