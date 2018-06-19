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
package com.raytheon.uf.edex.plugin.mpe.dao.impl;

import com.raytheon.uf.common.dataplugin.shef.tables.Rwbiasstat;
import com.raytheon.uf.edex.plugin.mpe.dao.AbstractIHFSDbDao;

/**
 * IHFS Database Dao for interacting with the {@link Rwbiasstat} entity.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 13, 2016 5576       bkowal      Initial creation
 * May 19, 2016 5576       bkowal      Made constructor public
 * 
 * </pre>
 * 
 * @author bkowal
 */

public class RwbiasstatDao extends AbstractIHFSDbDao<Rwbiasstat, String> {

    public RwbiasstatDao() {
        super(Rwbiasstat.class);
    }

    /**
     * Retrieves the {@link Rwbiasstat} record associated with the specified
     * office id
     * 
     * @param officeId
     *            the specified office id
     * @return the {@link Rwbiasstat} record that has been retrieved or
     *         {@code null} if a record was not found.
     */
    public Rwbiasstat getForOffice(final String officeId) {
        return this.retrieveById(officeId);
    }
}