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

import java.util.List;

import com.raytheon.uf.common.dataplugin.shef.tables.Admin;
import com.raytheon.uf.edex.plugin.mpe.dao.AbstractIHFSDbDao;

/**
 * IHFS Database Dao for interacting with the {@link Admin} entity.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 11, 2016 5576       bkowal      Initial creation
 * May 19, 2016 5576       bkowal      Made constructor public
 * 
 * </pre>
 * 
 * @author bkowal
 * @version 1.0
 */

public class AdminDao extends AbstractIHFSDbDao<Admin, String> {

    public AdminDao() {
        super(Admin.class);
    }

    /**
     * Retrieves the ihfs {@link Admin} record for the site. The expectation is
     * that only one record will actually be present.
     * 
     * @return the {@link Admin} record that was retrieved.
     * @throws IllegalStateException
     *             if the ihfs admin table does not contain one record.
     */
    public Admin getAdminRecord() {
        List<Admin> records = this.findByNamedQuery(Admin.SELECT_ADMIN_RECORD);
        if (records.size() != 1) {
            throw new IllegalStateException(
                    "Retrieved an unexpected number of admin records. Please verify the contents of your ihfs database.");
        }

        return records.iterator().next();
    }
}