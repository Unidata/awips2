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
package com.raytheon.uf.common.plugin.nwsauth;

import java.util.Collections;
import java.util.List;

import com.raytheon.uf.common.auth.user.IPermission;
import com.raytheon.uf.common.auth.user.IRole;

/**
 * NWS implementation of {@link IRole}.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 5, 2012  1302      djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class NwsRole implements IRole {

    private final List<IPermission> permissions;

    private final String description;

    private final String name;

    public NwsRole(String name, List<IPermission> permissions,
            String description) {
        this.name = name;
        this.permissions = (permissions == null) ? Collections
                .<IPermission> emptyList() : permissions;
        this.description = description;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<IPermission> getPermissions() {
        return permissions;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public String getDescription() {
        return description;
    }

    @Override
    public String toString() {
        return name;
    }

}
