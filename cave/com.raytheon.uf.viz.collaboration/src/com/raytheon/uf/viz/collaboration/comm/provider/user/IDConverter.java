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
package com.raytheon.uf.viz.collaboration.comm.provider.user;

import com.raytheon.uf.viz.collaboration.comm.provider.Tools;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 28, 2012            jkorman     Initial creation
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */

public class IDConverter {

    /**
     * 
     * @param user
     * @return
     */
    public static UserId convertFrom(org.eclipse.ecf.core.identity.ID id) {
        String name = Tools.parseName(id.getName());
        String host = Tools.parseHost(id.getName());
        String rsc = Tools.parseResource(id.getName());
        return new UserId(name, host, rsc);
    }

    /**
     * 
     * @param user
     * @return
     */
    public static UserId convertFrom(org.eclipse.ecf.core.user.IUser user) {
        String name = Tools.parseName(user.getID().getName());
        String host = Tools.parseHost(user.getID().getName());
        UserId rosterId = new UserId(name, host);
        rosterId.setAlias(user.getNickname());
        return rosterId;
    }

    public static UserId convertFrom(String fqName) {
        return new UserId(Tools.parseName(fqName), Tools.parseHost(fqName),
                Tools.parseResource(fqName));
    }

}
