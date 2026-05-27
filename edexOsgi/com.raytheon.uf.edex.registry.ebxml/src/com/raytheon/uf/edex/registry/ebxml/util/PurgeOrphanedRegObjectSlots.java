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
package com.raytheon.uf.edex.registry.ebxml.util;

import com.raytheon.uf.edex.registry.ebxml.dao.RegistryObjectDao;

/**
 * Thread used to purge registry object orphan slots
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 13, 2019 7839       skabasele     Initial creation
 * 
 * </pre>
 * 
 * @author skabasele
 * @version 1.0
 */

public class PurgeOrphanedRegObjectSlots implements Runnable {

    private RegistryObjectDao registryObjectDao;

    public PurgeOrphanedRegObjectSlots(RegistryObjectDao registryObjectDao) {
        this.registryObjectDao = registryObjectDao;

    }

    @Override
    public void run() {
        registryObjectDao.purgeAllSlotWithoutRegObjParent();

    }

}
