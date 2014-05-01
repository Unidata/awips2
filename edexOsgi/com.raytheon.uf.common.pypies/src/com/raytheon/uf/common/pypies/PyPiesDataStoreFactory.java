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
package com.raytheon.uf.common.pypies;

import java.io.File;

import com.raytheon.uf.common.datastorage.IDataStore;
import com.raytheon.uf.common.datastorage.IDataStoreFactory;

/**
 * Factory used for constructing PyPiesDataStore objects
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 8, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class PyPiesDataStoreFactory implements IDataStoreFactory {

    private PypiesProperties pypiesProps;

    public PyPiesDataStoreFactory(PypiesProperties pypiesProps) {
        this.pypiesProps = pypiesProps;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.datastorage.IDataStoreFactory#getDataStore(java
     * .io.File, boolean)
     */
    @Override
    public IDataStore getDataStore(File file, boolean useLocking) {
        return new PyPiesDataStore(file, useLocking, pypiesProps);
    }

}
