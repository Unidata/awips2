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
package com.raytheon.uf.edex.datadelivery.bandwidth;

import java.sql.SQLException;

import org.hibernate.cfg.AnnotationConfiguration;

import com.raytheon.uf.edex.datadelivery.bandwidth.dao.IBandwidthDbInit;

/**
 * Provides an in-memory bandwidth database initialization. Intentionally
 * package-private.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 24, 2012 1286       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
class InMemoryBandwidthDbInit implements IBandwidthDbInit {

    /**
     * {@inheritDoc}
     */
    @Override
    public void init() {
        // Nothing required
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void dropTables(AnnotationConfiguration aConfig) throws SQLException {
        // Nothing required

    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void createTables(AnnotationConfiguration aConfig)
            throws SQLException {
        // Nothing required
    }
}