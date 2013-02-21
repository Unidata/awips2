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
package com.raytheon.uf.edex.datadelivery.bandwidth.dao;

import java.sql.SQLException;

import org.hibernate.cfg.AnnotationConfiguration;

/**
 * Performs any required database initialization.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 24, 2012 1286       djohnson     Separated from the implementation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public interface IBandwidthDbInit {

    /**
     * Initializes the database.
     */
    void init();

    /**
     * Drop tables.
     * 
     * @param aConfig
     *            the annotation configuration
     * 
     * @throws SQLException
     */
    void dropTables(AnnotationConfiguration aConfig)
            throws java.sql.SQLException;

    /**
     * Create the tables.
     * 
     * @param aConfig
     * 
     * @throws SQLException
     */
    void createTables(AnnotationConfiguration aConfig)
            throws java.sql.SQLException;;
}
