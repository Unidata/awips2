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
package com.raytheon.uf.edex.database.dao;

import org.springframework.context.ApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.core.EDEXUtil;

/**
 * Utility class for test classes to start and stop an in-memory database.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 10, 2012 0726       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public final class DatabaseUtil {

    private static IUFStatusHandler statusHandler = UFStatus
            .getHandler(DatabaseUtil.class);

    private static final String UNIT_TEST_DB_BEANS_XML = "/unit-test-db-beans.xml";

    private static ApplicationContext originalApplicationContext;

    private static ClassPathXmlApplicationContext applicationContext;

    /**
     * No construction.
     */
    private DatabaseUtil() {
    }

    /**
     * Start the in-memory database for unit testing code that utilizes the
     * database. This should be called in the @Before section of a JUnit test,
     * and {@link #shutdown()} should be called in the @After section.
     */
    public static void start() {
        statusHandler.info("Starting the in-memory database.");

        originalApplicationContext = EDEXUtil.getSpringContext();
        applicationContext = new ClassPathXmlApplicationContext(
                UNIT_TEST_DB_BEANS_XML, DatabaseUtil.class);
        new EDEXUtil().setApplicationContext(applicationContext);

        statusHandler.info("Started.");
    }

    /**
     * Shutdown the database. Should be called in the @After section of a JUnit
     * test.
     */
    public static void shutdown() {
        statusHandler.info("Stopping the in-memory database.");

        if (applicationContext != null) {
            applicationContext.close();
            applicationContext = null;
        }
        new EDEXUtil().setApplicationContext(originalApplicationContext);
        originalApplicationContext = null;

        statusHandler.info("Stopped.");
    }
}