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
package com.raytheon.uf.common.registry.handler;

import org.mockito.Mockito;
import org.springframework.context.support.ClassPathXmlApplicationContext;

import com.raytheon.uf.common.util.SpringFiles;
import com.raytheon.uf.common.util.TestUtil;

/**
 * Since the handlers are registered via Spring, this provides a one-stop shop
 * for adding all registry object handlers. The list of handlers should grow as
 * required for tests.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 04, 2012  1241      djohnson     Initial creation
 * Jul 10, 2013  2106      djohnson     Spring file path moved to SpringFiles for reuse.
 * Jan 09, 2014  2615      bgonzale     Added spring files to initialize beans missing in tests.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

public class RegistryObjectHandlersUtil {

    private static final String MOCK_DATADELIVERY_HANDLERS_XML = "/datadelivery/mock-datadelivery-handlers.xml";

    /**
     * Initializes the handlers with the set of production implementations,
     * which interact with the registry proper.
     */
    public static void init() {
        initHandlersFromSpringFile(SpringFiles.DATADELIVERY_HANDLERS_IMPL_XML);
    }

    /**
     * Initializes the handlers with a set of {@link Mockito} provided handlers.
     */
    public static void initMocks() {
        initHandlersFromSpringFile(MOCK_DATADELIVERY_HANDLERS_XML);
    }

    /**
     * Initializes the handlers with a set of in-memory implementations.
     */
    public static void initMemory() {
        initHandlersFromSpringFile(SpringFiles.MEMORY_DATADELIVERY_HANDLERS_XML);
    }

    /**
     * @param resResource
     */
    private static void initHandlersFromSpringFile(String resResource) {
        RegistryObjectHandlers.clear();
        new ClassPathXmlApplicationContext(
                new String[] {
                        TestUtil.getResResourcePath(SpringFiles.DATADELIVERY_HANDLERS_IMPL_XML),
                        TestUtil.getResResourcePath(SpringFiles.DATADELIVERY_HANDLERS_XML),
                        TestUtil.getResResourcePath(SpringFiles.DATADELIVERY_STANDALONE_XML),
                        TestUtil.getResResourcePath(resResource) },
                RegistryObjectHandlersUtil.class);
    }

}
