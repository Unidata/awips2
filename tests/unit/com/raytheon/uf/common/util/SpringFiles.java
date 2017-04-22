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
package com.raytheon.uf.common.util;

import org.junit.Ignore;

/**
 * Utility class for Spring integration.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 12, 2013 1543       djohnson     Initial creation
 * Apr 23, 2013 1910       djohnson     Add constants for ebxml spring files.
 * May 02, 2013 1910       djohnson     Add validator plugins spring file.
 * May 28, 2013 1650       djohnson     Add event bus spring files.
 * Jun 24, 2013 2106       djohnson     Remove spring file.
 * Jul 10, 2013 2106       djohnson     Add MEMORY_DATADELIVERY_HANDLERS_XML.
 * Jan 09, 2014 2615       bgonzale     Add DATADELIVERY_STANDALONE_XML.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@Ignore
public class SpringFiles {

    /**
     * Disabled constructor.
     */
    private SpringFiles() {
    }

    public static final String RETRIEVAL_DATADELIVERY_DAOS_XML = "/spring/retrieval-datadelivery-daos.xml"; 

    public static final String BANDWIDTH_DATADELIVERY_DAOS_XML = "/spring/bandwidth-datadelivery-daos.xml";

    public static final String BANDWIDTH_DATADELIVERY_EVENTBUS_XML = "/spring/bandwidth-datadelivery-eventbus.xml";

    public static final String BANDWIDTH_DATADELIVERY_INTEGRATION_TEST_XML = "/bandwidth/bandwidth-datadelivery-integrationtest-impl.xml";

    public static final String BANDWIDTH_DATADELIVERY_INTEGRATION_TEST_NCF_XML = "/bandwidth/bandwidth-datadelivery-integrationtest-ncf-impl.xml";

    public static final String BANDWIDTH_DATADELIVERY_INTEGRATION_TEST_WFO_XML = "/bandwidth/bandwidth-datadelivery-integrationtest-wfo-impl.xml";

    public static final String BANDWIDTH_DATADELIVERY_XML = "/spring/bandwidth-datadelivery.xml";
    
    public static final String BANDWIDTH_DATADELIVERY_NCF_XML = "/spring/bandwidth-datadelivery-ncf.xml";

    public static final String BANDWIDTH_DATADELIVERY_WFO_XML = "/spring/bandwidth-datadelivery-wfo.xml";

    public static final String DATADELIVERY_HANDLERS_XML = "/spring/datadelivery-handlers.xml";

    public static final String DATADELIVERY_HANDLERS_IMPL_XML = "/spring/datadelivery-handlers-impl.xml";

    public static final String DATADELIVERY_STANDALONE_XML = "/spring/datadelivery-standalone.xml";

    public static final String EBXML_XML = "/spring/ebxml.xml";

    public static final String EBXML_IMPL_XML = "/spring/ebxml-impl.xml";

    public static final String EBXML_QUERYTYPES_XML = "/spring/ebxml-querytypes.xml";

    public static final String EBXML_REGISTRY_DAO_XML = "/spring/ebxml-registry-dao.xml";

    public static final String EBXML_REPLICATION_DATADELIVERY_WFO_XML = "/spring/registry-replication-datadelivery-wfo.xml";

    public static final String EBXML_SUBSCRIPTION_XML = "/spring/ebxml-subscription.xml";

    public static final String EBXML_XACML_XML = "/spring/ebxml-xacml.xml";

    public static final String EBXML_WEBSERVICES_XML = "/spring/ebxml-webservices.xml";

    public static final String EBXML_VALIDATOR_PLUGINS_XML = "/spring/ebxml-validator-plugins.xml";

    public static final String EVENTBUS_COMMON_XML = "/spring/eventbus-common.xml";

    public static final String MEMORY_DATADELIVERY_HANDLERS_XML = "/datadelivery/memory-datadelivery-handlers.xml";

    public static final String UNIT_TEST_DB_BEANS_XML = "/unit-test-db-beans.xml";

    public static final String UNIT_TEST_LOCALIZATION_BEANS_XML = "/unit-test-localization-beans.xml";

    public static final String UNIT_TEST_EBXML_BEANS_XML = "/ebxml/unit-test-ebxml-beans.xml";

    public static final String UNIT_TEST_EBXML_REPLICATION_BEANS_XML = "/ebxml/unit-test-ebxml-replication-beans.xml";

    public static final String UNIT_TEST_EBXML_PLUGIN_NOTIFICATION_LISTENER_XML = "/ebxml/ebxml-plugin-notification-listener.xml";

}