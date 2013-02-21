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

    public static final String BANDWIDTH_DATADELIVERY_INTEGRATION_TEST_XML = "/bandwidth/bandwidth-datadelivery-integrationtest-impl.xml";

    public static final String BANDWIDTH_DATADELIVERY_INTEGRATION_TEST_NCF_XML = "/bandwidth/bandwidth-datadelivery-integrationtest-ncf-impl.xml";

    public static final String BANDWIDTH_DATADELIVERY_INTEGRATION_TEST_WFO_XML = "/bandwidth/bandwidth-datadelivery-integrationtest-wfo-impl.xml";

    public static final String BANDWIDTH_DATADELIVERY_XML = "/spring/bandwidth-datadelivery.xml";
    
    public static final String BANDWIDTH_DATADELIVERY_NCF_XML = "/spring/bandwidth-datadelivery-ncf.xml";

    public static final String BANDWIDTH_DATADELIVERY_WFO_XML = "/spring/bandwidth-datadelivery-wfo.xml";
}