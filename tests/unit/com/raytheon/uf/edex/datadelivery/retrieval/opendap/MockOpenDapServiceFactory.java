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
package com.raytheon.uf.edex.datadelivery.retrieval.opendap;

import org.junit.Ignore;

import com.raytheon.uf.common.datadelivery.registry.Provider;
import com.raytheon.uf.edex.datadelivery.retrieval.RetrievalGenerator;

/**
 * Extends {@link OpenDapServiceFactory} to not require external systems.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 30, 2013 1543       djohnson     Initial creation
 * May 31, 2013 2038       djohnson     Call setProvider in constructor.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@Ignore
public class MockOpenDapServiceFactory extends OpenDapServiceFactory {

    /**
     * @param provider
     */
    public MockOpenDapServiceFactory(Provider provider) {
        super();
        setProvider(provider);
    }

    @Override
    public RetrievalGenerator getRetrievalGenerator() {
        return new MockOpenDapRetrievalGenerator();
    }
}
