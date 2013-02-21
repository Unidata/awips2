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

import com.raytheon.uf.common.datadelivery.retrieval.xml.RetrievalAttribute;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.util.TestUtil;
import com.raytheon.uf.edex.datadelivery.retrieval.interfaces.IRetrievalRequestBuilder;
import com.raytheon.uf.edex.datadelivery.retrieval.response.MockOpenDAPTranslator;
import com.raytheon.uf.edex.datadelivery.retrieval.response.OpenDAPTranslator;

import dods.dap.DConnectTest;
import dods.dap.DataDDS;

/**
 * Overrides specific methods in {@link OpenDAPRetrievalAdapter} that require
 * external resources.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 06, 2013 1543       djohnson     Initial creation
 * Feb 12, 2013 1543       djohnson     Use DodsUtils.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
@Ignore
public class MockOpenDapRetrievalAdapter extends OpenDAPRetrievalAdapter {
    /**
     * Instead of connecting to an external OpenDAP system, it reads in the dods
     * file and recreates the {@link DataDDS} instance.
     */
    @Override
    public OpenDapRetrievalResponse performRequest(
            IRetrievalRequestBuilder request) {
        DataDDS data;
        try {
            data = DodsUtils.restoreDataDdsFromByteArray(TestUtil.readResource(
                    DConnectTest.class,
                    "/datadelivery/opendap/compressed_rap_dataset.dods"));
        } catch (SerializationException e) {
            throw new RuntimeException(e);
        }

        final OpenDapRetrievalResponse response = new OpenDapRetrievalResponse();
        response.setAttribute(request.getAttribute());
        response.setPayLoad(data);

        return response;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    OpenDAPTranslator getOpenDapTranslator(RetrievalAttribute attribute)
            throws InstantiationException {
        return new MockOpenDAPTranslator(attribute);
    }
}
