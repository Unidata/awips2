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
package com.raytheon.uf.edex.datadelivery.retrieval.wfs;

import org.junit.Ignore;

import com.raytheon.uf.common.util.TestUtil;
import com.raytheon.uf.edex.datadelivery.retrieval.interfaces.IRetrievalRequestBuilder;


/**
 * Overrides specific methods in {@link WfsRetrievalAdapter} that require
 * external resources.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 04, 2013 2267       bgonzale    Initial creation
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */
@Ignore
public class MockWfsRetrievalAdapter extends WfsRetrievalAdapter {
    /**
     * Create a WfsRetrievalResponse with a test wfs data file.
     */
    @Override
    public WfsRetrievalResponse performRequest(IRetrievalRequestBuilder request) {
        String data = new String(TestUtil.readResource(this.getClass(),
                "/datadelivery/wfs/wfs_madis_dataset"));

        final WfsRetrievalResponse response = new WfsRetrievalResponse(null);
        response.setAttribute(request.getAttribute());
        response.setPayLoad(data);

        return response;
    }

}
