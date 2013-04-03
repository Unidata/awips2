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
package com.raytheon.uf.edex.datadelivery.retrieval.wms;

import java.util.Date;

import com.raytheon.uf.edex.datadelivery.retrieval.IExtractMetaData;
import com.raytheon.uf.edex.datadelivery.retrieval.IParseMetaData;
import com.raytheon.uf.edex.datadelivery.retrieval.RetrievalGenerator;
import com.raytheon.uf.edex.datadelivery.retrieval.ServiceFactory;

/**
 * {@link ServiceFactory} implementation for WMS.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 25, 2012 955        djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class WmsServiceFactory implements ServiceFactory {

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.datadelivery.retrieval.ServiceFactory#getExtractor()
     */
    @Override
    public IExtractMetaData getExtractor() {
        // TODO Auto-generated method stub
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.datadelivery.retrieval.ServiceFactory#getParser(
     * java.util.Date)
     */
    @Override
    public IParseMetaData getParser(Date lastUpdate) {
        // TODO Auto-generated method stub
        return null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.datadelivery.retrieval.ServiceFactory#
     * getRetrievalGenerator()
     */
    @Override
    public RetrievalGenerator getRetrievalGenerator() {
        return new WmsRetrievalGenerator();
    }

}
