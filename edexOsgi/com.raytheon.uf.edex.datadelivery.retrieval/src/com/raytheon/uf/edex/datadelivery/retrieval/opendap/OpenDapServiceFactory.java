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

import java.util.Date;

import com.raytheon.uf.common.datadelivery.registry.Provider;
import com.raytheon.uf.edex.datadelivery.retrieval.IExtractMetaData;
import com.raytheon.uf.edex.datadelivery.retrieval.IParseMetaData;
import com.raytheon.uf.edex.datadelivery.retrieval.RetrievalGenerator;
import com.raytheon.uf.edex.datadelivery.retrieval.ServiceFactory;

/**
 * Implementation of {@link ServiceFactory} that handles OpenDAP. This should be
 * the ONLY non package-private class in this entire package.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 24, 2012 955        djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public final class OpenDapServiceFactory implements ServiceFactory {

    private static final OpenDAPMetaDataParser PARSER = new OpenDAPMetaDataParser();

    private final Provider provider;

    public OpenDapServiceFactory(final Provider provider) {
        this.provider = provider;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.datadelivery.harvester.interfaces.MetaDataFactory
     * #getExtractor()
     */
    @Override
    public IExtractMetaData getExtractor() {
        return new OpenDAPMetaDataExtracter(provider.getConnection());
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.edex.datadelivery.harvester.interfaces.MetaDataFactory
     * #getParser()
     */
    @Override
    public IParseMetaData getParser(final Date lastUpdate) {
        return PARSER;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.edex.datadelivery.retrieval.ServiceFactory#
     * getRetrievalGenerator()
     */
    @Override
    public RetrievalGenerator getRetrievalGenerator() {
        return new OpenDAPRetrievalGenerator();
    }
}
