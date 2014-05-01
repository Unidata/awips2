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
package com.raytheon.uf.edex.datadelivery.retrieval.handlers;

import javax.xml.bind.JAXBException;

import com.raytheon.edex.esb.Headers;
import com.raytheon.uf.common.datadelivery.registry.Coverage;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.edex.datadelivery.retrieval.opendap.OpenDapRetrievalResponse;
import com.raytheon.uf.edex.datadelivery.retrieval.wfs.WfsRetrievalResponse;
import com.raytheon.uf.edex.wmo.message.WMOMessage;
import com.raytheon.uf.edex.wmo.message.XmlWMOMessage;

/**
 * Deserializes the retrieved data in a retrievalQueue.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 01, 2013 1543       djohnson     Initial creation
 * Mar 05, 2013 1647       djohnson     Remove WMO header.
 * Mar 19, 2013 1794       djohnson     Read from a queue rather than the file system.
 * Oct 04, 2013 2267       bgonzale     Added WfsRetrieval to unmarshal classes.
 * Nov 04, 2013 2506       bgonzale     Added SbnRetrievalResponseXml to unmarshal classes.
 *                                      Trim content after last xml tag during 
 *                                      marshaling from xml.
 * Jan 30, 2014 2686       dhladky      refactor of retrieval.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class DeserializeRetrievedDataFromIngest implements IRetrievalsFinder {

    private final JAXBManager jaxbManager;

    /**
     * @param retrievalQueue
     */
    public DeserializeRetrievedDataFromIngest() {

        try {
            this.jaxbManager = new JAXBManager(RetrievalResponseXml.class,
                    SbnRetrievalResponseXml.class,
                    OpenDapRetrievalResponse.class, WfsRetrievalResponse.class,
                    Coverage.class);
        } catch (JAXBException e) {
            throw new ExceptionInInitializerError(e);
        }

    }

    /**
     * {@inheritDoc}
     */
    @Override
    public RetrievalResponseXml processRequest(RetrievalRequestWrapper rrw) throws Exception {
       
        String xml = (String) rrw.getPayload();

        if (xml == null) {
            return null;
        } else {
            WMOMessage message = new XmlWMOMessage(xml, new Headers());
            return (RetrievalResponseXml) jaxbManager.unmarshalFromXml(message
                    .getBodyText());
        }

    }

}
