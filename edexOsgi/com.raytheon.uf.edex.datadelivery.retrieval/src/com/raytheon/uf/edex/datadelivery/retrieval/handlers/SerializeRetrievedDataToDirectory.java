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

import java.io.File;
import java.util.UUID;

import javax.xml.bind.JAXBException;

import com.raytheon.uf.common.datadelivery.registry.Coverage;
import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.edex.datadelivery.retrieval.opendap.OpenDapRetrievalResponse;

/**
 * Serializes the retrieved data to a directory.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 01, 2013 1543       djohnson     Initial creation
 * Feb 15, 2013 1543       djohnson     Serialize data out as XML.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
public class SerializeRetrievedDataToDirectory implements
        IRetrievalPluginDataObjectsProcessor {

    private final JAXBManager jaxbManager;

    private final File targetDirectory;

    /**
     * @param directory
     */
    public SerializeRetrievedDataToDirectory(File directory) {
        this.targetDirectory = directory;
        try {
            this.jaxbManager = new JAXBManager(RetrievalResponseXml.class,
                    OpenDapRetrievalResponse.class, Coverage.class);
        } catch (JAXBException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void processRetrievedPluginDataObjects(
            RetrievalResponseXml retrievalPluginDataObjects)
            throws SerializationException {
        retrievalPluginDataObjects.prepareForSerialization();

        try {
            final File output = new File(targetDirectory, UUID.randomUUID()
                    .toString());
            final String xml = jaxbManager
                    .marshalToXml(retrievalPluginDataObjects);
            FileUtil.bytes2File(xml.getBytes(), output);
        } catch (Exception e) {
            throw new SerializationException(e);
        }
    }

}
