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
package com.raytheon.uf.common.registry.ebxml.encoder;

import static com.raytheon.uf.common.registry.ebxml.encoder.RegistryEncoders.Type.JAXB;

import javax.xml.bind.JAXBException;

import com.raytheon.uf.common.registry.annotations.RegistryObjectVersion;
import com.raytheon.uf.common.registry.schemas.ebxml.util.EbxmlJaxbManager;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.util.ReflectionUtil;

/**
 * A {@link StringBasedEncoder} implementation that uses JAXB. Package-private
 * as it is not directly accessibly in the public API.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 07, 2012 1102      djohnson     Initial creation
 * Jun 03, 2013 2038      djohnson     Add equals/hashcode.
 * Oct 31, 2013 2361      njensen      Use specific JAXBManager instead of SerializationUtil
 * Nov 14, 2013 2552      bkowal       EbxmlJaxbManager is now accessed via getInstance
 * Dec 08, 2013 2584      dhladky      Versions for JAXB objects, Only use the JAXb encoder now.
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */

class JaxbEncoder extends StringBasedEncoder {
    
    /** The logger */
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(JaxbEncoder.class);
    
    /**
     * @param type
     */
    JaxbEncoder() {
        super(JAXB);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    Object decodeContent(String content, String className, String version)
            throws SerializationException {

        String classVersion = getClassVersion(className);

        if (classVersion.equals(version)) {
            try {
                return EbxmlJaxbManager.getInstance().getJaxbManager()
                        .unmarshalFromXml(content);
            } catch (JAXBException e) {
                throw new SerializationException(
                        "Unable to decode the object!", e);
            }
        } else {
            statusHandler.handle(Priority.INFO,
                    "Mismatching class versions, returning content. "
                            + className + " version: " + version);
            return content;
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    String encodeContent(Object objectToEncode) throws SerializationException {
        try {
            // We always encode using our current version
            return new String(EbxmlJaxbManager.getInstance().getJaxbManager()
                    .marshalToXml(objectToEncode));
        } catch (JAXBException e) {
            throw new SerializationException("Unable to encode the object!", e);
        }
    }
   
    /**
     * Get the version of the class
     * 
     * @param className
     * @return version
     */
    public String getClassVersion(String className) {

        String version = EbxmlJaxbManager.getInstance().getVersion(className);

        if (version == null) {

            Class<?> clazz = EbxmlJaxbManager.getInstance().getClass(className);
            RegistryObjectVersion rov = ReflectionUtil.getAnnotationFromClass(
                    clazz, RegistryObjectVersion.class);
            if (rov != null) {
                version = String.valueOf(rov.value());
                EbxmlJaxbManager.getInstance().addVersion(className, version);
            } else {
                throw new IllegalArgumentException(
                        "Unable to extract RegistryObjectVersion tag from class! "
                                + className);
            }
        }

        return version;
    }
}


