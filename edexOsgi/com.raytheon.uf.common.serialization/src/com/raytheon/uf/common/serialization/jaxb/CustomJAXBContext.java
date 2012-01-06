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
package com.raytheon.uf.common.serialization.jaxb;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import javax.xml.bind.Validator;

import com.sun.xml.internal.bind.v2.runtime.JAXBContextImpl;

/**
 * Custom JAXBContext, used to create CustomJAXBUnmarshaller (which creates
 * needed CustomContentHandler)
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 13, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class CustomJAXBContext extends JAXBContext {

    private JAXBContextImpl delegate;

    /**
     * @param delegate
     */
    public CustomJAXBContext(JAXBContextImpl delegate) {
        this.delegate = delegate;
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.xml.bind.JAXBContext#createMarshaller()
     */
    @Override
    public Marshaller createMarshaller() throws JAXBException {
        return delegate.createMarshaller();
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.xml.bind.JAXBContext#createUnmarshaller()
     */
    @Override
    public Unmarshaller createUnmarshaller() throws JAXBException {
        return new CustomJAXBUnmarshaller(delegate.createUnmarshaller());
    }

    /*
     * (non-Javadoc)
     * 
     * @see javax.xml.bind.JAXBContext#createValidator()
     */
    @Override
    public Validator createValidator() throws JAXBException {
        return delegate.createValidator();
    }

}
