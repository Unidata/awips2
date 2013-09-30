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
package com.raytheon.uf.common.serialization;

import java.io.File;

import javax.xml.bind.JAXBException;

/**
 * A JAXBManager that only supports a single class (including any classes that
 * are contained within that class). Useful when dealing specifically with an
 * XML file where you know the type that corresponds to it.
 * 
 * Primarily used for convenience to avoid casting.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 1, 2013  2361       njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 * @param <T>
 */

public class SingleTypeJAXBManager<T extends Object> extends JAXBManager {

    protected Class<T> type;

    /**
     * Constructor. Only accepts a single class.
     * 
     * @param clazz
     *            the class of the object to read/write XML for.
     * @throws JAXBException
     */
    public SingleTypeJAXBManager(Class<T> clazz) throws JAXBException {
        super(clazz);
        this.type = clazz;
    }

    /**
     * Instantiates an object from the XML representation in a File.
     * 
     * @param file
     *            The XML file
     * @return A new instance from the XML representation
     * @throws SerializationException
     */
    public T unmarshalFromXmlFile(File file) throws SerializationException {
        return super.unmarshalFromXmlFile(type, file);
    }

    /**
     * Instantiates an object from the XML representation in a File.
     * 
     * @param filePath
     *            The path to the XML file
     * @return A new instance from the XML representation
     * @throws SerializationException
     */
    public T unmarshalFromXmlFile(String filePath)
            throws SerializationException {
        return super.unmarshalFromXmlFile(type, new File(filePath));
    }

}
