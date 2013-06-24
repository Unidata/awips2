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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;

import com.sun.xml.internal.bind.api.TypeReference;
import com.sun.xml.internal.bind.v2.model.annotation.RuntimeInlineAnnotationReader;
import com.sun.xml.internal.bind.v2.runtime.JAXBContextImpl;
import com.sun.xml.internal.bind.v2.runtime.JAXBContextImpl.JAXBContextBuilder;

/**
 * JAXBContext factory, creates the jaxb context. This classes is used so that
 * jaxb doesn't waste time on startup looking in the package of every class for
 * jaxb.properties. This class is referenced in DummyObject's jaxb.properties
 * file and DummyObject is added to the class list so this factory is
 * immediately found. Saves about 2s of initialization time
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 12, 2011            mschenke     Initial creation
 * June 24, 2013 #2126     bkowal       Update for Java 7 compatibility
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class SerializationContextFactory {

    @SuppressWarnings("rawtypes")
	public static JAXBContext createContext(Class[] classes, Map props) {
        // Construct delegate implementation
        System.setProperty(JAXBContextImpl.class.getName() + ".fastBoot",
                "true");
        JAXBContextBuilder builder = new JAXBContextBuilder();
        builder.setAnnotationReader(new RuntimeInlineAnnotationReader());
        builder.setClasses(classes);
        builder.setSubclassReplacements(new HashMap<Class, Class>());
        builder.setDefaultNsUri("");
        builder.setTypeRefs(new ArrayList<TypeReference>());
        try {
            // TODO: Can we override/extend some part of the context
            // implementation to allow for ignoring of xsi:type fields that we
            // don't have classes for?
            return new CustomJAXBContext(builder.build());
        } catch (JAXBException e) {
            throw new RuntimeException(e);
        }
    }

}
