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
package com.raytheon.uf.common.registry.schemas.ebxml.util;

import java.util.Set;

import javax.xml.bind.JAXBException;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRegistry;

import org.reflections.Reflections;
import org.reflections.scanners.TypeAnnotationsScanner;
import org.reflections.util.ClasspathHelper;
import org.reflections.util.ConfigurationBuilder;

import com.raytheon.uf.common.serialization.JAXBManager;

/**
 * A JAXB Manager for transforming EBXML objects to/from XML.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 30, 2013 2361       njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class EbxmlJaxbManager {

    private static JAXBManager jaxb;

    /**
     * Uses reflections to scan for ebxml datadelivery registry classes that can
     * be transformed to/from xml, and then adds in the ebxml object factories.
     * 
     * @return the classes it found without any duplicates
     */
    private static Class<?>[] getClasses() {
        String[] packageNames = new String[] {
                "com.raytheon.uf.common.datadelivery.registry" };

        long t0 = System.currentTimeMillis();
        ConfigurationBuilder cb = new ConfigurationBuilder();
        for (String pkg : packageNames) {
            cb.addUrls(ClasspathHelper.forPackage(pkg));
        }
        cb.setScanners(new TypeAnnotationsScanner());
        // the call to build() will do the actual scanning so the separate
        // calls to getTypesAnnotatedWith(class, false) will not slow it down
        Reflections reflecs = cb.build();
        Set<Class<?>> set = reflecs.getTypesAnnotatedWith(
                XmlAccessorType.class, false);
        set.addAll(reflecs.getTypesAnnotatedWith(XmlRegistry.class, false));
        long t1 = System.currentTimeMillis();
        System.out.println("Found " + set.size() + " classes for ebxml in "
                + (t1 - t0) + " ms");

        set.add(oasis.names.tc.ebxml.regrep.xsd.lcm.v4.ObjectFactory.class);
        set.add(oasis.names.tc.ebxml.regrep.xsd.query.v4.ObjectFactory.class);
        set.add(oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectFactory.class);
        set.add(oasis.names.tc.ebxml.regrep.xsd.rs.v4.ObjectFactory.class);
        set.add(oasis.names.tc.ebxml.regrep.xsd.spi.v4.ObjectFactory.class);

        return set.toArray(new Class[0]);
    }

    public static synchronized JAXBManager getJaxbManager()
            throws JAXBException {
        if (jaxb == null) {
            jaxb = new JAXBManager(getClasses());
        }
        return jaxb;
    }

    private EbxmlJaxbManager() {

    }

}
