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

import java.util.HashSet;
import java.util.Set;

import javax.xml.bind.JAXBException;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRegistry;

import org.reflections.Reflections;
import org.reflections.scanners.TypeAnnotationsScanner;
import org.reflections.util.ClasspathHelper;
import org.reflections.util.ConfigurationBuilder;

import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

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
 * Nov 14, 2013 2252       bkowal      Added the ability to dynamically inject packages
 *                                     that this jaxb implementation should support.
 *                                     Eliminated use of System.out.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class EbxmlJaxbManager {

    /** The logger */
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(EbxmlJaxbManager.class);

    private static EbxmlJaxbManager instance;

    private static JAXBManager jaxb;

    private static Set<Class<?>> jaxables;

    public static synchronized EbxmlJaxbManager getInstance() {
        if (instance == null) {
            instance = new EbxmlJaxbManager();
        }
        return instance;
    }

    public String findJaxables(String packageName) {
        statusHandler.info("Scanning package ... " + packageName);

        long t0 = System.currentTimeMillis();
        ConfigurationBuilder cb = new ConfigurationBuilder();
        cb.addUrls(ClasspathHelper.forPackage(packageName));
        cb.setScanners(new TypeAnnotationsScanner());
        // the call to build() will do the actual scanning so the separate
        // calls to getTypesAnnotatedWith(class, false) will not slow it down

        Reflections reflecs = cb.build();
        Set<Class<?>> set = reflecs.getTypesAnnotatedWith(
                XmlAccessorType.class, false);
        synchronized (jaxables) {
            // add them to set for auditing purposes initially
            set.addAll(reflecs.getTypesAnnotatedWith(XmlRegistry.class, false));
            // copy set to jaxables
            jaxables.addAll(set);
        }
        long t1 = System.currentTimeMillis();
        statusHandler.info("Found " + set.size() + " classes for ebxml in "
                + (t1 - t0) + " ms");
        // if jaxb has already been initialized, reset it so that it will be
        // recreated with the latest set of jaxable classes.
        synchronized (this) {
            jaxb = null;
        }

        return packageName;
    }

    public synchronized JAXBManager getJaxbManager() throws JAXBException {
        if (jaxb == null) {
            jaxb = new JAXBManager(jaxables.toArray(new Class[0]));
        }
        return jaxb;
    }

    private EbxmlJaxbManager() {
        jaxables = new HashSet<Class<?>>();

        // add the default jaxables
        jaxables.add(oasis.names.tc.ebxml.regrep.xsd.lcm.v4.ObjectFactory.class);
        jaxables.add(oasis.names.tc.ebxml.regrep.xsd.query.v4.ObjectFactory.class);
        jaxables.add(oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectFactory.class);
        jaxables.add(oasis.names.tc.ebxml.regrep.xsd.rs.v4.ObjectFactory.class);
        jaxables.add(oasis.names.tc.ebxml.regrep.xsd.spi.v4.ObjectFactory.class);

        statusHandler.info("Initialization Complete.");
    }
}