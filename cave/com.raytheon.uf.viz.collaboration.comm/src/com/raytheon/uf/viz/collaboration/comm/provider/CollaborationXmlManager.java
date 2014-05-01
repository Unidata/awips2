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
package com.raytheon.uf.viz.collaboration.comm.provider;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.xml.bind.JAXBException;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;

import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.jaxb.JAXBClassLocator;
import com.raytheon.uf.common.serialization.jaxb.JaxbDummyObject;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.procedures.ProcedureXmlManager;
import com.raytheon.uf.viz.core.reflect.SubClassLocator;

/**
 * Loads the XML context for all collaboration objects by using the xmlRoot
 * objects defined in an extension point.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Oct 31, 2013  2491     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class CollaborationXmlManager {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(CollaborationXmlManager.class);

    private static final String EXTENSION_ID = "com.raytheon.uf.viz.collaboration.xml";

    private static CollaborationXmlManager instance;

    private final JAXBManager manager;

    private CollaborationXmlManager() {
        manager = initManager();
    }

    private JAXBManager initManager() {
        List<Class<?>> baseClasses = new ArrayList<Class<?>>();
        IExtensionRegistry registry = Platform.getExtensionRegistry();
        IExtensionPoint point = registry.getExtensionPoint(EXTENSION_ID);
        if (point != null) {
            IExtension[] extensions = point.getExtensions();
            for (int i = 0; i < extensions.length; i++) {
                IConfigurationElement[] config = extensions[i]
                        .getConfigurationElements();

                for (int j = 0; j < config.length; j++) {
                    try {
                        baseClasses.add(config[j].createExecutableExtension(
                                "class").getClass());
                    } catch (CoreException e) {
                        statusHandler.handle(Priority.PROBLEM,
                                e.getLocalizedMessage(), e);
                    }
                }
            }
        }
        SubClassLocator locator = new SubClassLocator();
        Collection<Class<?>> classes = JAXBClassLocator.getJAXBClasses(locator,
                baseClasses.toArray(new Class<?>[0]));
        locator.save();

        Class<?>[] jaxbClasses = new Class<?>[classes.size() + 1];
        classes.toArray(jaxbClasses);
        /*
         * Add JaxbDummyObject at the begining so properties are loaded
         * correctly
         */
        jaxbClasses[jaxbClasses.length - 1] = jaxbClasses[0];
        jaxbClasses[0] = JaxbDummyObject.class;

        try {
            return new JAXBManager(jaxbClasses);
        } catch (JAXBException e) {
            statusHandler.handle(Priority.PROBLEM,
                    ProcedureXmlManager.class.getSimpleName()
                            + " Failed to initialize.", e);
        }
        return null;

    }

    private JAXBManager getManager() throws SerializationException {
        if (manager == null) {
            throw new SerializationException(
                    ProcedureXmlManager.class.getSimpleName()
                            + " Failed to initialize.");
        }
        return manager;
    }

    public Object unmarshal(String xml) throws SerializationException {
        try {
            return getManager().unmarshalFromXml(xml);
        } catch (JAXBException e) {
            throw new SerializationException(e);
        }
    }

    public String marshal(Object obj) throws SerializationException {
        try {
            return getManager().marshalToXml(obj);
        } catch (JAXBException e) {
            throw new SerializationException(e);
        }
    }

    public static synchronized CollaborationXmlManager getInstance() {
        if (instance == null) {
            instance = new CollaborationXmlManager();
        }
        return instance;
    }

}
