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
package com.raytheon.uf.viz.core.procedures;

import java.io.File;
import java.util.Collection;

import javax.xml.bind.JAXBException;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

import com.raytheon.uf.common.serialization.JAXBManager;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.jaxb.JAXBClassLocator;
import com.raytheon.uf.common.serialization.jaxb.JaxbDummyObject;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.reflect.SubClassLocator;
import com.raytheon.uf.viz.core.rsc.ResourceGroup;

/**
 * Centralized class for handling xml mapping of {@link Procedure}s,
 * {@link Bundle}s, {@link IRenderableDisplay}s, {@link ResourceGroup}s, and
 * anything else that might appear in a procedure.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Oct 18, 2013  2491     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

public class ProcedureXmlManager {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ProcedureXmlManager.class);

    private static ProcedureXmlManager instance;

    private final JAXBManager manager;

    /**
     * Get the singleton instance.
     * 
     * @return singleton ProcedureXmlManager
     */
    public static synchronized ProcedureXmlManager getInstance() {
        if (instance == null) {
            instance = new ProcedureXmlManager();
        }
        return instance;
    }

    /**
     * Start initializing singleton manager in a background thread.
     * 
     * @return true
     */
    public static boolean inititializeAsync() {
        new Job("Preparing to parse procedure Xml.") {

            @Override
            protected IStatus run(IProgressMonitor monitor) {
                getInstance();
                return Status.OK_STATUS;
            }

        }.schedule();
        return true;
    }

    private ProcedureXmlManager() {
        this.manager = initManager();
    }

    private JAXBManager initManager() {
        SubClassLocator locator = new SubClassLocator();
        Collection<Class<?>> classes = JAXBClassLocator.getJAXBClasses(locator,
                Procedure.class);
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

    /**
     * Transform an object to XML using the internal procedure context and all
     * its registered classes.
     * 
     * @param obj
     *            something to marshal
     * @return XML in a String
     * @throws SerializationException
     */
    public String marshal(Object obj) throws SerializationException {
        try {
            return getManager().marshalToXml(obj);
        } catch (JAXBException je) {
            throw new SerializationException(je);
        }
    }

    /**
     * Transofrm an object to XML and store it in the specified file.
     * 
     * @param obj
     *            Object to store
     * @param filePath
     *            file to store object in.
     * @throws SerializationException
     */
    public void marshalToFile(Object obj, String filePath)
            throws SerializationException {
        getManager().marshalToXmlFile(obj, filePath);
    }

    /**
     * Transform some xml in a String to an object of the specified class.
     * 
     * @param clazz
     *            expected type of object
     * @param xml
     *            string containing xml
     * @return the object
     * @throws SerializationException
     */
    public <T> T unmarshal(Class<T> clazz, String xml)
            throws SerializationException {
        try {
            return clazz.cast(getManager().unmarshalFromXml(xml));
        } catch (ClassCastException cce) {
            throw new SerializationException(cce);
        } catch (JAXBException je) {
            throw new SerializationException(je);
        }
    }

    /**
     * Transform some xml in a File to an object of the specified class.
     * 
     * @param clazz
     *            expected type of object
     * @param file
     *            file containing xml
     * @return the object
     * @throws SerializationException
     */
    public <T> T unmarshal(Class<T> clazz, File file)
            throws SerializationException {
        return getManager().unmarshalFromXmlFile(clazz, file);
    }

}
