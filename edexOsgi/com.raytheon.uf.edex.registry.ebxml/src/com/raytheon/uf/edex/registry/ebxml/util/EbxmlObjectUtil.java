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
package com.raytheon.uf.edex.registry.ebxml.util;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Enumeration;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.UUID;

import javax.servlet.http.HttpServletRequest;
import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.ws.WebServiceContext;
import javax.xml.ws.handler.MessageContext;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.DeliveryInfoType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ExtensibleObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.StringValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.VersionInfoType;

import com.raytheon.uf.common.registry.ebxml.RegistryUtil;

/**
 * General utility class containing the ebXML object factories.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 18, 2012 184        bphillip     Initial creation
 * 3/18/2013    1082       bphillip     Removed utility methods for VersionInfoType
 * 4/9/2013     1802       bphillip     Removed unused methods and addded a few new ones
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public class EbxmlObjectUtil {

    /** Default registry base URL */
    public static final String REGISTRY_BASE_URL = "http://"
            + System.getenv("EBXML_REGISTRY_HOST") + ":"
            + System.getenv("EBXML_REGISTRY_WEBSERVER_PORT");

    /** Slot name that holds the source of the notification */
    public static final String NOTIFICATION_SOURCE_URL_SLOT_NAME = "NotificationSourceURL";

    /**
     * The name of the slot designated to hold the home server address of a
     * registry object
     */
    public static final String HOME_SLOT_NAME = "urn:oasis:names:tc:ebxml-regrep:rim:RegistryObject:home";

    /** The name of the slot designated to hold the email notification formatter */
    public static final String EMAIL_NOTIFICATION_FORMATTER_SLOT = "urn:oasis:names:tc:ebxml-regrep:rim:DeliveryInfo:emailNotificationFormatter";

    /**
     * Slot name of the slot on the subscription object that holds the last run
     * time
     */
    public static final String SUBSCRIPTION_LAST_RUN_TIME_SLOT_NAME = "SubscriptionLastRunTime";

    /**
     * The lifecycle manager object factory
     */
    public static oasis.names.tc.ebxml.regrep.xsd.lcm.v4.ObjectFactory lcmObjectFactory = new oasis.names.tc.ebxml.regrep.xsd.lcm.v4.ObjectFactory();

    /**
     * The query object factory
     */
    public static oasis.names.tc.ebxml.regrep.xsd.query.v4.ObjectFactory queryObjectFactory = new oasis.names.tc.ebxml.regrep.xsd.query.v4.ObjectFactory();

    /**
     * The registry information model object factory
     */
    public static oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectFactory rimObjectFactory = new oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectFactory();

    /**
     * The registry services object factory
     */
    public static oasis.names.tc.ebxml.regrep.xsd.rs.v4.ObjectFactory rsObjectFactory = new oasis.names.tc.ebxml.regrep.xsd.rs.v4.ObjectFactory();

    /**
     * The supporting services object factory
     */
    public static oasis.names.tc.ebxml.regrep.xsd.spi.v4.ObjectFactory spiObjectFactory = new oasis.names.tc.ebxml.regrep.xsd.spi.v4.ObjectFactory();

    /**
     * Gets a new random UUID
     * 
     * @return The UUID
     */
    public static String getUUID() {
        return UUID.randomUUID().toString();
    }

    /**
     * Creates an object reference list from a list of objects. The list may be
     * a string of ids, ObjectRefType objects or a list of RegistryObjectTypes
     * 
     * @param objList
     *            The list of registry object references
     * @return The ObjectRefListType object containing the given references
     */
    public static ObjectRefListType createObjectRefList(
            Collection<?> objCollection) {
        ObjectRefListType refList = rimObjectFactory.createObjectRefListType();
        for (Object obj : objCollection) {
            ObjectRefType ref = null;
            if (obj instanceof ObjectRefType) {
                ref = (ObjectRefType) obj;
            } else if (obj instanceof RegistryObjectType) {
                ref = new ObjectRefType();
                ref.setId(((RegistryObjectType) obj).getId());
            } else if (obj instanceof String) {
                ref = new ObjectRefType();
                ref.setId((String) obj);
            } else {
                throw new IllegalArgumentException(
                        "Invalid type submitted to createObjectRefList");
            }
            refList.getObjectRef().add(ref);
        }
        return refList;
    }

    /**
     * Creates a RegistryObjectListType object from the given list of registry
     * objects
     * 
     * @param objList
     *            The registry objects to be put in the list object
     * @return The RegistryObjectListType object containing the given objects
     */
    public static RegistryObjectListType createRegistryObjectList(
            Collection<?> objCollection) {
        RegistryObjectListType registryObjectList = rimObjectFactory
                .createRegistryObjectListType();
        for (Object obj : objCollection) {
            if (obj instanceof RegistryObjectType) {
                registryObjectList.getRegistryObject().add(
                        (RegistryObjectType) obj);
            } else {
                throw new IllegalArgumentException(
                        "createRegistryObjectList only excepts collections with RegistryObjectTypes");
            }
        }
        return registryObjectList;
    }

    /**
     * Gets the XMLGregorianCalendar representation of the current time
     * 
     * @return XMLGregorianCalendar representation of the current time
     * @throws DatatypeConfigurationException
     *             if the time cannot be constructed properly
     */
    public static XMLGregorianCalendar getCurrentTimeAsXMLGregorianCalendar()
            throws DatatypeConfigurationException {
        return getTimeAsXMLGregorianCalendar(System.currentTimeMillis());
    }

    /**
     * Gets the XMLGregorianCalendar representation of the provided time in
     * milliseconds
     * 
     * @param timeInMillis
     *            The time in milliseconds to get the XMLGregorianCalendar for
     * @return The XMLGregorianCalendar representation of the provided time in
     *         milliseconds
     * @throws DatatypeConfigurationException
     *             if the time cannot be constructed properly
     */
    public static XMLGregorianCalendar getTimeAsXMLGregorianCalendar(
            long timeInMillis) throws DatatypeConfigurationException {
        GregorianCalendar cal = new GregorianCalendar();
        cal.setTimeInMillis(timeInMillis);
        return DatatypeFactory.newInstance().newXMLGregorianCalendar(cal);
    }

    /**
     * Scans all classes accessible from the context class loader which belong
     * to the given package and subpackages.
     * 
     * @param packageName
     *            The base package
     * @return The classes
     * @throws ClassNotFoundException
     * @throws IOException
     */
    public static Class<?>[] getClasses(String packageName)
            throws ClassNotFoundException, IOException {
        ClassLoader classLoader = Thread.currentThread()
                .getContextClassLoader();
        assert classLoader != null;
        String path = packageName.replace('.', '/');
        Enumeration<URL> resources = classLoader.getResources(path);
        List<File> dirs = new ArrayList<File>();
        while (resources.hasMoreElements()) {
            URL resource = resources.nextElement();
            dirs.add(new File(resource.getFile()));
        }
        List<Class<?>> classes = new ArrayList<Class<?>>();
        for (File directory : dirs) {
            classes.addAll(findClasses(directory, packageName));
        }
        return classes.toArray(new Class[classes.size()]);
    }

    /**
     * Recursive method used to find all classes in a given directory and
     * subdirs.
     * 
     * @param directory
     *            The base directory
     * @param packageName
     *            The package name for classes found inside the base directory
     * @return The classes
     * @throws ClassNotFoundException
     */
    private static List<Class<?>> findClasses(File directory, String packageName)
            throws ClassNotFoundException {
        List<Class<?>> classes = new ArrayList<Class<?>>();
        if (!directory.exists()) {
            return classes;
        }
        File[] files = directory.listFiles();
        for (File file : files) {
            if (file.isDirectory()) {
                assert !file.getName().contains(".");
                classes.addAll(findClasses(file,
                        packageName + "." + file.getName()));
            } else if (file.getName().endsWith(".class")) {
                classes.add(Class.forName(packageName
                        + '.'
                        + file.getName().substring(0,
                                file.getName().length() - 6)));
            }
        }
        return classes;
    }

    public static VersionInfoType incrementVersion(
            VersionInfoType existingVersion) {
        String newVersion = String.valueOf(Integer.parseInt(existingVersion
                .getVersionName()) + 1);
        VersionInfoType versionObj = new VersionInfoType();
        versionObj.setVersionName(newVersion);
        versionObj.setUserVersionName(existingVersion.getUserVersionName());
        return versionObj;
    }

    public static List<String> getIdsFromObjectRefListType(
            ObjectRefListType refList) {
        if (refList == null) {
            return Collections.emptyList();
        }
        return getIdsFromObjectRefList(refList.getObjectRef());
    }

    public static List<String> getIdsFromObjectRefList(List<ObjectRefType> refs) {
        List<String> ids = new ArrayList<String>();
        for (ObjectRefType ref : refs) {
            ids.add(ref.getId());
        }
        return ids;
    }

    public static void addStringSlot(ExtensibleObjectType object,
            String slotName, String slotValue, boolean overwrite) {
        if (containsSlot(object, slotName)) {
            if (overwrite) {
                getSlot(object, slotName).getSlotValue().setValue(slotValue);
            }
        } else {
            SlotType slot = RegistryUtil.getSlot(String.class.getName(),
                    slotName, slotValue);
            object.getSlot().add(slot);
        }
    }

    public static SlotType getSlot(ExtensibleObjectType object, String slotName) {
        for (SlotType slot : object.getSlot()) {
            if (slot.getName().equals(slotName)) {
                ValueType slotValue = slot.getSlotValue();
                if (slotValue instanceof StringValueType) {
                    return slot;
                }
            }
        }
        return null;
    }

    public static String getStringSlotValue(ExtensibleObjectType object,
            String slotName) {
        SlotType slot = getSlot(object, slotName);
        if (slot == null) {
            return null;
        }
        return ((StringValueType) slot.getSlotValue()).getStringValue();
    }

    public static boolean containsSlot(ExtensibleObjectType object,
            String slotName) {
        for (SlotType slot : object.getSlot()) {
            if (slot.getName().equals(slotName)) {
                return true;
            }
        }
        return false;

    }

    public static String getHomeSlot(ExtensibleObjectType object) {
        return getStringSlotValue(object, HOME_SLOT_NAME);
    }

    public static String getEmailNotificationFormatterSlot(
            DeliveryInfoType deliveryInfo) {
        return getStringSlotValue(deliveryInfo,
                EMAIL_NOTIFICATION_FORMATTER_SLOT);
    }

    private static List<String> HTTP_HEADERS;
    static {
        HTTP_HEADERS = new ArrayList<String>(5);
        HTTP_HEADERS.add("X-Forwarded-For");
        HTTP_HEADERS.add("Proxy-Client-IP");
        HTTP_HEADERS.add("WL-Proxy-Client-IP");
        HTTP_HEADERS.add("HTTP_CLIENT_IP");
        HTTP_HEADERS.add("HTTP_X_FORWARDED_FOR");
    }

    public static String getClientHost(WebServiceContext wsContext) {
        if (wsContext == null) {
            return "INTERNAL";
        }
        MessageContext mc = wsContext.getMessageContext();
        if (mc == null) {
            return "INTERNAL";
        }
        HttpServletRequest request = (HttpServletRequest) mc
                .get(MessageContext.SERVLET_REQUEST);
        String ip = null;
        request.getHeader("X-Forwarded-For");

        for (int i = 0; (i < 5)
                && (ip == null || ip.isEmpty() || "unknown"
                        .equalsIgnoreCase(ip)); i++) {
            ip = request.getHeader(HTTP_HEADERS.get(i));
        }
        if (ip == null || ip.length() == 0 || "unknown".equalsIgnoreCase(ip)) {
            ip = request.getRemoteAddr();
        }
        return ip;
    }
}
