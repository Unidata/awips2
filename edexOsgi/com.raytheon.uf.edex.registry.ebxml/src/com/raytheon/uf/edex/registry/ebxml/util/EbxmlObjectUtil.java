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
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Enumeration;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.UUID;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectRefType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectListType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.RegistryObjectType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.VersionInfoType;

import com.raytheon.uf.edex.registry.ebxml.dao.VersionInfoTypeDao;
import com.raytheon.uf.edex.registry.ebxml.exception.EbxmlRegistryException;

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
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public class EbxmlObjectUtil {

    /**
     * The ebXML namespace
     */
    public static final String EBXML_PREFIX = "urn:oasis:names:tc:ebxml-regrep";

    /**
     * The default launguge
     */
    public static final String EN_US = "en-US";

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
     * Populates a VersionInfoType object with the next version number. Version
     * numbering starts at 1.
     * 
     * @param version
     *            The version object to increment
     * @throws EbxmlRegistryException
     */
    public static VersionInfoType getNewVersion(VersionInfoType existingVersion)
            throws EbxmlRegistryException {
        String newVersion = String.valueOf(Integer.parseInt(existingVersion
                .getVersionName()) + 1);

        VersionInfoType versionObj = rimObjectFactory.createVersionInfoType();
        versionObj.setVersionName(newVersion);
        versionObj.setUserVersionName("1");
        return new VersionInfoTypeDao().sync(versionObj);
    }

    /**
     * Creates a new VersionInfoType object with the default version numbers
     * 
     * @return The new VersionInfoType object
     * @throws EbxmlRegistryException
     */
    public static VersionInfoType newVersionObject()
            throws EbxmlRegistryException {
        VersionInfoType version = rimObjectFactory.createVersionInfoType();
        version.setVersionName("1");
        version.setUserVersionName("1");
        return new VersionInfoTypeDao().sync(version);
    }

    /**
     * Creates an object reference list from a list of registry object
     * references
     * 
     * @param objList
     *            The list of registry object references
     * @return The ObjectRefListType object containing the given references
     */
    public static ObjectRefListType createObjectRefList(
            List<ObjectRefType> objList) {
        ObjectRefListType refList = rimObjectFactory.createObjectRefListType();
        refList.getObjectRef().addAll(objList);
        return refList;
    }

    /**
     * Creates an object reference list from a list of registry objects
     * 
     * @param objs
     *            The list of registry object references
     * @return The ObjectRefListType object containing the references to the
     *         given registry objects
     */
    public static <T extends RegistryObjectType> ObjectRefListType createObjectRefListFromObjects(
            List<T> objs) {
        List<ObjectRefType> refs = new ArrayList<ObjectRefType>();
        for (RegistryObjectType obj : objs) {
            ObjectRefType ref = rimObjectFactory.createObjectRefType();
            ref.setId(obj.getId());
            refs.add(ref);
        }
        return createObjectRefList(refs);
    }

    /**
     * Creates a RegistryObjectListType object from the given list of registry
     * objects
     * 
     * @param objList
     *            The registry objects to be put in the list object
     * @return The RegistryObjectListType object containing the given objects
     */
    public static <T extends RegistryObjectType> RegistryObjectListType createRegistryObjectList(
            List<T> objList) {
        RegistryObjectListType refList = rimObjectFactory
                .createRegistryObjectListType();
        refList.getRegistryObject().addAll(objList);
        return refList;
    }

    /**
     * Gets the XMLGregorianCalendar representation of the current time
     * 
     * @return XMLGregorianCalendar representation of the current time
     * @throws DatatypeConfigurationException
     *             if the time cannot be constructed properly
     */
    public static XMLGregorianCalendar getCurrentTime()
            throws DatatypeConfigurationException {
        return getTime(System.currentTimeMillis());
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
    public static XMLGregorianCalendar getTime(long timeInMillis)
            throws DatatypeConfigurationException {
        GregorianCalendar cal = new GregorianCalendar();
        cal.setTimeInMillis(timeInMillis);
        return DatatypeFactory.newInstance().newXMLGregorianCalendar(cal);
    }

    /**
     * Extracts the ids from the given list of ObjectRefType objects and returns
     * them in a list
     * 
     * @param refs
     *            The ids
     * @return The ids in a list of Strings
     */
    public static List<String> getIdsFromRefs(List<ObjectRefType> refs) {
        List<String> ids = new ArrayList<String>();
        for (ObjectRefType ref : refs) {
            ids.add(ref.getId());
        }
        return ids;
    }

    /**
     * Utility function to convert an array of objects into a list. The array
     * may contain collections. Each member of the collection is added to the
     * returned list.
     * 
     * @param objects
     *            The objects to be moved to a list
     * @return The compiled list of objects
     */
    @SuppressWarnings("unchecked")
    public static <T extends Object> List<T> getList(T... objects) {
        List<T> retVal = new ArrayList<T>();
        for (T obj : objects) {
            if (obj instanceof Collection<?>) {
                Collection<?> coll = (Collection<?>) obj;
                for (Object obj2 : coll) {
                    retVal.add((T) obj2);
                }
            } else {
                retVal.add(obj);
            }
        }
        return retVal;
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

    public static List<Class<?>> getClassesForPackage(Package pkg) {
        String pkgname = pkg.getName();
        ArrayList<Class<?>> classes = new ArrayList<Class<?>>();
        // Get a File object for the package
        File directory = null;
        String fullPath;
        String relPath = pkgname.replace('.', '/');
        System.out.println("ClassDiscovery: Package: " + pkgname
                + " becomes Path:" + relPath);
        URL resource = ClassLoader.getSystemClassLoader().getResource(relPath);
        System.out.println("ClassDiscovery: Resource = " + resource);
        if (resource == null) {
            throw new RuntimeException("No resource for " + relPath);
        }
        fullPath = resource.getFile();
        System.out.println("ClassDiscovery: FullPath = " + resource);

        try {
            directory = new File(resource.toURI());
        } catch (URISyntaxException e) {
            throw new RuntimeException(
                    pkgname
                            + " ("
                            + resource
                            + ") does not appear to be a valid URL / URI.  Strange, since we got it from the system...",
                    e);
        } catch (IllegalArgumentException e) {
            directory = null;
        }
        System.out.println("ClassDiscovery: Directory = " + directory);

        if (directory != null && directory.exists()) {
            // Get the list of the files contained in the package
            String[] files = directory.list();
            for (int i = 0; i < files.length; i++) {
                // we are only interested in .class files
                if (files[i].endsWith(".class")) {
                    // removes the .class extension
                    String className = pkgname + '.'
                            + files[i].substring(0, files[i].length() - 6);
                    System.out.println("ClassDiscovery: className = "
                            + className);
                    try {
                        classes.add(Class.forName(className));
                    } catch (ClassNotFoundException e) {
                        throw new RuntimeException(
                                "ClassNotFoundException loading " + className);
                    }
                }
            }
        } else {
            try {
                String jarPath = fullPath.replaceFirst("[.]jar[!].*", ".jar")
                        .replaceFirst("file:", "");
                JarFile jarFile = new JarFile(jarPath);
                Enumeration<JarEntry> entries = jarFile.entries();
                while (entries.hasMoreElements()) {
                    JarEntry entry = entries.nextElement();
                    String entryName = entry.getName();
                    if (entryName.startsWith(relPath)
                            && entryName.length() > (relPath.length() + "/"
                                    .length())) {
                        System.out.println("ClassDiscovery: JarEntry: "
                                + entryName);
                        String className = entryName.replace('/', '.')
                                .replace('\\', '.').replace(".class", "");
                        System.out.println("ClassDiscovery: className = "
                                + className);
                        try {
                            classes.add(Class.forName(className));
                        } catch (ClassNotFoundException e) {
                            throw new RuntimeException(
                                    "ClassNotFoundException loading "
                                            + className);
                        }
                    }
                }
            } catch (IOException e) {
                throw new RuntimeException(pkgname + " (" + directory
                        + ") does not appear to be a valid package", e);
            }
        }
        return classes;
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

}
