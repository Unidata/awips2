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
package com.raytheon.uf.docs.datauri;

import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.reflections.Reflections;
import org.reflections.scanners.SubTypesScanner;
import org.reflections.util.ClasspathHelper;
import org.reflections.util.ConfigurationBuilder;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;

/**
 * Outputs the dataURI field names related to all plugins by using the
 * reflections plugin to find subclasses of PluginDataObject.
 * 
 * If running this on future builds, you need to make sure it has knowledge of
 * the plugins and packages that a subclass of PluginDataObject might reside in.
 * 
 * Specifically plugins are added to the Eclipse build path by right clicking
 * this class's plugin project and editing the build path to include other
 * projects in the workspace that may contain PluginDataObjects.
 * 
 * Package names that are scanned within those plugins are in the constant
 * PKG_NAMES below.
 * 
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 27, 2014            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class DataURIOutputter {

    private static final String[] PKG_NAMES = { "com.raytheon", "gov.noaa" };

    private static final DataURIAnnotationComparator fieldCompare = new DataURIAnnotationComparator();

    protected Writer writer;

    public DataURIOutputter(Writer writer) {
        this.writer = writer;
    }

    /**
     * Main method that actually runs the tool
     * 
     * @param args
     * @throws Exception
     */
    public static void main(String[] args) throws Exception {
        // could change it to write to a file if we desire
        StringWriter writer = new StringWriter();
        DataURIOutputter tool = new DataURIOutputter(writer);
        tool.run();
        System.out.println(writer.toString());
    }

    /**
     * Finds classes that extend PluginDataObject and analyzes their dataURI
     * annotations to produce output for the writer.
     * 
     * @throws IOException
     */
    public void run() throws IOException {
        Set<Class<? extends PluginDataObject>> clzSet = findPDOs();
        List<Class<? extends PluginDataObject>> clzList = new ArrayList<Class<? extends PluginDataObject>>(
                clzSet);

        List<String> pluginNames = new ArrayList<String>(clzList.size());
        Map<String, Class<?>> map = new HashMap<String, Class<?>>();
        for (Class<? extends PluginDataObject> clz : clzList) {
            if (!Modifier.isAbstract(clz.getModifiers())
                    && !Modifier.isPrivate(clz.getModifiers()))
                try {
                    PluginDataObject pdo = clz.newInstance();
                    String pluginName = pdo.getPluginName();
                    pluginNames.add(pluginName);
                    map.put(pluginName, clz);
                } catch (Exception e) {
                    System.err.println("Error instantiating " + clz.getName());
                    e.printStackTrace();
                }

        }

        // sort the plugins alphabetically
        Collections.sort(pluginNames);

        // produce the output
        for (String plugin : pluginNames) {
            writer.append(DataURI.SEPARATOR);
            writer.append(plugin);
            writer.append(DataURI.SEPARATOR);
            processFields(findDataUriFields(map.get(plugin)));
            writer.append("\n");
        }

    }

    /**
     * Finds the classes that extend PluginDataObject
     * 
     * @return
     */
    private Set<Class<? extends PluginDataObject>> findPDOs() {
        ConfigurationBuilder cb = new ConfigurationBuilder();
        for (String pkg : PKG_NAMES) {
            cb.addUrls(ClasspathHelper.forPackage(pkg));
        }
        cb.setScanners(new SubTypesScanner());
        Reflections refl = cb.build();
        return refl.getSubTypesOf(PluginDataObject.class);
    }

    /**
     * Walks up the class inheritance tree and over the class's member variables
     * looking for @DataURI annotations and returns the corresponding fields in
     * proper order.
     * 
     * @param clz
     *            the class to find @DataURI fields
     * @return
     */
    private List<Field> findDataUriFields(Class<?> clz) {
        Class<?> superClz = clz.getSuperclass();
        List<Field> superFields = null;
        if (!superClz.equals(Object.class)) {
            superFields = findDataUriFields(superClz);
        } else {
            superFields = new ArrayList<Field>();
        }

        List<Field> thisFields = new ArrayList<Field>();
        for (Field field : clz.getDeclaredFields()) {
            if (field.isAnnotationPresent(DataURI.class)) {
                thisFields.add(field);
            }
        }

        /*
         * sorting only applies to this particular class, as a position's
         * numeric value can repeat in a super class
         */
        Collections.sort(thisFields, fieldCompare);

        // super fields always come first
        superFields.addAll(thisFields);
        return superFields;
    }

    /**
     * Loops through the fields and appends corresponding output to the writer
     * 
     * @param fields
     *            fields that have @DataURI annotations
     * @throws IOException
     */
    private void processFields(List<Field> fields) throws IOException {
        Iterator<Field> itr = fields.iterator();
        while (itr.hasNext()) {
            Field f = itr.next();
            DataURI dataURI = f.getAnnotation(DataURI.class);
            if (!dataURI.embedded()) {
                writer.append(f.getName());
            } else {
                processFields(findDataUriFields(f.getType()));
            }
            if (itr.hasNext()) {
                writer.append(DataURI.SEPARATOR);
            }
        }
    }

}
