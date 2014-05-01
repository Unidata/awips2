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
package com.raytheon.uf.viz.core.reflect;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Writer;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.regex.Pattern;

import org.osgi.framework.Bundle;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Provides a file based cache of all types available for each installed plugin.
 * Each type can either be a super class and all sub classes or an annotation
 * class and all classes with that annotation.
 * 
 * JAXB/XML was not used because it was 20x slower and every ms counts.
 * 
 * The file format uses space separated fields. Each bundle is described on a
 * Bundle line which includes the name, version, bundleId, and mod time. After a
 * Bundle line there will be multiple "Type" lines, each describing one base
 * class that has been searched within that bundle. When subclasses or annotated
 * classes are found they will be appended to the type line, space separated.
 * For must types/bundles there will be no subclasses but the type line is
 * included so that the bundle is not rescanned and so the set of base classes
 * can remain dynamic.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Oct 21, 2013  2491     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class BundleClassCache {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(BundleClassCache.class);

    private static final String BUNDLE = "Bundle";

    private static final String TYPE = "Type";

    private static final char SEPERATOR = ' ';

    private static final Pattern SEPERATOR_SPLIT = Pattern.compile(" ");

    private final File file;

    private final Map<PluginKey, SubTypeNameMap> pluginMap = new HashMap<PluginKey, SubTypeNameMap>();

    private boolean modified = false;

    /**
     * Create a new cache using the provided file. If the file exists it is read
     * in to prepopulate the cache.
     * 
     * @param file
     */
    public BundleClassCache(File file) {
        this.file = file;
        if (file.exists()) {
            try {
                BufferedReader r = new BufferedReader(new FileReader(file));
                PluginKey key = null;
                SubTypeNameMap value = new SubTypeNameMap();

                String line = r.readLine();
                while (line != null) {
                    line = line.trim();
                    if (line.startsWith(BUNDLE)) {
                        key = new PluginKey(line);
                        value = new SubTypeNameMap();
                        pluginMap.put(key, value);
                    } else if (line.startsWith(TYPE)) {
                        value.addType(line);
                    }
                    line = r.readLine();
                }
                r.close();
            } catch (IOException e) {
                statusHandler.handle(Priority.VERBOSE, e.getLocalizedMessage(),
                        e);
            }
        }
    }

    /**
     * Get all types in a bundles that extend the provided base class.
     * 
     * @param bundle
     *            bundle to search
     * @param baseClassName
     *            name of a base class
     * @return names of all subclasses(possible empty), or null if the bundle
     *         does not have a cache entry for the base class.
     */
    public String[] getTypes(Bundle bundle, String baseClassName) {
        SubTypeNameMap map = pluginMap.get(new PluginKey(bundle));
        if (map == null) {
            return null;
        }
        return map.getSubTypes(baseClassName);
    }

    /**
     * Add the type to the cache.
     * 
     * @param bundle
     *            the bundle that was scanned.
     * @param baseClassName
     *            name of base class
     * @param subTypes
     *            all subtypes, can be length 0 but not null.
     */
    public void putTypes(Bundle bundle, String baseClassName, String[] subTypes) {
        modified = true;
        PluginKey key = new PluginKey(bundle);
        SubTypeNameMap map = pluginMap.get(key);
        if (map == null) {
            map = new SubTypeNameMap();
            pluginMap.put(key, map);
        }
        map.putSubTypes(baseClassName, subTypes);
    }

    /**
     * Peresist the contents of the cache to the file used during construction.
     * If any {@link IOException}s occur they will be logged but not returned.
     */
    public void save() {
        if (modified) {
            try {
                File tmpFile = File.createTempFile("tmpClassCache", ".txt",
                        file.getParentFile());
                Writer w = new BufferedWriter(new FileWriter(tmpFile));
                for (Entry<PluginKey, SubTypeNameMap> e : pluginMap.entrySet()) {
                    e.getKey().save(w);
                    e.getValue().save(w);
                }
                w.close();
                tmpFile.renameTo(file);
            } catch (IOException e) {
                statusHandler.handle(Priority.VERBOSE, e.getLocalizedMessage(),
                        e);
            }
        }
    }

    /**
     * Cache entry, containing all base/subtype mappings.
     */
    private static class SubTypeNameMap {
        private final Map<String, String[]> typeMap = new HashMap<String, String[]>();

        public void addType(String line) {
            String[] parts = SEPERATOR_SPLIT.split(line);
            String base = parts[1];
            String[] subTypes = Arrays.copyOfRange(parts, 2, parts.length);
            typeMap.put(base, subTypes);
        }

        public String[] getSubTypes(String className) {
            return typeMap.get(className);
        }

        public void putSubTypes(String className, String[] subTypes) {
            typeMap.put(className, subTypes);
        }

        public void save(Writer w) throws IOException {
            for (Entry<String, String[]> e : typeMap.entrySet()) {
                w.write(TYPE);
                w.write(SEPERATOR);
                w.write(e.getKey());
                for (String val : e.getValue()) {
                    w.write(SEPERATOR);
                    w.write(val);
                }
                w.write('\n');
            }
        }

    }

    /**
     * Key for uniquely identifing a plugin.
     */
    private static class PluginKey {

        /*
         * On development boxes the modtime changes every time CAVE is run, so
         * it is not used in comparisons by default.
         */
        private static final boolean useModTime = false;

        /*
         * The unique ID is not strictly needed for comparisons since
         * name/version should be unique already. It is kept in just in case
         * there is an unaccounted for edge case.
         */
        private static final boolean useUnique = true;

        private final String name;

        private final String version;

        private final long unique;

        private final long modTime;

        public PluginKey(String line) {
            String[] split = SEPERATOR_SPLIT.split(line);
            name = split[1];
            version = split[2];
            unique = Long.parseLong(split[3]);
            modTime = Long.parseLong(split[4]);
        }

        public PluginKey(Bundle bundle) {
            name = bundle.getSymbolicName();
            version = bundle.getVersion().toString();
            unique = bundle.getBundleId();
            modTime = bundle.getLastModified();
        }

        public void save(Writer w) throws IOException {
            w.write(BUNDLE);
            w.write(SEPERATOR);
            w.write(name);
            w.write(SEPERATOR);
            w.write(version);
            w.write(SEPERATOR);
            w.write(Long.toString(unique));
            w.write(SEPERATOR);
            w.write(Long.toString(modTime));
            w.write('\n');
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            if (useModTime)
                result = prime * result + (int) (modTime ^ (modTime >>> 32));
            if (useUnique)
                result = prime * result + (int) (unique ^ (unique >>> 32));
            result = prime * result + ((name == null) ? 0 : name.hashCode());
            result = prime * result
                    + ((version == null) ? 0 : version.hashCode());
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null)
                return false;
            if (getClass() != obj.getClass())
                return false;
            PluginKey other = (PluginKey) obj;

            if (useModTime)
                if (modTime != other.modTime)
                    return false;
            if (name == null) {
                if (other.name != null)
                    return false;
            } else if (!name.equals(other.name))
                return false;
            if (useUnique)
                if (unique != other.unique)
                    return false;
            if (version == null) {
                if (other.version != null)
                    return false;
            } else if (!version.equals(other.version))
                return false;
            return true;
        }

    }
}
