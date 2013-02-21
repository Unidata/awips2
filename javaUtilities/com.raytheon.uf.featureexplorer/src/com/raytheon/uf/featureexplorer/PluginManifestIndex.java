package com.raytheon.uf.featureexplorer;

import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.jar.Attributes;
import java.util.jar.JarFile;
import java.util.jar.Manifest;

/**
 * This class is to be used for indexing sets of manifests that are found in a
 * plug-in project for Eclipse.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 3, 2008             bclement    Initial creation
 * Oct 6, 2008             dglazesk    Added support for indexing the jar files.
 *                                     This just helps with Eclipse plugin indexing.
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class PluginManifestIndex extends AbstractResourceIndex {

    /**
     * Create the index based on a base directory. The base directory must
     * exist.
     * 
     * @param aBaseDirectory
     *            The base directory where all the plugins are subdirectories
     * @throws IOException
     *             Thrown if the directory does not exist or is not a directory.
     */
    public PluginManifestIndex(Collection<File> aBaseDirectories)
            throws IOException {
        super(aBaseDirectories);
    }

    /**
     * This function does all of the work. It looks for a manifest in the
     * "META-INF" folder in the project directory and creates entries in the map
     * for the IDs and the version numbers for the project into the map.
     * 
     * @param projectDirectory
     *            Directory for the project to look into
     */
    protected void catalog(File projectDirectory) {
        File maniFile = new File(projectDirectory, "META-INF/MANIFEST.MF");
        Manifest m = null;

        try {
            if (projectDirectory.isFile()) {
                // regular file means it may be a jar, so try it
                JarFile proj = new JarFile(projectDirectory);
                m = proj.getManifest();
            } else if (maniFile.exists()) {
                m = new Manifest();
                // we only care if the manifest actually exists
                InputStream is = new FileInputStream(maniFile);
                m.read(is);
                is.close();
            }
        } catch (IOException e) {
            e.printStackTrace();
        }

        // if m is null, we either didn't have the manifest or there was an
        // exception, skip
        if (m != null) {
            Attributes attribs = m.getMainAttributes();
            String id = attribs.getValue("Bundle-SymbolicName");
            String version = attribs.getValue("Bundle-Version");

            // ignore manifests that do not have a bundle name and version
            if (id != null && version != null) {
                if (index.containsValue(id)) {
                    index.get(id).put(version, projectDirectory);
                } else {
                    Map<String, File> versionMap = new HashMap<String, File>();
                    versionMap.put(version, projectDirectory);
                    index.put(id, versionMap);
                }
            }
        }
    }

    /**
     * Finds the sub-directories and any jar files in the base directory. The
     * plugin manifest index needs to support indexing the plug-in jars as well.
     * 
     * @return The array of sub-directories or jars in the base directory
     */
    @Override
    public File[] getSubDirectories() {
        return baseDir.listFiles(new FileFilter() {
            /**
             * This function accepts if the file is a directory or is a jar
             * file.
             * 
             * @param f
             *            The file being checked for acceptance
             * @return True if the file is a directory or a jar
             */
            @Override
            public boolean accept(File f) {
                return f.isDirectory() || f.getName().endsWith(".jar");
            }
        });
    }
}
