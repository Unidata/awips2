package com.raytheon.uf.featureexplorer;

import java.io.File;
import java.io.FileFilter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * This class is to be used for indexing sets of manifests that are found in a
 * plug-in project for Eclipse.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 3, 2008             bclement    Initial creation
 * Oct 6, 2008             dglazesk    Added a function that adds a directory to
 *                                     the index
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
abstract public class AbstractResourceIndex {

    /**
     * File object for the base directory.
     */
    protected File baseDir;

    /**
     * Structure that houses the information.
     * 
     * index[id][ver] == path to directory with id and version ver
     */
    protected Map<String, Map<String, File>> index;

    /**
     * Create the index based on a base directory. The base directory must
     * exist.
     * 
     * @param aBaseDirectory
     *            The base directory where all the projects are subdirectories
     */
    public AbstractResourceIndex(File aBaseDirectory) {
        index = new HashMap<String, Map<String, File>>();
        indexDirectory(aBaseDirectory);
    }

    public AbstractResourceIndex(Collection<File> aBaseDirectories) {
        index = new HashMap<String, Map<String, File>>();
        for (File file : aBaseDirectories) {
            indexDirectory(file);
        }
    }

    /**
     * Adds a particular directory to the index. It doesn't add anything if it
     * is an invalid directory (doesn't exist | it's not a directory | it isn't
     * readable).
     * 
     * @param aBaseDirectory
     *            Base directory to be added to the index
     */
    public void indexDirectory(File aBaseDirectory) {
        if (!aBaseDirectory.exists() || !aBaseDirectory.isDirectory()
                || !aBaseDirectory.canRead())
            return;

        baseDir = aBaseDirectory;
        File[] directories = getSubDirectories();
        for (File dir : directories) {
            catalog(dir);
        }
    }

    /**
     * This function does all of the work. It creates entries in the map for the
     * IDs and the version numbers for the project into the map.
     * 
     * @param projectDirectory
     *            Directory for the project to look into
     */
    abstract protected void catalog(File projectDirectory);

    /**
     * Finds the sub-directories of the base directory.
     * 
     * @return The array of sub-directories for the base directory
     */
    public File[] getSubDirectories() {
        return baseDir.listFiles(new FileFilter() {
            /**
             * This function accepts if the file is a directory.
             * 
             * @param f
             *            The file being checked for acceptance
             * @return True if the file is a directory
             */
            @Override
            public boolean accept(File f) {
                return f.isDirectory();
            }
        });
    }

    /**
     * Returns the base directory for this index.
     * 
     * @return File object representing the base for the index
     */
    public File getBaseDirectory() {
        return this.baseDir;
    }

    /**
     * Returns the map of items that maps the version number to the location on
     * the filesystem for the desired project.
     * 
     * @param anId
     * @return
     */
    public Map<String, File> getVersions(String anId) {
        return index.get(anId);
    }

    /**
     * Queries the index in search of a particular ID with an associated version
     * that is greater than aVersion. This will return an empty list if the
     * query results in an empty set.
     * 
     * @param anId
     *            The ID of the plugin being searched for
     * @param aVersion
     *            The minimum version of the plugin
     * @return List of file objects representing the locations of the results of
     *         the query in descending order based on version
     */
    public List<File> query(String anId, String aVersion) {
        List<File> rval = new ArrayList<File>();
        // get the versions associated with this id
        Map<String, File> verFileMap = this.getVersions(anId);

        if (verFileMap != null) {
            // if we have something, look for all of the versions that are
            // greater than aVersion
            Version base = new Version(aVersion);
            List<Version> versions = new ArrayList<Version>();
            for (String version : verFileMap.keySet()) {
                Version cur = new Version(version);
                if (base.compareTo(cur) <= 0) {
                    versions.add(cur);
                }
            }

            // sort the list of versions and create the return list
            Collections.sort(versions, new ReverseVersionComparator());
            for (Version version : versions) {
                rval.add(verFileMap.get(version.toString()));
            }
        }

        return rval;
    }
}
