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
package com.raytheon.uf.featureexplorer;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.jar.Attributes;
import java.util.jar.Manifest;
import java.util.Map;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.Unmarshaller;

import com.raytheon.uf.featureexplorer.jaxb.Feature;
import com.raytheon.uf.featureexplorer.jaxb.Includes;
import com.raytheon.uf.featureexplorer.jaxb.Plugin;
import com.raytheon.uf.featureexplorer.search.IFeatureSearch;
import com.raytheon.uf.featureexplorer.search.IPluginSearch;

/**
 * This represents an interface for a user to search through a feature for
 * plugins. This class utilizes other interfaces to do the actual searches, but
 * handles the bundling of the final list as well as the recursion into included
 * features.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 3, 2008             dglazesk    Initial creation
 * Oct 7, 2008  SP#15      bclement    Added FeatureException code
 * Oct 7, 2008  SP#15      bclement    Changed ArrayList references to List
 * Oct 10, 2008 SP#15      bclement    Added static functions for reading manifests
 * Feb 4, 2013  #1577      bkowal      Verify that a plugin has not been included in more than one feature.
 * May 22, 2013 #1927      bkowal      Fix improper String comparison
 * Feb 25, 2015 #3299      garmendariz Process a list of included features
 * Jun 16, 2016 #5694      bkowal      Include plugins that are in features required by a feature.
 * Sep 12, 2016 #5833      garmendariz Check list of processed features on recursive call
 * Mar 22, 2017 #5894      dlovely     Removed processing of features required by a feature.
 * Dec 19, 2018 #7679      lsingh      Added workaround to JAXBContext call to get correct classpath.
 * </pre>
 * 
 * @author dglazesk
 */
public class FeatureExplorer {
    /**
     * This holds the feature searching object for the class.
     */
    protected IFeatureSearch featureSearch;

    /**
     * This holds the plugin searching object for the class.
     */
    protected IPluginSearch pluginSearch;

    /**
     * This holds the feature file object should the user choose to set it.
     */
    protected File feature = null;

    private Map<String, File> pluginLookupMap = new HashMap<>();

    private HashSet<File> processedList = new HashSet<>();

    /**
     * This constructor allows a user to setup what the feature for this
     * instance is. This allows the user to use getPlugins(), though the other
     * methods are still available.
     * 
     * @param aFeature
     *            File object for the feature for this instance
     * @param aFeatureSearch
     *            The object that can search for features
     * @param aPluginSearh
     *            The object that can search for plugins
     */
    public FeatureExplorer(File aFeature, IFeatureSearch aFeatureSearch,
            IPluginSearch aPluginSearh) {
        featureSearch = aFeatureSearch;
        pluginSearch = aPluginSearh;
        feature = aFeature;
    }

    /**
     * This constructor sets up the classes that will be used for searching for
     * plugins and features. It is expected that a user will use
     * getPlugins(File) or getPlugins(String) later when getting the plugins.
     * 
     * @param aFeatureSearch
     *            The object that can search for features
     * @param aPluginSearh
     *            The object that can search for plugins
     */
    public FeatureExplorer(IFeatureSearch aFeatureSearch,
            IPluginSearch aPluginSearh) {
        featureSearch = aFeatureSearch;
        pluginSearch = aPluginSearh;
    }

    /**
     * This is a convenience method for when the feature file object is set in a
     * constructor.
     * 
     * @return The list of files in the feature for this instance
     * @throws FeatureException
     */
    public List<File> getPlugins() throws FeatureException {
        return getPlugins(feature, null);
    }

    /**
     * This is just a convenience method for getting plugins from a feature.
     * This is equivalent to doing getPlugins(new File(aPath)).
     * 
     * @param aPath
     *            Path to the feature.xml file to be scanned
     * @return The list of file objects for all of the plugins in the feature
     * @throws FeatureException
     */
    public List<File> getPlugins(String aPath, List<File> incList)
            throws FeatureException {
        File feat = new File(aPath);
        return getPlugins(feat, incList);
    }

    /**
     * This function attempts to find all of the plugins associated with the
     * feature. This includes recursing into any included features and grabbing
     * their plugins.
     * 
     * @param aFeature
     *            The file object for the feature to be scanned
     * @param addlFeatureFiles
     * @return The list of file objects for the located plugins
     * @throws FeatureException
     */
    public List<File> getPlugins(File aFeature, List<File> addlFeatureFiles)
            throws FeatureException {
        ArrayList<File> rval = new ArrayList<>();

        Map<String, File> plugins = getFeaturePlugins(aFeature,
                addlFeatureFiles);
        rval = new ArrayList<>(plugins.values());

        return rval;
    }

    /**
     * This finds all of the plugins listed in a feature and maps their ids to
     * their file system locations as file objects. This is the function that
     * does the brunt of the work in locating the plugins.
     * 
     * @param aFile
     *            The feature file that is being scanned
     * @param incList
     * @return A hash map that links the plugin id to its file system location
     * @throws FeatureException
     *             If there are any problems with JAXB, a feature cannot be
     *             found, or a plugin cannot be found
     */
    protected Map<String, File> getFeaturePlugins(File aFile,
            List<File> incList) throws FeatureException {
        if (aFile == null || !aFile.exists() || !aFile.canRead()
                || processedList.contains(aFile)) {
            return Collections.emptyMap();
        }

        HashMap<String, File> rval = new HashMap<>();

        Feature feat = null;
        try {
            // Workaround because javax.xml.bind calls com.sun.xml.bind
            // using a Thread classloader.
            // The Thread classloader does NOT inherit the ANT classpath when
            // FeatureExplorer runs directly from the deploy-install script,
            // However, we can pass the Class classloader into the thread
            // classloader.
            // as a workaround.
            ClassLoader cl = this.getClass().getClassLoader();
            Thread.currentThread().setContextClassLoader(cl);

            JAXBContext jc = JAXBContext.newInstance(Feature.class);
            Unmarshaller unmarshaller = jc.createUnmarshaller();
            feat = (Feature) unmarshaller.unmarshal(aFile);
        } catch (Exception e) {
            throw new FeatureException("Unable to unmarshal file " + aFile, e);
        }

        // if additional includes are passed in, add them for processing
        if (incList != null && feat != null) {
            List<Includes> featList = feat.getIncludes();

            for (Iterator<File> iterator = incList.iterator(); iterator
                    .hasNext();) {
                File incFile = (File) iterator.next();
                featList.add(new Includes(incFile.getName(), "0.0.0", false));
            }

        }

        for (Includes include : feat.getIncludes()) {
            // go through all of the included features and try to find them
            List<File> features = featureSearch.findFeature(include.getId(),
                    include.getVersion());
       
            //Check for an empty list, which means no feature was found.
            if (features.isEmpty()) {               
                if (!include.getOptional()) {
                    throw new FeatureException("Could not find feature "
                            + include.getId() + " with version greater than "
                            + include.getVersion());
                } 
            }
                
            // get all of the plugin id to file objects and add them
            rval.putAll(getFeaturePlugins(features.get(0), null));
            
        }

        for (Plugin plugin : feat.getPlugins()) {
            // go through all of the mentioned plugins
            List<File> plugs = pluginSearch.findPlugin(plugin.getId(),
                    plugin.getVersion());
    
            if (this.pluginLookupMap.containsKey(plugin.getId())
                    && !this.pluginLookupMap.get(plugin.getId())
                            .equals(aFile)) {
                StringBuilder stringBuilder = new StringBuilder("Plugin ");
                stringBuilder.append(plugin.getId());
                stringBuilder.append(" is in Feature ");
                stringBuilder
                        .append(FeatureExplorer.generateFeatureFileName(
                                aFile.getParent(), aFile.getName()));
                stringBuilder.append(" and Feature ");
                stringBuilder
                        .append(FeatureExplorer.generateFeatureFileName(
                                this.pluginLookupMap.get(plugin.getId())
                                        .getParent(),
                                this.pluginLookupMap.get(plugin.getId())
                                        .getName()));
                stringBuilder.append("!");
                throw new FeatureException(stringBuilder.toString());
            }

            //check for an empty list in case no plugin was found
            if( plugs.isEmpty() ) {
                throw new FeatureException("Could not find plugin "
                        + plugin.getId() + " with version greater than "
                        + plugin.getVersion());
            }
            
            // add the plugin id and its file object to the map
            rval.put(plugin.getId(), plugs.get(0));
            this.pluginLookupMap.put(plugin.getId(), aFile);
 
        }

        processedList.add(aFile);
        System.out.println("Processed: " + aFile);

        return rval;
    }

    private static String generateFeatureFileName(String parentPath,
            String fileName) {
        String[] pathElements = parentPath.split(File.separator);
        return pathElements[pathElements.length - 1] + File.separator
                + fileName;
    }

    /**
     * Searches a project's manifest for a specific attribute. The returned list
     * will contain all values for the attribute or empty if not found.
     * 
     * @param projectRoot
     * @param attrib
     * @return a list of a values for the attribute or an empty list if not
     *         found
     * @throws IOException
     */
    public static List<String> readManifest(File projectRoot, String attrib)
            throws IOException {
        File maniFile = new File(projectRoot, "/META-INF/MANIFEST.MF");
        Manifest m = null;
        List<String> rval = new ArrayList<>();

        try {
            m = new Manifest();
            // we only care if the manifest actually exists
            InputStream is = new FileInputStream(maniFile);
            m.read(is);
            is.close();

        } catch (IOException e) {
            throw new IOException(
                    "IO Error while reading manifest for project: "
                            + projectRoot.getName(), e);
        }

        // if we get this far, m shouldn't be null
        if (m != null) {
            Attributes attribs = m.getMainAttributes();
            String deploys = attribs.getValue(attrib);

            // manifests that do not have a deploy entry will return a wildcard
            if (deploys != null) {
                for (String s : deploys.split(",")) {
                    rval.add(s.trim());
                }
            }
        }
        return rval;
    }

    /**
     * Reads the manifest for the project and returns all values for the
     * "Edex-Deploy" attribute. If the attribute could not be found, default to
     * returning a wildcard for all jars.
     * 
     * @param projectRoot
     * @return a list of jar names or a wildcard for all jars if attribute not
     *         found
     * @throws IOException
     */
    public static List<String> getJars(File projectRoot) throws IOException {
        List<String> rval = readManifest(projectRoot, "Edex-Deploy");
        if (rval.isEmpty()) {
            rval.add("*.jar");
        }
        return rval;
    }
}
