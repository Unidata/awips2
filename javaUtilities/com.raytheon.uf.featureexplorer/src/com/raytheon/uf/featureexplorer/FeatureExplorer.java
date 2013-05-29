package com.raytheon.uf.featureexplorer;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
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
 * </pre>
 * 
 * @author dglazesk
 * @version 1.0
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

	private Map<String, File> pluginLookupMap = new HashMap<String, File>();

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
	public ArrayList<File> getPlugins() throws FeatureException {
		return getPlugins(feature);
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
	public ArrayList<File> getPlugins(String aPath) throws FeatureException {
		File feat = new File(aPath);
		return getPlugins(feat);
	}

	/**
	 * This function attempts to find all of the plugins associated with the
	 * feature. This includes recursing into any included features and grabbing
	 * their plugins.
	 * 
	 * @param aFeature
	 *            The file object for the feature to be scanned
	 * @return The list of file objects for the located plugins
	 * @throws FeatureException
	 */
	public ArrayList<File> getPlugins(File aFeature) throws FeatureException {
		ArrayList<File> rval = new ArrayList<File>();

		HashMap<String, File> plugins = getFeaturePlugins(aFeature);
		rval = new ArrayList<File>(plugins.values());

		return rval;
	}

	/**
	 * This finds all of the plugins listed in a feature and maps their ids to
	 * their file system locations as file objects. This is the function that
	 * does the brunt of the work in locating the plugins.
	 * 
	 * @param aFile
	 *            The feature file that is being scanned
	 * @return A hash map that links the plugin id to its file system location
	 * @throws FeatureException
	 *             If there are any problems with JAXB, a feature cannot be
	 *             found, or a plugin cannot be found
	 */
	protected HashMap<String, File> getFeaturePlugins(File aFile)
			throws FeatureException {
		HashMap<String, File> rval = new HashMap<String, File>();
		if (aFile == null || !aFile.exists() || !aFile.canRead())
			return rval;

		Feature feat = null;
		try {
			JAXBContext jc = JAXBContext.newInstance(Feature.class);
			Unmarshaller unmarshaller = jc.createUnmarshaller();
			feat = (Feature) unmarshaller.unmarshal(aFile);
		} catch (Exception e) {
			throw new FeatureException("Unable to unmarshal file " + aFile, e);
		}

		for (Includes include : feat.getIncludes()) {
			// go through all of the included features and try to find them
			List<File> features = featureSearch.findFeature(include.getId(),
					include.getVersion());
			try {
				// get all of the plugin id to file objects and add them
				rval.putAll(getFeaturePlugins(features.get(0)));
			} catch (IndexOutOfBoundsException e) {
				if (!include.getOptional()) {
					// this means we received an empty list, no feature found
					throw new FeatureException("Could not find feature "
							+ include.getId() + " with version greater than "
							+ include.getVersion());
				}
			}
		}

		for (Plugin plugin : feat.getPlugins()) {
			// go through all of the mentioned plugins
			List<File> plugs = pluginSearch.findPlugin(plugin.getId(),
					plugin.getVersion());
			try {
				if (this.pluginLookupMap.containsKey(plugin.getId())
						&& !this.pluginLookupMap.get(plugin.getId()).equals(aFile)) {
					StringBuilder stringBuilder = new StringBuilder("Plugin ");
					stringBuilder.append(plugin.getId());
					stringBuilder.append(" is in Feature ");
					stringBuilder.append(this.generateFeatureFileName(
							aFile.getParent(), aFile.getName()));
					stringBuilder.append(" and Feature ");
					stringBuilder
							.append(this.generateFeatureFileName(
									this.pluginLookupMap.get(plugin.getId())
											.getParent(), this.pluginLookupMap
											.get(plugin.getId()).getName()));
					stringBuilder.append("!");
					throw new FeatureException(stringBuilder.toString());
				}

				// add the plugin id and its file object to the map
				rval.put(plugin.getId(), plugs.get(0));
				this.pluginLookupMap.put(plugin.getId(), aFile);
			} catch (IndexOutOfBoundsException e) {
				// this means we received an empty list, no plugin found
				throw new FeatureException("Could not find plugin "
						+ plugin.getId() + " with version greater than "
						+ plugin.getVersion());
			}
		}

		return rval;
	}

	private String generateFeatureFileName(String parentPath, String fileName) {
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
		List<String> rval = new ArrayList<String>();

		try {
			m = new Manifest();
			// we only care if the manifest actually exists
			InputStream is = new FileInputStream(maniFile);
			m.read(is);
			is.close();

		} catch (IOException e) {
			throw new IOException(
					"IO Error while reading manifest for project: "
							+ projectRoot.getName());
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
