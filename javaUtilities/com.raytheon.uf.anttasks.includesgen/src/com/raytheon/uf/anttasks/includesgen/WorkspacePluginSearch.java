package com.raytheon.uf.anttasks.includesgen;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.featureexplorer.PluginManifestIndex;
import com.raytheon.uf.featureexplorer.search.IPluginSearch;

/**
 * Plugin search that is used in the FeatureExplorer class as a custom way to
 * search for plugins.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 3, 2008             bclement    Initial creation
 * Jan 5, 2013  #1577      bkowal      changed basedirs declaration to List from ArrayList
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class WorkspacePluginSearch implements IPluginSearch {

	private List<File> baseDirs;

	public WorkspacePluginSearch(File base) {
		this.baseDirs = new ArrayList<File>(1);
		this.baseDirs.add(base);
	}

	public WorkspacePluginSearch(ArrayList<File> baseDirs) {
		this.baseDirs = baseDirs;
	}

	@Override
	/**
	 * Finds a list of plugin locations in which all have the ID anId and whose
	 * version is greater than or equal to aVersion. The location with the
	 * latest version will be at index 0 in the list.
	 * 
	 */
	public List<File> findPlugin(String anId, String aVersion) {
		List<File> rval = new ArrayList<File>();
		try {
			PluginManifestIndex index = new PluginManifestIndex(baseDirs);
			rval = index.query(anId, aVersion);
		} catch (IOException e) {
			e.printStackTrace();
		}
		return rval;
	}

}
