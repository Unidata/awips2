package com.raytheon.uf.featureexplorer.search;

import java.io.File;
import java.util.List;

/**
 * This interface is used for creating objects that are capable of searching for
 * a plugins.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 3, 2008             dglazesk    Initial creation
 * Oct 7, 2008  SP#15      bclement    changed return value to List
 * </pre>
 * 
 * @author dglazesk
 * @version 1.0
 */
public interface IPluginSearch {
    /**
     * This will search for a plugin with anId as its ID and has a version
     * number greater than or equal to aVersion. The final list will be ordered
     * by version number with the highest version at element 0.
     * 
     * @param anId
     *            The ID of the plugin being searched for
     * @param aVersion
     *            The minimum version of the plugin
     * @return List of file locations for the plugins matching the search
     *         criteria
     */
    public List<File> findPlugin(String anId, String aVersion);
}
