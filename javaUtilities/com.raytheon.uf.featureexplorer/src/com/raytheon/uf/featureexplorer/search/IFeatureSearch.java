package com.raytheon.uf.featureexplorer.search;

import java.io.File;
import java.util.List;

/**
 * This interface is used for creating objects that are capable of searching for
 * a feature.
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
public interface IFeatureSearch {
    /**
     * This will search for a feature that has anId as its ID and has a version
     * greater than or equal to aVersion. The final list will be ordered by
     * version number with the highest version number at element 0.
     * 
     * @param anId
     *            ID of the feature being searched for
     * @param aVersion
     *            Version minimum for the feature
     * @return List of file objects for the feature.xml that match the search
     *         criteria
     */
    public List<File> findFeature(String anId, String aVersion);
}
