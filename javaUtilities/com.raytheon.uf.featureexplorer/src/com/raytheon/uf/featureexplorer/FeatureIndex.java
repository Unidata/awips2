package com.raytheon.uf.featureexplorer;

import java.io.File;
import java.io.IOException;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import com.raytheon.uf.featureexplorer.jaxb.Feature;

/**
 * This class represents an index of feature.xml files in a certain directory.
 * This also has querying abilities thanks to the abstract class.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 6, 2008             bclement    Initial creation
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public class FeatureIndex extends AbstractResourceIndex {

    /**
     * Needed constructor that sets the base directory for the index.
     * 
     * @param aBaseDirectory
     *            File object for the base directory to start the index in
     * @throws IOException
     */
    public FeatureIndex(Collection<File> aBaseDirectories) throws IOException {
        super(aBaseDirectories);
    }

    /**
     * This function does all of the work. It looks for a feature in the project
     * directory and creates entries in the map for the IDs and the version
     * numbers for the project into the map.
     * 
     * @param projectDirectory
     *            Directory for the project to look into
     * @see AbstractResourceIndex#catalog(File)
     */
    protected void catalog(File projectDirectory) {
        File featureFile = new File(projectDirectory, "feature.xml");

        if (featureFile.exists()) {
            try {
                // using JAXB to do the unmarshalling of feature.xml
                JAXBContext jc = JAXBContext.newInstance(Feature.class);
                Unmarshaller unmarshaller = jc.createUnmarshaller();
                Feature feat = (Feature) unmarshaller.unmarshal(featureFile);

                String id = feat.getId();
                String version = feat.getVersion();

                // ignore features that do not have a bundle name and version
                if (id != null && version != null) {
                    if (index.containsValue(id)) {
                        index.get(id).put(version, projectDirectory);
                    } else {
                        Map<String, File> versionMap = new HashMap<String, File>();
                        versionMap.put(version, featureFile);
                        index.put(id, versionMap);
                    }
                }
            } catch (JAXBException e) {
                e.printStackTrace();
            }

        }
    }

}
