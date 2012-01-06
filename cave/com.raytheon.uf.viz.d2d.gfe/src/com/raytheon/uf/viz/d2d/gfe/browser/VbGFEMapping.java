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
package com.raytheon.uf.viz.d2d.gfe.browser;

import java.io.File;
import java.util.List;

import javax.xml.bind.JAXB;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;

/**
 * 
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 9, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement
public class VbGFEMapping {

    private final static String VB_GFE_FILE = "volumebrowser/VbGFEMapping.xml";

    private static final ILocalizationFileObserver observer = new ILocalizationFileObserver() {
        @Override
        public void fileUpdated(FileUpdatedMessage message) {
            synchronized (VB_GFE_FILE) {
                instance = null;
            }
        }
    };

    @XmlAccessorType(XmlAccessType.NONE)
    public static class Mapping {

        @XmlAttribute(required = true)
        public String vbId;

        @XmlAttribute(required = true)
        public String gfeId;
    }

    @XmlElement(name = "levelMapping")
    private List<Mapping> levelMappings;

    @XmlElement(name = "fieldMapping")
    private List<Mapping> fieldMappings;

    @XmlElement(name = "sourceMapping")
    private List<Mapping> sourcedMappings;

    private VbGFEMapping() {

    }

    private static VbGFEMapping instance = null;

    private static VbGFEMapping getInstance() {
        synchronized (VB_GFE_FILE) {
            if (instance == null) {
                LocalizationFile file = PathManagerFactory.getPathManager()
                        .getStaticLocalizationFile(VB_GFE_FILE);

                File path = file.getFile();

                try {
                    instance = JAXB.unmarshal(file.getFile(),
                            VbGFEMapping.class);
                } catch (Exception e) {
                    throw new RuntimeException(
                            "An error has occured while reading "
                                    + path.toString(), e);
                }
                file.addFileUpdatedObserver(observer);
            }
            return instance;
        }
    }

    private static String getVbId(String gfeId, List<Mapping> mappings) {
        for (Mapping mapping : mappings) {
            if (mapping.gfeId.equals(gfeId)) {
                return mapping.vbId;
            }
        }
        return null;
    }

    private static String getGfeId(String vbId, List<Mapping> mappings) {
        for (Mapping mapping : mappings) {
            if (mapping.vbId.equals(vbId)) {
                return mapping.gfeId;
            }
        }
        return null;
    }

    public static String getVbLevel(String gfeLevel) {
        return getVbId(gfeLevel, getInstance().levelMappings);
    }

    public static String getGfeLevel(String vbLevel) {
        return getGfeId(vbLevel, getInstance().levelMappings);

    }

    public static String getVbParam(String gfeParam) {
        return getVbId(gfeParam, getInstance().fieldMappings);
    }

    public static String getGfeParam(String vbParam) {
        return getGfeId(vbParam, getInstance().fieldMappings);
    }

    public static String getVbSource(String gfeSource) {
        return getVbId(gfeSource, getInstance().sourcedMappings);
    }

    public static String getGfeSource(String vbSource) {
        return getGfeId(vbSource, getInstance().sourcedMappings);
    }

}
