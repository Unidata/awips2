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
package com.raytheon.uf.viz.xy.crosssection;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXB;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 1, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

@XmlRootElement(name = "crossSectionRotations")
@XmlAccessorType(XmlAccessType.NONE)
public class CrossSectionRotationsFile {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(CrossSectionRotationsFile.class);

    @XmlAccessorType(XmlAccessType.NONE)
    public static enum RotationMode {
        VR_NO_ROTATION, VR_EARTH_COORDINATES, VR_SECTION_COORDINATES, VR_COMPONENT_INTO, VR_COMPONENT_ALONG, VR_GEO_MOMENTUM, VR_VERT_CIRC
    }

    @XmlAccessorType(XmlAccessType.NONE)
    public static class Rotation {

        @XmlAttribute
        private String parameter;

        @XmlAttribute
        private RotationMode mode;

        public String getParameter() {
            return parameter;
        }

        public void setParameter(String parameter) {
            this.parameter = parameter;
        }

        public RotationMode getMode() {
            return mode;
        }

        public void setMode(RotationMode mode) {
            this.mode = mode;
        }

    }

    private static Map<String, RotationMode> map = null;

    @XmlElement(name = "rotation")
    private List<Rotation> rotations;

    public List<Rotation> getRotations() {
        return rotations;
    }

    public void setRotations(List<Rotation> rotations) {
        this.rotations = rotations;
    }

    public static RotationMode getRotationMode(String parameter) {
        if (map == null) {
            try {
                LocalizationFile file = PathManagerFactory.getPathManager()
                        .getStaticLocalizationFile(
                                "volumebrowser" + IPathManager.SEPARATOR
                                        + "CrossSectionRotations.xml");
                file.addFileUpdatedObserver(new ILocalizationFileObserver() {

                    @Override
                    public void fileUpdated(FileUpdatedMessage message) {
                        map = null;
                    }
                });
                CrossSectionRotationsFile instance = JAXB.unmarshal(
                        file.getFile(), CrossSectionRotationsFile.class);
                map = new HashMap<String, CrossSectionRotationsFile.RotationMode>();
                for (Rotation r : instance.getRotations()) {
                    map.put(r.getParameter(), r.getMode());
                }
            } catch (Exception e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error deserializing cross section rotations", e);
                return RotationMode.VR_NO_ROTATION;
            }
        }
        if (map.containsKey(parameter)) {
            return map.get(parameter);
        }
        return RotationMode.VR_NO_ROTATION;
    }

}
