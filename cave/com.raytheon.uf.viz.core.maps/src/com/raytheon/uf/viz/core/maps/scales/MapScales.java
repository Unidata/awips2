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
package com.raytheon.uf.viz.core.maps.scales;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.JAXB;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.eclipse.ui.IWorkbenchWindow;

import com.raytheon.uf.common.localization.FileUpdatedMessage;
import com.raytheon.uf.common.localization.ILocalizationFileObserver;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.procedures.Bundle;
import com.raytheon.uf.viz.core.procedures.Procedure;
import com.raytheon.viz.ui.actions.LoadSerializedXml;

/**
 * Serializable object representation of map scales
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 7, 2010             mschenke    Initial creation
 * Mar 21, 2013       1638 mschenke    Made map scales not tied to d2d
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class MapScales implements ISerializableObject {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(MapScales.class);

    @XmlAccessorType(XmlAccessType.NONE)
    public static class PartId {
        @XmlAttribute
        private String id;

        @XmlAttribute
        private boolean view = true;

        public String getId() {
            return id;
        }

        public void setId(String id) {
            this.id = id;
        }

        public boolean isView() {
            return view;
        }

        public void setView(boolean view) {
            this.view = view;
        }

    }

    @XmlAccessorType(XmlAccessType.NONE)
    public static class MapScale {

        @XmlAttribute
        private String displayName;

        @XmlAttribute
        private String fileName;

        private PartId[] partIds;

        public MapScale() {
            partIds = new PartId[0];
        }

        public String getDisplayName() {
            return displayName;
        }

        public void setDisplayName(String displayName) {
            this.displayName = displayName;
        }

        public String getFileName() {
            return fileName;
        }

        public void setFileName(String fileName) {
            this.fileName = fileName;
        }

        @XmlElement(name = "partId")
        public PartId[] getPartIds() {
            return partIds;
        }

        public void setPartIds(PartId[] partIds) {
            this.partIds = partIds;
        }

        public File getFile() {
            return PathManagerFactory.getPathManager().getStaticFile(
                    SCALES_DIR + fileName);
        }

    }

    private static final String SCALES_DIR = "bundles" + File.separator
            + "scales" + File.separator;

    public static final String FILE_NAME = SCALES_DIR + "scalesInfo.xml";

    private static ILocalizationFileObserver listener = new ILocalizationFileObserver() {
        @Override
        public void fileUpdated(FileUpdatedMessage message) {
            MapScales.fileUpdated();
        }
    };

    private static LocalizationFile locFile = null;

    private static MapScales instance;

    private MapScale[] scales;

    public static synchronized MapScales getInstance() {
        if (instance == null) {
            loadInstance();
        }
        return instance;
    }

    public static void loadScales(IWorkbenchWindow window) throws VizException {
        Procedure procedure = new Procedure();
        List<Bundle> bundles = new ArrayList<Bundle>();
        for (MapScale scale : MapScales.getInstance().getScales()) {
            String editorId = null;
            for (PartId partId : scale.getPartIds()) {
                if (partId.isView() == false) {
                    editorId = partId.getId();
                    break;
                }
            }
            if (editorId != null) {
                File file = scale.getFile();
                try {
                    Bundle b = SerializationUtil.jaxbUnmarshalFromXmlFile(
                            Bundle.class, file);
                    b.setEditor(editorId);
                    bundles.add(b);
                } catch (SerializationException e) {
                    statusHandler.handle(
                            Priority.PROBLEM,
                            "Error deserializing bundle: "
                                    + file.getAbsolutePath(), e);
                }
            }
        }
        procedure.setBundles(bundles.toArray(new Bundle[bundles.size()]));
        LoadSerializedXml.loadProcedureToScreen(procedure, window);
    }

    private static synchronized void fileUpdated() {
        instance = null;
    }

    private static void loadInstance() {
        if (locFile != null) {
            locFile.removeFileUpdatedObserver(listener);
        }
        locFile = PathManagerFactory.getPathManager()
                .getStaticLocalizationFile(FILE_NAME);
        locFile.addFileUpdatedObserver(listener);
        File file = locFile.getFile();
        if (file == null) {
            statusHandler.handle(Priority.PROBLEM,
                    "Could not find any version of scale file: " + FILE_NAME);
        } else {
            try {
                instance = JAXB.unmarshal(file, MapScales.class);
            } catch (RuntimeException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Could not parse scale file: " + FILE_NAME, e);
            }
        }
    }

    public MapScales() {
        scales = new MapScale[0];
    }

    @XmlElement(name = "mapScale")
    public MapScale[] getScales() {
        return scales;
    }

    public void setScales(MapScale[] scales) {
        this.scales = scales;
    }

    public MapScale getScaleByName(String name) {
        for (MapScale scale : scales) {
            if (scale.displayName.equals(name)) {
                return scale;
            }
        }
        return null;
    }
}
