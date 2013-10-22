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
package com.raytheon.viz.core.slice.request;

import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.JAXB;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
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
 * Collection of {@link HeightScale} objects used by the volume browser and
 * several height displays to scale verticle data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Nov 24, 2009           mschenke    Initial creation
 * Oct 22, 2013  2491     bsteffen    Remove ISerializableObject
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement
public class HeightScales {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(HeightScales.class);

    @XmlElement
    private HeightScale[] scales;

    private Map<String, HeightScale> scaleMap = new HashMap<String, HeightScale>();

    private static HeightScales instance = null;

    private static LocalizationFile scalesFile;

    private static ILocalizationFileObserver listener = new ILocalizationFileObserver() {
        @Override
        public void fileUpdated(FileUpdatedMessage message) {
            HeightScales.fileUpdated();
        }
    };

    public static synchronized HeightScales getInstance() {
        if (instance == null) {
            loadHeightScales();
        }
        return instance;
    }

    public HeightScale[] getScales() {
        return scales;
    }

    public void setScales(HeightScale[] scales) {
        this.scales = scales;
    }

    private void construct() {
        for (HeightScale scale : scales) {
            scaleMap.put(scale.getName(), scale);
        }
    }

    private static void loadHeightScales() {
        try {
            scalesFile = PathManagerFactory.getPathManager()
                    .getStaticLocalizationFile(
                            "scales" + IPathManager.SEPARATOR
                                    + "heightScales.xml");
            scalesFile.addFileUpdatedObserver(listener);
            instance = JAXB.unmarshal(scalesFile.getFile(), HeightScales.class);
            instance.construct();
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error deserializing height scales", e);
        }
    }

    public static HeightScale fromName(String name) {
        HeightScales scales = getInstance();
        return scales.scaleMap.get(name);
    }

    private static synchronized void fileUpdated() {
        instance = null;
    }
}
