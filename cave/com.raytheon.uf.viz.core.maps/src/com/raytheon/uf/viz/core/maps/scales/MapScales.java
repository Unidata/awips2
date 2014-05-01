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

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * Serializable object representation of a group of {@link MapScale}s
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct  7, 2010            mschenke    Initial creation
 * Mar 21, 2013       1638 mschenke    Made map scales not tied to d2d
 * Oct  8, 2013       2104 mschenke    Moved logic into manager class
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class MapScales {

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

    /** Serializable object representation of a single map scale */
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
    }

    private MapScale[] scales;

    public MapScales() {
        scales = new MapScale[0];
    }

    @XmlElement(name = "mapScale")
    public MapScale[] getScales() {
        return scales;
    }

    public void setScales(MapScale[] scales) {
        this.scales = scales != null ? scales : new MapScale[0];
    }

}
