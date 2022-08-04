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

package com.raytheon.uf.viz.alertviz.config;

/**
 * 
 * Provides an abstract implementation for saving a priority to metadata mapping
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 21Jan2011    1978       cjeanbap    Initial creation
 * </pre>
 * 
 * @author cjeanbap
 * @version 1.0
 */
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import com.raytheon.uf.common.serialization.ISerializableObject;

@XmlAccessorType(XmlAccessType.NONE)
public class MonitorMetadata implements ISerializableObject {

    /**
     * Is audio enabled for this message?
     */
    @XmlAttribute
    private String imageFile;

    @XmlAttribute
    private boolean omit;

    public MonitorMetadata() {
    }

    public MonitorMetadata(String imageFile, boolean isOmitted) {
        super();
        this.imageFile = imageFile;
    }

    public MonitorMetadata(MonitorMetadata metadata) {
        this.imageFile = metadata.imageFile;
    }

    public String getImageFile() {
        return imageFile;
    }

    /***
     * Set the Image file value.
     * 
     * @param imageFile
     *            the name of the image file.
     */
    public void setImageFile(String imageFile) {
        this.imageFile = imageFile;
    }

    public MonitorMetadata clone() {
        MonitorMetadata mm = new MonitorMetadata();

        mm.imageFile = imageFile;
        mm.omit = omit;

        return mm;
    }

    /**
     * @param omit
     *            the omit to set
     */
    public void setOmit(boolean omit) {
        this.omit = omit;
    }

    /**
     * @return the omit
     */
    public boolean getOmit() {
        return omit;
    }

    public void switchOmit() {
        this.omit = !this.omit;
    }

    public boolean hasImage() {
        return !(this.imageFile == null || this.imageFile.isEmpty());
    }
}
