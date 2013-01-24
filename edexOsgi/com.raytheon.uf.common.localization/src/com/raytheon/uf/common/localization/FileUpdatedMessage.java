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
package com.raytheon.uf.common.localization;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * A message sent when a file is updated
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * May 12, 2008				randerso	Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class FileUpdatedMessage implements ISerializableObject {
    public static enum FileChangeType {
        ADDED, UPDATED, DELETED
    };

    @DynamicSerializeElement
    @XmlElement
    private LocalizationContext context;

    @DynamicSerializeElement
    @XmlAttribute
    private String fileName;

    @DynamicSerializeElement
    @XmlElement
    private FileChangeType changeType;

    @DynamicSerializeElement
    @XmlAttribute
    private long timeStamp;

    public FileUpdatedMessage() {
    }

    public FileUpdatedMessage(LocalizationContext context, String fileName,
            FileChangeType changeType, long timeStamp) {
        this.context = context;
        this.fileName = fileName;
        this.changeType = changeType;
        this.timeStamp = timeStamp;
    }

    public LocalizationContext getContext() {
        return context;
    }

    public void setContext(LocalizationContext context) {
        this.context = context;
    }

    public String getFileName() {
        return fileName;
    }

    public void setFileName(String fileName) {
        this.fileName = fileName;
    }

    public FileChangeType getChangeType() {
        return changeType;
    }

    public void setChangeType(FileChangeType changeType) {
        this.changeType = changeType;
    }

    /**
     * @return the timeStamp
     */
    public long getTimeStamp() {
        return timeStamp;
    }

    /**
     * @param timeStamp
     *            the timeStamp to set
     */
    public void setTimeStamp(long timeStamp) {
        this.timeStamp = timeStamp;
    }

}
