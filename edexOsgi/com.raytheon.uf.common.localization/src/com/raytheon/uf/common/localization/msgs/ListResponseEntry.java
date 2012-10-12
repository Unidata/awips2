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

package com.raytheon.uf.common.localization.msgs;

import java.util.Date;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * 
 * Defines the properties of an individual list response item
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 19, 2007            chammack    Initial Creation.
 * Aug 22, 2008  #1502     bclement    Added JAXB/Serializable annotations
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class ListResponseEntry implements ISerializableObject {

    @XmlAttribute
    @DynamicSerializeElement
    protected String fileName;

    @XmlElement
    @DynamicSerializeElement
    protected LocalizationContext context;

    @XmlAttribute
    @DynamicSerializeElement
    protected Date date;

    @XmlAttribute
    @DynamicSerializeElement
    protected String checksum;

    @XmlAttribute
    @DynamicSerializeElement
    protected boolean directory;

    @XmlAttribute
    @DynamicSerializeElement
    protected LocalizationLevel protectedLevel;

    @XmlAttribute
    @DynamicSerializeElement
    protected boolean existsOnServer;

    public boolean isExistsOnServer() {
        return existsOnServer;
    }

    public void setExistsOnServer(boolean existsOnServer) {
        this.existsOnServer = existsOnServer;
    }

    /**
     * @return the protectedLevel
     */
    public LocalizationLevel getProtectedLevel() {
        return protectedLevel;
    }

    /**
     * @param protectedLevel
     *            the protectedLevel to set
     */
    public void setProtectedLevel(LocalizationLevel protectedLevel) {
        this.protectedLevel = protectedLevel;
    }

    /**
     * @return the fileName
     */
    public String getFileName() {
        return fileName;
    }

    /**
     * @param fileName
     *            the fileName to set
     */
    public void setFileName(String fileName) {
        this.fileName = fileName;
    }

    /**
     * @return the date
     */
    public Date getDate() {
        return date;
    }

    /**
     * @param date
     *            the date to set
     */
    public void setDate(Date date) {
        this.date = date;
    }

    /**
     * @return the checksum
     */
    public String getChecksum() {
        return checksum;
    }

    /**
     * @param checksum
     *            the checksum to set
     */
    public void setChecksum(String checksum) {
        this.checksum = checksum;
    }

    public boolean isDirectory() {
        return directory;
    }

    public void setDirectory(boolean isDirectory) {
        this.directory = isDirectory;
    }

    @Override
    public String toString() {
        return fileName;
    }

    public LocalizationContext getContext() {
        return context;
    }

    public void setContext(LocalizationContext context) {
        this.context = context;
    }

}
