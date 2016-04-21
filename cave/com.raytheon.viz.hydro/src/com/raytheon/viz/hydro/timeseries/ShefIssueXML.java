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
package com.raytheon.viz.hydro.timeseries;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * Hydro Time Series Shef Issue xml.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 28, 2011            mpduff     Initial creation
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0	
 */

@XmlRootElement(name = "ShefIssue")
@XmlAccessorType(XmlAccessType.NONE)
public class ShefIssueXML implements ISerializableObject {
    
    @XmlElement(name = "internalDirectory")
    private InternalDirectoryXML internalDirectory;
    
    @XmlElement(name = "distributeProduct")
    private boolean distributeProduct;
    
    @XmlElement(name = "dirCopy")
    private boolean dirCopy = true;

    /**
     * @return the internalDirectory
     */
    public InternalDirectoryXML getInternalDirectory() {
        if (internalDirectory == null) {
            internalDirectory = new InternalDirectoryXML();
        }
        return internalDirectory;
    }

    /**
     * @param internalDirectory the internalDirectory to set
     */
    public void setInternalDirectory(InternalDirectoryXML internalDirectory) {
        this.internalDirectory = internalDirectory;
    }

    /**
     * @return the distributeProduct
     */
    public boolean isDistributeProduct() {
        return distributeProduct;
    }

    /**
     * @param distributeProduct the distributeProduct to set
     */
    public void setDistributeProduct(boolean distributeProduct) {
        this.distributeProduct = distributeProduct;
    }

    /**
     * @param dirCopy the dirCopy to set
     */
    public void setDirCopy(boolean dirCopy) {
        this.dirCopy = dirCopy;
    }

    /**
     * @return the dirCopy
     */
    public boolean isDirCopy() {
        return dirCopy;
    }
}
