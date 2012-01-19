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
package com.raytheon.viz.grid.xml;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 9, 2009            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

@XmlRootElement(name = "validTypesFile")
@XmlAccessorType(XmlAccessType.NONE)
public class ValidTypeFile implements ISerializableObject {

    @XmlElements( { @XmlElement(name = "validType", type = ValidType.class) })
    private ArrayList<String> validTypesFile;

    public ValidTypeFile() {

    }

    public ArrayList<String> getValidTypesFile() {
        return validTypesFile;
    }

    public void setValidTypesFile(ArrayList<String> validTypesFile) {
        this.validTypesFile = validTypesFile;
    }

    /**
     * Checks if the given type is contained in the array
     * 
     * @param type
     * @return
     */
    public boolean contains(ValidType type) {
        return validTypesFile.contains(type);
    }
}
