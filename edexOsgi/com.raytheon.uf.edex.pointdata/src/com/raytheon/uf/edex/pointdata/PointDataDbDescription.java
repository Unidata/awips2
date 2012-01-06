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
package com.raytheon.uf.edex.pointdata;

import java.io.File;
import java.io.InputStream;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

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
 * Jan 27, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "pointDataDbDescription")
public class PointDataDbDescription implements ISerializableObject {

    @XmlElement(name = "parameter")
    public DbParameterDescription[] parameters;

    public DbParameterDescription getDescription(String parameterName) {
        for (DbParameterDescription desc : parameters) {
            if (desc.getParameterName().equals(parameterName)) {
                return desc;
            }
        }
        return null;
    }

    public static PointDataDbDescription fromFile(File file)
            throws JAXBException {
        PointDataDbDescription pdd = null;

        JAXBContext ctx = JAXBContext.newInstance(PointDataDbDescription.class);
        if (ctx != null) {
            Unmarshaller um = ctx.createUnmarshaller();
            if (um != null) {
                pdd = (PointDataDbDescription) um.unmarshal(file);
            }
        }
        return pdd;
    }

    public static PointDataDbDescription fromStream(InputStream is)
            throws JAXBException {
        PointDataDbDescription pdd = null;

        JAXBContext ctx = JAXBContext.newInstance(PointDataDbDescription.class);
        if (ctx != null) {
            Unmarshaller um = ctx.createUnmarshaller();
            if (um != null) {
                pdd = (PointDataDbDescription) um.unmarshal(is);
            }
        }
        return pdd;
    }

}
