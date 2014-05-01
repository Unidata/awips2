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
package com.raytheon.viz.aviation.xml;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * Contains static information used to generate the Climate Data FTP scripts.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 19, 2011 #4864      rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
@XmlRootElement(name = "ClimateDataFTPArgs")
@XmlAccessorType(XmlAccessType.NONE)
public class ClimateDataFTPArgs implements ISerializableObject {

    @XmlElement(name = "Site")
    private String site;

    @XmlElement(name = "DataDir")
    private String dataDir;

    @XmlElement(name = "IshDir")
    private String ishDir;

    @XmlElement(name = "User")
    private String user;

    @XmlElement(name = "Password")
    private String password;

    public ClimateDataFTPArgs() {

    }

    public String getSite() {
        return site;
    }

    public String getDataDir() {
        return dataDir;
    }

    public String getIshDir() {
        return ishDir;
    }

    public String getUser() {
        return user;
    }

    public String getPassword() {
        return password;
    }

}
