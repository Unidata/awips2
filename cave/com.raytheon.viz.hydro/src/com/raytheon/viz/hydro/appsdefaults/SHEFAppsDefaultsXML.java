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
package com.raytheon.viz.hydro.appsdefaults;

import java.util.ArrayList;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;


/**
 * SHEF Apps Defaults token object.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 18, 2011            mpduff     Initial creation
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0	
 */

@XmlRootElement(name = "SHEFGadTokens")
@XmlAccessorType(XmlAccessType.NONE)
public class SHEFAppsDefaultsXML {
    @XmlElements({ @XmlElement(name = "token", type = String.class) })
    protected ArrayList<String> tokenList;

    /**
     * @return the tokenList
     */
    public ArrayList<String> getTokenList() {
        return tokenList;
    }

    /**
     * @param tokenList the tokenList to set
     */
    public void setTokenList(ArrayList<String> tokenList) {
        this.tokenList = tokenList;
    }
}
