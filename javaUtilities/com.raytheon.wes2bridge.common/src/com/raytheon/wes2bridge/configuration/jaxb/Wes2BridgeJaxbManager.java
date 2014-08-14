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
package com.raytheon.wes2bridge.configuration.jaxb;

import java.io.File;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;

import com.raytheon.wes2bridge.common.configuration.Wes2BridgeCase;

/**
 * Uses jaxb to convert Wes2Bridge configuration in XML format to a Java POJO.
 * 
 * <pre>
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 12, 2014 3521       bkowal      Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 * @version 1.0
 */

public class Wes2BridgeJaxbManager {

    /**
     * 
     */
    protected Wes2BridgeJaxbManager() {
    }

    public static Wes2BridgeCase toWes2BridgeCase(File wes2bridgeXMLFile)
            throws JAXBException {
        JAXBContext jaxbContext = JAXBContext.newInstance(Wes2BridgeCase.class);

        return (Wes2BridgeCase) jaxbContext.createUnmarshaller().unmarshal(
                wes2bridgeXMLFile);
    }
}