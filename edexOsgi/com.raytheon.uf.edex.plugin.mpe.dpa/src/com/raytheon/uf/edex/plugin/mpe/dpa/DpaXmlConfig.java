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
package com.raytheon.uf.edex.plugin.mpe.dpa;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * Localized value determines whether to create database entries for decoded DPA
 * records. Command line argument 'writeToDB' ported and modified to XML state
 * from: main_decodedpa.c
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jul 15, 2016  4622     jschmid     Initial creation
 * 
 * </pre>
 * 
 * @author jschmid
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class DpaXmlConfig {

    private Boolean writeToDB;

    public boolean getWriteToDB() {
        return writeToDB.booleanValue();
    }

    @XmlElement
    public void setWriteToDB(final boolean writeToDB) {
        this.writeToDB = new Boolean(writeToDB);
    }
}