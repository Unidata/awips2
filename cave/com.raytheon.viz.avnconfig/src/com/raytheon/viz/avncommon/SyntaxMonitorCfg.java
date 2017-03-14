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
package com.raytheon.viz.avncommon;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

@XmlRootElement(name = "SyntaxMonitorConfig")
@XmlAccessorType(XmlAccessType.NONE)
public class SyntaxMonitorCfg implements ISerializableObject
{
    /**
     * Name for the syntax fatal color.
     */
    @XmlElement(name = "SyntaxFatal")
    private String syntaxFatalColor;
    
    /**
     * Name for the syntax fatal color.
     */
    @XmlElement(name = "SyntaxError")
    private String syntaxErrorColor;
    
    /**
     * Name for the syntax fatal color.
     */
    @XmlElement(name = "SyntaxWarning")
    private String syntaxWarningColor;
    
    /**
     * Name for the syntax fatal color.
     */
    @XmlElement(name = "MonitorColors")
    private String monitorColors;
    
    public SyntaxMonitorCfg()
    {        
    }

    public String getSyntaxFatalColor() {
        return syntaxFatalColor;
    }

    public void setSyntaxFatalColor(String syntaxFatalColor) {
        this.syntaxFatalColor = syntaxFatalColor;
    }

    public String getSyntaxErrorColor() {
        return syntaxErrorColor;
    }

    public void setSyntaxErrorColor(String syntaxErrorColor) {
        this.syntaxErrorColor = syntaxErrorColor;
    }

    public String getSyntaxWarningColor() {
        return syntaxWarningColor;
    }

    public void setSyntaxWarningColor(String syntaxWarningColor) {
        this.syntaxWarningColor = syntaxWarningColor;
    }

    public String getMonitorColors() {
        return monitorColors;
    }

    public void setMonitorColors(String monitorColors) {
        this.monitorColors = monitorColors;
    }   
}
