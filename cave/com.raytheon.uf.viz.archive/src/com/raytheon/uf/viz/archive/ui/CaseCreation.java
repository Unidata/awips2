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
package com.raytheon.uf.viz.archive.ui;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * Archive case creation dialog's configuration options.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 11, 2014 3023       rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

@XmlRootElement(name = "CaseCreation")
@XmlAccessorType(XmlAccessType.NONE)
public class CaseCreation {
    @XmlElement(name = "CautionThreshold")
    private float cautionThreshold;

    @XmlElement(name = "DangerThreshold")
    private float dangerThreshold;

    @XmlElement(name = "FatalThreshold")
    private float fatalThreshold;

    public float getCautionThreshold() {
        return cautionThreshold;
    }

    public void setCautionThreshold(float cautionThreshold) {
        this.cautionThreshold = cautionThreshold;
    }

    public float getDangerThreshold() {
        return dangerThreshold;
    }

    public void setDangerThreshold(float dangerThreshold) {
        this.dangerThreshold = dangerThreshold;
    }

    public float getFatalThreshold() {
        return fatalThreshold;
    }

    public void setFatalThreshold(float fatalThreshold) {
        this.fatalThreshold = fatalThreshold;
    }
}
