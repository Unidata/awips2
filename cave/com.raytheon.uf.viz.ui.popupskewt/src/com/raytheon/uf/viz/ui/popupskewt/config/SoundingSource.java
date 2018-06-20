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
package com.raytheon.uf.viz.ui.popupskewt.config;

import java.util.HashMap;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestableMetadataMarshaller;

/**
 * Source configuration for popup-skewt sounding provider
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 19, 2013       2190 mschenke    Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class SoundingSource {

    @XmlAttribute
    private String type;

    @XmlAttribute
    private String displayString;

    @XmlAttribute
    private long validTimeInterval;

    @XmlJavaTypeAdapter(value = RequestableMetadataMarshaller.class)
    protected HashMap<String, RequestConstraint> constraints = new HashMap<String, RequestConstraint>();

    /**
     * The type of the source (pluginName)
     * 
     * @return
     */
    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    /**
     * Display string for the source
     * 
     * @return
     */
    public String getDisplayString() {
        return displayString;
    }

    public void setDisplayString(String displayString) {
        this.displayString = displayString;
    }

    /**
     * The valid time interval for data in minutes
     * 
     * @return
     */
    public long getValidTimeInterval() {
        return validTimeInterval;
    }

    public void setValidTimeInterval(long validTimeInterval) {
        this.validTimeInterval = validTimeInterval;
    }

    /**
     * The request constraints for the source
     * 
     * @return
     */
    public HashMap<String, RequestConstraint> getConstraints() {
        return constraints;
    }

    public void setConstraints(HashMap<String, RequestConstraint> constraints) {
        this.constraints = constraints;
    }

}
