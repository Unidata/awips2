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
package com.raytheon.uf.common.dataplugin.gfe.weatherelement;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.serialize.ParmIDAdapter;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Defines a Weather Element Item (Formerly BundleItem in GFE I).
 * 
 * Specifies a parm id and its relative cycle number
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jun 5, 2008				chammack	Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class WEItem {

    @XmlAttribute
    @XmlJavaTypeAdapter(value = ParmIDAdapter.class)
    @DynamicSerializeElement
    private ParmID parmID;

    @XmlAttribute
    @DynamicSerializeElement
    private int relativeCycleNumber;

    public WEItem() {

    }

    public WEItem(ParmID parmID, int relativeCycleNumber) {
        super();
        this.parmID = parmID;
        this.relativeCycleNumber = relativeCycleNumber;
    }

    /**
     * @return the parmID
     */
    public ParmID getParmID() {
        return parmID;
    }

    /**
     * @return the relativeCycleNumber
     */
    public int getRelativeCycleNumber() {
        return relativeCycleNumber;
    }

    /**
     * @param parmID
     *            the parmID to set
     */
    public void setParmID(ParmID parmID) {
        this.parmID = parmID;
    }

    /**
     * @param relativeCycleNumber
     *            the relativeCycleNumber to set
     */
    public void setRelativeCycleNumber(int relativeCycleNumber) {
        this.relativeCycleNumber = relativeCycleNumber;
    }

}
