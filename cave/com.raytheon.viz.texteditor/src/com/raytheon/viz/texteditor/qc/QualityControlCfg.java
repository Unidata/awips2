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
package com.raytheon.viz.texteditor.qc;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 6, 2011  10764      rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class QualityControlCfg implements ISerializableObject {
    @XmlElement(name = "ImmediateCause")
    String[] immediateCause;

    /** Maps warning PILs to appropriate warning text */
    @XmlJavaTypeAdapter(QCMapAdapter.class)
    private Map<String, String> productTypeMap;

    @XmlJavaTypeAdapter(QCMapAdapter.class)
    private Map<String, String> followupNNN;

    @XmlJavaTypeAdapter(QCMapAdapter.class)
    private Map<String, String> nnnOfIdent;

    @XmlElement(name = "SegmentedNNN")
    private ArrayList<String> segmentedNNN;

    @XmlJavaTypeAdapter(BulletAdapter.class)
    private Map<String, List<String>> bulletTypeMap;

    public String[] getImmediateCause() {
        return immediateCause;
    }

    public void setImmediateCause(String[] immediateCause) {
        this.immediateCause = immediateCause;
    }

    public Map<String, String> getProductTypeMap() {
        return productTypeMap;
    }

    public void setProductTypeMap(Map<String, String> productTypeMap) {
        this.productTypeMap = productTypeMap;
    }

    public Map<String, String> getFollowupNNN() {
        return followupNNN;
    }

    public void setFollowupNNN(Map<String, String> followupNNN) {
        this.followupNNN = followupNNN;
    }

    public Map<String, String> getNnnOfIdent() {
        return nnnOfIdent;
    }

    public void setNnnOfIdent(Map<String, String> nnnOfIdent) {
        this.nnnOfIdent = nnnOfIdent;
    }

    public ArrayList<String> getSegmentedNNN() {
        return segmentedNNN;
    }

    public void setSegmentedNNN(ArrayList<String> segmentedNNN) {
        this.segmentedNNN = segmentedNNN;
    }

    public Map<String, List<String>> getBulletTypeMap() {
        return bulletTypeMap;
    }

    public void setBulletTypeMap(Map<String, List<String>> bulletTypeMap) {
        this.bulletTypeMap = bulletTypeMap;
    }
}
