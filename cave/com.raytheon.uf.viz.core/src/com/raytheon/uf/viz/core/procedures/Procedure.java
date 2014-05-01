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
package com.raytheon.uf.viz.core.procedures;

import java.io.File;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.eclipse.ui.IMemento;

import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Procedure
 * 
 * Represents a procedure-- a series of bundles that can be loaded either
 * serially (manually) or automatically
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Sep 05, 2007           chammack    Initial Creation.
 * Oct 22, 2013  2491     bsteffen    Switch serialization to 
 *                                    ProcedureXmlManager
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class Procedure {

    @XmlAttribute
    private String perspective;

    @XmlElementWrapper(name = "bundles")
    @XmlElement(name = "bundle")
    private Bundle[] bundles;

    @XmlElement
    @XmlJavaTypeAdapter(MementoAdapter.class)
    private IMemento layout;

    public IMemento getLayout() {
        return layout;
    }

    public void setLayout(IMemento layout) {
        this.layout = layout;
    }

    /**
     * @return the perspective
     */
    public String getPerspective() {
        return perspective;
    }

    /**
     * @param perspective
     *            the perspective to set
     */
    public void setPerspective(String perspective) {
        this.perspective = perspective;
    }

    /**
     * @return the bundles
     */
    public Bundle[] getBundles() {
        return bundles;
    }

    /**
     * @param bundles
     *            the bundles to set
     */
    public void setBundles(Bundle[] bundles) {
        this.bundles = bundles;
    }

    public String toXML() throws VizException {
        try {
            return ProcedureXmlManager.getInstance().marshal(this);
        } catch (SerializationException e) {
            throw new VizException(e);
        }
    }

    public static Procedure loadProcedure(File fileName) throws VizException {
        try {
            return ProcedureXmlManager.getInstance().unmarshal(
                    Procedure.class, fileName);
        } catch (SerializationException e) {
            throw new VizException(e);
        }
    }

    public static Procedure loadProcedure(String xml) throws VizException {
        try {
            return ProcedureXmlManager.getInstance().unmarshal(
                    Procedure.class, xml);
        } catch (SerializationException e) {
            throw new VizException(e);
        }
    }
}
