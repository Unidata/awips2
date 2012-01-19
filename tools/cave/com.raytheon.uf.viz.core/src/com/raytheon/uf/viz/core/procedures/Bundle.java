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
import java.io.FileReader;
import java.util.Map;

import javax.xml.bind.JAXBException;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElementWrapper;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.SerializationUtil;
import com.raytheon.uf.viz.core.VariableSubstitutionUtil;
import com.raytheon.uf.viz.core.datastructure.LoopProperties;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Bundle
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Aug 30, 2007             chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class Bundle implements ISerializableObject {

    /** Contains the descriptors */
    @XmlElement
    @XmlElementWrapper(name = "displayList")
    protected AbstractRenderableDisplay[] displays;

    /** Optional: An editor to load the bundle to */
    @XmlAttribute
    protected String editor;

    /** Optional: A view to load the bundle to */
    @XmlAttribute
    protected String view;

    /** Optional: A name to give the bundle in the procedure list */
    @XmlAttribute
    protected String name;

    @XmlAttribute
    protected String layoutId;

    @XmlElement
    protected LoopProperties loopProperties = null;

    /**
     * Default constructor
     */
    public Bundle() {

    }

    public String getLayoutId() {
        return layoutId;
    }

    public void setLayoutId(String layoutId) {
        this.layoutId = layoutId;
    }

    /**
     * @return the editor
     */
    public String getEditor() {
        return editor;
    }

    /**
     * @param editor
     *            the editor to set
     */
    public void setEditor(String editor) {
        this.editor = editor;
    }

    /**
     * used only for saving procedures
     * 
     * @return
     */
    public LoopProperties getLoopProperties() {
        return loopProperties;
    }

    /**
     * used only for saving procedures
     * 
     * @param props
     */
    public void setLoopProperties(LoopProperties props) {
        loopProperties = props;
    }

    /**
     * @return the view
     */
    public String getView() {
        return view;
    }

    /**
     * @param view
     *            the view to set
     */
    public void setView(String view) {
        this.view = view;
    }

    /**
     * @return the name
     */
    public String getName() {
        return name;
    }

    /**
     * @param name
     *            the name to set
     */
    public void setName(String name) {
        this.name = name;
    }

    public String toXML() throws VizException {
        try {
            return SerializationUtil.marshalToXml(this);
        } catch (JAXBException e) {
            throw new VizException(e);
        }
    }

    /**
     * Unmarshal a bundle
     * 
     * @param fileName
     *            the bundle to load
     * 
     * @param descriptor
     *            Optional: A descriptor that should be used for time matching
     * @return bundle loaded
     * 
     * @throws VizException
     */
    public static Bundle unmarshalBundle(File fileName) throws VizException {
        return unmarshalBundle(fileName, null);
    }

    /**
     * @return the displays
     */
    public AbstractRenderableDisplay[] getDisplays() {
        return displays;
    }

    /**
     * @param displays
     *            the displays to set
     */
    public void setDisplays(AbstractRenderableDisplay[] displays) {
        this.displays = displays;
    }

    /**
     * Unmarshal a bundle
     * 
     * @param fileName
     *            the bundle to load
     * 
     * @param descriptor
     *            Optional: A descriptor that should be used for time matching
     * 
     * @param variables
     *            Optional: A map containing key value pairs to be used to
     *            perform variable substitution.
     * 
     * @return bundle loaded
     * 
     * @throws VizException
     */
    public static Bundle unmarshalBundle(File fileName,
            Map<String, String> variables) throws VizException {

        String s = null;
        try {
            FileReader fr = new FileReader(fileName);
            char[] b = new char[(int) fileName.length()];
            fr.read(b);
            fr.close();
            s = new String(b);

        } catch (Exception e) {
            throw new VizException("Error opening bundle file " + fileName, e);
        }

        return unmarshalBundle(s, variables);

    }

    /**
     * Unmarshal a bundle
     * 
     * @param bundle
     *            the bundle to load as a string
     * 
     * @param descriptor
     *            Optional: A descriptor that should be used for time matching
     * 
     * @return bundle loaded
     * 
     * @throws VizException
     */
    public static Bundle unmarshalBundle(String bundleStr) throws VizException {
        return unmarshalBundle(bundleStr, null);
    }

    /**
     * Unmarshal a bundle
     * 
     * @param bundle
     *            the bundle to load as a string
     * 
     * @param descriptor
     *            Optional: A descriptor that should be used for time matching
     * 
     * @param variables
     *            Optional: A map containing key value pairs to be used to
     *            perform variable substitution.
     * 
     * @return bundle loaded
     * 
     * @throws VizException
     */
    public static Bundle unmarshalBundle(String bundleStr,
            Map<String, String> variables) throws VizException {

        try {
            String substStr = VariableSubstitutionUtil.processVariables(
                    bundleStr, variables);

            Bundle b = (Bundle) SerializationUtil.unmarshalFromXml(substStr);

            return b;
        } catch (Exception e) {
            throw new VizException("Error loading bundle", e);
        }
    }

}
