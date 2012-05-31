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
package com.raytheon.uf.viz.collaboration.display.editor;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.adapters.XmlJavaTypeAdapter;

import org.geotools.coverage.grid.GeneralGridGeometry;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.adapters.GridGeometryAdapter;
import com.raytheon.uf.common.serialization.adapters.JTSEnvelopeAdapter;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;
import com.vividsolutions.jts.geom.Envelope;

/**
 * A SharedEditorData is a POJO to be sent out by the Data Provider that
 * contains enough information to create the CollaborationEditor for the
 * participants.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 16, 2012            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement
public class SharedEditorData implements ISerializableObject {

    /** The geometry of the descriptor */
    private GeneralGridGeometry geometry;

    @XmlElement
    private AbstractRenderableDisplay display;

    /** the view's extent, i.e. the current zoom/pan */
    private Envelope envelope;

    @XmlAttribute
    private int width;

    @XmlAttribute
    private int height;

    @XmlElement
    @XmlJavaTypeAdapter(value = GridGeometryAdapter.class)
    public GeneralGridGeometry getGeometry() {
        return geometry;
    }

    public void setGeometry(GeneralGridGeometry geometry) {
        this.geometry = geometry;
    }

    /**
     * @return the display
     */
    public AbstractRenderableDisplay getDisplay() {
        return display;
    }

    /**
     * @param display
     *            the display to set
     */
    public void setDisplay(AbstractRenderableDisplay display) {
        this.display = display;
    }

    @XmlElement
    @XmlJavaTypeAdapter(value = JTSEnvelopeAdapter.class)
    public Envelope getEnvelope() {
        return envelope;
    }

    public void setEnvelope(Envelope envelope) {
        this.envelope = envelope;
    }

    /**
     * @return the width
     */
    public int getWidth() {
        return width;
    }

    /**
     * @param width
     *            the width to set
     */
    public void setWidth(int width) {
        this.width = width;
    }

    /**
     * @return the height
     */
    public int getHeight() {
        return height;
    }

    /**
     * @param height
     *            the height to set
     */
    public void setHeight(int height) {
        this.height = height;
    }

}
