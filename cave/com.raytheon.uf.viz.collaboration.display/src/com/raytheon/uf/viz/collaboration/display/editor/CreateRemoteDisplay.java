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

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.viz.core.drawables.AbstractRenderableDisplay;

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
public class CreateRemoteDisplay implements ISerializableObject {

    @XmlAttribute
    private int displayId;

    @XmlElement
    private AbstractRenderableDisplay display;

    /**
     * @return the displayId
     */
    public int getDisplayId() {
        return displayId;
    }

    /**
     * @param displayId
     *            the displayId to set
     */
    public void setDisplayId(int displayId) {
        this.displayId = displayId;
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

}
