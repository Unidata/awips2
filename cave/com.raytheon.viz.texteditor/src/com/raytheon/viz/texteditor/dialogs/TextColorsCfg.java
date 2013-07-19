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
package com.raytheon.viz.texteditor.dialogs;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

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
 * Jun 15, 2013  DR 15733   Xiaochuan     Initial creation
 * 
 * </pre>
 * 
 * @author XHuang
 * @version 1.0
 */
@XmlRootElement(name = "textColorsCfg")
@XmlAccessorType(XmlAccessType.NONE)
public class TextColorsCfg implements ISerializableObject {
	@XmlElements({ @XmlElement(name = "TextColorElement", type = TextColorElement.class) })
    private List<TextColorElement> colorElements;

    public List<TextColorElement> getTextColorElements() {
        return colorElements;
    }

    public void setTextColorElements(List<TextColorElement> colorElements) {
        this.colorElements = colorElements;
    }
    
 
    
    
    
}
