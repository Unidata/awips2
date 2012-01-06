/*****************************************************************************************
 * COPYRIGHT (c), 2007, RAYTHEON COMPANY
 * ALL RIGHTS RESERVED, An Unpublished Work 
 *
 * RAYTHEON PROPRIETARY
 * If the end user is not the U.S. Government or any agency thereof, use
 * or disclosure of data contained in this source code file is subject to
 * the proprietary restrictions set forth in the Master Rights File.
 *
 * U.S. GOVERNMENT PURPOSE RIGHTS NOTICE
 * If the end user is the U.S. Government or any agency thereof, this source
 * code is provided to the U.S. Government with Government Purpose Rights.
 * Use or disclosure of data contained in this source code file is subject to
 * the "Government Purpose Rights" restriction in the Master Rights File.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * Use or disclosure of data contained in this source code file is subject to
 * the export restrictions set forth in the Master Rights File.
 ******************************************************************************************/
package gov.noaa.nws.ncep.viz.ui.display;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Feb 20, 2010   #226      ghull       added Pane layout info to Bundle class.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class PredefinedArea implements ISerializableObject {

	@XmlElement
    protected NCMapRenderableDisplay  predefinedArea;

    public NCMapRenderableDisplay getPredefinedArea() {
		return predefinedArea;
	}

	public void setPredefinedArea(NCMapRenderableDisplay predefinedArea) {
		this.predefinedArea = predefinedArea;
	}

	public String getPredefinedAreaName() {
		return predefinedArea.getPredefinedAreaName();
	}

	public void setPredefinedAreaName(String predefinedAreaName) {
		predefinedArea.setPredefinedAreaName(predefinedAreaName);
	}
	
    /**
     * Default constructor
     */
    public PredefinedArea() {
    }
}
