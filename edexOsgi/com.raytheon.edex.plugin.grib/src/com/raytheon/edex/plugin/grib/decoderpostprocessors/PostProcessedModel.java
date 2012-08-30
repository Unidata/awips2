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
package com.raytheon.edex.plugin.grib.decoderpostprocessors;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * A container class to hold which post processors apply to a grib model
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 7/24/12      949         bphillip    Initial Creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@XmlRootElement(name = "postProcessedModel")
@XmlAccessorType(XmlAccessType.NONE)
public class PostProcessedModel implements ISerializableObject {

	/**
	 * The model name to which the processors apply. May be a regular expression
	 */
	@XmlElement
	private String modelName;

	/**
	 * The list of grib decoder post processors. The short class name may be
	 * used if the class is in the
	 * com.raytheon.edex.plugin.grib.decoderpostprocessors package. A fully
	 * qualified name may be used if the grib post processor is defined
	 * elsewhere
	 */
	@XmlElement(name = "processorName")
	private List<String> processors;

	public PostProcessedModel() {

	}

	public String getModelName() {
		return modelName;
	}

	public void setModelName(String modelName) {
		this.modelName = modelName;
	}

	public List<String> getProcessors() {
		if (processors == null) {
			processors = new ArrayList<String>();
		}
		return processors;
	}

	public void setProcessors(List<String> processors) {
		this.processors = processors;
	}

	public String toString() {
		StringBuffer buf = new StringBuffer();
		buf.append(modelName).append("\n");
		for (String proc : processors) {
			buf.append(proc).append("\n");
		}
		return buf.toString();
	}

}
