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

import jakarta.xml.bind.annotation.XmlAccessType;
import jakarta.xml.bind.annotation.XmlAccessorType;
import jakarta.xml.bind.annotation.XmlElement;
import jakarta.xml.bind.annotation.XmlElements;
import jakarta.xml.bind.annotation.XmlRootElement;

/**
 * A container class to hold the list of models needing to be post processed
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jul 24, 2012  949      bphillip    Initial Creation
 * Oct 15, 2013  2473     bsteffen    Remove deprecated ISerializableObject.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
@XmlRootElement(name = "postProcessedModels")
@XmlAccessorType(XmlAccessType.NONE)
public class PostProcessedModelSet {

	/**
	 * List of post processed modesl
	 */
	@XmlElements({ @XmlElement(name = "postProcessedModel", type = PostProcessedModel.class) })
	private ArrayList<PostProcessedModel> models;

	public ArrayList<PostProcessedModel> getModels() {
		if(models == null){
			models = new ArrayList<PostProcessedModel>();
		}
		return models;
	}

	public void setModels(ArrayList<PostProcessedModel> models) {
		this.models = models;
	}

}
