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
package com.raytheon.uf.edex.ogc.common.db;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

 /**
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 29, 2011            bclement     Initial creation
 *
**/
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class LayerHolder<L extends SimpleLayer> {
	@XmlElement
	protected L value;

	@SuppressWarnings("unchecked")
	public static Class<LayerHolder<SimpleLayer>> getDefaultClass() {
		return (Class<LayerHolder<SimpleLayer>>) new LayerHolder<SimpleLayer>()
				.getClass();
	}

	public L getValue() {
		return value;
	}

	public void setValue(L value) {
		this.value = value;
	}

}
