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
package com.raytheon.viz.mpe.ui.rsc;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

import com.raytheon.uf.common.dataplugin.shef.tables.Colorvalue;
import com.raytheon.uf.common.mpe.util.XmrgFile;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.viz.hydrocommon.util.MPEColors;
import com.raytheon.viz.hydrocommon.whfslib.colorthreshold.GetColorValues;
import com.raytheon.viz.mpe.ui.DisplayFieldData;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;

/**
 * The Xmrg Resource Data
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 27, 2009            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement(name = "xmrgResourceData")
@XmlType(name = "xmrgResourceData", namespace = "com.raytheon.viz.mpe.ui.rsc.XmrgResourceData")
public class XmrgResourceData extends AbstractResourceData {

	private final MPEDisplayManager dm;

	@XmlElement
	private DisplayFieldData dt;

	@XmlElement
	private XmrgFile xmfile;

	private List<Colorvalue> colors;

	public XmrgResourceData() {
		IDisplayPane pane = null;
		dm = MPEDisplayManager.getInstance(pane);
	}

	/**
	 * @param displayManager
	 * @param displayFieldType
	 * @param xmrg
	 * @param colorSet
	 */
	public XmrgResourceData(MPEDisplayManager displayManager,
			DisplayFieldData displayFieldType, XmrgFile xmrg,
			List<Colorvalue> colorSet) {
		dm = displayManager;
		dt = displayFieldType;
		xmfile = xmrg;
		colors = colorSet;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.viz.core.rsc.AbstractResourceData#construct(com.raytheon
	 * .uf.viz.core.comm.LoadProperties,
	 * com.raytheon.uf.viz.core.drawables.IDescriptor)
	 */
	@Override
	public XmrgResource construct(LoadProperties loadProperties,
			IDescriptor descriptor) throws VizException {
		if (colors == null) {
			String user_id = System.getProperty("user.name");
			colors = GetColorValues.get_colorvalues(user_id,
					MPEDisplayManager.APPLICATION_NAME, dt.getCv_use(),
					dt.getCv_duration(), "E", MPEColors.build_mpe_colors());
			dm.setDisplayFieldType(dt);
		}
		return new XmrgResource(this, dm, dt, xmfile, colors);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.viz.core.rsc.AbstractResourceData#update(java.lang.Object
	 * )
	 */
	@Override
	public void update(Object updateData) {

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((dm == null) ? 0 : dm.hashCode());
		return result;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (getClass() != obj.getClass()) {
			return false;
		}
		return true;
	}
}
