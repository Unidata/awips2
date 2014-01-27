package com.raytheon.uf.edex.plugin.obs.ogc.metar;

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

import java.util.Set;
import java.util.TreeSet;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.edex.ogc.common.db.DefaultPointDataDimension;
import com.raytheon.uf.edex.ogc.common.db.PointDataLayer;

/**
 * 
 * Make Metar Layers
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 08/09/2012   753       dhladky     Initial Creation.
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class MetarLayer extends PointDataLayer {

    private static final long serialVersionUID = 7334386034469721056L;

	/**
	 * 
	 */
	public MetarLayer() {
	}

	/**
	 * @param layer
	 */
	public MetarLayer(MetarLayer layer) {
		super(layer);
	}

	@Override
    public Set<DefaultPointDataDimension> getDimensions() {
        return new TreeSet<DefaultPointDataDimension>();
	}

}
