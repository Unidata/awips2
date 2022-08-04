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
package com.raytheon.viz.warngen.gui;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.maps.rsc.AbstractMapResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractNameGenerator;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

/**
 * Resource data for WarnGen Extension Area map resource
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 30, 2018 6562       tgurney     Initial creation
 *
 * </pre>
 *
 * @author tgurney
 */

@XmlAccessorType(XmlAccessType.NONE)
public class WarngenExtensionAreaLayerResourceData
        extends AbstractMapResourceData {

    public WarngenExtensionAreaLayerResourceData() {
        super();
        this.nameGenerator = new AbstractNameGenerator() {
            @Override
            public String getName(AbstractVizResource<?, ?> resource) {
                return mapName;
            }
        };

    }

    @Override
    public WarngenExtensionAreaLayer construct(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {
        return new WarngenExtensionAreaLayer(this, loadProperties);
    }

    @Override
    public void update(Object updateData) {
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (obj.getClass() != getClass()) {
            return false;
        }
        WarngenExtensionAreaLayerResourceData other = (WarngenExtensionAreaLayerResourceData) obj;
        if (mapName == null) {
            if (other.mapName != null) {
                return false;
            }
        } else if (!mapName.equals(other.mapName)) {
            return false;
        }
        return true;
    }

}
