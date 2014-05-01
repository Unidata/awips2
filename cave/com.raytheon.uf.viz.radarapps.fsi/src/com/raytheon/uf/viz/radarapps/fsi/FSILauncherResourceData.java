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
package com.raytheon.uf.viz.radarapps.fsi;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

@XmlAccessorType(XmlAccessType.NONE)
public class FSILauncherResourceData extends AbstractResourceData {

    public FSILauncherResourceData() {

    }

    @Override
    public FSILauncherLayer construct(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {
        FSILauncherLayer layer = new FSILauncherLayer(this, loadProperties);
        layer.setDescriptor((MapDescriptor) descriptor);
        return layer;
    }

    @Override
    public boolean equals(Object obj) {
        // All instances are the same
        return obj instanceof FSILauncherResourceData;
    }

    @Override
    public void update(Object updateData) {
        // nothing
    }

}
