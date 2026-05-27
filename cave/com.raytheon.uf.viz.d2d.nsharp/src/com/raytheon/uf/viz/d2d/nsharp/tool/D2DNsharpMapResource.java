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
package com.raytheon.uf.viz.d2d.nsharp.tool;

import gov.noaa.nws.ncep.ui.nsharp.display.map.AbstractNsharpMapResource;
import gov.noaa.nws.ncep.ui.nsharp.display.map.AbstractNsharpMapResourceData;

import com.raytheon.uf.viz.core.rsc.IInputHandler;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.ResourceProperties;

/**
 *
 * The class for D2D NSHARP Map resource
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------
 * Mar 23, 2020  73172    smanoj   Initial creation
 * Sep 09, 2020  82714    smanoj   Fix an issue with Blinking functionality
 * 
 * </pre>
 *
 * @author smanoj
 */
public class D2DNsharpMapResource extends AbstractNsharpMapResource {

    protected D2DNsharpMapResource(AbstractNsharpMapResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
    }

    @Override
    public IInputHandler getMouseHandler() {

        if (mouseHandler == null) {

            mouseHandler = new D2DNsharpMapMouseHandler();
        }
        return mouseHandler;
    }

    @Override
    public AbstractNsharpMapResourceData getNewMapResourceData() {
        return new D2DNsharpMapResourceData();
    }

    @Override
    public void propertiesChanged(ResourceProperties updatedProps) {
        // called when the ResourceProperties change(e.g., setVisible,
        // and setBlinking)
        issueRefresh();
    }

}