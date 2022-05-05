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
/**
 * 
 */
package com.raytheon.viz.gfe.rsc.zones;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;

/**
 * A simple class to encapsulate data that needs to be passed from
 * ShapeBuilderJob to ZoneDbResource, in a single, queueable object.
 * <p>
 * Because this class is meant for this very simple communications interface,
 * all its members are public.
 * 
 * @author wldougher
 * 
 */
public class ShapeBuilderResponse {
    public int id;

    public List<String> tables;

    public List<List<String>> groups;

    public IShadedShape background;

    public IWireframeShape outline;

    public IWireframeShape cwaOutline;

    public ArrayList<IShadedShape> groupShapes;

    public Map<Object, RGB> colorMap;
}
