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

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IShadedShape;
import com.raytheon.uf.viz.core.drawables.IWireframeShape;

/**
 * @author wldougher
 * 
 */
public class ShapeBuilderRequest {
    public IShadedShape background;

    public IWireframeShape cwaOutline;

    public DbData[] dbData;

    public List<List<String>> groups;

    public ArrayList<IShadedShape> groupShapes;

    public IWireframeShape outline;

    public List<String> tables;

    public IGraphicsTarget target;

    public DbData[] zoneData;

    public boolean shaded;

    public boolean outlined;

    public Map<Object, RGB> colorMap;

    public int id;

    public String reason;
}
