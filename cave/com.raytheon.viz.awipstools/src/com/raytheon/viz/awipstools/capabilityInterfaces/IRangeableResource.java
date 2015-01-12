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
package com.raytheon.viz.awipstools.capabilityInterfaces;

import javax.measure.Measure;
import javax.measure.quantity.Length;

import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.viz.awipstools.ui.layer.RangeRingsOverlayLayer;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Interface for {@link AbstractVizResource}s that are able to provide
 * information that can be used to draw range rings.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jun 11, 2014  2061     bsteffen    Switch from Amount to Measure.
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 * @see RangeRingsOverlayLayer
 */

public interface IRangeableResource {

    public Measure<?, Length> getElevation();

    public Coordinate getCenter();

    public double getTilt();
}
