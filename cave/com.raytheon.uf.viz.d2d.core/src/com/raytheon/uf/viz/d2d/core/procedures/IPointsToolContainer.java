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
package com.raytheon.uf.viz.d2d.core.procedures;

import org.locationtech.jts.geom.Coordinate;

/**
 * Interface for points tool user
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -------------------
 * Jan 05, 2010           mschenke  Initial creation
 * Nov 10, 2016  5976     bsteffen  Move to D2D plugin
 * 
 * </pre>
 * 
 * @author mschenke
 */
public interface IPointsToolContainer {

    public void setPointCoordinate(Coordinate pointCoordinate);

    public Coordinate getPointCoordinate();

    public void setPointLetter(String pointLetter);

    public String getPointLetter();
}
