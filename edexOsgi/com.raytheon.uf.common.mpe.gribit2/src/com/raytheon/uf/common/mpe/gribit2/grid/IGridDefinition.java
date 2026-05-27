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
package com.raytheon.uf.common.mpe.gribit2.grid;

/**
 * Generic reference to any xmrg -> grib Grid Definition.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 14, 2016 4619       bkowal      Initial creation
 * Aug 10, 2016 4619       bkowal      Added getters/setters for interacting
 *                                     with common grid definition attributes.
 * 
 * </pre>
 * 
 * @author bkowal
 */

public interface IGridDefinition {

    public int getNumVertCoords();

    public void setNumVertCoords(int numVertCoords);

    public int getPvPL255();

    public void setPvPL255(int pvPL255);

    public int getDataRepresentationType();

    public void setDataRepresentationType(int dataRepresentationType);

    public int getNumberXPoints();

    public void setNumberXPoints(int xPoints);

    public int getNumberYPoints();

    public void setNumberYPoints(int yPoints);

    public int getOriginLat();

    public void setOriginLat(int originLat);

    public int getOriginLon();

    public void setOriginLon(int originLon);
}