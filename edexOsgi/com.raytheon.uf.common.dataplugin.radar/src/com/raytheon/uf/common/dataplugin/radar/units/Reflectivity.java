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
package com.raytheon.uf.common.dataplugin.radar.units;

import javax.measure.quantity.Quantity;
import javax.measure.unit.Unit;

/**
 * 
 * Reflectivity Quantity
 * 
 * <pre>
 *   
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    Jul 27, 2007             chammack    Initial Creation.
 *    Oct 09, 2007 465         randerso    Moved to radar plugin.
 *   
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public interface Reflectivity extends Quantity {

	public final static Unit<Reflectivity> UNIT = RadarUnits.DBZ;

}
