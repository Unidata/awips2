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
package com.raytheon.uf.viz.core.rsc.capabilities;

import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Interface that provides button 2 click for legends.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Aug 23, 2007             ebabin      Initial Creation.
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1
 */
public interface IMiddleClickCapableResource {

    /**
     * Whether a resource is middle click capable. (i.e. Home tool, middle click
     * (button 2) causes a 'edit' mode.
     * 
     * @return isMiddleClickCapable.
     * @throws VizException
     */

    public void middleClicked() throws VizException;

}
