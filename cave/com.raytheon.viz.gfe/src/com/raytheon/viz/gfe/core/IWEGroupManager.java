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

package com.raytheon.viz.gfe.core;

import java.util.List;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.dataplugin.gfe.weatherelement.WEGroup;

/**
 * Interface for the Weather Element group manager. The weather element group
 * manager manages the set of weather element groups, formerly called bundles
 * (sets of parms).
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jun 30, 2008	 #878	    chammack	Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */
public interface IWEGroupManager {

    /**
     * Public function called to save a Bundle. This function takes a Bundle
     * name, the sequence of ParmIDs to be saved, and the sequence of available
     * ParmIDs.
     * 
     * @param name
     * @param parmIDs
     * @param availableParmIDs
     */
    public abstract void save(String name, ParmID[] parmIDs,
            ParmID[] availableParmIDs);

    /**
     * Public function called to delete a Bundle. This function takes a Bundle
     * name.
     * 
     * @param name
     */
    public abstract boolean remove(String name);

    /**
     * Public function called to get the ParmIDs of a Bundle. This function
     * takes a Bundle name, the sequence of available ParmIDs, and return a list
     * of ParmIDs as saved in the Bundle.
     * 
     * @param name
     * @param availableParmIDs
     */
    public abstract ParmID[] getParmIDs(String name, ParmID[] availableParmIDs);

    /**
     * Public function called to get the ParmIDs of a Bundle. This function
     * takes bundle text, the sequence of available ParmIDs, and return a list
     * of ParmIDs as saved in the Bundle.
     */
    public abstract ParmID[] getParmIDs(final WEGroup bundle,
            final ParmID[] availableParmIDs);

    /**
     * Return a list of the available weather element groups
     * 
     * @return the available weather element groups
     */
    public abstract List<String> getInventory();

    /**
     * Returns just the user elements groups
     * 
     * @return the user elements groups
     */
    public abstract List<String> getUserInventory();

    /**
     * Retrieve the default Weather Element group
     * 
     * @return the default weather element group
     */
    public abstract String getDefaultGroup();

    /**
     * Returns whether or not a particular group is protected or not.
     * 
     * @param name
     *            The name of the weather element group to check.
     * @return True, if it is protected. False, in all other cases.
     */
    boolean isProtected(String name);
}