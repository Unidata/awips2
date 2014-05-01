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
package com.raytheon.uf.common.localization;

/**
 * Observer class to listen for changes on LocalizationFile objects. Observers
 * will be notified of changes to files with levels higher than the level of
 * file registered on. For example registering as an observer on a BASE file
 * will give update messages for files at all other LocalizationLevels since
 * BASE is the lowest
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * May 20, 2008				randerso	Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public interface ILocalizationFileObserver {

    /**
     * LocalizationFile being listened on was updated. It is imported to check
     * the context of the message
     * 
     * @param message
     */
    public abstract void fileUpdated(FileUpdatedMessage message);

}
