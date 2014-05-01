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
package com.raytheon.uf.viz.archive.ui;

import java.io.File;
import java.io.IOException;
import java.util.concurrent.atomic.AtomicBoolean;

import com.raytheon.uf.common.archive.config.DisplayData;

/**
 * Interface for copying source files/directories to the desired type of
 * destination.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 21, 2013 2225       rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public interface ICaseCopy {

    /**
     * Prepare copier for sending display data's archive and category
     * selections.
     * 
     * @param caseDir
     *            - top level case directory file
     * @param displayData
     *            - data preparing to move
     * @param shutdown
     *            - Flag to check for orderly shudown
     * @throws CaseCreateException
     */
    public void startCase(File caseDir, DisplayData displayData,
            AtomicBoolean shutdown) throws CaseCreateException;

    /**
     * A source to copy.
     * 
     * @param source
     * @throws IOException
     */
    public void copy(File source) throws CaseCreateException;

    /**
     * Finish the move process for the current archive and category.
     * 
     */
    public void finishCase() throws CaseCreateException;
}
