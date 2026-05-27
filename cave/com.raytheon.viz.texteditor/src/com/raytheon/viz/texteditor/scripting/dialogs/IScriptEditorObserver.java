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
package com.raytheon.viz.texteditor.scripting.dialogs;

import java.nio.file.Path;

import com.raytheon.viz.texteditor.scripting.runner.ITextWsScriptController;

/**
 * Interface for Script Editor Observer
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Jul 07, 2009           mfegan    Initial creation
 * Nov 05, 2018  6804     tgurney   executeTextScript return the script
 *                                  controller
 * Feb 26, 2019  7746     randerso  Change to use Path instead of String for
 *                                  TextWS script path.
 * Mar 04, 2019  7601     tgurney   Cleanup
 *
 * </pre>
 *
 * @author mfegan
 */

public interface IScriptEditorObserver {
    ITextWsScriptController executeTextScript(Path scriptPath);

    void setShowScriptOutput(boolean visible);

    void scriptEditorClosed();
}
