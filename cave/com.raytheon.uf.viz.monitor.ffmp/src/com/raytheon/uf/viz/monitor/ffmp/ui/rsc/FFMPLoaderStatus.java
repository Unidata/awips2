package com.raytheon.uf.viz.monitor.ffmp.ui.rsc;

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

import com.raytheon.uf.viz.monitor.ffmp.ui.rsc.FFMPDataLoader.LOADER_TYPE;

/**
 * Loader status for FFMP
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 04/23/10     4494        D. Hladky   Initial release
 * 02/01/13     1569        D. Hladky   Added constants
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1
 */

public class FFMPLoaderStatus {

    private LOADER_TYPE loaderType = null;

    private String message = null;

    private boolean isDone = false;

    public FFMPLoaderStatus(LOADER_TYPE loaderType, String message,
            boolean isDone) {
        this.loaderType = loaderType;
        this.message = message;
        this.isDone = isDone;
    }

    public LOADER_TYPE getLoaderType() {
        return loaderType;
    }

    public void setLoaderName(LOADER_TYPE loaderType) {
        this.loaderType = loaderType;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    public boolean isDone() {
        return isDone;
    }

    public void isDone(boolean isDone) {
        this.isDone = isDone;
    }

}
