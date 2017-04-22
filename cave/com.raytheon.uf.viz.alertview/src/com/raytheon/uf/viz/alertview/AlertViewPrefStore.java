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
package com.raytheon.uf.viz.alertview;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

/**
 * 
 * An OSGi service interface that supplies configuration information to
 * AlertView components.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Jun 17, 2015  4474     bsteffen  Initial creation
 * Aug 18, 2015  3806     njensen   Renamed config stream methods
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public interface AlertViewPrefStore {

    public InputStream openConfigInputStream(String fileName)
            throws IOException;

    public OutputStream openConfigOutputStream(String fileName)
            throws IOException;

    public void addListener(AlertViewPrefListener listener);

    public void removeListener(AlertViewPrefListener listener);

    public static interface AlertViewPrefListener {

        public void prefFileChanged(String fileName);

    }

}
