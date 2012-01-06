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
package com.raytheon.edex.msg;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.message.response.AbstractResponseMessage;

/**
 * Response Message for ArchiveSrvs status
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date         Ticket#     Engineer    Description
 *  ------------ ----------  ----------- --------------------------
 *  Dec 18, 2007 561         dfitch      Initial Creation.
 *  
 * </pre>
 * 
 * @author dfitch
 * @version 1
 */
public class ResponseMessageArchive extends AbstractResponseMessage {

    /*
     * An array list of Service
     */
    private ArrayList<Service> archiveSrvs;

    /**
     * Constructor. No argument constructor.
     */
    public ResponseMessageArchive() {
        // intentionally empty.
    }

    public ResponseMessageArchive(ArrayList<Service> lstArchiveSrv) {
        this.archiveSrvs = lstArchiveSrv;
    }

    public List<Service> getArchiveSrvs() {
        return archiveSrvs;
    }

    public void setArchiveSrvs(ArrayList<Service> archiveSrvs) {
        this.archiveSrvs = archiveSrvs;
    }

}
