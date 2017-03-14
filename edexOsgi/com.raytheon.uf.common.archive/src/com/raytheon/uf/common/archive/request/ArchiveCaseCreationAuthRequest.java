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
package com.raytheon.uf.common.archive.request;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Authorization request for Case Creation.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 25, 2014 2853       rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */
@DynamicSerialize
public class ArchiveCaseCreationAuthRequest extends ArchiveAdminAuthRequest {

    /** Resource property value for case directory location. */
    @DynamicSerializeElement
    private String caseDirectory;

    /** Default constructor. */
    public ArchiveCaseCreationAuthRequest() {
        super();
    }

    /**
     * Getter.
     * 
     * @return caseDirectory
     */
    public String getCaseDirectory() {
        return caseDirectory;
    }

    /**
     * Setter.
     * 
     * @param caseDirectory
     */
    public void setCaseDirectory(String caseDirectory) {
        this.caseDirectory = caseDirectory;
    }

}
