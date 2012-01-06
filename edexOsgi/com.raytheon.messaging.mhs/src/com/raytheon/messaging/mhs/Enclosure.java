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

package com.raytheon.messaging.mhs;

import java.io.File;

/**
 * The <code>Enclosure</code> class represents MHS file enclosures. Each
 * <code>Enclosure</code> object can hold a single fully qualified file name.
 * <p>
 * Each MHS message can contain an arbitrary number of enclosures, or none at
 * all. Enclosures are added to an <code>MhsMessage</code> object's
 * {@link EnclosureList} using the <code>add</code> method.
 * 
 * @see EnclosureList
 * @see MhsMessage
 * 
 * @author Brian M. Rapp
 * @version 1.0
 */

public class Enclosure {
    private String enclosureName;

    private long enclosureSize;

    /**
     * Constructs a new <code>Enclosure</code> consisting of a fully qualified
     * file name (path/filename).
     * 
     * @param fileName
     *            <code>String</code> containing the fully qualified file name
     *            of the enclosure.
     */
    public Enclosure(String fileName) {
        File file = new File(fileName);

        enclosureName = fileName;
        enclosureSize = file.length();
    }

    /**
     * Gets the enclosure file name.
     * 
     * @return String containing the fully qualified file name specified by this
     *         <code>Enclosure</code> object.
     */
    public String getEnclosureName() {
        return enclosureName;
    }

    /**
     * Gets the size of the enclosure file in bytes.
     * 
     * @return <code>long</code> number of bytes in the enclosure file.
     */
    public long getEnclosureSize() {
        return enclosureSize;
    }
}
