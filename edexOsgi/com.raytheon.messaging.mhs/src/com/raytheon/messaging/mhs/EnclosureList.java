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

import java.util.ArrayList;

/**
 * The <code>EnclosureList</code> contains methods for managing lists of
 * <code>Enclosure</code>s for an <code>MhsMessage</code> object. Each MHS
 * message can contain an arbitrary number of <code>Enclosure</code>s.
 * 
 * @author brapp
 * @see Enclosure
 * @see MhsMessage
 * @version 1.0
 */

public class EnclosureList {
    private ArrayList<Enclosure> fileList;

    /**
     * Initializes a newly created <code>EnclosureList</code> so <code>
	 * Addressees</code>
     * can be added.
     */
    public EnclosureList() {
        fileList = new ArrayList<Enclosure>();
    }

    /**
     * Allocates a new <code>Enclosure</code>, populates it with the specified
     * enclosure file name, then adds it to the end of the list if this
     * <code>Enclosure</code> is not already in the list. Only a single
     * enclosure can be added with a call to <code>add</code>. If multiple
     * enclosures are to be added, a separate <code>add</code> call will be
     * needed for each.
     * 
     * @param fileName
     *            enclosure file name <code>String</code>.
     * @return <code>true</code> if the operation succeeded, <code>false</code>
     *         if the false parameter is <code>null</code>. If the enclosure is
     *         already attached, it will not be attached again, though
     *         <code>true</code> will still be returned.
     * @see Enclosure
     */

    public boolean add(String fileName) {
        boolean status = true;
        Enclosure enc;

        if (fileName == null) {
            fileName = "";
            status = false;
        }
        enc = new Enclosure(fileName);

        if (!fileList.contains(enc)) {
            fileList.add(enc);
        }
        return status;
    }

    /**
     * Traverses the list looking for an <code>Enclosure</code> with a path/file
     * name <code>String</code> matching the passed parameter. If found the
     * matching <code>Enclosure</code> is removed from the list.
     * 
     * @param fileName
     *            fully qualified file name <code>String</code>
     * @return <code>true</code> if the specified enclosure was found in the
     *         list and removed; <code>false</code> if the file name was not
     *         found.
     */
    public boolean remove(String fileName) {
        for (int i = 0; i < fileList.size(); i++) {
            Enclosure enc = fileList.get(i);
            if (enc.getEnclosureName().equals(fileName)) {
                fileList.remove(i);
                return true;
            }
        }
        return false;
    }

    /**
     * Removes a specific <code>Enclosure</code> by list index. This is normally
     * used in conjunction with the <code>get</code> method.
     * <p>
     * Example use: removing all Enclosure elements for non-existent files
     * 
     * <pre>
     * EnclosureList encList = new EnclosureList();
     * [...]
     * for (int i = 0; i &lt; encList.getCount(); i++) {
     * 	if (!(new File(encList.get(i).getEnclosureName()).exists)) {
     * 		encList.remove(i);
     * 	}
     * }
     * [...]
     * </pre>
     * 
     * @param i
     *            Integer index of the <code>Enclosure</code> to remove.
     * @return the removed <code>Enclosure</code> object
     * @throws IndexOutOfBoundsException
     * @see #get(int)
     */
    public Enclosure remove(int i) {
        return fileList.remove(i);
    }

    /**
     * Removes all <code>Enclosures</code> from the list.
     * 
     * @return Integer number of <code>Enclosures</code> removed.
     */
    public int removeAll() {
        int count = fileList.size();
        fileList.clear();
        return count;
    }

    /**
     * Fetches an <code>Enclosure</code> from the list by index.
     * 
     * @param i
     *            Integer index of <code>Enclosure</code> to fetch.
     * @return the fetched <code>Enclosure</code>
     * @throws IndexOutOfBoundsException
     */
    public Enclosure get(int i) {
        return fileList.get(i);
    }

    /**
     * Returns the number of <code>Enclosure</code>s in this list.
     * 
     * @return Integer number of <code>Enclosure</code>s in this list.
     */
    public int getCount() {
        return fileList.size();
    }
}
