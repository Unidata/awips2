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
 * The <code>AddresseeList</code> contains methods for managing lists of
 * <code>Addressee</code>s for an <code>MhsMessage</code> object. Each MHS
 * message must contain at least one addressee.
 * 
 * @author brapp
 * @see Addressee
 * @see MhsMessage
 * @version 1.0
 */
public class AddresseeList {
    private ArrayList<Addressee> addressees;

    /**
     * Initializes a newly created <code>AddresseeList</code> so <code>
	 * Addressees</code>
     * can be added.
     */
    public AddresseeList() {
        addressees = new ArrayList<Addressee>();
    }

    /**
     * Allocates a new <code>Addressee</code>, populates it with the specified
     * addressee string and boolean acknowledgment flag, then adds it to the end
     * of the list if this <code>Addressee</code> is not already in the list.
     * 
     * @param addressee
     *            <code>String</code> containing a single destination.
     * @param ackRequired
     *            <code>boolean</code> flag indicating whether or not an
     *            acknowledgment will be requested from this addressee.
     * @return <code>true</code> if the operation succeeded, <code>false</code>
     *         if the addressee parameter is <code>null</code>.
     * @see Addressee
     */
    public boolean add(String addressee, boolean ackRequired) {
        boolean status = true;
        Addressee addr;

        if (addressee == null) {
            status = false;
        } else {
            addr = new Addressee(addressee, ackRequired);
            if (!addressees.contains(addr)) {
                status = addressees.add(addr);
            }
        }
        return status;
    }

    /**
     * Traverses the list looking for an <code>Addressee</code> with an
     * addressee <code>String</code> and acknowledgment flag matching the two
     * parameters. If found the matching <code>Addressee</code> is removed from
     * the list.
     * 
     * @param addressee
     *            <code>String</code> value representing the destination
     * @param ackRequired
     *            <code>boolean</code> value indicating whether the Addressee in
     *            question has the acknowledgment request flag set.
     * @return <code>true</code> if the specified addressee was found and
     *         removed; <code>
	 * false</code> if the addressee was not found.
     */
    public boolean remove(String addressee, boolean ackRequired) {
        for (int i = 0; i < addressees.size(); i++) {
            Addressee addr = addressees.get(i);
            if (addr.getAddress().equals(addressee)
                    && (addr.isAckRequired() == ackRequired)) {
                addressees.remove(i);
                return true;
            }
        }
        return false;
    }

    /**
     * Removes a specific <code>Addressee</code> by list index. This is normally
     * used in conjunction with the <code>get</code> method.
     * <p>
     * Example use: removing all addressees with the acknowledgment flag set
     * 
     * <pre>
     * AddresseeList addrList = new AddresseeList();
     * [...]
     * for (int i = 0; i &lt; addrList.getCount(); i++) {
     * 	Addressee addr = addrList.get(i);
     * 	if (addr.isAckRequired) {
     * 		addrList.remove(i);
     * 	}
     * }
     * [...]
     * </pre>
     * 
     * @param i
     *            Integer index of the <code>Addressee</code> to remove.
     * @return the removed <code>Addressee</code> object
     * @throws IndexOutOfBoundsException
     * @see #get(int)
     */
    public Addressee remove(int i) {
        return addressees.remove(i);
    }

    /**
     * Removes all <code>Addressees</code> from the list.
     * 
     * @return Integer number of <code>Addressees</code> removed.
     */
    public int removeAll() {
        int count = addressees.size();
        addressees.clear();
        return count;
    }

    /**
     * Fetches an <code>Addressee</code> from the list by index.
     * 
     * @param i
     *            Integer index of <code>Addressee</code> to fetch.
     * @return the fetched <code>Addressee</code>
     * @throws IndexOutOfBoundsException
     */
    public Addressee get(int i) {
        return addressees.get(i);
    }

    /**
     * Returns the number of <code>Addressee</code>s in this list.
     * 
     * @return Integer number of <code>Addressee</code>s in this list.
     */
    public int getCount() {
        return addressees.size();
    }
}
