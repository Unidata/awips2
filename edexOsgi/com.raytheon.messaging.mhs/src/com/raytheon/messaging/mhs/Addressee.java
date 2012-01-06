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

/**
 * The <code>Addressee</code> class represents MHS addressees, which can be
 * AWIPS site IDs, such as <code>BOX</code>, or special destinations, such as
 * <code>NWSTG</code>. Each <code>Addressee</code> object can hold a single
 * destination address.
 * <p>
 * Each MHS message must have at least one destination addressee. Addressees are
 * added to an <code>MhsMessage</code> object's {@link AddresseeList} using the
 * <code>addAddressee</code> or <code>addAckAddressee</code> methods. The only
 * difference between the two methods is that <code>addAckAddressee</code> will
 * generate an acknowledgment request while <code>addAddressee</code> will not.
 * <p>
 * Special destinations:
 * <p>
 * Below is a complete list of special addressees and their meanings. Messages
 * sent to any of these destinations will be routed to the NCF, where the
 * product ID (WMO header) will be used to determine message disposition
 * according to configuration.
 * <p>
 * <table border="1">
 * <tr>
 * <td>DEFAULTNCF</td>
 * <td>Product will be disseminated to various interfaces determined by address
 * expansion using the product ID. For instance, WWAs are disseminated via MHS
 * to all WFOs and RFC, Weather Wire, NWSTG, and over the SBN. Configuration
 * file is default_addr.data. This is usually the address used by sites when
 * they wish to have this message disseminated widely.</td>
 * </tr>
 * <tr>
 * <td>SBN</td>
 * <td>Product will be sent to the SBN over the NMC channel. Radar products will
 * be compressed prior to uplinking. Configuration file is dfltaddr.sbn_dist.</td>
 * </tr>
 * <tr>
 * <td>NWSTG</td>
 * <td>Product will be sent to the NWSTG over an interface determined by WMO
 * header. Configuration file is dfltaddr.nwstg_dist.</td>
 * </tr>
 * <tr>
 * <td>NWWS</td>
 * <td>Product will be disseminated over the Weather Wire Service. Configuration
 * file is dfltaddr.nwws>dist.</td>
 * </tr>
 * <tr>
 * <td>ALLNCF</td>
 * <td>Product will be disseminated to all WFOs and RFCs via MHS from the NCF.</td>
 * </tr>
 * <tr>
 * <td>INET</td>
 * <td>Product will be sent to the Radar interface at the NWSTG.</td>
 * </tr>
 * <tr>
 * <td>NDFD</td>
 * <td>Product will be sent to the National Digital Forecast Database interface.
 * </td>
 * </tr>
 * <tr>
 * <td>RETRANS</td>
 * <td>This is a retransmission request message that will be sent to one of the
 * SBN uplink servers at the NCF.</td>
 * </tr>
 * <tr>
 * <td>ARCHIVE</td>
 * <td>This is a request to retrieve an archived text product from the NCF
 * archive server.</td>
 * </tr>
 * </table>
 * 
 * @see AddresseeList
 * @see MhsMessage
 * 
 * @author Brian M. Rapp
 * @version 1.0
 * 
 */
public class Addressee {
    private final String address;

    private final boolean ackRequired;

    /**
     * Constructs a new <code>Addressee</code> consisting of the address string
     * and a boolean, which indicates whether or not an acknowledgment is to be
     * requested for this particular destination.
     * 
     * @param addressee
     * @param needAck
     */
    public Addressee(String addressee, boolean needAck) {
        address = addressee;
        ackRequired = needAck;
    }

    /**
     * Gets the address string.
     * 
     * @return String containing the destination address specified by this
     *         Addressee object
     */
    public String getAddress() {
        return address;
    }

    /**
     * Gets the acknowledgment request flag value.
     * 
     * @return boolean value indicating whether or not an acknowledgment is to
     *         be requested from this addressee.
     */
    public boolean isAckRequired() {
        return ackRequired;
    }
}
