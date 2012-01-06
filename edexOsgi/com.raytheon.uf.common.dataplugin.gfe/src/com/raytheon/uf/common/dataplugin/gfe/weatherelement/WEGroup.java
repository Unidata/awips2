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
package com.raytheon.uf.common.dataplugin.gfe.weatherelement;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.DatabaseID;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Defines a Weather Element Group (formerly "Bundle" in GFE I)
 * 
 * This is a list of WEItems to be displayed, with a human readable name.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jun 5, 2008				chammack	Initial creation
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

@XmlRootElement(name = "WEGroup")
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class WEGroup implements ISerializableObject {

    @XmlAttribute
    @DynamicSerializeElement
    private String name;

    @XmlElements({ @XmlElement(name = "WEItem", type = WEItem.class) })
    @DynamicSerializeElement
    private WEItem[] weItems;

    public WEGroup() {
        super();
    }

    public WEGroup(String name, ParmID[] parmIDs, ParmID[] availableParmIDs) {
        List<WEItem> weItemList = makeItems(parmIDs, availableParmIDs);
        this.weItems = weItemList.toArray(new WEItem[weItemList.size()]);
        this.name = name;
    }

    /**
     * @return the name
     */
    public String getName() {
        return name;
    }

    /**
     * @return the weItems
     */
    public WEItem[] getWeItems() {
        return weItems;
    }

    /**
     * @param name
     *            the name to set
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * @param weItems
     *            the weItems to set
     */
    public void setWeItems(WEItem[] weItems) {
        this.weItems = weItems;
    }

    // -- public
    // -----------------------------------------------------------------
    // Bundle::makeItems()
    // Private function called to make the SeqOf<BundleItem>. Each item contains
    // a modified ParmID string and its relative cycle model time on a separate
    // line. Each BundleItem is appended to the SeqOf<BundleItem> _bundleItems.
    // The modified ParmID string is the ParmID with the model time striped
    // (ex: Temp:DEN_type_Fcst_00000000_0000). The relative cycle number for a
    // parameter of a singleton database (Fcst, LAPS) is -1. For a parameter in
    // a version database (NAM12), the relative cycle number is 0 for the
    // latest,
    // 1 for the previous, 2 for the second previous, etc.
    // ---------------------------------------------------------------------------
    private List<WEItem> makeItems(final ParmID[] parmIDs,
            final ParmID[] availableParmIDs) {
        int relCycleNumber;
        List<WEItem> bundleItems = new ArrayList<WEItem>();

        for (int i = 0; i < parmIDs.length; i++) {
            // Get the relative cycle number for the ParmID
            relCycleNumber = getRelCycleNo(parmIDs[i], availableParmIDs);

            // Make the BundleItem
            WEItem bundleItem = new WEItem(parmIDs[i], relCycleNumber);

            // Append to the private data
            bundleItems.add(bundleItem);
        }

        return bundleItems;
    }

    // -- public
    // -----------------------------------------------------------------
    // Bundle::getRelCycleNo()
    // Private function called to get the relative cycle number for a ParmID.
    // The relative cycle number for a parameter of a singleton database (Fcst,
    // LAPS) is -1. For a parameter in a version database (NAM12), the relative
    // cycle number is 0 for the latest, 1 for the previous, 2 for the second
    // previous, etc.
    // The relative order of the current ParmID is computed by determining its
    // position in the list of sorted availableParmIDs for that ParmID.
    // ---------------------------------------------------------------------------
    public int getRelCycleNo(final ParmID parmID,
            final ParmID[] availableParmIDs) {
        // If this is a singleton database, return -1
        if (parmID.getDbId().getModelTimeAsDate().getTime() == 0) {
            return -1;
        }

        // Extract the list of ParmIDs which are the same as the current ParmID
        // (only the time is different)
        List<ParmID> extractedParmIDs = new ArrayList<ParmID>();
        int i;
        for (i = 0; i < availableParmIDs.length; i++) {
            if (parmID.getParmName().equals(availableParmIDs[i].getParmName())
                    && parmID.getParmLevel().equals(
                            availableParmIDs[i].getParmLevel())
                    && parmID
                            .getDbId()
                            .stripModelTime()
                            .equals(availableParmIDs[i].getDbId()
                                    .stripModelTime())) {
                extractedParmIDs.add(availableParmIDs[i]);
            }
        }
        // Sort in the reverse order (latest first, then older times)

        Collections.sort(extractedParmIDs);

        // Find the index for the ParmID within the list of extractedParmIDs
        for (i = 0; i < extractedParmIDs.size(); i++) {
            if (parmID.equals(extractedParmIDs.get(i))) {
                return i;
            }
        }

        return 0;
    }

    /**
     * Private function called to get the ParmID given modified ParmIDName and a
     * relative cycle number.
     */
    private ParmID getParmID(final String parmIDMod, int relCycleNo,
            final ParmID[] availableParmIDs, final String siteID) {
        ParmID pIDMod = new ParmID(parmIDMod);

        // modify the site id
        DatabaseID dbid = pIDMod.getDbId();
        dbid = new DatabaseID(siteID, dbid.getFormat(), dbid.getDbType(),
                dbid.getModelName(), dbid.getModelTime());
        pIDMod = new ParmID(pIDMod.getParmName(), dbid, pIDMod.getParmLevel());

        // Extract the list of ParmIDs which are the same as the current ParmID
        // (only the time is different)
        List<ParmID> extractedParmIDs = new ArrayList<ParmID>();
        for (int i = 0; i < availableParmIDs.length; i++) {
            if (pIDMod.getParmName().equals(availableParmIDs[i].getParmName())
                    && pIDMod.getParmLevel().equals(
                            availableParmIDs[i].getParmLevel())
                    && pIDMod
                            .getDbId()
                            .stripModelTime()
                            .equals(availableParmIDs[i].getDbId()
                                    .stripModelTime())) {
                extractedParmIDs.add(availableParmIDs[i]);
            }
        }

        // return a null ParmID is extractedParmIDs is of length 0
        if (extractedParmIDs.size() == 0) {
            return null;
        }

        // Sort in the reverse order
        Collections.sort(extractedParmIDs);

        // Return the ParmID within the list of extractedParmIDs which matches
        // the relative cycle number
        if (relCycleNo == -1) {
            return extractedParmIDs.get(0);
        } else if (relCycleNo < extractedParmIDs.size()) {
            return extractedParmIDs.get(relCycleNo);
        } else {
            return extractedParmIDs.get(extractedParmIDs.size() - 1);
        }
    }

    /**
     * This rectifies the loaded bundle for a site This should be called after
     * deJiBXing.
     * 
     * This replaces the old makeItems(File, String, ParmID[])
     * 
     * The modified ParmID string is the ParmID with the model time striped (ex:
     * Temp:DEN_type_Fcst_00000000_0000). The relative cycle number for a
     * parameter of a singleton database (Fcst, LAPS) is -1. For a parameter in
     * a version database (NAM12), the relative cycle number is 0 for the
     * latest, 1 for the previous, 2 for the second previous, etc.
     * 
     * @param siteID
     */
    public void rectifyForSite(String siteID, ParmID[] availableParmIDs) {
        for (WEItem weItem : this.weItems) {
            weItem.setParmID(getParmID(weItem.getParmID().getParmId(),
                    weItem.getRelativeCycleNumber(), availableParmIDs, siteID));
        }
    }

}
