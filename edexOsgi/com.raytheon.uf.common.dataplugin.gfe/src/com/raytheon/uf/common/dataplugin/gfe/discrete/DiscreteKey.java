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
package com.raytheon.uf.common.dataplugin.gfe.discrete;

import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Encapsulation of multiple sub keys that describe a discrete element.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 10, 2011            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

@DynamicSerialize
public class DiscreteKey implements Comparable<DiscreteKey> {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DiscreteKey.class);

    /**
     * between sub keys
     * 
     */
    public static final char SUBKEY_SEPARATOR = '^';

    /**
     * between key and aux data
     * 
     */
    public static final char AUXDATA_SEPARATOR = ':';

    private static Map<String, DiscreteDefinition> discreteDefinition;

    public static DiscreteDefinition discreteDefinition(String siteId) {
        if (discreteDefinition == null) {
            return null;
        }
        return discreteDefinition.get(siteId);
    }

    public static synchronized void setDiscreteDefinition(String siteId,
            DiscreteDefinition def) {
        if (discreteDefinition == null) {
            discreteDefinition = new HashMap<String, DiscreteDefinition>();
        }
        discreteDefinition.put(siteId, def);
    }

    @DynamicSerializeElement
    private String siteId;

    private String origStr;

    @DynamicSerializeElement
    private List<String> subKeys;

    @DynamicSerializeElement
    private ParmID parmID;

    private int[] indexes;

    /**
     * Constructor for DiscreteKey class. Initializes the discrete key to no
     * entries.
     * 
     */
    public DiscreteKey() {
        this.subKeys = new ArrayList<String>();
        this.subKeys.add("<Invalid>");
        this.indexes = new int[] { 0 };
    }

    /**
     * Constructor for DiscreteKey class taking a text string.
     * 
     * -- implementation
     * 
     * parseString() is called to initialize the DiscreteKey.
     * 
     * @param dString
     * @param parmID
     */
    public DiscreteKey(String siteId, String dString, ParmID parmID) {
        this.siteId = siteId;
        this.parmID = parmID;
        parseString(dString);
    }

    /**
     * @param key
     */
    public DiscreteKey(DiscreteKey key) {
        this.siteId = key.siteId;
        this.parmID = key.parmID;
        this.subKeys = new ArrayList<String>(key.getSubKeys());
        this.indexes = Arrays.copyOf(key.indexes, key.indexes.length);
    }

    /**
     * Constructor for DiscreteKey class taking an array of discrete sub keys.
     * 
     * -- implementation
     * 
     * The List<String> is initialized using the copy constructor and
     * normalize() is called to clean up the DiscreteKey.
     * 
     * @param subKeys
     * @param parmID
     */
    public DiscreteKey(String siteId, List<String> subKeys, ParmID parmID) {
        this.siteId = siteId;
        this.parmID = parmID;
        setSubKeys(subKeys);
    }

    /**
     * @return the siteId
     */
    public String getSiteId() {
        return siteId;
    }

    /**
     * @param siteId
     *            the siteId to set
     */
    public void setSiteId(String siteId) {
        this.siteId = siteId;
    }

    public String getOrigStr() {
        return origStr;
    }

    /**
     * @return the subKeys
     */
    public List<String> getSubKeys() {
        return subKeys;
    }

    /**
     * @param subKeys
     *            the subKeys to set
     */
    public void setSubKeys(List<String> subKeys) {
        this.subKeys = new ArrayList<String>(subKeys);
        normalize();
    }

    /**
     * @return the parmID
     */
    public ParmID getParmID() {
        return parmID;
    }

    /**
     * @param parmID
     *            the parmID to set
     */
    public void setParmID(ParmID parmID) {
        this.parmID = parmID;
    }

    // -- private
    // -----------------------------------------------------------------
    // DiscreteKey::parseString()
    // Parses the string to initialize this discrete key.
    // -- implementation
    // ----------------------------------------------------------
    // The discrete text string is separated into individual sub keys, stored
    // in the discrete sub key array, and then normalized using normalize().
    // -----------------------------------------------------------------------------
    /**
     * Parses the string to initialize this discrete key.
     * 
     * @param key
     */
    private void parseString(String key) {
        // empty the current array
        this.subKeys = new ArrayList<String>();

        // separate the string into sub keys and store them in the array
        int startPos = 0; // beginning of subkey
        int pos = 0;
        while (startPos < key.length()) {
            pos = key.indexOf(SUBKEY_SEPARATOR, startPos);
            if (pos != -1) {
                this.subKeys.add(key.substring(startPos, pos));
                startPos = pos + 1;
            } else {
                this.subKeys.add(key.substring(startPos));
                break;
            }
        }

        // normalize to eliminate duplicates, keys that are almost identical,
        // and to place the keys in the proper order
        normalize();
    }

    /**
     * @return the discreteDefinition for this key
     */
    public DiscreteDefinition discreteDefinition() {
        return discreteDefinition(this.siteId);
    }

    /**
     * Examines all of the subkeys and then normalizes them. Normalizing a
     * discrete key does several things: 1) eliminates duplicates, 2) orders
     * them by the definition in DiscreteSubKey, 3) eliminates all subkeys with
     * the none value except for one if that is the only one left.
     * 
     * -- implementation
     * 
     * The ordering values are defined in DiscreteSubKey. The ordering values
     * are based on the desired ordering. The individual values are added up to
     * make a composite ordering.
     */
    private void normalize() {
        String pnL = getParmID().getCompositeName();

        // overlaps allowed?
        boolean overlaps = discreteDefinition().overlaps(pnL);

        // reorder the sub keys from most important to not so important
        // 1st keys in definition are the most important

        for (int i = 0; i < subKeys.size() - 1; i++) {
            for (int j = i + 1; j < subKeys.size(); j++) {
                int index1 = discreteDefinition().keyIndex(getParmID(),
                        subKeys.get(i));
                int index2 = discreteDefinition().keyIndex(getParmID(),
                        subKeys.get(j));
                if (index1 == -1 || index2 == -1) {
                    setInvalid();
                    return;
                }
                if (index1 > index2) {
                    String k = subKeys.get(i);
                    subKeys.set(i, subKeys.get(j));
                    subKeys.set(j, k);
                }
            }
        }

        // now eliminate duplicates. A duplicate has the
        // same ordering value.

        for (int i = 0; i < subKeys.size(); i++) // base value for comparisons
        {
            String base = subKeys.get(i);
            int j = i + 1; // start index for compares

            // eliminate exact duplicates
            while (j < subKeys.size()) {
                if (!base.equals(subKeys.get(j))) {
                    break;
                } else {
                    subKeys.remove(j);
                }
            }
        }

        // in the case of overlaps, eliminate all <None> keys
        if (overlaps) {
            for (int i = subKeys.size() - 1; i >= 0; i--) {
                if (subKeys.get(i).equals("<None>")) {
                    subKeys.remove(i);
                }
            }
            if (subKeys.size() == 0) {
                subKeys.add("<None>");
            } // ensure something is here
        }

        // make sure that all subkeys are valid
        if (!isValid()) {
            // setting DiscreteKey to have invalid subkey
            setInvalid();
        }

        this.indexes = new int[subKeys.size()];
        int i = 0;
        for (String subKey : subKeys) {
            indexes[i++] = discreteDefinition().keyIndex(getParmID(), subKey);
        }
    }

    /**
     * Sets a DiscreteKey as Invalid.
     */
    public void setInvalid() {
        statusHandler.handle(Priority.PROBLEM,
                "Invalid Key found for DISCRETE: " + subKeys);

        // reset the _array to 1 entry with <Invalid>
        subKeys.clear();
        subKeys.add("<Invalid>");
    }

    /**
     * @return true if there are no subkeys in this DiscreteKey.
     */
    public boolean isValid() {
        // search keys to ensure they are part of definition
        List<DiscreteKeyDef> keys = discreteDefinition().keys(
                getParmID().getCompositeName());
        for (int i = 0; i < subKeys.size(); i++) {
            String base = baseData(siteId, subKeys.get(i));
            boolean found = false;
            for (int j = 0; j < keys.size(); j++) {
                if (keys.get(j).getSymbol().equals(base)) {
                    found = true;
                    break;
                }
            }
            if (!found) {
                return false;
            }
        }

        // check for overlaps when overlaps not permitted
        if (subKeys.size() > 1
                && !discreteDefinition().overlaps(
                        getParmID().getCompositeName())) {
            return false;
        } // multiple keys in non-overlapping situation

        // check for aux data lengths
        int auxLength = discreteDefinition().auxDataLength(
                getParmID().getCompositeName());
        for (int i = 0; i < subKeys.size(); i++) {
            if (auxData(siteId, subKeys.get(i)).length() > auxLength) {
                return false;
            }
        }

        if (subKeys.size() == 0) {
            return false;
        }

        return true;
    }

    // -- public
    // -----------------------------------------------------------------
    // DiscreteKey::descriptionSubKeys()
    // Returns a set of valid sub keys based on the input description string.
    // The input description string consist of one space-separated
    // string. A '*' in the string indicates a wildcard. If a string
    // is surrounded by brackets, then there are multiple entries in that
    // string.
    // For example, [A,B] indicates two values , A and B. A comma separates
    // the interior string.
    //
    // Example: [A,B,C]
    // Types of A, B, C.
    // -- implementation
    // ---------------------------------------------------------
    // Static function.
    // ---------------------------------------------------------------------------
    /**
     * DiscreteKey::descriptionSubKeys() Returns a set of valid sub keys based
     * on the input description string. The input description string consist of
     * one space-separated string. A '*' in the string indicates a wildcard. If
     * a string is surrounded by brackets, then there are multiple entries in
     * that string. For example, [A,B] indicates two values , A and B. A comma
     * separates the interior string.
     * 
     * Example: [A,B,C]
     * 
     * Types of A, B, C.
     * 
     * @param string
     * @return the valid subkeys
     */
    public List<String> descriptionSubKeys(String string) {
        List<DiscreteKeyDef> keys = discreteDefinition().keys(
                getParmID().getCompositeName());
        List<String> allkeys = new ArrayList<String>();
        for (DiscreteKeyDef key : keys) {
            allkeys.add(key.getSymbol());
        }

        int pos = 0, lastPos = 0;
        if (string.equals("*")) {
            return allkeys;
        }

        List<String> type = new ArrayList<String>();
        if ((pos = string.indexOf('[')) != -1) {
            lastPos = 1; // start at the first position (past the '[')
            while ((pos = string.indexOf(',', lastPos)) != -1) {
                String t = string.substring(lastPos, pos);
                if (allkeys.contains(t)) {
                    type.add(t);
                } else {
                    statusHandler.handle(Priority.PROBLEM, "Illegal Key ["
                            + string + "] in " + " descriptionSubKeys");
                }
                lastPos = pos + 1;
            }

            // get the final one
            String f = string.substring(lastPos, string.length() - 1);
            if (allkeys.contains(f)) {
                type.add(f);
            } else {
                statusHandler.handle(Priority.PROBLEM, "Illegal Key [" + string
                        + "] in " + " descriptionSubKeys");
            }
        }

        else if (allkeys.contains(string)) {
            type.add(string); // only a single type if no []
        } else {
            statusHandler.handle(Priority.PROBLEM, "Illegal Key [" + string
                    + "] in " + " descriptionSubKeys");
        }

        return type;
    }

    /**
     * @return the individual key indexes for this Discrete Key.
     */
    public int[] keyIndexes() {
        int[] indexes = new int[subKeys.size()];
        int i = 0;
        for (String subKey : subKeys) {
            indexes[i++] = discreteDefinition().keyIndex(
                    getParmID().getCompositeName(), subKey);
        }
        return indexes;
    }

    // -- public
    // -----------------------------------------------------------------
    // DiscreteKey::defaultKey()
    // Returns the default key for the given ParmID.
    // -- implementation
    // ---------------------------------------------------------
    // ---------------------------------------------------------------------------
    public static DiscreteKey defaultKey(String siteId, ParmID id) {
        String key = discreteDefinition(siteId).defaultKey(
                id.getCompositeName());
        return new DiscreteKey(siteId, key, id);
    }

    /**
     * @param subkey
     * @return the auxiliary data field for the supplied subkey.
     */
    public static String auxData(String siteId, String subkey) {
        return discreteDefinition(siteId).auxData(subkey);
    }

    /**
     * Base data is without the aux data.
     * 
     * @param subkey
     * @return the base data field for the supplied subkey.
     */
    public static String baseData(String siteId, String subkey) {
        return discreteDefinition(siteId).stripAuxData(subkey);
    }

    /**
     * Outputs information about this object.
     * 
     * Outputs the DiscreteKey as a formatted string using keyAsString(). If
     * this object is invalid, then it should also output "<Invalid>".
     * 
     * @param o
     *            the stream
     * @return the stream
     */
    public PrintStream printOn(PrintStream o) {
        o.print(toString());
        return o;
    }

    @Override
    public String toString() {
        StringBuilder result = new StringBuilder();
        for (int i = 0; i < subKeys.size(); i++) {
            result.append(subKeys.get(i));
            if (i != subKeys.size() - 1) {
                result.append(SUBKEY_SEPARATOR);
            }
        }
        return result.toString();
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#hashCode()
     */
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((parmID == null) ? 0 : parmID.hashCode());
        result = prime * result + ((siteId == null) ? 0 : siteId.hashCode());
        result = prime * result + ((subKeys == null) ? 0 : subKeys.hashCode());
        return result;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        DiscreteKey other = (DiscreteKey) obj;
        if (parmID == null) {
            if (other.parmID != null) {
                return false;
            }
        } else if (!parmID.equals(other.parmID)) {
            return false;
        }
        if (siteId == null) {
            if (other.siteId != null) {
                return false;
            }
        } else if (!siteId.equals(other.siteId)) {
            return false;
        }
        if (subKeys == null) {
            if (other.subKeys != null) {
                return false;
            }
        } else if (!subKeys.equals(other.subKeys)) {
            return false;
        }
        return true;
    }

    /**
     * @param index
     * @return the inedexed subkey
     */
    public String get(int index) {
        return subKeys.get(index);
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Comparable#compareTo(java.lang.Object)
     */
    @Override
    public int compareTo(DiscreteKey rhs) {
        if (this.less(rhs)) {
            return -1;
        } else if (this.equals(rhs)) {
            return 0;
        }
        return 1;
    }

    private boolean less(DiscreteKey rhs) {
        if (!getParmID().getParmName().equals(rhs.getParmID().getParmName())) {
            throw new IllegalArgumentException("");
        }

        int pairedLength = Math.min(subKeys.size(), rhs.subKeys.size());
        for (int i = 0; i < pairedLength; i++) {
            // compare base data
            int index1 = indexes[i];
            int index2 = rhs.indexes[i];
            if (index1 < index2) {
                return true;
            } else if (index1 > index2) {
                return false;
            }

            // compare aux data
            int auxCompare = auxData(siteId, subKeys.get(i)).compareTo(
                    auxData(siteId, rhs.subKeys.get(i)));
            if (auxCompare < 0) {
                return true;
            } else if (auxCompare > 0) {
                return false;
            }
        }

        if (subKeys.size() < rhs.subKeys.size()) {
            // the rhs is longer, therefore this is less than
            return true;
        }

        return false;
    }

    /**
     * Adds all subkeys from the supplied DiscreteKey to this DiscreteKey
     * 
     * @param rhs
     */
    public void addAll(DiscreteKey rhs) {
        if (!this.getSiteId().equals(rhs.getSiteId())
                || !getParmID().equals(rhs.getParmID())) {
            throw new IllegalArgumentException("DiscreteKey siteId ("
                    + rhs.getSiteId() + ") and parmID (" + rhs.getParmID()
                    + ") must match this siteId (" + this.getSiteId()
                    + ") and parmID (" + this.getParmID() + ")");
        }

        subKeys.addAll(rhs.subKeys);
        normalize();
    }

    /**
     * @param k1
     * @param k2
     * @return a DiscreteKey which is composed of all of the subkeys in k1 and
     *         k2.
     */
    public static DiscreteKey combine(DiscreteKey k1, DiscreteKey k2) {
        DiscreteKey rVal = new DiscreteKey(k1);
        rVal.addAll(k2);
        return rVal;
    }
}
