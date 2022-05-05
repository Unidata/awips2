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
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import com.raytheon.uf.common.dataplugin.gfe.db.objects.ParmID;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * DiscreteDefinition defining the contents of a DiscreteKey
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 10, 2011            randerso     Initial creation
 * Jan 05, 2015  #5184     dgilling     Added getHazardDescription.
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

@DynamicSerialize
public class DiscreteDefinition {
    private static final String SFC = "_SFC";

    @DynamicSerialize
    public static class InternalDef {
        @DynamicSerializeElement
        private boolean overlaps;

        @DynamicSerializeElement
        private int auxDataLength;

        @DynamicSerializeElement
        private List<DiscreteKeyDef> keys;

        Map<String, Integer> indexMap;

        public InternalDef() {
        }

        public InternalDef(boolean overlaps, int auxDataLength,
                List<DiscreteKeyDef> keys) {
            this.overlaps = overlaps;
            this.auxDataLength = auxDataLength;
            setKeys(keys);
        }

        /**
         * @return the overlaps
         */
        public boolean isOverlaps() {
            return overlaps;
        }

        /**
         * @param overlaps
         *            the overlaps to set
         */
        public void setOverlaps(boolean overlaps) {
            this.overlaps = overlaps;
        }

        /**
         * @return the auxDataLength
         */
        public int getAuxDataLength() {
            return auxDataLength;
        }

        /**
         * @param auxDataLength
         *            the auxDataLength to set
         */
        public void setAuxDataLength(int auxDataLength) {
            this.auxDataLength = auxDataLength;
        }

        /**
         * @return the keys
         */
        public List<DiscreteKeyDef> getKeys() {
            return keys;
        }

        /**
         * @param keys
         *            the keys to set
         */
        public void setKeys(List<DiscreteKeyDef> keys) {
            this.keys = keys;
            indexMap = new HashMap<String, Integer>();
            for (int i = 0; i < keys.size(); i++) {
                indexMap.put(keys.get(i).getSymbol(), i);
            }
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
            result = prime * result + auxDataLength;
            result = prime * result + ((keys == null) ? 0 : keys.hashCode());
            result = prime * result + (overlaps ? 1231 : 1237);
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
            InternalDef other = (InternalDef) obj;
            if (auxDataLength != other.auxDataLength) {
                return false;
            }
            if (keys == null) {
                if (other.keys != null) {
                    return false;
                }
            } else if (!keys.equals(other.keys)) {
                return false;
            }
            if (overlaps != other.overlaps) {
                return false;
            }
            return true;
        }
    }

    @DynamicSerializeElement
    private Map<String, InternalDef> definitions;

    /**
     * Default Constructor
     */
    public DiscreteDefinition() {
        this.definitions = new HashMap<String, DiscreteDefinition.InternalDef>();
    }

    /**
     * @return the definitions
     */
    public Map<String, InternalDef> getDefinitions() {
        return definitions;
    }

    /**
     * @param definitions
     *            the definitions to set
     */
    public void setDefinitions(Map<String, InternalDef> definitions) {
        this.definitions = definitions;
    }

    /**
     * Adds a new definition for the given parmNameAndLevel, the overlaps
     * allowed flag, and the list of keys.
     * 
     * @param parmNameAndLevel
     *            in the parmName_Level format.
     * @param overlaps
     * @param auxDataLength
     * @param keys
     */
    public void addDefinition(String parmNameAndLevel, boolean overlaps,
            int auxDataLength, final List<DiscreteKeyDef> keys) {

        InternalDef def = new InternalDef(overlaps, auxDataLength, keys);

        String composite = compositeName(parmNameAndLevel);
        definitions.put(composite, def);
    }

    /**
     * Returns a composite name for use in this class: parmName_level
     * 
     * @param pn
     * @return the composite name
     */
    private String compositeName(final String pn) {
        if (pn.indexOf('_') != -1) {
            return pn;
        }
        return pn + SFC;
    }

    /**
     * @param key
     * @return Removes any aux data found in the input key string.
     */
    public String stripAuxData(String key) {
        int pos = key.indexOf(':');
        if (pos != -1) {
            return key.substring(0, pos);
        } else {
            return key;
        }
    }

    /**
     * @param key
     * @return Returns the aux data from the input key string.
     */
    public String auxData(String key)

    {
        int pos = key.indexOf(':');
        if (pos != -1) {
            return key.substring(pos + 1);
        } else {
            return "";
        }
    }

    /**
     * @param pid
     * @return the valid set of keys for the given parmNameAndLevel. Returns an
     *         empty sequence if an unknown parmNameAndLevel is specified.
     */
    public List<DiscreteKeyDef> keys(ParmID pid) {
        InternalDef def = definitions.get(pid.getCompositeName());
        if (def != null) {
            return def.keys;
        }
        return new ArrayList<DiscreteKeyDef>();
    }

    /**
     * @param parmNameAndLevel
     * @return the valid set of keys for the given parmNameAndLevel. Returns an
     *         empty sequence if an unknown parmNameAndLevel is specified.
     */
    public List<DiscreteKeyDef> keys(String parmNameAndLevel) {
        InternalDef def = definitions.get(compositeName(parmNameAndLevel));
        if (def != null) {
            return def.keys;
        }
        return new ArrayList<DiscreteKeyDef>();
    }

    /**
     * Function to find the index matching the given key and parmNameAndIndex.
     * 
     * @param parmNameAndLevel
     * @param key
     * @return the index. Returns -1 if not found.
     */
    public int keyIndex(String parmNameAndLevel, String key) {
        int rval = -1;
        InternalDef def = definitions.get(compositeName(parmNameAndLevel));
        if (def != null) {
            Integer index = def.indexMap.get(stripAuxData(key));
            if (index != null) {
                rval = index.intValue();
            }
        }
        return rval;
    }

    /**
     * Function to find the index matching the given key and ParmID.
     * 
     * @param pid
     * @param key
     * @return the index. Returns -1 if not found.
     */
    int keyIndex(ParmID pid, String key) {
        return keyIndex(pid.getCompositeName(), key);
    }

    /**
     * @return list of available definitions by parmNameAndLevel.
     */
    public List<String> availableDefs() {
        return new ArrayList<String>(definitions.keySet());
    }

    /**
     * @param parmNameAndLevel
     *            in the parmName_Level format.
     * @return true if the given parmNameAndLevel is set for overlapping
     *         discrete areas. Returns false if an unknown parmNameAndLevel.
     */
    public boolean overlaps(String parmNameAndLevel) {
        InternalDef def = definitions.get(compositeName(parmNameAndLevel));
        if (def == null) {
            return false;
        }
        return def.overlaps;
    }

    /**
     * @param parmNameAndLevel
     *            in the parmName_Level format.
     * @return the maximum length of the auxiliary data field for a composite
     *         name. Returns 0 if an unknown parmNameAndLevel.
     */
    public int auxDataLength(String parmNameAndLevel) {
        InternalDef def = definitions.get(compositeName(parmNameAndLevel));
        if (def == null) {
            return 0;
        }
        return def.auxDataLength;
    }

    /**
     * @param parmNameAndLevel
     *            in the parmName_Level format.
     * @return the valid set of symbols for the given parmNameAndLevel. Returns
     *         an empty list if an unknown parmNameAndLevel is specified.
     */
    public List<String> symbols(String parmNameAndLevel) {
        InternalDef def = definitions.get(compositeName(parmNameAndLevel));
        List<String> sym = new ArrayList<String>();
        if (def != null) {
            for (DiscreteKeyDef key : def.keys) {
                sym.add(key.getSymbol());
            }
        }
        return sym;
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
        result = prime * result
                + ((definitions == null) ? 0 : definitions.hashCode());
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
        DiscreteDefinition other = (DiscreteDefinition) obj;
        if (definitions == null) {
            if (other.definitions != null) {
                return false;
            }
        } else if (!definitions.equals(other.definitions)) {
            return false;
        }
        return true;
    }

    /**
     * Function to find the symbol given a key index.
     * 
     * @param parmNameAndLevel
     * @param keyIndex
     * @return the symbol. Returns <Invalid> if not found.
     */
    public String keySymbol(String parmNameAndLevel, int keyIndex) {
        InternalDef def = definitions.get(compositeName(parmNameAndLevel));

        // The index is bigger or equal to the length of the array
        if (def.keys.size() <= keyIndex) {
            return "<Invalid>";
        } else {
            // return the entry for the given index
            return def.keys.get(keyIndex).getSymbol();
        }
    }

    /**
     * Function to find the key description matching the given type.
     * 
     * @param parmNameAndLevel
     * @param keySymbol
     * @return the description. Returns empty TextString if not found.
     */
    public String keyDesc(String parmNameAndLevel, String keySymbol) {
        for (DiscreteKeyDef key : keys(parmNameAndLevel)) {
            if (key.getSymbol().equals(keySymbol)) {
                return key.getDescription();
            }
        }
        return "";
    }

    /**
     * @param parmNameAndLevel
     * @return the default key for the given parmNameAndLevel.
     */
    public String defaultKey(String parmNameAndLevel) {
        List<DiscreteKeyDef> value = keys(compositeName(parmNameAndLevel));
        if (value.size() > 0) {
            return value.get(0).getSymbol();
        } else {
            return "";
        }
    }

    /**
     * Prints the contents of this class on the given output stream.
     * 
     * @param o
     *            the output stream
     */
    public void printOn(PrintStream o) {
        o.println(this.toString());
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("[");
        for (Entry<String, InternalDef> entry : definitions.entrySet()) {
            InternalDef def = entry.getValue();
            sb.append("P=").append(entry.getKey());
            sb.append(", O=").append(def.overlaps);
            sb.append(", AL=").append(def.auxDataLength);
            sb.append(", K=").append(def.keys);
            sb.append(",");
        }
        sb.append("]");
        return sb.toString();
    }

    /**
     * Get a hazard description for a phen.sig value. This code was essentially
     * copied from MakeHazardDialog.
     * 
     * @param parmNameAndLevel
     *            Parm name in the parmName_Level format.
     * @param phenSig
     *            The phensig whose description is needed.
     * 
     * @return the description for the phensig.
     */
    public String getHazardDescription(String parmNameAndLevel, String phenSig) {
        String sdesc = keyDesc(parmNameAndLevel, phenSig);

        if (sdesc.isEmpty()) {
            if ("CF.S".equals(phenSig)) {
                sdesc = "Coastal Flood Statement";
            } else if ("LS.S".equals(phenSig)) {
                sdesc = "Lakeshore Flood Statement";
            } else if ("MA.S".equals(phenSig)) {
                sdesc = "Marine Weather Statement";
            } else if ("HU.S".equals(phenSig)) {
                sdesc = "Hurricane Local Statement";
            } else {
                sdesc = "UNKNOWN PHENOMENON.SIGNIFICANCE";
            }
        }

        return sdesc;
    }
}
