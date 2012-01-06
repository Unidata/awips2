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
package com.raytheon.uf.common.dataplugin.gfe.weather;

import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import com.raytheon.uf.common.dataplugin.gfe.weather.WeatherSubKey.Ordering;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * WeatherKey is a class that encapsulates several weather sub keys. This class
 * is used to contain, encode, and decode the text strings to/from weather sub
 * keys.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 10, 2011      #8156 randerso    Re-ported from AWIPS 1
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

@DynamicSerialize
public class WeatherKey implements Comparable<WeatherKey> {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(WeatherKey.class);

    public static final char SUBKEY_SEPARATOR = '^';

    private static final ArrayList<WeatherSubKey> INVALID = new ArrayList<WeatherSubKey>(
            Arrays.asList(new WeatherSubKey()));

    private static ArrayList<WeatherSubKey> invalidSubKey() {
        return INVALID;
    }

    @DynamicSerializeElement
    private String siteId;

    private String origStr;

    @DynamicSerializeElement
    private List<WeatherSubKey> subKeys;

    /**
     * Constructor for WeatherKey class. Initializes the weather key to no
     * entries.
     * 
     */
    public WeatherKey() {
        this.subKeys = invalidSubKey();
    }

    /**
     * Constructor for WeatherKey class taking a weather text string.
     * 
     * @param siteId
     * @param wxString
     */
    public WeatherKey(String siteId, String wxString) {
        this.siteId = siteId;
        this.origStr = wxString;
        parseString(wxString);
    }

    /**
     * @param key
     */
    public WeatherKey(WeatherKey key) {
        this.siteId = key.siteId;
        this.origStr = key.origStr;
        this.subKeys = new ArrayList<WeatherSubKey>(key.getSubKeys().size());
        for (WeatherSubKey subKey : key.getSubKeys()) {
            this.subKeys.add(new WeatherSubKey(subKey));
        }
    }

    /**
     * Constructor for WeatherKey class taking an subKeys of weather sub keys.
     * 
     * @param siteId
     * @param subKeys
     */
    public WeatherKey(String siteId, List<WeatherSubKey> subKeys) {
        this.siteId = siteId;
        setSubKeys(subKeys);
    }

    /**
     * Constructor for WeatherKey taking a coded int. This performs the opposite
     * function as KeyAsInts().
     * 
     * @param siteId
     * @param codedkey
     */
    public WeatherKey(String siteId, int codedkey) {
        this(siteId, new int[] { codedkey });
    }

    /**
     * Constructor for WeatherKey taking a series of coded ints. This performs
     * the opposite function as KeyAsInts().
     * 
     * @param siteId
     * @param codedkeys
     */
    public WeatherKey(String siteId, int[] codedkeys) {
        this.siteId = siteId;

        // look at each unsigned long int
        this.subKeys = new ArrayList<WeatherSubKey>();
        for (int codedkey : codedkeys) {
            this.subKeys.add(new WeatherSubKey(siteId, codedkey));
        }

        normalize();
    }

    /**
     * Returns the a SeqOf<WxComposite>. Each WxComposite contains Strings of
     * coverage, composite weather types, and composite weather types with
     * intensity, visibility, and appended attributes. Uses the pretty versions
     * of all fields.
     * 
     * @return the composite types
     */
    public List<WxComposite> getCompositeTypes() {
        // Start with a clean slate
        String coverage;
        String prettyCoverage;
        String types;
        String typesWithInten;
        String visibility;
        String attributes;

        List<WxComposite> comps = new ArrayList<WxComposite>();

        // Find all of the coverages. Place them in a temporary SeqOf while
        // filling types and typesWithInten.
        List<String> foundCov = new ArrayList<String>();
        List<WeatherSubKey> keys = getSubKeys();

        for (WeatherSubKey keyI : keys) {
            if (!foundCov.contains(keyI.getCoverage())) {
                foundCov.add(keyI.getCoverage());
                coverage = keyI.getCoverage();
                prettyCoverage = keyI.prettyCoverage();
                types = "";
                typesWithInten = "";
                attributes = "";
                visibility = "";

                // Look at all keys that have this coverage
                for (WeatherSubKey keyJ : keys) {
                    if (coverage.equals(keyJ.getCoverage())) {
                        types = types + keyJ.getType();
                        if (keyJ.getIntensity().charAt(0) != '<'
                                && keyJ.getIntensity().charAt(0) != '(') {
                            typesWithInten = typesWithInten + keyJ.getType()
                                    + keyJ.getIntensity();
                        } else {
                            typesWithInten = typesWithInten + keyJ.getType();
                        }
                        for (String attribute : keyJ.getAttributes()) {
                            attributes = attributes + attribute + ',';
                        }

                        // Keep the smallest visibility
                        if (keyJ.getVisibility().charAt(0) != '<'
                                && keyJ.getVisibility().charAt(0) != '(') {
                            // If this is the first visibility
                            if (visibility.length() > 0
                                    && keyJ.wxDef().visibilityIndex(
                                            keyJ.getVisibility()) < keyJ
                                            .wxDef()
                                            .visibilityIndex(visibility)) {
                                visibility = keyJ.getVisibility();
                            } else if (visibility.length() == 0) {
                                visibility = keyJ.getVisibility();
                            }
                        }
                    }
                }
                // Remove the last comma
                if (attributes.length() > 0) {
                    attributes = attributes.substring(0,
                            attributes.length() - 1);
                }

                // Make the WxComposite
                WxComposite wc = new WxComposite(prettyCoverage, types,
                        typesWithInten, visibility, attributes);
                comps.add(wc);
            }
        }

        return comps;
    }

    /**
     * Parses the string to initialize this weather key.
     * 
     * @param key
     */
    public void parseString(String key) {
        // empty the current subKeys
        subKeys = new ArrayList<WeatherSubKey>();

        // separate the string into sub keys and store them in the subKeys
        if (key.length() != 0) {
            int startPos = 0; // beginning of subkey
            for (int pos = 0; pos < key.length(); pos++) {
                if (pos + 1 == key.length()
                        || key.charAt(pos + 1) == SUBKEY_SEPARATOR) {
                    if (startPos != pos) {
                        WeatherSubKey subkey = new WeatherSubKey(this.siteId,
                                key.substring(startPos, pos + 1));
                        // put the bad ones in as well
                        // so that we know that this key is invalid.
                        // if (subkey.isValid())
                        subKeys.add(subkey);
                    }
                    startPos = pos + 2; // set to next character after separator
                    pos++; // skip over processing the separator
                }
            }
        }
        // normalize to eliminate duplicates, keys that are almost identical,
        // and to place the keys in the proper order
        normalize();
    }

    @Override
    public String toString() {
        StringBuilder result = new StringBuilder();
        for (int i = 0; i < subKeys.size(); i++) {
            result.append(subKeys.get(i).subTypeAsString());
            if (i != subKeys.size() - 1) {
                result.append(SUBKEY_SEPARATOR);
            }
        }
        return result.toString();
    }

    /**
     * @return a 'pretty' (i.e. more readable) String representing the
     *         WeatherKey.
     */
    public String toPrettyString() {
        // invalid WeatherKey
        if (!isValid()) {
            return "<Invalid>";
        }

        // special case for no weather case or simple case
        if (subKeys.size() == 1) {
            return subKeys.get(0).subTypeAsPrettyString();
        }

        StringBuilder result = new StringBuilder();

        // get the composite types for the pretty string
        List<WxComposite> comps = getCompositeTypes();

        int i = 0;
        for (WxComposite comp : comps) {
            // add vis and attr
            result.append(comp.coverage());
            if (comp.typesWithInten().length() > 0) {
                result.append(' ').append(comp.typesWithInten());
            }
            if (comp.visibility().length() > 0) {
                result.append(' ').append(comp.visibility());
            }
            if (comp.attributes().length() > 0) {
                result.append(' ').append(comp.attributes());
            }
            if (i != comps.size() - 1) {
                result.append(' ');
            }
            i++;
        }

        return result.toString();
    }

    // -- public
    // -----------------------------------------------------------------
    // WeatherKey::keyAsInts()
    // Returns the WeatherKey as a series of coded shorts. This performs the
    // opposite function as the constructor taking a coded sequence.
    // -- implementation
    // ---------------------------------------------------------
    // Takes each WeatherSubKey and extracts its components using the
    // WeatherSubKey's subKeyAsInt().
    // ---------------------------------------------------------------------------
    public int[] keyAsInts() {
        int[] keysInts = new int[subKeys.size()];
        for (int i = 0; i < subKeys.size(); i++) {
            keysInts[i] = subKeys.get(i).subKeyAsInt();
        }
        return keysInts;
    }

    /**
     * Examines all of the subkeys and then normalizes them. Normalizing a
     * weather key does several things: 1) eliminates duplicates, 2) orders them
     * by the definition in WeatherSubKey, 3) eliminates closely related
     * subgroups (e.g., SCT RW- and SCT RW would be replaced with SCT RW), 6)
     * eliminates all subkeys with the none value except for one if that is the
     * only one left.
     * 
     * -- implementation
     * 
     * The ordering values are defined in WeatherSubKey. The ordering values are
     * based on the desired ordering. The individual values are added up to make
     * a composite ordering.
     * 
     * ASSUMES THAT THE NO WX CASE HAS ORDERING 0.
     * 
     * Clears the invalid string if this is valid.
     */
    private void normalize() {
        int i, j;
        // make sure that all subkeys are valid
        if (!isValid()) {
            setInvalid(); // setting WeatherKey to have one invalid
                          // WeatherSubKey
            return;
        }

        // order the sub keys from most important to not so important
        Collections.sort(subKeys, new Comparator<WeatherSubKey>() {

            @Override
            public int compare(WeatherSubKey o1, WeatherSubKey o2) {
                return o2.order() - o1.order();
            }
        });

        // eliminate all occurrences of ordering 0, which is the
        // <none><none><none>
        // case.
        // Look at whole Seq and eliminate all with subKeyasInt() == 0
        //
        for (i = subKeys.size() - 1; i >= 0; i--) {
            if (subKeys.get(i).subKeyAsInt() == 0) {
                subKeys.remove(i);
            }
        }

        // now eliminate duplicates or near duplicates. A duplicate has the
        // same ordering value. A near duplicate consists of the same weather
        // type and intensity for different coverages for which the larger
        // coverage is used, or different intensities for same weather types
        // and coverages for which the stronger intensity is used.

        // get the masks and then invert them
        Ordering ordering = WeatherSubKey.ordering();

        int coverageMask = ~ordering.coverageMask;
        int intenMask = ~ordering.intensityMask;
        int visMask = ~ordering.visibilityMask;
        int attrMask = ~ordering.attributesMask;

        for (i = 0; i < subKeys.size(); i++) // base value for comparisons
        {
            boolean removed = false;
            WeatherSubKey base = subKeys.get(i); // for efficiency
            j = i + 1; // start index for compares

            // eliminate exact duplicates
            while (j < subKeys.size()) {
                if (base != subKeys.get(j)) {
                    break;
                } else {
                    subKeys.remove(j);
                }
            }

            // eliminate nearly duplicate intensities
            if (!removed) {
                j = i + 1;
                while (j < subKeys.size()) {
                    if ((base.order() & intenMask) != (subKeys.get(j).order() & intenMask)) {
                        break;
                    } else {
                        subKeys.remove(j);
                        removed = true;
                    }
                }
            }

            // eliminate different coverages for the same wx type and intensity
            // and visibility
            // j is still set from above
            // keep highest intensity
            if (!removed) {
                j = i + 1;
                while (j < subKeys.size()) {
                    if ((base.order() & coverageMask) != (subKeys.get(j)
                            .order() & coverageMask)) {
                        j++;
                    } else {
                        subKeys.remove(j);
                        removed = true;
                    }
                }
            }

            // eliminate different visibilities for the same wx type and
            // intensity and coverage
            // keep the lowest visibility
            if (!removed) {
                j = i + 1;
                while (j < subKeys.size()) {
                    if ((base.order() & visMask) != (subKeys.get(j).order() & visMask)) {
                        j++;
                    } else {
                        subKeys.remove(i);
                        removed = true;
                    }
                }
            }

            // eliminate different visibilities and intensities for the same wx
            // type and coverage
            // keep the lowest visibility and the largest intensity
            if (!removed) {
                j = i + 1;
                while (j < subKeys.size()) {
                    if ((base.order() & visMask & intenMask) != (subKeys.get(j)
                            .order() & visMask & intenMask)) {
                        j++;
                    } else {
                        subKeys.remove(j);
                        removed = true;
                    }
                }
            }

            // eliminate different attributes for the same wx type, intensity,
            // coverage, visibility
            // append the attributes
            if (!removed) {
                j = i + 1;
                // List<String> existingAttrs;

                while (j < subKeys.size()) {
                    // List<String> existingAttrs;
                    if ((base.order() & attrMask) != (subKeys.get(j).order() & attrMask)) {
                        j++;
                    } else {
                        List<String> attrs = base.getAttributes();
                        for (String attr : subKeys.get(j).getAttributes()) {
                            // append only new attributes
                            if (!attrs.contains(attr)) {
                                attrs.add(attr);
                            }
                        }
                        subKeys.get(i).setAttributes(attrs);
                        subKeys.remove(j);
                        removed = true;
                    }
                }
            }
        }

        // now if there are no keys left in the subKeys, append a none,none,none
        // key back in
        // appends a none key
        // WON'T WORK IF WXDEFINITION NOT SET IN WEATHER SUB KEY
        // NEED TO KNOW THE NO WX CASE.
        if (subKeys.size() == 0) {
            int coded = 0;
            subKeys.add(new WeatherSubKey(this.siteId, coded));
        }
    }

    /**
     * Sets a WeatherKey as Invalid.
     * 
     */
    public void setInvalid() {
        // reset the subKeys of WeatherSubKey to length 0
        subKeys = invalidSubKey();
    }

    /**
     * @return true if there are no invalid subkeys in this WeatherKey.
     */
    public boolean isValid() {
        for (WeatherSubKey subKey : subKeys) {
            if (!subKey.isValid()) {
                return false;
            }
        }

        return true;
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

    /**
     * @return the origStr
     */
    public String getOrigStr() {
        return origStr;
    }

    /**
     * @return the sub keys in this weather key.
     */
    public List<WeatherSubKey> getSubKeys() {
        return Collections.unmodifiableList(subKeys);
    }

    /**
     * @param subKeys
     *            the subKeys to set
     */
    public void setSubKeys(List<WeatherSubKey> subKeys) {
        this.subKeys = new ArrayList<WeatherSubKey>(subKeys.size());
        for (WeatherSubKey subKey : subKeys) {
            if (!this.siteId.equals(subKey.getSiteId())) {
                throw new IllegalArgumentException("WeatherSubKey siteId ("
                        + subKey.getSiteId()
                        + ") must match WeatherKey siteId (" + this.siteId
                        + ")");
            }
            this.subKeys.add(new WeatherSubKey(subKey));
        }
        normalize();
    }

    /**
     * Outputs the WeatherKey as a formatted string using toString().
     * 
     * If this object is invalid, then it should also output "<Invalid>".
     * 
     * @param o
     *            the stream
     * @return the stream
     */
    public PrintStream printOn(PrintStream o) {
        o.print(toString());

        return o;
    }

    /**
     * Returns a set of valid sub keys based on the input description string.
     * The input description string can consist of up to five space-separated
     * strings in the following order: coverage type intensity visibility and
     * optional attributes. If there aren't five strings, then the trailing
     * strings are interpreted as wildcards (all options). An '*' in any string
     * indicates a wildcard. If a string is surrounded by brackets, then there
     * are multiple entries in that string. For example, [RW,SW] indicates two
     * types, RW and SW. A comma separates the interior string.
     * 
     * Example: [Sct,Iso] [RW,SW] * DmgWnd Coverage of Sct or Iso. Type of RW or
     * SW. Any visibility. Optional attribute of DmgWnd.
     * 
     * @param string
     * @return the valid sub keys
     */
    static public List<WeatherSubKey> descriptionSubKeys(String siteId,
            String string) {
        // extract the token strings for the 5 categories
        String[] tokens = string.split(" ");
        int l = tokens.length;
        if (l < 5) {
            tokens = Arrays.copyOf(tokens, 5);

            // make remainder wildcards
            while (l < 5) {
                tokens[l] = "*";
            }
        }

        for (int i = 0; i < tokens.length; i++) {
            statusHandler.handle(Priority.DEBUG, "Descriptive Token " + i
                    + " [" + tokens[i] + ']');
        }

        // now extract the token strings for the subcategories. These
        // are filtered based on type for validity.
        // Token order: COV TYPE INTEN VIS ATTR
        // Expecting a * for all, [] for list, or no [] for a single entry
        List<String> type = decodeTypeDescription(siteId, tokens[1]);
        List<String> inten = decodeIntenDescription(siteId, tokens[2], type);
        List<String> cov = decodeCovDescription(siteId, tokens[0], type);
        List<String> attr = decodeAttrDescription(siteId, tokens[4], type);
        List<String> vis = decodeVisDescription(siteId, tokens[3]);

        statusHandler.handle(Priority.DEBUG, "Decoded Types: " + type);
        statusHandler.handle(Priority.DEBUG, "Decoded Cov: " + cov);
        statusHandler.handle(Priority.DEBUG, "Decoded Inten: " + inten);
        statusHandler.handle(Priority.DEBUG, "Decoded Vis: " + vis);
        statusHandler.handle(Priority.DEBUG, "Decoded Attr: " + attr);

        // now attempt all combinations of the type, cov, inten, vis, attr
        // and make subkeys for each valid one. We make copies of the
        // arrays for performance rather than calling the weather sub key
        // functions inside the loops. Attributes are handled differently.
        // A special weather sub key is added containing no attributes and
        // multiple with combinations of all attributes. Since visibilities
        // are not type-dependent,
        // there is no need to modify or filter its list.
        List<WeatherSubKey> keys = new ArrayList<WeatherSubKey>();
        for (int t = 0; t < type.size(); t++) {
            statusHandler.handle(Priority.DEBUG,
                    "Processing type = " + type.get(t));
            List<String> acov = WeatherSubKey.availableCoverages(siteId,
                    type.get(t));
            List<String> ainten = WeatherSubKey.availableIntensities(siteId,
                    type.get(t));
            List<String> aattr = WeatherSubKey.availableAttributes(siteId,
                    type.get(t));

            // need to find all possible combinations of the attributes for type
            List<String> typeAttributes = new ArrayList<String>();
            for (int a = 0; a < attr.size(); a++) {
                if (aattr.contains(attr.get(a))) {
                    typeAttributes.add(attr.get(a));
                }
            }
            statusHandler.handle(Priority.DEBUG, "Filtered attributes: "
                    + typeAttributes);

            // these are the combos
            List<List<String>> attributeCombos = new ArrayList<List<String>>();
            attributeCombos.add(new ArrayList<String>()); // include no
                                                          // attributes
            int numPossibleCombos = (int) Math.pow(2.0, aattr.size());
            statusHandler.handle(Priority.DEBUG, "NumPossibleCombos: "
                    + numPossibleCombos);
            for (int a = 0; a < numPossibleCombos; a++) {
                List<String> combo = new ArrayList<String>();
                for (int b = 0; b < aattr.size(); b++) {
                    if (((a >> b) & 1) != 0) {
                        combo.add(aattr.get(b));
                    }
                }

                List<String> temp = new ArrayList<String>(typeAttributes);
                temp.retainAll(combo);
                if (temp.size() > 0) {
                    attributeCombos.add(combo);
                }
            }
            statusHandler.handle(Priority.DEBUG, "Number actual combinations: "
                    + attributeCombos.size());
            statusHandler.handle(Priority.DEBUG,
                    "Filtered attribute combinations: " + attributeCombos);

            for (int c = 0; c < cov.size(); c++) {
                statusHandler.handle(Priority.DEBUG, "Processing Coverage = "
                        + cov.get(c));
                if (!acov.contains(cov.get(c))) {
                    // illegal coverage for this type
                    continue;
                }
                for (int i = 0; i < inten.size(); i++) {
                    statusHandler.handle(Priority.DEBUG,
                            "Processing Intensity = " + inten.get(i));
                    if (!ainten.contains(inten.get(i))) {
                        // illegal intensity for this type and coverage
                        continue;
                    }
                    for (int v = 0; v < vis.size(); v++) {
                        for (int a = 0; a < attributeCombos.size(); a++) {
                            keys.add(new WeatherSubKey(siteId, cov.get(c), type
                                    .get(t), inten.get(i), vis.get(v),
                                    attributeCombos.get(a)));
                        }
                        statusHandler.handle(Priority.DEBUG,
                                "Added: " + keys.get(keys.size() - 1));
                    }
                }
            }
        }

        statusHandler.handle(Priority.DEBUG,
                "Number of returned keys: " + keys.size());
        return keys;
    }

    /**
     * Decodes the weather type description for descriptionSubKeys(). Parses the
     * type string, which may be "*" for all types, abc for a single type, or
     * "[abc,def,ghi]" for multiple types. Validates each entry.
     * 
     * @param string
     * @return list of types from string
     */
    private static List<String> decodeTypeDescription(String siteId,
            String string) {
        List<String> type;
        type = new ArrayList<String>();
        if (string.equals("*")) {
            type = WeatherSubKey.availableWxTypes(siteId);
        } else if (string.contains("[")) {

            // start at the first position (past the '[')
            int lastPos = 1;
            int pos;
            while ((pos = string.indexOf(',', lastPos)) != -1) {
                String t = string.substring(lastPos, pos);
                if (WeatherSubKey.availableWxTypes(siteId).contains(t)) {
                    type.add(t);
                } else {
                    statusHandler.handle(Priority.PROBLEM, "Illegal WxType ["
                            + string + "] in " + " descriptionSubKeys");
                }
                lastPos = pos + 1;
            }
            // get the final one
            String f = string.substring(lastPos, string.length() - 1);
            if (WeatherSubKey.availableWxTypes(siteId).contains(f)) {
                type.add(f);
            } else {
                statusHandler.handle(Priority.PROBLEM, "Illegal WxType ["
                        + string + "] in " + " descriptionSubKeys");
            }
        } else if (WeatherSubKey.availableWxTypes(siteId).contains(string)) {
            // only a single type if no []
            type.add(string);
        } else {
            statusHandler.handle(Priority.PROBLEM, "Illegal WxType [" + string
                    + "] in " + " descriptionSubKeys");
        }
        return type;
    }

    /**
     * Decodes the weather visibility description for descriptionSubKeys().
     * Parses the vis string, which may be "*" for all vis, abc for a single
     * vis, or "[abc,def,ghi]" for multiple vis. Validates each entry.
     * 
     * @param string
     * @return list of visibilities from string
     */
    private static List<String> decodeVisDescription(String siteId,
            String string) {
        List<String> vis = new ArrayList<String>();
        int pos = 0, lastPos = 0;
        if (string.equals("*")) {
            vis = WeatherSubKey.availableVisibilities(siteId);
        } else if (string.contains("[")) {
            lastPos = 1; // start at the first position (past the '[')
            while ((pos = string.indexOf(",", lastPos)) != -1) {
                String t = string.substring(lastPos, pos);
                if (WeatherSubKey.availableVisibilities(siteId).contains(t)) {
                    vis.add(t);
                } else {
                    statusHandler.handle(Priority.PROBLEM, "Illegal WxVis ["
                            + string + "] in " + " descriptionSubKeys");
                }
                lastPos = pos + 1;
            }
            // get the final one
            String f = string.substring(lastPos, string.length() - 1);
            if (WeatherSubKey.availableVisibilities(siteId).contains(f)) {
                vis.add(f);
            } else {
                statusHandler.handle(Priority.PROBLEM, "Illegal WxVis ["
                        + string + "] in " + " descriptionSubKeys");
            }
        } else if (WeatherSubKey.availableVisibilities(siteId).contains(string)) {
            vis.add(string); // only a single vis if no []
        } else {
            statusHandler.handle(Priority.PROBLEM, "Illegal WxVis [" + string
                    + "] in " + " descriptionSubKeys");
        }
        return vis;
    }

    /**
     * Decodes the weather intensity description for descriptionSubKeys().
     * Parses the intensity string, which may be "*" for all types, abc for a
     * single type, or "[abc,def,ghi]" for multiple types. Validates each entry.
     * The types is the listing of selected weather types which are used in the
     * validation.
     * 
     * @param string
     * @param types
     * @return list of intensities for the selected types
     */
    private static List<String> decodeIntenDescription(String siteId,
            String string, List<String> types) {
        boolean found;
        int i, j;
        List<String> inten = new ArrayList<String>();
        int pos = 0, lastPos = 0;
        if (string.equals("*")) {
            // include only those intensities valid for types, eliminate dups
            for (i = 0; i < types.size(); i++) {
                List<String> aInten = WeatherSubKey.availableIntensities(
                        siteId, types.get(i));
                for (j = 0; j < aInten.size(); j++) {
                    if (!inten.contains(aInten.get(j))) {
                        inten.add(aInten.get(j));
                    }
                }
            }
        } else if (string.contains("[")) {
            lastPos = 1; // start at the first position (past the '[')
            while ((pos = string.indexOf(",", lastPos)) != -1) {
                found = false;
                String t = string.substring(lastPos, pos);
                for (i = 0; i < types.size(); i++) {
                    if (WeatherSubKey
                            .availableIntensities(siteId, types.get(i))
                            .contains(t)) {
                        inten.add(t);
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    statusHandler.handle(Priority.PROBLEM, "Illegal WxInten ["
                            + string + "] in " + " descriptionSubKeys");
                }
                lastPos = pos + 1;
            }
            // get the final one
            String f = string.substring(lastPos, string.length() - 1);
            found = false;
            for (i = 0; i < types.size(); i++) {
                if (WeatherSubKey.availableIntensities(siteId, types.get(i))
                        .contains(f)) {
                    inten.add(f);
                    found = true;
                    break;
                }
            }
            if (!found) {
                statusHandler.handle(Priority.PROBLEM, "Illegal WxInten ["
                        + string + "] in " + " descriptionSubKeys");
            }
        } else // single entry
        {
            found = false;
            for (i = 0; i < types.size(); i++) {
                if (WeatherSubKey.availableIntensities(siteId, types.get(i))
                        .contains(string)) {
                    inten.add(string);
                    found = true;
                    break;
                }
            }
            if (!found) {
                statusHandler.handle(Priority.PROBLEM, "Illegal WxInten ["
                        + string + "] in " + " descriptionSubKeys");
            }
        }
        return inten;
    }

    /**
     * Decodes the weather coverage description for descriptionSubKeys(). Parses
     * the coverage string, which may be "*" for all types, abc for a single
     * type, or "[abc,def,ghi]" for multiple types. Validates each entry. The
     * types is the listing of selected weather types which are used in the
     * validation.
     * 
     * @param string
     * @param types
     * @return the list of coverages for the selected types
     */
    private static List<String> decodeCovDescription(String siteId,
            String string, List<String> types) {
        boolean found;
        int i, j;
        List<String> cov = new ArrayList<String>();
        int pos = 0, lastPos = 0;
        if (string.equals("*")) {
            // include only those coverages valid for types, eliminate dups
            for (i = 0; i < types.size(); i++) {
                List<String> aCov = WeatherSubKey.availableCoverages(siteId,
                        types.get(i));
                for (j = 0; j < aCov.size(); j++) {
                    if (!cov.contains(aCov.get(j))) {
                        cov.add(aCov.get(j));
                    }
                }
            }
        } else if (string.contains("[")) {
            lastPos = 1; // start at the first position (past the '[')
            while ((pos = string.indexOf(",", lastPos)) != -1) {
                found = false;
                String t = string.substring(lastPos, pos);
                for (i = 0; i < types.size(); i++) {
                    if (WeatherSubKey.availableCoverages(siteId, types.get(i))
                            .contains(t)) {
                        cov.add(t);
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    statusHandler.handle(Priority.PROBLEM, "Illegal WxCov ["
                            + string + "] in " + " descriptionSubKeys");
                }
                lastPos = pos + 1;
            }
            // get the final one
            String f = string.substring(lastPos, string.length() - 1);
            found = false;
            for (i = 0; i < types.size(); i++) {
                if (WeatherSubKey.availableCoverages(siteId, types.get(i))
                        .contains(f)) {
                    cov.add(f);
                    found = true;
                    break;
                }
            }
            if (!found) {
                statusHandler.handle(Priority.PROBLEM, "Illegal WxCov ["
                        + string + "] in " + " descriptionSubKeys");
            }
        } else // single entry
        {
            found = false;
            for (i = 0; i < types.size(); i++) {
                if (WeatherSubKey.availableCoverages(siteId, types.get(i))
                        .contains(string)) {
                    cov.add(string);
                    found = true;
                    break;
                }
            }
            if (!found) {
                statusHandler.handle(Priority.PROBLEM, "Illegal WxCov ["
                        + string + "] in " + " descriptionSubKeys");
            }
        }
        return cov;
    }

    /**
     * Decodes the weather attribute description for descriptionSubKeys().
     * Parses the coverage string, which may be "*" for NO types, abc for a
     * single type, or "[abc,def,ghi]" for multiple types. Validates each entry.
     * The types is the listing of selected weather types which are used in the
     * validation. Note that attributes are handled differently. If * is
     * selected, that means that no selection has been made and the attribute
     * list is empty.
     * 
     * @param string
     * @param types
     * @return the list of attributes for the selected types
     */
    private static List<String> decodeAttrDescription(String siteId,
            String string, List<String> types) {
        boolean found;
        int i;
        List<String> attr = new ArrayList<String>();
        if (string.equals("*")) {
            return attr; // * in this case means no selections at all
        } else if (string.contains("[")) {
            int lastPos = 1; // start at the first position (past the '[')
            int pos;
            while ((pos = string.indexOf(",", lastPos)) != -1) {
                found = false;
                String t = string.substring(lastPos, pos);
                for (i = 0; i < types.size(); i++) {
                    if (WeatherSubKey.availableAttributes(siteId, types.get(i))
                            .contains(t)) {
                        attr.add(t);
                        found = true;
                        break;
                    }
                }
                if (!found) {
                    statusHandler.handle(Priority.PROBLEM, "Illegal WxAttr ["
                            + string + "] in " + " descriptionSubKeys");
                }
                lastPos = pos + 1;
            }
            // get the final one
            String f = string.substring(lastPos);
            found = false;
            for (i = 0; i < types.size(); i++) {
                if (WeatherSubKey.availableAttributes(siteId, types.get(i))
                        .contains(f)) {
                    attr.add(f);
                    found = true;
                    break;
                }
            }
            if (!found) {
                statusHandler.handle(Priority.PROBLEM, "Illegal WxAttr ["
                        + string + "] in " + " descriptionSubKeys");
            }
        } else // single entry
        {
            found = false;
            for (i = 0; i < types.size(); i++) {
                if (WeatherSubKey.availableAttributes(siteId, types.get(i))
                        .contains(string)) {
                    attr.add(string);
                    found = true;
                    break;
                }
            }
            if (!found) {
                statusHandler.handle(Priority.PROBLEM, "Illegal WxAttr ["
                        + string + "] in " + " descriptionSubKeys");
            }
        }
        return attr;
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
        WeatherKey other = (WeatherKey) obj;
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
     * Adds all subkeys from the supplied WeatherKey to this WeatherKey
     * 
     * @param rhs
     */
    public void addAll(WeatherKey rhs) {
        if (!this.siteId.equals(rhs.siteId)) {
            throw new IllegalArgumentException("WeatherKey siteId ("
                    + rhs.siteId + ") must match this siteId (" + this.siteId
                    + ")");
        }

        subKeys.addAll(rhs.subKeys);
        normalize();
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Comparable#compareTo(java.lang.Object)
     */
    @Override
    public int compareTo(WeatherKey rhs) {
        if (this.equals(rhs)) {
            return 0;
        } else if (this.less(rhs)) {
            return -1;
        } else {
            return 1;
        }
    }

    private boolean less(WeatherKey rhs) {
        int pairedLength = Math.min(subKeys.size(), rhs.subKeys.size());
        // compare orders for the common length
        for (int i = 0; i < pairedLength; i++) {
            int thisOrder = subKeys.get(i).order();
            int rhsOrder = rhs.subKeys.get(i).order();
            int delta = thisOrder - rhsOrder;
            if (delta < 0) {
                return true;
            } else if (delta > 0) {
                return false;
            }
        }
        if (subKeys.size() < rhs.subKeys.size()) {
            return true; // the rhs is longer, therefore this is less than
        } else {
            return false; // this is longer, therefore this is >=
        }
    }

    public WeatherSubKey get(int index) {
        return subKeys.get(index);
    }
}
