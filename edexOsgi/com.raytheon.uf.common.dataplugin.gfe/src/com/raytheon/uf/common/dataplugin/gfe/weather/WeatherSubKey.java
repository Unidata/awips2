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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.gfe.serialize.WeatherSubKeyAdapter;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeTypeAdapter;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * The WeatherSubKey is a structure containing three enumerations. The first is
 * the coverage, the second the weather type, and the third the intensity. It is
 * used to contain the decoded weather key into its three components. The enum
 * ordering for Coverage, Intensity, and WxType are very important. They are
 * used to determine ordering priority. Coverage has values of 1000, WxType
 * values of 10, and Intensities values of 1.
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
@DynamicSerializeTypeAdapter(factory = WeatherSubKeyAdapter.class)
public class WeatherSubKey {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(WeatherSubKey.class);

    private class Indices {

        private int typeIndex;

        private int covIndex;

        private int intenIndex;

        private int visIndex;

        private List<Integer> attrIndex;

        public Indices(int typeIndex, int covIndex, int intenIndex,
                int visIndex, List<Integer> attrIndex) {
            this.typeIndex = typeIndex;
            this.covIndex = covIndex;
            this.intenIndex = intenIndex;
            this.visIndex = visIndex;
            this.attrIndex = attrIndex;
        }

    }

    public static class Ordering {
        public final int typeMask;

        public final int coverageMask;

        public final int intensityMask;

        public final int visibilityMask;

        public final int attributesMask;

        private Ordering() {
            typeMask = TypeMask;
            coverageMask = CoverageMask;
            intensityMask = IntensityMask;
            visibilityMask = VisibilityMask;
            attributesMask = AttributesMask;
        }
    }

    private static Map<String, WxDefinition> wxDefinition;

    public static final char PARAMETER_SEPARATOR = ':';

    public static final char ATTRIBUTE_SEPARATOR = ',';

    private static final String INVALID = "<Invalid>";

    private static String invalidStr() {
        return INVALID;
    }

    private static final int CoverageShift = 27;

    private static final int TypeShift = 20;

    private static final int IntensityShift = 16;

    private static final int VisibilityShift = 12;

    private static final int AttributesShift = 0;

    private static final int CoverageMask = 0xF8000000;

    private static final int TypeMask = 0x07F00000;

    private static final int IntensityMask = 0x000F0000;

    private static final int VisibilityMask = 0x0000F000;

    private static final int AttributesMask = 0x00000FFF;

    private static final Ordering ORDERING = new Ordering();

    // number of possible attributes
    private static final int NumAttributeBits = 12;

    // private static final int NumCoverageBits = 5;
    // private static final int NumTypeBits = 7;
    // private static final int NumIntensityBits = 4;
    // private static final int NumVisibilityBits = 4;

    /**
     * Get the weather definition for the desired site
     * 
     * @param siteId
     * @return the weather definition
     */
    public static WxDefinition wxDef(String siteId) {
        if (wxDefinition == null) {
            return null;
        }
        return wxDefinition.get(siteId);
    }

    /**
     * Sets the WxDefinition to be used for the WeatherKey/WeatherSubKey class.
     * The WxDefinition must be set if coded values are to be used, or if the
     * validity of the sub key is to be checked.
     * 
     * @param wxDef
     */
    public static synchronized void setWxDefinition(String siteId,
            WxDefinition wxDef) {
        if (wxDefinition == null) {
            wxDefinition = new HashMap<String, WxDefinition>();
        }
        wxDefinition.put(siteId, wxDef);
    }

    private String siteId;

    private String coverage;

    private String type;

    private String intensity;

    private String visibility;

    private List<String> attributes;

    private boolean intCached;

    private int intCache;

    /**
     * Constructor for WeatherSubKey taking no arguments. This results in a key
     * that is invalid.
     */
    public WeatherSubKey() {
        coverage = INVALID;
        type = INVALID;
        intensity = INVALID;
        visibility = INVALID;
        attributes = Arrays.asList(INVALID);
        intCached = false;
    }

    /**
     * Constructor for WeatherSubKey taking individual strings for the coverage,
     * intensity, weather type, visibility, and attributes.
     * 
     * @param coverage
     * @param type
     * @param intensity
     * @param visibility
     * @param attributes
     */
    public WeatherSubKey(String siteId, String coverage, String type,
            String intensity, String visibility, List<String> attributes) {
        this.siteId = siteId;
        this.coverage = coverage;
        this.type = type;
        this.intensity = intensity;
        this.visibility = visibility;
        this.attributes = new ArrayList<String>(attributes);
        this.intCached = false;
    }

    public WeatherSubKey(WeatherSubKey subKey) {
        this.siteId = subKey.siteId;
        this.coverage = subKey.coverage;
        this.type = subKey.type;
        this.intensity = subKey.intensity;
        this.visibility = subKey.visibility;
        this.attributes = new ArrayList<String>(subKey.attributes);
        this.intCached = subKey.intCached;
        this.intCache = subKey.intCache;
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
     * @return the coverage
     */
    public String getCoverage() {
        return coverage;
    }

    /**
     * @param coverage
     *            the coverage to set
     */
    public void setCoverage(String coverage) {
        this.coverage = coverage;
    }

    /**
     * @return the type
     */
    public String getType() {
        return type;
    }

    /**
     * @param type
     *            the type to set
     */
    public void setType(String type) {
        this.type = type;
    }

    /**
     * @return the intensity
     */
    public String getIntensity() {
        return intensity;
    }

    /**
     * @param intensity
     *            the intensity to set
     */
    public void setIntensity(String intensity) {
        this.intensity = intensity;
    }

    /**
     * @return the visibility
     */
    public String getVisibility() {
        return visibility;
    }

    /**
     * @param visibility
     *            the visibility to set
     */
    public void setVisibility(String visibility) {
        this.visibility = visibility;
    }

    /**
     * @return the attributes
     */
    public List<String> getAttributes() {
        return attributes;
    }

    /**
     * @param attributes
     *            the attributes to set
     */
    public void setAttributes(List<String> attributes) {
        this.attributes = attributes;
    }

    /**
     * Constructor for WeatherSubKey taking a coded number. This initializes a
     * WeatherSubKey with the appropriate values for coverage, intensity,
     * weather type, visibility, and attributes.
     * 
     * @param coded
     */
    public WeatherSubKey(String siteId, int coded) {
        intCached = false;

        this.siteId = siteId;
        WxDefinition wxDefinition = wxDef(siteId);
        if (wxDefinition == null) {
            setInvalid();
            return;
        }

        // extract the bit fields from the code
        int coverageBit = (coded & CoverageMask) >> CoverageShift;
        int typeBit = (coded & TypeMask) >> TypeShift;
        int intensityBit = (coded & IntensityMask) >> IntensityShift;
        int visibilityBit = (coded & VisibilityMask) >> VisibilityShift;
        int attrBit = (coded & AttributesMask) >> AttributesShift;

        this.type = wxDefinition.typeSymbol(typeBit);
        this.coverage = wxDefinition.coverageSymbol(typeBit, coverageBit);
        this.intensity = wxDefinition.intensitySymbol(typeBit, intensityBit);
        this.visibility = wxDefinition.visibilitySymbol(visibilityBit);

        // get attributes
        this.attributes = new ArrayList<String>();
        if (typeBit < wxDefinition.getWeatherTypes().size()) {
            for (int i = 0; i < NumAttributeBits; i++) {
                if ((attrBit & (1 << i)) != 0) {
                    this.attributes.add(wxDefinition
                            .attributeSymbol(typeBit, i));
                }
            }
        }

        if (this.coverage.equals(invalidStr())
                || this.type.equals(invalidStr())
                || this.intensity.equals(invalidStr())
                || this.visibility.equals(invalidStr())
                || this.attributes.contains(invalidStr())) {
            statusHandler.handle(Priority.DEBUG,
                    "Unable to decode coded number since no WxDefinition set"
                            + " or invalid coded number");
            statusHandler.handle(Priority.DEBUG, "Code: " + coded);
            setInvalid();
        }
    }

    /**
     * Constructor taking a text string representing a sub type.
     * 
     * @param subType
     */
    public WeatherSubKey(String siteId, String subType) {
        this.intCached = false;
        this.siteId = siteId;

        parseString(subType);
        if (!isValid()) {
            setInvalid();
        }
    }

    /**
     * @return the coverage as a pretty string.
     */
    public String prettyCoverage() {
        if (coverage.indexOf('<') != -1 || coverage.indexOf('(') != -1) {
            return "";
        } else {
            return coverage;
        }
    }

    /**
     * @return the weather type as a pretty string.
     */
    public String prettyWxType() {
        if (type.indexOf('<') != -1 || type.indexOf('(') != -1) {
            return "<NoWx>";
        } else {
            return type;
        }
    }

    /**
     * @return the intensity as a pretty string.
     */
    public String prettyIntensity() {
        if (intensity.indexOf('<') != -1 || intensity.indexOf('(') != -1) {
            return "";
        } else {
            return intensity;
        }
    }

    /**
     * @return the visibility as a pretty string.
     */
    public String prettyVisibility() {
        if (visibility.indexOf('<') != -1 || visibility.indexOf('(') != -1) {
            return "";
        } else {
            return visibility;
        }
    }

    /**
     * @return the attributes as a pretty string.
     */
    public String prettyAttributes() {
        StringBuilder pretty = new StringBuilder();
        // optional attributes
        for (int i = 0; i < attributes.size(); i++) {
            if (i == 0) {
                pretty.append(" [");
            } else {
                pretty.append(',');
            }
            pretty.append(attributes.get(i));
        }
        if (attributes.size() != 0) {
            pretty.append(']');
        }
        return pretty.toString();
    }

    /**
     * @return the WeatherSubKey as a text string.
     */
    public String subTypeAsString() {
        StringBuilder builder = new StringBuilder();
        builder.append(this.coverage).append(PARAMETER_SEPARATOR);
        builder.append(this.type).append(PARAMETER_SEPARATOR);
        builder.append(this.intensity).append(PARAMETER_SEPARATOR);
        builder.append(this.visibility).append(PARAMETER_SEPARATOR);

        // make the attributes string
        for (int i = 0; i < this.attributes.size(); i++) {
            if (i != 0) {
                builder.append(ATTRIBUTE_SEPARATOR);
            } // add a comma before all entries but the 1st
            builder.append(this.attributes.get(i));
        }

        return builder.toString();
    }

    /**
     * @return the WeatherSubKey as a coded unsigned long. (Requires that a
     *         WxDefinition has been defined and that the key is valid.) Returns
     *         0xFFFFFFFF on an error.
     */
    public int subKeyAsInt() {
        if (intCached) {
            return intCache;
        }

        Indices indices = okay();
        if (indices == null) {
            statusHandler.handle(Priority.DEBUG,
                    "subKeyAsInt called with invalid subkey or "
                            + "no wxDefinition");
            return 0xFFFFFFFF;
        }

        // convert the attribute indexes into a bit mask
        int attrMask = 0;
        for (int i = 0; i < indices.attrIndex.size(); i++) {
            attrMask |= (1 << indices.attrIndex.get(i));
        }

        this.intCache = (indices.typeIndex << TypeShift)
                | (indices.covIndex << CoverageShift)
                | (indices.intenIndex << IntensityShift)
                | (indices.visIndex << VisibilityShift)
                | (attrMask << AttributesShift);

        this.intCached = true;
        return this.intCache;
    }

    /**
     * Puts together the text string. There is a space between the coverage and
     * the type/intensity. There are no characters between the type and
     * intensity. Uses the pretty versions of the accessors.
     * 
     * @return the WeatherSubKey as a 'pretty' (i.e. more readable) text string.
     */
    public String subTypeAsPrettyString() {
        // special case for no weather
        if (type.indexOf('<') != -1) {
            return prettyWxType();
        }

        StringBuilder pretty = new StringBuilder();
        if (prettyCoverage().length() != 0) {
            pretty.append(prettyCoverage()).append(' ');
        }
        if (prettyVisibility().length() != 0) {
            pretty.append(prettyVisibility()).append(' ');
        }
        pretty.append(prettyWxType());
        pretty.append(prettyIntensity());
        pretty.append(prettyAttributes());

        return pretty.toString();
    }

    /**
     * Returns a List<String> containing all available coverages as Text Strings
     * for a given weather type. If no weather definition has been assigned,
     * then this routine returns an empty sequence.
     * 
     * @param wxType
     * @return available coverages
     */
    public static List<String> availableCoverages(String siteId, String wxType) {
        WxDefinition wxDefinition = wxDef(siteId);

        List<String> strings = new ArrayList<String>();
        int index = wxDefinition.typeIndex(wxType);

        if (index == -1) {
            return strings;
        } else {
            List<WeatherCoverage> cov = wxDefinition.getWeatherTypes()
                    .get(index).getWeatherCoverages();
            for (int i = 0; i < cov.size(); i++) {
                strings.add(cov.get(i).getSymbol());
            }
            return strings;
        }
    }

    /**
     * Returns a List<String> containing all available attributes as Text
     * Strings for a given weather type. If no weather definition has been
     * assigned, then this routine returns an empty sequence.
     * 
     * @param wxType
     * @return available attributes
     */
    public static List<String> availableAttributes(String siteId, String wxType) {
        WxDefinition wxDefinition = wxDef(siteId);

        List<String> strings = new ArrayList<String>();
        int index = wxDefinition.typeIndex(wxType);
        if (index == -1) {
            return strings;
        } else {
            List<WeatherAttribute> attr = wxDefinition.getWeatherTypes()
                    .get(index).getWeatherAttributes();
            for (int i = 0; i < attr.size(); i++) {
                strings.add(attr.get(i).getSymbol());
            }
            return strings;
        }
    }

    /**
     * Returns a SeqOf<TextString> containing all available weather types. If no
     * weather definition has been assigned, then this routine returns an empty
     * sequence.
     * 
     * @return available types
     */
    public static List<String> availableWxTypes(String siteId) {
        WxDefinition wxDefinition = wxDef(siteId);

        List<String> available = new ArrayList<String>();
        for (int i = 0; i < wxDefinition.getWeatherTypes().size(); i++) {
            available.add(wxDefinition.getWeatherTypes().get(i).getSymbol());
        }
        return available;
    }

    /**
     * Returns a List<String> containing all available intensities for a given
     * weather type. If no weather definition has been assigned, then this
     * routine returns an empty sequence.
     * 
     * @param wxType
     * @return available intensities
     */
    public static List<String> availableIntensities(String siteId, String wxType) {
        WxDefinition wxDefinition = wxDef(siteId);

        List<String> strings = new ArrayList<String>();
        int index = wxDefinition.typeIndex(wxType);
        if (index == -1) {
            return strings;
        } else {
            List<WeatherIntensity> inten = wxDefinition.getWeatherTypes()
                    .get(index).getWeatherIntensities();
            for (int i = 0; i < inten.size(); i++) {
                strings.add(inten.get(i).getSymbol());
            }
            return strings;
        }
    }

    /**
     * Returns a List<String> containing all available visibilities. If no
     * weather definition has been assigned, then this routine returns an empty
     * sequence.
     * 
     * @return available visibilities
     */
    public static List<String> availableVisibilities(String siteId) {
        WxDefinition wxDefinition = wxDef(siteId);

        List<String> available = new ArrayList<String>();
        for (int i = 0; i < wxDefinition.getWeatherVisibilities().size(); i++) {
            available.add(wxDefinition.getWeatherVisibilities().get(i)
                    .getSymbol());
        }
        return available;
    }

    /**
     * The ordering is defined as the bit-mask order and therefore, the ordering
     * is simply the coded value.
     * 
     * @return the ordering number for this particular weather sub key.
     */
    public int order() {
        // get the key in bit form
        int key = subKeyAsInt();

        // The type value is backwards. So, subtract it from
        // (NumTypeBits) and put it back.
        int tmp = (TypeMask >> TypeShift) - ((key & TypeMask) >> TypeShift);
        tmp = tmp << TypeShift;

        // Clean out the type bits in key.
        // Add in the "inverted" type bits.
        key = (key & (~TypeMask)) + tmp;

        return key;
    }

    private void badFormat(String subkey) {
        statusHandler.handle(Priority.PROBLEM, "Bad Subkey Format: [" + subkey
                + ']');
        setInvalid();
    }

    /**
     * @return false if any of the parameters is not defined.
     */
    public boolean isValid() {
        if (siteId == null) {
            return false;
        }

        // get the indexes
        return okay() != null;
    }

    /**
     * Sets this sub key invalid.
     * 
     */
    private void setInvalid() {
        coverage = type = intensity = visibility = invalidStr();
        attributes = Arrays.asList(invalidStr());
        intCached = false;
    }

    /**
     * Validates the subkey values against the WxDefinition. Returns the indexes
     * in the WxDefinition that have been indexOf. Returns true if the
     * components are valid, false otherwise.
     * 
     * @return
     */
    private Indices okay() {
        WxDefinition wxDefinition = wxDef(this.siteId);
        if (wxDefinition == null) {
            return null;
        }

        // get the indexes
        int typeIndex = wxDefinition.typeIndex(type);
        int covIndex = wxDefinition.coverageIndex(type, coverage);
        int intenIndex = wxDefinition.intensityIndex(type, intensity);
        int visIndex = wxDefinition.visibilityIndex(visibility);
        List<Integer> attrIndex = new ArrayList<Integer>();

        for (String attr : attributes) {
            attrIndex.add(wxDefinition.attributeIndex(type, attr));
        }

        // check for any errors
        if (typeIndex == -1 || covIndex == -1 || intenIndex == -1
                || visIndex == -1 || attrIndex.contains(-1)) {
            return null;
        } else {
            return new Indices(typeIndex, covIndex, intenIndex, visIndex,
                    attrIndex);
        }
    }

    /**
     * Parses the specified string and initializes this WeatherSubKey. In the
     * event of a parse error, then a warning is produced.
     * 
     * -- implementation
     * 
     * Parses the string looking for the separator. The first part is decoded
     * into the coverage, the second into the weather type, the third into the
     * intensity, the fourth into the visibility, and the fifth into the
     * optional attributes. The two separators are defined as part of this
     * class.
     * 
     * @param subType
     */
    private void parseString(String subType) {
        // nothing to process
        if (subType.length() == 0) {
            setInvalid();
            return;
        }

        // COVERAGE, TYPE, INTEN, VIS
        int covPos = 0;
        int typePos = subType.indexOf(PARAMETER_SEPARATOR, covPos);
        if ((typePos == -1) || (typePos == (subType.length() - 1))) {
            badFormat(subType);
            return;
        }

        int intenPos = subType.indexOf(PARAMETER_SEPARATOR, typePos + 1);
        if ((intenPos == -1) || (intenPos == (subType.length() - 1))) {
            badFormat(subType);
            return;
        }
        int visPos = subType.indexOf(PARAMETER_SEPARATOR, intenPos + 1);
        if ((visPos == -1) || (visPos == (subType.length() - 1))) {
            badFormat(subType);
            return;
        }

        int attPos = subType.indexOf(PARAMETER_SEPARATOR, visPos + 1);
        if (attPos == -1) {
            badFormat(subType);
            return;
        } else {
            coverage = subType.substring(covPos, typePos);
            type = subType.substring(typePos + 1, intenPos);
            intensity = subType.substring(intenPos + 1, visPos);
            visibility = subType.substring(visPos + 1, attPos);
        }

        // OPTIONAL ATTRIBUTES
        attributes = new ArrayList<String>();
        if (attPos != subType.length() - 1) {
            int pos = subType.indexOf(ATTRIBUTE_SEPARATOR, attPos + 1);
            while (pos != -1) {
                attributes.add(subType.substring(attPos + 1, pos));
                attPos = pos;
                pos = subType.indexOf(ATTRIBUTE_SEPARATOR, attPos + 1);
            }
            // last optional attribute
            String lastOne = subType.substring(attPos + 1);

            if (lastOne.length() > 0
                    && (!lastOne.equals(WxDefinition.noAttr()))) {
                attributes.add(lastOne);
            }
        }

        return;
    }

    /**
     * @return masks for the type, coverage, intensity, visibility, and
     *         attributes for the ordering.
     */
    public static Ordering ordering() {
        return ORDERING;
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return subTypeAsString();
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
                + ((attributes == null) ? 0 : attributes.hashCode());
        result = prime * result
                + ((coverage == null) ? 0 : coverage.hashCode());
        result = prime * result
                + ((intensity == null) ? 0 : intensity.hashCode());
        result = prime * result + ((siteId == null) ? 0 : siteId.hashCode());
        result = prime * result + ((type == null) ? 0 : type.hashCode());
        result = prime * result
                + ((visibility == null) ? 0 : visibility.hashCode());
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
        WeatherSubKey other = (WeatherSubKey) obj;
        if (attributes == null) {
            if (other.attributes != null) {
                return false;
            }
        } else if (!attributes.equals(other.attributes)) {
            return false;
        }
        if (coverage == null) {
            if (other.coverage != null) {
                return false;
            }
        } else if (!coverage.equals(other.coverage)) {
            return false;
        }
        if (intensity == null) {
            if (other.intensity != null) {
                return false;
            }
        } else if (!intensity.equals(other.intensity)) {
            return false;
        }
        if (siteId == null) {
            if (other.siteId != null) {
                return false;
            }
        } else if (!siteId.equals(other.siteId)) {
            return false;
        }
        if (type == null) {
            if (other.type != null) {
                return false;
            }
        } else if (!type.equals(other.type)) {
            return false;
        }
        if (visibility == null) {
            if (other.visibility != null) {
                return false;
            }
        } else if (!visibility.equals(other.visibility)) {
            return false;
        }
        return true;
    }

    public WxDefinition wxDef() {
        return wxDefinition.get(this.siteId);
    }
}
