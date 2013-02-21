package com.raytheon.uf.edex.ebxml.util;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.xml.datatype.XMLGregorianCalendar;

import oasis.names.tc.ebxml.regrep.xsd.rim.v4.AnyValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.BooleanValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.CollectionValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.DateTimeValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.DurationValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.FloatValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.IntegerValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.InternationalStringType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.InternationalStringValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.LocalizedStringType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.MapValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.SlotValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.StringValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.ValueType;
import oasis.names.tc.ebxml.regrep.xsd.rim.v4.VocabularyTermValueType;
import oasis.names.tc.ebxml.regrep.xsd.rs.v4.RegistryExceptionType;

/**
 * Utility class containing functions commonly used with ebXML.
 * 
 * @author jsherida
 */
public class EbxmlUtil {

    public static List<String> UPDATE_MODES = new ArrayList<String>();
    static {
        UPDATE_MODES.add("Insert");
        UPDATE_MODES.add("Update");
        UPDATE_MODES.add("Delete");
    }

    /**
     * Create an {@link InternationalStringType} from the given string.
     * Currently this method assumes the "en-US" language.
     * 
     * @param string
     *            The string to convert.
     * @return An internationalized string object.
     */
    public static InternationalStringType getIntlString(String string) {
        oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectFactory rimFactory = new oasis.names.tc.ebxml.regrep.xsd.rim.v4.ObjectFactory();

        InternationalStringType intlString = rimFactory
                .createInternationalStringType();

        LocalizedStringType localString = rimFactory
                .createLocalizedStringType();
        localString.setLang("en-US");
        localString.setValue(string);

        List<LocalizedStringType> localizedList = intlString
                .getLocalizedString();
        localizedList.add(localString);

        return intlString;
    }

    /**
     * Get the {@link String} of the given language from an
     * {@link InternationalStringType}.
     * 
     * @param intlString
     *            The international string.
     * @param lang
     *            The language to retrieve the {@link String} for.
     * @return The {@link String} of the given language, or null if a string for
     *         the language could not be found.
     */
    public static String getLocalizedString(InternationalStringType intlString,
            String lang) {
        List<LocalizedStringType> localStrings = intlString
                .getLocalizedString();
        String localizedString = null;
        for (LocalizedStringType localStrType : localStrings) {
            if (lang.equals(localStrType.getLang())) {
                localizedString = localStrType.getValue();
            }
        }
        return localizedString;
    }

    /**
     * Convenience method for converting a known string {@link ValueType} into a
     * {@link String}.
     * 
     * @param value
     *            The ValueType
     * @return The String value contained.
     */
    public static String getStringValue(ValueType value) {
        if (value == null) {
            return null;
        }
        return ((StringValueType) value).getValue();
    }

    /**
     * Convenience method for converting a known date {@link ValueType} into a
     * {@link Date}.
     * 
     * @param value
     *            The ValueType
     * @return The Date value contained.
     */
    public static Date getDateValue(ValueType value) {
        if (value == null) {
            return null;
        }
        XMLGregorianCalendar cal = ((DateTimeValueType) value).getValue();
        return cal.toGregorianCalendar().getTime();
    }

    /**
     * Convenience method for converting a known float {@link ValueType} into a
     * {@link Float}.
     * 
     * @param value
     *            The ValueType
     * @return The Float value contained.
     */
    public static Float getFloatValue(ValueType value) {
        if (value == null) {
            return null;
        }
        return ((FloatValueType) value).getValue();
    }

    /**
     * Convenience method for converting a known long {@link ValueType} into a
     * {@link Long}.
     * 
     * @param value
     *            The ValueType
     * @return The Long value contained.
     */
    public static Long getLongValue(ValueType value) {
        if (value == null) {
            return null;
        }
        BigInteger bigint = ((IntegerValueType) value).getValue();
        return Long.valueOf(bigint.longValue());
    }

    /**
     * Convenience method for converting a known boolean {@link ValueType} into
     * a {@link Boolean}.
     * 
     * @param value
     *            The ValueType
     * @return The Boolean value contained.
     */
    public static Boolean getBooleanValue(ValueType value) {
        if (value == null) {
            return null;
        }
        return ((BooleanValueType) value).isValue();
    }

    public static String translateException(
            RegistryExceptionType registryException) {
        StringBuilder msg = new StringBuilder();
        msg.append("Registry Exception");
        msg.append("\nCode: ").append(registryException.getCode());
        msg.append("\nDetail: ").append(registryException.getDetail());
        msg.append("\nMessage: ").append(registryException.getMessage());
        msg.append("\nSeverity: ").append(registryException.getSeverity());
        return msg.toString();
    }

    public static Object getValue(ValueType value) {
        Object retVal = null;

        if (value instanceof AnyValueType) {
            return ((AnyValueType) value).getAny();
        } else if (value instanceof AnyValueType) {
            return ((AnyValueType) value).getAny();
        } else if (value instanceof BooleanValueType) {
            return ((BooleanValueType) value).isValue();
        } else if (value instanceof CollectionValueType) {
            return ((CollectionValueType) value).getElement();
        } else if (value instanceof DateTimeValueType) {
            return ((DateTimeValueType) value).getValue();
        } else if (value instanceof DurationValueType) {
            return ((DurationValueType) value).getValue();
        } else if (value instanceof FloatValueType) {
            return ((FloatValueType) value).getValue();
        } else if (value instanceof IntegerValueType) {
            return ((IntegerValueType) value).getValue();
        } else if (value instanceof InternationalStringValueType) {
            return ((InternationalStringValueType) value).getValue();
        } else if (value instanceof MapValueType) {
            return ((MapValueType) value).getMap();
        } else if (value instanceof SlotValueType) {
            return ((SlotValueType) value).getSlot();
        } else if (value instanceof StringValueType) {
            return ((StringValueType) value).getValue();
        } else if (value instanceof VocabularyTermValueType) {
            return ((VocabularyTermValueType) value).getValue();
        }
        return retVal;
    }

    public static boolean isUpdateModeValid(String updateMode) {
        for (String mode : UPDATE_MODES) {
            if (mode.equalsIgnoreCase(updateMode)) {
                return true;
            }
        }
        return false;
    }

}
