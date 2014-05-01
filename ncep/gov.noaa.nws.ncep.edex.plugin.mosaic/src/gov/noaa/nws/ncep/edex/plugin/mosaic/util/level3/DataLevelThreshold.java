package gov.noaa.nws.ncep.edex.plugin.mosaic.util.level3;

/**
 * DataLevelThreshold is a class that defines data level thresholds.
 * 
 * Date         Ticket#         Engineer    Description
 * ------------ ----------      ----------- --------------------------
 * 09/2009      143				L. Lin     	Initial coding
 * </pre>
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 * @author L. Lin
 * @version 1.0
 */

public class DataLevelThreshold {
    private byte flags;

    private int value;

    public DataLevelThreshold() {
        this(0);
    }

    public DataLevelThreshold(int encodedValue) {
        flags = (byte) ((encodedValue >> 8) & 0xFF);
        value = (encodedValue & 0xFF);
    }

    public void set(int encodedValue) {
        flags = (byte) ((encodedValue >> 8) & 0xFF);
        value = (encodedValue & 0xFF);
    }

    public int getValue() {
        return value;
    }

    public boolean isSpecial() {
        return (flags & 0x80) != 0;
    }

    public boolean isTimes100() {
        return (flags & 0x40) != 0;
    }

    public boolean isTimes20() {
        return (flags & 0x20) != 0;
    }

    public boolean isTimes10() {
        return (flags & 0x10) != 0;
    }

    public boolean isGtrThan() {
        return (flags & 0x08) != 0;
    }

    public boolean isLessThan() {
        return (flags & 0x04) != 0;
    }

    public boolean isPlus() {
        return (flags & 0x02) != 0;
    }

    public boolean isMinus() {
        return (flags & 0x01) != 0;
    }

    public Object decode() {

        Object retVal = null;
        String string = "";
        float value = getValue();

        if (isGtrThan()) {
            string += '>';
        } else if (isLessThan()) {
            string += '<';
        }

        if (isPlus()) {
            string += '+';
        } else if (isMinus()) {
            string += '-';
            value *= -1.0f;
        }

        if (isSpecial()) {
            switch (getValue()) {
            case 0:
                break;

            case 1:
                string += "TH";
                break;

            case 2:
                string += "NO DATA";
                break;

            case 3:
                string += "RF";
                break;
            }
            retVal = string;
        } /* if special */
        else {
            if (isTimes10()) {
                value *= 0.1f;
            } else if (isTimes20()) {
                value *= 0.05f;
            } else if (isTimes100()) {
                value *= 0.01f;
            }
            retVal = (Float) value;
        }

        return retVal;
    }

    @Override
    public String toString() {
        return decode().toString();
    }
}
