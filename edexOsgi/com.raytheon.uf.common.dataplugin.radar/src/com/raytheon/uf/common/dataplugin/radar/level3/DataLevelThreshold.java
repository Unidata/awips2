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
package com.raytheon.uf.common.dataplugin.radar.level3;

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
                string += "ND";
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
