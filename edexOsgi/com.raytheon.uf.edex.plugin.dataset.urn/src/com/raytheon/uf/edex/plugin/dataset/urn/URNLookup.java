/**
 * Copyright 09/24/12 Raytheon Company.
 *
 * Unlimited Rights
 * This software was developed pursuant to Contract Number 
 * DTFAWA-10-D-00028 with the US Government. The US Government’s rights 
 * in and to this copyrighted software are as specified in DFARS
 * 252.227-7014 which was made part of the above contract. 
 */
package com.raytheon.uf.edex.plugin.dataset.urn;


/**
 * Lookup URNs.
 * 
 * @author ekladstrup
 * @version 1.0
 */
public class URNLookup {

    public static final String WXSRV_URN_PREFIX = "urn:x-wxsrv:";

    public static final String MODEL_URN_PREFIX = WXSRV_URN_PREFIX + "Dataset";

    // From ucar.edu WXCM primer
    public static final String FDC_AIRCRAFT_REPORT = "urn:fdc:icao:procedure:AircraftReport";

    // From ucar.edu WXCM primer
    public static final String ICAO_CODE_PREFIX = "urn:icao:code:weatherStation";

    private static enum State {
        START, ONE_SLASH, MULT_SLASH, ONE_COLON
    }

    /**
     * Convert EDEX internal ID to external URN
     * 
     * Single slashes are replaced by single colons. Any additional slashes
     * immediately following are converted into URN escape sequences. 'foo/bar'
     * -> 'foo:bar', 'foo//bar' -> 'foo:%2fbar'. Any colons in the local string
     * are escaped with another colon. 'foo:bar/baz' -> 'foo::bar:baz'. Other
     * non-URN characters are escaped using URN escape formatting.
     * 
     * Dataset URN prefix is attached.
     * 
     * @param local
     *            dataURI based ID
     * @return
     */
    public static String localToUrn(String local) {
        State s = State.START;
        StringBuilder sb = new StringBuilder(MODEL_URN_PREFIX).append(':');
        for (int i = 0; i < local.length(); ++i) {
            char curr = local.charAt(i);
            switch (curr) {
            case '/':
                if (s.equals(State.START)) {
                    s = State.ONE_SLASH;
                    sb.append(':');
                } else if (s.equals(State.ONE_SLASH)) {
                    s = State.MULT_SLASH;
                    sb.append("%2f");
                } else if (s.equals(State.MULT_SLASH)) {
                    sb.append("%2f");
                }
                break;
            case ':':
                sb.append("::");
                s = State.START;
                break;
            case '%':
            case '?':
            case '#':
            case ' ':
                sb.append(String.format("%%%02x", (short) curr));
                s = State.START;
                break;
            default:
                sb.append(curr);
                s = State.START;
            }
        }
        return sb.toString();
	}

    /**
     * Convert external URN to internal EDEX ID
     * 
     * URN escaped characters are converted to ASCII. Single colons are
     * converted to slashes. Colon pairs are converted to single colons.
     * 
     * Dataset URN prefix is assumed to be on input.
     * 
     * @param urn
     * @return dataURI based ID
     */
    public static String urnToLocal(String urn) {
        // add 1 since the last ":" is not included in the length
        String unique = urn.substring(MODEL_URN_PREFIX.length() + 1);
        State s = State.START;
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < unique.length(); ++i) {
            char curr = unique.charAt(i);
            char next;
            switch (curr) {
            case ':':
                if (s.equals(State.START)) {
                    s = State.ONE_COLON;
                    continue;
                } else {
                    sb.append(':');
                    s = State.START;
                    continue;
                }
            case '%':
                next = (char) Short.parseShort(unique.substring(i + 1, i + 3),
                        16);
                i += 2;
                break;
            default:
                next = curr;
                break;
            }
            if (s.equals(State.ONE_COLON)) {
                sb.append('/');
                s = State.START;
            }
            sb.append(next);
        }
        return sb.toString();
    }

    public static String getAircraftReportURN() {
        return FDC_AIRCRAFT_REPORT;
    }

    public static String icaoToUrn(String station) {
        return ICAO_CODE_PREFIX + ":" + station;
    }

    /**
     * Slice the icao urn and return the local part
     * 
     * @param urn
     * @return Blank string or local part if prefix is the proper icao prefix,
     *         null if the prefix does not match expectations
     */
    public static String urnToIcao(String urn) {
        if (urn.startsWith(ICAO_CODE_PREFIX)) {
            // add 1 since the last ":" is not included in the length
            return urn.substring(ICAO_CODE_PREFIX.length() + 1);
        } else {
            return null;
        }
    }
}
