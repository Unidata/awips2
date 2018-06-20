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
package com.raytheon.edex.plugin.shef.util;

import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.TimeZone;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.raytheon.uf.common.dataplugin.shef.util.SHEFErrorCodes;
import com.raytheon.uf.common.dataplugin.shef.util.ShefConstants;

/**
 * TODO
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 17, 2009            jkorman     Initial creation
 *
 * </pre>
 *
 * @author jkorman
 * @version 1.0	
 */

public class ParserToken {

    // Check for a qualifier that may be embedded in the data.
    private static final Pattern BAD_DATA_1 = Pattern.compile("\\d*\\.{1}\\d*([A-Z]\\d{1,})*"); 
    
    public static final HashMap<TokenType, Integer> DATE_TYPES = new HashMap<TokenType, Integer>();
    static {
        DATE_TYPES.put(TokenType.DATE_SEC, 0);
        DATE_TYPES.put(TokenType.DATE_MIN, 0);
        DATE_TYPES.put(TokenType.DATE_HOUR, 0);
        DATE_TYPES.put(TokenType.DATE_DAY, 0);
        DATE_TYPES.put(TokenType.DATE_MON, 0);
        DATE_TYPES.put(TokenType.DATE_YEAR, 0);
        DATE_TYPES.put(TokenType.DATE_DATE, 0);
        DATE_TYPES.put(TokenType.DATE_CREATE, 0);
        DATE_TYPES.put(TokenType.DATE_JUL, 0);
//        DATE_TYPES.put(TokenType.DATE_REL, 1);
//        DATE_TYPES.put(TokenType.INT_CODE, 1);
        DATE_TYPES.put(TokenType.OBS_DATE_4, 4);
        DATE_TYPES.put(TokenType.OBS_DATE_6, 6);
        DATE_TYPES.put(TokenType.OBS_DATE_8, 8);
    }
    
    public static final int ERR_NO_ERROR = 0;
    
    public static final int ERR_INVALID_QUAL = -1;
    
    public static final int ERR_INV_JUL_DATE = -2;

    public static final int ERR_INV_CREATE_DATE = -3;

    
    public static final int ERR_INV_SECONDS = -4;
    public static final int ERR_INV_MINUTES = -5;
    public static final int ERR_INV_HOURS = -6;
    public static final int ERR_INV_DAY = -7;
    public static final int ERR_INV_MONTH = -8;

    public static final int ERR_LOCID_NULL = -50;
    public static final int ERR_LOCID_SHORT = -51;
    public static final int ERR_LOCID_LONG  = -52;
    public static final int ERR_LOCID_INVCHAR = -53;

    
    public static final int ERR_LOG035 = -135;
    public static final int ERR_LOG044 = -144;
    public static final int ERR_LOG079 = -179;

    
    
    private final String rawToken;
    
    private String token;

    private String sendCode;
    
    private final TokenType type;
    
    private final boolean numeric;

    private final boolean qualifiedNumeric;

    private Double numericValue = null;
    
    private String qualifier = null;
    
    private boolean isTrace = false;
    
    // This value will be set for DT, DY, DM, DD, DH, DN, DS, and DC
    private SHEFDate dateData = null;
    
    private int error = ERR_NO_ERROR;
    
    private ParserToken(String token, TokenType type, boolean numeric, boolean qnumeric) {
        rawToken = token;
        this.token = token;
        this.type = type;
        this.numeric = numeric;
        this.qualifiedNumeric = qnumeric;
    }
    
    /**
     * Construct a ParserToken with a given token string. The TokenType
     * is determined 
     * @param token
     */
    public ParserToken(String token) {
        rawToken = token;
        this.token = token;
        type = TokenType.getToken(token); 
        if(TokenType.ERROR.equals(type)) {
            error = ERR_INVALID_QUAL;
        }
        numeric = TokenType.NUMERIC.equals(type);
        qualifiedNumeric = TokenType.QNUMERIC.equals(type);
        setDataValues();
        setDateValues();
    }

    /**
     * Construct a ParserToken with a given token string and an
     * explicit TokenType. 
     * @param token
     * @param type
     */
    public ParserToken(String token, TokenType type) {
        rawToken = token;
        this.token = token;
        this.type = type;
        numeric = TokenType.NUMERIC.equals(type);
        qualifiedNumeric = TokenType.QNUMERIC.equals(type);
        setDataValues();
        setDateValues();
    }

    /**
     * Construct a ParserToken with a given token string and an
     * explicit TokenType. 
     * @param token
     * @param type
     */
    private ParserToken(String token, TokenType type, int errCode) {
        rawToken = token;
        this.token = token;
        this.type = type;
        error = errCode;
        if(error == ERR_NO_ERROR) {
            numeric = TokenType.NUMERIC.equals(type);
            qualifiedNumeric = TokenType.QNUMERIC.equals(type);
            setDataValues();
            setDateValues();
        } else {
            numeric = false;
            qualifiedNumeric = false;
        }
    }
    
    /**
     * @return the rawToken
     */
    public String getRawToken() {
        return rawToken;
    }

    /**
     * 
     * @return
     */
    public String getToken() {
        return token;
    }
    
    /**
     * @return the sendCode
     */
    public String getSendCode() {
        return sendCode;
    }

    /**
     * @param sendCode the sendCode to set
     */
    public void setSendCode(String sendCode) {
        this.sendCode = sendCode;
    }

    public TokenType getType() {
        return type;
    }

    /**
     * Is this token numeric. Checks are made for
     * integer and simple decimal numbers.
     * @return Is this a numeric token.
     */
    public boolean isNumeric() {
        return numeric;
    }
    
    /**
     * @return the qualifiedNumeric
     */
    public boolean isQualifiedNumeric() {
        return qualifiedNumeric;
    }
    
    /**
     * @return the numericValue
     */
    public Double getNumericValue() {
        return numericValue;
    }

    /**
     * @param numericValue the numericValue to set
     */
    public void setNumericValue(Double numericValue) {
        this.numericValue = numericValue;
    }

    /**
     * @return the qualifier
     */
    public String getQualifier() {
        return qualifier;
    }

    /**
     * @param qualifier the qualifier to set
     */
    public void setQualifier(String qualifier) {
        this.qualifier = qualifier;
    }

    /**
     * @return the dateData
     */
    public SHEFDate getDateData() {
        return dateData;
    }

    /**
     * @param dateData the dateData to set
     */
    public void setDateData(SHEFDate dateData) {
        this.dateData = dateData;
    }
    
    /**
     * Was the numeric value derived from T (trace)
     * @return the isTrace
     */
    public boolean isTrace() {
        return isTrace;
    }

    /**
     * @param isTrace the isTrace to set
     */
    public void setTrace(boolean isTrace) {
        this.isTrace = isTrace;
    }

    /**
     * 
     * @return
     */
    public int getError() {
        return error;
    }
    
    public void setError(int errorCode) {
        error = errorCode;
    }

    /**
     * Is this token a value type.
     * @return
     */
    public boolean isValueToken() {
        return (TokenType.NUMERIC.equals(type) || TokenType.QNUMERIC.equals(type));
    }
    
    public void adjustToTimezone(TimeZone tz) {
        if(dateData != null) {
            dateData.setTimeZone(tz);
            dateData.adjustToTimezone();
            token = dateData.toLocal();
            
            // Did we set a julian day?
            int ddd = getDateData().getJulian();
            if(ddd > 0) {
                GregorianCalendar c = new GregorianCalendar(tz);
                int year = getDateData().getYear();
                if(ddd > 365) {
                    if(!c.isLeapYear(year)) {
                        error = ERR_INV_JUL_DATE;
                    }
                }
            }
            error = dateData.getError();
            if(dateData.isDSTExclusion()) {
                error = ERR_LOG044;
            }
        }
    }
    
    private void setDateValues() {
        if(DATE_TYPES.containsKey(type)) {
            int ddd = 0;
            int second = 0;
            int minute = 0;
            int hour = 0;
            int day = 0;
            int month = 0;
            // 2 digit year
            int ly = 0;
            // 2 digit century
            int cc = 0;
            StringBuilder dateData = null;
            if(token.startsWith("D")) {
                dateData = new StringBuilder(token.substring(2));
            } else {
                dateData = new StringBuilder(token);
            }
            boolean fallThrough = false;
            switch(type) {
            case DATE_YEAR : {
                cc = -1;
                ly = getDateItem(dateData);
            }
            case DATE_MON : {
                month = getDateItem(dateData);
            }
            case DATE_DAY : {
                day = getDateItem(dateData);
                hour = getDateItem(dateData);
                minute = getDateItem(dateData);
                second = getDateItem(dateData);
                
                break;
            }
            case DATE_HOUR : {
                hour = getDateItem(dateData);
            }
            case DATE_MIN : {
                minute = getDateItem(dateData);
                if((minute < 0)&&(!fallThrough)) {
                    minute = 0;
                }
            }
            case DATE_SEC : {
                second = getDateItem(dateData);
                if((second < 0)&&(!fallThrough)) {
                    second = 0;
                }
                break;
            }
            case DATE_DATE : {
                cc = getDateItem(dateData);
                ly = getDateItem(dateData);
                month = getDateItem(dateData);
                day = getDateItem(dateData);
                if(second == -1) {
                    second = 0;
                }
                hour = getDateItem(dateData);
                minute = getDateItem(dateData);
                if(minute == -1) {
                    minute = 0;
                }
                second = getDateItem(dateData);
                if(second == -1) {
                    second = 0;
                }
                break;
            }
            case DATE_JUL : {
                switch(dateData.length()) {
                case 7 : {
                    cc = getDateItem(dateData);
                }
                case 5 : {
                    ly = getDateItem(dateData);
                }
                case 1 :
                case 2 :
                case 3 : {
                    ddd = Integer.parseInt(dateData.toString());
                    break;
                }
                default : {
                    error = ERR_LOG079;
                }
                } // switch()
                break;
            }
            case OBS_DATE_8 :
            case OBS_DATE_6 :
            case OBS_DATE_4 : {
                fallThrough = false;
                switch(dateData.length()) {
                case 8 : {
                    fallThrough = true;
                }
                case 6 : {
                    if(fallThrough) {
                        cc = getDateItem(dateData);
                    } else {
                        cc = -1;
                    }
                    fallThrough = true;
                }
                case 4 : {
                    if(fallThrough) {
                        ly = getDateItem(dateData);
                    } else {
                        cc = -1;
                        ly = -1;
                    }
                    month = getDateItem(dateData);
                    day = getDateItem(dateData);
                    hour = -1;
                }
            }
                break;
            }
            case DATE_CREATE : {
                fallThrough = false;
                switch(dateData.length()) {
                case 12 : {
                    fallThrough = true;
                }
                case 10 : {
                    if(fallThrough) {
                        cc = getDateItem(dateData);
                    } else {
                        cc = -1;
                    }
                    fallThrough = true;
                }
                case 8 : {
                    if(fallThrough) {
                        ly = getDateItem(dateData);
                    } else {
                        cc = -1;
                        ly = -1;
                    }
                    month = getDateItem(dateData);
                    day = getDateItem(dateData);
                    hour = getDateItem(dateData);
                    minute = getDateItem(dateData);
                    second = 0;
                    break;
                }
                case 6 : { // MMDDHH
                    cc = -1;
                    ly = -1;
                    month = getDateItem(dateData);
                    day = getDateItem(dateData);
                    hour = getDateItem(dateData);
                    minute = 0;
                    second = 0;
                    break;
                }
                case 4 : { // MMDD
                    cc = -1;
                    ly = -1;
                    month = getDateItem(dateData);
                    day = getDateItem(dateData);
                    hour = -1;
                    minute = 0;
                    second = 0;
                    break;
                }
                default : {
                    error = ERR_INV_CREATE_DATE;
                }
                }
                break;
            }
            }
            // Check for a julian day (day of year).
            if(ddd == 0) {
                setDateData(new SHEFDate(cc,ly,month,day,hour,minute,second));
            } else {
                SHEFDate d = new SHEFDate();
                d.setCentury(cc);
                d.setLy(ly);
                d.setJulian(ddd);
                setDateData(d);
            }
            
            if(month > 12) {
                error = ERR_INV_MONTH;
            } else if((month > 0) && (day > SHEFDate.DAYS_MONTH[month])) {
                error = ERR_INV_DAY;
            } else if(hour > 24) {
                error = ERR_INV_HOURS;
            } else if(minute > 59) {
                error = ERR_INV_MINUTES;
            } else if(second > 59) {
                error = ERR_INV_SECONDS;
            }
            
            token = getDateData().toString();
        }
    }
    
    /**
     * 
     * @param data
     * @return
     */
    private static int getDateItem(StringBuilder data) {
        int intData = -1;
        if(data.length() >= 2) {
            intData = Integer.parseInt(data.substring(0,2));
            data.delete(0,2);
        }
        return intData;
    }

    /**
     * Use the derived type information to populate the numeric data
     * for this token if it is numeric or qualified numeric.
     */
    private void setDataValues() {
        if(numeric) {
            numericValue = Double.parseDouble(token);
        } else if (qualifiedNumeric) {
            numericValue = Double.parseDouble(token.substring(0,token.length()-1));
            qualifier = token.substring(token.length()-1);
        }
    }
    
    /**
     * 
     * @param locId
     * @return
     */
    public static final ParserToken createLocIdToken(String locId) {
        ParserToken t = null;
        if(locId != null) {
            Matcher m = TokenType.LOC_ID.getPattern().matcher(locId);
            if(m.matches()) {
                t = new ParserToken(locId, TokenType.LOC_ID);
            } else {
                t = new ParserToken(locId,TokenType.ERROR);
                // locId is no good, find out why
                int n = locId.length();
                if(n < 3) {
                    t.error = ERR_LOCID_SHORT;
                } else if (n > 8) {
                    t.error = ERR_LOCID_LONG;
                } else {
                    t.error = ERR_LOCID_INVCHAR;
                }
            }
        } else {
            t = new ParserToken("<null>",TokenType.ERROR);
            t.error = ERR_LOCID_NULL;
        }
        return t;
    }
    
    
    /**
     * This is a factory method that takes a ParserToken with the type set
     * to unknown and attempts to do further analysis of the unknown text.
     * 
     * @return
     */
    public ParserToken check_D_Directives() {
        ParserToken pToken = null;
        TokenType type = TokenType.UNKNOWN;
        int error = ERR_NO_ERROR;
        
        if(token != null) {
            if(rawToken.length() > 2) {
                String s = token.substring(0,2);
                String ss = token.substring(2);
                
                if("DS".equals(s)) {
//                  DATE_SEC("DS\\d{2}"),             // SS
                    type = TokenType.DATE_SEC;
                    if(ss.length() != 2) {
                        error = SHEFErrorCodes.LOG_016;
                    } else {
                        error = evaluateDateTime(ss);
                    }
                } else if("DN".equals(s)) {
//                  DATE_MIN("DN\\d{2,4}"),           // NNSS
                    type = TokenType.DATE_MIN;
                    if((ss.length() < 2)||(ss.length() > 4)) {
                        error = SHEFErrorCodes.LOG_016;
                    } else {
                        error = evaluateDateTime(ss);
                    }
                } else if("DH".equals(s)) {
//                  DATE_HOUR("DH\\d{2,6}"),          // HHNNSS
                    type = TokenType.DATE_HOUR;
                    if((ss.length() < 2)||(ss.length() > 6)) {
                        error = SHEFErrorCodes.LOG_016;
                    } else {
                        error = evaluateDateTime(ss);
                    }
                } else if("DD".equals(s)) {
//                  DATE_DAY("DD\\d{2,8}"),           // DDHHNNSS
                    type = TokenType.DATE_DAY;
                    if((ss.length() < 2)||(ss.length() > 8)) {
                        error = SHEFErrorCodes.LOG_016;
                    } else {
                        error = evaluateDateTime(ss);
                    }
                    error = evaluateDateTime(ss);
                } else if("DM".equals(s)) {
//                  DATE_MON("DM\\d{2,10}"),          // MMDDHHNNSS
                    type = TokenType.DATE_MON;
                    if((ss.length() < 2)||(ss.length() > 10)) {
                        error = SHEFErrorCodes.LOG_016;
                    } else {
                        error = evaluateDateTime(ss);
                    }
                } else if("DY".equals(s)) {
//                  DATE_YEAR("DY\\d{2,12}"),         // YYMMDDHHNNSS
                    type = TokenType.DATE_YEAR;
                    if((ss.length() < 2)||(ss.length() > 12)) {
                        error = SHEFErrorCodes.LOG_016;
                    } else {
                        error = evaluateDateTime(ss);
                    }
                } else if("DT".equals(s)) {
//                  DATE_DATE("DT\\d{2,14}"),          // CCYYMMDDHHNNSS
                    type = TokenType.DATE_DATE;
                    if((ss.length() < 2)||(ss.length() > 14)) {
                        error = SHEFErrorCodes.LOG_016;
                    } else {
                        error = evaluateDateTime(ss);
                    }
                } else if("DJ".equals(s)) {
//                  DATE_JUL("DJ\\d{3,7}"),           // CCYYDDD | YYDDD | DDD 
                    type = TokenType.DATE_JUL;
                    error = evaluateDateTime(ss);
                } else if("DC".equals(s)) {
//                  DATE_CREATE("DC\\d{4,12}"),       // CCYYMMDDHHNN
                    type = TokenType.DATE_CREATE;
                    if((ss.length() < 2)||(ss.length() > 12)) {
                        error = SHEFErrorCodes.LOG_016;
                    } else {
                        error = evaluateDateTime(ss);
                    }
//----------------------
//done with date codes
//----------------------
                } else if("DU".equals(s)) {
//                  UNITS_CODE("DU[ES]"),                // DUu
                    type = TokenType.UNITS_CODE;
                    error = SHEFErrorCodes.LOG_022;
                } else if("DQ".equals(s)) {
//                  QUAL_CODE("DQ[A-Z]"),                // DQq
                    type = TokenType.QUAL_CODE;
                    if(ss.length() == 1) {
                        if(!isValidQualifier(ss)) {
                            error = SHEFErrorCodes.LOG_084;
                        }
                    } else {
                        error = SHEFErrorCodes.LOG_021;
                    }
                } else if("DR".equals(s)) {
//                  DATE_REL("(DR)([SNHDMEY])([\\+-]?)(\\d{1,2})"),    // DRtXX
                    type = TokenType.DATE_REL;
                    type = TokenType.DUR_CODE;
                    if(ss.length() == 0) {
                        error = SHEFErrorCodes.LOG_027;
                    } else {
                        if(isValidIntervalCode(ss.charAt(0))) {
                            ss = ss.substring(1);
                            if((ss.length() > 0) && isSign(ss.substring(0,1))) {
                                ss = ss.substring(1);
                            }
                            if(!isAllDigits(ss)) {
                                error = SHEFErrorCodes.LOG_028;
                            } else {
                                if(ss.length() > 3) {
                                    error = SHEFErrorCodes.LOG_103;
                                }
                            }
                        } else {
                            error = SHEFErrorCodes.LOG_027;
                        }
                    }
                } else if("DV".equals(s)) {
//                  DUR_CODE("DV[SNHDMYZ]\\d{1,2}"),     // DVvXX
                    type = TokenType.DUR_CODE;
                    if(ss.length() == 0) {
                        error = SHEFErrorCodes.LOG_023;
                    } else {
                        if(isValidDurationCode(ss.charAt(0))) {
                            if(!isAllDigits(ss)) {
                                error = SHEFErrorCodes.LOG_024;
                            } else {
                                if(ss.length() > 2) {
                                    error = SHEFErrorCodes.LOG_104;
                                }
                            }
                        } else {
                            error = SHEFErrorCodes.LOG_023;
                        }
                    }
                } else if("DI".equals(s)) {
//                  INT_CODE("(DI)([SNHDMEY])([\\+-]?)(\\d{1,2})"),     // DIvXX
                    type = TokenType.INT_CODE;
                    if(ss.length() == 0) {
                        error = SHEFErrorCodes.LOG_025;
                    } else {
                        if(isValidIntervalCode(ss.charAt(0))) {
                            ss = ss.substring(1);
                            if((ss.length() > 0) && isSign(ss.substring(0,1))) {
                                ss = ss.substring(1);
                            }
                            if(!isAllDigits(ss)) {
                                error = SHEFErrorCodes.LOG_026;
                            } else {
                                if(ss.length() > 2) {
                                    error = SHEFErrorCodes.LOG_102;
                                }
                            }
                        } else {
                            error = SHEFErrorCodes.LOG_025;
                        }
                    }
                } else {
                    
                }
            }
        }
        // Now that we've collected error data, create the token
        pToken = new ParserToken(rawToken, type, error);
        return pToken;
    }

    /**
     * Enter with a unknown parser token to analyze. This should only be
     * used once the positional data has been established. Gets us past
     * the location identifier, timezone issues.
     * @param token
     * @return
     */
    public ParserToken analyzeUnknown(String token) {
        ParserToken t = null;
        if(token != null) {
            if((token.length() == 1) || (token.length() == 2)) {
                // the only legal 1 character are timezones
                // and single digits. These will have already
                // been identified, so just set up an error.
                t = new ParserToken(token,TokenType.UNKNOWN);
                t.setError(64);
            } else if (token.length() > 2) {
                if(token.indexOf(',') > 0) {
                    // some sort of numeric that isn't right.
                    t = new ParserToken(token,TokenType.UNKNOWN);
                    t.setError(65);
                } else if(isAllDigitPunctuation(token)) {
                    // some sort of numeric that isn't right.
                    t = new ParserToken(token,TokenType.UNKNOWN);
                    t.setError(78);
                } else {
                    Matcher m = BAD_DATA_1.matcher(token);
                    if(m.matches()) {
                        t = new ParserToken(token,TokenType.UNKNOWN);
                        t.setError(78);
                    }
                }
            }
        }
        if(t == null) {
            t = new ParserToken(token,TokenType.UNKNOWN);
            t.setError(1);
        }
        return t;
    }

    /**
     * 
     * @param t
     * @return
     */
    public static ParserToken identifyUnknown(ParserToken t) {
        ParserToken newToken = null;
        Character PLUS = '+';
        Character MINUS = '-';
        Character DECIMAL = '.';

        String target = t.getRawToken();
        if (target != null) {
            target = target.trim();

            StringBuilder sb = new StringBuilder();
            String qual = null;
            boolean seenSign = false;
            boolean seenWhole = false;
            boolean seenFrac = false;
            
            int error = 0;

            if (target.length() > 0) {
                // eat any leading space
                int index = -1;
                Character c = getChar(target, ++index);
                if (c != null) {
                    if (PLUS.equals(c)) {
                        seenSign = true;
                        sb.append(c);
                        c = getChar(target, ++index);
                    } else if (MINUS.equals(c)) {
                        seenSign = true;
                        sb.append(c);
                        c = getChar(target, ++index);
                    }
                    if (c != null) {
                        if(Character.isDigit(c)) {
                            while (isDigit(c)) {
                                sb.append(c);
                                c = getChar(target, ++index);
                                if(c == null) {
                                    break;
                                }
                            }
                            seenWhole = true;
                        }
                        if (c != null) {
                            if(DECIMAL.equals(c)) {
                                sb.append(c);
                                c = getChar(target, ++index);
                                if (c != null) {
                                    if(Character.isDigit(c)) {
                                        while (isDigit(c)) {
                                            sb.append(c);
                                            c = getChar(target, ++index);
                                            if(c == null) {
                                                break;
                                            }
                                        }
                                        seenFrac = true;
                                    }
                                }
                            }
                            if(c != null) {
                                if(seenFrac || seenWhole) {
                                    // From here we're looking for a qualifier
                                    qual = String.valueOf(c);
                                    c = getChar(target, ++index);
                                }
                                // Shouldn't see another character after
                                // the qualifier.
                                if(c != null) {
                                    error = -1;
                                    newToken = t;
                                    if(seenFrac || seenWhole) {
                                        newToken.setError(SHEFErrorCodes.LOG_084);
                                    } else {
                                        newToken.setError(SHEFErrorCodes.LOG_078);
                                    }
                                }
                            }
                        }
                    }                                        }
            }
            if(error == 0) {
                if(seenFrac || seenWhole) {
                    if(qual != null) {
                        newToken = new ParserToken(sb.toString(), TokenType.QNUMERIC, false, true);                    
                    } else {
                        newToken = new ParserToken(sb.toString(), TokenType.NUMERIC, true, false);                    
                    }
                    newToken.setDataValues();
                    newToken.setError(ERR_NO_ERROR);
                } else {
                    if("+.".equals(sb.toString())) {
                        newToken = new ParserToken("-9999", TokenType.NUMERIC, true, false);                    
                        newToken.setDataValues();
                        newToken.setError(ERR_NO_ERROR);
                    } else if("-.".equals(sb.toString())) {
                        newToken = new ParserToken("-9999", TokenType.NUMERIC, true, false);                    
                        newToken.setDataValues();
                        newToken.setError(ERR_NO_ERROR);
                    }
                }
            }
        }
        return newToken;
    }
    
    /**
     * 
     * @param s
     * @param index
     * @return
     */
    public static Character getChar(String s, int index) {
        Character c = null;
        if (s != null) {
            if (index >= 0) {
                if (index < s.length()) {
                    c = s.charAt(index);
                }
            }
        }
        return c;
    }

    /**
     * Is the target string all digits or punctuation?
     * @param s
     * @return
     */
    public static final boolean isAllDigitPunctuation(String s) {
        boolean retValue = true;
        if(s != null) {
            for(int i = 0;i < s.length();i++) {
                char c = s.charAt(i);
                if(!isDigit(c)) {
                    if(!isPunctuation(c)) {
                        retValue = false;
                        break;
                    }
                }
            }
        } else {
            retValue = false;
        }
        return retValue;
    }
    
    public static final boolean isPunctuation(char c) {
        return ",.-_=+!@#$%^&*(){}[]|\\;<>".indexOf(c) > 0;
    }
    
    /**
     * 
     * @param c
     * @return
     */
    public static final boolean isSign(String s) {
        boolean isSign = false;
        if(s != null) {
            isSign = (s.startsWith("+") || s.startsWith("-"));
        }
        return isSign;
    }
    
    /**
     * 
     * @param c
     * @return
     */
    public static final boolean isDigit(char c) {
        return "0123456789".indexOf(c) > -1;
    }
    
    /**
     * 
     * @param c
     * @return
     */
    public static final boolean isNotDigit(char c) {
        return !isDigit(c);
    }
    
    public static final boolean isValidQualifier(String s) {
        return (ShefParm.getDataQualifierCodes(s) != null);
    }
    
    public static final boolean isValidQualifier(Character c) {
        return (ShefParm.getDataQualifierCodes(String.valueOf(c)) != null);
    }
    
    
    /**
     * 
     * @param c
     * @return
     */
    public static final boolean isValidDurationCode(char c) {
        return ShefConstants.DURATION_CODES.indexOf(c) > -1;
    }
    
    /**
     * 
     * @param c
     * @return
     */
    public static final boolean isNotValidDurationCode(char c) {
        return !isValidDurationCode(c);
    }
    
    /**
     * 
     * @param c
     * @return
     */
    public static final boolean isValidIntervalCode(char c) {
        return ShefConstants.DATE_INC_CODES.indexOf(c) > -1;
    }
    
    /**
     * 
     * @param c
     * @return
     */
    public static final boolean isNotValidIntervalCode(char c) {
        return !isValidIntervalCode(c);
    }

    /**
     * 
     * @param s
     * @return
     */
    public static final int evaluateDateTime(String s) {
        int error = SHEFErrorCodes.LOG_000;
        if(s != null) {
            if(!isAllDigits(s)) {
                error = SHEFErrorCodes.LOG_002;
            } else {
                if((s.length() % 2) == 1) {
                    error = SHEFErrorCodes.LOG_002;
                }
            }
        }
        return error;
    }
    
    /**
     * Is the target string all digits?
     * @param s
     * @return
     */
    public static final boolean isAllDigits(String s) {
        boolean retValue = true;
        if(s != null) {
            for(int i = 0;i < s.length();i++) {
                if(isNotDigit(s.charAt(i))) {
                    retValue = false;
                    break;
                }
            }
        } else {
            retValue = false;
        }
        return retValue;
    }
    
    
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("ParserToken:{");
        sb.append(token);
        sb.append("}:{");
        sb.append(type);
        sb.append("}:{");
        sb.append((isNumeric() ? "numeric" : "nonnumeric"));
        sb.append("}:{");
        sb.append((isQualifiedNumeric() ? "qnumeric}" : "nonqnumeric}"));
        if(isNumeric() || isQualifiedNumeric()) {
            sb.append("}:[");
            sb.append(numericValue);
            sb.append("]{");
            sb.append(qualifier);
            sb.append("}");
        }
        if(dateData != null) {
            sb.append("[");
            sb.append(dateData.toString());
            sb.append("]");
        }
        return sb.toString();
    }
    
    public static final void main(String [] args) {
        
//        ParserToken t = new ParserToken("001");
//        System.out.println(t.type + " " + t.isNumeric() + " " + t.isQualifiedNumeric());
//        
//        t = new ParserToken("DH0311");
//        System.out.println(t.type + " " + t.isNumeric() + " " + t.isQualifiedNumeric());
//        
//        t = new ParserToken("3.2M");
//        System.out.println(t.type + " " + t.isNumeric() + " " + t.isQualifiedNumeric());

//        System.out.println(new SHEFDate(20,11,11,31,18,11,27));
//        
//        ParserToken pt = new ParserToken("20111131000000");
//        System.out.println(pt);
//        System.out.println(pt.getError());
//        
//        pt = new ParserToken("DT20010812124530");
//        System.out.println(pt);
//        System.out.println(pt.getError());
//        
//        String[] locIds = { null, "", "TO", "TOP", "TO%40", "_TO", "TOP_5",
//                "ABCDEFGH", "ABCDEFGHI", };
//        int[] locErrs = { ERR_LOCID_NULL, ERR_LOCID_SHORT, ERR_LOCID_SHORT,
//                ERR_NO_ERROR, ERR_LOCID_INVCHAR, ERR_NO_ERROR, ERR_NO_ERROR, ERR_NO_ERROR, ERR_LOCID_LONG};
//        public static final int ERR_LOCID_NULL = -50;
//        public static final int ERR_LOCID_SHORT = -51;
//        public static final int ERR_LOCID_LONG  = -52;
//        public static final int ERR_LOCID_INVCHAR = -53;
        
//        int i = 0;
//        for(String s : locIds) {
//            ParserToken id = createLocIdToken(s);
//            System.out.println(String.format("%10s %10b %3d %s",s, (id.getError() == locErrs[i++]), id.getError(), id.getType()));
//        }
        
//        TimeZone tz = TimeZone.getTimeZone("US/Central");
//        ParserToken pt = new ParserToken("DJ84366");
//        System.out.println(pt);
//        pt.adjustToTimezone(tz);
//        System.out.println(pt);
//
//        GregorianCalendar c = new GregorianCalendar(tz);
//        System.out.println(c.isLeapYear(1984));       
//       
//        String s = "DI+I0";
//        
//        String s1 = s.substring(0,2);
//        
//        String s2 = s.substring(2);
//        
//        System.out.println(String.format("\"%s\"  [%s].%d [%s].%d", s, s1, s1.length(), s2, s2.length()));
//       
//        if (s2.startsWith("+") || s2.startsWith("-")) {
//            System.out.println("Stripping sign"); 
//            s2 = s2.substring(1);
//        }
//        if (isAllDigits(s2)) {
//            System.out.println("All is well");
//        } else {
//            System.out.println("Bad date interval");
//        }
//
//        System.out.println(String.format("\"%s\"  [%s].%d [%s].%d", s, s1, s1.length(), s2, s2.length()));
//
//        System.out.println("32".length() % 2);
//        
//        pt = new ParserToken("DM801");
//        System.out.println(pt);
//        
//        pt = new ParserToken("20110502", TokenType.OBS_DATE_6);
//        pt.adjustToTimezone(SHEFTimezone.GMT_TIMEZONE);
//        pt.dateData.applyData(new ParserToken("DH1200"));
//        
//        System.out.println(pt + " [Error = " + pt.getError() + "]");
//        

        
        ParserToken t = new ParserToken("3.3A6");
        System.out.println(t + " error = " + t.getError());
        ParserToken tt = t.analyzeUnknown(t.getRawToken());
        System.out.println(tt + " error = " + tt.getError());
        
        isAllDigitPunctuation("3.1+");
        
//        .E EE0116   820101 Z DH12
//        .E1 Z DH12/HG/DIH06/2.1/2.2
//              ?
//         ** ERROR 64 ** Parameter code too short or field misinterpreted as param-code

        t = new ParserToken("DH04");
        System.out.println(t);
        
        ParserToken p = new ParserToken("3.2+");
        System.out.println(p);
        System.out.println(p.getError());
        
        
        
    }
}
