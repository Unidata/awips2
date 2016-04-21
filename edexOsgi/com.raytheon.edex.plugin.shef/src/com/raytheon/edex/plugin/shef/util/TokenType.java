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

import java.util.HashSet;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * TODO Add Description
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

public enum TokenType {
    SLASH("/"),
    SPACEINMIDDLE("[^ \t].*[ \t][ \t]*.*[^ \t]"),
    SPACE(" "),
    TAB("\t"),
    COMMA(","),
    NEWLINE("[\\r\\n]"),
    //*******************************
    // NUMERIC("\\d*(\\.\\d+)?"),
    NUMERIC("^([+-]?)((\\d+)|(\\d+\\.)|(\\.\\d+)|(\\d+\\.\\d+))"),
    // QNUMERIC("(\\d*(\\.\\d+)?)+([A-Z])"),
    // QNUMERIC("^([+-]?)((\\d+)|(\\d+\\.)|(\\.\\d+)|(\\d+\\.\\d+))([" + ShefConstants.QUALIFER_CODES + "])"),
    QNUMERIC("^([+-]?)((\\d+)|(\\d+\\.)|(\\.\\d+)|(\\d+\\.\\d+))([A-Za-z])(.*)"),
    DECIMAL("\\."),
    //*******************************
    DATE_SEC("DS\\d{2}"),             // SS
    DATE_MIN("DN\\d{2,4}"),           // NNSS
    DATE_HOUR("DH\\d{2,6}"),          // HHNNSS
    DATE_DAY("DD\\d{2,8}"),           // DDHHNNSS
    DATE_MON("DM\\d{2,10}"),          // MMDDHHNNSS
    DATE_YEAR("DY\\d{2,12}"),         // YYMMDDHHNNSS
    DATE_DATE("DT\\d{2,14}"),         // CCYYMMDDHHNNSS
    DATE_JUL("DJ\\d{1,7}"),           // CCYYDDD | YYDDD | DDD 
    //*******************************
    DATE_REL("(DR)([SNHDMEY])([\\+-]?)(\\d{1,2})"),    // DRtXX
    //*******************************
    DATE_CREATE("DC\\d{4,12}"),       // CCYYMMDDHHNN
    //*******************************
    UNITS_CODE("DU[ES]"),                // DUu
    QUAL_CODE("DQ[A-Z]"),                // DQq
    DUR_CODE("DV[SNHDMYZ]\\d{1,2}"),     // DVvXX
    INT_CODE("(DI)([SNHDMEY])([\\+-]?)(\\d{1,2})"),     // DIvXX
    //*******************************
    A_REC("\\.A"),
    A_REC_R("\\.AR"),
    A_REC_C("\\.A\\d"),
    B_REC("\\.B"),
    B_REC_R("\\.BR"),
    B_REC_C("\\.B\\d"),
    E_REC("\\.E"),
    E_REC_R("\\.ER"),
    E_REC_C("\\.E\\d"),
    //*******************************
    OBS_DATE_4("\\d{4,4}"),          // MMDD
    OBS_DATE_6("\\d{6,6}"),          // YYMMDD
    OBS_DATE_8("\\d{8,8}"),          // CCYYMMDD
    PEDTSEP(""),
    TIMEZONE(""),
    LOC_ID("[_A-Z0-9]{3,8}"),
    EMPTY(""),
    RETAINEDCOMMENT(""),
    UNKNOWN(""),
    START(""),
    B_PATTERN(""),
    B_DATA(""),
    B_END(""),
    NIL(""),
    ERROR("");

    private static final Set<TokenType> SKIP_SET = new HashSet<TokenType>();
    static {
        SKIP_SET.add(RETAINEDCOMMENT);
        SKIP_SET.add(PEDTSEP);
        SKIP_SET.add(EMPTY);
        SKIP_SET.add(LOC_ID);
        SKIP_SET.add(START);
        SKIP_SET.add(UNKNOWN);
        SKIP_SET.add(RETAINEDCOMMENT);
        SKIP_SET.add(TIMEZONE);
        SKIP_SET.add(B_PATTERN);
        SKIP_SET.add(B_DATA);
        SKIP_SET.add(B_END);
        SKIP_SET.add(NIL);
        SKIP_SET.add(ERROR);
    }
    
    public static final Set<TokenType> INVALID_B_PATTERN = new HashSet<TokenType>();
    static {
        INVALID_B_PATTERN.add(RETAINEDCOMMENT);
        INVALID_B_PATTERN.add(START);
        INVALID_B_PATTERN.add(UNKNOWN);
        INVALID_B_PATTERN.add(RETAINEDCOMMENT);
        INVALID_B_PATTERN.add(B_DATA);
        INVALID_B_PATTERN.add(B_END);
        INVALID_B_PATTERN.add(NIL);
        INVALID_B_PATTERN.add(ERROR);
        INVALID_B_PATTERN.add(NUMERIC);
        INVALID_B_PATTERN.add(QNUMERIC);
        INVALID_B_PATTERN.add(DECIMAL);
        INVALID_B_PATTERN.add(INT_CODE);
    }
    
    private final Pattern matchPattern;

    private TokenType(String pattern) {
        matchPattern = Pattern.compile(pattern);
    }

    public Pattern getPattern() {
        return matchPattern;
    }
    
    /**
     * 
     * @param token
     * @return
     */
    public static TokenType getToken(String token) {
        TokenType type = UNKNOWN; 
        
        for(TokenType t : TokenType.values()) {
            if(SKIP_SET.contains(t)) {
                continue;
            }
            Matcher m = t.matchPattern.matcher(token);
            if(m.matches()) {
                if(QNUMERIC.equals(t)) {
                    if(token.length() == 1) {
                        // Special case because the QNUMERIC pattern will match a single
                        // Alpha character. If that's the case then set the token to ERROR
                        char c = token.charAt(0); 
                        if((c < 'A')&&(c > 'Z')) {
                            type = t;
                        } else {
                            type = ERROR;
                        }
                    } else {
                        // Shouldn't be any data in group 8, if so then we have
                        // an ill-formed qualifier.
                        String s = m.group(8);
                        if((s != null)&&(s.length() > 0)) {
                            type = ERROR;
                        } else {
                            type = t;
                        }
                    }
                } else if (NUMERIC.equals(t)){
                    type = t;
                } else if(TAB.equals(t)) {
                    type = SPACE;
                } else {
                    type = t;
                }
                break;
            }
        }
        return type;
    }
    
    public static final void main(String [] args) {
        
        String [] data = {
                "DIN45","DIN-45","DIN+45",
        };
        
//        for(String s : data) {
//            Matcher m = INT_CODE.getPattern().matcher(s);
//            if(m.find()) {
//                for(int i = 1;i <= m.groupCount();i++) {
//                    System.out.print("{" + m.group(i) + "}");
//                }
//                System.out.println();
//                System.out.println("*****************************");
//            }
//        }

//        data = new String [] {
//                "10E","-15H","20.J",".20J","12.5N", "A"
//        };
//
//        Pattern p = Pattern.compile("^([+-]?)((\\d+)|(\\d+\\.)|(\\.\\d+)|(\\d+\\.\\d+))([A-Z])");
//        
//        for(String s : data) {
////            Matcher m = QNUMERIC.getPattern().matcher(s);
//            Matcher m = p.matcher(s);
//
//            if(m.find()) {
//                for(int i = 1;i <= m.groupCount();i++) {
//                    System.out.print("{" + m.group(i) + "}");
//                }
//                System.out.println();
//                System.out.println("*****************************");
//            }
//        }

//        data = new String [] {
//                "10VV","AA", "10A","-20AF","20.A+",".20aF","12.5dg", "12.3+",
//        };
//
//        Pattern p = Pattern.compile("^([+-]?)((\\d+)|(\\d+\\.)|(\\.\\d+)|(\\d+\\.\\d+))([A-Za-z])(.*)");
//        
//        for(String s : data) {
//            Matcher m = p.matcher(s);
//            if(m.matches()) {
//                for(int i = 0;i <= m.groupCount();i++) {
//                    System.out.print(String.format("{%10s}",m.group(i)));
//                }
//            }
//            System.out.println();
//            TokenType t = getToken(s);
//            System.out.println(s + " : " + t);
//            System.out.println("---------------------------------");
//        }
        
        
        
//        for(String s : data) {
//            TokenType t = getToken(s);
//            System.out.println(s + " : " + t);
//        
//        
//        }
    
    }
}
