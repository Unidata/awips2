package gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm;

/**
 * Mapping for the symbol encoding. The mapping is based on:
 * <pre>
 *  Name:             Adobe Symbol Encoding to Unicode
 *  Unicode version:  2.0
 *  Table version:    0.2
 *  Date:             30 March 1999
 *  
 *  Copyright (c) 1991-1999 Unicode, Inc. All Rights reserved.
 *  
 *  This file is provided as-is by Unicode, Inc. (The Unicode Consortium). No
 *  claims are made as to fitness for any particular purpose. No warranties of
 *  any kind are expressed or implied. The recipient agrees to determine
 *  applicability of information provided. If this file has been provided on
 *  magnetic media by Unicode, Inc., the sole remedy for any claim will be
 *  exchange of defective media within 90 days of receipt.
 *  
 *  Recipient is granted the right to make copies in any form for internal
 *  distribution and to freely use the information supplied in the creation of
 *  products supporting Unicode. Unicode, Inc. specifically excludes the right
 *  to re-distribute this file directly to third parties or other organizations
 *  whether for profit or not.
 *  
 *  Format: 4 tab-delimited fields:
 * 
 *    (1) The Unicode value (in hexadecimal)
 *    (2) The Symbol Encoding code point (in hexadecimal)
 *    (3) # Unicode name
 *    (4) # PostScript character name
 *  
 *  General Notes:
 * 
 *    The Unicode values in this table were produced as the result of applying
 *    the algorithm described in the section &quot;Populating a Unicode space&quot; in the
 *    document &quot;Unicode and Glyph Names,&quot; at
 *    http://partners.adobe.com/asn/developer/typeforum/unicodegn.html
 *    to the characters in Symbol. Note that some characters, such as &quot;space&quot;,
 *    are mapped to 2 Unicode values. 29 characters have assignments in the
 *    Corporate Use Subarea; these are indicated by &quot;(CUS)&quot; in field 4. Refer to
 *    the above document for more details.
 * 
 *  Revision History:
 * 
 *    [v0.2, 30 March 1999]
 *    Different algorithm to produce Unicode values (see notes above) results in
 *    some character codes being mapped to 2 Unicode values; use of Corporate
 *    Use subarea values; addition of the euro character; changed assignments of
 *    some characters such as the COPYRIGHT SIGNs and RADICAL EXTENDER. Updated
 *    Unicode names to Unicode 2.0 names.
 * 
 *    [v0.1, 5 May 1995] First release.
 * 
 *  Contact &lt;unicode-inc@unicode.org&gt; with any questions or comments.
 * </pre>
 * 
 * @version $Id$
 * @since May 7, 2009
 */
public class SymbolDecoder {

	final static char[] map = {
		'\u0000', //	00	# null
		'\u0000', //	01	# null
		'\u0000', //	02	# null
		'\u0000', //	03	# null
		'\u0000', //	04	# null
		'\u0000', //	05	# null
		'\u0000', //	06	# null
		'\u0000', //	07	# null
		'\u0000', //	08	# null
		'\u0000', //	09	# null
		'\u0000', //	0A	# null
		'\u0000', //	0B	# null
		'\u0000', //	0C	# null
		'\u0000', //	0D	# null
		'\u0000', //	0E	# null
		'\u0000', //	0F	# null
		'\u0000', //	10	# null
		'\u0000', //	11	# null
		'\u0000', //	12	# null
		'\u0000', //	13	# null
		'\u0000', //	14	# null
		'\u0000', //	15	# null
		'\u0000', //	16	# null
		'\u0000', //	17	# null
		'\u0000', //	18	# null
		'\u0000', //	19	# null
		'\u0000', //	1A	# null
		'\u0000', //	1B	# null
		'\u0000', //	1C	# null
		'\u0000', //	1D	# null
		'\u0000', //	1E	# null
		'\u0000', //	1F	# null
		'\u0020', // 	20	# SPACE	# space
		'\u0021', // 	21	# EXCLAMATION MARK	# exclam
		'\u2200', // 	22	# FOR ALL	# universal
		'\u0023', // 	23	# NUMBER SIGN	# numbersign
		'\u2203', // 	24	# THERE EXISTS	# existential
		'\u0025', // 	25	# PERCENT SIGN	# percent
		'\u0026', // 	26	# AMPERSAND	# ampersand
		'\u220B', // 	27	# CONTAINS AS MEMBER	# suchthat
		'\u0028', // 	28	# LEFT PARENTHESIS	# parenleft
		'\u0029', // 	29	# RIGHT PARENTHESIS	# parenright
		'\u2217', // 	2A	# ASTERISK OPERATOR	# asteriskmath
		'\u002B', // 	2B	# PLUS SIGN	# plus
		'\u002C', // 	2C	# COMMA	# comma
		'\u2212', // 	2D	# MINUS SIGN	# minus
		'\u002E', // 	2E	# FULL STOP	# period
		'\u002F', // 	2F	# SOLIDUS	# slash
		'\u0030', // 	30	# DIGIT ZERO	# zero
		'\u0031', // 	31	# DIGIT ONE	# one
		'\u0032', // 	32	# DIGIT TWO	# two
		'\u0033', // 	33	# DIGIT THREE	# three
		'\u0034', // 	34	# DIGIT FOUR	# four
		'\u0035', // 	35	# DIGIT FIVE	# five
		'\u0036', // 	36	# DIGIT SIX	# six
		'\u0037', // 	37	# DIGIT SEVEN	# seven
		'\u0038', // 	38	# DIGIT EIGHT	# eight
		'\u0039', // 	39	# DIGIT NINE	# nine
		'\u003A', // 	3A	# COLON	# colon
		'\u003B', // 	3B	# SEMICOLON	# semicolon
		'\u003C', // 	3C	# LESS-THAN SIGN	# less
		'\u003D', // 	3D	# EQUALS SIGN	# equal
		'\u003E', // 	3E	# GREATER-THAN SIGN	# greater
		'\u003F', // 	3F	# QUESTION MARK	# question
		'\u2245', // 	40	# APPROXIMATELY EQUAL TO	# congruent
		'\u0391', // 	41	# GREEK CAPITAL LETTER ALPHA	# Alpha
		'\u0392', // 	42	# GREEK CAPITAL LETTER BETA	# Beta
		'\u03A7', // 	43	# GREEK CAPITAL LETTER CHI	# Chi
		'\u0394', // 	44	# GREEK CAPITAL LETTER DELTA	# Delta
		'\u0395', // 	45	# GREEK CAPITAL LETTER EPSILON	# Epsilon
		'\u03A6', // 	46	# GREEK CAPITAL LETTER PHI	# Phi
		'\u0393', // 	47	# GREEK CAPITAL LETTER GAMMA	# Gamma
		'\u0397', // 	48	# GREEK CAPITAL LETTER ETA	# Eta
		'\u0399', // 	49	# GREEK CAPITAL LETTER IOTA	# Iota
		'\u03D1', // 	4A	# GREEK THETA SYMBOL	# theta1
		'\u039A', // 	4B	# GREEK CAPITAL LETTER KAPPA	# Kappa
		'\u039B', // 	4C	# GREEK CAPITAL LETTER LAMDA	# Lambda
		'\u039C', // 	4D	# GREEK CAPITAL LETTER MU	# Mu
		'\u039D', // 	4E	# GREEK CAPITAL LETTER NU	# Nu
		'\u039F', // 	4F	# GREEK CAPITAL LETTER OMICRON	# Omicron
		'\u03A0', // 	50	# GREEK CAPITAL LETTER PI	# Pi
		'\u0398', // 	51	# GREEK CAPITAL LETTER THETA	# Theta
		'\u03A1', // 	52	# GREEK CAPITAL LETTER RHO	# Rho
		'\u03A3', // 	53	# GREEK CAPITAL LETTER SIGMA	# Sigma
		'\u03A4', // 	54	# GREEK CAPITAL LETTER TAU	# Tau
		'\u03A5', // 	55	# GREEK CAPITAL LETTER UPSILON	# Upsilon
		'\u03C2', // 	56	# GREEK SMALL LETTER FINAL SIGMA	# sigma1
		'\u03A9', // 	57	# GREEK CAPITAL LETTER OMEGA	# Omega
		'\u039E', // 	58	# GREEK CAPITAL LETTER XI	# Xi
		'\u03A8', // 	59	# GREEK CAPITAL LETTER PSI	# Psi
		'\u0396', // 	5A	# GREEK CAPITAL LETTER ZETA	# Zeta
		'\u005B', // 	5B	# LEFT SQUARE BRACKET	# bracketleft
		'\u2234', // 	5C	# THEREFORE	# therefore
		'\u005D', // 	5D	# RIGHT SQUARE BRACKET	# bracketright
		'\u22A5', // 	5E	# UP TACK	# perpendicular
		'\u005F', // 	5F	# LOW LINE	# underscore
		'\uF8E5', // 	60	# RADICAL EXTENDER	# radicalex (CUS)
		'\u03B1', // 	61	# GREEK SMALL LETTER ALPHA	# alpha
		'\u03B2', // 	62	# GREEK SMALL LETTER BETA	# beta
		'\u03C7', // 	63	# GREEK SMALL LETTER CHI	# chi
		'\u03B4', // 	64	# GREEK SMALL LETTER DELTA	# delta
		'\u03B5', // 	65	# GREEK SMALL LETTER EPSILON	# epsilon
		'\u03C6', // 	66	# GREEK SMALL LETTER PHI	# phi
		'\u03B3', // 	67	# GREEK SMALL LETTER GAMMA	# gamma
		'\u03B7', // 	68	# GREEK SMALL LETTER ETA	# eta
		'\u03B9', // 	69	# GREEK SMALL LETTER IOTA	# iota
		'\u03D5', // 	6A	# GREEK PHI SYMBOL	# phi1
		'\u03BA', // 	6B	# GREEK SMALL LETTER KAPPA	# kappa
		'\u03BB', // 	6C	# GREEK SMALL LETTER LAMDA	# lambda
		'\u03BC', // 	6D	# GREEK SMALL LETTER MU	# mu
		'\u03BD', // 	6E	# GREEK SMALL LETTER NU	# nu
		'\u03BF', // 	6F	# GREEK SMALL LETTER OMICRON	# omicron
		'\u03C0', // 	70	# GREEK SMALL LETTER PI	# pi
		'\u03B8', // 	71	# GREEK SMALL LETTER THETA	# theta
		'\u03C1', // 	72	# GREEK SMALL LETTER RHO	# rho
		'\u03C3', // 	73	# GREEK SMALL LETTER SIGMA	# sigma
		'\u03C4', // 	74	# GREEK SMALL LETTER TAU	# tau
		'\u03C5', // 	75	# GREEK SMALL LETTER UPSILON	# upsilon
		'\u03D6', // 	76	# GREEK PI SYMBOL	# omega1
		'\u03C9', // 	77	# GREEK SMALL LETTER OMEGA	# omega
		'\u03BE', // 	78	# GREEK SMALL LETTER XI	# xi
		'\u03C8', // 	79	# GREEK SMALL LETTER PSI	# psi
		'\u03B6', // 	7A	# GREEK SMALL LETTER ZETA	# zeta
		'\u007B', // 	7B	# LEFT CURLY BRACKET	# braceleft
		'\u007C', // 	7C	# VERTICAL LINE	# bar
		'\u007D', // 	7D	# RIGHT CURLY BRACKET	# braceright
		'\u223C', // 	7E	# TILDE OPERATOR	# similar
		'\u0000', //	7F	# null
		'\u0000', //	80	# null
		'\u0000', //	81	# null
		'\u0000', //	82	# null
		'\u0000', //	83	# null
		'\u0000', //	84	# null
		'\u0000', //	85	# null
		'\u0000', //	86	# null
		'\u0000', //	87	# null
		'\u0000', //	88	# null
		'\u0000', //	89	# null
		'\u0000', //	8A	# null
		'\u0000', //	8B	# null
		'\u0000', //	8C	# null
		'\u0000', //	8D	# null
		'\u0000', //	8E	# null
		'\u0000', //	8F	# null
		'\u0000', //	90	# null
		'\u0000', //	91	# null
		'\u0000', //	92	# null
		'\u0000', //	93	# null
		'\u0000', //	94	# null
		'\u0000', //	95	# null
		'\u0000', //	96	# null
		'\u0000', //	97	# null
		'\u0000', //	98	# null
		'\u0000', //	99	# null
		'\u0000', //	9A	# null
		'\u0000', //	9B	# null
		'\u0000', //	9C	# null
		'\u0000', //	9D	# null
		'\u0000', //	9E	# null
		'\u0000', //	9F	# null
		'\u0000', // 	A0	# null
		'\u03D2', // 	A1	# GREEK UPSILON WITH HOOK SYMBOL	# Upsilon1
		'\u2032', // 	A2	# PRIME	# minute
		'\u2264', // 	A3	# LESS-THAN OR EQUAL TO	# lessequal
		'\u2215', // 	A4	# DIVISION SLASH	# fraction
		'\u221E', // 	A5	# INFINITY	# infinity
		'\u0192', // 	A6	# LATIN SMALL LETTER F WITH HOOK	# florin
		'\u2663', // 	A7	# BLACK CLUB SUIT	# club
		'\u2666', // 	A8	# BLACK DIAMOND SUIT	# diamond
		'\u2665', // 	A9	# BLACK HEART SUIT	# heart
		'\u2660', // 	AA	# BLACK SPADE SUIT	# spade
		'\u2194', // 	AB	# LEFT RIGHT ARROW	# arrowboth
		'\u2190', // 	AC	# LEFTWARDS ARROW	# arrowleft
		'\u2191', // 	AD	# UPWARDS ARROW	# arrowup
		'\u2192', // 	AE	# RIGHTWARDS ARROW	# arrowright
		'\u2193', // 	AF	# DOWNWARDS ARROW	# arrowdown
		'\u00B0', // 	B0	# DEGREE SIGN	# degree
		'\u00B1', // 	B1	# PLUS-MINUS SIGN	# plusminus
		'\u2033', // 	B2	# DOUBLE PRIME	# second
		'\u2265', // 	B3	# GREATER-THAN OR EQUAL TO	# greaterequal
		'\u00D7', // 	B4	# MULTIPLICATION SIGN	# multiply
		'\u221D', // 	B5	# PROPORTIONAL TO	# proportional
		'\u2202', // 	B6	# PARTIAL DIFFERENTIAL	# partialdiff
		'\u2022', // 	B7	# BULLET	# bullet
		'\u00F7', // 	B8	# DIVISION SIGN	# divide
		'\u2260', // 	B9	# NOT EQUAL TO	# notequal
		'\u2261', // 	BA	# IDENTICAL TO	# equivalence
		'\u2248', // 	BB	# ALMOST EQUAL TO	# approxequal
		'\u2026', // 	BC	# HORIZONTAL ELLIPSIS	# ellipsis
		'\uF8E6', // 	BD	# VERTICAL ARROW EXTENDER	# arrowvertex (CUS)
		'\uF8E7', // 	BE	# HORIZONTAL ARROW EXTENDER	# arrowhorizex (CUS)
		'\u21B5', // 	BF	# DOWNWARDS ARROW WITH CORNER LEFTWARDS	# carriagereturn
		'\u2135', // 	C0	# ALEF SYMBOL	# aleph
		'\u2111', // 	C1	# BLACK-LETTER CAPITAL I	# Ifraktur
		'\u211C', // 	C2	# BLACK-LETTER CAPITAL R	# Rfraktur
		'\u2118', // 	C3	# SCRIPT CAPITAL P	# weierstrass
		'\u2297', // 	C4	# CIRCLED TIMES	# circlemultiply
		'\u2295', // 	C5	# CIRCLED PLUS	# circleplus
		'\u2205', // 	C6	# EMPTY SET	# emptyset
		'\u2229', // 	C7	# INTERSECTION	# intersection
		'\u222A', // 	C8	# UNION	# union
		'\u2283', // 	C9	# SUPERSET OF	# propersuperset
		'\u2287', // 	CA	# SUPERSET OF OR EQUAL TO	# reflexsuperset
		'\u2284', // 	CB	# NOT A SUBSET OF	# notsubset
		'\u2282', // 	CC	# SUBSET OF	# propersubset
		'\u2286', // 	CD	# SUBSET OF OR EQUAL TO	# reflexsubset
		'\u2208', // 	CE	# ELEMENT OF	# element
		'\u2209', // 	CF	# NOT AN ELEMENT OF	# notelement
		'\u2220', // 	D0	# ANGLE	# angle
		'\u2207', // 	D1	# NABLA	# gradient
		'\uF6DA', // 	D2	# REGISTERED SIGN SERIF	# registerserif (CUS)
		'\uF6D9', // 	D3	# COPYRIGHT SIGN SERIF	# copyrightserif (CUS)
		'\uF6DB', // 	D4	# TRADE MARK SIGN SERIF	# trademarkserif (CUS)
		'\u220F', // 	D5	# N-ARY PRODUCT	# product
		'\u221A', // 	D6	# SQUARE ROOT	# radical
		'\u22C5', // 	D7	# DOT OPERATOR	# dotmath
		'\u00AC', // 	D8	# NOT SIGN	# logicalnot
		'\u2227', // 	D9	# LOGICAL AND	# logicaland
		'\u2228', // 	DA	# LOGICAL OR	# logicalor
		'\u21D4', // 	DB	# LEFT RIGHT DOUBLE ARROW	# arrowdblboth
		'\u21D0', // 	DC	# LEFTWARDS DOUBLE ARROW	# arrowdblleft
		'\u21D1', // 	DD	# UPWARDS DOUBLE ARROW	# arrowdblup
		'\u21D2', // 	DE	# RIGHTWARDS DOUBLE ARROW	# arrowdblright
		'\u21D3', // 	DF	# DOWNWARDS DOUBLE ARROW	# arrowdbldown
		'\u25CA', // 	E0	# LOZENGE	# lozenge
		'\u2329', // 	E1	# LEFT-POINTING ANGLE BRACKET	# angleleft
		'\uF8E8', // 	E2	# REGISTERED SIGN SANS SERIF	# registersans (CUS)
		'\uF8E9', // 	E3	# COPYRIGHT SIGN SANS SERIF	# copyrightsans (CUS)
		'\uF8EA', // 	E4	# TRADE MARK SIGN SANS SERIF	# trademarksans (CUS)
		'\u2211', // 	E5	# N-ARY SUMMATION	# summation
		'\uF8EB', // 	E6	# LEFT PAREN TOP	# parenlefttp (CUS)
		'\uF8EC', // 	E7	# LEFT PAREN EXTENDER	# parenleftex (CUS)
		'\uF8ED', // 	E8	# LEFT PAREN BOTTOM	# parenleftbt (CUS)
		'\uF8EE', // 	E9	# LEFT SQUARE BRACKET TOP	# bracketlefttp (CUS)
		'\uF8EF', // 	EA	# LEFT SQUARE BRACKET EXTENDER	# bracketleftex (CUS)
		'\uF8F0', // 	EB	# LEFT SQUARE BRACKET BOTTOM	# bracketleftbt (CUS)
		'\uF8F1', // 	EC	# LEFT CURLY BRACKET TOP	# bracelefttp (CUS)
		'\uF8F2', // 	ED	# LEFT CURLY BRACKET MID	# braceleftmid (CUS)
		'\uF8F3', // 	EE	# LEFT CURLY BRACKET BOTTOM	# braceleftbt (CUS)
		'\uF8F4', // 	EF	# CURLY BRACKET EXTENDER	# braceex (CUS)
		'\u0000', //	F0	# null
		'\u232A', // 	F1	# RIGHT-POINTING ANGLE BRACKET	# angleright
		'\u222B', // 	F2	# INTEGRAL	# integral
		'\u2320', // 	F3	# TOP HALF INTEGRAL	# integraltp
		'\uF8F5', // 	F4	# INTEGRAL EXTENDER	# integralex (CUS)
		'\u2321', // 	F5	# BOTTOM HALF INTEGRAL	# integralbt
		'\uF8F6', // 	F6	# RIGHT PAREN TOP	# parenrighttp (CUS)
		'\uF8F7', // 	F7	# RIGHT PAREN EXTENDER	# parenrightex (CUS)
		'\uF8F8', // 	F8	# RIGHT PAREN BOTTOM	# parenrightbt (CUS)
		'\uF8F9', // 	F9	# RIGHT SQUARE BRACKET TOP	# bracketrighttp (CUS)
		'\uF8FA', // 	FA	# RIGHT SQUARE BRACKET EXTENDER	# bracketrightex (CUS)
		'\uF8FB', // 	FB	# RIGHT SQUARE BRACKET BOTTOM	# bracketrightbt (CUS)
		'\uF8FC', // 	FC	# RIGHT CURLY BRACKET TOP	# bracerighttp (CUS)
		'\uF8FD', // 	FD	# RIGHT CURLY BRACKET MID	# bracerightmid (CUS)
		'\uF8FE', // 	FE	# RIGHT CURLY BRACKET BOTTOM	# bracerightbt (CUS)
		'\u0000', // 	FF	# null
	};

	static String decode(String s) {
		StringBuffer sb = new StringBuffer(s.length());

		for (int i = 0; i < s.length(); i++) {
			sb.append(map[s.charAt(i)]);
		}

		return sb.toString();
	}
}

/*
 * vim:encoding=utf8
 */
