<?xml version="1.0" encoding="ISO-8859-1"?>
<!-- 
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
  * 
 * GriddedCoverage.xsl
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 ******************************************************************
 * Mar 02, 2014 2789      dhladky      XSLT transformation of other versions to current.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
-->



<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" version="1.0">
    <xsl:output method="xml" encoding="utf-8" indent="yes" />
    <xsl:template match="coverage" name="griddedCoverage">

        <xsl:if test="@gridName != ''">
            <xsl:attribute name="gridName">
    		<xsl:value-of select="@gridName" />
		</xsl:attribute>
        </xsl:if>
        <xsl:if test="@modelName != ''">
            <xsl:attribute name="modelName">
    		<xsl:value-of select="@modelName" />
		</xsl:attribute>
        </xsl:if>

        <xsl:apply-templates select="gridCoverage" />

    </xsl:template>

    <xsl:template match="gridCoverage">
        <xsl:element name="gridCoverage">
            <xsl:attribute name="xsi:type">
    		<xsl:value-of select="@xsi:type" />
		</xsl:attribute>
            <!-- figure out which type of coverage we are processing -->
            <xsl:choose>
                <xsl:when test="@xsi:type='latLonGridCoverage'">
                    <xsl:element name="name">
                        <xsl:value-of select="name" />
                    </xsl:element>
                    <xsl:element name="geometry">
                        <xsl:value-of select="geometry" />
                    </xsl:element>
                    <xsl:element name="crsWKT">
                        <xsl:value-of select="crsWKT" />
                    </xsl:element>
                    <xsl:element name="la1">
                        <xsl:value-of select="la1" />
                    </xsl:element>
                    <xsl:element name="lo1">
                        <xsl:value-of select="lo1" />
                    </xsl:element>
                    <xsl:element name="firstGridPointCorner">
                        <xsl:value-of select="firstGridPointCorner" />
                    </xsl:element>
                    <xsl:element name="nx">
                        <xsl:value-of select="nx" />
                    </xsl:element>
                    <xsl:element name="ny">
                        <xsl:value-of select="ny" />
                    </xsl:element>
                    <xsl:element name="dx">
                        <xsl:value-of select="dx" />
                    </xsl:element>
                    <xsl:element name="dy">
                        <xsl:value-of select="dy" />
                    </xsl:element>
                    <xsl:element name="spacingUnit">
                        <xsl:value-of select="spacingUnit" />
                    </xsl:element>
                    <xsl:element name="la2">
                        <xsl:value-of select="la2" />
                    </xsl:element>
                    <xsl:element name="lo2">
                        <xsl:value-of select="lo2" />
                    </xsl:element>
                    <xsl:element name="isThin">
                        <xsl:value-of select="isThin" />
                    </xsl:element>
                </xsl:when>
                <xsl:otherwise>
                    <!-- We don't have any others right now. -->
                </xsl:otherwise>
            </xsl:choose>
        </xsl:element>
    </xsl:template>

</xsl:stylesheet>