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
 * Time.xsl
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
    <xsl:import
        href="./com.raytheon.uf.common.datadelivery.registry.GriddedTime.xsl" />
    <xsl:import
        href="./com.raytheon.uf.common.datadelivery.registry.PointTime.xsl" />
    <xsl:output method="xml" encoding="utf-8" indent="yes" />

    <xsl:template match="time" name="time">
        <time xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">

            <xsl:if test="@xsi:type != ''">
                <xsl:attribute name="xsi:type">
        		 	<xsl:value-of select="@xsi:type" />
				</xsl:attribute>
            </xsl:if>
            <xsl:if test="@format != ''">
                <xsl:attribute name="format">
        			<xsl:value-of select="@format" />
				</xsl:attribute>
            </xsl:if>
            <xsl:if test="@numTimes != ''">
                <xsl:attribute name="numTimes">
        			<xsl:value-of select="@numTimes" />
				</xsl:attribute>
            </xsl:if>

            <!-- figure out which type of time we are processing -->
            <xsl:choose>
                <xsl:when test="@xsi:type='pointTime'">
                    <xsl:call-template name="pointTime" />
                </xsl:when>
                <xsl:when test="@xsi:type='griddedTime'">
                    <xsl:call-template name="griddedTime" />
                </xsl:when>
                <xsl:otherwise>
                    <!-- unknown type, do nothing -->
                </xsl:otherwise>
            </xsl:choose>

            <xsl:if test="start">
                <xsl:element name="start">
                    <xsl:value-of select="start" />
                </xsl:element>
            </xsl:if>
            <xsl:if test="end">
                <xsl:element name="end">
                    <xsl:value-of select="end" />
                </xsl:element>
            </xsl:if>
            <xsl:if test="requestStart">
                <xsl:element name="requestStart">
                    <xsl:value-of select="requestStart" />
                </xsl:element>
            </xsl:if>
            <xsl:if test="requestEnd">
                <xsl:element name="requestEnd">
                    <xsl:value-of select="requestEnd" />
                </xsl:element>
            </xsl:if>
        </time>
    </xsl:template>

</xsl:stylesheet>